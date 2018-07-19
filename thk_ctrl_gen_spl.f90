subroutine thk_ctrl_gen_spl(uthk, thk, n, nknt, d_0, d_1, dAdE, dBdE, dCdE, dDdE, dEdE, &
	ddB1, ddC1, ddD1, u_spl, np, iknt, niknt, d3v_ang_flag, min_flag, Eder_flag, out_flag, lete_flag, &
	grad_flag, d1_flag, F, G, out_coord, coeff_out, fail_flag_2, dev_flag)

implicit none

integer, parameter :: sp = kind(0.0), dp = kind(0.0D0)
integer, intent(in) :: n, nknt, np, niknt, d3v_ang_flag, min_flag, Eder_flag, out_flag, &
	lete_flag, grad_flag, d1_flag, dev_flag
integer, intent(out) :: fail_flag_2
real, intent(in) :: uthk(n), thk(n), iknt(niknt), d_0(3), d_1(3), u_spl(np), ddB1(nknt-1, 5), &
	ddC1(nknt-1, 5), ddD1(nknt-1, 5), dAdE(nknt-1, niknt), dBdE(nknt-1, niknt), dCdE(nknt-1, niknt), &
	dDdE(nknt-1, niknt), dEdE(nknt-1, niknt)
real, intent(out) ::  F, G(6), out_coord(np, 12), coeff_out(nknt-1, 5)

integer :: i, j, k, l, p, nun, nseg, thkm_i, row, col, fail_flag, row_Emin, row_D1min, seg_end, &
	d2v_sign, d3v_sign, seg_id, er, nseg1, nseg2, nmax, nmin, line_flag
integer, allocatable, dimension(:) :: imin, imax
complex(kind = dp) :: root(4)
real :: u_thkm, t, d1v_0, d2v_0, d3v_0, d1v_1, d2v_1, d3v_1, d2v_mod, d2v_det, d2v_m, thk_m, &
	tr(2), tr2, temp, F1, dseg1, dseg2, d2v_mid, d3v_mod, F2, obj_scale, dd2v_mod_dB1, &
	dd3v_mod_dB1, dd2v_mod_dC1, dd3v_mod_dC1, dd2v_mod_dD1, dd3v_mod_dD1, dRdB1, dRdC1, dRdD1, slope(2)
real, allocatable, dimension(:) :: u, r, delu, A, B, C, D, E, coeff, u_end, v_end, t_spl, v_spl, &
	d1v_spl, d2v_spl, d3v_spl, d4v_spl, u_spl_dev
real, allocatable, dimension(:, :) :: M, M1, out_coord_temp
character (len = 2) :: tvalid

Allocate(u_spl_dev(np))
Allocate(out_coord_temp(np, 12))
if (min_flag .eq. 0 .or. min_flag .eq. 10) then
	thkm_i = maxloc(thk, 1)
elseif (min_flag .eq. 1 .or. min_flag .eq. 11) then
	thkm_i = minloc(thk, 1)
endif
u_thkm = uthk(thkm_i)
d1v_0 = d_0(1); d2v_0 = d_0(2); d3v_0 = d_0(3)
d1v_1 = d_1(1); d2v_1 = d_1(2); d3v_1 = d_1(3)
line_flag = 1
slope(1) = (thk(2)-thk(1))/(uthk(2)-uthk(1))
do i = 2, n-1
	slope(2) = (thk(i+1)-thk(i))/(uthk(i+1)-uthk(i))
	if (abs(slope(2) - slope(1)) .gt. 1.E-10*slope(1)) then
		line_flag = 0
		exit
	endif
enddo

nmax = 0; nmin = 0
do i = 2, n-1
	if (thk(i) .ge. thk(i-1) .and. thk(i) .ge. thk(i+1)) then
		nmax = nmax+1
	elseif (thk(i) .le. thk(i-1) .and. thk(i) .le. thk(i+1)) then
		nmin = nmin+1
	endif
enddo
if (nmin .gt. 0) then
	Allocate(imin(nmin))
else
	Allocate(imin(1))
endif
if (nmax .gt. 0) then
	Allocate(imax(nmax))
else
	Allocate(imax(1))
endif
j = 1; k = 1
do i = 2, n-1
	if (thk(i) .ge. thk(i-1) .and. thk(i) .ge. thk(i+1) .and. nmax .gt. 0) then
		imax(j) = i; j = j+1
	elseif (thk(i) .le. thk(i-1) .and. thk(i) .le. thk(i+1) .and. nmin .gt. 0) then
		imin(k) = i; k = k+1
	endif
enddo

!	Segmentation
Allocate(u(nknt))
nseg = nknt-1
nun = 5*nseg-4
if (d1_flag .eq. 1) nun = nun + 1
if (min_flag .eq. 0 .or. min_flag .eq. 1) then
	u(1) = uthk(1); u(nknt) = uthk(n)
	nseg1 = ceiling(nseg/2.)
	dseg1 = (u_thkm-u(1))/nseg1
	u(2:nseg1+1) = (/ (i, i = 1,nseg1) /)*dseg1 + u(1)
	nseg2 = nseg-nseg1
	dseg2 = (u(nknt)-u_thkm)/nseg2
	u(nseg1+2:nknt) = (/ (i, i = 1, nseg2) /)*dseg2 + u_thkm
elseif (min_flag .eq. 10 .or. min_flag .eq. 11) then
	u(1) = uthk(1); u(nknt) = uthk(n)
	dseg1 = (uthk(n)-u(1))/nseg
	u(2:nseg+1) = (/ (i, i = 1,nseg) /)*dseg1 + u(1)
endif

Allocate(A(nseg))
Allocate(B(nseg))
Allocate(C(nseg))
Allocate(D(nseg))
Allocate(E(nseg))
Allocate(M(nun, nun))
Allocate(r(nun))
Allocate(M1(nun, nun+1))
Allocate(delu(nknt-1))
Allocate(coeff(nun))
Allocate(t_spl(np))
Allocate(v_spl(np))
Allocate(d1v_spl(np))
Allocate(d2v_spl(np))
Allocate(d3v_spl(np))
Allocate(d4v_spl(np))
Allocate(u_end(nseg))
Allocate(v_end(nseg))

A = 0.; B = 0.; C = 0.; D = 0.; E = 0.
M = 0.; r = 0.; out_coord = 0.; out_coord_temp = 0.
delu = u(2:nknt) - u(1:nknt-1)
A(1) = thk(1)
B(1) = d1v_0*delu(1)
C(1) = d2v_0/2.*delu(1)**2
D(1) = d3v_0*delu(1)**3/6.

if (Eder_flag .eq. 1) then
	do i = 1, niknt
		call thk_ctrl_gen_Eder_fn(uthk, thk, u, n, nknt, iknt, niknt, i, min_flag, &
		dAdE, dBdE, dCdE, dDdE, dEdE)
	enddo
endif

!!      C0 continuity
M(1, 1:2) = (/ 1., -1. /)
r(1) = -A(1) - B(1) - C(1) - D(1)
if (d1_flag .eq. 1) then
	M(1, nun) = 1.
	r(1) = -A(1) - B(1) - C(1)
endif
col = 2
row = 1
do i = 2, nknt-2
    row = row + 1    
    M(row, col:col+5) = (/ 1., 1., 1., 1., 1., -1. /)
    col = col + 5
enddo

!!      C1 continuity
row = row + 1
M(row, 1 : 3) = (/ 4., 0., -delu(1)/delu(2) /)
r(row) = - B(1) - 2.*C(1) - 3.*D(1)
if (d1_flag .eq. 1) then
	M(row, nun) = 3.
	r(row) = - B(1) - 2.*C(1)
endif
col = 2
do i = 2, nknt-2
    row = row + 1
    M(row, col:col+6) = (/ 0., 1., 2., 3., 4., 0., -delu(i)/delu(i+1) /)
    col = col + 5
enddo

!!      C2 continuity
row = row + 1
M(row, 1 : 4) = (/ 6., 0., 0., -(delu(1)/delu(2))**2 /)
r(row) = - C(1) - 3.*D(1)
if (d1_flag .eq. 1) then
	M(row, nun) = 3.
	r(row) = - C(1)
endif
col = 2
do i = 2, nknt-2
    row = row + 1
    M(row, col:col+7) = (/ 0., 0., 1., 3., 6., 0., 0., -(delu(i)/delu(i+1))**2 /)
    col = col + 5
enddo

!!      C3 continuity
row = row + 1
M(row, 1 : 5) = (/ 4., 0., 0., 0., -(delu(1)/delu(2))**3 /)
r(row) = -D(1)
if (d1_flag .eq. 1) then
	M(row, nun) = 1.
	r(row) = 0.
endif
col = 2
do i = 2, nknt-2
    row = row + 1
    M(row, col:col+8) = (/ 0., 0., 0., 1., 4., 0., 0., 0., -(delu(i)/delu(i+1))**3 /)
    col = col + 5
enddo

!!      Thickness values
do i = 2, n-1
    do j = 1, nseg
        if (uthk(i) .gt. u(j) .and. uthk(i) .le. u(j+1)) then
            t = (uthk(i)-u(j))/(u(j+1)-u(j))
            col = (j-2)*5 + 2
            row = row + 1            
            M(row, col:col+4) = (/ 1., t, t**2, t**3, t**4 /)
			r(row) = thk(i)
            exit
        endif
    enddo
enddo

!!      Slope at maximum/minimum thickness
! if (min_flag .ne. 10 .and. min_flag .ne. 11) then
if (nmax .gt. 0) then
	do i = 1, nmax
		do j = 1, nseg
			if (uthk(imax(i)) .gt. u(j) .and. uthk(imax(i)) .le. u(j+1)) then
				t = (uthk(imax(i))-u(j))/(u(j+1)-u(j))
				col = (j-2)*5 + 2
				row = row + 1
				M(row, col:col+4) = (/ 0., 1., 2.*t, 3.*t**2, 4.*t**3 /)
				exit
			endif
		enddo
	enddo
endif
if (nmin .gt. 0) then
	do i = 1, nmin
		do j = 1, nseg
			if (uthk(imin(i)) .gt. u(j) .and. uthk(imin(i)) .le. u(j+1)) then
				t = (uthk(imin(i))-u(j))/(u(j+1)-u(j))
				col = (j-2)*5 + 2
				row = row + 1
				M(row, col:col+4) = (/ 0., 1., 2.*t, 3.*t**2, 4.*t**3 /)
				exit
			endif
		enddo
	enddo
endif
! endif

!!      TE conditions
if (d1_flag .eq. 0) then
	col = nun-4
elseif (d1_flag .eq. 1) then
	col = nun-5
endif
!       TE thickness
row = row+1
M(row, col:col+4) = (/ 1., 1., 1., 1., 1. /)
r(row) = thk(n)
!       TE slope
row = row+1
M(row, col : col+4) = (/ 0., 1., 2., 3., 4. /)
r(row) = d1v_1*delu(nknt-1)
!       TE second derivative
row = row+1
M(row, col : col+4) = (/ 0., 0., 2., 6., 12. /)
r(row) = d2v_1*delu(nknt-1)**2
!       TE third derivative
row = row+1
M(row, col : col+4) = (/ 0., 0., 0., 6., 24. /)
r(row) = d3v_1*delu(nknt-1)**3

!! Minimization conditions for E coefficients
row_Emin = row
! Minimization of d2v_sq only
do i = 1, niknt
    row = row + 1
    M(row, 1) = 288./5./delu(1)**3*dEdE(1, i)
	if (d1_flag .eq. 0) then	
		r(row) = - C(1)/delu(1)**3*16.*dEdE(1, i) - D(1)/delu(1)**3*36.*dEdE(1, i)
	elseif (d1_flag .eq. 1) then
		M(row, nun) = M(row, nun) + 1./delu(1)**3*36.*dEdE(1, i)
		r(row) = - C(1)/delu(1)**3*16.*dEdE(1, i)
	endif
    col = 2
    do j = 2, nseg
        M(row, col:col+4) = (/ 0., 0., 16.*dEdE(j, i)+12.*dDdE(j, i)+8.*dCdE(j, i), &
            36.*dEdE(j, i)+24.*dDdE(j, i)+12.*dCdE(j, i), &
            288./5.*dEdE(j, i)+36.*dDdE(j, i)+16.*dCdE(j, i) /)
        M(row, col:col+4) = M(row, col:col+4)/delu(j)**3
        col = col + 5
    enddo
enddo

! Minimization of d3v_sq
if (d3v_ang_flag .eq. 1) then
	row = row_Emin
	do i = 1, niknt
		row = row + 1
		M(row, 1) = M(row, 1) + 16./delu(1)**5*dEdE(1, i)*delu(1)**1		
		if (d1_flag .eq. 0) then
			r(row) = r(row) - 6.*D(1)/delu(1)**5*dEdE(1, i)*delu(1)**1
		elseif (d1_flag .eq. 1) then
			M(row, nun) = M(row, nun) + 6./delu(1)**5*dEdE(1, i)*delu(1)**1
		endif
		col = 2
		do j = 2, nseg
			M(row, col:col+4) = M(row, col:col+4) + (/ 0., 0., 0., &
				6.*dEdE(j, i) + 3.*dDdE(j, i), &
				16.*dEdE(j, i) + 6.*dDdE(j, i) /) / delu(j)**5*delu(1)**1
			col = col + 5
		enddo
	enddo
endif

! Minimization of d3v/du3 angle change squared only
! if (d3v_ang_flag .eq. 1) then
    ! row = row_Emin
    ! do i = 1, niknt
        ! row = row+1
        ! seg_end = nseg-1
		! M(row, 1) = M(row, 1) + (2*24**2*(dEdE(1, i)/delu(1)**4 - dEdE(2, i)/delu(2)**4))*delu(1)**2
		! M(row, 6) = M(row, 6) + (2*24**2*(dEdE(2, i)/delu(2)**4 - dEdE(1, i)/delu(1)**4))*delu(1)**2
        ! col = 2
        ! do j = 2, seg_end
            ! M(row, col:col+9) = M(row, col:col+9) + (/ 0., 0., 0., 0., &
                ! 2.*delu(j)**2*24**2*(dEdE(j, i)/delu(j)**4 - dEdE(j+1, i)/delu(j+1)**4), &
                ! 0., 0., 0., 0., &
                ! 2.*delu(j)**2*24**2*(dEdE(j+1, i)/delu(j+1)**4 - dEdE(j, i)/delu(j)**4) /)
			! ! M(row, col:col+9) = M(row, col:col+9)*delu(i)**4
            ! col = col + 5
        ! enddo
    ! enddo
! endif


! Minimization of d3v/du3 arclength
! if (d3v_ang_flag .eq. 1) then
    ! row = row_Emin
	! do i = 1, niknt
		! row = row + 1
		! M(row, 1) = M(row, 1) + 1152.*dEdE(1, i)/delu(1)**3
		! col = 2
		! do j = 2, nseg
			! M(row, col:col+4) = M(row, col:col+4)+ (/ 0., 0., 0., 0., &
				! 1152.*dEdE(j, i) /) /delu(1)**3
			! col = col + 5
		! enddo
	! enddo
! endif

! ! Minimization of d3v_sq
! if (min_flag .eq. 1) then
    ! row = row_Emin
    ! mid_knt = nseg/2 + 1
    ! do i = 1, niknt
        ! row = row + 1
        ! if (iknt(i) .lt. mid_knt) then
            ! col = 2
            ! M(row, 1) = M(row, 1) + 36.*(32./3.*dEdE(1, i) + 4.*dDdE(1, i))!/delu(1)**5
            ! r(row) = r(row) - ddD1(1)*36.*(4.*dEdE(1, i) + 2.*dDdE(1, i))!/delu(1)**5
            ! do j = 2, mid_knt-1
                ! M(row, col : col+4) = M(row, col : col+4) + (/ 0., 0., 0., &
                    ! 36./delu(j)**5*(4.*dEdE(j, i) + 2.*dDdE(j, i)), &
                    ! 36./delu(j)**5*(32./3.*dEdE(j, i) + 4.*dDdE(j, i)) /)*delu(j)**5
                ! col = col + 5
            ! enddo
        ! elseif (iknt(i) .gt. mid_knt) then
            ! col = (nseg/2 - 1)*5 + 2
            ! do j = mid_knt, nseg
                ! M(row, col : col+4) = M(row, col : col+4) + (/ 0., 0., 0., &
                    ! 36./delu(j)**5*(4.*dEdE(j, i) + 2.*dDdE(j, i)), &
                    ! 36./delu(j)**5*(32./3.*dEdE(j, i) + 4.*dDdE(j, i)) /)*delu(j)**5
                ! col = col + 5
            ! enddo
        ! endif
    ! enddo
! endif


!! Minimization conditions for D1
if (d1_flag .eq. 1) then
	row_D1min = row
	! Minimization of d2v_sq only
	row = row + 1
	M(row, 1) = 288./5./delu(1)**3*ddD1(1, 5) + 36./delu(1)**3
	M(row, nun) = 1./delu(1)**3*(36.*ddD1(1, 5) +24.)
	r(row) = - C(1)/delu(1)**3*(16.*ddD1(1, 5) +12.)
	col = 2
	do j = 2, nseg
		M(row, col:col+4) = (/ 0., 0., 16.*ddD1(j, 5)+12.*ddD1(j, 4)+8.*ddD1(j, 3), &
			36.*ddD1(j, 5)+24.*ddD1(j, 4)+12.*ddD1(j, 3), &
			288./5.*ddD1(j, 5)+36.*ddD1(j, 4)+16.*ddD1(j, 3) /) / delu(j)**3
		col = col + 5
	enddo
endif

! ! Minimization of d3v_sq
if (d3v_ang_flag .eq. 1 .and. d1_flag .eq. 1) then
	row = row_D1min
	row = row + 1
	M(row, 1) = M(row, 1) + (16.*ddD1(1, 5) + 6.) /delu(1)**5*delu(1)**1
	M(row, nun) = M(row, nun) + (6.*ddD1(1, 5) + 3.)/delu(1)**5*delu(1)**1
	col = 2
	do j = 2, nseg
		M(row, col:col+4) = M(row, col:col+4) + (/ 0., 0., 0., &
			6.*ddD1(j, 5) + 3.*ddD1(j, 4), &
			16.*ddD1(j, 5) + 6.*ddD1(j, 4) /) / delu(j)**5*delu(1)**1
		col = col + 5
	enddo
endif

!!      Solving for spline coefficients
do i = 1, nun
    r(i) = r(i)/maxval(M(i, :))
    M(i, :) = M(i, :)/maxval(M(i, :))
enddo
M1(:, 1:nun) = M
M1(:, nun+1) = r
call gauss_jordan(nun, 1, M1, fail_flag)
if (fail_flag .eq. 1) then
    write (*, *) 'Failed to solve linear system: thk_ctrl_1der_fn'
endif
coeff = M1(:, nun+1)
do i = 1, nun
	if (coeff(i) /= coeff(i)) then
		print*, 'thk_ctrl_gen_spl Error: Nan value in solution to linear system'
		return
	endif
enddo
if (d1_flag .eq. 0) then
	E(1) = coeff(1)
	A(2 : nseg) = coeff(2 : nun : 5)
	B(2 : nseg) = coeff(3 : nun : 5)
	C(2 : nseg) = coeff(4 : nun : 5)
	D(2 : nseg) = coeff(5 : nun : 5)
	E(2 : nseg) = coeff(6 : nun : 5)
elseif (d1_flag .eq. 1) then
	E(1) = coeff(1)
	A(2 : nseg) = coeff(2 : nun-1 : 5)
	B(2 : nseg) = coeff(3 : nun-1 : 5)
	C(2 : nseg) = coeff(4 : nun-1 : 5)
	D(2 : nseg) = coeff(5 : nun-1 : 5)
	E(2 : nseg) = coeff(6 : nun-1 : 5)
	D(1) = coeff(nun)
endif

fail_flag_2 = 0
if (nmax .gt. 0 .and. line_flag .eq. 0) then
	do i = 1, nmax
		do j = 1, nseg
			if (uthk(imax(i)) .gt. u(j) .and. uthk(imax(i)) .le. u(j+1)) then
				t = (uthk(imax(i))-u(j))/(u(j+1)-u(j))
				d2v_m = (12.*E(j)*t**2 + 6.*D(j)*t + 2.*C(j))/delu(j)**2
				if (d2v_m .gt. 0.) then
					fail_flag_2 = 1
					exit
				endif
			endif
		enddo
	enddo
endif
if (nmin .gt. 0 .and. line_flag .eq. 0) then
	do i = 1, nmin
		do j = 1, nseg
			if (uthk(imin(i)) .gt. u(j) .and. uthk(imin(i)) .le. u(j+1)) then
				t = (uthk(imin(i))-u(j))/(u(j+1)-u(j))
				d2v_m = (12.*E(j)*t**2 + 6.*D(j)*t + 2.*C(j))/delu(j)**2
				if (d2v_m .lt. 0.) then
					fail_flag_2 = 1
					exit
				endif
			endif
		enddo
	enddo
endif

if (out_flag .eq. 1) then
	call thk_ctrl_gen_out ( (/ A, B, C, D, E /), u, u_spl, np, delu, nseg, lete_flag, out_coord)
	if (min_flag .eq. 0) then
		thk_m = maxval(thk)
	elseif (min_flag .eq. 1) then
		thk_m = minval(thk)
	! elseif (min_flag .eq. 10) then
		! thk_m = maxval(thk(2:end-1))
	! elseif (min_flag .eq. 11) then
		! thk_m = minval(thk(2:end-1))		
	endif
	do i = 1, np
		if (min_flag .eq. 0) then
			if (out_coord(i, 2) - thk_m .gt. 1.E-15) then
				fail_flag_2 = 2
				exit
			endif
		elseif (min_flag .eq. 1) then
			if (out_coord(i, 2) - thk_m .lt. -1.E-15) then
				fail_flag_2 = 2
				exit
			endif
		endif
	enddo
	if (lete_flag .eq. 1 .and. dev_flag .eq. 1) then
		do i = 1, np
			u_spl_dev(i) = (i-1.)/(np-1)*2.*uthk(3) + uthk(1)
		enddo
		call thk_ctrl_gen_out ( (/ A, B, C, D, E /), u, u_spl_dev, np, delu, nseg, 0, out_coord_temp)
		out_coord(:, 7:12) = out_coord_temp(:, 1:6)
	endif
	if (lete_flag .eq. 2 .and. dev_flag .eq. 1) then
		do i = 1, np
			u_spl_dev(i) = (i-1.)/(np-1)*2.*uthk(3) + uthk(1)
		enddo
		call thk_ctrl_gen_out ( (/ A, B, C, D, E /), u, u_spl_dev, np, delu, nseg, 0, out_coord_temp)
		out_coord(:, 7:12) = out_coord_temp(:, 1:6)
	endif	
endif

!!   d2v_mod computation
d2v_mod = 0.
do i = 1, nseg
    tvalid = '00'
    d2v_det = 9.*D(i)**2 - 24.*E(i)*C(i)
    d2v_sign = -1
    if (C(i) .gt. 0.) then
        d2v_sign = 1
    endif
    if (C(i) .eq. 0. .and. D(i) .gt. 0.) then
        d2v_sign = 1
    elseif (C(i) .eq. 0. .and. D(i) .eq. 0. .and. E(i) .gt. 0.) then
        d2v_sign = 1
    endif
    if (d2v_det .lt. 0.) then
        d2v_mod = d2v_mod + (4.*E(i) + 3.*D(i) + 2.*C(i))/delu(i)*d2v_sign
    elseif (d2v_det .ge. 0.) then
        tr(1) = (-3.*D(i) + sqrt(d2v_det))/12./E(i)
        tr(2) = (-3.*D(i) - sqrt(d2v_det))/12./E(i)
        if (tr(2) .lt. tr(1)) then
            temp = tr(1); tr(1) = tr(2); tr(2) = temp
        endif
        if (tr(1) .ge. 0. .and. tr(1) .lt. 1.) then
            if (i .eq. 1 .and. abs(tr(1)) .lt. 1.E-15) then
            else
                tvalid(1:1) = '1'
            endif
        endif
        if (tr(2) .ge. 0. .and. tr(2) .lt. 1. .and. tr(2) .ne. tr(1)) then
            if (i .eq. 1 .and. abs(tr(2)) .lt. 1.E-15) then
            else
                tvalid(2:2) = '1'
            endif
        endif
        if (tvalid .eq. '00') then
            d2v_mod = d2v_mod + (4.*E(i) + 3.*D(i) + 2.*C(i))/delu(i)*d2v_sign
        elseif (tvalid .eq. '10') then
            d2v_mod = d2v_mod + (4.*E(i)*tr(1)**3 + 3.*D(i)*tr(1)**2 + 2.*C(i)*tr(1))/delu(i)*d2v_sign
            d2v_sign = -d2v_sign
            d2v_mod = d2v_mod + (4.*E(i)*(1.-tr(1)**3) + 3.*D(i)*(1.-tr(1)**2) + 2.*C(i)*(1.-tr(1)))/delu(i)*d2v_sign
        elseif (tvalid .eq. '01') then
            d2v_mod = d2v_mod + (4.*E(i)*tr(2)**3 + 3.*D(i)*tr(2)**2 + 2.*C(i)*tr(2))/delu(i)*d2v_sign
            d2v_sign = -d2v_sign
            d2v_mod = d2v_mod + (4.*E(i)*(1.-tr(2)**3) + 3.*D(i)*(1.-tr(2)**2) + 2.*C(i)*(1.-tr(2)))/delu(i)*d2v_sign
        elseif (tvalid .eq. '11') then
            d2v_mod = d2v_mod + (4.*E(i)*tr(1)**3 + 3.*D(i)*tr(1)**2 + 2.*C(i)*tr(1))/delu(i)*d2v_sign
            d2v_sign = -d2v_sign
            d2v_mod = d2v_mod + (4.*E(i)*(tr(2)**3-tr(1)**3) + 3.*D(i)*(tr(2)**2-tr(1)**2) + 2.*C(i)*(tr(2)-tr(1)))/delu(i)*d2v_sign
            d2v_sign = -d2v_sign
            d2v_mod = d2v_mod + (4.*E(i)*(1.-tr(2)**3) + 3.*D(i)*(1.-tr(2)**2) + 2.*C(i)*(1.-tr(2)))/delu(i)*d2v_sign
        endif
    endif
enddo

! if (min_flag .eq. 0) then
    ! F1 = (d2v_mod + (d1v_1 - d1v_0))
! elseif (min_flag .eq. 1) then
    ! F1 = (d2v_mod - (d1v_1 - d1v_0))
! endif
F1 = (d2v_mod - abs(d1v_1 - d1v_0))
F = F1

!!   d3v_mod computation
! if (lete_flag .eq. 0) then
	nseg1 = nseg/2
    d2v_mid = (12.*E(nseg1) + 6.*D(nseg1) + 2.*C(nseg1))/delu(nseg1)**2
    d3v_mod = 0.
    if (min_flag .eq. 1) then
        seg_end = nseg/2
    else
        seg_end = nseg
    endif
    do i = 1, seg_end
        if (D(i) .gt. 0.) then
            d3v_sign = 1
        elseif (D(i) .lt. 0.) then
            d3v_sign = -1
        elseif (D(i) .eq. 0.) then
            if (E(i) .gt. 0.) then
                d3v_sign = 1
            elseif (E(i) .lt. 0.) then
                d3v_sign = -1
            elseif (E(i) .eq. 0.) then
                d3v_sign = 0
            endif
        endif
        tr2 = -D(i)/4./E(i)
        if (tr2 .ge. 0. .and. tr2 .lt. 1.) then
            d3v_mod = d3v_mod + (12.*E(i)*tr2**2 + 6.*D(i)*tr2)/delu(i)**2*d3v_sign
            d3v_sign = -d3v_sign
            d3v_mod = d3v_mod + (12.*E(i)*(1.-tr2**2) + 6.*D(i)*(1.-tr2))/delu(i)**2*d3v_sign
        else
            d3v_mod = d3v_mod + (12.*E(i) + 6.*D(i))/delu(i)**2*d3v_sign
        endif
    enddo
    F2 = d3v_mod - abs(d2v_1 - d2v_0)
	obj_scale = 1.E2
    F = F + F2/obj_scale
! endif

G = 0.
!!   Gradient computation
if (grad_flag .eq. 1) then
    ! dF/dB1
    dd2v_mod_dB1 = 0.
    do i = 1, nseg
        tvalid = '00'
        d2v_det = 9.*D(i)**2 - 24.*E(i)*C(i)
        d2v_sign = -1
        if (C(i) .gt. 0.) then
            d2v_sign = 1
        endif
        if (C(i) .eq. 0. .and. D(i) .gt. 0.) then
            d2v_sign = 1
        elseif (C(i) .eq. 0. .and. D(i) .eq. 0. .and. E(i) .gt. 0.) then
            d2v_sign = 1
        endif
        if (d2v_det .lt. 0.) then
            dd2v_mod_dB1 = dd2v_mod_dB1 + (4.*ddB1(i, 5) + 3.*ddB1(i, 4) + 2.*ddB1(i, 3))/delu(i)*d2v_sign
        elseif (d2v_det .ge. 0.) then
            tr(1) = (-3.*D(i) + sqrt(d2v_det))/12./E(i)
            tr(2) = (-3.*D(i) - sqrt(d2v_det))/12./E(i)
            if (tr(2) .lt. tr(1)) then
                temp = tr(1); tr(1) = tr(2); tr(2) = temp
            endif
            if (tr(1) .ge. 0. .and. tr(1) .lt. 1.) then
                if (i .eq. 1 .and. abs(tr(1)) .lt. 1.E-15) then
                else
                    tvalid(1:1) = '1'
                endif
            endif
            if (tr(2) .ge. 0. .and. tr(2) .lt. 1. .and. tr(2) .ne. tr(1)) then
                if (i .eq. 1 .and. abs(tr(2)) .lt. 1.E-15) then
                else
                    tvalid(2:2) = '1'
                endif
            endif
            if (tvalid .eq. '00') then
                dd2v_mod_dB1 = dd2v_mod_dB1 + (4.*ddB1(i, 5) + 3.*ddB1(i, 4) + 2.*ddB1(i, 3))/delu(i)*d2v_sign
            elseif (tvalid .eq. '10') then
                dd2v_mod_dB1 = dd2v_mod_dB1 + (4.*ddB1(i, 5)*tr(1)**3 + 3.*ddB1(i, 4)*tr(1)**2 + 2.*ddB1(i, 3) * &
                               tr(1))/delu(i)*d2v_sign
                d2v_sign = -d2v_sign
                dd2v_mod_dB1 = dd2v_mod_dB1 + (4.*ddB1(i, 5)*(1.-tr(1)**3) + 3.*ddB1(i, 4)*(1.-tr(1)**2) + 2. *  &
                               ddB1(i, 3)*(1.-tr(1)))/delu(i)*d2v_sign
            elseif (tvalid .eq. '01') then
                dd2v_mod_dB1 = dd2v_mod_dB1 + (4.*ddB1(i, 5)*tr(2)**3 + 3.*ddB1(i, 4)*tr(2)**2 + 2.*ddB1(i, 3) * &
                               tr(2))/delu(i)*d2v_sign
                d2v_sign = -d2v_sign
                dd2v_mod_dB1 = dd2v_mod_dB1 + (4.*ddB1(i, 5)*(1.-tr(2)**3) + 3.*ddB1(i, 4)*(1.-tr(2)**2) + 2. *  &
                               ddB1(i, 3)*(1.-tr(2)))/delu(i)*d2v_sign
            elseif (tvalid .eq. '11') then
                dd2v_mod_dB1 = dd2v_mod_dB1 + (4.*ddB1(i, 5)*tr(1)**3 + 3.*ddB1(i, 4)*tr(1)**2 + 2.*ddB1(i, 3) * &
                               tr(1))/delu(i)*d2v_sign
                d2v_sign = -d2v_sign
                dd2v_mod_dB1 = dd2v_mod_dB1 + (4.*ddB1(i, 5)*(tr(2)**3-tr(1)**3) + 3.*ddB1(i, 4)*(tr(2)**2-tr(1)**2) + &
                               2.*ddB1(i, 3)*(tr(2)-tr(1)))/delu(i)*d2v_sign
                d2v_sign = -d2v_sign
                dd2v_mod_dB1 = dd2v_mod_dB1 + (4.*ddB1(i, 5)*(1.-tr(2)**3) + 3.*ddB1(i, 4)*(1.-tr(2)**2) + 2.*ddB1(i, 3) * &
                               (1.-tr(2)))/delu(i)*d2v_sign
            endif
        endif
    enddo
	
	dRdB1 = (dd2v_mod_dB1*delu(1) - 1.)
    if (lete_flag .eq. 1) then
        dRdB1 = (dd2v_mod_dB1*delu(1) - 2.)
    endif
    G(1) = dRdB1
    
    dd3v_mod_dB1 = 0.
	! if (lete_flag .eq. 0) then
		do i = 1, nseg
			if (d3v_0 .gt. 0.) then
				d3v_sign = 1
			elseif (d3v_0 .lt. 0.) then
				d3v_sign = -1
			elseif (d3v_0 .eq. 0.) then
				if (E(i) .gt. 0.) then
					d3v_sign = 1
				elseif (E(i) .lt. 0.) then
					d3v_sign = -1
				elseif (E(i) .eq. 0.) then
					d3v_sign = 0
				endif
			endif
			tr2 = -D(i)/4./E(i)
			if (tr2 .ge. 0. .and. tr2 .lt. 1.) then
				dd3v_mod_dB1 = dd3v_mod_dB1 + (12.*ddB1(i, 5)*tr2**2 + 6.*ddB1(i, 4)*tr2)/delu(i)**2*d3v_sign
				d3v_sign = -d3v_sign
				dd3v_mod_dB1 = dd3v_mod_dB1 + (12.*ddB1(i, 5)*(1.-tr2**2) + 6.*ddB1(i, 4)*(1.-tr2))/delu(i)**2*d3v_sign
			else
				dd3v_mod_dB1 = dd3v_mod_dB1 + (12.*ddB1(i, 5) + 6.*ddB1(i, 4))/delu(i)**2*d3v_sign
			endif
		enddo
		G(1) = G(1) + dd3v_mod_dB1/obj_scale*delu(1)
    ! endif
    
    ! dF/dC1
	dd2v_mod_dC1 = 0.
	do i = 1, nseg
		tvalid = '00'
		d2v_det = 9.*D(i)**2 - 24.*E(i)*C(i)
		d2v_sign = -1
		if (C(i) .gt. 0.) then
			d2v_sign = 1
		endif
		if (C(i) .eq. 0. .and. D(i) .gt. 0.) then
			d2v_sign = 1
		elseif (C(i) .eq. 0. .and. D(i) .eq. 0. .and. E(i) .gt. 0.) then
			d2v_sign = 1
		endif
		if (d2v_det .lt. 0.) then
			dd2v_mod_dC1 = dd2v_mod_dC1 + (4.*ddC1(i, 5) + 3.*ddC1(i, 4) + 2.*ddC1(i, 3))/delu(i)*d2v_sign
		elseif (d2v_det .ge. 0.) then
			tr(1) = (-3.*D(i) + sqrt(d2v_det))/12./E(i)
			tr(2) = (-3.*D(i) - sqrt(d2v_det))/12./E(i)
			if (tr(2) .lt. tr(1)) then
				temp = tr(1); tr(1) = tr(2); tr(2) = temp
			endif
			if (tr(1) .ge. 0. .and. tr(1) .lt. 1.) then
				if (i .eq. 1 .and. abs(tr(1)) .lt. 1.E-15) then
				else
					tvalid(1:1) = '1'
				endif
			endif
			if (tr(2) .ge. 0. .and. tr(2) .lt. 1. .and. tr(2) .ne. tr(1)) then
				if (i .eq. 1 .and. abs(tr(2)) .lt. 1.E-15) then
				else
					tvalid(2:2) = '1'
				endif
			endif
			if (tvalid .eq. '00') then
				dd2v_mod_dC1 = dd2v_mod_dC1 + (4.*ddC1(i, 5) + 3.*ddC1(i, 4) + 2.*ddC1(i, 3))/delu(i)*d2v_sign
			elseif (tvalid .eq. '10') then
				dd2v_mod_dC1 = dd2v_mod_dC1 + (4.*ddC1(i, 5)*tr(1)**3 + 3.*ddC1(i, 4)*tr(1)**2 + 2.*ddC1(i, 3)*tr(1))/delu(i)*d2v_sign
				d2v_sign = -d2v_sign
				dd2v_mod_dC1 = dd2v_mod_dC1 + (4.*ddC1(i, 5)*(1.-tr(1)**3) + 3.*ddC1(i, 4)*(1.-tr(1)**2) + 2.*ddC1(i, 3) * &
                               (1.-tr(1)))/delu(i)*d2v_sign
			elseif (tvalid .eq. '01') then
				dd2v_mod_dC1 = dd2v_mod_dC1 + (4.*ddC1(i, 5)*tr(2)**3 + 3.*ddC1(i, 4)*tr(2)**2 + 2.*ddC1(i, 3)*tr(2))/delu(i)*d2v_sign
				d2v_sign = -d2v_sign
				dd2v_mod_dC1 = dd2v_mod_dC1 + (4.*ddC1(i, 5)*(1.-tr(2)**3) + 3.*ddC1(i, 4)*(1.-tr(2)**2) + 2.*ddC1(i, 3) * &
                               (1.-tr(2)))/delu(i)*d2v_sign
			elseif (tvalid .eq. '11') then
				dd2v_mod_dC1 = dd2v_mod_dC1 + (4.*ddC1(i, 5)*tr(1)**3 + 3.*ddC1(i, 4)*tr(1)**2 + 2.*ddC1(i, 3)*tr(1))/delu(i)*d2v_sign
				d2v_sign = -d2v_sign
				dd2v_mod_dC1 = dd2v_mod_dC1 + (4.*ddC1(i, 5)*(tr(2)**3-tr(1)**3) + 3.*ddC1(i, 4)*(tr(2)**2-tr(1)**2) + 2. * &
                               ddC1(i, 3)*(tr(2)-tr(1)))/delu(i)*d2v_sign
				d2v_sign = -d2v_sign
				dd2v_mod_dC1 = dd2v_mod_dC1 + (4.*ddC1(i, 5)*(1.-tr(2)**3) + 3.*ddC1(i, 4)*(1.-tr(2)**2) + 2.*ddC1(i, 3) * &
                               (1.-tr(2)))/delu(i)*d2v_sign
			endif
		endif
	enddo
	
	! if (min_flag .eq. 0) then
	dRdC1 = dd2v_mod_dC1*delu(1)**2/2.
	! elseif (min_flag .eq. 1) then
		! dRdC1 = dd2v_mod_dC1*delu(1)**2/2. ! + d1v_0**2/3/d2v_0*2
	! endif
	G(2) = dRdC1
	
	dd3v_mod_dC1 = 0.
	! if (lete_flag .eq. 0) then
		do i = 1, nseg
			if (d3v_0 .gt. 0.) then
				d3v_sign = 1
			elseif (d3v_0 .lt. 0.) then
				d3v_sign = -1
			elseif (d3v_0 .eq. 0.) then
				if (E(i) .gt. 0.) then
					d3v_sign = 1
				elseif (E(i) .lt. 0.) then
					d3v_sign = -1
				elseif (E(i) .eq. 0.) then
					d3v_sign = 0
				endif
			endif
			tr2 = -D(i)/4./E(i)
			if (tr2 .ge. 0. .and. tr2 .lt. 1.) then
				dd3v_mod_dC1 = dd3v_mod_dC1 + (12.*ddC1(i, 5)*tr2**2 + 6.*ddC1(i, 4)*tr2)/delu(i)**2*d3v_sign
				d3v_sign = -d3v_sign
				dd3v_mod_dC1 = dd3v_mod_dC1 + (12.*ddC1(i, 5)*(1.-tr2**2) + 6.*ddC1(i, 4)*(1.-tr2))/delu(i)**2*d3v_sign
			else
				dd3v_mod_dC1 = dd3v_mod_dC1 + (12.*ddC1(i, 5) + 6.*ddC1(i, 4))/delu(i)**2*d3v_sign
			endif
		enddo
		G(2) = G(2) + dd3v_mod_dC1*delu(1)**2/2./obj_scale - 1./obj_scale
	! endif
    
    ! dF/dD1
	dd2v_mod_dD1 = 0.
	do i = 1, nseg
		tvalid = '00'
		d2v_det = 9.*D(i)**2 - 24.*E(i)*C(i)
		d2v_sign = -1
		if (C(i) .gt. 0.) then
			d2v_sign = 1
		endif
		if (C(i) .eq. 0. .and. D(i) .gt. 0.) then
			d2v_sign = 1
		elseif (C(i) .eq. 0. .and. D(i) .eq. 0. .and. E(i) .gt. 0.) then
			d2v_sign = 1
		endif
		if (d2v_det .lt. 0.) then
			dd2v_mod_dD1 = dd2v_mod_dD1 + (4.*ddD1(i, 5) + 3.*ddD1(i, 4) + 2.*ddD1(i, 3))/delu(i)*d2v_sign
		elseif (d2v_det .ge. 0.) then
			tr(1) = (-3.*D(i) + sqrt(d2v_det))/12./E(i)
			tr(2) = (-3.*D(i) - sqrt(d2v_det))/12./E(i)
			if (tr(2) .lt. tr(1)) then
				temp = tr(1); tr(1) = tr(2); tr(2) = temp
			endif
			if (tr(1) .ge. 0. .and. tr(1) .lt. 1.) then
				if (i .eq. 1. .and. abs(tr(1)) .lt. 1.E-15) then
				else
					tvalid(1:1) = '1'
				endif
			endif
			if (tr(2) .ge. 0. .and. tr(2) .lt. 1. .and. tr(2) .ne. tr(1)) then
				if (i .eq. 1 .and. abs(tr(2)) .lt. 1.E-15) then
				else
					tvalid(2:2) = '1'
				endif
			endif
			if (tvalid .eq. '00') then
				dd2v_mod_dD1 = dd2v_mod_dD1 + (4.*ddD1(i, 5) + 3.*ddD1(i, 4) + 2.*ddD1(i, 3))/delu(i)*d2v_sign
			elseif (tvalid .eq. '10') then
				dd2v_mod_dD1 = dd2v_mod_dD1 + (4.*ddD1(i, 5)*tr(1)**3 + 3.*ddD1(i, 4)*tr(1)**2 + 2.*ddD1(i, 3)*tr(1))/delu(i)*d2v_sign
				d2v_sign = -d2v_sign
				dd2v_mod_dD1 = dd2v_mod_dD1 + (4.*ddD1(i, 5)*(1.-tr(1)**3) + 3.*ddD1(i, 4)*(1.-tr(1)**2) + 2.*ddD1(i, 3) * &
                               (1.-tr(1)))/delu(i)*d2v_sign
			elseif (tvalid .eq. '01') then
				dd2v_mod_dD1 = dd2v_mod_dD1 + (4.*ddD1(i, 5)*tr(2)**3 + 3.*ddD1(i, 4)*tr(2)**2 + 2.*ddD1(i, 3)*tr(2))/delu(i)*d2v_sign
				d2v_sign = -d2v_sign
				dd2v_mod_dD1 = dd2v_mod_dD1 + (4.*ddD1(i, 5)*(1.-tr(2)**3) + 3.*ddD1(i, 4)*(1.-tr(2)**2) + 2.*ddD1(i, 3) * &
                               (1.-tr(2)))/delu(i)*d2v_sign
			elseif (tvalid .eq. '11') then
				dd2v_mod_dD1 = dd2v_mod_dD1 + (4.*ddD1(i, 5)*tr(1)**3 + 3.*ddD1(i, 4)*tr(1)**2 + 2.*ddD1(i, 3)*tr(1))/delu(i)*d2v_sign
				d2v_sign = -d2v_sign
				dd2v_mod_dD1 = dd2v_mod_dD1 + (4.*ddD1(i, 5)*(tr(2)**3-tr(1)**3) + 3.*ddD1(i, 4)*(tr(2)**2-tr(1)**2) + 2.* &
                               ddD1(i, 3)*(tr(2)-tr(1)))/delu(i)*d2v_sign
				d2v_sign = -d2v_sign
				dd2v_mod_dD1 = dd2v_mod_dD1 + (4.*ddD1(i, 5)*(1.-tr(2)**3) + 3.*ddD1(i, 4)*(1.-tr(2)**2) + 2.*ddD1(i, 3) * &
                               (1.-tr(2)))/delu(i)*d2v_sign
			endif
		endif
	enddo
	
	dRdD1 = dd2v_mod_dD1*delu(1)**3/6.
	G(3) = dRdD1
	
	dd3v_mod_dD1 = 0.
	! if (lete_flag .eq. 0) then
		do i = 1, nseg
			if (d3v_0 .gt. 0.) then
				d3v_sign = 1
			elseif (d3v_0 .lt. 0.) then
				d3v_sign = -1
			elseif (d3v_0 .eq. 0.) then
				if (E(i) .gt. 0.) then
					d3v_sign = 1
				elseif (E(i) .lt. 0.) then
					d3v_sign = -1
				elseif (E(i) .eq. 0.) then
					d3v_sign = 0
				endif
			endif
			tr2 = -D(i)/4./E(i)
			if (tr2 .ge. 0. .and. tr2 .lt. 1.) then
				dd3v_mod_dD1 = dd3v_mod_dD1 + (12.*ddD1(i, 5)*tr2**2 + 6.*ddD1(i, 4)*tr2)/delu(i)**2*d3v_sign
				d3v_sign = -d3v_sign
				dd3v_mod_dD1 = dd3v_mod_dD1 + (12.*ddD1(i, 5)*(1.-tr2**2) + 6.*ddD1(i, 4)*(1.-tr2))/delu(i)**2*d3v_sign
			else
				dd3v_mod_dD1 = dd3v_mod_dD1 + (12.*ddD1(i, 5) + 6.*ddD1(i, 4))/delu(i)**2*d3v_sign
			endif
		enddo
		G(3) = G(3) + dd3v_mod_dD1*delu(1)**3/6./obj_scale
	! endif
endif

coeff_out(:, 1) = A; coeff_out(:, 2) = B; coeff_out(:, 3) = C
coeff_out(:, 4) = D; coeff_out(:, 5) = E

if (allocated(u)) deallocate(u)
if (allocated(A)) deallocate(A)
if (allocated(B)) deallocate(B)
if (allocated(C)) deallocate(C)
if (allocated(D)) deallocate(D)
if (allocated(E)) deallocate(E)
if (allocated(M)) deallocate(M)
if (allocated(r)) deallocate(r)
if (allocated(M1)) deallocate(M1)
if (allocated(delu)) deallocate(delu)
if (allocated(coeff)) deallocate(coeff)
if (allocated(t_spl)) deallocate(t_spl)
if (allocated(v_spl)) deallocate(v_spl)
if (allocated(d1v_spl)) deallocate(d1v_spl)
if (allocated(d2v_spl)) deallocate(d2v_spl)
if (allocated(d3v_spl)) deallocate(d3v_spl)
if (allocated(d4v_spl)) deallocate(d4v_spl)

end subroutine thk_ctrl_gen_spl


subroutine thk_ctrl_gen_out (coeff, u, u_spl, np, delu, nseg, lete_flag, out_coord)

implicit none

integer, parameter :: sp = kind(0.0), dp = kind(0.0D0)
integer, intent(in) :: np, nseg, lete_flag
real, intent(in) :: coeff(nseg, 5), u_spl(np), &
	delu(nseg), u(nseg+1)
real, intent(out) :: out_coord(np, 12)

integer :: i, j, k, l, p, er
complex(kind = dp) :: root(4)
real :: t, t_spl(np), v_spl(np), d1v_spl(np), d2v_spl(np), d3v_spl(np), d4v_spl(np), &
	A(nseg), B(nseg), C(nseg), D(nseg), E(nseg), d1v_spl_rot(np), d2v_spl_rot(np), &
	d3v_spl_rot(np), d4v_spl_rot(np)

A = coeff(:, 1); B = coeff(:, 2); C = coeff(:, 3); D = coeff(:, 4); E = coeff(:, 5)
d1v_spl = 0.; d2v_spl = 0.; d3v_spl = 0.; d4v_spl = 0.
d1v_spl_rot = 0.; d2v_spl_rot = 0.; d3v_spl_rot = 0.; d4v_spl_rot = 0.
t_spl = 0.
l = 1
if (lete_flag .eq. 0) then
	k = 1
	do i = 1, nseg
		do j = l, np
			if (u_spl(j) .gt. u(i+1)) exit
		enddo
		if (j .eq. l) cycle
		t_spl(l:j-1) = (u_spl(l:j-1) - u(i))/(u(i+1)-u(i))
		v_spl(l:j-1) = E(i)*t_spl(l:j-1)**4 + D(i)*t_spl(l:j-1)**3 + C(i)*t_spl(l:j-1)**2 + B(i)*t_spl(l:j-1) + A(i)
		d1v_spl(l:j-1) = (4.*E(i)*t_spl(l:j-1)**3 + 3.*D(i)*t_spl(l:j-1)**2 + 2.*C(i)*t_spl(l:j-1) + B(i))/delu(i)
		d2v_spl(l:j-1) = (12.*E(i)*t_spl(l:j-1)**2 + 6.*D(i)*t_spl(l:j-1) + 2.*C(i))/delu(i)**2
		d3v_spl(l:j-1) = (24.*E(i)*t_spl(l:j-1) + 6.*D(i))/delu(i)**3
		d4v_spl(l:j-1) = 24.*E(i)/delu(i)**3
		l = j
	enddo
elseif (lete_flag .eq. 1) then
	do i = nseg/2, 1, -1
		do j = l, np
			if (u_spl(j) .gt. A(i)) exit
		enddo	
		if (j .eq. l) cycle
		do k = l, j-1
			if (abs(A(i)-u_spl(k)) .lt. 1e-9) then
				t_spl(k) = 0.
			elseif (abs(A(i)+B(i)+C(i)+D(i)+E(i)-u_spl(k)) .lt. 1e-9) then
				t_spl(k) = 1.
			else
				call quartic_roots (real((/ A(i)-u_spl(k), B(i), C(i), D(i), E(i) /), dp), er, root)
				do p = 1, 4
					if (aimag(root(p)) .eq. 0.0D0) then
						if (dble(root(p)) .le. 1.0D0 .and. dble(root(p)) .ge. 0.0D0) then
							t_spl(k) = dble(root(p))
							exit
						endif
					endif
					if (p .eq. 4) then
						print*, 'LE Root not found, Possible values of t are: ', &
						dble(root(1)), '+i', aimag(root(1)), dble(root(2)), '+i', aimag(root(2)), &
						dble(root(3)), '+i', aimag(root(3)), dble(root(4)), '+i', aimag(root(4))					
						print*, 'LE Root not found, Coeeficients are: ', real((/ A(i)-u_spl(k), B(i), C(i), D(i), E(i) /), dp), &
                                                                         abs(A(i)+B(i)+C(i)+D(i)+E(i)-u_spl(k))
						print*, 'LE Root not found at ', u_spl(k)
					endif
				enddo
			endif
			v_spl(k) = - (t_spl(k)*(u(i+1)-u(i)) + u(i))
			if (t_spl(k) .lt. 0.) print*, 'LE: Negative t: ', t, ' Segment: ', i, 'Point number: ', k
			if (t_spl(k) .gt. 1.) print*, 'LE: Greater than 1 t: ', t, ' Segment: ', i, 'Point number: ', k
			d1v_spl_rot(k) = (4.*E(i)*t_spl(k)**3 + 3.*D(i)*t_spl(k)**2 + 2.*C(i)*t_spl(k) + B(i))/delu(i)
			d1v_spl(k) = -1./d1v_spl_rot(k)
			d2v_spl_rot(k) = (12.*E(i)*t_spl(k)**2 + 6.*D(i)*t_spl(k) + 2.*C(i))/delu(i)**2
			d2v_spl(k) = d2v_spl_rot(k)/d1v_spl_rot(k)**3
			d3v_spl_rot(k) = (24.*E(i)*t_spl(k) + 6.*D(i))/delu(i)**3
			d3v_spl(k) = (d3v_spl_rot(k)*d1v_spl(k)**5 + 3.*d2v_spl(k)**2)/d1v_spl(k)	
			! if (k .eq. 1) then
				! d1v_spl(k) = 0.; d2v_spl(k) = 0.; d3v_spl(k) = 0.
			! endif		
		enddo
		l = j
	enddo
elseif (lete_flag .eq. 2) then
	do i = 1, nseg/2
		do j = l, np
			if (u_spl(j) .gt. A(i)+B(i)+C(i)+D(i)+E(i) .and. i .lt. nseg/2) exit
		enddo
		if (j .eq. l) cycle
		do k = l, j-1
			if (abs(A(i)-u_spl(k)) .lt. 1e-9) then
				t_spl(k) = 0.
			elseif (abs(A(i)+B(i)+C(i)+D(i)+E(i)-u_spl(k)) .lt. 1e-9) then
				t_spl(k) = 1.
			else
				call quartic_roots (real((/ A(i)-u_spl(k), B(i), C(i), D(i), E(i) /), dp), er, root)
				do p = 1, 4
					if (aimag(root(p)) .eq. 0.0D0) then
						! print*, 'Res :   ', A(i)-u_spl(k) + B(i)*root(p) + C(i)*root(p)**2 +  D(i)*root(p)**3 +  E(i)*root(p)**4, 'Root ', p
						if (dble(root(p)) .le. 1.0D0 .and. dble(root(p)) .ge. 0.0D0) then
							t_spl(k) = dble(root(p))
							exit
						endif
					endif
					if (p .eq. 4) then
						print*, 'TE Root not found, Possible values of t are: ', &
						dble(root(1)), '+i', aimag(root(1)), dble(root(2)), '+i', aimag(root(2)), &
						dble(root(3)), '+i', aimag(root(3)), dble(root(4)), '+i', aimag(root(4))
						print*, 'TE Root not found, Coeeficients are: ', real((/ A(i)-u_spl(k), B(i), C(i), D(i), E(i) /), dp)
						print*, 'TE Root not found at ', u_spl(k)						
					endif
				enddo
			endif
			v_spl(k) = - (t_spl(k)*(u(i+1)-u(i)) + u(i))
			if (t_spl(k) .lt. 0.) then
				t_spl(k) = 0.; v_spl(k) = 0.
				print*, 'TE: Negative t: ', t, ' Segment: ', i, 'Point number: ', k
			endif
			d1v_spl_rot(k) = (4.*E(i)*t_spl(k)**3 + 3.*D(i)*t_spl(k)**2 + 2.*C(i)*t_spl(k) + B(i))/delu(i)
			d1v_spl(k) = -1./d1v_spl_rot(k)
			d2v_spl_rot(k) = (12.*E(i)*t_spl(k)**2 + 6.*D(i)*t_spl(k) + 2.*C(i))/delu(i)**2
			d2v_spl(k) = - d2v_spl_rot(k)*d1v_spl(k)**3
			d3v_spl_rot(k) = (24.*E(i)*t_spl(k) + 6.*D(i))/delu(i)**3
			d3v_spl(k) = (d3v_spl_rot(k)*d1v_spl(k)**5 + 3.*d2v_spl(k)**2)/d1v_spl(k)	
			! if (k .eq. 1) then
				! d1v_spl(k) = 0.; d2v_spl(k) = 0.; d3v_spl(k) = 0.
			! endif
		enddo
		l = j
	enddo
endif

out_coord = 0.
if (lete_flag .eq. 0) then
	out_coord(:, 1) = u_spl
	out_coord(:, 2) = v_spl
	out_coord(:, 3) = atan(d1v_spl)
	out_coord(:, 4) = d2v_spl
	out_coord(:, 5) = d3v_spl	
elseif (lete_flag .eq. 1 .or. lete_flag .eq. 2) then
	out_coord(:, 1) = u_spl
	out_coord(:, 2) = v_spl
	out_coord(:, 3) = atan(d1v_spl)
endif
! out_coord(:, 6) = d2v_spl/(1.+d1v_spl**2)**1.5
! out_coord(:, 7) = atan(d1v_spl_rot)
! out_coord(:, 8) = d2v_spl_rot
! out_coord(:, 9) = d3v_spl_rot
! out_coord(:, 10) = d2v_spl_rot/(1.+d1v_spl_rot**2)**1.5

end subroutine thk_ctrl_gen_out
