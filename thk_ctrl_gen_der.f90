subroutine thk_ctrl_gen_Eder_fn(uthk, thk, u, n, nknt, iknt, niknt, ik, min_flag, dAdE, dBdE, dCdE, dDdE, dEdE)

implicit none

integer, intent(in) :: n, nknt, niknt, iknt(niknt), ik, min_flag
real, intent(in) :: uthk(n), thk(n), u(nknt)
real, intent(inout) ::  dAdE(nknt-1, niknt), dBdE(nknt-1, niknt), dCdE(nknt-1, niknt), dDdE(nknt-1, niknt), dEdE(nknt-1, niknt)

integer :: i, j, k, nun, nseg, thkm_i, iknt_der, i_iknt, row, col, cseg, ip, fail_flag, nmax, nmin
integer, allocatable, dimension(:) :: imin, imax
real :: u_thkm, t
real, allocatable, dimension(:) :: r, delu, ddE
real, allocatable, dimension(:, :) :: M, A

iknt_der = iknt(ik)
nseg = nknt-1
nun = 5*nseg-4-niknt
Allocate(ddE(nun))
if (min_flag .eq. 0 .or. min_flag .eq. 10) then
	thkm_i = maxloc(thk, 1)
	u_thkm = uthk(thkm_i)
elseif (min_flag .eq. 1 .or. min_flag .eq. 11) then
	thkm_i = minloc(thk, 1)
	u_thkm = uthk(thkm_i)
endif
nmax = 0; nmin = 0
do i = 2, n-1
	if (thk(i) .ge. thk(i-1) .and. thk(i) .ge. thk(i+1)) then
		nmax = nmax+1
	elseif (thk(i) .le. thk(i-1) .and. thk(i) .le. thk(i+1)) then
		nmin = nmin+1
	endif
enddo
if (allocated(imin)) deallocate(imin)
if (nmin .gt. 0) then
	Allocate(imin(nmin))
else
	Allocate(imin(1))
endif
if (allocated(imax)) deallocate(imax)
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

Allocate(M(nun, nun))
Allocate(r(nun))
Allocate(A(nun, nun+1))
Allocate(delu(nknt-1))

M = 0.
r = 0.
delu = u(2:nknt) - u(1:nknt-1)

!	C0 continuity
M(1, 1:2) = (/ 1., -1. /)
col = 2
i_iknt = 1
do row = 2, nknt-2
	if (i_iknt .le. niknt) then
		if (row .eq. iknt(i_iknt)) then
			M(row, col:col+4) = (/ 1., 1., 1., 1., -1. /)
			if (row .eq. iknt_der) then
				r(row) = -1.
			else
				r(row) = 0.
			endif
			i_iknt = i_iknt + 1
			col = col + 4
			cycle
		endif
	endif
	M(row, col:col+5) = (/ 1., 1., 1., 1., 1., -1. /)
	col = col + 5
enddo

!	C1 continuity
M(nknt-1, 1:3) = (/ 4., 0., -delu(1)/delu(2) /)
col = 2
i = 2
i_iknt = 1
cseg = 2
do row = nknt, 2*(nknt-2)
    if (i_iknt .le. niknt) then
		if (cseg .eq. iknt(i_iknt)) then
			M(row, col:col+5) = (/ 0., 1., 2., 3., 0., -delu(i)/delu(i+1) /)
			if (i .eq. iknt_der) then
				r(row) = -4.
			else
				r(row) = 0.
			endif
			i_iknt = i_iknt + 1
			col = col + 4
			i = i+1
			cseg = cseg+1			
			cycle
		endif
	endif
	M(row, col:col+6) = (/ 0., 1., 2., 3., 4., 0., -delu(i)/delu(i+1) /)
	col = col + 5
    i = i+1
    cseg = cseg+1
enddo

!	C2 continuity
M(2*(nknt-2)+1, 1:4) = (/ 6., 0., 0., -(delu(1)/delu(2))**2 /)
col = 2
i = 2
i_iknt = 1
cseg = 2
do row = 2*(nknt-2)+2, 3*(nknt-2)
    if (i_iknt .le. niknt) then
		if (cseg .eq. iknt(i_iknt)) then
			M(row, col:col+6) = (/ 0., 0., 1., 3., 0., 0., -(delu(i)/delu(i+1))**2 /)
			if (i .eq. iknt_der) then
				r(row) = -6.
			else
				r(row) = 0.
			endif
			i_iknt = i_iknt + 1
			col = col + 4
			i = i+1
			cseg = cseg+1
			cycle
		endif
	endif
	M(row, col:col+7) = (/ 0., 0., 1., 3., 6., 0., 0., -(delu(i)/delu(i+1))**2 /)
	col = col + 5
    i = i+1
    cseg = cseg+1
enddo

!	C3 continuity
M(3*(nknt-2)+1, 1:5) = (/ 4., 0., 0., 0., -(delu(1)/delu(2))**3 /)
r(3*(nknt-2)+1) = -dDdE(1, ik)
col = 2
i = 2
i_iknt = 1
cseg = 2
do row = 3*(nknt-2)+2, 4*(nknt-2)
    if (i_iknt .le. niknt) then
		if (cseg .eq. iknt(i_iknt)) then
			M(row, col:col+7) = (/ 0., 0., 0., 1., 0., 0., 0., -(delu(i)/delu(i+1))**3 /)
			if (i .eq. iknt_der) then
				r(row) = -4.
			else
				r(row) = 0.
			endif
			i_iknt = i_iknt + 1
			col = col + 4
			i = i+1
			cseg = cseg+1			
			cycle
		endif
	endif
	M(row, col:col+8) = (/ 0., 0., 0., 1., 4., 0., 0., 0., -(delu(i)/delu(i+1))**3 /)
	col = col + 5
    i = i+1
    cseg = cseg+1
enddo

!	Thickness values
row = 4*(nknt-2)
do i = 2, n-1
    do j = 1, nseg
        if (uthk(i) .ge. u(j) .and. uthk(i) .lt. u(j+1)) then
            row = row + 1
            t = (uthk(i)-u(j))/(u(j+1)-u(j))
            i_iknt = 0
            do k = 1, niknt
                if (u(j) .gt. u(iknt(k))) then
                    i_iknt = i_iknt+1
                else
                    exit
                endif
            enddo
            col = (j-2)*5 + 2 - i_iknt
            if (i_iknt .ne. niknt) then
				if (u(j) .eq. u(iknt(i_iknt+1))) then
					M(row, col:col+3) = (/ 1., t, t**2, t**3 /)
					if (j .eq. iknt_der) then
						r(row) = -t**4
					else
						r(row) = 0
					endif
					exit
				endif
			endif
			M(row, col:col+4) = (/ 1., t, t**2, t**3, t**4 /)
			exit
        endif
    enddo
enddo

! if (min_flag .ne. 10 .and. min_flag .ne. 11) then
! !	Slope at maximum thickness
! row = row+1
! do j = 1, nseg
    ! if (u_thkm .ge. u(j) .and. u_thkm .lt. u(j+1)) then
        ! t = (u_thkm-u(j))/(u(j+1)-u(j))
        ! i_iknt = 0
        ! do k = 1, niknt
            ! if (u(j) .gt. u(iknt(k))) then
                ! i_iknt = i_iknt+1
            ! else
                ! exit
            ! endif
        ! enddo
        ! col = (j-2)*5 + 2 - i_iknt
        ! if (i_iknt .ne. niknt .and. uthk(i) .eq. u(iknt(i_iknt+1))) then
            ! M(row, col:col+3) = (/ 0., 1., 2.*t, 3.*t**2 /)
            ! if (j .eq. iknt_der) then
                ! r(row) = -4.*t**3
            ! else
                ! r(row) = 0.
            ! endif
            ! exit
        ! else
            ! M(row, col:col+4) = (/ 0., 1., 2.*t, 3.*t**2, 4.*t**3 /)
            ! exit
        ! endif
    ! endif
! enddo
! endif

!	Slope at maximum thickness
! if (min_flag .ne. 10 .and. min_flag .ne. 11) then
if (nmax .gt. 0) then
	do i = 1, nmax
	row = row+1
	do j = 1, nseg
		if (uthk(imax(i)) .ge. u(j) .and. uthk(imax(i)) .lt. u(j+1)) then
			t = (uthk(imax(i))-u(j))/(u(j+1)-u(j))
			i_iknt = 0
			do k = 1, niknt
				if (u(j) .gt. u(iknt(k))) then
					i_iknt = i_iknt+1
				else
					exit
				endif
			enddo
			col = (j-2)*5 + 2 - i_iknt
			if (i_iknt .ne. niknt .and. uthk(i) .eq. u(iknt(i_iknt+1))) then
				M(row, col:col+3) = (/ 0., 1., 2.*t, 3.*t**2 /)
				if (j .eq. iknt_der) then
					r(row) = -4.*t**3
				else
					r(row) = 0.
				endif
				exit
			else
				M(row, col:col+4) = (/ 0., 1., 2.*t, 3.*t**2, 4.*t**3 /)
				exit
			endif
		endif
	enddo
	enddo
endif
if (nmin .gt. 0) then
	do i = 1, nmin
	row = row+1
	do j = 1, nseg
		if (uthk(imin(i)) .ge. u(j) .and. uthk(imin(i)) .lt. u(j+1)) then
			t = (uthk(imin(i))-u(j))/(u(j+1)-u(j))
			i_iknt = 0
			do k = 1, niknt
				if (u(j) .gt. u(iknt(k))) then
					i_iknt = i_iknt+1
				else
					exit
				endif
			enddo
			col = (j-2)*5 + 2 - i_iknt
			if (i_iknt .ne. niknt .and. uthk(i) .eq. u(iknt(i_iknt+1))) then
				M(row, col:col+3) = (/ 0., 1., 2.*t, 3.*t**2 /)
				if (j .eq. iknt_der) then
					r(row) = -4.*t**3
				else
					r(row) = 0.
				endif
				exit
			else
				M(row, col:col+4) = (/ 0., 1., 2.*t, 3.*t**2, 4.*t**3 /)
				exit
			endif
		endif
	enddo
	enddo
endif
! endif

!	TE conditions
!TE thickness
row = row+1
if (u(nknt-1) .eq. u(iknt(niknt))) then
    col = nun-3
    M(row, col:col+3) = (/ 1., 1., 1., 1. /)
    if (iknt_der .eq. iknt(niknt)) then
        r(row) = -1
    endif
else
    col = nun-4
    M(row, col:col+4) = (/ 1., 1., 1., 1., 1. /)
endif
!TE slope
row = row+1
if (u(nknt-1) .eq. u(iknt(niknt))) then
    col = nun-3
    M(row, col:col+3) = (/ 0., 1., 2., 3. /)
    if (iknt_der .eq. iknt(niknt)) then
        r(row) = -4.
    endif
else
    col = nun-4
    M(row, col:col+4) = (/ 0., 1., 2., 3., 4. /)
endif
!TE second derivative
row = row+1
if (u(nknt-1) .eq. u(iknt(niknt))) then
    col = nun-3
    M(row, col:col+3) = (/ 0., 0., 2., 6. /)
    if (iknt_der .eq. iknt(niknt)) then
        r(row) = -12.
    endif
else
    col = nun-4
    M(row, col:col+4) = (/ 0., 0., 2., 6., 12. /)
endif
!TE third derivative
row = row+1
if (u(nknt-1) .eq. u(iknt(niknt))) then
    col = nun-3
    M(row, col:col+3) = (/ 0., 0., 0., 6. /)
    if (iknt_der .eq. iknt(niknt)) then
        r(row) = -24.
    endif
else
    col = nun-4
    M(row, col:col+4) = (/ 0., 0., 0., 6., 24. /)
endif

!	Solving for spline coefficients
A(:, 1:nun) = M
A(:, nun+1) = r
do i = 1, nun
	do j = 1, nun
		if (M(i, j) /= M(i, j)) then
			print*, 'thk_ctrl_gen_Eder_fn Error: Nan value in M matrix', i, j
			return
		endif
	enddo
	if (r(i) /= r(i)) then
		print*, 'thk_ctrl_gen_Eder_fn Error: Nan value in r'
		return
	endif
enddo
call gauss_jordan(nun, 1, A, fail_flag)
if (fail_flag .eq. 1) then
     write (*, *) 'Failed to solve linear system: thk_ctrl_gen_Eder_fn'
	 return
endif
ddE = A(:, nun+1)
do i = 1, nun
	if (ddE(i) /= ddE(i)) then
		print*, 'thk_ctrl_gen_1der_fn Error: Nan value in solution to linear system'
		return
	endif
enddo
dEdE(1, ik) = ddE(1)
j = 1
i = 2
k = 2
do while (k .le. nseg)
    if (j .gt. niknt) then
	  dAdE(k, ik) = ddE(i)
	  dBdE(k, ik) = ddE(i+1)
	  dCdE(k, ik) = ddE(i+2)
	  dDdE(k, ik) = ddE(i+3)
	  dEdE(k, ik) = ddE(i+4)
	  ip = 5
    elseif (k .eq. iknt(j)) then
	  dEdE(k, ik) = 0.
	  dAdE(k, ik) = ddE(i)
	  dBdE(k, ik) = ddE(i+1)
	  dCdE(k, ik) = ddE(i+2)
	  dDdE(k, ik) = ddE(i+3)
	  j = j+1
	  ip = 4
    else
        dAdE(k, ik) = ddE(i)
        dBdE(k, ik) = ddE(i+1)
        dCdE(k, ik) = ddE(i+2)
        dDdE(k, ik) = ddE(i+3)
        dEdE(k, ik) = ddE(i+4)
        ip = 5
    endif
    i = i+ip
    k = k+1
enddo
dEdE(iknt_der, ik) = 1.

if (allocated(ddE)) deallocate(ddE)
if (allocated(M)) deallocate(M)
if (allocated(r)) deallocate(r)
if (allocated(A)) deallocate(A)
if (allocated(delu)) deallocate(delu)

end subroutine thk_ctrl_gen_Eder_fn



subroutine thk_ctrl_gen_1der_fn(uthk, thk, u, n, nknt, iknt, niknt, dCdE, dDdE, dEdE, dvDte_dvDle, &
	d3v_ang_flag, der_flag, min_flag, d1_flag, dxdB1, dxdC1, dxdD1, dAd1, dBd1, dCd1, dDd1, dEd1)

implicit none

integer, intent(in) :: n, nknt, niknt, iknt(niknt), d3v_ang_flag, der_flag, min_flag, d1_flag
real, intent(in) :: uthk(n), thk(n), u(nknt), dvDte_dvDle, dCdE(nknt-1, niknt), &
	dDdE(nknt-1, niknt), dEdE(nknt-1, niknt), dxdB1(nknt-1, 5), dxdC1(nknt-1, 5), dxdD1(nknt-1, 5)
real, intent(out) ::  dAd1(nknt-1), dBd1(nknt-1), dCd1(nknt-1), dDd1(nknt-1), dEd1(nknt-1)

integer :: i, j, k, nun, nseg, thkm_i, row, col, fail_flag, row_Emin, row_D1min, seg_end, nmax, nmin
integer, allocatable, dimension(:) :: imin, imax
real :: u_thkm, t
real, allocatable, dimension(:) :: r, delu, dd1
real, allocatable, dimension(:, :) :: M, A

nmax = 0; nmin = 0
do i = 2, n-1
	if (thk(i) .ge. thk(i-1) .and. thk(i) .ge. thk(i+1)) then
		nmax = nmax+1
	elseif (thk(i) .le. thk(i-1) .and. thk(i) .le. thk(i+1)) then
		nmin = nmin+1
	endif
enddo
if (allocated(imin)) deallocate(imin)
if (nmin .gt. 0) then
	Allocate(imin(nmin))
else
	Allocate(imin(1))
endif
if (allocated(imax)) deallocate(imax)
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
nseg = nknt-1
nun = 5*nseg-4
if (d1_flag .eq. 1 .and. der_flag .eq. 2) nun = nun + 1
Allocate(dd1(nun))
if (min_flag .eq. 0 .or. min_flag .eq. 10) then
	thkm_i = maxloc(thk, 1)
elseif (min_flag .eq. 1 .or. min_flag .eq. 11) then
	thkm_i = minloc(thk, 1)
endif
u_thkm = uthk(thkm_i)

Allocate(M(nun, nun))
Allocate(r(nun))
Allocate(A(nun, nun+1))
Allocate(delu(nknt-1))

M = 0.
r = 0.
delu = u(2:nknt) - u(1:nknt-1)

if (der_flag .eq. 1) then
    dBd1(1) = 1.
    ! if (min_flag .eq. 1) then
	! if (lete_flag .eq. 1) then
		! dCd1(1) = delu(1)*d2v_0/d1v_0
		! dDd1(1) = delu(1)**2/3.*(d3v_0*d1v_0**3 + 2.*d2v_0**2*d1v_0**3 - 4.*d2v_0**2*d1v_0**3)
        !dCd1(1) = 3*d2v_0/d1v_0**4*delu(1)/2
        !dDd1(1) = (15*d2v_0**2/d1v_0**4 - 4*d3v_0/d1v_0**3)*delu(1)**2/6
    ! endif
elseif (der_flag .eq. 2) then
    dCd1(1) = 1.
    if (min_flag .eq. 1) then
        !dBd1(1) = 2*d1v_0**4/3/d2v_0/delu(1)
        !dDd1(1) = 2*d2v_0/d1v_0**2*delu(1)
    endif
elseif (der_flag .eq. 3) then
    dDd1(1) = 1.
endif

!!      C0 continuity
M(1, 1:2) = (/ 1., -1. /)
r(1) = - dAd1(1) - dBd1(1) - dCd1(1) - dDd1(1)
if (d1_flag .eq. 1 .and. der_flag .eq. 2) then
	M(1, nun) = 1.
	r(1) = - dAd1(1) - dBd1(1) - dDd1(1)
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
r(row) = - dBd1(1) - 2.*dCd1(1) - 3.*dDd1(1)
if (d1_flag .eq. 1 .and. der_flag .eq. 2) then
	M(row, nun) = 3.
	r(row) = - dBd1(1) - 2.*dCd1(1)
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
r(row) = - dCd1(1) - 3.*dDd1(1)
if (d1_flag .eq. 1 .and. der_flag .eq. 2) then
	M(row, nun) = 3.
	r(row) = - dCd1(1)
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
r(row) = -dDd1(1)
if (d1_flag .eq. 1 .and. der_flag .eq. 2) then
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
            exit
        endif
    enddo
enddo

!!      Slope at maximum/minimum thickness
! if (min_flag .ne. 10 .and. min_flag .ne. 11) then
! do j = 1, nseg
    ! if (u_thkm .gt. u(j) .and. u_thkm .le. u(j+1)) then
        ! t = (u_thkm-u(j))/(u(j+1)-u(j))
        ! col = (j-2)*5 + 2
        ! row = row + 1
        ! M(row, col:col+4) = (/ 0., 1., 2*t, 3*t**2, 4*t**3 /)
        ! exit
    ! endif
! enddo
! endif

! if (min_flag .ne. 10 .and. min_flag .ne. 11) then
if (nmax .gt. 0) then
	do i = 1, nmax
		do j = 1, nseg
			if (uthk(imax(i)) .gt. u(j) .and. uthk(imax(i)) .le. u(j+1)) then
				t = (uthk(imax(i))-u(j))/(u(j+1)-u(j))
				col = (j-2)*5 + 2
				row = row + 1
				M(row, col:col+4) = (/ 0., 1., 2*t, 3*t**2, 4*t**3 /)
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
				M(row, col:col+4) = (/ 0., 1., 2*t, 3*t**2, 4*t**3 /)
				exit
			endif
		enddo
	enddo
endif

!!      TE conditions
col = nun-4
if (d1_flag .eq. 1 .and. der_flag .eq. 2) col = nun-5
!       TE thickness
row = row+1
M(row, col:col+4) = (/ 1., 1., 1., 1., 1. /)
!       TE slope
row = row+1
M(row, col:col+4) = (/ 0., 1., 2., 3., 4. /)
if (der_flag .eq. 1) then
    r(row) = dvDte_dvDle/delu(1)*delu(nknt-1)
elseif (der_flag .eq. 2 .and. min_flag .eq. 1) then
    !r(row) = 2*d1v_0**4/3/d2v_0/delu(1)**2/2*delu(nknt-1)
endif
!       TE second derivative
row = row+1
M(row, col:col+4) = (/ 0., 0., 2., 6., 12. /)
if (der_flag .eq. 2) then
    r(row) = 2.*dvDte_dvDle/delu(1)**2*delu(nknt-1)**2
endif
!       TE third derivative
row = row+1
M(row, col:col+4) = (/ 0., 0., 0., 6., 24. /)
if (der_flag .eq. 3) then
    r(row) = 6.*dvDte_dvDle/delu(1)**3*delu(nknt-1)**3
elseif (der_flag .eq. 2 .and. min_flag .eq. 1) then
    !r(row) = 12*d2v_0/d1v_0**2/delu(1)**2
endif

!! Minimization conditions for E coefficients
row_Emin = row
! Minimization of d2v_sq only
do i = 1, niknt
    row = row + 1
    M(row, 1) = 288./5./delu(1)**3*dEdE(1, i)
	if (d1_flag .eq. 1 .and. der_flag .eq. 2) then
		M(row, nun) = M(row, nun) + 1./delu(1)**3*36.*dEdE(1, i)
		r(row) = - 1./delu(1)**3*16.*dEdE(1, i)
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
		if (d1_flag .eq. 1 .and. der_flag .eq. 2) then
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

!! Minimization conditions for D1
if (d1_flag .eq. 1 .and. der_flag .eq. 2) then
	row_D1min = row
	! Minimization of d2v_sq only
	row = row + 1
	M(row, 1) = 288./5./delu(1)**3*dxdD1(1, 5) + 36./delu(1)**3
	M(row, nun) = 1./delu(1)**3*(36.*dxdD1(1, 5) +24.)
	r(row) = - 1/delu(1)**3*(16.*dxdD1(1, 5) +12.)
	col = 2
	do j = 2, nseg
		M(row, col:col+4) = (/ 0., 0., 16.*dxdD1(j, 5)+12.*dxdD1(j, 4)+8.*dxdD1(j, 3), &
			36.*dxdD1(j, 5)+24.*dxdD1(j, 4)+12.*dxdD1(j, 3), &
			288./5.*dxdD1(j, 5)+36.*dxdD1(j, 4)+16.*dxdD1(j, 3) /) / delu(j)**3
		col = col + 5
	enddo
endif

! ! Minimization of d3v_sq
if (d1_flag .eq. 1 .and. der_flag .eq. 2) then
	row = row_D1min
	row = row + 1
	M(row, 1) = M(row, 1) + (16.*dxdD1(1, 5) + 6.) /delu(1)**5*delu(1)**1
	M(row, nun) = M(row, nun) + (6.*dxdD1(1, 5) + 3.)/delu(1)**5*delu(1)**1
	col = 2
	do j = 2, nseg
		M(row, col:col+4) = M(row, col:col+4) + (/ 0., 0., 0., &
			6.*dxdD1(j, 5) + 3.*dxdD1(j, 4), &
			16.*dxdD1(j, 5) + 6.*dxdD1(j, 4) /) / delu(j)**5*delu(1)**1
		col = col + 5
	enddo
endif

! Minimization of d3v/du3 angle change squared only (excluding end segments)
! if (d3v_ang_flag .eq. 1) then
    ! row = row_Emin
    ! do i = 1, niknt
        ! row = row+1
        ! seg_end = nseg-2
        ! if (min_flag .eq. 1) then
            ! M(row, 1) = M(row, 1) + 2*24**2*(dEdE(1, i)/delu(1)**4 - dEdE(2, i)/delu(2)**4)
! !             M(row, 6) = M(row, 6) + 2*24**2*(dEdE(2, i)/delu(2)**4 - dEdE(1, i)/delu(1)**4)
            ! seg_end = nseg-1
        ! endif
        ! col = 2
        ! do j = 2, seg_end
            ! M(row, col:col+9) = M(row, col:col+9) + (/ 0., 0., 0., 0., &
                ! 2.*24.**2*(dEdE(j, i)/delu(j)**4 - dEdE(j+1, i)/delu(j+1)**4), &
                ! 0., 0., 0., 0., &
                ! 2.*24.**2*(dEdE(j+1, i)/delu(j+1)**4 - dEdE(j, i)/delu(j)**4) /)
            ! col = col + 5
        ! enddo
    ! enddo
! endif

! ! Minimization of d3v_sq
! if (min_flag .eq. 1) then
!     row = row_Emin
!     mid_knt = nseg/2 + 1
!     do i = 1, niknt
!         row = row + 1
!         if (iknt(i) .lt. mid_knt) then
!             col = 2
!             M(row, 1) = M(row, 1) + 36.*(32./3.*dEdE(1, i) + 4.*dDdE(1, i))!/delu(1)**5
!             r(row) = r(row) - dDd1(1)*36.*(4.*dEdE(1, i) + 2.*dDdE(1, i))!/delu(1)**5
!             do j = 2, mid_knt-1
!                 M(row, col : col+4) = M(row, col : col+4) + (/ 0., 0., 0., &
!                     36./delu(j)**5*(4.*dEdE(j, i) + 2.*dDdE(j, i)), &
!                     36./delu(j)**5*(32./3.*dEdE(j, i) + 4.*dDdE(j, i)) /)*delu(j)**5
!                 col = col + 5
!             enddo
!         elseif (iknt(i) .gt. mid_knt) then
!             col = (nseg/2 - 1)*5 + 2
!             do j = mid_knt, nseg
!                 M(row, col : col+4) = M(row, col : col+4) + (/ 0., 0., 0., &
!                     36./delu(j)**5*(4.*dEdE(j, i) + 2.*dDdE(j, i)), &
!                     36./delu(j)**5*(32./3.*dEdE(j, i) + 4.*dDdE(j, i)) /)*delu(j)**5
!                 col = col + 5
!             enddo
!         endif
!     enddo
! endif

! ! Minimization of d3v_sq
! if (min_flag .eq. 0) then
!     row = row_Emin
!     do i = 1, niknt
!         row = row + 1
!         col = 2
!         M(row, 1) = M(row, 1) + 36.*(32./3.*dEdE(1, i) + 4.*dDdE(1, i))!/delu(1)**5
!         r(row) = r(row) - dDd1(1)*36.*(4.*dEdE(1, i) + 2.*dDdE(1, i))!/delu(1)**5
!         do j = 2, nseg
!             M(row, col : col+4) = M(row, col : col+4) + (/ 0., 0., 0., &
!                 36./delu(j)**5*(4.*dEdE(j, i) + 2.*dDdE(j, i)), &
!                 36./delu(j)**5*(32./3.*dEdE(j, i) + 4.*dDdE(j, i)) /)*delu(j)**5
!             col = col + 5
!         enddo
!     enddo
! endif

!!      Solving for spline coefficients
! do i = 1, nun
!     r(i) = r(i)/maxval(M(i, :))
!     M(i, :) = M(i, :)/maxval(M(i, :))
! enddo
A(:, 1:nun) = M
A(:, nun+1) = r
do i = 1, nun
	do j = 1, nun
		if (M(i, j) /= M(i, j)) then
			print*, 'thk_ctrl_gen_1der_fn Error: Nan value in M matrix', i, j
			return
		endif
	enddo
	if (r(i) /= r(i)) then
		print*, 'thk_ctrl_gen_1der_fn Error: Nan value in r'
		return
	endif
enddo
call gauss_jordan(nun, 1, A, fail_flag)
if (fail_flag .eq. 1) then
    write (*, *) 'Failed to solve linear system: thk_ctrl_1der_fn'
	print*, der_flag
	return
endif
dd1 = A(:, nun+1)
do i = 1, nun
	if (dd1(i) /= dd1(i)) then
		print*, 'thk_ctrl_gen_1der_fn Error: Nan value in solution to linear system'
		return
	endif
enddo
if (d1_flag .eq. 1 .and. der_flag .eq. 2) then
	dDd1(1) = dd1(nun)
	dEd1(1) = dd1(1)
	dAd1(2:nseg) = dd1(2:nun-1:5)
	dBd1(2:nseg) = dd1(3:nun-1:5)
	dCd1(2:nseg) = dd1(4:nun-1:5)
	dDd1(2:nseg) = dd1(5:nun-1:5)
	dEd1(2:nseg) = dd1(6:nun-1:5)
else
	dEd1(1) = dd1(1)
	dAd1(2:nseg) = dd1(2:nun:5)
	dBd1(2:nseg) = dd1(3:nun:5)
	dCd1(2:nseg) = dd1(4:nun:5)
	dDd1(2:nseg) = dd1(5:nun:5)
	dEd1(2:nseg) = dd1(6:nun:5)
endif
if (allocated(dd1)) deallocate(dd1)
if (allocated(M)) deallocate(M)
if (allocated(r)) deallocate(r)
if (allocated(A)) deallocate(A)
if (allocated(delu)) deallocate(delu)

end subroutine thk_ctrl_gen_1der_fn
