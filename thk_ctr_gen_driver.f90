subroutine thk_ctrl_gen_driver (cname, isdev, sec, uthk, thk, n, u_spl, np, le_angle, te_angle, te_flag, le_opt_flag, &
                                te_opt_flag, out_coord)

implicit none

real, parameter :: dtor = 4.*atan(1.)/180.
logical, intent(in) :: isdev
character (len = 20), intent(in) :: sec
character (len = 32), intent(in) :: cname
integer, intent(in) :: np, n, le_opt_flag, te_opt_flag
integer, intent(inout) :: te_flag
real, intent(in) :: uthk(n), thk(n), u_spl(np)
real, intent(inout) :: le_angle, te_angle
real, intent(out) :: out_coord(np, 12)
integer :: i, j, k, i_te, iter_max, niknt_spl, nknt_spl, nseg_spl, thkmx_i, nseg1, nseg2, d3v_ang_flag_le, &
	n_le, niknt_le, nknt_le, nseg_le, n_te, niknt_te, nknt_te, nseg_te, np_le, np_te, np_bl, grad_flag, &
	d1_flag, np_fine, nmax, nmin, fail_flag, dev_flag
real :: d1v_te, d2v_te, d3v_te, d1v_le, d2v_le, d3v_le, u_thkmx, dseg1, dseg2, u_thkmn_le, u_thkmx_te, &
	d1v_rot, d2v_rot, d3v_rot, F, F_opt, tol, Gn, uthk_le(3), thk_le(3), uthk_te(3), thk_te(3), d_0(3), &
	d_1(3), d_0_temp(3), d_1_temp(3), d_0_init(3), d_0_opt(3), a, a0, a2, a3, h1, h2, h3, F0, F1, F2, F3, &
	d_1_opt(3), G(6), G_opt(6), G_temp(6)
integer, allocatable, dimension(:) :: iknt_spl, iknt_le, iknt_te, imin, imax
real, allocatable, dimension(:) :: u, u_le, u_te, u_spl_le, u_spl_te, u_spl_bl, dAdB1, dBdB1, dCdB1, &
	dDdB1, dEdB1, dAdB1_le, dBdB1_le, dCdB1_le, dDdB1_le, dEdB1_le, dAdB1_te, dBdB1_te, dCdB1_te, dDdB1_te, &
	dEdB1_te, dAdC1, dBdC1, dCdC1, dDdC1, dEdC1, dAdC1_le, dBdC1_le, dCdC1_le, dDdC1_le, dEdC1_le, &
	dAdC1_te, dBdC1_te, dCdC1_te, dDdC1_te, dEdC1_te, dAdD1, dBdD1, dCdD1, dDdD1, dEdD1, dAdD1_le, &
	dBdD1_le, dCdD1_le, dDdD1_le, dEdD1_le, dAdD1_te, dBdD1_te, dCdD1_te, dDdD1_te, dEdD1_te
real, allocatable, dimension(:, :) :: dAdE, dBdE, dCdE, dDdE, dEdE, ddB1, ddC1, ddD1, out_coord_te, &
	dAdE_le, dBdE_le, dCdE_le, dDdE_le, dEdE_le, ddB1_le, ddC1_le, ddD1_le, dAdE_te, dBdE_te, dCdE_te, &
	dDdE_te, dEdE_te, ddB1_te, ddC1_te, ddD1_te, coeff_te
real, allocatable, dimension(:) :: u_spl_fine
real, allocatable, dimension(:, :) :: out_coord_fine	

dev_flag = 0
if (isdev) dev_flag = 1
np_fine = 1000
if (allocated(u_spl_fine)) deallocate(u_spl_fine)
Allocate(u_spl_fine(np_fine))
if (allocated(out_coord_fine)) deallocate(out_coord_fine)
Allocate(out_coord_fine(np_fine, 12))

! if (te_opt_flag .eq. 1 .or. te_opt_flag .eq. 2) then
	! if (thk(n) .ne. 0.) then
		! te_angle = ((thk(n)/(1.-uthk(n)))/dtor)
	! elseif (thk(n) .eq. 0.) then
		! te_angle = -((thk(n) - thk(n-1)/(uthk(n)-uthk(n-1)))/dtor)	
	! endif
! endif	
grad_flag = 1
d1_flag = 1
!	TE Values
d1v_te = tan(-1.*te_angle*dtor)
d2v_te = 0.
d3v_te = 0.

!	LE Initial Values
if (le_opt_flag .eq. 1) then
	d1v_le = (thk(2)-thk(1))/(uthk(2)-uthk(1))
else
	d1v_le = tan(le_angle*dtor)
endif
d2v_le = 0.
d3v_le = 0.

!	Spline Segmentation
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
niknt_spl = (n+3+nmax+nmin-1)*2
! nknt_spl = n+4+niknt_spl
nknt_spl = n+3+nmax+nmin+niknt_spl
nseg_spl = nknt_spl-1
thkmx_i = maxloc(thk, 1)
u_thkmx = uthk(thkmx_i)
Allocate(u(nknt_spl))
u(1) = uthk(1); u(nknt_spl) = uthk(n)
nseg1 = ceiling(nseg_spl/2.)
dseg1 = (u_thkmx-uthk(1))/nseg1
u(2:nseg1+1) = (/ (i, i = 1,nseg1) /)*dseg1 + u(1)
nseg2 = nseg_spl-nseg1
dseg2 = (uthk(n)-u_thkmx)/nseg2
u(nseg1+2:nknt_spl) = (/ (i, i = 1, nseg2) /)*dseg2 + u_thkmx
Allocate(iknt_spl(niknt_spl))
do i = 1, niknt_spl/2
	iknt_spl(2*i-1) = 3*i-1
	iknt_spl(2*i) = 3*i
enddo
! iknt_spl = (/ 2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18 /)

!	LE Segmentation
uthk_le = (/ -thk(1), 0., thk(1) /)
thk_le = (/ uthk(1), 0., uthk(1) /)
n_le = 3
niknt_le = 6
nknt_le = n_le+4+niknt_le
nseg_le = nknt_le-1
u_thkmn_le = 0.
Allocate(u_le(nknt_le))
u_le(1) = -thk(1); u_le(nknt_le) = thk(1)
nseg1 = ceiling(nseg_le/2.)
dseg1 = (u_thkmn_le-u_le(1))/nseg1
u_le(2:nseg1+1) = (/ (i, i = 1,nseg1) /)*dseg1 + u_le(1)
nseg2 = nseg_le-nseg1
dseg2 = (u_le(nknt_le)-u_thkmn_le)/nseg2
u_le(nseg1+2:nknt_le) =  (/ (i, i = 1, nseg2) /)*dseg2 + u_thkmn_le
Allocate(iknt_le(niknt_le))
iknt_le = (/ 2, 4, 6, 8, 10, 12 /)

!	TE Segmentation
uthk_te = (/ -thk(n), 0., thk(n) /)
thk_te = (/ uthk(n), 1., uthk(n) /)
n_te = 3
niknt_te = 6
nknt_te = n_te+4+niknt_te
nseg_te = nknt_te-1
u_thkmx_te = 0.
Allocate(u_te(nknt_te))
u_te(1) = uthk_te(1); u_te(nknt_te) = uthk_te(3)
nseg1 = ceiling(nseg_te/2.)
dseg1 = (u_thkmx_te-u_te(1))/nseg1
u_te(2:nseg1+1) = (/ (i, i = 1,nseg1) /)*dseg1 + u_te(1)
nseg2 = nseg_te-nseg1
dseg2 = (u_te(nknt_te)-u_thkmx_te)/nseg2
u_te(nseg1+2:nknt_le) =  (/ (i, i = 1, nseg2) /)*dseg2 + u_thkmx_te
Allocate(iknt_te(niknt_te))
iknt_te = (/ 2, 4, 6, 8, 10, 12 /)

Allocate(dAdE(nseg_spl, niknt_spl))
Allocate(dBdE(nseg_spl, niknt_spl))
Allocate(dCdE(nseg_spl, niknt_spl))
Allocate(dDdE(nseg_spl, niknt_spl))
Allocate(dEdE(nseg_spl, niknt_spl))
Allocate(dAdE_le(nseg_le, niknt_le))
Allocate(dBdE_le(nseg_le, niknt_le))
Allocate(dCdE_le(nseg_le, niknt_le))
Allocate(dDdE_le(nseg_le, niknt_le))
Allocate(dEdE_le(nseg_le, niknt_le))
Allocate(dAdE_te(nseg_te, niknt_te))
Allocate(dBdE_te(nseg_te, niknt_te))
Allocate(dCdE_te(nseg_te, niknt_te))
Allocate(dDdE_te(nseg_te, niknt_te))
Allocate(dEdE_te(nseg_te, niknt_te))

Allocate(dAdB1(nseg_spl))
Allocate(dBdB1(nseg_spl))
Allocate(dCdB1(nseg_spl))
Allocate(dDdB1(nseg_spl))
Allocate(dEdB1(nseg_spl))
Allocate(dAdC1(nseg_spl))
Allocate(dBdC1(nseg_spl))
Allocate(dCdC1(nseg_spl))
Allocate(dDdC1(nseg_spl))
Allocate(dEdC1(nseg_spl))
Allocate(dAdD1(nseg_spl))
Allocate(dBdD1(nseg_spl))
Allocate(dCdD1(nseg_spl))
Allocate(dDdD1(nseg_spl))
Allocate(dEdD1(nseg_spl))
Allocate(ddB1(nseg_spl, 5))
Allocate(ddC1(nseg_spl, 5))
Allocate(ddD1(nseg_spl, 5))
Allocate(dAdB1_le(nseg_le))
Allocate(dBdB1_le(nseg_le))
Allocate(dCdB1_le(nseg_le))
Allocate(dDdB1_le(nseg_le))
Allocate(dEdB1_le(nseg_le))
Allocate(dAdC1_le(nseg_le))
Allocate(dBdC1_le(nseg_le))
Allocate(dCdC1_le(nseg_le))
Allocate(dDdC1_le(nseg_le))
Allocate(dEdC1_le(nseg_le))
Allocate(dAdD1_le(nseg_le))
Allocate(dBdD1_le(nseg_le))
Allocate(dCdD1_le(nseg_le))
Allocate(dDdD1_le(nseg_le))
Allocate(dEdD1_le(nseg_le))
Allocate(ddB1_le(nseg_le, 5))
Allocate(ddC1_le(nseg_le, 5))
Allocate(ddD1_le(nseg_le, 5))
Allocate(dAdB1_te(nseg_te))
Allocate(dBdB1_te(nseg_te))
Allocate(dCdB1_te(nseg_te))
Allocate(dDdB1_te(nseg_te))
Allocate(dEdB1_te(nseg_te))
Allocate(dAdC1_te(nseg_te))
Allocate(dBdC1_te(nseg_te))
Allocate(dCdC1_te(nseg_te))
Allocate(dDdC1_te(nseg_te))
Allocate(dEdC1_te(nseg_te))
Allocate(dAdD1_te(nseg_te))
Allocate(dBdD1_te(nseg_te))
Allocate(dCdD1_te(nseg_te))
Allocate(dDdD1_te(nseg_te))
Allocate(dEdD1_te(nseg_te))
Allocate(ddB1_te(nseg_te, 5))
Allocate(ddC1_te(nseg_te, 5))
Allocate(ddD1_te(nseg_te, 5))
Allocate(coeff_te(nknt_te-1, 5))

dAdE = 0.; dBdE = 0.; dCdE = 0.; dDdE = 0.; dEdE = 0.
dAdE_le = 0.; dBdE_le = 0.; dCdE_le = 0.; dDdE_le = 0.; dEdE_le = 0.
dAdE_te = 0.; dBdE_te = 0.; dCdE_te = 0.; dDdE_te = 0.; dEdE_te = 0.
dAdB1 = 0.; dBdB1 = 0.; dCdB1 = 0.; dDdB1 = 0.; dEdB1 = 0.
dAdB1_le = 0.; dBdB1_le = 0.; dCdB1_le = 0.; dDdB1_le = 0.; dEdB1_le = 0.
dAdB1_te = 0.; dBdB1_te = 0.; dCdB1_te = 0.; dDdB1_te = 0.; dEdB1_te = 0.
dAdC1 = 0.; dBdC1 = 0.; dCdC1 = 0.; dDdC1 = 0.; dEdC1 = 0.
dAdC1_le = 0.; dBdC1_le = 0.; dCdC1_le = 0.; dDdC1_le = 0.; dEdC1_le = 0.
dAdC1_te = 0.; dBdC1_te = 0.; dCdC1_te = 0.; dDdC1_te = 0.; dEdC1_te = 0.
dAdD1 = 0.; dBdD1 = 0.; dCdD1 = 0.; dDdD1 = 0.; dEdD1 = 0.
dAdD1_le = 0.; dBdD1_le = 0.; dCdD1_le = 0.; dDdD1_le = 0.; dEdD1_le = 0.
dAdD1_te = 0.; dBdD1_te = 0.; dCdD1_te = 0.; dDdD1_te = 0.; dEdD1_te = 0.
d_0 = 0.; G = 0.; G_temp = 0.; G_opt = 0.; out_coord = 0.
out_coord_fine = 0.

d3v_ang_flag_le = 0
if (uthk(n) .eq. 1. .and. te_flag .eq. 1) then
    te_flag = 0
endif

!	Determining E derivatives
do i = 1, niknt_spl
	call thk_ctrl_gen_Eder_fn(uthk, thk, u, n, nknt_spl, iknt_spl, niknt_spl, i, 0, dAdE, dBdE, dCdE, dDdE, dEdE)
enddo
do i = 1, niknt_le
	call thk_ctrl_gen_Eder_fn(uthk_le, thk_le, u_le, n_le, nknt_le, iknt_le, niknt_le, i, 1, dAdE_le, dBdE_le,  &
                              dCdE_le, dDdE_le, dEdE_le)
enddo

!	Determining B1 C1 and D1 derivatives
call thk_ctrl_gen_1der_fn(uthk, thk, u, n, nknt_spl, iknt_spl, niknt_spl, dCdE, dDdE, dEdE, 0., 1, 1, 0, d1_flag, &
                          ddB1, ddC1, ddD1, dAdB1, dBdB1, dCdB1, dDdB1, dEdB1)
ddB1(:, 1) = dAdB1; ddB1(:, 2) = dBdB1; ddB1(:, 3) = dCdB1; ddB1(:, 4) = dDdB1; ddB1(:, 5) = dEdB1
call thk_ctrl_gen_1der_fn(uthk, thk, u, n, nknt_spl, iknt_spl, niknt_spl, dCdE, dDdE, dEdE, 0., 1, 3, 0, d1_flag, &
                          ddB1, ddC1, ddD1, dAdD1, dBdD1, dCdD1, dDdD1, dEdD1)
ddD1(:, 1) = dAdD1; ddD1(:, 2) = dBdD1; ddD1(:, 3) = dCdD1; ddD1(:, 4) = dDdD1; ddD1(:, 5) = dEdD1
call thk_ctrl_gen_1der_fn(uthk, thk, u, n, nknt_spl, iknt_spl, niknt_spl, dCdE, dDdE, dEdE, 0., 1, 2, 0, d1_flag, &
                          ddB1, ddC1, ddD1, dAdC1, dBdC1, dCdC1, dDdC1, dEdC1)
ddC1(:, 1) = dAdC1; ddC1(:, 2) = dBdC1; ddC1(:, 3) = dCdC1; ddC1(:, 4) = dDdC1; ddC1(:, 5) = dEdC1

! Included input parameters ddB1_le, ddC1_le, ddD1_le in subroutine calls 262-267 for gfortran
call thk_ctrl_gen_1der_fn(uthk_le, thk_le, u_le, n_le, nknt_le, iknt_le, niknt_le, dCdE_le, dDdE_le, dEdE_le, -1.,d3v_ang_flag_le, &
                          1, 1, 0, ddB1_le, ddC1_le, ddD1_le, dAdB1_le, dBdB1_le, dCdB1_le,       &
                          dDdB1_le, dEdB1_le)
call thk_ctrl_gen_1der_fn(uthk_le, thk_le, u_le, n_le, nknt_le, iknt_le, niknt_le, dCdE_le, dDdE_le, dEdE_le, 1., d3v_ang_flag_le, &
                          2, 1, 0, ddB1_le, ddC1_le, ddD1_le, dAdC1_le, dBdC1_le, dCdC1_le, dDdC1_le, dEdC1_le)
call thk_ctrl_gen_1der_fn(uthk_le, thk_le, u_le, n_le, nknt_le, iknt_le, niknt_le, dCdE_le, dDdE_le, dEdE_le, -1.,d3v_ang_flag_le, &
                          3, 1, 0, ddB1_le, ddC1_le, ddD1_le, dAdD1_le, dBdD1_le, dCdD1_le, dDdD1_le, dEdD1_le)
ddB1_le(:, 1) = dAdB1_le; ddB1_le(:, 2) = dBdB1_le; ddB1_le(:, 3) = dCdB1_le; ddB1_le(:, 4) = dDdB1_le; ddB1_le(:, 5) = dEdB1_le
ddC1_le(:, 1) = dAdC1_le; ddC1_le(:, 2) = dBdC1_le; ddC1_le(:, 3) = dCdC1_le; ddC1_le(:, 4) = dDdC1_le; ddC1_le(:, 5) = dEdC1_le
ddD1_le(:, 1) = dAdD1_le; ddD1_le(:, 2) = dBdD1_le; ddD1_le(:, 3) = dCdD1_le; ddD1_le(:, 4) = dDdD1_le; ddD1_le(:, 5) = dEdD1_le

i_te = np
!	TE
if (te_flag .eq. 1) then
	do i = np, 1, -1
		if (thk(n) .eq. 0. .and. u_spl(i) .lt. uthk(n-1)) exit
		if (thk(n) .ne. 0. .and. u_spl(i) .lt. uthk(n)) exit
	enddo
	i_te = i+1
	np_te = np-i_te+1
    if (allocated(u_spl_te)) deallocate(u_spl_te)
	Allocate(u_spl_te(np_te))
    if (allocated(out_coord_te)) deallocate(out_coord_te)
	Allocate(out_coord_te(np_te, 12))
	out_coord_te = 0.
	! u_spl = (/ (uthk(3) + j*(1.-uthk(3))/(np-1), j = 0, np-1) /)
	u_spl_te = u_spl(i_te : np)
elseif (te_flag .eq. 0) then
	np_te = 1
    if (allocated(u_spl_te)) deallocate(u_spl_te)
	Allocate(u_spl_te(np_te))
    if (allocated(out_coord_te)) deallocate(out_coord_te)
	Allocate(out_coord_te(np_te, 12))
	out_coord_te = 0.
	u_spl_te = 0.
endif
if (te_flag .eq. 1 .and. te_opt_flag .eq. 0) then
	do i = 1, niknt_te
		call thk_ctrl_gen_Eder_fn(uthk_te, thk_te, u_te, n_te, nknt_te, iknt_te, niknt_te, i, 0, dAdE_te, dBdE_te, dCdE_te,        &
                                  dDdE_te, dEdE_te)
	enddo

    ! Included input parameters ddB1_te, ddC1_te, ddD1_te in subroutine calls 303-305 for gfortran
	call thk_ctrl_gen_1der_fn(uthk_te, thk_te, u_te, n_te, nknt_te, iknt_te, niknt_te, dCdE_te, dDdE_te, dEdE_te, -1., 0, 1, 0, 0, &
		                      ddB1_te, ddC1_te, ddD1_te, dAdB1_te, dBdB1_te, dCdB1_te, dDdB1_te, dEdB1_te)
	call thk_ctrl_gen_1der_fn(uthk_te, thk_te, u_te, n_te, nknt_te, iknt_te, niknt_te, dCdE_te, dDdE_te, dEdE_te, -1., 0, 3, 0, 0, &
		                      ddB1_te, ddC1_te, ddD1_te, dAdD1_te, dBdD1_te, dCdD1_te, dDdD1_te, dEdD1_te)	
	call thk_ctrl_gen_1der_fn(uthk_te, thk_te, u_te, n_te, nknt_te, iknt_te, niknt_te, dCdE_te, dDdE_te, dEdE_te, 1., 0, 2, 0, 0,  &
		                      ddB1_te, ddC1_te, ddD1_te, dAdC1_te, dBdC1_te, dCdC1_te, dDdC1_te, dEdC1_te)
	ddB1_te(:, 1) = dAdB1_te; ddB1_te(:, 2) = dBdB1_te; ddB1_te(:, 3) = dCdB1_te; ddB1_te(:, 4) = dDdB1_te; ddB1_te(:, 5) = dEdB1_te
	ddD1_te(:, 1) = dAdD1_te; ddD1_te(:, 2) = dBdD1_te; ddD1_te(:, 3) = dCdD1_te; ddD1_te(:, 4) = dDdD1_te; ddD1_te(:, 5) = dEdD1_te		
	ddC1_te(:, 1) = dAdC1_te; ddC1_te(:, 2) = dBdC1_te; ddC1_te(:, 3) = dCdC1_te; ddC1_te(:, 4) = dDdC1_te; ddC1_te(:, 5) = dEdC1_te
	d_0(1) = d1v_te
	call dnv_rot(d_0, d1v_rot, d2v_rot, d3v_rot)
	d_0(1) = d1v_rot; d_0(2) = d2v_rot; d_0(3) = d3v_rot
	d_1(1) = -d1v_rot; d_1(2) = d2v_rot; d_1(3) = -d3v_rot
	! u_spl = (/ (uthk(3) + j*(1.-uthk(3))/(np-1), j = 0, np-1) /)
	call thk_ctrl_gen_spl(uthk_te, thk_te, n_te, nknt_te, d_0, d_1, dAdE_te, dBdE_te, dCdE_te, dDdE_te, dEdE_te, ddB1_te, ddC1_te, &
                          ddD1_te, u_spl_te, np_te, iknt_te, niknt_te, 0, 0, 1, 1, 2, 0, 0, F, G, out_coord_te, coeff_te,          &
                          fail_flag, dev_flag)
	out_coord(i_te : np, :) = out_coord_te
	open (unit = 81, file = 'thk_dist_te.' // trim(adjustl(sec)) // '.' // trim(cname) // '.dat')
	write (81, '(12F40.12)') (out_coord_te(i, :), i = 1, np_te)			
	close (81)	
	if (fail_flag .ne. 0) print*, 'FATAL ERROR: Trailing edge thickness distribution generation failed. TE angle &
                                   may be too small/large for specified thickness.'
	if (dev_flag .eq. 1) then	
		do i = 1, np_fine
			u_spl_fine(i) = (i-1.)/(np_fine-1)*(1. - thk_te(1)) + thk_te(1)
		enddo
		call thk_ctrl_gen_spl(uthk_te, thk_te, n_te, nknt_te, d_0, d_1, dAdE_te, dBdE_te, dCdE_te, &
		dDdE_te, dEdE_te, ddB1_te, ddC1_te, ddD1_te, u_spl_fine, np_fine, iknt_te, niknt_te, 0, 0, 1, &
		1, 2, 0, 0, F, G, out_coord_fine, coeff_te, fail_flag, dev_flag)
		open (unit = 81, file = 'thk_dist_fine_te.' // trim(adjustl(sec)) // '.' // trim(cname) // '.dat')
		write (81, '(12F40.12)') (out_coord_fine(i, :), i = 1, np_fine)			
		close (81)
	endif
endif

d_1 = (/ d1v_te, d2v_te, d3v_te /)
d_0_init = (/ d1v_le, d2v_le, d3v_le /)

! call thk_ctrl_gen_driver_aux (cname, dev_flag,  sec, uthk, thk, n, nknt_spl, d_0_init, d_1, dAdE, dBdE, dCdE, dDdE, dEdE, &
    ! np, iknt_spl, niknt_spl, uthk_le, thk_le, n_le, nknt_le, dAdE_le, dBdE_le, dCdE_le, dDdE_le, dEdE_le, &
    ! ddB1, ddC1, ddD1, ddB1_le, ddC1_le, ddD1_le, iknt_le, niknt_le, 1, &
	! F, G, out_coord, out_coord_le)
	
! open (unit = 101, file = 'LE_FORTRAN')
! write (101, '(6F40.16)') (out_coord_le(i, 1), out_coord_le(i, 2), out_coord_le(i, 3), out_coord_le(i, 4), out_coord_le(i, 5), out_coord_le(i, 6), i = 1, nseg_le*np)
! close (101)
! print*, F, G

do i = 1, np
	if (u_spl(i) .gt. uthk(1))  then
		np_le = i-1
		exit
	endif
enddo
np_bl = i_te-i
if (allocated(u_spl_le)) deallocate(u_spl_le)
if (allocated(u_spl_bl)) deallocate(u_spl_bl)
Allocate(u_spl_le(np_le))
Allocate(u_spl_bl(np_bl))
u_spl_le = u_spl(1 : np_le)
u_spl_bl = u_spl(np_le+1 : i_te)

iter_max = 30; tol = 1.E-8
d_0_opt = d_0_init; d_1_opt = d_1
d_0 = d_0_init
i = 1
! goto 40
do while (i .lt. iter_max)
	call thk_ctrl_gen_driver_aux (cname, dev_flag, sec, uthk, thk, n, nknt_spl, d_0, d_1, dAdE, dBdE, dCdE, dDdE, dEdE, &
		np, iknt_spl, niknt_spl, uthk_le, thk_le, n_le, nknt_le, dAdE_le, dBdE_le, dCdE_le, dDdE_le, dEdE_le, &
		ddB1, ddC1, ddD1, ddB1_le, ddC1_le, ddD1_le, iknt_le, niknt_le, 0, u_spl_le, np_le, u_spl_bl, np_bl, &
		uthk_te, thk_te, n_te, nknt_te, dAdE_te, dBdE_te, dCdE_te, dDdE_te, dEdE_te, ddB1_te, ddC1_te, &
		ddD1_te, iknt_te, niknt_te, u_spl_te, np_te, le_opt_flag, te_opt_flag, te_flag, grad_flag, u, &		
		F1, G, out_coord)

	Gn = 0.
	do j = 1, 6
		Gn = Gn + G(j)**2
	enddo
	Gn = sqrt(Gn)
	if (abs(Gn) .lt. 1.E-13) then
		print*, 'Zero gradient.'
		d_0_opt = d_0
		d_1_opt = d_1
		exit
    endif
    G = G/Gn
	a3 = 1.

	d_0_temp = d_0
	if (d1_flag .eq. 0 .and. (le_opt_flag .eq. 1 .or. le_opt_flag .eq. 2)) then
		d_0_temp = d_0-a3*G(1:3)
	elseif (d1_flag .eq. 1 .and. (le_opt_flag .eq. 1 .or. le_opt_flag .eq. 2)) then
		d_0_temp(1:2) = d_0(1:2)-a3*G(1:2)
	elseif (d1_flag .eq. 0 .and. le_opt_flag .eq. 0) then
		d_0_temp(2:3) = d_0(2:3)-a3*G(2:3)
	elseif (d1_flag .eq. 1 .and. le_opt_flag .eq. 0) then
		d_0_temp(2) = d_0(2)-a3*G(2)
	endif
	if (te_opt_flag .eq. 0) then
		d_1_temp = d_1
	elseif (te_opt_flag .eq. 1) then
		d_1_temp = (/ d_1(1)-a3*G(4), d_1(2), d_1(3)  /)
	elseif (te_opt_flag .eq. 2) then
		d_1_temp = d_1-a3*G(4:6)
	endif
	call thk_ctrl_gen_driver_aux (cname, dev_flag, sec, uthk, thk, n, nknt_spl, d_0_temp, d_1_temp, dAdE, dBdE, dCdE, dDdE, dEdE, &
		3, iknt_spl, niknt_spl, uthk_le, thk_le, n_le, nknt_le, dAdE_le, dBdE_le, dCdE_le, dDdE_le, dEdE_le, &
		ddB1, ddC1, ddD1, ddB1_le, ddC1_le, ddD1_le, iknt_le, niknt_le, 0, u_spl_le, np_le, u_spl_bl, np_bl, &
		uthk_te, thk_te, n_te, nknt_te, dAdE_te, dBdE_te, dCdE_te, dDdE_te, dEdE_te, ddB1_te, ddC1_te, &
		ddD1_te, iknt_te, niknt_te, u_spl_te, np_te, le_opt_flag, te_opt_flag, te_flag, grad_flag, u, &			
		F3, G_temp, out_coord)	
	do while (F3 .ge. F1)
        a3 = a3/2.
		if (d1_flag .eq. 0 .and. (le_opt_flag .eq. 1 .or. le_opt_flag .eq. 2)) then
			d_0_temp = d_0-a3*G(1:3)
		elseif (d1_flag .eq. 1 .and. (le_opt_flag .eq. 1 .or. le_opt_flag .eq. 2)) then
			d_0_temp(1:2) = d_0(1:2)-a3*G(1:2)
		elseif (d1_flag .eq. 0 .and. le_opt_flag .eq. 0) then
			d_0_temp(2:3) = d_0(2:3)-a3*G(2:3)
		elseif (d1_flag .eq. 1 .and. le_opt_flag .eq. 0) then
			d_0_temp(2) = d_0(2)-a3*G(2)
		endif
		if (te_opt_flag .eq. 0) then
			d_1_temp = d_1
		elseif (te_opt_flag .eq. 1) then
			d_1_temp = (/ d_1(1)-a3*G(4), d_1(2), d_1(3)  /)
		elseif (te_opt_flag .eq. 2) then
			d_1_temp = d_1-a3*G(4:6)
		endif
		call thk_ctrl_gen_driver_aux (cname, dev_flag, sec, uthk, thk, n, nknt_spl, d_0_temp, d_1_temp, dAdE, dBdE, dCdE, dDdE, dEdE, &
			3, iknt_spl, niknt_spl, uthk_le, thk_le, n_le, nknt_le, dAdE_le, dBdE_le, dCdE_le, dDdE_le, dEdE_le, &
			ddB1, ddC1, ddD1, ddB1_le, ddC1_le, ddD1_le, iknt_le, niknt_le, 0, u_spl_le, np_le, u_spl_bl, np_bl, &
			uthk_te, thk_te, n_te, nknt_te, dAdE_te, dBdE_te, dCdE_te, dDdE_te, dEdE_te, ddB1_te, ddC1_te, &
			ddD1_te, iknt_te, niknt_te, u_spl_te, np_te, le_opt_flag, te_opt_flag, te_flag, grad_flag, u, &
			F3, G_temp, out_coord)		
        if (a3 .lt. tol/2.) then
			print*, 'No improvement likely.'
			d_0_opt = d_0
			d_1_opt = d_1
			exit
        endif
    enddo
	if (a3 .lt. tol/2.) then
		exit
	endif
    a2 = a3/2.
	d_0_temp = d_0
	if (d1_flag .eq. 0 .and. (le_opt_flag .eq. 1 .or. le_opt_flag .eq. 2)) then
		d_0_temp = d_0-a2*G(1:3)
	elseif (d1_flag .eq. 1 .and. (le_opt_flag .eq. 1 .or. le_opt_flag .eq. 2)) then
		d_0_temp(1:2) = d_0(1:2)-a2*G(1:2)
	elseif (d1_flag .eq. 0 .and. le_opt_flag .eq. 0) then
		d_0_temp(2:3) = d_0(2:3)-a2*G(2:3)
	elseif (d1_flag .eq. 1 .and. le_opt_flag .eq. 0) then
		d_0_temp(2) = d_0(2)-a2*G(2)
	endif
	if (te_opt_flag .eq. 0) then
		d_1_temp = d_1
	elseif (te_opt_flag .eq. 1) then
		d_1_temp = (/ d_1(1)-a2*G(4), d_1(2), d_1(3)  /)
	elseif (te_opt_flag .eq. 2) then
		d_1_temp = d_1-a2*G(4:6)
	endif
	call thk_ctrl_gen_driver_aux (cname, dev_flag, sec, uthk, thk, n, nknt_spl, d_0_temp, d_1_temp, dAdE, dBdE, dCdE, dDdE, dEdE, &
		3, iknt_spl, niknt_spl, uthk_le, thk_le, n_le, nknt_le, dAdE_le, dBdE_le, dCdE_le, dDdE_le, dEdE_le, &
		ddB1, ddC1, ddD1, ddB1_le, ddC1_le, ddD1_le, iknt_le, niknt_le, 0, u_spl_le, np_le, u_spl_bl, np_bl, &
		uthk_te, thk_te, n_te, nknt_te, dAdE_te, dBdE_te, dCdE_te, dDdE_te, dEdE_te, ddB1_te, ddC1_te, &
		ddD1_te, iknt_te, niknt_te, u_spl_te, np_te, le_opt_flag, te_opt_flag, te_flag, grad_flag, u, &			
		F2, G_temp, out_coord)	
    h1 = (F2 - F1)/a2; h2 = (F3 - F2)/(a3 - a2)
    h3 = (h2 - h1)/a3
    a0 = 0.5*(a2 - h1/h3)
	d_0_temp = d_0
	if (d1_flag .eq. 0 .and. (le_opt_flag .eq. 1 .or. le_opt_flag .eq. 2)) then
		d_0_temp = d_0-a0*G(1:3)
	elseif (d1_flag .eq. 1 .and. (le_opt_flag .eq. 1 .or. le_opt_flag .eq. 2)) then
		d_0_temp(1:2) = d_0(1:2)-a0*G(1:2)
	elseif (d1_flag .eq. 0 .and. le_opt_flag .eq. 0) then
		d_0_temp(2:3) = d_0(2:3)-a0*G(2:3)
	elseif (d1_flag .eq. 1 .and. le_opt_flag .eq. 0) then
		d_0_temp(2) = d_0(2)-a0*G(2)
	endif
	if (te_opt_flag .eq. 0) then
		d_1_temp = d_1
	elseif (te_opt_flag .eq. 1) then
		d_1_temp = (/ d_1(1)-a0*G(4), d_1(2), d_1(3)  /)
	elseif (te_opt_flag .eq. 2) then
		d_1_temp = d_1-a0*G(4:6)
	endif
	call thk_ctrl_gen_driver_aux (cname, dev_flag, sec, uthk, thk, n, nknt_spl, d_0_temp, d_1_temp, dAdE, dBdE, dCdE, dDdE, dEdE, &
		3, iknt_spl, niknt_spl, uthk_le, thk_le, n_le, nknt_le, dAdE_le, dBdE_le, dCdE_le, dDdE_le, dEdE_le, &
		ddB1, ddC1, ddD1, ddB1_le, ddC1_le, ddD1_le, iknt_le, niknt_le, 0, u_spl_le, np_le, u_spl_bl, np_bl, &
		uthk_te, thk_te, n_te, nknt_te, dAdE_te, dBdE_te, dCdE_te, dDdE_te, dEdE_te, ddB1_te, ddC1_te, &
		ddD1_te, iknt_te, niknt_te, u_spl_te, np_te, le_opt_flag, te_opt_flag, te_flag, grad_flag, u, &			
		F0, G_temp, out_coord)	
    if (F0 .lt. F3) then
        a = a0
    else
        a = a3
    endif

	if (d1_flag .eq. 0 .and. (le_opt_flag .eq. 1 .or. le_opt_flag .eq. 2)) then
		d_0 = d_0-a*G(1:3)
	elseif (d1_flag .eq. 1 .and. (le_opt_flag .eq. 1 .or. le_opt_flag .eq. 2)) then
		d_0(1:2) = d_0(1:2)-a*G(1:2)
	elseif (d1_flag .eq. 0 .and. le_opt_flag .eq. 0) then
		d_0(2:3) = d_0(2:3)-a*G(2:3)
	elseif (d1_flag .eq. 1 .and. le_opt_flag .eq. 0) then
		d_0(2) = d_0(2)-a*G(2)
	endif

	if (te_opt_flag .eq. 1) then	
		d_1(1) = d_1(1) - a*G(4)
	elseif (te_opt_flag .eq. 2) then
		d_1 = d_1 - a*G(4:6)		
	endif
    i = i+1
enddo
d_0_opt = d_0
d_1_opt = d_1

40 call thk_ctrl_gen_driver_aux (cname, dev_flag, sec, uthk, thk, n, nknt_spl, d_0_opt, d_1_opt, dAdE, dBdE, dCdE, dDdE, dEdE, &
    np, iknt_spl, niknt_spl, uthk_le, thk_le, n_le, nknt_le, dAdE_le, dBdE_le, dCdE_le, dDdE_le, dEdE_le, &
    ddB1, ddC1, ddD1, ddB1_le, ddC1_le, ddD1_le, iknt_le, niknt_le, 1, u_spl_le, np_le, u_spl_bl, np_bl, &
	uthk_te, thk_te, n_te, nknt_te, dAdE_te, dBdE_te, dCdE_te, dDdE_te, dEdE_te, ddB1_te, ddC1_te, &
	ddD1_te, iknt_te, niknt_te, u_spl_te, np_te, le_opt_flag, te_opt_flag, te_flag, grad_flag, u, &	
	F_opt, G_opt, out_coord)

print*, 'Number of iterations: ', i
print*, 'Optimum parameters: ', d_0_opt(1)/dtor, d_0_opt(2), d_0_opt(3), d_1_opt(1)/dtor, d_1_opt(2), d_1_opt(3)
! print*, 'Slope angle (deg) at LE: ', d_0_opt(1)/dtor
! print*, 'Second derivative at LE: ', d_0_opt(2)
! print*, 'Third derivative at LE: ', d_0_opt(3)
print*, 'Objective: ', F_opt
print*, 'Gradient: ', G_opt

if (allocated(u)) deallocate(u)
if (allocated(iknt_spl)) deallocate(iknt_spl)
if (allocated(u_le)) deallocate(u_le)
if (allocated(iknt_le)) deallocate(iknt_le)
if (allocated(u_te)) deallocate(u_te)
if (allocated(iknt_te)) deallocate(iknt_te)
if (allocated(dAdE)) deallocate(dAdE)
if (allocated(dBdE)) deallocate(dBdE)
if (allocated(dCdE)) deallocate(dCdE)
if (allocated(dDdE)) deallocate(dDdE)
if (allocated(dEdE)) deallocate(dEdE)
if (allocated(dAdE_le)) deallocate(dAdE_le)
if (allocated(dBdE_le)) deallocate(dBdE_le)
if (allocated(dCdE_le)) deallocate(dCdE_le)
if (allocated(dDdE_le)) deallocate(dDdE_le)
if (allocated(dEdE_le)) deallocate(dEdE_le)
if (allocated(dAdE_te)) deallocate(dAdE_te)
if (allocated(dBdE_te)) deallocate(dBdE_te)
if (allocated(dCdE_te)) deallocate(dCdE_te)
if (allocated(dDdE_te)) deallocate(dDdE_te)
if (allocated(dEdE_te)) deallocate(dEdE_te)
if (allocated(dAdB1)) deallocate(dAdB1)
if (allocated(dBdB1)) deallocate(dBdB1)
if (allocated(dCdB1)) deallocate(dCdB1)
if (allocated(dDdB1)) deallocate(dDdB1)
if (allocated(dEdB1)) deallocate(dEdB1)
if (allocated(dAdC1)) deallocate(dAdC1)
if (allocated(dBdC1)) deallocate(dBdC1)
if (allocated(dCdC1)) deallocate(dCdC1)
if (allocated(dDdC1)) deallocate(dDdC1)
if (allocated(dEdC1)) deallocate(dEdC1)
if (allocated(dAdD1)) deallocate(dAdD1)
if (allocated(dBdD1)) deallocate(dBdD1)
if (allocated(dCdD1)) deallocate(dCdD1)
if (allocated(dDdD1)) deallocate(dDdD1)
if (allocated(dEdD1)) deallocate(dEdD1)
if (allocated(ddB1)) deallocate(ddB1)
if (allocated(ddC1)) deallocate(ddC1)
if (allocated(ddD1)) deallocate(ddD1)
if (allocated(dAdB1_le)) deallocate(dAdB1_le)
if (allocated(dBdB1_le)) deallocate(dBdB1_le)
if (allocated(dCdB1_le)) deallocate(dCdB1_le)
if (allocated(dDdB1_le)) deallocate(dDdB1_le)
if (allocated(dEdB1_le)) deallocate(dEdB1_le)
if (allocated(dAdC1_le)) deallocate(dAdC1_le)
if (allocated(dBdC1_le)) deallocate(dBdC1_le)
if (allocated(dCdC1_le)) deallocate(dCdC1_le)
if (allocated(dDdC1_le)) deallocate(dDdC1_le)
if (allocated(dEdC1_le)) deallocate(dEdC1_le)
if (allocated(dAdD1_le)) deallocate(dAdD1_le)
if (allocated(dBdD1_le)) deallocate(dBdD1_le)
if (allocated(dCdD1_le)) deallocate(dCdD1_le)
if (allocated(dDdD1_le)) deallocate(dDdD1_le)
if (allocated(dEdD1_le)) deallocate(dEdD1_le)
if (allocated(ddB1_le)) deallocate(ddB1_le)
if (allocated(ddC1_le)) deallocate(ddC1_le)
if (allocated(ddD1_le)) deallocate(ddD1_le)
if (allocated(dAdB1_te)) deallocate(dAdB1_te)
if (allocated(dBdB1_te)) deallocate(dBdB1_te)
if (allocated(dCdB1_te)) deallocate(dCdB1_te)
if (allocated(dDdB1_te)) deallocate(dDdB1_te)
if (allocated(dEdB1_te)) deallocate(dEdB1_te)
if (allocated(dAdC1_te)) deallocate(dAdC1_te)
if (allocated(dBdC1_te)) deallocate(dBdC1_te)
if (allocated(dCdC1_te)) deallocate(dCdC1_te)
if (allocated(dDdC1_te)) deallocate(dDdC1_te)
if (allocated(dEdC1_te)) deallocate(dEdC1_te)
if (allocated(dAdD1_te)) deallocate(dAdD1_te)
if (allocated(dBdD1_te)) deallocate(dBdD1_te)
if (allocated(dCdD1_te)) deallocate(dCdD1_te)
if (allocated(dDdD1_te)) deallocate(dDdD1_te)
if (allocated(dEdD1_te)) deallocate(dEdD1_te)
if (allocated(ddB1_te)) deallocate(ddB1_te)
if (allocated(ddC1_te)) deallocate(ddC1_te)
if (allocated(ddD1_te)) deallocate(ddD1_te)
if (allocated(out_coord_te)) deallocate(out_coord_te)
if (allocated(coeff_te)) deallocate(coeff_te)

end subroutine thk_ctrl_gen_driver


subroutine thk_ctrl_gen_driver_span (isdev, uthk, thk, n, u_spl, np, opt_flag, out_coord)

implicit none

real, parameter :: dtor = 4.*atan(1.)/180.
logical, intent(in) :: isdev
integer, intent(in) :: np, n, opt_flag
real, intent(in) :: uthk(n), thk(n), u_spl(np)
real, intent(out) :: out_coord(np, 12)
integer :: i, j, k, iter_max, niknt_spl, nknt_spl, nseg_spl, thkmx_i, nseg1, &
	nseg2, nparam, nmax, nmin, d3v_ang_flag, dev_flag, sl_flag
real :: d1v_te, d2v_te, d3v_te, d1v_le, d2v_le, d3v_le, u_thkmx, dseg, dseg2, &
	F, F_opt, tol, Gn, d_0(3), d_1(3), d_0_init(3), d_0_opt(3), a, a0, a2, a3, &
	h1, h2, h3, F0, F1, F2, F3, d_1_opt(3), te_angle, G(6), G_opt(6), G_temp(6)
integer, allocatable, dimension(:) :: iknt_spl, imin, imax
real, allocatable, dimension(:) :: u, dAdB1, dBdB1, dCdB1, dDdB1, dEdB1, &
	dAdC1, dBdC1, dCdC1, dDdC1, dEdC1, dAdD1, dBdD1, dCdD1, dDdD1, dEdD1
real, allocatable, dimension(:, :) :: dAdE, dBdE, dCdE, dDdE, dEdE, ddB1, ddC1, ddD1

dev_flag = 0
if (isdev) dev_flag = 1
d3v_ang_flag = 1
!	TE Values
d1v_te = (thk(n)-thk(n-1))/(uthk(n)-uthk(n-1))
d2v_te = 0.
d3v_te = 0.

!	LE Initial Values
d1v_le = (thk(2)-thk(1))/(uthk(2)-uthk(1))
d2v_le = 0.
d3v_le = 0.

out_coord = 0.
sl_flag = 1
do i = 1, n
	if (thk(i) .ne. thk(1)) then
		sl_flag = 0
		exit
	endif
enddo
if (sl_flag .eq. 1) then
	print*, 'Skipping spline routine - straight line.'
	out_coord(:, 1) = u_spl
	do i = 1, np
		out_coord(i, 2) = d1v_le*(u_spl(i)-u_spl(1)) + thk(1)
		out_coord(:, 3) = d1v_le
	enddo
	return
endif

!	Spline Segmentation
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
! niknt_spl = n+3+nmax+nmin-1
! nknt_spl = n+3+niknt_spl
niknt_spl = (n+3+nmax+nmin-1)*2
nknt_spl = n+3+nmax+nmin+niknt_spl
nseg_spl = nknt_spl-1
thkmx_i = maxloc(thk, 1)
u_thkmx = uthk(thkmx_i)
Allocate(u(nknt_spl))
u(1) = uthk(1); u(nknt_spl) = uthk(n)
dseg = (uthk(n)-uthk(1))/nseg_spl
u(2:nseg_spl+1) = (/ (i, i = 1, nseg_spl) /)*dseg + u(1)
Allocate(iknt_spl(niknt_spl))
! iknt_spl = (/ 3, 7 /)
! do i = 1, niknt_spl
	! iknt_spl(i) = 2*i
! enddo
do i = 1, niknt_spl/2
	iknt_spl(2*i-1) = 3*i-1
	iknt_spl(2*i) = 3*i
enddo

Allocate(dAdE(nseg_spl, niknt_spl))
Allocate(dBdE(nseg_spl, niknt_spl))
Allocate(dCdE(nseg_spl, niknt_spl))
Allocate(dDdE(nseg_spl, niknt_spl))
Allocate(dEdE(nseg_spl, niknt_spl))

Allocate(dAdB1(nseg_spl))
Allocate(dBdB1(nseg_spl))
Allocate(dCdB1(nseg_spl))
Allocate(dDdB1(nseg_spl))
Allocate(dEdB1(nseg_spl))
Allocate(dAdC1(nseg_spl))
Allocate(dBdC1(nseg_spl))
Allocate(dCdC1(nseg_spl))
Allocate(dDdC1(nseg_spl))
Allocate(dEdC1(nseg_spl))
Allocate(dAdD1(nseg_spl))
Allocate(dBdD1(nseg_spl))
Allocate(dCdD1(nseg_spl))
Allocate(dDdD1(nseg_spl))
Allocate(dEdD1(nseg_spl))
Allocate(ddB1(nseg_spl, 5))
Allocate(ddC1(nseg_spl, 5))
Allocate(ddD1(nseg_spl, 5))

dAdE = 0.; dBdE = 0.; dCdE = 0.; dDdE = 0.; dEdE = 0.
dAdB1 = 0.; dBdB1 = 0.; dCdB1 = 0.; dDdB1 = 0.; dEdB1 = 0.
dAdC1 = 0.; dBdC1 = 0.; dCdC1 = 0.; dDdC1 = 0.; dEdC1 = 0.
dAdD1 = 0.; dBdD1 = 0.; dCdD1 = 0.; dDdD1 = 0.; dEdD1 = 0.
d_0 = 0.

!	Determining E derivatives
do i = 1, niknt_spl
	call thk_ctrl_gen_Eder_fn(uthk, thk, u, n, nknt_spl, iknt_spl, niknt_spl, i, 10, dAdE, dBdE, dCdE, dDdE, dEdE)
enddo

!	Determining B1 C1 and D1 derivatives
call thk_ctrl_gen_1der_fn(uthk, thk, u, n, nknt_spl, iknt_spl, niknt_spl, dCdE, dDdE, dEdE, 0., d3v_ang_flag, 1, 10, 1, &
                          ddB1, ddC1, ddD1, dAdB1, dBdB1, dCdB1, dDdB1, dEdB1)
ddB1(:, 1) = dAdB1; ddB1(:, 2) = dBdB1; ddB1(:, 3) = dCdB1; ddB1(:, 4) = dDdB1; ddB1(:, 5) = dEdB1
call thk_ctrl_gen_1der_fn(uthk, thk, u, n, nknt_spl, iknt_spl, niknt_spl, dCdE, dDdE, dEdE, 0., d3v_ang_flag, 3, 10, 1, &
                          ddB1, ddC1, ddD1, dAdD1, dBdD1, dCdD1, dDdD1, dEdD1)
ddD1(:, 1) = dAdD1; ddD1(:, 2) = dBdD1; ddD1(:, 3) = dCdD1; ddD1(:, 4) = dDdD1; ddD1(:, 5) = dEdD1
call thk_ctrl_gen_1der_fn(uthk, thk, u, n, nknt_spl, iknt_spl, niknt_spl, dCdE, dDdE, dEdE, 0., d3v_ang_flag, 2, 10, 1, &
                          ddB1, ddC1, ddD1, dAdC1, dBdC1, dCdC1, dDdC1, dEdC1)
ddC1(:, 1) = dAdC1; ddC1(:, 2) = dBdC1; ddC1(:, 3) = dCdC1; ddC1(:, 4) = dDdC1; ddC1(:, 5) = dEdC1

d_1 = (/ d1v_te, d2v_te, d3v_te /)
d_0_init = (/ d1v_le, d2v_le, d3v_le /)
iter_max = 500; tol = 1.E-8
d_0 = d_0_init
d_0_opt = d_0; d_1_opt = d_1
goto 40
i = 1
do while (i .lt. iter_max)
	call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, d_0, d_1, dAdE, dBdE, dCdE, dDdE, dEdE, &
		np, iknt_spl, niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, &
		F1, G, out_coord)
	Gn = 0.
	do j = 1, 6
		Gn = Gn + G(j)**2
	enddo
	Gn = sqrt(Gn)
	if (abs(Gn) .lt. 1.E-13) then
		print*, 'Zero gradient.'
		d_0_opt = d_0
		d_1_opt = d_1
		exit
    endif
    G = G/Gn
	! print*, 'G   :', G
	a3 = 1.
	if (opt_flag .eq. 0) then
		call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, d_0-a3*G(1:3), d_1, dAdE, dBdE, dCdE, dDdE, dEdE, &
			np, iknt_spl, niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, &
			F3, G_temp, out_coord)
	elseif (opt_flag .eq. 1) then
		call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, (/ d_0(1)-a3*G(1), d_0(2), d_0(3) /), &
                                          (/ d_1(1)-a3*G(4), d_1(2), d_1(3) /), dAdE, dBdE, dCdE, dDdE, dEdE, np, iknt_spl, &
                                          niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, F3, G_temp, out_coord)
	elseif (opt_flag .eq. 2) then
		call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, d_0-a3*G(1:3), d_1-a3*G(4:6), dAdE, dBdE, dCdE, dDdE, &
                                           dEdE, np, iknt_spl, niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, F3, G_temp,        &
                                           out_coord)
	endif
	do while (F3 .ge. F1)
        a3 = a3/2.
		if (opt_flag .eq. 0) then
			call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, d_0-a3*G(1:3), d_1, dAdE, dBdE, dCdE, dDdE, dEdE, &
				np, iknt_spl, niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, &
				F3, G_temp, out_coord)			
		elseif (opt_flag .eq. 1) then
			call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, (/ d_0(1)-a3*G(1), d_0(2), d_0(3) /), &
                                              (/ d_1(1)-a3*G(4), d_1(2), d_1(3) /), dAdE, dBdE, dCdE, dDdE, dEdE, np, iknt_spl, &
                                              niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, F3, G_temp, out_coord)
		elseif (opt_flag .eq. 2) then
			call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, d_0-a3*G(1:3), d_1-a3*G(4:6), dAdE, dBdE, dCdE, &
                                               dDdE, dEdE, np, iknt_spl, niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, F3,    &
                                               G_temp, out_coord)	
		endif
        if (a3 .lt. tol/2.) then
			print*, 'No improvement likely.'
			d_0_opt = d_0
			d_1_opt = d_1
			exit
        endif
    enddo
	if (a3 .lt. tol/2.) then
		exit
	endif
    a2 = a3/2.
	if (opt_flag .eq. 0) then
		call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, d_0-a2*G(1:3), d_1, dAdE, dBdE, dCdE, dDdE, dEdE, &
			np, iknt_spl, niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, &
			F2, G_temp, out_coord)
	elseif (opt_flag .eq. 1) then
		call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, (/ d_0(1)-a2*G(1), d_0(2), d_0(3) /), &
                                          (/ d_1(1)-a2*G(4), d_1(2), d_1(3) /), dAdE, dBdE, dCdE, dDdE, dEdE, np, iknt_spl, &
                                          niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, F2, G_temp, out_coord)		
	elseif (opt_flag .eq. 2) then
		call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, d_0-a2*G(1:3), d_1-a2*G(4:6), dAdE, dBdE, dCdE, dDdE, &
                                           dEdE, np, iknt_spl, niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, F2, G_temp,        &
                                           out_coord)
	endif
    h1 = (F2 - F1)/a2; h2 = (F3 - F2)/(a3 - a2)
    h3 = (h2 - h1)/a3
    a0 = 0.5*(a2 - h1/h3)
	if (opt_flag .eq. 0) then
		call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, d_0-a0*G(1:3), d_1, dAdE, dBdE, dCdE, dDdE, dEdE, &
			np, iknt_spl, niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, &
			F0, G_temp, out_coord)
	elseif (opt_flag .eq. 1) then			
		call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, (/ d_0(1)-a0*G(1), d_0(2), d_0(3) /), &
                                          (/ d_1(1)-a0*G(4), d_1(2), d_1(3) /), dAdE, dBdE, dCdE, dDdE, dEdE, np, iknt_spl, &
                                          niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, F0, G_temp, out_coord)			
	elseif (opt_flag .eq. 2) then			
		call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, d_0-a0*G(1:3), d_1-a0*G(4:6), dAdE, dBdE, dCdE, dDdE, dEdE, &
			np, iknt_spl, niknt_spl, ddB1, ddC1, ddD1, 0, u_spl, opt_flag, &
			F0, G_temp, out_coord)
	endif
    if (F0 .lt. F3) then
        a = a0
    else
        a = a3
    endif
	if (opt_flag .eq. 1) then	
		d_0(1) = d_0(1) - a*G(1)
		d_1(1) = d_1(1) - a*G(4)
	elseif (opt_flag .eq. 2) then
		d_0 = d_0 - a*G(1:3)
		d_1 = d_1 - a*G(4:6)		
	endif
    i = i+1
enddo
d_0_opt = d_0
d_1_opt = d_1
print*, 'Number of iterations: ', i
40 print*, 'Optimum parameters: ', d_0_opt/dtor, d_1_opt/dtor

call thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt_spl, d_0_opt, d_1_opt, dAdE, dBdE, dCdE, dDdE, dEdE, &
	np, iknt_spl, niknt_spl, ddB1, ddC1, ddD1, 1, u_spl, opt_flag, &
	F_opt, G_opt, out_coord)	

print*, 'Objective: ', F_opt
print*, 'Gradient: ', G_opt

if (allocated(u)) deallocate(u)
if (allocated(iknt_spl)) deallocate(iknt_spl)
if (allocated(dAdE)) deallocate(dAdE)
if (allocated(dBdE)) deallocate(dBdE)
if (allocated(dCdE)) deallocate(dCdE)
if (allocated(dDdE)) deallocate(dDdE)
if (allocated(dEdE)) deallocate(dEdE)
if (allocated(dAdB1)) deallocate(dAdB1)
if (allocated(dBdB1)) deallocate(dBdB1)
if (allocated(dCdB1)) deallocate(dCdB1)
if (allocated(dDdB1)) deallocate(dDdB1)
if (allocated(dEdB1)) deallocate(dEdB1)
if (allocated(dAdC1)) deallocate(dAdC1)
if (allocated(dBdC1)) deallocate(dBdC1)
if (allocated(dCdC1)) deallocate(dCdC1)
if (allocated(dDdC1)) deallocate(dDdC1)
if (allocated(dEdC1)) deallocate(dEdC1)
if (allocated(dAdD1)) deallocate(dAdD1)
if (allocated(dBdD1)) deallocate(dBdD1)
if (allocated(dCdD1)) deallocate(dCdD1)
if (allocated(dDdD1)) deallocate(dDdD1)
if (allocated(dEdD1)) deallocate(dEdD1)
if (allocated(ddB1)) deallocate(ddB1)
if (allocated(ddC1)) deallocate(ddC1)
if (allocated(ddD1)) deallocate(ddD1)

end subroutine thk_ctrl_gen_driver_span


subroutine thk_ctrl_gen_driver_aux (cname, dev_flag, sec, uthk, thk, n, nknt, d_0, d_1, dAdE, dBdE, dCdE, dDdE, dEdE, &
    np, iknt, niknt, uthk_le, thk_le, n_le, nknt_le, dAdE_le, dBdE_le, dCdE_le, dDdE_le, dEdE_le, &
    ddB1, ddC1, ddD1, ddB1_le, ddC1_le, ddD1_le, iknt_le, niknt_le, out_flag, u_spl_le, np_le, u_spl_bl, np_bl, &
	uthk_te, thk_te, n_te, nknt_te, dAdE_te, dBdE_te, dCdE_te, dDdE_te, dEdE_te, ddB1_te, ddC1_te, &
	ddD1_te, iknt_te, niknt_te, u_spl_te, np_te, le_opt_flag, te_opt_flag, te_flag, grad_flag, u_spl, &
	F, G, out_coord)

implicit none

character (len = 20), intent(in) :: sec
character (len = 32), intent(in) :: cname
integer, intent(in) :: n, nknt, n_le, nknt_le, np, np_le, np_bl, niknt, niknt_le, out_flag, &
	iknt(niknt), iknt_le(niknt_le), n_te, nknt_te, np_te, niknt_te, iknt_te(niknt_te), te_opt_flag, &
	le_opt_flag, te_flag, grad_flag, dev_flag
real, intent(in) :: uthk(n), thk(n), uthk_le(n_le), thk_le(n_le), &
	d_1(3), ddB1(nknt-1, 5), ddC1(nknt-1, 5), ddD1(nknt-1, 5), ddB1_le(nknt_le-1, 5), &
	ddC1_le(nknt_le-1, 5), ddD1_le(nknt_le-1, 5), dAdE(nknt-1, niknt), dBdE(nknt-1, niknt), &
	dCdE(nknt-1, niknt), dDdE(nknt-1, niknt), dEdE(nknt-1, niknt), dAdE_le(nknt_le-1, niknt_le), &
	dBdE_le(nknt_le-1, niknt_le), dCdE_le(nknt_le-1, niknt_le), dDdE_le(nknt_le-1, niknt_le), &
	dEdE_le(nknt_le-1, niknt_le), &
	uthk_te(n_te), thk_te(n_te), ddB1_te(nknt_te-1, 5), ddC1_te(nknt_te-1, 5), ddD1_te(nknt_te-1, 5), &
	dAdE_te(nknt_te-1, niknt_te), dBdE_te(nknt_te-1, niknt_te), dCdE_te(nknt_te-1, niknt_te), &
	dDdE_te(nknt_te-1, niknt_te), dEdE_te(nknt_te-1, niknt_te), u_spl(nknt)
real, intent(inout) :: d_0(3)
real, intent(out) ::  F, G(6), out_coord(np, 12)

integer :: i, j, d3v_ang_flag_le, grad_end_le, grad_end_te, d1_flag, np_fine, fail_flag
real :: F_spl, G_spl(6), G_spl_temp(6), d1v_rot, d2v_rot, d3v_rot, d_0_rot_0(3), d_0_rot_1(3), G_le(6), &
	F_spl_eps, F_le, eps, d_0_eps(3), d_0_rot_0_eps(3), d_0_rot_1_eps(3), G_le_temp(6), &
	F_le_eps, u_spl_le(np_le), u_spl_bl(np_bl), out_coord_le(np_le, 12), out_coord_bl(np_bl, 12), &
	d_1_rot_0(3), d_1_rot_1(3), G_te(6), F_te, d_1_eps(3), d_1_rot_0_eps(3), d_1_rot_1_eps(3), &
	G_te_temp(6), F_te_eps, u_spl_te(np_te), out_coord_te(np_te, 12), coeff_bl(nknt-1, 5), &
	coeff_le(nknt_le-1, 5), coeff_te(nknt_te-1, 5), delu(nknt-1)
real, allocatable, dimension(:) :: u_spl_fine
real, allocatable, dimension(:, :) :: out_coord_fine	

d3v_ang_flag_le = 0
F = 0.; G = 0.
G_spl = 0.; G_le = 0.; G_te = 0.
d1_flag = 1
fail_flag = 0

np_fine = 1000
if (allocated(u_spl_fine)) deallocate(u_spl_fine)
Allocate(u_spl_fine(np_fine))
if (allocated(out_coord_fine)) deallocate(out_coord_fine)
Allocate(out_coord_fine(np_fine, 12))

delu = u_spl(2:nknt) - u_spl(1:nknt-1)
call thk_ctrl_gen_spl(uthk, thk, n, nknt, d_0, d_1, dAdE, dBdE, dCdE, &
	dDdE, dEdE, ddB1, ddC1, ddD1, u_spl_bl, np_bl, iknt, niknt, 1, 0, 0, &
	out_flag, 0, 1, d1_flag, F_spl, G_spl, out_coord_bl, coeff_bl, fail_flag, dev_flag)
if (out_flag .eq. 1 .and. fail_flag .ne. 0) print*, 'FATAL ERROR: Maximum/minimum thickness point not imposed.'
eps = 1.E-9
if (out_flag .eq. 1 .and. dev_flag .eq. 1) then
	do i = 1, np_fine
		u_spl_fine(i) = (i-1.)/(np_fine-1.)*(uthk(n) - uthk(1)) + uthk(1)
	enddo
	call thk_ctrl_gen_spl(uthk, thk, n, nknt, d_0, d_1, dAdE, dBdE, dCdE, &
		dDdE, dEdE, ddB1, ddC1, ddD1, u_spl_fine, np_fine, iknt, niknt, 1, 0, 0, &
		out_flag, 0, 1, d1_flag, F_spl, G_spl, out_coord_fine, coeff_bl, fail_flag, dev_flag)
	open (unit = 81, file = 'thk_dist_fine_spl.' // trim(adjustl(sec)) // '.' // trim(cname) // '.dat')!, Access = 'append',Status='old')
	write (81, '(12F40.12)') (out_coord_fine(i, :), i = 1, np_fine)
	close (81)
endif

if ((le_opt_flag .eq. 1 .or. le_opt_flag .eq. 2) .and. grad_flag .eq. 1) then ! .and. d1_flag .eq. 1) then
    do i = 1, 3
        d_0_eps = d_0
        d_0_eps(i) = d_0_eps(i) + eps
		call thk_ctrl_gen_spl(uthk, thk, n, nknt, d_0_eps, d_1, dAdE, dBdE, dCdE, &
			dDdE, dEdE, ddB1, ddC1, ddD1, u_spl_bl, np_bl, iknt, niknt, 1, 0, 0, &
			out_flag, 0, 1, d1_flag, F_spl_eps, G_spl_temp, out_coord_bl, coeff_bl, fail_flag, dev_flag)
        G_spl(i) = (F_spl_eps - F_spl)/eps
    enddo
endif
	
if (te_opt_flag .eq. 1 .and. grad_flag .eq. 1) then
    do i = 1, 1
        d_1_eps = d_1
        d_1_eps(i) = d_1_eps(i) + eps
		call thk_ctrl_gen_spl(uthk, thk, n, nknt, d_0, d_1_eps, dAdE, dBdE, dCdE, &
			dDdE, dEdE, ddB1, ddC1, ddD1, u_spl_bl, np_bl, iknt, niknt, 1, 0, 0, &
			out_flag, 0, 1, d1_flag, F_spl_eps, G_spl_temp, out_coord_bl, coeff_bl, fail_flag, dev_flag)
        G_spl(4) = (F_spl_eps - F_spl)/eps
    enddo
elseif (te_opt_flag .eq. 2 .and. grad_flag .eq. 1) then
    do i = 1, 3
        d_1_eps = d_1
        d_1_eps(i) = d_1_eps(i) + eps
		call thk_ctrl_gen_spl(uthk, thk, n, nknt, d_0, d_1_eps, dAdE, dBdE, dCdE, &
			dDdE, dEdE, ddB1, ddC1, ddD1, u_spl_bl, np_bl, iknt, niknt, 1, 0, 0, &
			out_flag, 0, 1, d1_flag, F_spl_eps, G_spl_temp, out_coord_bl, coeff_bl, fail_flag, dev_flag)
        G_spl(i+3) = (F_spl_eps - F_spl)/eps
    enddo	
endif
	
if (d1_flag .eq. 1) then
	d_0(3) = 6.*coeff_bl(1, 4)/delu(1)**3
	d1_flag = 0
endif
	
if (out_flag .eq. 1) out_coord(np_le+1 : np_le+np_bl, :) = out_coord_bl
	
call dnv_rot(d_0, d1v_rot, d2v_rot, d3v_rot)
d_0_rot_0 = (/ d1v_rot, d2v_rot, d3v_rot /)
d_0_rot_1 = (/ -d1v_rot, d2v_rot, -d3v_rot /)

call thk_ctrl_gen_spl(uthk_le, thk_le, n_le, nknt_le, d_0_rot_0, d_0_rot_1, dAdE_le, dBdE_le, dCdE_le, &
	dDdE_le, dEdE_le, ddB1_le, ddC1_le, ddD1_le, u_spl_le, np_le, iknt_le, niknt_le, d3v_ang_flag_le, 1, 0, &
	out_flag, 1, 1, d1_flag, F_le, G_le_temp, out_coord_le, coeff_le, fail_flag, dev_flag)
if (out_flag .eq. 1 .and. fail_flag .ne. 0)  print*, 'FATAL ERROR: Leading edge thickness distribution generation failed.'
G_le_temp(1) = G_le_temp(1)/d_0(1)**2
G_le_temp(2) = G_le_temp(2)/d_0(1)**2

if (out_flag .eq. 1 .and. dev_flag .eq. 1) then
	do i = 1, np_fine
		u_spl_fine(i) = (i-1.)/(np_fine-1)*thk_le(1)
	enddo
	call thk_ctrl_gen_spl(uthk_le, thk_le, n_le, nknt_le, d_0_rot_0, d_0_rot_1, dAdE_le, dBdE_le, dCdE_le, &
		dDdE_le, dEdE_le, ddB1_le, ddC1_le, ddD1_le, u_spl_fine, np_fine, iknt_le, niknt_le, d3v_ang_flag_le, 1, 0, &
		out_flag, 1, 1, d1_flag, F_le, G_le_temp, out_coord_fine, coeff_le, fail_flag, dev_flag)
	open (unit = 81, file = 'thk_dist_fine_le.'  // trim(adjustl(sec)) //  '.' // trim(cname) // '.dat')!, Access = 'append',Status='old')
	write (81, '(12F40.12)') (out_coord_fine(i, :), i = 1, np_fine)
	close (81)		
endif		

if (out_flag .eq. 1) then
	out_coord(1 : np_le, :) = out_coord_le
endif
	
if ((le_opt_flag .eq. 1 .or. le_opt_flag .eq. 2) .and. grad_flag .eq. 1) then
    do i = 1, 3
        d_0_eps = d_0
        d_0_eps(i) = d_0_eps(i) + eps
		call dnv_rot(d_0_eps, d1v_rot, d2v_rot, d3v_rot)
		d_0_rot_0_eps = (/ d1v_rot, d2v_rot, d3v_rot /)
        d_0_rot_1_eps = (/ -d_0_rot_0_eps(1), d_0_rot_0_eps(2), -d_0_rot_0_eps(3) /)
		call thk_ctrl_gen_spl(uthk_le, thk_le, n_le, nknt_le, d_0_rot_0_eps, d_0_rot_1_eps, dAdE_le, dBdE_le, dCdE_le, &
			dDdE_le, dEdE_le, ddB1_le, ddC1_le, ddD1_le, u_spl_le, np_le, iknt_le, niknt_le, d3v_ang_flag_le, 1, 0, &
			0, 1, 0, d1_flag, F_le_eps, G_le_temp, out_coord_le, coeff_le, fail_flag, dev_flag)	
        G_le(i) = (F_le_eps - F_le)/eps
    enddo
endif

if (te_opt_flag .eq. 1 .and. te_flag .eq. 1) then
	call dnv_rot(d_1, d1v_rot, d2v_rot, d3v_rot)
	d_1_rot_0 = (/ d1v_rot, d2v_rot, d3v_rot /)
	d_1_rot_1 = (/ -d1v_rot, d2v_rot, -d3v_rot /)
	call thk_ctrl_gen_spl(uthk_te, thk_te, n_te, nknt_te, d_1_rot_0, d_1_rot_1, dAdE_te, dBdE_te, dCdE_te, &
		dDdE_te, dEdE_te, ddB1_te, ddC1_te, ddD1_te, u_spl_te, np_te, iknt_te, niknt_te, 0, 0, 1, &
		out_flag, 2, 0, d1_flag, F_te, G_te_temp, out_coord_te, coeff_te, fail_flag, dev_flag)
	if (out_flag .eq. 1 .and. fail_flag .ne. 0)  print*, 'FATAL ERROR: Trailing edge thickness distribution generation failed.'
	if (out_flag .eq. 1) out_coord(np_le+np_bl+1 : np, :) = out_coord_te

	if (grad_flag .eq. 1) then
		do i = 1, 1
			d_1_eps = d_1
			d_1_eps(i) = d_1_eps(i) + eps
			call dnv_rot(d_1_eps, d1v_rot, d2v_rot, d3v_rot)
			d_1_rot_0_eps = (/ d1v_rot, d2v_rot, d3v_rot /)
			d_1_rot_1_eps = (/ -d_1_rot_0_eps(1), d_1_rot_0_eps(2), -d_1_rot_0_eps(3) /)
			call thk_ctrl_gen_spl(uthk_te, thk_te, n_te, nknt_te, d_1_rot_0_eps, d_1_rot_1_eps, dAdE_te, dBdE_te, dCdE_te, &
				dDdE_te, dEdE_te, ddB1_te, ddC1_te, ddD1_te, u_spl_te, np_te, iknt_te, niknt_te, 0, 0, 1, &
				0, 2, 0, d1_flag, F_te_eps, G_te_temp, out_coord_te, coeff_te, fail_flag, dev_flag)	
			G_te(i+3) = (F_te_eps - F_te)/eps
		enddo
	endif
	if (out_flag .eq. 1 .and. dev_flag .eq. 1) then
		do i = 1, np_fine
			u_spl_fine(i) = (i-1.)/(np_fine-1)*(1. - thk_te(1)) + thk_te(1)
		enddo
		call thk_ctrl_gen_spl(uthk_te, thk_te, n_te, nknt_te, d_1_rot_0, d_1_rot_1, dAdE_te, dBdE_te, dCdE_te, &
			dDdE_te, dEdE_te, ddB1_te, ddC1_te, ddD1_te, u_spl_fine, np_fine, iknt_te, niknt_te, 0, 0, 1, &
			out_flag, 2, 0, d1_flag, F_te, G_te_temp, out_coord_fine, coeff_te, fail_flag, dev_flag)
		open (unit = 81, file = 'thk_dist_fine_te.'  // trim(adjustl(sec)) //  '.' // trim(cname) // '.dat')!, Access = 'append',Status='old')
		write (81, '(12F40.12)') (out_coord_fine(i, :), i = 1, np_fine)
		close (81)		
	endif
	F = F + F_te
	G = G + G_te
elseif (te_opt_flag .eq. 2 .and. te_flag .eq. 1) then
	call dnv_rot(d_1, d1v_rot, d2v_rot, d3v_rot)
	d_1_rot_0 = (/ d1v_rot, d2v_rot, d3v_rot /)
	d_1_rot_1 = (/ -d1v_rot, d2v_rot, -d3v_rot /)
	call thk_ctrl_gen_spl(uthk_te, thk_te, n_te, nknt_te, d_1_rot_0, d_1_rot_1, dAdE_te, dBdE_te, dCdE_te, &
		dDdE_te, dEdE_te, ddB1_te, ddC1_te, ddD1_te, u_spl_te, np_te, iknt_te, niknt_te, 0, 0, 1, &
		out_flag, 2, 0, d1_flag, F_te, G_te_temp, out_coord_te, coeff_te, fail_flag, dev_flag)
	if (out_flag .eq. 1 .and. fail_flag .ne. 0)  print*, 'FATAL ERROR: Trailing edge thickness distribution generation failed.'
	if (out_flag .eq. 1) out_coord(np_le+np_bl+1 : np, :) = out_coord_te

	if (grad_flag .eq. 1) then
		do i = 1,3
			d_1_eps = d_1
			d_1_eps(i) = d_1_eps(i) + eps
			call dnv_rot(d_1_eps, d1v_rot, d2v_rot, d3v_rot)
			d_1_rot_0_eps = (/ d1v_rot, d2v_rot, d3v_rot /)
			d_1_rot_1_eps = (/ -d_1_rot_0_eps(1), d_1_rot_0_eps(2), -d_1_rot_0_eps(3) /)
			call thk_ctrl_gen_spl(uthk_te, thk_te, n_te, nknt_te, d_1_rot_0_eps, d_1_rot_1_eps, dAdE_te, dBdE_te, dCdE_te, &
				dDdE_te, dEdE_te, ddB1_te, ddC1_te, ddD1_te, u_spl_te, np_te, iknt_te, niknt_te, 0, 0, 1, &
				0, 2, 0, d1_flag, F_te_eps, G_te_temp, out_coord_te, coeff_te, fail_flag, dev_flag)	
			G_te(i+3) = (F_te_eps - F_te)/eps
		enddo
	endif
	if (out_flag .eq. 1 .and. dev_flag .eq. 1) then
		do i = 1, np_fine
			u_spl_fine(i) = (i-1.)/(np_fine-1)*(1. - thk_te(1)) + thk_te(1)
		enddo
		call thk_ctrl_gen_spl(uthk_te, thk_te, n_te, nknt_te, d_1_rot_0, d_1_rot_1, dAdE_te, dBdE_te, dCdE_te, &
			dDdE_te, dEdE_te, ddB1_te, ddC1_te, ddD1_te, u_spl_fine, np_fine, iknt_te, niknt_te, 0, 0, 1, &
			out_flag, 2, 0, d1_flag, F_te, G_te_temp, out_coord_fine, coeff_te, fail_flag, dev_flag)
		open (unit = 81, file = 'thk_dist_fine_te.'  // trim(adjustl(sec)) //  '.' // trim(cname) // '.dat')!, Access = 'append',Status='old')
		write (81, '(12F40.12)') (out_coord_fine(i, :), i = 1, np_fine)
		close (81)		
	endif	
	F = F + F_te
	G = G + G_te	
endif

! print*, 'F, F_spl, F_le', F, F_spl, F_le
F = F + F_le + F_spl
G = G + G_le + G_spl
! F = F_spl
! G = G_spl
! if (out_flag .eq. 1) then
	! print*, '!! KB F_spl', F_spl, 'F_le', F_le, 'F_te', F_te
! endif

end subroutine thk_ctrl_gen_driver_aux


subroutine thk_ctrl_gen_driver_aux_span (uthk, dev_flag, thk, n, nknt, d_0, d_1, dAdE, dBdE, dCdE, dDdE, dEdE, &
    np, iknt, niknt, ddB1, ddC1, ddD1, out_flag, u_spl, te_opt_flag, &
	F, G, out_coord)

implicit none

integer, intent(in) :: n, nknt, np, niknt, out_flag, &
	iknt(niknt), te_opt_flag, dev_flag
real, intent(in) :: uthk(n), thk(n), d_0(3), d_1(3), ddB1(nknt-1, 5), ddC1(nknt-1, 5), ddD1(nknt-1, 5), &
	dAdE(nknt-1, niknt), dBdE(nknt-1, niknt), dCdE(nknt-1, niknt), dDdE(nknt-1, niknt), dEdE(nknt-1, niknt)
real, intent(out) ::  F, G(6), out_coord(np, 12)

integer :: i, j, d3v_ang_flag, fail_flag
real :: F_spl, G_spl(6), G_spl_temp(6), F_spl_eps, eps, d_0_eps(3), u_spl(np), out_coord_temp(np, 12), &
	d_1_eps(3), coeff(nknt-1, 5)

F = 0.; G = 0.
G_spl = 0.
d3v_ang_flag = 1
call thk_ctrl_gen_spl(uthk, thk, n, nknt, d_0, d_1, dAdE, dBdE, dCdE, &
	dDdE, dEdE, ddB1, ddC1, ddD1, u_spl, np, iknt, niknt, d3v_ang_flag, 10, 0, &
	out_flag, 0, 1, 0, F_spl, G_spl, out_coord, coeff, fail_flag, dev_flag)
if (fail_flag .eq. 1) print*, 'WARNING: Failed to impose local minimum/maximum spanwise thickness constraints'
eps = 1.E-9
if (te_opt_flag .eq. 1) then
    do i = 1,1
        d_1_eps = d_1
        d_1_eps(i) = d_1_eps(i) + eps
		call thk_ctrl_gen_spl(uthk, thk, n, nknt, d_0, d_1, dAdE, dBdE, dCdE, &
			dDdE, dEdE, ddB1, ddC1, ddD1, u_spl, np, iknt, niknt, d3v_ang_flag, 10, 0, &
			out_flag, 0, 1, 0, F_spl_eps, G_spl_temp, out_coord_temp, coeff, fail_flag, dev_flag)
        G_spl(4) = (F_spl_eps - F_spl)/eps
    enddo
elseif (te_opt_flag .eq. 2) then
    do i = 1,3
        d_1_eps = d_1
        d_1_eps(i) = d_1_eps(i) + eps
		call thk_ctrl_gen_spl(uthk, thk, n, nknt, d_0, d_1, dAdE, dBdE, dCdE, &
			dDdE, dEdE, ddB1, ddC1, ddD1, u_spl, np, iknt, niknt, d3v_ang_flag, 10, 0, &
			out_flag, 0, 1, 0, F_spl_eps, G_spl_temp, out_coord_temp, coeff, fail_flag, dev_flag)
        G_spl(i+3) = (F_spl_eps - F_spl)/eps
    enddo	
endif
	
F = F + F_spl
G = G + G_spl

end subroutine thk_ctrl_gen_driver_aux_span


subroutine dnv_rot(d_0, d1v_rot, d2v_rot, d3v_rot)

implicit none

real, intent(in) :: d_0(3)
real, intent(out) :: d1v_rot, d2v_rot, d3v_rot
real :: d1v, d2v, d3v

d1v = d_0(1); d2v = d_0(2); d3v = d_0(3)
d1v_rot = -1./d1v
d2v_rot = -d2v/d1v**3
d3v_rot = (d3v*d1v - 3.*d2v**2)/d1v**5
! d1v_rot = -1./d1v
! d2v_rot = d2v/d1v**3
! d3v_rot = -(d3v*d1v - 3.*d2v**2)/d1v**5

end subroutine dnv_rot
