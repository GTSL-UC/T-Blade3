
!!		1-D Cubic Spline Interpolation
!		Added by Karthik Balasubramanian (12/2016)
!		Usage:
!			Spline parameter array t(i) is known or obtained from arclength subroutine.
!			Spline curve points array y(i) is known.
!			Subroutine spline determines spline derivative array at knots.
!			Use subroutine spl_eval and derivative array to interpolate spline.
!			Use subroutine dspl_eval and derivative array to evaluate spline derivative.
!		For space curves and spline/spline intersection of x1, y1 and x2, y2:
!			Determine derivative arrays for each space curve first using:
!				spline(x1, dxdt1, t1, n1, dspec1, dspecn)
!				spline(y1, dydt1, t1, n1, dspec1, dspecn)
!				spline(x2, dxdt2, t2, n2, dspec1, dspecn)
!				spline(y2, dydt2, t2, n2, dspec1, dspecn)
!			To evaluate interpolated points at tt1:
!				spl_eval(tt1, x1, dxdt1, t1, n1)
!				spl_eval(tt1, y1, dydt1, t1, n1)
!			To determine intersection point tt1, tt2:
!				spl_intersect(tt1, tt2, x1, dxdt1, y1, dydt1, t1, n1, x2, dxdt2, y2, dydt2, t2, n2)

subroutine arclength(t,y,arcl,n) ! used by(3dbgb, bladegrid2d, ellipgrid)

!		This subroutine calculates the arc lengths at each point
!		for a specified set of (t, y) points.

    implicit none

    !!		Inputs
    !			t		Real array,	independent variable values.
    !			y		Real array,	dependent variable values.
    !			n		Integer,	number of spline points.

    integer, intent (in) :: n
    real, intent (in) :: t(n), y(n)

    !!		Outputs
    !			arcl	Real array,	arc lengths at each (t, y).
    real, intent (out) :: arcl(n)

    !!		Other local variables
    integer :: i

    arcl(1) = 0.
    do i = 2, n
        arcl(i) = arcl(i-1) + sqrt((t(i)-t(i-1))**2 + (y(i)-y(i-1))**2)
    enddo

    return

end subroutine arclength



!
! Subroutine to compute the arc lengths at each points for a
! specified set of (x,y,z) points
!
subroutine :rclength_3D(n, x, y, z, arcl)
    implicit none

    integer,        intent(in)          :: n
    real,           intent(in)          :: x(n), y(n), z(n)
    real,           intent(inout)       :: arcl(n)

    ! Local variables
    integer                             :: i


    ! Arclength starts at zero
    arcl(1)     = 0.0

    do i = 2, n
        arcl(i) = arcl(i - 1) + sqrt((x(i) - x(i - 1))**2 + (y(i) - y(i - 1))**2 + (z(i) - z(i - 1))**2) 
    end do


end subroutine arclength_3D




subroutine tridiag_solve(d, ld, ud, r, n) ! used by spline

!		This subroutine solves a tridiagonal linear system.
use errors
implicit none

!!		Inputs
!			n		Integer, 	matric order.
!			d		Real array,	diagonal elements.
!			ld		Real array,	lower diagonal elements.
!			ud		Real array,	upper diagonal elements.
!			r		Real array,	RHS elements.
!!		Outputs
!			r		Real array,	RHS elements are replaced.
integer, intent (in) :: n
real, intent (inout) :: d(n), ld(n), ud(n), r(n)

!!		Other local variables
integer :: k
real :: m
real,   parameter    :: tol = 10e-10
character(:),   allocatable :: error_msg, dev_msg


do k = 2, n
    ! If d(k - 1) = 0
    ! Compare to a tolerance to avoid floating point errors in an equality comparison
    if (abs(d(k-1)) .le. tol) then
        error_msg   = 'tridiag_solve failed - zero diagonal element'
        dev_msg     = 'Check subroutine tridiag_solve in spline.f90'
        call fatal_error(error_msg, dev_msg = dev_msg)
    end if
    m = ld(k)/d(k-1)
    d(k) = d(k) - m*ud(k-1)
    r(k) = r(k) - m*r(k-1)
enddo

! If d(n) = 0 
! Compare to a tolerance to avoid floating point errors in an equality comparison
if (abs(d(n)) .le. tol) then
    error_msg   = 'tridiag_solve failed - zero diagonal element'
    dev_msg     = 'Check subroutine tridiag_solve in spline.f90'
    call fatal_error(error_msg, dev_msg = dev_msg)
end if

r(n) = r(n)/d(n)

do k = n-1, 1, -1
    r(k) = (r(k) - ud(k)*r(k+1))/d(k)
enddo

return

end subroutine tridiag_solve


function find_knt (tt, t, n)

implicit none

integer, intent (in) :: n
real, intent (in) :: t(n), tt
integer :: knt1, knt2, find_knt, m

knt1 = 1
knt2 = n

do while (knt2-knt1 .gt. 1)
    m = (knt2+knt1)/2
    if (tt .lt. t(m)) then
        knt2 = m
    else
        knt1 = m
    endif
    if (tt .eq. t(m)) exit
enddo

find_knt = knt1

return

end function find_knt


subroutine spline(y, dydt, t, n, dspec1, dspecn) ! Used by(3dbgb, bladegen, bladegrid2D, ellipgrid, funcNsubs, lespline)

!		This subroutine calculates spline first derivatives at knots for y(t).
!		First derivative, zero 2nd and/or 3rd derivatives at spline begin t(1)
!		and end points t(n) are specified.
!		Spline parameter t can be calculated using arclength subroutine.
!		Reference: 
!		Kreyszig, E., Advanced Engineering Mathematics, 10th Ed., John Wiley and Sons, 2011.

implicit none

!!		Inputs
!			t		Real array,	independent variable values.
!			y		Real array,	dependent variable values.
!			n		Integer,	number of spline points.
!			dspec1	Real,		first derivative at spline begin t(1),
!								zero 2nd derivative at t(1) if = 999.0,
!								zero 3rd derivative at t(1) if = -999.0.
!			dspecn	Real,		first derivative at spline end t(n),
!								zero 2nd derivative at t(n) if = 999.0,
!								zero 3rd derivative at t(n) if = -999.0.
integer, intent (in) :: n
real, intent (in) :: t(n), y(n), dspec1, dspecn

!!		Outputs
!			dydt	Real array,	spline derivatives at knots.
real, intent(out) :: dydt(n)

!!		Other local variables
integer :: i
real :: d(n), ld(n), ud(n), dt(n-1), dy(n-1), r(n)

do i = 1, n-1
    dt(i) = t(i+1)-t(i)
    dy(i) = y(i+1)-y(i)
enddo

if(n.eq.2) then
    ! Linear interpolation if only two points are present.
    dydt(1) = dy(1)/dt(1)
    dydt(2) = dydt(1)
    return
endif

do i = 2, n-1
    ud(i) = dt(i-1)
    d(i) = 2.*(dt(i) + dt(i-1))
    ld(i) = dt(i)
    r(i) = 3.*(dy(i-1)*dt(i)/dt(i-1) + dy(i)*dt(i-1)/dt(i))
enddo

! Implementing specified 1st derivative
d(1) = 1.; ud(1) = 0.; r(1) = dspec1
d(n) = 1.; ld(1) = 0.; r(n) = dspecn

! Implementing zero 2nd derivative
if (dspec1 .eq. 999.0) then 
    d(1) = 2.; ud(1) = 1.; r(1) = 3.*dy(1)/dt(1)
endif
if (dspecn .eq. 999.0) then 
    d(n) = 2.; ld(n) = 1.; r(n) = 3.*dy(n-1)/dt(n-1)
endif
    
! Implementing zero 3rd derivative
if (dspec1 .eq. -999.0) then 
    d(1) = 1.; ud(1) = 1.; r(1) = 2.*dy(1)/dt(1)
endif
if (dspecn .eq. -999.0) then 
    d(n) = 1.; ld(n) = 1.; r(n) = 2.*dy(n-1)/dt(n-1)
endif

call tridiag_solve(d,ld,ud,r,n)

dydt = r

return

end subroutine spline


function spl_eval(tt, y, dydt, t, n) ! used by(3dbgb, bladestack), used by spl_inv, inters

!		This function evaluates spline value y(tt) using knot first derivatives.
!		Spline parameter t can be calculated using arclength subroutine.
!		First derivatives at spline knots can then be obtained from spline subroutine.
!		Reference:
!		Kreyszig, E., Advanced Engineering Mathematics, 10th Ed., John Wiley and Sons, 2011.
use errors
implicit none

!!		Inputs
!			t		Real array,	independent variable values.
!			y		Real array,	dependent variable values.
!			dydt	Real array,	splined y coefficients.
!			n		Integer,	number of spline points.
!			tt		Real,		t-value at which spline is evaluated.
integer, intent (in) :: n
real, intent (in) :: y(n), dydt(n), t(n), tt

!!		Other local variables
integer :: knt1, knt2, find_knt
real :: dt, dy, a(4), dt2, frac, spl_eval
real,   parameter   :: tol = 10e-10
character(:),   allocatable :: error_msg, dev_msg


knt1 = find_knt(tt, t, n)
knt2 = knt1+1

dt = t(knt2) - t(knt1)
dy = y(knt2) - y(knt1)
dt2 = tt-t(knt1)
frac = dt2/dt


! If dt = 0
! Compare to a tolerance to avoid floating point errors in an equality comparison
if (abs(dt) .le. tol) then
    error_msg   = 'spl_eval failed - identical knot locations'
    dev_msg     = 'Check function spl_eval in spline.f90'
    call fatal_error(error_msg, dev_msg = dev_msg)
end if

a(1) = y(knt1)
if (tt .eq. t(knt1)) then
    spl_eval = a(1); return
endif
a(2) = dydt(knt1)
a(3) = (3.*dy*(frac**2))-((dydt(knt2)+(2.*dydt(knt1)))*frac*dt2)
a(4) = (-2.*dy*(frac**3))+((dydt(knt2)+dydt(knt1))*(frac**2)*dt2)
spl_eval = a(1) + (a(2)*dt2) + a(3) + a(4)

return

end function spl_eval


function dspl_eval(tt,y,dydt,t,n) ! used by(3dbgb), spl_inv, inters

!		This function evaluates derivative of spline df(tt)/dx using knot first derivatives.
!		Spline parameter t can be calculated using arclength subroutine.
!		First derivatives at spline knots can then be obtained from spline subroutine.
!		Reference:
!		Kreyszig, E., Advanced Engineering Mathematics, 10th Ed., John Wiley and Sons, 2011.
use errors
implicit none

!!		Inputs
!			t		Real array,	independent variable values.
!			y		Real array,	dependent variable values.
!			dydt	Real array,	dydt/dx values.
!			n		Integer,	number of spline points.
!			tt		Real,		t-value at which spline is evaluated.
integer, intent (in) :: n
real, intent (in) ::y(n), dydt(n), t(n), tt

!!		Other local variables
integer :: knt1, knt2, find_knt
real :: dt, dy, a(3), dt2, frac, dspl_eval
real,   parameter   :: tol = 10e-10
character(:),   allocatable :: error_msg, dev_msg

knt1 = find_knt(tt, t, n)
knt2 = knt1+1

dt = t(knt2) - t(knt1)
dy = y(knt2) - y(knt1)
dt2 = tt-t(knt1)
frac = dt2/dt

! If dt = 0
! Compare to a tolerance to avoid floating point errors in an equality comparison
if (abs(dt) .le. tol) then
    error_msg   = 'dspl_eval failed - identical knot locations'
    dev_msg     = 'Check function dspl_eval in spline.f90'
    call fatal_error(error_msg, dev_msg = dev_msg)
end if

a(1) = dydt(knt1)
if (tt .eq. t(knt1)) then
    dspl_eval = a(1); return
endif
a(2) = 2.*frac*((3.*dy/dt)-(dydt(knt2)+(2.*dydt(knt1))))
a(3) = 3.*(frac**2)*((-2.*dy/dt)+(dydt(knt2)+dydt(knt1)))
dspl_eval = a(1) + a(2) + a(3)

end function dspl_eval


subroutine spl_inv(tt,yy,y,dydt,t,n) ! used by(3dbgb, bladestack)

!		This subroutine inverts the spline to determine tt for an input spline 
!		value yy using first derivatives at spline knots. An initial guess 
!		for tt is specified since tt(yy) may be multi-valued.
!		Spline parameter t can be calculated using arclength subroutine.
!		First derivatives at spline knots can then be obtained from spline subroutine.
use errors
implicit none

!!		Inputs
!			t		Real array,	independent variable values.
!			y		Real array,	dependent variable values.
!			dydt	Real array,	splined y coefficients.
!			n		Integer,	number of spline points.
!			yy		Real,		specified spline value.
!			tt		Real,		initial guess for t at which yy occurs.
integer, intent (in) :: n
real, intent (in) :: y(n), dydt(n), t(n), yy

!!		Outputs
!			tt		Real,		t value at which yy occurs.
real, intent (inout) :: tt

!!		Other local variables
integer :: iter, maxiter
real :: spl_eval, dspl_eval, dt_newt, y_newt, dy_newt, tol, mint, maxt
character(:),   allocatable :: error_msg, warning_msg, dev_msg

mint = minval(t)
maxt = maxval(t)
if (tt .lt. mint) then
    warning_msg = 'Bad initial guess provided. Using minimum value.'
    dev_msg     = 'Check subroutine spl_inv in spline.f90'
    call warning(warning_msg, dev_msg = dev_msg)
    tt = mint
elseif (tt .gt. maxt) then
    warning_msg = 'Bad initial guess provided. Using maximum value.'
    dev_msg     = 'Check subroutine spl_inv in spline.f90'
    call warning(warning_msg, dev_msg = dev_msg)
    tt = maxt
endif

tol = 1.0e-5
maxiter = 25 ! increased from 10 for radial cases
do iter = 1, maxiter
    y_newt = spl_eval(tt, y, dydt, t, n) - yy
    dy_newt = dspl_eval(tt, y, dydt, t, n)
    dt_newt = -y_newt/dy_newt
    tt = tt + 0.8*dt_newt
    if (tt < mint) then
        warning_msg = 'spl_inv - spline parameter clipped at spline begin'
        dev_msg     = 'Check subroutine spl_inv in spline.f90'
        call warning(warning_msg, dev_msg = dev_msg)
        tt = mint
    elseif(tt > maxt) then
        warning_msg = 'spl_inv- spline parameter clipped at spline end'
        dev_msg     = 'Check subroutine spl_inv in spline.f90'
        call warning(warning_msg, dev_msg = dev_msg)
        tt = maxt
    endif
    if(abs(dt_newt/(t(n)-t(1))) .lt. tol) return
enddo

error_msg   = 'spl_inv - spline parameter not determined. Maximum iteration reached.'
dev_msg     = 'Check subroutine spl_inv in spline.f90'
call error(error_msg, dev_msg)
return

end subroutine spl_inv

   
subroutine spl_discjoint(y,dydt,t,n) ! used by(3dbgb)

!		This subroutine calculates spline first derivatives at knots for y(t) 
!		and allows zero second derivative at segment joints.
!		Consecutive identical y values indicate segment joints.
!		Spline parameter t can be calculated using arclength subroutine.
use errors
implicit none

!!		Inputs
!			t		Real array,	independent variable values.
!			y		Real array,	dependent variable values.
!			n		Integer,	number of spline points.
integer, intent (in) :: n
real, intent (in) :: y(n), t(n)

!!		Outputs
!			dydt	Real array,	splined y coefficients.
real, intent (out) :: dydt(n)

!!		Other local variables
integer :: i, seg_start, seg_end, n0
real,   parameter   :: tol = 10e-10
character(:),   allocatable :: error_msg, dev_msg

! If t(1) = t(2)
! If t(n) = t(n - 1)
! Compare to a tolerance to avoid floating point errors in an equality comparison
if(abs(t(1) - t(2)) .le. tol) then
    error_msg   = 'spl_discjoint - identical knot locations at spline begin'
    dev_msg     = 'Check subroutine spl_discjoint in spline.f90'
    call fatal_error(error_msg, dev_msg = dev_msg)
end if
if(abs(t(n) - t(n-1)) .le. tol) then
    error_msg   = 'spl_discjoint - identical knot locations at spline end'
    dev_msg     = 'Check subroutine spl_discjoint in spline.f90'
    call fatal_error(error_msg, dev_msg = dev_msg)
end if

seg_start = 1
do i = 2, n-2
    if(abs(t(i) - t(i+1)) .le. tol) then
        n0 = i-seg_start+1
        seg_end = seg_start+n0-1
        ! Implementing zero second derivative segment end conditions
        call spline(y(seg_start), dydt(seg_start), t(seg_start), n0, 999.0, 999.0)
        seg_start = i+1
    endif
enddo

n0 = n-seg_start+1
! Implementing zero second derivative segment and zero third derivative end conditions
! print*, 'seg_start, seg_end', seg_start, seg_end
call spline(y(seg_start), dydt(seg_start), t(seg_start), n0, 999.0, -999.0)

return

end subroutine spl_discjoint


subroutine spl_intersect(ia,tt1, tt2, x1, dxdt1, y1, dydt1, t1, n1, x2, dxdt2, y2, dydt2, t2, n2) ! used by(3dbgb)
    use errors  
    use file_operations

!		This subroutine determines intersection of two cubic splines t1 and t2 in x-y space.
!		Spline parameters tt1, tt2 at intersection point are calculated.
!		Initial guesses for tt1 and tt2 are required.
!		Spline parameter t can be calculated using arclength subroutine.
!		First derivatives at spline knots can then be obtained from spline subroutine.

implicit none

!!		Inputs
!			t1, t2	Real array,	spline parameters at knots.
!			x1, x2	Real array,	spline x values at knots.
!			y1, y2	Real array,	spline y values at knots.
!			n1, n2	Integer,	number of spline knots.
!			dydt1,	Real array,	spline derivatives at knots.
!			dydt2
!			tt1,	Real,		Initial guesses of spline parameters
!			tt2					at intersection.
integer,    intent(in)   :: ia
integer, intent (in) :: n1, n2
real, intent (in) :: x1(n1), y1(n1), t1(n1), dxdt1(n1), dydt1(n1), &
                     x2(n2), y2(n2), t2(n2), dxdt2(n2), dydt2(n2)

!!		Outputs
!			tt1,	Real,		spline parameters at intersection.
!			tt2					
real, intent (inout) :: tt1, tt2

!!		Other local variables
real, parameter :: GR = 0.5*(3.-sqrt(5.)), tol = 1.0e-12
logical :: GS_flag, trunc1knt1, trunc1kntn, trunc2knt1, trunc2kntn
real :: dt1, dt2, r, ra, rb, rt1, rt2, F, Fa, Fb, Ft1, Ft2, &
        F1, F2, J11, J12, J21, J22, xx1, xx2, yy1, yy2, tt1old, tt2old, &
        mint1, mint2, maxt1, maxt2
integer :: iter, i, l, nopen
real :: dspl_eval
character(10)                   :: warning_arg
character(len = :), allocatable :: log_file, warning_msg, dev_msg
logical                         :: file_open

write(warning_arg, '(I5)') ia

mint1 = minval(t1); maxt1 = maxval(t1)
mint2 = minval(t2); maxt2 = maxval(t2)
if (tt1 .lt. mint1) then
    warning_msg = 'Bad initial guess provided for spline 1 in section '//adjustl(trim(warning_arg))//'. Using minimum value.'
    dev_msg     = 'Check subroutine spl_intersect in spline.f90'
    call warning(warning_msg, dev_msg = dev_msg)
    tt1 = mint1
elseif (tt1 .gt. maxt1) then
    warning_msg = 'Bad initial guess provided for spline 1 in section '//adjustl(trim(warning_arg))//'. Using maximum value.'
    dev_msg     = 'Check subroutine spl_intersect in spline.f90'
    call warning(warning_msg, dev_msg = dev_msg)
    tt1 = maxt1
endif
if (tt2 .lt. mint2) then
    warning_msg = 'Bad initial guess provided for spline 2 in section '//adjustl(trim(warning_arg))//'. Using minimum value.'
    dev_msg     = 'Check subroutine spl_intersect in spline.f90'
    call warning(warning_msg, dev_msg = dev_msg)
    tt2 = mint2
elseif (tt2 .gt. maxt2) then
    warning_msg = 'Bad initial guess provided for spline 2 in section '//adjustl(trim(warning_arg))//'. Using maximum value.'
    dev_msg     = 'Check subroutine spl_intersect in spline.f90'
    call warning(warning_msg, dev_msg = dev_msg)
    tt2 = maxt2
endif

dt1 = 0.0
dt2 = 0.0
l = 0

do iter = 1, 100
    GS_flag = .true.
    tt1old = tt1
    tt2old = tt2
    ra = 0.0
    r = ra; call feval; Fa = F
    rb = 1.1
    r = rb; call feval; Fb = F
    do i = 1, 1
        rt1 = i*1.0
        r = rt1; call feval; Ft1 = F
        if ((Ft1 .lt. Fa) .and. (Ft1 .lt. Fb)) exit
    enddo
    if ((Ft1 .ge. Fa) .or. (Ft1 .ge. Fb)) then
        r = 1.; i = 0; GS_flag = .false.
    endif
    if (GS_flag) then
    l = l+1
        if (rt1-ra .lt. rb-rt1) then
            rt2 = rt1+(GR*(rb-rt1))
        else
            rt2 = rt1; rt1 = rt1-(GR*(rt1-ra))
        endif
        r = rt1; call feval; Ft1 = F
        r = rt2; call feval; Ft2 = F
        do i = 1, 50
            if (Ft1 .lt. Ft2) then
                rb = rt2; rt2 = rt1
                rt1 = rt2-(GR*(rt2-ra))
                Ft2 = Ft1;
                r = rt1; call feval; Ft1 = F
            else
                ra = rt1; rt1 = rt2
                rt2 = rt1+(GR*(rb-rt1))
                Ft1 = Ft2
                r = rt2; call feval; Ft2 = F
            endif
            if (abs(rb-ra) .le. 1.0e-5) exit
        enddo
        if (Ft1 .lt. Ft2) then
            r = rt1; F = Ft1
        else
            r = rt2; F = Ft2
        endif
    endif
    call feval
    J11 = -dspl_eval(tt1, x1, dxdt1, t1, n1)
    J12 = dspl_eval(tt2, x2, dxdt2, t2, n2)
    J21 = -dspl_eval(tt1, y1, dydt1, t1, n1)
    J22 = dspl_eval(tt2, y2, dydt2, t2, n2)
    dt1 = -(F1*J22 - J12*F2)/(J11*J22 - J12*J21)
    dt2 = -(J11*F2 - F1*J21)/(J11*J22 - J12*J21)
    if(abs(dt1) .lt. tol*(t1(n1)-t1(1)) .and. abs(dt2) .lt. tol*(t2(n2)-t2(1))) then
        ! write (*, '(3(A, I3))'), 'Golden Search executed ', l, ' out of ', iter, ' times.'
        return
    endif
enddo

! TODO: No condition for failure?
!write(*, *) 'SPL_INTERSECT FAILED: Convergence failed. Residuals normalized to arclength:', &
!	F1/min(t1(n1), t2(n2)), F2/min(t1(n1), t2(n2))
call log_file_exists(log_file, nopen, file_open)
if(trunc1knt1) then
    warning_msg = 'spl_intersect - splines may not intersect for section '//adjustl(trim(warning_arg))//'. Knot 1 of spline 1 is closest possible to spline 2.'
    dev_msg     = 'Check subroutine spl_intersect in spline.f90'
    call warning(warning_msg, dev_msg = dev_msg)
    !write(nopen,*) 'spl_intersect - splines may not intersect. Knot 1 of spline 1 is closest possible to spline 2.'
end if
if(trunc1kntn) then
    warning_msg = 'spl_intersect - splines may not intersect for section '//adjustl(trim(warning_arg))//'. End knot of spline 1 is closest possible to spline 2.'
    dev_msg     = 'Check subroutine spl_intersect in spline.f90'
    call warning(warning_msg, dev_msg = dev_msg)
    !write(nopen,*) 'spl_intersect - splines may not intersect. End knot of spline 1 is closest possible to spline 2.'
end if
if(trunc2knt1) then
    warning_msg = 'spl_intersect - splines may not intersect for section '//adjustl(trim(warning_arg))//'. Knot 1 of spline 2 is closest possible to spline 1.'
    dev_msg     = 'Check subroutine spl_intersect in spline.f90'
    call warning(warning_msg, dev_msg = dev_msg)
    !write(nopen,*) 'spl_intersect - splines may not intersect. Knot 1 of spline 2 is closest possible to spline 1.'
end if
if(trunc2kntn) then
    warning_msg = 'spl_intersect - Splines may not intersect for section '//adjustl(trim(warning_arg))//'. End knot of spline 2 is closest possible to spline 1.'
    dev_msg     = 'Check subroutine spl_intersect in spline.f90'
    call warning(warning_msg, dev_msg = dev_msg)
    !write(nopen,*) 'spl_intersect - Splines may not intersect. End knot of spline 2 is closest possible to spline 1.'
end if

call close_log_file(nopen, file_open)

! print*, 'tt1, tt2'
! write (*, *), tt1, tt2
! print*, 't1'
! write(*, '(F20.16)'), t1
! print*, 't2'
! write(*, '(F20.16)'), t2

contains
    subroutine feval
    real :: spl_eval
    trunc1knt1 = .false.; trunc1kntn = .false.
    trunc2knt1 = .false.; trunc2kntn = .false.
    tt1 = tt1old + r*dt1
    tt2 = tt2old + r*dt2
    if(tt1.lt.t1(1)) then
        tt1 = t1(1); trunc1knt1 = .true.
    endif
    if(tt1.gt.t1(n1)) then
        tt1 = t1(n1); trunc1kntn = .true.
    endif
    if(tt2.lt.t2(1)) then
        tt2 = t2(1); trunc2knt1 = .true.
    endif
    if(tt2.gt.t2(n2)) then
        tt2 = t2(n2); trunc2kntn = .true.
    endif
    xx1 = spl_eval(tt1, x1, dxdt1, t1, n1)
    xx2 = spl_eval(tt2, x2, dxdt2, t2, n2)
    yy1 = spl_eval(tt1, y1, dydt1, t1, n1)
    yy2 = spl_eval(tt2, y2, dydt2, t2, n2)
    F1 = xx2 - xx1
    F2 = yy2 - yy1
    F = (F1**2)+(F2**2)
    end subroutine feval

end subroutine spl_intersect
