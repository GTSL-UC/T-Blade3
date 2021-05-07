!
! Subroutine to compute the arc lengths at each point for a
! specified set of (x,y) points
!
! Input parameters: t   - independent variable values
!                   y   - dependent variable values
!                   n   - number of spline points
!------------------------------------------------------------------------------------
subroutine arclength(n, t, y, arcl)
    implicit none

    integer,    intent (in)                 :: n
    real,       intent (in)                 :: t(n), y(n)
    real,       intent (inout)              :: arcl(n)

    ! Local variables
    integer                                 :: i


    ! Arclength starts at zero
    arcl(1) = 0.0

    do i = 2, n
        arcl(i) = arcl(i-1) + sqrt((t(i)-t(i-1))**2 + (y(i)-y(i-1))**2)
    end do


end subroutine arclength
!------------------------------------------------------------------------------------






!
! Subroutine to compute the arc lengths at each points for a
! specified set of (x,y,z) points
!
! Input parameters: n   - number of spline points
!                   x   - x coordinates
!                   y   - y coordinates
!                   z   - z coordinates
!
!------------------------------------------------------------------------------------
subroutine arclength_3D(n, x, y, z, arcl)
    implicit none

    integer,        intent(in)              :: n
    real,           intent(in)              :: x(n), y(n), z(n)
    real,           intent(inout)           :: arcl(n)

    ! Local variables
    integer                                 :: i


    ! Arclength starts at zero
    arcl(1)     = 0.0

    do i = 2, n
        arcl(i) = arcl(i - 1) + sqrt((x(i) - x(i - 1))**2 + (y(i) - y(i - 1))**2 + (z(i) - z(i - 1))**2) 
    end do


end subroutine arclength_3D
!------------------------------------------------------------------------------------






!
! Function to find intermediate knot for spline evaluation
!
!------------------------------------------------------------------------------------
function find_knt (tt, t, n)
    implicit none

    integer,        intent (in)             :: n
    real,           intent (in)             :: t(n), tt

    ! Local variables
    integer                                 :: knt1, knt2, find_knt, m


    knt1 = 1
    knt2 = n

    do while (knt2-knt1 > 1)

        m = (knt2+knt1)/2
        if (tt .lt. t(m)) then
            knt2 = m
        else
            knt1 = m
        end if
        if (tt .eq. t(m)) exit

    end do

    find_knt = knt1


end function find_knt
!------------------------------------------------------------------------------------






!
! Subroutine to compute spline first derivatives at knots for y(t)
! Spline parameter 't' is computed using the arclength subroutine
! 
! Input parameters: t               - independent variable values
!                   y               - dependent variable values
!                   n               - number of spline points
!                   dspec1          - boundary condition at spline beginning
!                                     a) can be specified as first derivative at t(1)
!                                     b) can be specified as 999.0 for a zero second
!                                        derivative at t(1)
!                                     c) can be specified as -999.0 for a zero third
!                                        derivative at t(1)
!                   dspecn          - boundary condition at spline end
!                                     a) can be specified as first derivative at t(n)
!                                     b) can be specified as 999.0 for a zero second
!                                        derivative at t(n)
!                                     c) can be specified as -999.0 for a zero third
!                                        derivative at t(n)
!
! References: Kreyszig, E., "Numerical Methods in General", Advanced Engineering Mathematics,
!             8th Ed., John Wiley and Sons, 1999, pp. 861-866
!
!------------------------------------------------------------------------------------
subroutine spline(n, y, dydt, t, dspec1, dspecn) 
    use funcNsubs
    implicit none

    integer,        intent(in)              :: n
    real,           intent(in)              :: t(n), y(n), dspec1, dspecn
    real,           intent(inout)           :: dydt(n)
    
    ! Local variables
    integer                                 :: i
    real                                    :: d(n), ld(n), ud(n), dt(n-1), dy(n-1), r(n)


    do i = 1, n-1
        dt(i) = t(i+1)-t(i)
        dy(i) = y(i+1)-y(i)
    end do

    ! Linear interpolation if only two points are present
    if(n == 2) then
        dydt(1) = dy(1)/dt(1)
        dydt(2) = dydt(1)
    end if

    do i = 2, n-1
        ud(i) = dt(i-1)
        d(i) = 2.*(dt(i) + dt(i-1))
        ld(i) = dt(i)
        r(i) = 3.*(dy(i-1)*dt(i)/dt(i-1) + dy(i)*dt(i-1)/dt(i))
    end do

    ! Implementing specified 1st derivative
    d(1) = 1.; ud(1) = 0.; r(1) = dspec1
    d(n) = 1.; ld(1) = 0.; r(n) = dspecn

    ! Implementing zero 2nd derivative
    if (dspec1 == 999.0) then 
        d(1) = 2.; ud(1) = 1.; r(1) = 3.*dy(1)/dt(1)
    end if
    if (dspecn == 999.0) then 
        d(n) = 2.; ld(n) = 1.; r(n) = 3.*dy(n-1)/dt(n-1)
    end if
        
    ! Implementing zero 3rd derivative
    if (dspec1 == -999.0) then 
        d(1) = 1.; ud(1) = 1.; r(1) = 2.*dy(1)/dt(1)
    end if
    if (dspecn == -999.0) then 
        d(n) = 1.; ld(n) = 1.; r(n) = 2.*dy(n-1)/dt(n-1)
    end if


    ! tridiag_solve in funcNsubs.f90
    call tridiag_solve(d,ld,ud,r,n)

    dydt = r


end subroutine spline
!------------------------------------------------------------------------------------






!
! Function to evaluate spline value y(tt) using knot first derivatives
!
! Input parameters: t       - independent variable values
!                   y       - dependent variable values
!                   dydt    - knot first derivatives
!                   n       - number of spline points
!                   tt      - t value at which spline is to be evaluated
!
! References: Kreyszig, E., "Numerical Methods in General", Advanced Engineering Mathematics,
!             8th Ed., John Wiley and Sons, 1999, pp. 861-866
!
!------------------------------------------------------------------------------------
function spl_eval(n, tt, y, dydt, t, print_from)
    use errors
    implicit none
    
    integer,        intent (in)             :: n
    real,           intent (in)             :: y(n), dydt(n), t(n), tt
    logical                                 :: print_from

    ! Local variables
    integer                                 :: knt1, knt2, find_knt
    real                                    :: dt, dy, a(4), dt2, frac, spl_eval
    real,   parameter                       :: tol = 10e-10
    character(:),   allocatable             :: error_msg, dev_msg


    knt1 = find_knt(tt, t, n)
    knt2 = knt1 + 1

    dt   = t(knt2) - t(knt1)
    dy   = y(knt2) - y(knt1)
    dt2  = tt - t(knt1)
    frac = dt2/dt


    ! If dt = 0
    if (abs(dt) <= tol) then
        error_msg   = 'spl_eval failed - identical knot locations'
        dev_msg     = 'Check function spl_eval in spline.f90'
        call fatal_error(error_msg, dev_msg = dev_msg)
    end if

    a(1) = y(knt1)
    if (tt == t(knt1)) then
        spl_eval = a(1); return
    end if
    a(2) = dydt(knt1)
    a(3) = (3.*dy*(frac**2)) - ((dydt(knt2) + (2.*dydt(knt1)))*frac*dt2)
    a(4) = (-2.*dy*(frac**3)) + ((dydt(knt2) + dydt(knt1))*(frac**2)*dt2)
    spl_eval = a(1) + (a(2)*dt2) + a(3) + a(4)


end function spl_eval
!------------------------------------------------------------------------------------






!
! Function to evaluate derivative of spline df(tt)/dx using knot first derivative
!
! Input parameters: t       - independent variable values
!                   y       - dependent variable values
!                   dydt    - knot first derivatives
!                   n       - number of spline points
!                   tt      - t value at which spline is to be evaluated
!
! References: Kreyszig, E., "Numerical Methods in General", Advanced Engineering Mathematics,
!             8th Ed., John Wiley and Sons, 1999, pp. 861-866
!
!------------------------------------------------------------------------------------
function dspl_eval(tt,y,dydt,t,n) 
    use errors
    implicit none
    
    integer,        intent (in)             :: n
    real,           intent (in)             :: y(n), dydt(n), t(n), tt

    ! Local variables
    integer                                 :: knt1, knt2, find_knt
    real                                    :: dt, dy, a(3), dt2, frac, dspl_eval
    real,   parameter                       :: tol = 10e-10
    character(:),   allocatable             :: error_msg, dev_msg


    knt1 = find_knt(tt, t, n)
    knt2 = knt1 + 1

    dt   = t(knt2) - t(knt1)
    dy   = y(knt2) - y(knt1)
    dt2  = tt - t(knt1)
    frac = dt2/dt

    ! If dt = 0
    if (abs(dt) <= tol) then
        error_msg   = 'dspl_eval failed - identical knot locations'
        dev_msg     = 'Check function dspl_eval in spline.f90'
        call fatal_error(error_msg, dev_msg = dev_msg)
    end if

    a(1) = dydt(knt1)
    if (tt == t(knt1)) then
        dspl_eval = a(1); return
    end if
    a(2) = 2.*frac*((3.*dy/dt) - (dydt(knt2) + (2.*dydt(knt1))))
    a(3) = 3.*(frac**2)*((-2.*dy/dt) + (dydt(knt2) + dydt(knt1)))
    dspl_eval = a(1) + a(2) + a(3)


end function dspl_eval
!------------------------------------------------------------------------------------






!
! Subroutine to invert a spline to determine tt for an input spline value yy using
! first derivatives at spline knots; an initial guess for tt is specified since 
! tt(yy) may be multi-varied
!
! Input parameters: t       - independent variable values
!                   y       - dependent variable values
!                   dydt    - knot first derivatives
!                   n       - number of spline points
!                   yy      - specified spline value
!                   tt      - initial guess for t at which yy occurs
!
!------------------------------------------------------------------------------------
subroutine spl_inv(tt,yy,y,dydt,t,n) 
    use errors
    implicit none

    integer,        intent (in)             :: n
    real,           intent (in)             :: y(n), dydt(n), t(n), yy
    real,           intent (inout)          :: tt

    ! Local variables
    integer                                 :: iter, maxiter
    real                                    :: spl_eval, dspl_eval, dt_newt, y_newt, &
                                               dy_newt, tol, mint, maxt
    character(:),   allocatable             :: error_msg, warning_msg, dev_msg

    
    ! Check if initial guess lies in range of 't'
    mint = minval(t)
    maxt = maxval(t)
    
    if (tt < mint) then
        warning_msg = 'Bad initial guess provided. Using minimum value.'
        dev_msg     = 'Check subroutine spl_inv in spline.f90'
        call warning(warning_msg, dev_msg = dev_msg)
        tt          = mint
    else if (tt > maxt) then
        warning_msg = 'Bad initial guess provided. Using maximum value.'
        dev_msg     = 'Check subroutine spl_inv in spline.f90'
        call warning(warning_msg, dev_msg = dev_msg)
        tt          = maxt
    end if


    tol = 1.0e-5
    maxiter = 25 ! increased from 10 for radial cases
    
    do iter = 1, maxiter
        
        y_newt  = spl_eval(n, tt, y, dydt, t, .false.) - yy
        dy_newt = dspl_eval(tt, y, dydt, t, n)
        dt_newt = -y_newt/dy_newt
        tt      = tt + 0.8*dt_newt
        
        if (tt < mint) then
            warning_msg = 'spl_inv - spline parameter clipped at spline begin'
            dev_msg     = 'Check subroutine spl_inv in spline.f90'
            call warning(warning_msg, dev_msg = dev_msg)
            tt          = mint
        else if(tt > maxt) then
            warning_msg = 'spl_inv- spline parameter clipped at spline end'
            dev_msg     = 'Check subroutine spl_inv in spline.f90'
            call warning(warning_msg, dev_msg = dev_msg)
            tt          = maxt
        end if
        if(abs(dt_newt/(t(n)-t(1))) .lt. tol) return

    end do  ! iter

    error_msg   = 'spl_inv - spline parameter not determined. Maximum iteration reached.'
    dev_msg     = 'Check subroutine spl_inv in spline.f90'
    call error(error_msg, dev_msg, write_to_file = 0)


end subroutine spl_inv
!------------------------------------------------------------------------------------


!
! Subroutine to calculate spline first derivatives at knots for y(t) and allows zero
! second derivative at segment joints; consecutive identical y values indicate segment
! joints
!
! Input parameters: t   - independent variable values
!                   y   - dependent variable values
!                   n   - number of spline points
!------------------------------------------------------------------------------------
subroutine spl_discjoint(y,dydt,t,n) 
    use errors
    implicit none

    integer,        intent (in)             :: n
    real,           intent (in)             :: y(n), t(n)
    real,           intent (inout)          :: dydt(n)

    ! Local variables
    integer                                 :: i, seg_start, seg_end, n0
    real,   parameter                       :: tol = 10e-10
    character(:),   allocatable             :: error_msg, dev_msg


    ! If t(1) = t(2)
    ! If t(n) = t(n - 1)
    if (abs(t(1) - t(2)) <= tol) then
        error_msg   = 'spl_discjoint - identical knot locations at spline begin'
        dev_msg     = 'Check subroutine spl_discjoint in spline.f90'
        call fatal_error(error_msg, dev_msg = dev_msg)
    end if

    if (abs(t(n) - t(n-1)) <= tol) then
        error_msg   = 'spl_discjoint - identical knot locations at spline end'
        dev_msg     = 'Check subroutine spl_discjoint in spline.f90'
        call fatal_error(error_msg, dev_msg = dev_msg)
    end if

    seg_start = 1
    do i = 2, n - 2

        if(abs(t(i) - t(i+1)) <= tol) then
            n0      = i - seg_start + 1
            seg_end = seg_start + n0 - 1

            ! Implementing zero second derivative segment end conditions
            call spline(n0, y(seg_start), dydt(seg_start), t(seg_start), 999.0, 999.0)
            seg_start = i + 1
        end if

    end do

    n0 = n - seg_start + 1
    
    ! Implementing zero second derivative segment and zero third derivative end conditions
    call spline(n0, y(seg_start), dydt(seg_start), t(seg_start), 999.0, -999.0)


end subroutine spl_discjoint
!------------------------------------------------------------------------------------






!
! Subroutine to determine intersection of two cubic spline t1 and t2 in (x,y) space
! Spline parameters tt1, tt2 intersection points are calculated
! Initial guesses for tt1 and tt2 are required
!
! Input parameters: t1, t2          - spline parameters at knots
!                   x1, x2          - spline x values at knots
!                   y1, y2          - spline y values at knots
!                   n1, n2          - number of spline knots
!                   dydt1, dydt2    - spline derivatives at knots
!                   tt1, tt2        - initial guesses of spline parameters at 
!                                     intersection
!
!------------------------------------------------------------------------------------
subroutine spl_intersect(ia,tt1, tt2, x1, dxdt1, y1, dydt1, t1, n1, x2, dxdt2, y2, dydt2, t2, n2) 
    use errors  
    use file_operations
    use funcNsubs
    implicit none

    integer,        intent(in)              :: ia
    integer,        intent (in)             :: n1, n2
    real,           intent (in)             :: x1(n1), y1(n1), t1(n1), dxdt1(n1), dydt1(n1), &
                                               x2(n2), y2(n2), t2(n2), dxdt2(n2), dydt2(n2)
    real,           intent (inout)          :: tt1, tt2

    ! Local variables
    real,           parameter               :: GR = 0.5*(3.-sqrt(5.)), tol = 1.0e-12
    logical                                 :: GS_flag, trunc1knt1, trunc1kntn, trunc2knt1, trunc2kntn
    real                                    :: dt1, dt2, r, ra, rb, rt1, rt2, F, Fa, Fb, Ft1, Ft2, &
                                               F1, F2, J11, J12, J21, J22, xx1, xx2, yy1, yy2, tt1old, tt2old, &
                                               mint1, mint2, maxt1, maxt2, dspl_eval
    integer                                 :: iter, i, l, nopen
    character(10)                           :: warning_arg
    character(len = :), allocatable         :: log_file, warning_msg, dev_msg
    logical                                 :: file_open


    write(warning_arg, '(I5)') ia

    mint1 = minval(t1); maxt1 = maxval(t1)
    mint2 = minval(t2); maxt2 = maxval(t2)
    
    if (tt1 < mint1) then
        warning_msg = 'Bad initial guess provided for spline 1 in section '//adjustl(trim(warning_arg))//'. Using minimum value.'
        dev_msg     = 'Check subroutine spl_intersect in spline.f90'
        call warning(warning_msg, dev_msg = dev_msg)
        tt1         = mint1
    else if (tt1 > maxt1) then
        warning_msg = 'Bad initial guess provided for spline 1 in section '//adjustl(trim(warning_arg))//'. Using maximum value.'
        dev_msg     = 'Check subroutine spl_intersect in spline.f90'
        call warning(warning_msg, dev_msg = dev_msg)
        tt1         = maxt1
    end if

    if (tt2 < mint2) then
        warning_msg = 'Bad initial guess provided for spline 2 in section '//adjustl(trim(warning_arg))//'. Using minimum value.'
        dev_msg     = 'Check subroutine spl_intersect in spline.f90'
        call warning(warning_msg, dev_msg = dev_msg)
        tt2         = mint2
    else if (tt2 > maxt2) then
        warning_msg = 'Bad initial guess provided for spline 2 in section '//adjustl(trim(warning_arg))//'. Using maximum value.'
        dev_msg     = 'Check subroutine spl_intersect in spline.f90'
        call warning(warning_msg, dev_msg = dev_msg)
        tt2         = maxt2
    end if

    dt1 = 0.0
    dt2 = 0.0
    l   = 0

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
        end do
        
        if ((Ft1 .ge. Fa) .or. (Ft1 .ge. Fb)) then
            r = 1.; i = 0; GS_flag = .false.
        end if
        
        if (GS_flag) then
            l = l+1
            if (rt1-ra .lt. rb-rt1) then
                rt2 = rt1+(GR*(rb-rt1))
            else
                rt2 = rt1; rt1 = rt1-(GR*(rt1-ra))
            end if
            
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
                end if
                if (abs(rb-ra) .le. 1.0e-5) exit
            end do
            
            if (Ft1 .lt. Ft2) then
                r = rt1; F = Ft1
            else
                r = rt2; F = Ft2
            end if

        end if

        call feval
        
        J11 = -dspl_eval(tt1, x1, dxdt1, t1, n1)
        J12 = dspl_eval(tt2, x2, dxdt2, t2, n2)
        J21 = -dspl_eval(tt1, y1, dydt1, t1, n1)
        J22 = dspl_eval(tt2, y2, dydt2, t2, n2)
        dt1 = -(F1*J22 - J12*F2)/(J11*J22 - J12*J21)
        dt2 = -(J11*F2 - F1*J21)/(J11*J22 - J12*J21)
        
        if(abs(dt1) .lt. tol*(t1(n1)-t1(1)) .and. abs(dt2) .lt. tol*(t2(n2)-t2(1))) then
            return
        end if

    end do  ! iter

    ! TODO: No condition for failure?
    call log_file_exists(log_file, nopen, file_open)
    if (trunc1knt1) then
        warning_msg = 'spl_intersect - splines may not intersect for section '//adjustl(trim(warning_arg))//'. Knot 1 of spline 1 is closest possible to spline 2.'
        dev_msg     = 'Check subroutine spl_intersect in spline.f90'
        call warning(warning_msg, dev_msg = dev_msg)
    end if
    if (trunc1kntn) then
        warning_msg = 'spl_intersect - splines may not intersect for section '//adjustl(trim(warning_arg))//'. End knot of spline 1 is closest possible to spline 2.'
        dev_msg     = 'Check subroutine spl_intersect in spline.f90'
        call warning(warning_msg, dev_msg = dev_msg)
    end if
    if (trunc2knt1) then
        warning_msg = 'spl_intersect - splines may not intersect for section '//adjustl(trim(warning_arg))//'. Knot 1 of spline 2 is closest possible to spline 1.'
        dev_msg     = 'Check subroutine spl_intersect in spline.f90'
        call warning(warning_msg, dev_msg = dev_msg)
    end if
    if (trunc2kntn) then
        warning_msg = 'spl_intersect - Splines may not intersect for section '//adjustl(trim(warning_arg))//'. End knot of spline 2 is closest possible to spline 1.'
        dev_msg     = 'Check subroutine spl_intersect in spline.f90'
        call warning(warning_msg, dev_msg = dev_msg)
    end if
    call close_log_file(nopen, file_open)

    contains
        subroutine feval
            
            real :: spl_eval
            trunc1knt1 = .false.; trunc1kntn = .false.
            trunc2knt1 = .false.; trunc2kntn = .false.
            tt1 = tt1old + r*dt1
            tt2 = tt2old + r*dt2
            if(tt1.lt.t1(1)) then
                tt1 = t1(1); trunc1knt1 = .true.
            end if
            if(tt1.gt.t1(n1)) then
                tt1 = t1(n1); trunc1kntn = .true.
            end if
            if(tt2.lt.t2(1)) then
                tt2 = t2(1); trunc2knt1 = .true.
            end if
            if(tt2.gt.t2(n2)) then
                tt2 = t2(n2); trunc2kntn = .true.
            end if
            xx1 = spl_eval(n1, tt1, x1, dxdt1, t1, .false.)
            xx2 = spl_eval(n2, tt2, x2, dxdt2, t2, .false.)
            yy1 = spl_eval(n1, tt1, y1, dydt1, t1, .false.)
            yy2 = spl_eval(n2, tt2, y2, dydt2, t2, .false.)
            F1  = xx2 - xx1
            F2  = yy2 - yy1
            F   = (F1**2)+(F2**2)

        end subroutine feval


end subroutine spl_intersect
!------------------------------------------------------------------------------------






!
! Subroutine for computing first derivatives of a parametric cubic spline 
! for a set of 'y' points with uniform knots; works with natural boundary
! conditions
!
! Input parameters: n               - number of spline points
!                   y               - dependent variable values
!
!------------------------------------------------------------------------------------
subroutine open_uniform_cubic_spline(n, y, dy)
    use funcNsubs
    implicit none

    integer,        intent(in)              :: n
    real,           intent(in)              :: y(n)
    real,           intent(inout)           :: dy(n)

    ! Local variables
    integer                                 :: i
    real                                    :: diag(n), sub_diag(n), super_diag(n), rhs(n)


    ! Generate lower diagonal with zero first element
    sub_diag(1)         = 0.0
    do i = 2,n
        sub_diag(i)     = 1.0
    end do

    ! Generate upper diagonal with zero last element
    do i = 1,n - 1
        super_diag(i)   = 1.0
    end do
    super_diag(n)       = 0.0

    ! Generate main diagonal 
    diag(1)             = 2.0
    do i = 2,n - 1
        diag(i)         = 4.0
    end do
    diag(n)             = 2.0

    ! Generate RHS with segment boundary conditions
    rhs(1)              = 3.0*(y(2) - y(1))
    do i = 2,n - 1
        rhs(i)          = 3.0*(y(i + 1) - y(i - 1))
    end do
    rhs(n)              = 3.0*(y(n) - y(n - 1))

    
    ! Solve tridiagonal system
    ! tridiag_solve in funcNsubs.f90
    call tridiag_solve(diag, sub_diag, super_diag, rhs, n)

    ! Set knot first derivatives
    dy                  = rhs


end subroutine open_uniform_cubic_spline
!------------------------------------------------------------------------------------






!
! Subroutine for computing the first derivatives of a parametric cubic spline for a
! set of closed 'x' (x(0) = x(n)) points with uniform knots
! This subroutine is based on the subroutine PSPLINE in Reference 1
!
! Input parameters: n               - number of spline points
!                   x               - zero-indexed array containing dependent variable
!                                     values, is passed in as an array of size n + 2
!                                     with x(0) = x(n) and x(1) = x(n + 1)
!
! References: 1) Smith, D.A., "Parametric Cubic Spline-Fitting Programs for Open and
!                Closed Curves", Computers in Physics, Vol. 6, no. 5, 1992, pp. 472 - 477
!             2) Ahlberg, J.H., Nilson, E.N., Walsh, J.L., "The Theory of Splines and 
!                Their Applications", Academic Press, London, UK, 1967, pp. 14, 15 and 51
!
!------------------------------------------------------------------------------------
subroutine closed_uniform_cubic_spline(n, x, dx)
    use errors
    implicit none

    integer,        intent(in)              :: n
    real,           intent(in)              :: x(0:n + 1)
    real,           intent(inout)           :: dx(n + 1)

    ! Local variables
    integer                                 :: i, j
    real                                    :: a(4,n), q(0:n), u(0:n), s(0:n), t(0:n), v(0:n), xdd(0:n), &
                                               fxn
    real                                    :: tol = 10E-8
    character(:),   allocatable             :: error_msg, warning_msg, dev_msg


    !
    ! Check if the incoming curve is periodic
    ! If not periodic, raise error and stop execution
    !
    if (abs(x(0) - x(n)) > tol .or. abs(x(1) - x(n + 1)) > tol) then
        error_msg                           = 'subroutine closed_uniform_cubic_spline requires closed curves as &
                                               &inputs'
        warning_msg                         = 'x(0) = x(n) and x(1) = x(n + 1)'
        dev_msg                             = 'Check closed_uniform_cubic_spline in funcNsubs'
        call fatal_error(error_msg, warning_msg, dev_msg)
    end if


    ! Coefficients for computing x"
    q(0)                                    = 0.0
    u(0)                                    = 0.0
    s(0)                                    = 1.0

    ! Forward substitution for q(j), u(j)
    do j = 1, n
        q(j)                                = -1.0/(4.0 + q(j - 1))
        u(j)                                = q(j) * (u(j - 1) - ((6.0*real(n*n)) * (x(j + 1) - (2.0*x(j)) + x(j - 1))))
        s(j)                                = q(j) * s(j - 1)
    end do

    t(n)                                    = 1.0
    v(n)                                    = 0.0

    ! Backward substitution for t(j), v(j)
    do j = n - 1, 0, -1
        t(j)                                = (q(j)*t(j + 1)) + s(j)
        v(j)                                = (q(j)*v(j + 1)) + u(j)
    end do


    fxn                                     = (6.0 * real(n*n)) * (x(1) - (2.0*x(n)) + x(n - 1))
    xdd(n)                                  = (fxn - v(1) - v(n - 1))/(4.0 + t(1) + t(n - 1)) 

    ! Compute x"
    do j = 0, n - 1
        xdd(j)                              = t(j)*xdd(n) + v(j)
    end do



    !
    ! Compute coefficients of all piecewise cubic polynomials
    !
    do j = 1, n
        a(1,j)                              = x(j)
        a(2,j)                              = n * (x(j) - x(j - 1)) + (xdd(j - 1) + 2.0*xdd(j))/(6.0*real(n))
        a(3,j)                              = 0.5*xdd(j)
        a(4,j)                              = (xdd(j) - xdd(j - 1))/(6.0/real(n))  
    end do



    !
    ! Compute first derivative of closed curve using piecewise cubic polynomials
    !
    dx(1)                                   = a(2,1) + (2.0*a(3,1)*(-1.0/real(n))) + (3.0*a(4,1)*(1.0/real(n)**2))
    do i = 1, n
        dx(i + 1)                           = a(2,i) 
    end do


end subroutine closed_uniform_cubic_spline
!------------------------------------------------------------------------------------





















