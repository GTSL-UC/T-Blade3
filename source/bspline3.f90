!
! Compute cubic B-spline at a parameter value for a given segment 
!
! Input parameters: cp  - control points
!                   t   - parameter value
!
!-----------------------------------------------------------------------------------
real function bspline(cp,t)  
    implicit none

    real,                   intent (in)     :: cp(4)
    real,                   intent (in)     :: t

    ! Local variables
    real                                    :: B1, B2, B3, B4       


    !
    ! Basis functions
    !
    B1      = ((-t**3) + (3*t**2) - (3*t) + 1)/6
    B2      = ((3*t**3) - (6*t**2) + 4)/6
    B3      = ((-3*t**3) + (3*t**2) + (3*t) + 1)/6
    B4      = (t**3)/6

    bspline = (cp(1)*B1) + (cp(2)*B2) + (cp(3)*B3) + (cp(4)*B4)


end function bspline 
!-----------------------------------------------------------------------------------






!
! Compute first derivative of cubic B-spline at a parameter value for a 
! given segment
!
! Input parameters: cp  - control points
!                   t   - parameter value
!
!-----------------------------------------------------------------------------------
real function d_bspline(cp,t)  
    implicit none

    real,                   intent (in)     :: cp(4)
    real,                   intent (in)     :: t
    
    ! Local variables
    real                                    :: B1, B2, B3, B4       


    !
    ! Basis function derivatives
    !
    B1          = ((-3*t**2) + (6*t) - 3)/6
    B2          = ((9*t**2) - 12*t)/6
    B3          = ((-9*t**2) + (6*t) + 3)/6
    B4          = (3*t**2)/6

    d_bspline   = (cp(1)*B1) + (cp(2)*B2) + (cp(3)*B3) + (cp(4)*B4)


end function d_bspline
!-----------------------------------------------------------------------------------






!
! Compute second derivative of cubic B-spline at a parameter value for a 
! given segment
!
! Input parameters: cp  - control points
!                   t   - parameter value
!
!-----------------------------------------------------------------------------------
real function dd_bspline(cp,t)  
    implicit none
    
    real,                   intent (in)     :: cp(4)
    real,                   intent (in)     :: t

    ! Local variables
    real                                    :: B1, B2, B3, B4       


    !
    ! Basis function derivatives
    !
    B1          = ((-6*t) + 6)/6
    B2          = ((18*t) - 12)/6
    B3          = ((-18*t) + 6)/6
    B4          = (6*t)/6

    dd_bspline  = (cp(1)*B1) + (cp(2)*B2) + (cp(3)*B3) + (cp(4)*B4)


end function dd_bspline 
!-----------------------------------------------------------------------------------






!
! Compute third derivative of cubic B-spline at a parameter value for a 
! given segment
!
! Input parameters: cp  - control points
!                   t   - parameter value
!        
!-----------------------------------------------------------------------------------
real function d3_bspline(cp,t)  
    implicit none
    real,                   intent (in)     :: cp(4)
    real,                   intent (in)     :: t

    ! Local variables
    real                                    :: B1, B2, B3, B4       


    !
    ! Basis function derivatives
    !
    B1          = -1
    B2          = 3
    B3          = -3
    B4          = 1

    d3_bspline  = (cp(1)*B1) + (cp(2)*B2) + (cp(3)*B3) + (cp(4)*B4)


end function    
!-----------------------------------------------------------------------------------






!
! Find arclength of a B-spline segment
!
! Input parameters: ncp     - number of control points
!                   degree  - degree of B-spline curve
!                   xcp     - x coordinate of control points
!                   ycp     - y coordinate of control points
!
!-----------------------------------------------------------------------------------
subroutine bspline_arclength(ncp,degree,xcp,ycp,arclength)
    implicit none
    
    integer,                intent(in)      :: ncp, degree
    real,                   intent(in)      :: xcp(ncp), ycp(ncp)
    real,                   intent(inout)   :: arclength(ncp - degree + 1)

    ! Local variables
    integer                                 :: i, j
    real                                    :: GQx(29), GQw(29)
    real                                    :: d_bspline4, d_bspline, dxdt, dydt, S


    ! Initializing dydt
    dxdt    = 0.0
    dydt    = 0.0

    GQx             = [-0.9966794422605956, -0.9825455052614139, -0.9572855957780887, -0.9211802329530591, & 
                       -0.874637804920103,  -0.8181854876152532, -0.752462851734477 , -0.6782145376026867, &
                       -0.5962817971382289, -0.5075929551242282, -0.413152888174008 , -0.3140316378676399, &
                       -0.2113522861660013, -0.1062782301326792,  0.0000000000000002,  0.106278230132679 , & 
                        0.2113522861660009,  0.3140316378676405,  0.4131528881740089,  0.5075929551242276, &
                        0.5962817971382278,  0.6782145376026861,  0.7524628517344769,  0.8181854876152522, & 
                        0.8746378049201026,  0.9211802329530594,  0.9572855957780867,  0.9825455052614134, & 
                        0.9966794422605948]
    GQw             = [ 0.0085169038787468,  0.0197320850561252,  0.0307404922020909,  0.041402062518683 , & 
                        0.0515948269024977,  0.0612030906570802,  0.0701179332550491,  0.0782383271357671, & 
                        0.0854722573661736,  0.0917377571392597,  0.0969638340944074,  0.1010912737599152, & 
                        0.1040733100777303,  0.1058761550973202,  0.1064793817183146,  0.1058761550973217, & 
                        0.1040733100777293,  0.1010912737599157,  0.0969638340944089,  0.0917377571392591, &
                        0.085472257366172 ,  0.0782383271357633,  0.0701179332550512,  0.06120309065708  , & 
                        0.0515948269024958,  0.0414020625186843,  0.0307404922020949,  0.0197320850561219, & 
                        0.0085169038787447]

    GQx             = 0.5*(GQx + 1)


    
    !
    ! Compute arclength
    !
    arclength(1)    = 0

    do j = 1, ncp - degree
        arclength(j + 1) &
                  & = arclength(j)
        
        do i = 1, 29

            if (degree == 3) then
              
              dxdt  = d_bspline(xcp(j:j + degree),GQx(i))
              dydt  = d_bspline(ycp(j:j + degree),GQx(i))

            else if (degree == 4) then

              dxdt  = d_bspline4(xcp(j:j + degree),GQx(i))
              dydt  = d_bspline4(ycp(j:j + degree),GQx(i))

            end if

            S       = sqrt(dxdt**2 + dydt**2)
            arclength(j + 1) &
                  & = arclength(j + 1) + GQw(i)*S

        end do  ! i = 1, 29

    end do  ! j = 1, ncp - degree



    !
    !Normalize the arc length
    !
    arclength = arclength/arclength(ncp - degree + 1)


end subroutine bspline_arclength
!-----------------------------------------------------------------------------------






!
! Find spline parameter value associated with a given arclength of a B-spline
!
! Input parameters: ncp         - number of control points
!                   degree      - degree of spline
!                   s           - given arclength
!                   arclength   - total arclength
!        
!-----------------------------------------------------------------------------------
subroutine bspline_jt(ncp,degree,s,arclength,j,t)  
    implicit none

    integer,                intent(in)      :: ncp, degree
    real,                   intent(in)      :: s
    real,                   intent(in)      :: arclength(ncp - degree + 1)
    integer,                intent(inout)   :: j
    real,                   intent(inout)   :: t
    
    ! Local variables
    integer                                 :: ns


    !
    ! Compute number of segments
    ns      = ncp - degree 



    ! 
    ! Account for spline endpoints
    !
    if (s <= 0) then
        t   = 0
        j   = 1
    else if (s >= 1) then
        t   = 1
        j   = ns
    end if

    

    !
    ! Compute spline parameter value
    !            
    do j = 1, ns
        if ((arclength(j) <= s) .and.  (s <= arclength(j+1))) then
            t = (s - arclength(j))/((arclength(j + 1) - arclength(j)))
        end if
    end do


end subroutine bspline_jt
!-----------------------------------------------------------------------------------






!
! Generate B-splines starting from the jth segment at parameter value t
!
! Input parameters: ncp             - number of control points
!                   degree          - degree of B-spline
!                   cp              - control points
!                   s               - arclength
!                   arclength       - total arclength
!
!-----------------------------------------------------------------------------------
function bspline_cp(ncp,degree,cp,s,arclength) result(temp) 
    implicit none

    integer,                intent(in)      :: ncp, degree
    real,                   intent(in)      :: s
    real,                   intent(in)      :: cp(ncp)
    real,                   intent(in)      :: arclength(ncp - 2)

    ! Local variables
    integer                                 :: j
    real                                    :: t, bspline, bspline4
    real                                    :: temp



    ! Initialize function result
    temp                    = 0.0

    call bspline_jt(ncp,degree,s,arclength,j,t)  
    if( degree == 3) temp   = bspline(cp(j:j + degree),t)
    if( degree == 4) temp   = bspline4(cp(j:j + degree),t)


end function    
!-----------------------------------------------------------------------------------
    





!
! Compute a B-spline as y = f(x)
!
! Input parameters: ncp     - number of control points
!                   degree  - degree of B-spline
!                   xcp     - x coordinates of control points
!                   ycp     - y coordinates of control points
!                   np      - number of points
!                   x       - x coordinates of points
!                              
!-----------------------------------------------------------------------
subroutine bspline_y_of_x(ncp, degree, xcp, ycp, np, x, y) 
    implicit none

    integer,                intent(in)      :: ncp, degree
    real,                   intent(in)      :: xcp(ncp), ycp(ncp)
    integer,                intent(in)      :: np
    real,                   intent(in)      :: x(np)
    real,                   intent(inout)   :: y(np)

    ! Local variables
    integer                                 :: i, j, seg_1, seg_end
    real                                    :: x_spl_end(ncp - degree + 1), &
                                               y_spl_end(ncp - degree + 1)
    real                                    :: bspline, bspline_t_newton, bspline4, &
                                               bspline4_t_newton, t, tolerance


    !
    ! Computing the end points for each segment
    !
    t = 0
    if (degree == 3) then
        x_spl_end(1)            = bspline(xcp(1:degree + 1),t)
    else if (degree == 4) then
        x_spl_end(1)            = bspline4(xcp(1:degree + 1),t)
        y_spl_end(1)            = bspline4(ycp(1:1 + degree),t)
    end if

    t = 1
    do j = 1,ncp - degree
        if (degree == 3) then
            x_spl_end(j + 1)    = bspline(xcp(j:j + degree),t)
        else if ( degree == 4) then
            x_spl_end(j + 1)    = bspline4(xcp(j:j + degree),t)
            y_spl_end(j + 1)    = bspline4(ycp(j:j + degree),t)
        end if
    end do



    !
    ! Determine if x is increasing or decreasing
    !
    if (x_spl_end(2) > x_spl_end(1)) then
        seg_1                   = 1
        seg_end                 = ncp - degree
    else
        seg_1                   = ncp - degree
        seg_end                 = 1
    end if



    !
    ! Compute y
    !
    tolerance                   = 1e-16

    do i = 1, np
        do j = 1, ncp - degree
            
            if (((x(i) >= x_spl_end(j)) .and. (x(i) <= x_spl_end(j+1))) .or. &
                 ((x(i) <= x_spl_end(j)) .and. (x(i) >= x_spl_end(j+1))) .or. &
                 ((i == 1) .and. (j == seg_1 )).or.&   
                 ((i == np) .and. (j == seg_end ))) then 

                if( degree == 3 )then
                    if (((x(i) - x_spl_end(j)) < tolerance).and.((x(i) - x_spl_end(j)) > -tolerance)) then
                        t       = 0
                    else if (((x(i) - x_spl_end(j)) > tolerance).and.((x(i) - x_spl_end(j)) < -tolerance)) then
                        t       = 1
                    else
                        t       = bspline_t_newton(xcp(j:j + degree), x(i))
                    end if
                    y(i) = bspline(ycp(j:j + degree), t)

                else if( degree == 4 ) then
                    if (((x(i) - x_spl_end(j)) < tolerance).and.((x(i) - x_spl_end(j)) > -tolerance)) then
                        t       = 0
                    else if (((x(i) - x_spl_end(j)) > tolerance).and.((x(i) - x_spl_end(j)) < -tolerance)) then
                        t       = 1
                    else
                        t       = bspline4_t_newton(xcp(j:j+degree), x(i))
                    end if
                    y(i)        = bspline4(ycp(j:j+degree), t)
                end if

                exit

            end if

        end do  ! j = 1, ncp - degree
    end do  ! i = 1, np


end subroutine bspline_y_of_x
!-----------------------------------------------------------------------






!
! Subroutine to calculate B-spline values for a random bunch of intermediate
! points without constraining the last point to be at the end of the blade
!
! Input parameters: ncp     - number of control points
!                   degree  - degree of B-spline
!                   xcp     - x coordinates of control points
!                   ycp     - y coordinates of control points
!                   np      - number of points
!                   x       - x coordinates
!
!-----------------------------------------------------------------------
subroutine bspline_y_of_x_refine(ncp, degree, xcp, ycp, np, x, y) 
    implicit none

    integer,                intent(in)      :: ncp, degree
    real,                   intent(in)      :: xcp(ncp), ycp(ncp)
    integer,                intent(in)      :: np
    real,                   intent(in)      :: x(np)
    real,                   intent(inout)   :: y(np)

    ! Local variables
    integer                                 :: i, j, seg_1, seg_end
    real                                    :: x_spl_end(ncp - degree + 1)
    real                                    :: bspline, bspline_t_newton, bspline4, &
                                               bspline4_t_newton, t

    !
    ! Computing the end points for each segment
    !
    t = 0
    if (degree == 3) then
        x_spl_end(1)        = bspline(xcp(1:degree + 1),t)
    else if (degree == 4) then
        x_spl_end(1)        = bspline4(xcp(1:degree + 1),t)
    end if

    t = 1
    do j = 1,ncp - degree
       if (degree == 3) then
         x_spl_end(j + 1)   = bspline(xcp(j:j + degree),t)
       else if (degree == 4) then
         x_spl_end(j + 1)   = bspline4(xcp(j:j + degree),t)
       end if
    enddo



    !
    ! Determine if x is increasing or decreasing
    !
    if (x_spl_end(2) > x_spl_end(1)) then
        seg_1               = 1
        seg_end             = ncp - degree
    else
        seg_1               = ncp - degree
        seg_end             = 1
    end if
    


    !
    ! Compute y
    !      
    do i = 1, np
        do j = 1, ncp - degree

            if (((x(i) >= x_spl_end(j)) .and. (x(i) <= x_spl_end(j + 1))) .or. &
                ((x(i) <= x_spl_end(j)) .and. (x(i) >= x_spl_end(j + 1))) .or. &
                ((i == 1) .and. (j == seg_1 ))) then
               
                ! Cubic B-spline 
                if( degree == 3 )then
                    t       = bspline_t_newton(xcp(j:j + degree), x(i))
                    y(i)    = bspline(ycp(j:j + degree), t)

                ! Quartic B-spline
                else if( degree == 4 ) then
                    t       = bspline4_t_newton(xcp(j:j + degree), x(i))
                    y(i)    = bspline4(ycp(j:j + degree), t)
                end if
                exit

            end if

        end do  ! j = 1, ncp - degree
    end do  ! i = 1, np


end subroutine bspline_y_of_x_refine      
!-----------------------------------------------------------------------






!
! Use Newton's method to obtain B-spline parameter value given
! segment control points and a B-spline point
!
! Input parameters: cp  - control points for segment
!                   u   - coordinate
!
!-----------------------------------------------------------------------
real function bspline_t_newton(cp,u)
    use errors
    implicit none

    real,                   intent(in)      :: cp(4)
    real,                   intent(in)      :: u
    
    ! Local variables
    integer                                 :: k
    real                                    :: B1, B2, B3, B4, d1_B1, d1_B2, &
                                               d1_B3,d1_B4, tt_0, xs_0,d1_xs_0
    character(256)                          :: error_arg_1, error_arg_2
    character(:),   allocatable             :: error_msg, dev_msg


    ! Initialize error string arguments
    !error_arg_1             = ''
    !error_arg_2             = ''



    !
    ! Initial guess for Newton's method
    !
    tt_0                    = 0.5

    ! Newton's method
    do k = 1, 100
        ! Cubic B-spline basis functions
        B1                  = ((-tt_0**3) + (3*tt_0**2) - (3*tt_0) + 1)/6
        B2                  = ((3*tt_0**3) - (6*tt_0**2) + 4)/6
        B3                  = ((-3*tt_0**3) + (3*tt_0**2) + (3*tt_0) + 1)/6
        B4                  = (tt_0**3)/6

        ! First derivatives of basis functions
        d1_B1               = ((-3*tt_0**2) + (6*tt_0) - (3))/6
        d1_B2               = ((9*tt_0**2) - (12*tt_0))/6
        d1_B3               = ((-9*tt_0**2) + (6*tt_0) + (3))/6
        d1_B4               = (3*tt_0**2)/6
        
        ! B-spline point calculation
        xs_0                = (cp(1)*B1) + (cp(2)*B2) + (cp(3)*B3) + (cp(4)*B4)
        
        ! Derivative of B-spline
        d1_xs_0             = (cp(1)*d1_B1) + (cp(2)*d1_B2) + (cp(3)*d1_B3) + &
                              (cp(4)*d1_B4)
        
        ! Newton's interpolation
        bspline_t_newton    = tt_0 + (u - xs_0)/d1_xs_0

        ! Stay within bounds
        if (bspline_t_newton > 1) bspline_t_newton = 1
        if (bspline_t_newton < 0) bspline_t_newton = 0

        ! Exit iteration if converged
        if (abs(u - xs_0) < 1e-15) then 
            return
        end if

        ! Set value for next Newton's iteration
        tt_0                = bspline_t_newton

    end do  ! k = 1, 100



    !
    ! Print error message if Newton's method hasn't converged
    !
    write(error_arg_1, '(f20.16)') abs(u - xs_0)
    write(error_arg_2, '(f20.16)') bspline_t_newton
    error_msg               = "Cubic abs(u - xs_0) not converged - "//trim(error_arg_1) &
                             &//' '//trim(error_arg_2)
    dev_msg                 = 'Check function bspline_t_newton in bspline3.f90'

    ! fatal_error in errors.f90
    call fatal_error(error_msg, dev_msg = dev_msg)


end function bspline_t_newton
!-----------------------------------------------------------------------
        





!
! Compute quartic B-spline at a parameter value 't' for a given segment
!
! Input parameters: cp  - segment control points
!                   t   - parameter value
!
!-----------------------------------------------------------------------
real function bspline4(cp,t)  
    implicit none

    real,                   intent (in)     :: cp(5)
    real,                   intent (in)     :: t

    ! Local variables
    real                                    :: B1, B2, B3, B4, B5      


    !
    ! Quartic B-spline basis functions
    !
    B1          = ((1-t)**4)/24
    B2          = ((-4*t**4) + (12*t**3) + (-6*t**2) + (-12*t) + 11)/24
    B3          = ((6*t**4) + (-12*t**3) + (-6*t**2) + (12*t) + 11)/24
    B4          = ((-4*t**4) + (4*t**3) + (6*t**2) + (4*t) + 1)/24
    B5          = (t**4)/24
    
    bspline4    = (cp(1)*B1) + (cp(2)*B2) + (cp(3)*B3) + (cp(4)*B4) + (cp(5)*B5)


end function bspline4
!-----------------------------------------------------------------------






!
! Compute quartic B-spline first derivatives at a parameter value 't' 
! for a given segment
!
! Input parameters: cp  - segment control points
!                   t   - parameter value
!
!-----------------------------------------------------------------------
real function d_bspline4(cp,t)  
    implicit none
    
    real,                   intent(in)      :: cp(5)
    real,                   intent(in)      :: t

    ! Local variables
    real                                    :: B1, B2, B3, B4, B5 


    !
    ! First derivatives of basis functions
    !
    B1          = t**3/6 - t**2/2 + t/2 - 1/6.
    B2          = -2*t**3/3 + 3*t**2/2 - t/2 - 1/2.
    B3          = t**3 - 3*t**2/2 - t/2 + 1/2.
    B4          = -2*t**3/3 + t**2/2 + t/2 + 1/6.
    B5          = t**3/6

    d_bspline4  = (cp(1)*B1) + (cp(2)*B2) + (cp(3)*B3) + (cp(4)*B4) + (cp(5)*B5)

end function d_bspline4
!-----------------------------------------------------------------------






!
! Compute quartic B-spline second derivatives at a parameter value 't'
! for a given segment
!
! Input parameters: cp  - segment control points
!                   t   - parameter value
!
!-----------------------------------------------------------------------
real function dd_bspline4(cp,t)  
    implicit none

    real,                   intent(in)      :: cp(5)
    real,                   intent(in)      :: t
    
    ! Local variables
    real                                    :: B1, B2, B3, B4, B5  


    !
    ! Second derivatives of basis functions
    !
    B1          = (12*(1-t)**2)/24
    B2          = ((-48*t**2) + (72*t) + (-12))/24
    B3          = ((72*t**2) + (-72*t) + (-12))/24
    B4          = ((-48*t**2) + (24*t) + (12))/24
    B5          = (12*t**2)/24

    dd_bspline4 = (cp(1)*B1) + (cp(2)*B2) + (cp(3)*B3) + (cp(4)*B4) + (cp(5)*B5)


end function dd_bspline4
!-----------------------------------------------------------------------






!
! Compute quartic B-spline third derivatives at a parameter value 't'
! for a given segment
! 
! Input parameters: cp  - segment control points
!                   t   - parameter value
!     
!-----------------------------------------------------------------------
real function d3_bspline4(cp,t)  
    implicit none

    real,                   intent(in)      :: cp(5)
    real,                   intent(in)      :: t

    ! Local variables
    real                                    :: B1, B2, B3, B4, B5       


    ! 
    ! Third derivatives of basis functions
    !
    B1          = (-24*(1-t))/24
    B2          = ((-96*t) + (72))/24
    B3          = ((144*t) + (-72))/24
    B4          = ((-96*t) + (24))/24
    B5          = (24*t)/24

    d3_bspline4 = (cp(1)*B1) + (cp(2)*B2) + (cp(3)*B3) + (cp(4)*B4) + (cp(5)*B5)


end function d3_bspline4   
!-----------------------------------------------------------------------






!
! Compute quartic B-spline fourth derivatives at a parameter value 't'
! for a given segment
!
! Input parameter: cp   - segment control points
!                  t    - parameter value
!
!-----------------------------------------------------------------------
real function d4_bspline4(cp,t)  
    implicit none

    real,                   intent(in)      :: cp(5)
    real,                   intent(in)      :: t
    
    ! Local variables
    real                                    :: B1, B2, B3, B4, B5       


    !
    ! Fourth derivaties of basis functions
    !
    B1          = 1
    B2          = -4
    B3          = 6
    B4          = -4
    B5          = 1

    d4_bspline4 = (cp(1)*B1) + (cp(2)*B2) + (cp(3)*B3) + (cp(4)*B4) + (cp(5)*B5)


end function d4_bspline4 
!-----------------------------------------------------------------------






!
! Use Newton's method to obtain quartic B-spline parameter value given
! segment control points and a B-spline point
!
! Input parameters: cp  - control points for segment
!                   u   - coordinate
!
!-----------------------------------------------------------------------
real function bspline4_t_newton(cp,u)
    use errors
    implicit none
    
    real,                   intent(in)      :: cp(5)
    real,                   intent(in)      :: u
    
    ! Local variables
    integer                                 :: k
    real                                    :: bspline4, d_bspline4, tt_0, &
                                               xs_0, d1_xs_0
    character(256)                          :: error_arg_1, error_arg_2
    character(:),   allocatable             :: error_msg, dev_msg


    ! Initializing error message string arguments
    error_arg_1             = ''
    error_arg_2             = ''


    
    !
    ! Initial guess for Newton's method       
    !
    tt_0                    = 0.5

    ! Newton's method'
    do k = 1, 100

        ! Quartic B-spline point
        xs_0                = bspline4(cp,tt_0)

        ! Quartic B-spline first derivative
        d1_xs_0             = d_bspline4(cp,tt_0)
        
        ! Newton's interpolation
        bspline4_t_newton   = tt_0 + (u-xs_0)/d1_xs_0

        ! Stay within bounds
        if(bspline4_t_newton > 1) bspline4_t_newton = 1
        if(bspline4_t_newton < 0) bspline4_t_newton = 0

        ! Exit loop if converged
        if (abs(u - xs_0) < 1e-15) then                
           return
        end if

        ! Set new value for next iteration
        tt_0                = bspline4_t_newton

    end do  ! k = 1, 100



    !
    ! Print error message if Newton's method hasn't converged
    ! 
    write(error_arg_1, '(f20.16)') abs(u - xs_0)
    write(error_arg_2, '(f20.16)') bspline4_t_newton
    error_msg               = "Quartic abs(u-xs_0) not converged - "//&
                              & trim(error_arg_1)//" "//trim(error_arg_2)
    dev_msg                 = 'Check function bspline4_t_newton in bspline3.f90'

    ! fatal_error in errors.f90
    call fatal_error(error_msg, dev_msg = dev_msg)


end function bspline4_t_newton
!-----------------------------------------------------------------------






!
! UNUSED SUBROUTINES
!
!-----------------------------------------------------------------------------------
!subroutine bspline3(xcp,ycp,ncp,x,y,nspan,ia)
!!xcp,ycp: control points
!!xb3,yb3: interpolated spline points
!!t: parameter value ( 0<t<1)
!!ncp: number of control points
!
!implicit none
!
!integer ncp,ncp1,i,j,k,np,nx,nax,nbsp,nspan,ia,ib
!real*8 xcp(ncp),ycp(ncp)
!!real*8 xb3(1000),yb3(1000)
!real*8 xc(ncp+2),yc(ncp+2)
!real*8 xs(1000),ys(1000)
!!real*8 xmin(1000),xmax(1000)
!real*8 xmax,xmin,xint
!
!real*8 t(1000), T1(1000), T2(1000), T3(1000), T4(1000)
!
!
!parameter(np=50,nx=1000,nax=100)
!real*8 xbs(nx),ybs(nx),min,max
!real*8 x(nspan),y(nspan)
!!print*,' Control points for bspline curve from Input are:'
!!print*,ncp
!!do i=1,ncp
!!   write(*,*)ycp(i),xcp(i)
!!enddo
!!
!!nspan = na
!!print*,'span:',span(ia)
!!write(*,*)
!t(1) = 0.
!t(np) = 1.0
!do i=2,np-1
!  t(i) = t(i-1) + (1.0/np)
!  T1(i) = ((-t(i)**3) + (3*t(i)**2) - (3*t(i)) + 1)/6
!  T2(i) = ((3*t(i)**3) - (6*t(i)**2) + 4)/6
!  T3(i) = ((-3*t(i)**3) + (3*t(i)**2) + (3*t(i)) + 1)/6
!  T4(i) = (t(i)**3)/6
!enddo
!! Making the start and end points as collocation points----
!! Start point: Considering the 1st CP as the mid point of the 2nd CP and the start point----------
!xc(1) = 2*xcp(1) - xcp(2)
!yc(1) = 2*ycp(1) - ycp(2)
!! End point: Considering the last CP as the mid point of the end point and the penultimate CP ----
!xc(ncp+2) = 2*xcp(ncp) - xcp(ncp-1)
!yc(ncp+2) = 2*ycp(ncp) - ycp(ncp-1)
!do i=1,ncp
!  xc(i+1) = xcp(i)
!  yc(i+1) = ycp(i)
!enddo
!!write(*,*)
!!print*,'New control points with start and end points as collocation points:'
!!do i=1,ncp+2
!! print*,yc(i),xc(i)
!!enddo
!ncp1 = ncp + 2 ! includes the start and end points
!!print*,'ncp-new:',ncp1
!nbsp = (np - 2)*(ncp1 - 3)
!!print*,'nbsp:',nbsp
!!--------------------------------------------------------------------------------------------------
!!constructing the bezier curve(cubic spline) using 4 points P0(x0,y0),P1(x1,y1),P2(x2,y2),P3(x3,y3)------
!!-----B(t) =(((1-t)^3)*P0) + (3*((1-t)^2)*t*P1) + (3*(1-t)*(t^2)*P1) + ((t^3)*P2); 0.le.t.le.1 ---------
!!write(*,*)
!!print*,'Curve points for Bspline curve of degree 3:'
!!print*,np
!!write(*,*)
! xs(1) = 0.
! ys(1) = 0.
!xbs(1) = xcp(1)
!ybs(1) = ycp(1)
!xbs(nbsp+2) = xcp(ncp)
!ybs(nbsp+2) = ycp(ncp)
!do j=1,ncp1-3
! do i=2,np-1
!  xs(i) = (xc(j)*T1(i)) + (xc(j+1)*T2(i)) + (xc(j+2)*T3(i)) + (xc(j+3)*T4(i))
!  ys(i) = (yc(j)*T1(i)) + (yc(j+1)*T2(i)) + (yc(j+2)*T3(i)) + (yc(j+3)*T4(i))
!  k = i + (np - 2)*(j - 1)! Transforming from 2D array to 1D array
!  xbs(k) = xs(i)
!  ybs(k) = ys(i)
!!  print*,ybs(k),xbs(k)
! enddo
!enddo
!!write(*,*) 
!!Finding the intersection of the line and the 2D B-spline curve------
!!print*,'span  deltatheta'
!do ib = 1, nspan
!! print*,'span:',span(ia)
! do i = 1, nbsp + 1
!!  print*,xbs(i),xbs(i+1)
!  xint = xbs(i+1) + (ybs(i+1) - x(ib))*(((xbs(i) - xbs(i+1))/(ybs(i+1) - ybs(i))))
!!  print*,'xint',xint
!  xmax = max(xbs(i),xbs(i+1))
!  xmin = min(xbs(i),xbs(i+1))
!!  print*,xmin,xmax
!!  xmin1 = xmin(i)
!!  xmax1 = xmax(i)
!  if (xint.ge.xmin.and.xint.le.xmax)y(ib) = xint
! enddo
!!write(*,*)
!!print*,xmin,xmax
!! print*,y(ia)
!enddo 
!
!return
!end subroutine bspline3

!subroutine bspline4_Beval(t, B)
!
!real ,intent (in) :: t
!real :: B(5)      
!
!B(1) = ((1-t)**4)/24
!B(2) = ((-4*t**4) + (12*t**3) + (-6*t**2) + (-12*t) + 11)/24
!B(3) = ((6*t**4) + (-12*t**3) + (-6*t**2) + (12*t) + 11)/24
!B(4) = ((-4*t**4) + (4*t**3) + (6*t**2) + (4*t) + 1)/24
!B(5) = (t**4)/24
!
!end subroutine bspline4_Beval
!
!subroutine d_bspline4_Beval(t, B)
!
!real ,intent (in) :: t
!real :: B(5)      
!
!B(1) = t**3/6 - t**2/2 + t/2 - 1/6.
!B(2) = -2*t**3/3 + 3*t**2/2 - t/2 - 1/2.
!B(3) = t**3 - 3*t**2/2 - t/2 + 1/2.
!B(4) = -2*t**3/3 + t**2/2 + t/2 + 1/6.
!B(5) = t**3/6
!
!end subroutine d_bspline4_Beval
!
!subroutine dd_bspline4_Beval(t, B)
!
!real ,intent (in) :: t
!real :: B(5)      
!
!B(1) = (12*(1-t)**2)/24
!B(2) = ((-48*t**2) + (72*t) + (-12))/24
!B(3) = ((72*t**2) + (-72*t) + (-12))/24
!B(4) = ((-48*t**2) + (24*t) + (12))/24
!B(5) = (12*t**2)/24
!
!end subroutine dd_bspline4_Beval

!function d_bspline_cp(cp,arclength,ncp,degree,s) result(temp) 
!    implicit none
!    real*8, dimension(ncp), intent (in) :: cp
!    real*8 , dimension(ncp-2) ,intent (in) :: arclength
!    integer, intent(in) :: ncp,degree
!    real*8 ,intent (in) :: s
!    integer :: j
!    real*8 :: t, d_bspline, d_bspline4
!    real    :: temp
!   
!    ! Initialize function result
!    temp    = 0.0
!
!    call bspline_jt(ncp,degree,s,arclength,j,t)  
!    if( degree == 3) temp = d_bspline(cp(j:j+degree),t)
!    if( degree == 4) temp = d_bspline4(cp(j:j+degree),t)
!
!end function   
!
!function dd_bspline_cp(cp,arclength,ncp,degree,s) result(temp)
!    implicit none
!    real*8, dimension(ncp), intent (in) :: cp
!    real*8 , dimension(ncp-2) ,intent (in) :: arclength
!    integer, intent(in) :: ncp,degree
!    real*8 ,intent (in) :: s
!    integer :: j
!    real*8 :: t, dd_bspline, dd_bspline4
!    real    :: temp
!
!    ! Initialize function result
!    temp    = 0.0
!    
!    call bspline_jt(ncp,degree,s,arclength,j,t)  
!    if( degree == 3) temp = dd_bspline(cp(j:j+degree),t)
!    if( degree == 4) temp = dd_bspline4(cp(j:j+degree),t)
!
!end function
!
!function d3_bspline_cp(cp,arclength,ncp,degree,s) result(temp) 
!    implicit none
!    real*8, dimension(ncp), intent (in) :: cp
!    real*8 , dimension(ncp-2) ,intent (in) :: arclength
!    integer, intent(in) :: ncp,degree
!    real*8 ,intent (in) :: s
!    integer :: j
!    real*8 :: t, d3_bspline, d3_bspline4
!    real    :: temp
!
!    ! Initialize function result
!    temp    = 0.0
!    
!    call bspline_jt(ncp,degree,s,arclength,j,t)  
!    if( degree == 3) temp = d3_bspline(cp(j:j+degree),t)
!    if( degree == 4) temp = d3_bspline4(cp(j:j+degree),t)
!
!end function
!        
!real*8 function d4_bspline_cp(cp,arclength,ncp,degree,s)  
!    implicit none
!    real*8, dimension(ncp), intent (in) :: cp
!    real*8 , dimension(ncp-2) ,intent (in) :: arclength
!    integer, intent(in) :: ncp,degree
!    real*8 ,intent (in) :: s
!    integer :: j
!    real*8 :: t, d4_bspline4
!
!    call bspline_jt(ncp,degree,s,arclength,j,t)  
!    if( degree == 3) d4_bspline_cp = 0
!    if( degree == 4) d4_bspline_cp = d4_bspline4(cp(j:j+degree),t)
!
!end function





















