!
! Construct camber-line slope value for a given spline parameter and control points
! 
! Input parameters: y_cp    - control points for camber-line second derivative
!                   x_cp    - control points for chord-line u
!                   angle0  - camber-line slope value for t = 0
!                   t       - spline parameter value
!
!----------------------------------------------------------------------------------------------------------------------------------
real function angle(y_cp, x_cp, angle0, t)
    implicit none

    real, intent (in)                   :: y_cp(4), x_cp(4)  
    real, intent (in)                   :: t, angle0


    !
    ! If t == 0
    !
    if ( t < 1e-16 ) then
        angle = angle0
        return
    end if


    angle = -1*(y_cp(1)*(x_cp(1)*(t**6/72 - t**5/12 + 5*t**4/24 - 5*t**3/18 + 5*t**2/24 - t/12) + x_cp(2)*(-t**6/24 +      &
            13*t**5/60 - 7*t**4/16 + 5*t**3/12 - t**2/6) + x_cp(3)*(t**6/24 - 11*t**5/60 + 7*t**4/24 - t**3/6 - t**2/24 +  &
            t/12) + x_cp(4)*(-t**6/72 + t**5/20 - t**4/16 + t**3/36)) + y_cp(2)*(x_cp(1)*(-t**6/24 + t**5/5 - 5*t**4/16 +  &
            t**3/18 + t**2/3 - t/3) + x_cp(2)*(t**6/8 - t**5/2 + t**4/2 + t**3/3 - 2*t**2/3) + x_cp(3)*(-t**6/8 + 2*t**5/5 & 
            - 3*t**4/16 - t**3/2 + t**2/3 + t/3) + x_cp(4)*(t**6/24 - t**5/10 + t**3/9)) + y_cp(3)*(x_cp(1)*(t**6/24 -     &
            3*t**5/20 + t**4/8 + t**3/18 - t**2/24 - t/12) + x_cp(2)*(-t**6/8 + 7*t**5/20 - t**4/16 - t**3/4 - t**2/6) +   &
            x_cp(3)*(t**6/8 - t**5/4 - t**4/8 + t**3/6 + 5*t**2/24 + t/12) + x_cp(4)*(-t**6/24 + t**5/20 + t**4/16 +       &
            t**3/36)) + y_cp(4)*(t**6*x_cp(4)/72 + x_cp(1)*(-t**6/72 + t**5/30 - t**4/48) + x_cp(2)*(t**6/24 - t**5/15) +  &
            x_cp(3)*(-t**6/24 + t**5/30 + t**4/48)) ) + angle0   


end function
!----------------------------------------------------------------------------------------------------------------------------------






!
! Construct camber-line value for a given spline parameter and control points
!
! Input parameters: y_cp    - control points for camber-line second derivative
!                   x_cp    - control points for chord-line u
!                   angle0  - camber-line value for t = 0
!                   t       - spline parameter value
!
!----------------------------------------------------------------------------------------------------------------------------------
real function camber(y_cp, x_cp, angle0, camber0, t)  
    implicit none
    real, intent (in)                   :: y_cp(4), x_cp(4)
    real, intent (in)                   :: t, angle0, camber0

    
    !
    ! If t == 0
    !
    if ( t < 1e-16 ) then
        camber = camber0
        return
    end if


    camber = -1*(y_cp(1)*(x_cp(1)*(x_cp(1)*(-t**9/1296 + t**8/144 - t**7/36 + 7*t**6/108 - 7*t**5/72 + 7*t**4/72 - t**3/16 +          &
             t**2/48) + x_cp(2)*(t**9/432 - 11*t**8/576 + 23*t**7/336 - 5*t**6/36 + 25*t**5/144 - 13*t**4/96 + t**3/18) +             &
             x_cp(3)*(-t**9/432 + 5*t**8/288 - t**7/18 + 7*t**6/72 - 7*t**5/72 + 7*t**4/144 + t**3/144 - t**2/48) + x_cp(4) *         &
             (t**9/1296 - t**8/192 + 5*t**7/336 - 5*t**6/216 + t**5/48 - t**4/96)) + x_cp(2)*(x_cp(1)*(t**9/432 - 3*t**8/160 +        &  
             73*t**7/1120 - 181*t**6/1440 + 23*t**5/160 - 3*t**4/32 + t**3/36) + x_cp(2)*(-t**9/144 + 49*t**8/960 - 523*t**7/3360     & 
             + t**6/4 - 13*t**5/60 + t**4/12) + x_cp(3)*(t**9/144 - 11*t**8/240 + 409*t**7/3360 - 229*t**6/1440 + 43*t**5/480 +       &
             t**4/96 - t**3/36) + x_cp(4)*(-t**9/432 + 13*t**8/960 - t**7/32 + 5*t**6/144 - t**5/60)) + x_cp(3)*(x_cp(1) *            &
             (-t**9/432 + t**8/60 - t**7/20 + 7*t**6/90 - 7*t**5/120 + 5*t**3/144 - t**2/48) + x_cp(2)*(t**9/144 - 43*t**8/960 +      &
             193*t**7/1680 - 5*t**6/36 + 13*t**5/240 + 5*t**4/96 - t**3/18) + x_cp(3)*(-t**9/144 + 19*t**8/480 - 3*t**7/35 +          &
             3*t**6/40 + t**5/120 - t**4/16 + t**3/48 + t**2/48) + x_cp(4)*(t**9/432 - 11*t**8/960 + t**7/48 - t**6/72 - t**5/240     &
             + t**4/96)) + x_cp(4)*(x_cp(1)*(t**9/1296 - 7*t**8/1440 + 127*t**7/10080 - 73*t**6/4320 + 17*t**5/1440 - t**4/288) +     &
             x_cp(2)*(-t**9/432 + 37*t**8/2880 - 31*t**7/1120 + t**6/36 - t**5/90) + x_cp(3)*(t**9/432 - t**8/90 + 197*t**7/10080     &
             - 19*t**6/1440 - t**5/1440 + t**4/288) + x_cp(4)*(-t**9/1296 + t**8/320 - t**7/224 + t**6/432))) + y_cp(2)*(x_cp(1) *    &
             (x_cp(1)*(t**9/432 - 17*t**8/960 + 181*t**7/3360 - 317*t**6/4320 + 13*t**5/1440 + 17*t**4/144 - t**3/6 + t**2/12) +      &
             x_cp(2)*(-t**9/144 + 23*t**8/480 - 139*t**7/1120 + 17*t**6/144 + 7*t**5/90 - 7*t**4/24 + 2*t**3/9) + x_cp(3)*(t**9/144   & 
             - 41*t**8/960 + 311*t**7/3360 - 71*t**6/1440 - 173*t**5/1440 + 31*t**4/144 - t**3/18 - t**2/12) + x_cp(4)*(-t**9/432 +   &
             t**8/80 - 5*t**7/224 + t**6/216 + t**5/30 - t**4/24)) + x_cp(2)*(x_cp(1)*(-t**9/144 + 3*t**8/64 - 13*t**7/112 +          &
             7*t**6/72 + t**5/12 - 5*t**4/24 + t**3/9) + x_cp(2)*(t**9/48 - t**8/8 + t**7/4 - t**6/12 - t**5/3 + t**4/3) + x_cp(3) *  &
             (-t**9/48 + 7*t**8/64 - 19*t**7/112 - t**6/24 + 19*t**5/60 - t**4/8 - t**3/9) + x_cp(4)*(t**9/144 - t**8/32 + t**7/28 +  & 
             t**6/36 - t**5/15)) + x_cp(3)*(x_cp(1)*(t**9/144 - 13*t**8/320 + 89*t**7/1120 - 11*t**6/480 - 11*t**5/96 + 5*t**4/48 +   &
             t**3/18 - t**2/12) + x_cp(2)*(-t**9/48 + 17*t**8/160 - 173*t**7/1120 - t**6/16 + 3*t**5/10 - t**4/24 - 2*t**3/9) +       &
             x_cp(3)*(t**9/48 - 29*t**8/320 + 99*t**7/1120 + 61*t**6/480 - 7*t**5/32 - 5*t**4/48 + t**3/6 + t**2/12) + x_cp(4) *      &
             (-t**9/144 + t**8/40 - 3*t**7/224 - t**6/24 + t**5/30 + t**4/24)) + x_cp(4)*(x_cp(1)*(-t**9/432 + 11*t**8/960 -          &
             29*t**7/1680 - t**6/1080 + t**5/45 - t**4/72) + x_cp(2)*(t**9/144 - 7*t**8/240 + t**7/35 + t**6/36 - 2*t**5/45) +        &
             x_cp(3)*(-t**9/144 + 23*t**8/960 - 19*t**7/1680 - 13*t**6/360 + t**5/45 + t**4/72) + x_cp(4)*(t**9/432 - t**8/160 +      &
             t**6/108))) + y_cp(3)*(x_cp(1)*(x_cp(1)*(-t**9/432 + 7*t**8/480 - t**7/30 + 31*t**6/1080 + t**5/360 - t**4/144 -         &
             t**3/48 + t**2/48) + x_cp(2)*(t**9/144 - 37*t**8/960 + 39*t**7/560 - t**6/36 - 5*t**5/144 - t**4/96 + t**3/18) +         &
             x_cp(3)*(-t**9/144 + t**8/30 - 19*t**7/420 - t**6/180 + 13*t**5/360 + t**4/36 - 5*t**3/144 - t**2/48) + x_cp(4) *        &
             (t**9/432 - 3*t**8/320 + t**7/112 + t**6/216 - t**5/240 - t**4/96)) + x_cp(2)*(x_cp(1)*(t**9/144 - 3*t**8/80 +           &
             71*t**7/1120 - 3*t**6/160 - 13*t**5/480 - t**4/96 + t**3/36) + x_cp(2)*(-t**9/48 + 31*t**8/320 - 127*t**7/1120 -         &
             t**6/24 + t**5/20 + t**4/12) + x_cp(3)*(t**9/48 - 13*t**8/160 + 61*t**7/1120 + 13*t**6/160 - t**5/160 - 7*t**4/96 -      &
             t**3/36) + x_cp(4)*(-t**9/144 + 7*t**8/320 - t**7/224 - t**6/48 - t**5/60)) + x_cp(3)*(x_cp(1)*(-t**9/144 + t**8/32 -    &
             t**7/28 - t**6/72 + t**5/40 + t**4/48 - t**3/144 - t**2/48) + x_cp(2)*(t**9/48 - 5*t**8/64 + 5*t**7/112 + t**6/12 -      &
             t**5/240 - 7*t**4/96 - t**3/18) + x_cp(3)*(-t**9/48 + t**8/16 - t**6/12 - t**5/24 + t**4/24 + t**3/16 +                  &
             t**2/48) + x_cp(4)*(t**9/144 - t**8/64 - t**7/112 + t**6/72 + t**5/48 + t**4/96)) + x_cp(4)*(x_cp(1)*(t**9/432 -         &
             t**8/120 + 19*t**7/3360 + 17*t**6/4320 - t**5/1440 - t**4/288) + x_cp(2)*(-t**9/144 + 19*t**8/960 - t**7/1120 - t**6/72  & 
             - t**5/90) + x_cp(3)*(t**9/144 - 7*t**8/480 - 31*t**7/3360 + 11*t**6/1440 + 17*t**5/1440 + t**4/288) + x_cp(4) *         &
             (-t**9/432 + t**8/320 + t**7/224 + t**6/432))) + y_cp(4)*(x_cp(1)*(x_cp(1)*(t**9/1296 - 11*t**8/2880 + 73*t**7/10080 -   &
             t**6/160 + t**5/480) + x_cp(2)*(-t**9/432 + 7*t**8/720 - 47*t**7/3360 + t**6/144) + x_cp(3)*(t**9/432 - 23*t**8/2880 +   &
             83*t**7/10080 - t**6/1440 - t**5/480) + x_cp(4)*(-t**9/1296 + t**8/480 - t**7/672)) + x_cp(2)*(x_cp(1)*(-t**9/432 +      &
             3*t**8/320 - t**7/80 + t**6/180) + x_cp(2)*(t**9/144 - 11*t**8/480 + 2*t**7/105) + x_cp(3)*(-t**9/144 + 17*t**8/960 -    &
             11*t**7/1680 - t**6/180) + x_cp(4)*(t**9/432 - t**8/240)) + x_cp(3)*(x_cp(1)*(t**9/432 - 7*t**8/960 + t**7/160 +         &
             t**6/1440 - t**5/480) + x_cp(2)*(-t**9/144 + t**8/60 - 17*t**7/3360 - t**6/144) + x_cp(3)*(t**9/144 - 11*t**8/960 -      &
             3*t**7/1120 + t**6/160 + t**5/480) + x_cp(4)*(-t**9/432 + t**8/480 + t**7/672)) + x_cp(4)*(t**9*x_cp(4)/1296 + x_cp(1) * &
             (-t**9/1296 + t**8/576 - t**7/1008) + x_cp(2)*(t**9/432 - t**8/288) + x_cp(3)*(-t**9/432 + t**8/576 + t**7/1008))) ) +   &
             angle0*(x_cp(1)*(-t**3/6 + t**2/2 - t/2) + x_cp(2)*(t**3/2 - t**2) + x_cp(3)*(-t**3/2 + t**2/2 + t/2) + x_cp(4) *        &
             (t**3/6) ) + camber0


end function
!----------------------------------------------------------------------------------------------------------------------------------






!
! Subroutine to construct camber-line using second derivative specification
! Also rotates the camber-line by specified stagger/twist
!
! Input parameters: casename    - name of current case
!                   isdev       - flag for debugging output file writes
!                   xcp         - control points along chord-line u
!                   ycp         - control points for camber-line second derivative
!                   ncp         - number of control points
!                   u           - chord-line distribution
!                   np          - number of points along the chord-line and camber-line
!                   ainl        - inlet angle of staggered curve if wing_flag = 0
!                                 used to calculate total camber (= aext - ainl) if wing_flag = 0
!                                 treated as twist (stagger) angle if wing_flag = 1
!                   aext        - exit angle of staggered curve if wing_flag = 0
!                                 used to calculate total camber (= aext - ainl) if wing_flag = 0
!                                 is equal to the total camber if wing_flag = 1
!                   chrdx       - projection of the curve length on x-axis (axial chord length)
!                                 used to calculate total chord length
!                   wing_flag   - if wing_flag = 0, total camber = aext - ainl
!                                 if wing_flag = 1, total camber = aext, ainl is the twist angle
!
! Computes the splinedata array of size (6, np) with rows 1 and 2 containing the coordinates of the
! camber-line, row 3 containing the slope of the camber-line and row 4 containing the second derivative
! of the camber-line
!
! Also computes the stagger/twist angle 'sang', the actual chord length 'chrd', the slopes of the camber
! line at the spline segment ends 'init_angles' and the camber at the spline segment ends 'init_cams'
!
!----------------------------------------------------------------------------------------------------------------------------------
subroutine camline(casename, isdev, ncp, np, xcp, ycp, u, ainl, aext, chrdx, wing_flag, &
                   sang, chrd, init_angles, init_cams, u_end, splinedata)
    use file_operations
    use errors
    use funcNsubs
    implicit none

    ! Constant parameters
    real,                   parameter   :: dtor = 4.*atan(1.)/180.
    integer,                parameter   :: splinedata_col = 6

    character(*),   intent(in)          :: casename
    logical,        intent(in)          :: isdev
    integer,        intent(in)          :: ncp, np
    real,           intent(in)          :: xcp(ncp), ycp(ncp), u(np), ainl, aext, chrdx
    integer,        intent(in)          :: wing_flag
    real,           intent(inout)       :: sang, chrd, init_angles(ncp - 2), init_cams(ncp - 2), &
                                           u_end(ncp - 2), splinedata(splinedata_col, np)

    ! Local variables    
    integer                             :: i, j, nopen, js
    real                                :: P, knew, det, k1, k2, curv(np), cam(np), cam_u(np), &
                                           cam_u_dev(np), tot_cam, d1v_end(ncp - 2), v_end(ncp - 2),  &
                                           xcp_seg(4), ycp_seg(4), t, angle0, camber0,                &
                                           intg_d2v_end(ncp - 2), intg_d1v_end(ncp - 2), sang2,       &
                                           sc_factor_dev, inlet_uv_dev, exit_uv_dev
    character(:),   allocatable         :: log_file, error_msg, dev_msg
    character(10)                       :: error_arg
    logical                             :: file_open, isquiet
    
    ! Functions used by this subroutine
    ! bspline_t_newton in bspline3.f90
    ! bspline in bspline3.f90
    real                                :: bspline_t_newton, camber, angle, bspline


    !
    ! Get isquiet status and section number
    ! Initialize variables
    !
    call get_quiet_status(isquiet)
    call get_sec_number(js)
    tot_cam                             = 0.0


    
    !
    ! Write control points to log file
    !
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) then
        print*, 'xcp', xcp
        print*, 'ycp', ycp
    end if
    write(nopen,*) 'xcp', xcp
    write(nopen,*) 'ycp', ycp



    !
    ! Get total camber depending on wing flag
    !
    if (wing_flag .eq. 0) then
        tot_cam                         = aext - ainl
    else if (wing_flag .eq. 1) then
        tot_cam                         = aext
    end if



    !
    ! Initialize quantities
    !
    intg_d2v_end                        = 0.0
    intg_d1v_end                        = 0.0
    u_end                               = 0.0
    t                                   = 1.0
    init_angles                         = 0.0
    init_cams                           = 0.0
    splinedata                          = 0.0

    if (tot_cam == 0.0) then
        sang                            = ainl
        chrd                            = chrdx/abs(cos(sang))
    end if

    

    !
    ! Initialize B-spline segments
    !
    do j = 1, ncp - 3

        xcp_seg                         = xcp(j:j + 3)
        ycp_seg                         = ycp(j:j + 3)
        
        if (j == ncp - 3) then
           u_end(j + 1)                 = 1.0
        else
           u_end(j + 1)                 = bspline(xcp_seg, t)
        end if

        angle0                          = intg_d2v_end(j)
        camber0                         = intg_d1v_end(j)
        intg_d2v_end(j+1)               = angle(ycp_seg, xcp_seg, angle0, t)
        intg_d1v_end(j+1)               = camber(ycp_seg, xcp_seg, angle0, camber0, t)

    end do



    !
    ! intg_d2v_end(ncp-2) is total integral of second derivative of v upto u = 1
    ! intg_d1v_end(ncp-2) is total integral of first derivative of v upto u = 1
    ! P is a grouping of terms in the scaling factor equation
    !
    P                                   = (intg_d2v_end(ncp - 2) * intg_d1v_end(ncp - 2)) - &
                                         &(intg_d1v_end(ncp - 2)**2)
   
    ! Write total camber to file and print to screen 
    if (.not. isquiet) write (*, '(A, F20.15)') 'Total camber is: ', tot_cam/dtor
    write (nopen, '(A, F20.15)') 'Total camber is: ', tot_cam/dtor



    !
    ! det is the determinant of quadratic equation in k (scaling factor)
    ! Equation (2.21) in Karthik Balasubramanian's MS thesis
    !
    det                                 = (intg_d2v_end(ncp - 2)**2) + (4*P*(tan(tot_cam)**2))

    ! Write determinant to log file and print to screen
    if (.not. isquiet) write (*, '(A, F20.15)') 'Determinant is: ', det
    write (nopen, '(A, F20.15)') 'Determinant is: ', det
   
    ! Display error message if no real roots are found 
    if (det < 0.0) then 

        write(error_arg,'(I2)') js
        error_msg                       = 'All possible scaling factors for curvature control points &
                                          &are complex for section '//trim(adjustl(error_arg))
        dev_msg                         = 'Check subroutine camline in bsplinecam.f90'
        call error(error_msg, dev_msg)
        call exit

    end if



    !
    ! Calculating both possible roots to solve for k
    !
    k1                                  = (-intg_d2v_end(ncp - 2) + sqrt(det))/(2*P*tan(tot_cam))
    k2                                  = (-intg_d2v_end(ncp - 2) - sqrt(det))/(2*P*tan(tot_cam))
    
    ! Choosing appropriate root
    if (.not. isquiet) write (*, '(A, 2F25.15)') 'Possible values of scaling factor are: ', k1, k2
    write (nopen, '(A, 2F25.15)') 'Possible values of scaling factor are: ', k1, k2

    !if (isdev) then

    !   knew2 = min(k1, k2)
    !   d1v_end = knew2*(intg_d2v_end-intg_d1v_end(ncp-2))
    !   if (abs((atan(d1v_end(ncp-2))-atan(d1v_end(1)) - tot_cam)/tot_cam) .gt. 1E-7) then
    !      knew2 = max(k1, k2)
    !      d1v_end = knew2*(intg_d2v_end-intg_d1v_end(ncp-2))
    !   end if
    !   if (abs(knew2 - k1) .lt. 10E-6) then
    !      knew2 = k2
    !   else if (abs(knew2 - k2) .lt. 10E-6) then
    !      knew2 = k1
    !   end if

    !   d1v_end = knew2*(intg_d2v_end - intg_d1v_end(ncp - 2))
    !   v_end = knew2*(intg_d1v_end-(u_end*intg_d1v_end(ncp-2)))
    !   sc_factor_dev = knew2
    !   inlet_uv_dev  = atan(d1v_end(1))/dtor
    !   exit_uv_dev   = atan(d1v_end(ncp - 2))/dtor
    !   !write (*, '(A, F20.15)') 'Camber line second derivative scaling factor: ', knew2
    !   !write (*, '(A, F20.15, /, A, F20.15)') 'Inlet u-v metal angle in deg: ', atan(d1v_end(1))/dtor, &
    !   !'Exit u-v metal angle in deg: ', atan(d1v_end(ncp-2))/dtor
    !   !write (nopen, '(A, F20.15)') 'Camber line second derivative scaling factor: ', knew2
    !   !write (nopen, '(A, F20.15, /, A, F20.15)') 'Inlet u-v metal angle in deg: ', atan(d1v_end(1))/dtor, &
    !   !'Exit u-v metal angle in deg: ', atan(d1v_end(ncp-2))/dtor

    !   init_angles = knew2*(intg_d2v_end-intg_d1v_end(ncp-2))
    !   init_cams = knew2*(intg_d1v_end-(u_end*intg_d1v_end(ncp-2)))

    !   ! Loop to construct the splinedata 2D array
    !   do i = 1, np
    !      do j = 1, (ncp-3)
    !         if (u(i) .eq. u_end(j)) then
    !            ycp_seg = ycp(j:j+3)
    !            t = 0.
    !            curv(i) = knew2*bspline(ycp_seg, t)
    !            cam_u(i) = d1v_end(j)
    !            cam(i) = v_end(j)
    !            exit
    !         else if (u(i) .eq. 1.) then
    !            ycp_seg = ycp(ncp-3:ncp)
    !            t = 1.
    !            curv(i) = knew2*bspline(ycp_seg, t)
    !            cam_u(i) = d1v_end(ncp-2)
    !            cam(i) = v_end(ncp-2)
    !            exit
    !         else if ((u(i) .gt. u_end(j)) .and. (u(i) .lt. u_end(j+1))) then
    !            xcp_seg = xcp(j:j+3)
    !            ycp_seg = ycp(j:j+3)
    !            angle0 = intg_d2v_end(j)
    !            camber0 = intg_d1v_end(j)
    !            t = bspline_t_newton(xcp_seg, u(i))
    !            curv(i) = knew2*bspline(ycp_seg, t)
    !            cam_u(i) = knew2*(angle(ycp_seg, xcp_seg, angle0, t)-intg_d1v_end(ncp-2))
    !            cam(i) = knew2*(camber(ycp_seg, xcp_seg, angle0, camber0, t)-(u(i)*intg_d1v_end(ncp-2)))
    !            exit
    !         end if
    !      end do
    !      splinedata(1, i) = u(i)
    !      splinedata(2, i) = cam(i)
    !      splinedata(3, i) = cam_u(i)
    !      splinedata(4, i) = curv(i) 
    !   end do
    ! 
    !   cam_u_dev   = cam_u

    !end if


    
    !
    ! Select scaling factor
    !
    knew                                = min(k1, k2)
    d1v_end                             = knew*(intg_d2v_end - intg_d1v_end(ncp - 2))
    if (abs((atan(d1v_end(ncp-2)) - atan(d1v_end(1)) - tot_cam)/tot_cam) > 1E-7) then
        knew                            = max(k1, k2)
        d1v_end                         = knew*(intg_d2v_end - intg_d1v_end(ncp - 2))
    end if
    v_end                               = knew*(intg_d1v_end - (u_end*intg_d1v_end(ncp - 2)))



    !
    ! Print output to screen and write to log file
    !
    if (isdev) then

        if (.not. isquiet) then
            write(*, '(A, 2F25.15)') 'Camber line second derivative scaling factor: ', knew, sc_factor_dev
            write(*, '(A, 2F25.15)') 'Inlet u-v metal angle in deg: ', atan(d1v_end(1))/dtor, inlet_uv_dev
            write(*, '(A, 2F25.15)') 'Exit u-v metal angle in deg: ', atan(d1v_end(ncp - 2))/dtor, exit_uv_dev
        end if
        write(nopen, '(A, 2F25.15)') 'Camber line second derivative scaling factor: ', knew, sc_factor_dev
        write(nopen, '(A, 2F25.15)') 'Inlet u-v metal angle in deg: ', atan(d1v_end(1))/dtor, inlet_uv_dev
        write(nopen, '(A, 2F25.15)') 'Exit u-v metal angle in deg: ', atan(d1v_end(ncp - 2))/dtor, exit_uv_dev

    else

        if (.not. isquiet) then
            write (*, '(A, F20.15)') 'Camber line second derivative scaling factor: ', knew
            write (*, '(A, F20.15, /, A, F20.15)') 'Inlet u-v metal angle in deg: ', atan(d1v_end(1))/dtor, &
                                                  &'Exit u-v metal angle in deg: ', atan(d1v_end(ncp-2))/dtor
        end if
        write (nopen, '(A, F20.15)') 'Camber line second derivative scaling factor: ', knew
        write (nopen, '(A, F20.15, /, A, F20.15)') 'Inlet u-v metal angle in deg: ', atan(d1v_end(1))/dtor, &
                                                  &'Exit u-v metal angle in deg: ', atan(d1v_end(ncp-2))/dtor

    end if



    !
    ! Populate arrays containing spline segment endpoints camber slope and camber
    ! values
    !
    init_angles                         = knew*(intg_d2v_end - intg_d1v_end(ncp - 2))
    init_cams                           = knew*(intg_d1v_end - (u_end*intg_d1v_end(ncp - 2)))



    !
    ! Generate splinedata array
    !
    do i = 1, np

        do j = 1, ncp - 3

            ! At segment end     
            if (u(i) == u_end(j)) then

                ycp_seg                 = ycp(j:j + 3)
                t                       = 0.0
                curv(i)                 = knew*bspline(ycp_seg, t)
                cam_u(i)                = d1v_end(j)
                cam(i)                  = v_end(j)
                exit

            ! At beginning of segment
            else if (u(i) == 1.0) then

                ycp_seg                 = ycp(ncp - 3:ncp)
                t                       = 1.0
                curv(i)                 = knew*bspline(ycp_seg, t)
                cam_u(i)                = d1v_end(ncp - 2)
                cam(i)                  = v_end(ncp - 2)
                exit

            ! Generate segment
            else if ((u(i) > u_end(j)) .and. (u(i) < u_end(j+1))) then

                xcp_seg                 = xcp(j:j + 3)
                ycp_seg                 = ycp(j:j + 3)
                angle0                  = intg_d2v_end(j)
                camber0                 = intg_d1v_end(j)
                t                       = bspline_t_newton(xcp_seg, u(i))
                curv(i)                 = knew*bspline(ycp_seg, t)
                cam_u(i)                = knew*(angle(ycp_seg, xcp_seg, angle0, t) - intg_d1v_end(ncp - 2))
                cam(i)                  = knew*(camber(ycp_seg, xcp_seg, angle0, camber0, t) - (u(i)*intg_d1v_end(ncp - 2)))
                exit

            end if  ! u(i)

        end do   ! j = 1, ncp - 3

        splinedata(1, i)                = u(i)
        splinedata(2, i)                = cam(i)
        splinedata(3, i)                = cam_u(i)
        splinedata(4, i)                = curv(i) 

    end do  ! i = 1, np


    
    !
    ! Stagger/Twist calculation
    !
    if (wing_flag == 0) then
        
        sang                            = (ainl - atan(cam_u(1)))
        if (isdev) then
           sang2                        = (ainl - atan(cam_u_dev(1)))
           if (.not. isquiet) write(*, '(A, 2F25.15)') 'Stagger angle in deg: ', sang/dtor, sang2/dtor
           write(nopen, '(A, 2F25.15)') 'Stagger angle in deg: ', sang/dtor, sang2/dtor
        else
           if (.not. isquiet) write (*, '(A, F20.15)') 'Stagger angle in deg: ', sang/dtor
           write (nopen, '(A, F20.15)') 'Stagger angle in deg: ', sang/dtor
        end if

    else if (wing_flag == 1) then

        sang                            = ainl
        if (.not. isquiet) write (*, '(A, F20.15)') 'Twist angle in deg: ', sang/dtor
        write (nopen, '(A, F20.15)') 'Twist angle in deg: ', sang/dtor

    end if  ! wing_flag

    ! Close log file
    call close_log_file(nopen, file_open)



    !
    ! Calculation of actual chord after stagger/twist
    !
    chrd = chrdx/abs(cos(sang))


end subroutine camline
!----------------------------------------------------------------------------------------------------------------------------------





















