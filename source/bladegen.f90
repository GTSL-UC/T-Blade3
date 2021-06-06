subroutine bladegen(nspn,thkc,mr1,sinl,sext,chrdx,js,fext,xcen,ycen,airfoil, stagger,stack,chord_switch,  &
                    stk_u,stk_v,xb_stk,yb_stk,stack_switch, clustering_switch, clustering_parameter,nsl,  &
                    nbls,curv_camber,thick,LE,np, ncp_curv,ncp_thk, curv_cp,thk_cp, wing_flag, lethk_all, &
                    tethk_all,s_all,ee_all,thick_distr, umxthk_all,C_le_x_top_all,C_le_x_bot_all,         &
                    C_le_y_top_all,C_le_y_bot_all,LE_vertex_ang_all,LE_vertex_dis_all,sting_l_all,        &
                    sting_h_all,LEdegree,no_LE_segments,sec_radius,bladedata,amount_data,scf,             &
                    intersec_coord,throat_index, n_normal_distance,casename,develop,mble,mbte,msle,       &
                    mste,i_slope,jcellblade_all, etawidth_all,BGgrid_all,thk_tm_c_spl, theta_offset,      &
                    TE_der_actual,TE_der_norm,m_prime,theta,spanwise_thk,n_ext,m_mean,th_mean,mt_umax,    &
                    dsinl_dxcp,dsinl_dycp,dsext_dxcp,dsext_dycp,chrdx_ders,dmprime_dcurv,dtheta_dcurv,    &
                    dmprime_dt,dtheta_dt,dmprime_dinbeta,dtheta_dinbeta,dmprime_doutbeta,dtheta_doutbeta, &
                    dmprime_dcm,dtheta_dcm)

    use globvar,                only: ncp_span_curv, ncp_chord_curv, ncp_span_thk, cpinbeta, cpoutbeta, cpchord
    use file_operations
    use errors
    use funcNsubs
    use derivatives
    implicit none

    integer,                    intent(in)      :: nspn, js, stack, stack_switch, chord_switch, nsl, nbls, curv_camber, thick, &
                                                   LE, ncp_curv(nsl), ncp_thk(nsl), wing_flag, thick_distr, LEdegree,          &
                                                   no_LE_segments, amount_data, i_slope, n_ext, clustering_switch
    integer,                    intent(inout)   :: np, throat_index(nspn), n_normal_distance
    real,                       intent(in)      :: thkc, mr1, chrdx, xcen, ycen, stk_u(1), stk_v(1), xb_stk, yb_stk,            &
                                                   clustering_parameter, curv_cp(20,2*nsl), thk_cp(20,2*nsl), lethk_all(nsl),   &
                                                   tethk_all(nsl), s_all(nsl), ee_all(nsl), umxthk_all(nsl),                    &
                                                   C_le_x_top_all(nsl), C_le_x_bot_all(nsl), C_le_y_top_all(nsl),               &
                                                   C_le_y_bot_all(nsl), LE_vertex_ang_all(nsl), LE_vertex_dis_all(nsl),         &
                                                   sting_l_all(nsl), sting_h_all(nsl), sec_radius(nsl,2), scf, msle, mste,      &
                                                   jcellblade_all(nspn), etawidth_all(nspn), BGgrid_all(nspn),                  &
                                                   thk_tm_c_spl(nsl), theta_offset, dsinl_dxcp(cpinbeta), dsinl_dycp(cpinbeta), &
                                                   dsext_dxcp(cpoutbeta), dsext_dycp(cpoutbeta), chrdx_ders(2, cpchord)
    real,                       intent(inout)   :: sinl, sext, stagger, bladedata(amount_data,nsl), m_prime(500), theta(500),   &
                                                   spanwise_thk(nspn), mble, mbte, intersec_coord(12,nsl), m_mean(200),         &
                                                   th_mean(200), mt_umax(4)
    real,                       intent(inout)   :: dmprime_dcurv(np, ncp_chord_curv - 1, ncp_span_curv),                        &
                                                   dtheta_dcurv(np, ncp_chord_curv - 1, ncp_span_curv),                         &
                                                   dmprime_dt(np, 5, ncp_span_thk), dtheta_dt(np, 5, ncp_span_thk),             &
                                                   dmprime_dinbeta(np, 2, cpinbeta), dtheta_dinbeta(np, 2, cpinbeta),           &
                                                   dmprime_doutbeta(np, 2, cpoutbeta), dtheta_doutbeta(np, 2, cpoutbeta),       &
                                                   dmprime_dcm(np, 2, cpchord), dtheta_dcm(np, 2, cpchord)
    character(*),               intent(in)      :: fext, airfoil, casename, develop
    logical                                     :: TE_der_actual, TE_der_norm

    ! Local variables
    integer                                     :: np_side, i, naca, np_cluster, ncp, i_le, i_te, oo, nopen
    integer,    parameter                       :: nspan = 200, nx = 500, nxx = 1000, nrow = 1, nax = 50, nb = 300,             &
                                                   spline_data = 6, interval = 6, pt2 = 1, TE_del = 0
    real                                        :: chrd, pitch, radius_pitch, scaling, lethk, thkmultip, aext, ainl, area, cam, &
                                                   cam_u, dtor, pi, flex, flin, fmxthk, rr1, rr2, sang, sexts, sinls, tethk,    &
                                                   thk, ui, umxthk, xi, yi, xxa, yya, u_le, uin_le, Zweifel(nsl), ucp_top(11),  &
                                                   vcp_top(11), ucp_bot(11), vcp_bot(11), xcp_LE, ycp_LE, xcp_TE, ycp_TE,       &
                                                   cp_LE(4,2), cp_TE(4,2), a_NACA(4), d_NACA(4), t_max, u_max, t_TE, dy_dx_TE,  &
                                                   LE_round, min_throat_2D, u_translation, camber_trans, u_rot, camber_rot,     &
                                                   u_TE_quadratic_a, u_TE_quadratic_b, u_TE_quadratic_c, u_TE, u_center,        &
                                                   TE_radius, cam_umax, slope_umax, u_stack, v_stack
    real,                   allocatable         :: init_angles(:), init_cambers(:), x_spl_end_curv(:), xcp_curv(:), ycp_curv(:),&
                                                   xcp_thk(:), ycp_thk(:), ueq(:), xmean(:), ymean(:), xtop(:), ytop(:),        &
                                                   xbot(:), ybot(:), u(:), xb(:), yb(:), u_new(:), splthick(:), thickness(:),   &
                                                   angle(:), camber(:), slope(:), thickness_data(:,:), splinedata(:,:),         &
                                                   u_ders(:,:), NACA_ders(:,:), dcam_dxcp(:,:),  dcam_dycp(:,:),                &
                                                   dslope_dxcp(:,:), dslope_dycp(:,:), dcam_u(:), dslope_u(:), cam_ders(:,:),   &
                                                   slope_ders(:,:), dsang_dcurv(:), dchrd_dcurv(:), dsang_dx_inbeta(:),         &
                                                   dsang_dy_inbeta(:), dchrd_dx_inbeta(:), dchrd_dy_inbeta(:),                  &
                                                   dslope_dx_inbeta(:,:), dslope_dy_inbeta(:,:), dcam_dx_inbeta(:,:),           &
                                                   dcam_dy_inbeta(:,:), dslope_doutbeta(:,:,:), dcam_doutbeta(:,:,:),           &
                                                   dsang_doutbeta(:,:), dchrd_doutbeta(:,:), dchrd_dcm(:,:)
    real,                   allocatable         :: dxbot_dt(:,:,:), dybot_dt(:,:,:), dxtop_dt(:,:,:), dytop_dt(:,:,:),          &
                                                   dxbot_dcurv(:,:,:), dybot_dcurv(:,:,:), dxtop_dcurv(:,:,:),                  &
                                                   dytop_dcurv(:,:,:), du_dt(:,:,:), dv_dt(:,:,:), du_dcurv(:,:,:),             &
                                                   dv_dcurv(:,:,:), dxbot_dinbeta(:,:,:), dybot_dinbeta(:,:,:),                 &
                                                   dxtop_dinbeta(:,:,:), dytop_dinbeta(:,:,:), du_dinbeta(:,:,:),               &
                                                   dv_dinbeta(:,:,:), dxbot_doutbeta(:,:,:), dybot_doutbeta(:,:,:),             &
                                                   dxtop_doutbeta(:,:,:), dytop_doutbeta(:,:,:), du_doutbeta(:,:,:),            &
                                                   dv_doutbeta(:,:,:)
    character(80)                               :: file1, file7
    character(20)                               :: sec
    character(:),           allocatable         :: log_file, error_msg, dev_msg, stagger_file, msg_1, msg_2
    logical                                     :: ellip, file_open, isdev, isquiet, monotonic = .true., write_to_file = .true.,&
                                                   exist
    common / BladeSectionPoints /xxa(nxx, nax), yya(nxx, nax) 



    !
    ! Get the value of isdev and isquiet
    !
    call get_dev_status(isdev)
    call get_quiet_status(isquiet)

    ! Initialize LE and TE indices
    i_le = 0
    i_te = 0

    ! Local constants
    pi = 4.*atan(1.0)
    dtor = pi/180.

    ! Initialize variables
    ucp_top = 0
    vcp_top = 0


    
    !
    ! Convert radial section number into a string
    !
    write(sec, '(i2)') js


    
    ! 
    ! Assign NACA airfoil type 
    !
    if(len_trim(airfoil) == 4)then
        naca = 0
        read (airfoil, '(i4)') naca
        if (.not. isquiet) print*, "naca = ", naca
    end if
   

    
    ! 
    ! Print airfoil type data to screen and write to log file
    !
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) then
        write(*, *)
        write(*, *) '---------------------------------------------------------------------------------'
        write(*, *) 'airfoil type:', js, trim(airfoil)
        write(*, *) ' =================================================== '
    end if

    write(nopen, *)
    write(nopen, *) '---------------------------------------------------------------------------------'
    write(nopen, *) 'airfoil type:', js, trim(airfoil)
    write(nopen, *) ' =================================================== '
    call close_log_file(nopen, file_open)



    !
    ! Initialize derivatives of (m',theta) sections to 0
    ! These are changed only when using 'sect1' airfoil,
    ! and curv_camber
    !
    !dmprime_dinbeta     = 0.0
    !dmprime_doutbeta    = 0.0
    !dmprime_dcm         = 0.0
    !dmprime_dcurv       = 0.0
    !dmprime_dt          = 0.0

    !dtheta_dinbeta      = 0.0
    !dtheta_doutbeta     = 0.0
    !dtheta_dcm          = 0.0
    !dtheta_dcurv        = 0.0
    !dtheta_dt           = 0.0


    
    !
    ! Number of coordinates for the airfoil
    !
    np      = 121
    np_side = np



    !
    ! Allocation of arrays
    !
    if (allocated(splinedata    )) deallocate(splinedata    )
    allocate(splinedata(spline_data,np_side))

    ! Top and bottom blade surface points
    if (allocated(xtop          )) deallocate(xtop          )
    if (allocated(ytop          )) deallocate(ytop          )
    if (allocated(xbot          )) deallocate(xbot          )
    if (allocated(ybot          )) deallocate(ybot          )
    allocate(xtop(np),ytop(np),xbot(np),ybot(np))

    ! Non-dimensional u
    if (allocated(u             )) deallocate(u             )
    if (allocated(u_new         )) deallocate(u_new         )
    allocate(u(np),u_new(np)) 

    ! Thickness arrays
    if (allocated(splthick      )) deallocate(splthick      )
    if (allocated(thickness     )) deallocate(thickness     )
    if (allocated(thickness_data)) deallocate(thickness_data)
    allocate(splthick(np),thickness(np), &
         thickness_data(np,12))

    ! Mean-line arrays
    if (allocated(angle         )) deallocate(angle         )
    if (allocated(camber        )) deallocate(camber        )
    if (allocated(slope         )) deallocate(slope         )
    allocate(angle(np),camber(np),slope(np))

    ! (m',theta) blade surface points
    if (allocated(xb            )) deallocate(xb            )
    if (allocated(yb            )) deallocate(yb            )
    allocate(xb(2*np - 1),yb(2*np - 1))

    ! (m', theta) meanline points
    if (allocated(ueq           )) deallocate(ueq           )
    if (allocated(xmean         )) deallocate(xmean         )
    if (allocated(ymean         )) deallocate(ymean         )
    allocate(ueq(np_side),xmean(np_side),ymean(np_side))

    ! Derivatives of u wrt parameters (ellipse-based clustering)
    if (allocated(u_ders        )) deallocate(u_ders        )
    allocate(u_ders(np, 5))

    ! Derivatives of NACA thickness wrt parameters
    if (allocated(NACA_ders     )) deallocate(NACA_ders     )
    allocate(NACA_ders(np, 5))

    ! Sensitivity of mean-line wrt chordwise control points
    if (allocated(dcam_dxcp)) deallocate(dcam_dxcp)
    allocate(dcam_dxcp(np, ncp_curv(js) - 2))
    if (allocated(dcam_dycp)) deallocate(dcam_dycp)
    allocate(dcam_dycp(np, ncp_curv(js) - 2))

    ! Sensitivity of mean-line 1st derivative wrt chordwise
    ! control points
    if (allocated(dslope_dxcp)) deallocate(dslope_dxcp)
    allocate(dslope_dxcp(np, ncp_curv(js) - 2))
    if (allocated(dslope_dycp)) deallocate(dslope_dycp)
    allocate(dslope_dycp(np, ncp_curv(js) - 2))

    ! Sensitivity of mean-line wrt u (ellipse-based clustering)
    ! and thickness parameters
    if (allocated(dcam_u)) deallocate(dcam_u)
    allocate(dcam_u(np))
    if (allocated(cam_ders)) deallocate(cam_ders)
    allocate(cam_ders(np, 5))

    ! Sensitivity of mean-line 1st derivatives wrt u
    ! (ellipse based clustering) and thickness parameters
    if (allocated(dslope_u)) deallocate(dslope_u)
    allocate(dslope_u(np))
    if (allocated(slope_ders)) deallocate(slope_ders)
    allocate(slope_ders(np, 5))

    ! Sensitivity of the stagger angle to the mean-line
    ! second derivative control points
    if (allocated(dsang_dcurv)) deallocate(dsang_dcurv)
    allocate(dsang_dcurv((2 * (ncp_curv(js) - 2)) - 2))
    if (allocated(dchrd_dcurv)) deallocate(dchrd_dcurv)
    allocate(dchrd_dcurv((2 * (ncp_curv(js) - 2)) - 2))

    ! Sensitivity of the stagger to the control points
    ! of span and in_beta* specified in 3dbgbinputs file
    if (allocated(dsang_dx_inbeta)) deallocate(dsang_dx_inbeta)
    allocate(dsang_dx_inbeta(size(dsinl_dxcp)))
    if (allocated(dsang_dy_inbeta)) deallocate(dsang_dy_inbeta)
    allocate(dsang_dy_inbeta(size(dsinl_dycp)))

    ! Sensitivity of the stagger to the control points
    ! of span and in_beta* specified in 3dbgbinputs file
    if (allocated(dchrd_dx_inbeta)) deallocate(dchrd_dx_inbeta)
    allocate(dchrd_dx_inbeta(size(dsinl_dxcp)))
    if (allocated(dchrd_dy_inbeta)) deallocate(dchrd_dy_inbeta)
    allocate(dchrd_dy_inbeta(size(dsinl_dycp)))

    ! Sensitivity of the mean-line derivatives wrt control
    ! points of Span (inlet flow angle) and in_beta*
    if (allocated(dslope_dx_inbeta)) deallocate(dslope_dx_inbeta)
    allocate(dslope_dx_inbeta(np, cpinbeta))
    if (allocated(dslope_dy_inbeta)) deallocate(dslope_dy_inbeta)
    allocate(dslope_dy_inbeta(np, cpinbeta))

    ! Sensitivity of the mean-line wrt control
    ! points of Span (inlet flow angle) and in_beta*
    if (allocated(dcam_dx_inbeta)) deallocate(dcam_dx_inbeta)
    allocate(dcam_dx_inbeta(np, cpinbeta))
    if (allocated(dcam_dy_inbeta)) deallocate(dcam_dy_inbeta)
    allocate(dcam_dy_inbeta(np, cpinbeta))

    ! Sensitivity of the mean-line 1st derivative wrt control
    ! points of Span (exit flow angle) and out_beta*
    if (allocated(dslope_doutbeta)) deallocate(dslope_doutbeta)
    allocate(dslope_doutbeta(np, 2, cpoutbeta))

    ! Sensitivity of the mean-line wrt control
    ! points of Span (exit flow angle) and out_beta*
    if (allocated(dcam_doutbeta)) deallocate(dcam_doutbeta)
    allocate(dcam_doutbeta(np, 2, cpoutbeta))

    ! Sensitivity of the stagger to the control points
    ! of span and out_beta* specified in 3dbgbinputs file
    if (allocated(dsang_doutbeta)) deallocate(dsang_doutbeta)
    allocate(dsang_doutbeta(2, cpoutbeta))

    ! Sensitivity of the chord to the control points
    ! of span and out_beta* specified in 3dbgbinputs file
    if (allocated(dchrd_doutbeta)) deallocate(dchrd_doutbeta)
    allocate(dchrd_doutbeta(2, cpoutbeta))

    ! Sensitivity of the chord to the control points
    ! of span and chord_multiplier
    if (allocated(dchrd_dcm)) deallocate(dchrd_dcm)
    allocate(dchrd_dcm(2, cpchord))



    !
    ! Initialization of variables
    !
    u           = 0 
    splthick    = 0 
    thickness   = 0
    angle       = 0
    camber      = 0 
    slope       = 0 
    xb          = 0 
    yb          = 0
    xtop        = 0 
    ytop        = 0 
    xbot        = 0 
    ybot        = 0 
    splinedata  = 0
    ueq         = 0 
    xmean       = 0 
    ymean       = 0 
    u_new       = 0



    !
    ! Compute NACA thickness coefficients
    ! Computing here to enable ellipse based clustering
    !
    if (thick_distr == 5) then

        t_max    = thk_cp(3,js)
        u_max    = thk_cp(2,js)
        LE_round = thk_cp(1,js)
        t_TE     = thk_cp(4,js)

        if (.not. TE_der_actual .and. .not. TE_der_norm) then
            call compute_te_angle(u_max,dy_dx_te)
            dy_dx_te    = -2.0 * t_max * dy_dx_te
        else if (TE_der_actual .and. .not. TE_der_norm) then
            dy_dx_te    = thk_cp(5,js)
        else if (.not. TE_der_actual .and. TE_der_norm) then
            dy_dx_te    = -2.0 * t_max * thk_cp(5,js)
        end if

        ! Compute u_TE and u_center for the modified four digit NACA
        ! thickness distribution circular TE
        u_TE_quadratic_b    = -2.0*(1.0 - (t_TE*dy_dx_te))
        u_TE_quadratic_a    = 1.0
        u_TE_quadratic_c    = 1.0 - (t_TE**2) - (2.0*t_TE*dy_dx_te)
        u_TE                = (-u_TE_quadratic_b - sqrt((u_TE_quadratic_b**2) - (4.0*u_TE_quadratic_a*u_TE_quadratic_c)))/ &
                              (2.0*u_TE_quadratic_a)
        u_center            = u_TE + (t_TE*dy_dx_te)
        TE_radius           = sqrt((u_TE - u_center)**2 + (t_TE)**2)
        u_center            = 1.0 - TE_radius

        call modified_NACA_four_digit_thickness_coeffs_2(t_max,u_max,u_TE,t_TE,dy_dx_te,LE_round,a_NACA,d_NACA)

    end if



    !
    ! Generate clustering around LE and TE
    ! uniform_clustering, sine_clustering, exponential_clustering, &
    ! hyperbolic_tan_clustering and elliptical_clustering in funcNsubs.f90
    !
    if (clustering_switch == 0) then
        call uniform_clustering(np,u)
    else if (clustering_switch == 1) then
        call sine_clustering(np,clustering_parameter,u)
        u = u/u(np)
    else if (clustering_switch == 2) then
        call exponential_clustering(np,clustering_parameter,u)
    else if (clustering_switch == 3) then
        call hyperbolic_tan_clustering(np,clustering_parameter,u)
    else if (clustering_switch == 4) then
        if (thick_distr == 5) then
            np_cluster  = int(clustering_parameter)

            ! LE ellipse control points
            xcp_LE      = 0.5*(a_NACA(1)**2)
            ycp_LE      = (a_NACA(1)*sqrt(xcp_LE)) + (a_NACA(2)*xcp_LE) + (a_NACA(3)*(xcp_LE**2)) + (a_NACA(4)*(xcp_LE**3))
            cp_LE(:,1)  = [xcp_LE, xcp_LE , 0.0, xcp_LE]
            cp_LE(:,2)  = [ycp_LE, -ycp_LE, 0.0, 0.0   ]

            ! TE ellipse control points
            xcp_TE      = u_TE!1.0 - t_TE
            ycp_TE      = d_NACA(1) + (d_NACA(2)*(1.0 -  xcp_TE)) + (d_NACA(3)*((1.0 - xcp_TE)**2)) + &
                          (d_NACA(4)*((1.0 - xcp_TE)**3))
            cp_TE(:,1)  = [xcp_TE, xcp_TE , 1.0, xcp_TE]
            cp_TE(:,2)  = [ycp_TE, -ycp_TE, 0.0, 0.0   ]

            call elliptical_clustering(np,np_cluster,cp_LE,cp_TE,u)

        else
            error_msg   = 'Ellipse-hyperbolic clustering not available for current thickness distribution'
            dev_msg     = 'Check subroutine bladegen in bladegen.f90'
            call fatal_error(error_msg, dev_msg = dev_msg)
        end if
    end if



    !
    ! Set blade parameters related to LE/TE thickness and location of max.
    ! thickness
    !
    flin    = 0.10
    flex    = 0.05   
    fmxthk  = thkc  
    umxthk  = umxthk_all(js)
    lethk   = lethk_all(js)
    tethk   = tethk_all(js)
    sinls   = sinl
    sexts   = sext

    ! True for elliptical LE/TE
    ellip   = .true.
    rr1     = 2.5
    rr2     = 1.0

    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) print*, "lethk = ", lethk
    write(nopen,*) "lethk = ", lethk
    call close_log_file(nopen, file_open)



    !
    ! Set airfoil camber definition according to the curvature camber switch and 
    ! airfoil type
    !
    ! Case without curvature controlled camber
    !
    if (curv_camber == 0) then
        
        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) print*, 'Using specified airfoil definition...'
        write(nopen,*) 'Using specified airfoil definition...'
        call close_log_file(nopen, file_open)

        !
        ! Use specified airfoil definitions
        ! Circular airfoil
        !
        if (trim(airfoil) == 'crcle') then

            call circle(np, xb, yb)
            call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE, u_stack, v_stack)
            
            ! Add stagger and scale the airfoil using the non-dimensional actual chord input
            do i = 1, np
                xi         = xb(i)
                yi         = yb(i)
                xb(i)      = scaled(xi, chrdx)
                yb(i)      = scaled(yi, chrdx)
                xxa(i, js) = xb(i)
                yya(i, js) = yb(i)
            end do
            call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrdx, fext, js, pitch, mble, mbte, airfoil)

        !
        ! s809m airfoil
        !
        else if (trim(airfoil) == 's809m') then

            stagger = stagger*dtor
            chrd    = chrdx/abs(cos(stagger))
            call s809m(np, xb, yb)
            call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE, u_stack, v_stack)
            
            ! Add stagger and scale the airfoil using the non-dimensional actual chord input
            do i = 1, np   
                xi         = xb(i)
                yi         = yb(i)
                call rotate(xb(i), yb(i), xi, yi, stagger)
                xi         = xb(i)
                yi         = yb(i)
                xb(i)      = scaled(xi, chrdx)
                yb(i)      = scaled(yi, chrdx)
                xxa(i, js) = xb(i)
                yya(i, js) = yb(i)
            end do 
            call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrd, fext, js, pitch, mble, mbte, airfoil)

        !
        ! ClarkY airfoil
        !
        else if (trim(airfoil) == 'clark') then

            stagger = stagger*dtor
            chrd    = chrdx/abs(cos(stagger))
            call clarky(np, xb, yb)
            call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE, u_stack, v_stack)
            
            ! Add stagger and scale the airfoil using the non-dimensional actual chord input
            do i = 1, np   
                xi         = xb(i)
                yi         = yb(i)
                call rotate(xb(i), yb(i), xi, yi, stagger)
                xi         = xb(i)
                yi         = yb(i)
                xb(i)      = scaled(xi, chrdx)
                yb(i)      = scaled(yi, chrdx)
                xxa(i, js) = xb(i)
                yya(i, js) = yb(i)
            end do 
            call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrd, fext, js, pitch, mble, mbte, airfoil)

        !
        ! Counter rotating propeller ClarkY airfoil 
        !
        else if (trim(airfoil) == 'negclarkCR') then
            
            stagger = stagger*dtor
            chrd    = chrdx/abs(cos(stagger))
            call negclarky(np, xb, yb)
            call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE, u_stack, v_stack)
            
            ! Add stagger and scale the airfoil using the non-dimensional actual chord input
            do i = 1, np   
                xi         = xb(i)
                yi         = yb(i)
                call rotate(xb(i), yb(i), xi, yi, stagger)
                xi         = xb(i)
                yi         = yb(i)
                xb(i)      = scaled(xi, chrdx)
                yb(i)      = scaled(yi, chrdx)
                xxa(i, js) = xb(i)
                yya(i, js) = yb(i)
            end do 
            call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrd, fext, js, pitch, mble, mbte, airfoil)
       
        !
        ! NACA 4 series airfoil with circular TE
        !
        else if (len_trim(airfoil) == 4) then
            
            stagger = stagger*dtor
            chrd    = chrdx/abs(cos(stagger))
            call MakeFoil(naca, np, u, xbot, ybot, xtop, ytop)
            call circularTE(np, xbot, ybot, xtop, ytop)
            
            do i = 1, np
                xb(i) = xtop(np-i+1)
                yb(i) = ytop(np-i+1)
            end do
            do i = 2, np
                xb(i+np-1) = xbot(i)
                yb(i+np-1) = ybot(i)
            end do
            
            ! New number of points
            np = 2*np-1
            
            ! Write u,v coordinates of airfoil before stacking to a file
            ! file_write_1D in file_operations.f90
            if(js.eq.1)then
                file1 = 'uvnaca.dat'
                call file_write_1D(file1, xb, yb, np)
            end if  
            call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE, u_stack, v_stack)
            
            ! Add stagger and scale the airfoil
            do i = 1, np
                xi         = xb(i)
                yi         = yb(i)
                call rotate(xb(i), yb(i), xi, yi, stagger)
                xi         = xb(i)
                yi         = yb(i)

                ! Use the non-dimensional actual chord input
                if (chord_switch == 1) then
                    xb(i)  = scaled(xi, chrdx)
                    yb(i)  = scaled(yi, chrdx)

                ! Use internally calculated chord
                else
                    xb(i)  = scaled(xi, chrd)
                    yb(i)  = scaled(yi, chrd)
                end if
                xxa(i, js) = xb(i)
                yya(i, js) = yb(i)
            end do
            call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrd, fext, js, pitch, mble, mbte, airfoil)

        !
        ! Default airfoil 'sect1' which is mixed camber
        !
        else if(trim(airfoil).eq.'sect1')then
            
            call log_file_exists(log_file, nopen, file_open)
            if (.not. isquiet) then
                print*, 'Using the default airfoil definition'
                write(*, *)
            end if
            write(nopen,*) 'Using the default airfoil definition'
            write(nopen,*) ''
            
            ainl = atan(sinl)
            aext = atan(sext)
            sang = 0.5*(ainl+aext)
            ainl = ainl-sang
            aext = aext-sang
            sinl = tan(ainl)
            sext = tan(aext)
            chrd = chrdx/abs(cos(sang))
            
            if (.not. isquiet) print*, 'sinl sext sang', sinl, sext, sang
            write(nopen,*) 'sinl sext sang ', sinl, sext, sang
            call close_log_file(nopen, file_open)
            
            do i = 1, np
                ui        = u(i)    
                call cambmix(ui, cam, cam_u, sinl, sext, flin, flex)
                camber(i) = cam
                slope(i)  = cam_u
            end do

        !
        ! Airfoil data from user defined file
        !
        else

            stagger = stagger*dtor
            chrd    = chrdx/abs(cos(stagger))
            call datafile(airfoil, np, xb, yb)

            ! Write airfoil u,v data before stacking to a file
            if(js == 1)then
                file1 = 'uvairfoil.dat'
                call file_write_1D(file1, xb, yb, np)
            end if
            call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE, u_stack, v_stack)

            ! Add stagger and scale the airfoil using the non-dimensional actual chord input
            do i = 1, np
                xi         = xb(i)
                yi         = yb(i)
                call rotate(xb(i), yb(i), xi, yi, stagger)
                xi         = xb(i)
                yi         = yb(i)
                xb(i)      = scaled(xi, chrdx)
                yb(i)      = scaled(yi, chrdx)
                xxa(i, js) = xb(i)
                yya(i, js) = yb(i)
            end do
            call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrd, fext, js, pitch, mble, mbte, airfoil)

        end if  ! trim(airfoil)

    !
    ! Using curvature control for camber definition
    !
    else if (curv_camber == 1) then 

        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) write (*, '(/, A)') 'Using curvature control for camber definition...'
        write (nopen, '(/, A)') 'Using curvature control for camber definition...'
        call close_log_file(nopen, file_open)
        
        ainl = atan(sinl)
        aext = atan(sext)
        
        ! Reading the section control points
        ncp = ncp_curv(js)
        if (allocated(xcp_curv)) deallocate(xcp_curv)
        if (allocated(ycp_curv)) deallocate(ycp_curv)
        allocate(xcp_curv(ncp),ycp_curv(ncp))
        do i = 1, ncp
            xcp_curv(i) = curv_cp(i, 2*js-1)
            ycp_curv(i) = curv_cp(i, 2*js)
        end do

        if (allocated(init_angles   )) deallocate(init_angles   )
        if (allocated(init_cambers  )) deallocate(init_cambers  )
        if (allocated(x_spl_end_curv)) deallocate(x_spl_end_curv)
        allocate(init_angles(ncp - 2),init_cambers(ncp - 2),x_spl_end_curv(ncp - 2))
        
        ! Get camber line from the spline curvature:
        ! TODO: Why aren't optional arguments working?
        if (thick_distr == 5) then
            call camline(casename, isdev, ncp, np, xcp_curv, ycp_curv, u, ainl,    &
                         aext, chrdx, wing_flag, sang, chrd, init_angles, init_cambers, x_spl_end_curv, &
                         splinedata, u_max, cam_umax, slope_umax, dcam_dxcp, dcam_dycp, dcam_u,         &
                         dslope_dxcp, dslope_dycp, dslope_u, dsang_dcurv, dchrd_dcurv, dsinl_dxcp,      &
                         dsinl_dycp, dsang_dx_inbeta, dsang_dy_inbeta, dchrd_dx_inbeta, dchrd_dy_inbeta,&
                         dslope_dx_inbeta, dslope_dy_inbeta, dcam_dx_inbeta, dcam_dy_inbeta, dsext_dxcp,&
                         dsext_dycp, dslope_doutbeta, dcam_doutbeta, dsang_doutbeta, dchrd_doutbeta,    &
                         chrdx_ders, dchrd_dcm)
        else
            call camline(casename, isdev, ncp, np, xcp_curv, ycp_curv, u, ainl, aext,    &
                         chrdx, wing_flag, sang, chrd, init_angles, init_cambers, x_spl_end_curv, splinedata, &
                         -1.0, 0.0, 0.0, dcam_dxcp, dcam_dycp, dcam_u, dslope_dxcp, dslope_dycp, dslope_u,    &
                         dsang_dcurv, dchrd_dcurv, dsinl_dxcp, dsinl_dycp, dsang_dx_inbeta, dsang_dy_inbeta,  &
                         dchrd_dx_inbeta, dchrd_dy_inbeta, dslope_dx_inbeta, dslope_dy_inbeta, dcam_dx_inbeta,&
                         dcam_dy_inbeta, dsext_dxcp, dsext_dycp, dslope_doutbeta, dcam_doutbeta, dsang_doutbeta, &
                         dchrd_doutbeta, chrdx_ders, dchrd_dcm)
        end if
        camber = splinedata(2, :)
        slope  = splinedata(3, :)

        ! If command line argument dev is used write a file with u, v, slope and curvature
        ! data
        if (isdev) call write_curvature_data(spline_data, np_side, splinedata, sec, casename)

    end if  ! curv_camber


    
    !
    ! Thickness and LE definition options
    ! Thickness will be defined only for the default section
    !
    if (trim(airfoil) == 'sect1') then

        ! Generate a spline multiplier thickness
        if (thick == 1) then 
            
            ncp = ncp_thk(js)

            ! Read thickness multiplier control points
            if (allocated(xcp_thk)) deallocate(xcp_thk)
            if (allocated(ycp_thk)) deallocate(ycp_thk)
            allocate(xcp_thk(ncp),ycp_thk(ncp)) 
            do i = 1, ncp
                xcp_thk(i) = thk_cp(i, 2*js-1)
                ycp_thk(i) = thk_cp(i, 2*js)
            end do

            ! Fourth order thickness multiplier
            call bspline_y_of_x(ncp, 4, xcp_thk, ycp_thk, np, u, splthick)
            do i = 1, np
                splinedata(6, i) = splthick(i)
            end do

            ! Writing a file in developer mode for debugging.
            if (isdev) then

                ! write_thick_multi_cp in file_operations               
                call write_thick_multi_cp(sec,casename,ncp,xcp_thk,ycp_thk) 

            end if 

        ! Thickness multiplier is not required
        else if (thick == 0 .and. thick_distr == 0) then
            splthick = 0
        end if 


        !
        ! Generate thickness for the default airfoil section
        !
        ! Modified four-digit NACA thickness
        !
        if (thick_distr == 5) then

            ! Allocate necessary arrays
            if (allocated(thickness_data)) deallocate(thickness_data)
            allocate(thickness_data(np,3))

            !if (allocated(thk_der)) deallocate(thk_der)
            !allocate(thk_der(np))

            call log_file_exists(log_file, nopen, file_open)
            if (.not. isquiet) then
                print *, ''
                print *, 'Using modified NACA four digit thickness distribution'
            end if
            write(nopen,*) ''
            write(nopen,*) 'Using modified NACA four digit thickness distribution'
             
            ! Print input values to screen and write to log file
            if (.not. isquiet) print *, 'Maximum thickness for the blade section = ', t_max
            write(nopen,*) 'Maximum thickness for the blade section = ', t_max
            if (.not. isquiet) print *, 'Chordwise location for the maximum thickness = ', u_max
            write(nopen,*) 'Chordwise location for the maximum thickness = ', u_max
            if (.not. isquiet) print *, 'Thickness at TE = ', t_TE
            write(nopen,*) 'Thickness at TE = ', t_TE
            if (.not. isquiet) print *, 'Leading edge radius = ', LE_round
            write(nopen,*) 'Leading edge radius = ', LE_round

            !
            ! Compute TE angle value for u_max
            ! Display on screen and write to log file
            !
            if (.not. TE_der_actual) then
                if (.not. isquiet) print *, 'TE derivative for maximum thickness chordwise location = ', dy_dx_te
                write(nopen,*) 'TE derivative for maximum thickness chordwise location = ', dy_dx_te
            else
                if (.not. isquiet) print *, 'Using TE derivative defined in auxiliary input file as = ', dy_dx_te
                write(nopen,*) 'Using TE derivative defined in auxiliary input file as = ', dy_dx_te
            end if

            !
            ! Apply modified NACA four digit thickness
            !
            call modified_NACA_four_digit_thickness_all(js,np,u,u_max,u_TE,u_center,t_max,t_TE,a_NACA,d_NACA, &
                                                        thickness_data,monotonic,write_to_file)
            thickness       = thickness_data(:,1)

            ! Print coefficients to screen and write to log file
            if (.not. isquiet) print *, 'Modified NACA thickness coefficients (u < u_max) = ', a_NACA
            write(nopen,*) 'Modified NACA thickness coefficients (u < u_max) = ', a_NACA
            if (.not. isquiet) print *, 'Modified NACA thickness coefficients (u > u_max) = ', d_NACA
            write(nopen,*) 'Modified NACA thickness coefficients (u > u_max) = ', d_NACA
            if (.not. isquiet) print *, ''
            write(nopen,*) ''

            ! Write thickness data to sectionwise files
            ! write_NACA_thickness in file_operations
            if (.not. isquiet) then
                print *, 'Writing thickness data to file'
                print *, ''
            end if
            write(nopen,*) 'Writing thickness data to file'
            write(nopen,*) ''
            call write_NACA_thickness(sec,casename,np,u,thickness_data)

            call close_log_file(nopen, file_open)


            !
            ! Compute the sensitivity of the NACA thickness distribution
            ! wrt to its thickness parameters
            !
            call NACA_thickness_derivatives (js, np, np_cluster, u, thickness, thk_cp(1:5, js), u_TE, u_center, TE_radius, &
                                             a_NACA, d_NACA, u_ders, NACA_ders)
            if (clustering_switch == 4) &
                 call meanline_thickness_derivatives (dslope_u, dcam_u, u_ders, slope_ders, cam_ders)

        !
        ! Spline thickness distribution with LE control
        !
        else if(thick_distr.ne.0) then
            
            call log_file_exists(log_file, nopen, file_open)
            if (.not. isquiet) then
                print*, 'np = ', np
                print*, ' the thickness segment points'
            end if
            write(nopen,*) 'mp = ', np
            write(nopen,*) ' the thickness segment points'
            call close_log_file(nopen, file_open)
            
            ! splinethick in splinethick.f90
            call splinethick(js, np, np_side, i_le, i_te, thickness, u, lethk, umxthk, fmxthk, tethk,    &
                             uin_le, thick_distr, ucp_top, vcp_top, ucp_bot, vcp_bot, casename, develop, &
                             isdev, spline_data, splinedata)
            thickness = thickness*(1 + splthick)
           
        !
        ! Wennerstrom thickness distribution
        ! 
        else if (thick_distr == 0)then
        
            spanwise_thk(js)     = fmxthk    
            do i = 1, np
                ui           = u(i)

                if (thick == 0) then
                    thkmultip    = thk_tm_c_spl(js)
                else
                    thkmultip    = splthick(i)
                end if

                ! thickellip in airfoiltypes.f90 
                call thickellip(i, ui, thk, lethk, tethk, fmxthk, umxthk, rr1, rr2, thkmultip, u_le, uin_le, i_le, oo, i_te)
                thickness(i) = thk
            end do

            call log_file_exists(log_file, nopen, file_open)

            ! Write thickness data to sectionwise files
            ! write_Wennerstrom_thickness in file_operations
            if (.not. isquiet) then
                print *, 'Writing thickness data to file'
                print *, ''
            end if
            write(nopen,*) 'Writing thickness data to file'
            write(nopen,*) ''
            call write_Wennerstrom_thickness(sec,casename,np,u,thickness)

            ! Write spanwise thickness variation to file
            ! write_Wennerstrom_thk_variation in file_operations.f90
            if (js == 21) &
                call write_Wennerstrom_thickness_variation(casename,nspn,umxthk_all,spanwise_thk)

            call close_log_file(nopen, file_open)

        end if  ! thick_distr



        ! 
        ! Print LE and TE indices to screen and write to main log file
        !
        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) then
            print*, 'i_le = ', i_le
            print*, 'i_te = ', i_te
        end if
        write(nopen,*) 'i_le = ', i_le
        write(nopen,*) 'i_te = ', i_te
        call close_log_file(nopen, file_open)
        


        ! 
        ! Creating the top and bottom curve coordinates
        !
        angle = atan(slope)
        xbot  = u   + thickness*sin(angle)
        ybot  = camber - thickness*cos(angle)
        xtop  = u   - thickness*sin(angle)
        ytop  = camber + thickness*cos(angle)

        ! Airfoil coordinates associated with maximum thickness
        ! location for NACA thickness
        if (thick_distr == 5) then
            mt_umax(1) = u_max    + (t_max * sin(atan(slope_umax)))
            mt_umax(2) = cam_umax - (t_max * cos(atan(slope_umax)))
            mt_umax(3) = u_max    - (t_max * sin(atan(slope_umax)))
            mt_umax(4) = cam_umax + (t_max * cos(atan(slope_umax)))
        end if


        !
        ! Compute sensitivities of the (u, v) blade sections
        ! wrt the mean-line second derivative and the thickness
        ! distribution parameter control pts.
        !
        if (trim(airfoil) == 'sect1') then
            if (curv_camber == 1) then

                call uv_curv_derivatives (thickness, slope, dslope_dxcp, dslope_dycp, dcam_dxcp, dcam_dycp, &
                                          dxbot_dcurv, dxtop_dcurv, dybot_dcurv, dytop_dcurv)
                call uv_thk_derivatives (np, u_ders, NACA_ders, slope_ders, cam_ders, thickness, &
                                         angle, slope, dxbot_dt, dybot_dt, dxtop_dt, dytop_dt)
                call uv_inbeta_derivatives (thickness, slope, dslope_dx_inbeta, dslope_dy_inbeta, dcam_dx_inbeta, &
                                            dcam_dy_inbeta, dxbot_dinbeta, dybot_dinbeta, dxtop_dinbeta,          &
                                            dytop_dinbeta)
                call uv_outbeta_derivatives (thickness, slope, dslope_doutbeta, dcam_doutbeta, &
                                             dxbot_doutbeta, dybot_doutbeta, dxtop_doutbeta, dytop_doutbeta)

            end if  ! curv_camber
        end if  ! trim(airfoil)


        ! Write the top and bottom curve coordinates to a file in developer mode
        if (isdev) then

            file7 = 'topcurve.'//trim(fext)
            call file_write_1D(file7, xtop, ytop, np)
            
            file7 = 'botcurve.'//trim(fext)
            call file_write_1D(file7, xbot, ybot, np)

        end if


        
        !
        !
        !
        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) print*, np
        write(nopen,*) np
        call close_log_file(nopen, file_open)
        
        ! Populate blade coordinate arrays
        do i = 1, np
            xb(i) = xtop(np - i + 1)
            yb(i) = ytop(np - i + 1)
        end do
        do i = 2, np
            xb(i + np - 1) = xbot(i)
            yb(i + np - 1) = ybot(i)
        end do


        ! Combine top and bottom surface derivatives
        if (trim(airfoil) == 'sect1') then
            if (curv_camber == 1) then

                ! Mean-line 2nd derivative control points
                call combine_uv_derivatives (dxbot_dcurv, dybot_dcurv, dxtop_dcurv, dytop_dcurv, du_dcurv, dv_dcurv)

                ! NACA thickness control points
                call combine_uv_derivatives (dxbot_dt, dybot_dt, dxtop_dt, dytop_dt, du_dt, dv_dt)

                ! in_beta* control points
                call combine_uv_derivatives (dxbot_dinbeta, dybot_dinbeta, dxtop_dinbeta, dytop_dinbeta, &
                                             du_dinbeta, dv_dinbeta)

                ! out_beta* control points
                call combine_uv_derivatives (dxbot_doutbeta, dybot_doutbeta, dxtop_doutbeta, dytop_doutbeta, &
                                             du_doutbeta, dv_doutbeta)

            end if
        end if


        ! New number of points for the entire blade section
        np = 2*np-1


        !
        ! Stacking of airfoils
        !
        call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE, u_stack, v_stack)

        ! Stacked (u,v) coordinates associated with maximum
        ! thickness location for NACA thickness
        if (thick_distr == 5) then
            mt_umax(1) = mt_umax(1) - u_stack
            mt_umax(2) = mt_umax(2) - v_stack
            mt_umax(3) = mt_umax(3) - u_stack
            mt_umax(4) = mt_umax(4) - v_stack
        end if

        ! Write u,v section coordinates to a file in developer mode
        if(isdev) then
            file7 = 'uvblade.'//trim(fext)
            call file_write_1D(file7, xb, yb, np)
        end if

        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) print*, 'chrd bladegen: ', chrd
        write(nopen,*) 'chrd bladegen: ', chrd
        call close_log_file(nopen, file_open)


        !
        ! Write stagger angles to a file
        ! TODO: Move to file_operations
        !
        stagger_file = 'stagger_angles.dat'
        if (isdev) then
            inquire(file = stagger_file, exist=exist)
            if (exist) then
                if (js == 1) then
                    open(903, file = stagger_file, form = 'formatted', status = 'old', action = 'write')
                else
                    open(903, file = stagger_file, form = 'formatted', status = 'old', position = 'append', action = 'write')
                end if
            else
                if (js == 1) then
                    open(903, file = stagger_file, form = 'formatted', status = 'new', action = 'write')
                else
                    open(903, file = stagger_file, form = 'formatted', status = 'old', position = 'append', action = 'write')
                end if
            end if
        end if
        write(903,'(F20.16)') sang
        close(903)


        ! Add stagger and scale the airfoil
        do i = 1, np
            
            xi = xb(i)
            yi = yb(i)
            call rotate(xb(i), yb(i), xi, yi, sang)
            xi = xb(i)
            yi = yb(i)

            ! Using non-dimensional actual chord input
            if(chord_switch.eq.1)then

                xb(i) = scaled(xi, chrdx)
                yb(i) = scaled(yi, chrdx)

            ! Using internally calculated chord
            else

                xb(i) = scaled(xi, chrd)
                yb(i) = scaled(yi, chrd)

            end if

            yb(i) = yb(i) + theta_offset*dtor
            xxa(i, js) = xb(i)
            yya(i, js) = yb(i)

        end do
        call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrd, fext, js, pitch, mble, mbte, airfoil)



        ! Compute (m',theta) coordinates of point associated
        ! with maximum thickness location for NACA thickness
        if (thick_distr == 5) then
            call rotate_point(mt_umax(1), mt_umax(2), sang)
            call rotate_point(mt_umax(3), mt_umax(4), sang)

            if (chord_switch == 1) then

                call scale_point(mt_umax(1), mt_umax(2), chrdx)
                call scale_point(mt_umax(3), mt_umax(4), chrdx)
            else
                call scale_point(mt_umax(1), mt_umax(2), chrd)
                call scale_point(mt_umax(3), mt_umax(4), chrd)
            end if

            mt_umax(2) = mt_umax(2) + (theta_offset * dtor)
            mt_umax(4) = mt_umax(4) + (theta_offset * dtor)

        end if

    end if  ! if (trim(airfoil) == 'sect1')

    m_prime(1:np) = xb
    theta(1:np)   = yb



    !
    ! Derivatives of (m',theta) blade section
    !
    if (trim(airfoil) == 'sect1') then
        if (curv_camber == 1) then

            ! NACA thickness distribution control points
            call mprime_theta_thk_derivatives (du_dt, dv_dt, chrdx, chrd, sang, dmprime_dt, dtheta_dt)

            ! Mean-line second derivative control points
            call mprime_theta_curv_derivatives (du_dcurv, dv_dcurv, xb, yb, chrdx, chrd, sang, dchrd_dcurv, &
                                                dsang_dcurv, dmprime_dcurv, dtheta_dcurv)

            ! in_beta* control points
            call mprime_theta_inbeta_derivatives (xb, yb, chrdx, chrd, sang, dchrd_dx_inbeta, dchrd_dy_inbeta, &
                                                  dsang_dx_inbeta, dsang_dy_inbeta, du_dinbeta, dv_dinbeta,    &
                                                  dmprime_dinbeta, dtheta_dinbeta)

            ! out_beta* control points
            call mprime_theta_outbeta_ders (xb, yb, chrdx, chrd, sang, du_doutbeta, dv_doutbeta, &
                                            dsang_doutbeta, dchrd_doutbeta, dmprime_doutbeta, dtheta_doutbeta)

            ! chord_multiplier control points
            call mprime_theta_cm_ders (xb, yb, chrdx, chrd, chrdx_ders, dchrd_dcm, dmprime_dcm, dtheta_dcm)

        end if
    end if

    ! Show warning if NACA thickness or mean-line second derivative
    ! control points are not being used
    if (trim(airfoil) /= 'sect1' .or. curv_camber /= 1) then

        msg_1   = "(m',theta) derivatives wrt curvature and thickness control points set to 0."
        msg_2   = "Either the mean-line second derivative or the NACA thickness options are not being used &
                  &in the spancontrolinputs file."
        call warning (warning_msg = msg_1, warning_msg_1 = msg_2)

    end if



    !
    ! Store bladedata for each section
    !
    bladedata(4, js)  = ainl/dtor    
    bladedata(5, js)  = aext/dtor
    bladedata(9, js)  = area*(chrd*scf)**2   
    bladedata(10, js) = lethk*chrd*scf    
    bladedata(11, js) = tethk*chrd*scf
    bladedata(13, js) = pitch              
    radius_pitch      = pitch*sec_radius(js, 1)  
    if (.not. isquiet) print*, 'pitch ', pitch 
    call log_file_exists(log_file, nopen, file_open)
    write(nopen,*) 'pitch ', pitch
    call close_log_file(nopen, file_open)


    
    !
    ! Calculation of the 2D throat
    ! Rotate, scale and translate the camber line
    !
    if (trim(airfoil) == 'sect1') then
        
        u_translation = u(1)
        camber_trans = camber(1)

        if (chord_switch.eq.1) then
        scaling = chrdx
        else
        scaling = chrd
        end if

        do i = 1, (np + 1)/2
            call rotate2 (u_rot, camber_rot, u(i), camber(i), sang)
            u(i) = scaled (u_rot, scaling)
            camber(i) = scaled (camber_rot, scaling)
            u(i) = u(i) + (xb((np+1)/2)-u_translation)
            camber(i) = camber(i) + (yb((np+1)/2)-camber_trans)
        end do

    else

        ! Average top and bottom curves to obtain the camber
        call averaged_camber(xb, yb, np, u, camber, angle, sinl)

    end if


    ! Compute (m',theta) extended meanlines
    ! get_extended_meanlines_2D in funcNsubs.f90
    call get_extended_meanlines_2D (n_ext, np_side, u, camber, m_mean, th_mean)

   
    ! throat_calc_pitch_line in funcNsubs.f90
    call throat_calc_pitch_line(xb, yb, np, camber, angle, sang, u, pi, pitch, intersec_coord(1:4, js), &
                                intersec_coord(5:8, js), intersec_coord(9:12, js), min_throat_2D,       &
                                throat_index(js), n_normal_distance, casename, js, nsl, develop)



    !
    ! 2D blade grid generation
    ! TODO: Will be separated from the main code
    !
    !if (isxygrid) then
    !
    !    call log_file_exists(log_file, nopen, file_open)
    !    print*, 'i_slope in bladgen: ', i_slope
    !    write(nopen,*) 'i_slope in bladegen: ', i_slope
    !    call close_log_file(nopen, file_open)
    !
    !    ! No 2D grid files for wind turbines and cases with less than 5 blades
    !    if ((nbls >= 5) .and. (i_slope == 0)) then 
    !        jcellblade = jcellblade_all(js)
    !        etawidth   = etawidth_all(js)
    !        BGgrid     = BGgrid_all(js)
    !
    !        call bladegrid2D(xb, yb, np, nbls, chrdx, thkc, fext, LE, le_pos, thick_distr, &
    !                         casename, msle, mste, mble, mbte, js, nspn, np_side,          &
    !                         curv_camber, stingl, jcellblade, etawidth, BGgrid, develop, isdev)
    !    end if
    !
    !end if ! isxygrid



    !
    ! Calculation of geometric Zweifel number assuming inlet and exit velocities are equal
    !
    Zweifel(js)       = 2*pitch/chrdx*((cos(aext))**2)*(tan(ainl) + tan(aext))
    bladedata(14, js) = Zweifel(js)

    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) print*, '******************************************'
    write(nopen,*) '******************************************'
    call close_log_file(nopen, file_open)
    return


end subroutine bladegen





















