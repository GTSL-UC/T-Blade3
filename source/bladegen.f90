subroutine bladegen(nspn,thkc,mr1,sinl,sext,chrdx,js,fext,xcen,ycen,airfoil, stagger,stack,chord_switch,  &
                    stk_u,stk_v,xb_stk,yb_stk,stack_switch, clustering_switch, clustering_parameter,nsl,  &
                    nbls,curv_camber,thick,LE,np, ncp_curv,ncp_thk, curv_cp,thk_cp, wing_flag, lethk_all, &
                    tethk_all,s_all,ee_all,thick_distr, umxthk_all,C_le_x_top_all,C_le_x_bot_all,         &
                    C_le_y_top_all,C_le_y_bot_all,LE_vertex_ang_all,LE_vertex_dis_all,sting_l_all,        &
                    sting_h_all,LEdegree,no_LE_segments,sec_radius,bladedata,amount_data,scf,             &
                    intersec_coord,throat_index, n_normal_distance,casename,develop,mble,mbte,msle,       &
                    mste,i_slope,jcellblade_all, etawidth_all,BGgrid_all,thk_tm_c_spl, theta_offset,      &
                    TE_derivative,from_gridgen,np_in,u_in,v_in,uv,uv_top,uv_bot,m_prime,theta)

    use file_operations
    use errors
    implicit none

    integer,                                intent(in)          :: nspn, js, stack, stack_switch, chord_switch, clustering_switch, nsl, nbls, curv_camber,         &
                                                                   thick, LE, ncp_curv(nsl), ncp_thk(nsl), wing_flag, thick_distr, LEdegree, no_LE_segments,       &
                                                                   amount_data, throat_index(nspn), n_normal_distance, i_slope, np_in
    integer,                                intent(inout)       :: np
    real,                                   intent(in)          :: thkc, mr1, chrdx, xcen, ycen, stk_u(1), stk_v(1), xb_stk, yb_stk, clustering_parameter,         &
                                                                   curv_cp(20,2*nsl), thk_cp(20,2*nsl), lethk_all(nsl), tethk_all(nsl), s_all(nsl), ee_all(nsl),   &
                                                                   umxthk_all(nsl), C_le_x_top_all(nsl), C_le_x_bot_all(nsl), C_le_y_top_all(nsl),                 &
                                                                   C_le_y_bot_all(nsl), LE_vertex_ang_all(nsl), LE_vertex_dis_all(nsl), sting_l_all(nsl),          &
                                                                   sting_h_all(nsl), sec_radius(nsl,2), scf, intersec_coord(12,nsl), mble, mbte, msle, mste,       &
                                                                   jcellblade_all(nspn), etawidth_all(nspn), BGgrid_all(nspn), thk_tm_c_spl(nsl), theta_offset
    real,                                   intent(inout)       :: sinl, sext, stagger, bladedata(amount_data,nsl), u_in(np_in), v_in(np_in), &
                                                                   uv(500,2), uv_top(500,2), uv_bot(500,2), m_prime(500), theta(500)
    character(*),                           intent(in)          :: fext, airfoil, casename, develop
    logical                                                     :: TE_derivative, from_gridgen

    ! Local variables
    integer                                                     :: np_side, i, k, naca, np_cluster, ncp, le_pos, i_le, i_te, oo, nopen
    integer,    parameter                                       :: nspan = 200, nx = 500, nxx = 1000, nrow = 1, nax = 50, nb = 300, spline_data = 6, interval = 6, &
                                                                   pt2 = 1, TE_del = 0 
    real                                                        :: chrd, pitch, radius_pitch, scaling, lethk, thkmultip, aext, ainl, area, cam, cam_u, dtor, pi,   &
                                                                   flex, flin, fmxthk, rr1, rr2, sang, sexts, sinls, tethk, thk, ui, umxthk, xi, yi, xxa, yya,     &
                                                                   u_le, uin_le, camber_le(interval+1), camber_ang(interval+1), Zweifel(nsl), ucp_top(11),         &
                                                                   vcp_top(11), ucp_bot(11), vcp_bot(11), xcp_LE, ycp_LE,  xcp_TE, ycp_TE, cp_LE(4,2), cp_TE(4,2), &
                                                                   a_NACA(4), d_NACA(4), t_max, u_max, t_TE, dy_dx_TE, LE_round, min_throat_2D, u_translation,     &
                                                                   camber_trans, scaled, u_rot, camber_rot
    real,                   allocatable                         :: xtop_refine(:), ytop_refine(:), xbot_refine(:), ybot_refine(:), init_angles(:), init_cambers(:),&
                                                                   x_spl_end_curv(:), cam_refine(:), u_refine(:), xcp_curv(:), ycp_curv(:), x_le_spl(:),           &
                                                                   y_le_spl(:), xcp_thk(:), ycp_thk(:), ueq(:), xmean(:), ymean(:), xtop(:), ytop(:), xbot(:),     &
                                                                   ybot(:), u(:), xb(:), yb(:), u_new(:), splthick(:), thickness(:), angle(:), camber(:), slope(:),&
                                                                   thickness_data(:,:), splinedata(:,:)
    character(80)                                               :: file1, file7
    character(20)                                               :: sec
    character(:),           allocatable                         :: log_file, error_msg, dev_msg, stagger_file
    logical                                                     :: ellip, file_open, isdev, isquiet, monotonic = .true., write_to_file = .true., exist
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
    ! Number of coordinates for the airfoil
    !
    np      = 121
    np_side = np



    !
    ! Allocation of arrays
    !
    if (allocated(splinedata    )) deallocate(splinedata    )
    allocate(splinedata(spline_data,np_side))

    if (allocated(xtop          )) deallocate(xtop          )
    if (allocated(ytop          )) deallocate(ytop          )
    if (allocated(xbot          )) deallocate(xbot          )
    if (allocated(ybot          )) deallocate(ybot          )
    allocate(xtop(np),ytop(np),xbot(np),ybot(np))

    if (allocated(u             )) deallocate(u             )
    if (allocated(u_new         )) deallocate(u_new         )
    allocate(u(np),u_new(np)) 
                                                      
    if (allocated(splthick      )) deallocate(splthick      )
    if (allocated(thickness     )) deallocate(thickness     )
    if (allocated(thickness_data)) deallocate(thickness_data)
    allocate(splthick(np),thickness(np), &
             thickness_data(np,12))

    if (allocated(angle         )) deallocate(angle         )
    if (allocated(camber        )) deallocate(camber        )
    if (allocated(slope         )) deallocate(slope         )
    allocate(angle(np),camber(np),slope(np))

    if (allocated(xb            )) deallocate(xb            )
    if (allocated(yb            )) deallocate(yb            )
    allocate(xb(2*np - 1),yb(2*np - 1))

    if (allocated(ueq           )) deallocate(ueq           )
    if (allocated(xmean         )) deallocate(xmean         )
    if (allocated(ymean         )) deallocate(ymean         )
    allocate(ueq(np_side),xmean(np_side),ymean(np_side))



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

        if (.not. TE_derivative) then
            call compute_te_angle(u_max,dy_dx_te)
            dy_dx_te    = -2.0*t_max*dy_dx_te
        else
            dy_dx_te    = thk_cp(5,js)
        end if
        
        call modified_NACA_four_digit_thickness_coeffs_2(t_max,u_max,t_TE,dy_dx_TE,LE_round,a_NACA,d_NACA)

    end if



    !
    ! Generate clustering around LE and TE
    ! uniform_clustering, sine_clustering, exponential_clustering, &
    ! hyperbolic_tan_clustering and elliptical_clustering in funcNsubs.f90
    !
    if (clustering_switch == 0) then
        call uniform_clustering(np,u)
    else if (clustering_switch == 1) then
        call sine_clustering(np,u,clustering_parameter)
        u = u/u(np)
    else if (clustering_switch == 2) then
        call exponential_clustering(np,u,clustering_parameter)
    else if (clustering_switch == 3) then
        call hyperbolic_tan_clustering(np,u,clustering_parameter)
    else if (clustering_switch == 4) then
        if (thick_distr == 5) then
            np_cluster  = int(clustering_parameter)

            ! LE ellipse control points
            xcp_LE      = 0.5*(a_NACA(1)**2)
            ycp_LE      = (a_NACA(1)*sqrt(xcp_LE)) + (a_NACA(2)*xcp_LE) + (a_NACA(3)*(xcp_LE**2)) + (a_NACA(4)*(xcp_LE**3))
            cp_LE(:,1)  = [xcp_LE, xcp_LE , 0.0, xcp_LE]
            cp_LE(:,2)  = [ycp_LE, -ycp_LE, 0.0, 0.0   ]

            ! TE ellipse control points
            xcp_TE      = 1.0 - t_TE
            ycp_TE      = d_NACA(1) + (d_NACA(2)*(1.0 -  xcp_TE)) + (d_NACA(3)*((1.0 - xcp_TE)**2)) + (d_NACA(4)*((1.0 - xcp_TE)**3))
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
    ! Set blade parameters related to LE/TE thickness and location of max. thickness
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

            call circle(xb, yb, np)
            call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)
            
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
            call s809m(xb, yb, np)
            call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)
            
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
            call clarky(xb, yb, np)
            call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)
            
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
            call negclarky(xb, yb, np)
            call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)
            
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
            call MakeFoil(naca, xbot, ybot, xtop, ytop, np, u)
            call circularTE(xbot, ybot, xtop, ytop, np)
            
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
            call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)
            
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
            call datafile(airfoil, xb, yb, np)

            ! Write airfoil u,v data before stacking to a file
            if(js == 1)then
                file1 = 'uvairfoil.dat'
                call file_write_1D(file1, xb, yb, np)
            end if
            call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)

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
        call camline(casename, isdev, xcp_curv, ycp_curv, ncp, u, np, ainl, aext, chrdx, wing_flag, &
        sang, chrd, init_angles, init_cambers, x_spl_end_curv, splinedata)
        camber = splinedata(2, :)
        slope  = splinedata(3, :)

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
            call bspline_y_of_x(splthick, u, np, xcp_thk, ycp_thk, ncp, 4 )
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
            if (.not. TE_derivative) then
                if (.not. isquiet) print *, 'TE derivative for maximum thickness chordwise location = ', dy_dx_te
                write(nopen,*) 'TE derivative for maximum thickness chordwise location = ', dy_dx_te
            else
                if (.not. isquiet) print *, 'Using TE derivative defined in auxiliary input file as = ', dy_dx_te
                write(nopen,*) 'Using TE derivative defined in auxiliary input file as = ', dy_dx_te
            end if

            !
            ! Apply modified NACA four digit thickness
            !
            call modified_NACA_four_digit_thickness_all(js,np,u,u_max,t_max,t_TE,a_NACA,d_NACA,thickness_data, &
                                                        monotonic,write_to_file)
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
            call splinethick(thickness, u, np, lethk, umxthk, fmxthk, tethk, i_le, i_te, uin_le, thick_distr, &
                             ucp_top, vcp_top, ucp_bot, vcp_bot, casename, js, develop, isdev, np_side,       &
                             spline_data, splinedata)
            thickness = thickness*(1 + splthick)
           
        !
        ! Wennerstrom thickness distribution
        ! 
        else if (thick_distr == 0)then
            
            do i = 1, np
                ui           = u(i)

                if (thick == 0) then
                    thkmultip   = thk_tm_c_spl(js)
                else
                    thkmultip    = splthick(i)
                end if

                ! thickellip in airfoiltypes.f90 
                call thickellip(i, ui, thk, lethk, tethk, fmxthk, umxthk, rr1, rr2, thkmultip, u_le, uin_le, i_le, oo, i_te)
                thickness(i) = thk
            end do

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

        ! Write meanline (u,v) data file in developer mode
        if (isdev) call meanline_u_v_file(np,sec,u,camber,slope)
        
        ! Write the top and bottom curve coordinates to a file in developer mode
        if (isdev) then

            file7 = 'topcurve.'//trim(fext)
            call file_write_1D(file7, xtop, ytop, np)
            
            file7 = 'botcurve.'//trim(fext)
            call file_write_1D(file7, xbot, ybot, np)

        end if



        ! 
        ! Definition of the LE if the LE spline switch is on
        ! 
        le_pos = 1000 
        if (LE /= 0) then
            
            le_pos = i_le
            call log_file_exists(log_file, nopen, file_open)
            if (.not. isquiet) then
                print*, 'uin_le bladgen = ', uin_le
                print*, 'le_pos bladgen = ', le_pos
            end if
            write(nopen,*) 'uin_le bladegen = ', uin_le
            write(nopen,*) 'le_pos_bladegen = ', le_pos
            
            ! Allocate arrays
            if (allocated(x_le_spl   )) deallocate(x_le_spl   )
            if (allocated(y_le_spl   )) deallocate(y_le_spl   )
            allocate(x_le_spl(2*le_pos - 1), y_le_spl(2*le_pos - 1))

            ! Arrays required for spline leading edge
            if (allocated(xtop_refine)) deallocate(xtop_refine)
            if (allocated(ytop_refine)) deallocate(ytop_refine)
            if (allocated(xbot_refine)) deallocate(xbot_refine)
            if (allocated(ybot_refine)) deallocate(ybot_refine)
            if (allocated(cam_refine )) deallocate(cam_refine )
            if (allocated(u_refine   )) deallocate(u_refine   )
            allocate(xtop_refine(interval + 1), ytop_refine(interval + 1), &
                     xbot_refine(interval + 1), ybot_refine(interval + 1), &
                     cam_refine(interval + 1), u_refine(interval + 1))

            if (.not. isquiet) print*, 'KB reached fini_diff_refine in bladegen'
            write(nopen,*) 'KB reached fini_diff_refine in bladegen'
            call close_log_file(nopen ,file_open)

            ! fini_diff_refine in lespline.f90
            call fini_diff_refine(curv_camber, thick, thick_distr, xcp_curv, ycp_curv, ncp_curv(js), xcp_thk,     &
                                  ycp_thk, ncp_thk(js), u(le_pos), interval, ucp_top, vcp_top, ucp_bot, vcp_bot,  &
                                  sinl, sext, flin, flex, fmxthk, umxthk, lethk, tethk, rr1, rr2, x_spl_end_curv, &
                                  init_angles, init_cambers, cam_refine, u_refine, xtop_refine, ytop_refine,      &
                                  xbot_refine, ybot_refine)

            do k = 1, interval + 1
                camber_ang(k) = atan(cam_refine(1)/u_refine(1))
            end do
            camber_le = cam_refine
            
            ! 
            ! LE spline definition
            ! 
            if (LE == 1) then

                ncp = LEdegree + no_LE_segments
                call lespline(xtop_refine,ytop_refine,xbot_refine,ybot_refine,interval + 1,camber_ang,camber_le,uin_le, &
                               le_pos,pi,x_le_spl,y_le_spl,js,nsl,s_all,ee_all,C_le_x_top_all,C_le_x_bot_all,           &
                               C_le_y_top_all,C_le_y_bot_all,LE_vertex_ang_all,LE_vertex_dis_all,ncp,LEdegree,          &
                               no_LE_segments,casename,develop,isdev)

            !
            ! LE sting definition
            !
            else if (LE == 2) then
                
                ncp = LEdegree + 2
                call log_file_exists(log_file, nopen, file_open)
                if (.not. isquiet) print*, 'ncp_sting', ncp
                write(nopen,*) 'ncp_sting', ncp
                call close_log_file(nopen, file_open)
                call lesting(xtop_refine,ytop_refine,xbot_refine,ybot_refine,interval + 1,camber_ang,camber_le,uin_le,     &
                             le_pos,pi,x_le_spl,y_le_spl,js,nsl,s_all,ee_all,C_le_x_top_all,C_le_x_bot_all,C_le_y_top_all, &
                             C_le_y_bot_all,sang,LE_vertex_ang_all,LE_vertex_dis_all,ncp,LEdegree,casename, develop,       &
                             sting_l_all,sting_h_all)

            end if  ! if (LE == 1)

            !
            ! Populate blade coordinate arrays
            !
            do i = 1, np
                
                if (i >= (np - le_pos + 1)) then
                    xb(i) = x_le_spl(i - (np - le_pos))
                    yb(i) = y_le_spl(i - (np - le_pos))
                else  
                    xb(i) = xtop(np - i + 1)
                    yb(i) = ytop(np - i + 1)
                end if
            end do
            do i = 2, np

                if (i <= le_pos) then
                    xb(i + np - 1) = x_le_spl(i + le_pos - 1)
                    yb(i + np - 1) = y_le_spl(i + le_pos - 1)
                else
                    xb(i + np - 1) = xbot(i)
                    yb(i + np - 1) = ybot(i)
                end if
            end do     
            
            ! New number of points for the entire blade section
            np = 2*np-1

        ! 
        ! If spline LE is not required
        !
        else 
            
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

            ! New number of points for the entire blade section
            np = 2*np-1
        end if  ! LE

        !
        ! Stacking of airfoils
        !
        call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)

        uv(1:np,1)              = xb
        uv(1:np,2)              = yb
        uv_top(1:(np + 1)/2,1)  = xtop
        uv_top(1:(np + 1)/2,2)  = ytop
        uv_bot(1:(np + 1)/2,1)  = xbot
        uv_bot(1:(np + 1)/2,2)  = ybot

        ! Write u,v section coordinates to a file in developer mode
        if(isdev) then
            file7 = 'uvblade.'//trim(fext)
            call file_write_1D(file7, xb, yb, np)
        end if

        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) print*, 'chrd bladegen: ', chrd
        write(nopen,*) 'chrd bladegen: ', chrd
        call close_log_file(nopen, file_open)

        if (from_gridgen) then
            xb                  = u_in
            yb                  = v_in
        end if

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

    end if  ! if (trim(airfoil) == 'sect1')

    m_prime(1:np) = xb
    theta(1:np)   = yb


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




















