!
! This subroutine arranges the values of the control points obtained
! by creating spanwise cubic B-splines in subroutine spanwise_variation() 
! into the specific format for curv_cp, thk_cp and LE arrays
! These arrays are used for calculation of blade sections in
! subroutine bladegen() in bladegen.f90
!
!-------------------------------------------------------------------------------------------------------------------------------
subroutine span_output_2()
    use globvar
    implicit none

    integer                     :: cp_start

    
    ! Allocate curvature control points array
    if (allocated(curv_cp)) deallocate(curv_cp)
    Allocate(curv_cp(20,2*na))

    ! Populate curvature control points array
    k = 1

    do i = 1,na
      if (k <= na) then

        ! Defining fixed control points 
        curv_cp(2, 2*k - 1)             = 0.0
        curv_cp(ncp_chord + 1, 2*k - 1) = 1.0
        
        ! Store spanwise curvature control points from spanwise_variation()
        do j = 3,ncp_chord
            curv_cp(j, 2*k - 1) = bspline_chord_curv(i,j - 1)
        end do
        
        ! Adding two phantom points required for cubic B-spline
        curv_cp(1, 2*k - 1)            = 2*curv_cp(2,2*k - 1) - curv_cp(3,2*k - 1)
        curv_cp(ncp_chord + 2,2*k - 1) = 2*curv_cp(ncp_chord + 1,2*k - 1) - curv_cp(ncp_chord,2*k - 1)
        
        ! Defining fixed control points 
        if (isold) then
            curv_cp(2,2*k) = 0.0
            cp_start       = 3
        else
            cp_start       = 2
        endif
        
        ! Store spanwise curvature control points from spanwise_variation()
        do j = cp_start,ncp_curvature + 1
            curv_cp(j,2*k) = bspline_chord_curv(i,ncp_chord + j - cp_start)
        end do
        
        !Adding two phantom points required for cubic B-spline
        curv_cp(1,2*k)                  = 2*curv_cp(2,2*k) - curv_cp(3,2*k)
        curv_cp(ncp_curvature + 2,2*k)  = 2*curv_cp(ncp_curvature + 1,2*k) - curv_cp(ncp_curvature,2*k)
        
        k = k + 1

      end if    ! if (k <= na)
    end do  ! i = 1,na


    !
    ! Populate thickness control points array
    ! Quartic spline thickness
    !
    if (thick .ne. 0) then
        
        ! Allocate thickness control points array
        if (allocated(thk_cp)) deallocate(thk_cp)
        allocate(thk_cp(20,2*na))
        
        ! Populate thickness control points array 
        k = 1

        do i = 1,na
          if (k <= na) then

            ! Defining fixed control points 
            thk_cp(3, 2*k - 1)                       = 0.0
            thk_cp(ncp_chord_thickness + 2, 2*k - 1) = 1.0
            
            ! Store spanwise thickness control points from spanwise_variation()
            do j = 4,ncp_chord_thickness + 1
                thk_cp(j, 2*k - 1) = bspline_thk(i, j - 2)
            end do
            
            !Adding four phantom points required for creating a quartic spline
            thk_cp(1,2*k - 1)                       = 2*thk_cp(3,2*k - 1) - thk_cp(5,2*k - 1)
            thk_cp(2,2*k - 1)                       = 2*thk_cp(3,2*k - 1) - thk_cp(4,2*k - 1)
            thk_cp(ncp_chord_thickness + 4,2*k - 1) = 2*thk_cp(ncp_chord_thickness + 2,2*k - 1) - thk_cp(ncp_chord_thickness,    2*k - 1)
            thk_cp(ncp_chord_thickness + 3,2*k - 1) = 2*thk_cp(ncp_chord_thickness + 2,2*k - 1) - thk_cp(ncp_chord_thickness + 1,2*k - 1)

            ! Defining fixed control points 
            thk_cp(3,2*k)                 = 0.0
            thk_cp(ncp_thickness + 2,2*k) = 0.0
            
            ! Store spanwise thickness control points from spanwise_variation()
            do j = 4,ncp_thickness + 1
                thk_cp(j,2*k) = bspline_thk(i,ncp_chord_thickness + j - 4)
            end do
            
            !Adding four phantom points required for creating a quartic spline
            thk_cp(1,2*k)                 = 2*thk_cp(3,2*k) - thk_cp(5,2*k)
            thk_cp(2,2*k)                 = 2*thk_cp(3,2*k) - thk_cp(4,2*k)
            thk_cp(ncp_thickness + 4,2*k) = 2*thk_cp(ncp_thickness + 2,2*k) - thk_cp(ncp_thickness,    2*k)
            thk_cp(ncp_thickness + 3,2*k) = 2*thk_cp(ncp_thickness + 2,2*k) - thk_cp(ncp_thickness + 1,2*k)
            
            k = k + 1

          end if    ! if (k <= na)
        end do  ! i = 1,na

    ! Direct thickness distribution
    else if (thick_distr .eq. 3) then
        
        ! Allocate thickness control points array
        if (allocated(thk_cp)) deallocate(thk_cp)
        Allocate(thk_cp(20,2*na))

        ! Populate thickness control points array
        k = 1

        do i = 1,na
            if (k <= na) then
                
                ! Store spanwise thickness control points from spanwise_variation()
                do j = 1,ncp_chord_thickness - 2
                    thk_cp(j,2*k - 1) = bspline_thk(i,j + 1)
                end do
                
                do j=1,ncp_thickness - 2
                    thk_cp(j,2*k) = bspline_thk(i,ncp_chord_thickness + j - 1)
                end do

                k = k + 1

            end if  ! if (k <= na)
        end do  ! i = 1,na

    ! Exact thickness distribution
    else if(thick_distr .eq. 4) then
        
        ! Allocate thickness control points array
        if (allocated(thk_cp)) deallocate(thk_cp)
        Allocate(thk_cp(20, 2*na))

        ! Populate thickness control points array
        k = 1
        do i = 1, na
            if (k <= na)then

                ! Store spanwise thickness control points from spanwise_variation()
                do j = 1, ncp_thickness
                    thk_cp(j,2*k - 1) = bspline_thk(i, 2*j)
                    thk_cp(j,2*k)     = bspline_thk(i, 2*j + 1)
                enddo

                k = k + 1

            end if  ! if (k <= na)
        end do  ! i = 1,na

    ! Modified NACA thickness distribution
    else if (thick_distr == 5) then

        ! Allocate thickness control points array
        if (allocated(thk_cp)) deallocate(thk_cp)
        allocate(thk_cp(20,2*na))

        ! Populate thickness control points array
        do i = 2,size(bspline_thk,2)
            thk_cp(i - 1,1:na)  = bspline_thk(:,i)
        end do

    end if  ! thick_distr



    ! Populate spline LE control points array
    if(LE .ne. 0) then
        
        ! Allocate necessary control points arrays
        if (allocated(sting_l_all)) deallocate(sting_l_all)
        allocate(sting_l_all(na))
          !if(LE .ne.0) then
        if (allocated(lethk_all)) deallocate(lethk_all)
        allocate(lethk_all(na))
        if (allocated(tethk_all)) deallocate(tethk_all)
        allocate(tethk_all(na))
        if (allocated(s_all)) deallocate(s_all)
        allocate(s_all(na))
        if (allocated(ee_all)) deallocate(ee_all)
        allocate(ee_all(na))
        if (allocated(C_le_x_top_all)) deallocate(C_le_x_top_all)
        allocate(C_le_x_top_all(na))
        if (allocated(C_le_x_bot_all)) deallocate(C_le_x_bot_all)
        allocate(C_le_x_bot_all(na))
        if (allocated(C_le_y_top_all)) deallocate(C_le_y_top_all)
        allocate(C_le_y_top_all(na))
        if (allocated(C_le_y_bot_all)) deallocate(C_le_y_bot_all)
        allocate(C_le_y_bot_all(na))
        if (allocated(LE_vertex_ang_all)) deallocate(LE_vertex_ang_all)
        allocate(LE_vertex_ang_all(na))
        if (allocated(LE_vertex_dis_all)) deallocate(LE_vertex_dis_all)
        allocate(LE_vertex_dis_all(na))
        if (allocated(sting_h_all)) deallocate(sting_h_all)
        allocate(sting_h_all(na,2))
            
        ! Store spanwise spline LE control points from spanwise_variation()
        k = 1

        do i = 1,na
            if (k <= na) then
                
                lethk_all(k)         = bspline_LE(i,2)
                tethk_all(k)         = bspline_LE(i,3)
                s_all(k)             = bspline_LE(i,4)
                ee_all(k)            = bspline_LE(i,5)
                C_le_x_top_all(k)    = bspline_LE(i,6)
                C_le_x_bot_all(k)    = bspline_LE(i,7)
                C_le_y_top_all(k)    = bspline_LE(i,8)
                C_le_y_bot_all(k)    = bspline_LE(i,9)
                LE_vertex_ang_all(k) = bspline_LE(i,10)
                LE_vertex_dis_all(k) = bspline_LE(i,11)
                sting_l_all(k)       = bspline_LE(i,12)
                sting_h_all(k,1)     = bspline_LE(i,13)
                sting_h_all(k,2)     = bspline_LE(i,14)
            
                k = k + 1

            end if  ! if (k <= na)
        end do  ! if i = 1,na
        !end if
    endif
    
    
    ! Write curvature control points to file
    open(150, file = 'curv_cp.dat')
        do i = 1,20
            write(150,'(22F15.2)') curv_cp(i,1:2*na)
        end do
    close(150)


end subroutine span_output_2
!-------------------------------------------------------------------------------------------------------------------------------






!
! This subroutine is used to create a cubic B-spline between variables
! taken from spancontrolinputs file and calls the subroutine span_output()
! in spanwise_output.f90
!
! Uses subroutines cubicspline and cubicbspline_intersec in cubicspline.f90
!
!---------------------------------------------------------------------------------------------------------------------------
subroutine span_variation()
    use file_operations
    use globvar
    implicit none

    ! Local variables
    integer                         :: np_fine, nopen
    real,           allocatable     :: span_fine(:), out_coord_u_fine(:,:), out_coord_v_fine(:,:)
    real                            :: out_coord_u(na, 12), out_coord_v(na, 12), intersec_u(nspan), &
                                       intersec_v(nspan)
    character(20)                   :: ind
    character(:),   allocatable     :: log_file, thickness_file_name, curvature_file_name, LE_file_name
    logical                         :: file_open, file_exist



    !
    ! Allocate spanwise curvature arrays
    !
    if (allocated(bspline_chord_curv)) deallocate(bspline_chord_curv)
    allocate(bspline_chord_curv(nsl,ncp_chord_curv))
   
    

    !
    ! Allocate spanwise thickness arrays
    !
    if (allocated(bspline_thk)) deallocate(bspline_thk)
    if (thick_distr == 4) then
        allocate(bspline_thk(nsl, 2*ncp_thickness + 1))
    else
        allocate(bspline_thk(nsl, ncp_chord_thk))
    end if



    !
    ! Allocate spanwise LE spline arrays
    !
    if (allocated(bspline_LE)) deallocate(bspline_LE)
    allocate(bspline_LE(nsl, ncp_LE + 1))


    
    !
    ! Allocate exact thickness spanwise LE_angle and TE_angle arrays
    !
    if (allocated(le_angle_all)) deallocate(le_angle_all)
    allocate(le_angle_all(na))
    if (allocated(te_angle_all)) deallocate(te_angle_all)
    allocate(te_angle_all(na))
   
   
   
    ! 
    ! Allocate exact thickness arrays
    !
    np_fine = 1000
    if (allocated(span_fine)) deallocate(span_fine)
    allocate(span_fine(np_fine))
    if (allocated(out_coord_u_fine)) deallocate(out_coord_u_fine)
    allocate(out_coord_u_fine(np_fine, 12))
    if (allocated(out_coord_v_fine)) deallocate(out_coord_v_fine)
    allocate(out_coord_v_fine(np_fine, 12))



    !
    ! Print message to screen and write to log file
    !
    call log_file_exists(log_file, nopen, file_open)
    if (thick_distr == 4) then
        print*, 'Creating spanwise thickness and TE angle distributions'
        print*, 'Creating cubic Bspline with spancontrolinput file for spanwise curvature'
        write(nopen,*) 'Creating spanwise thickness and TE angle distributions'
        write(nopen,*) 'Creating cubic Bspline with spancontrolinput file for spanwise curvature'
    else
        print*, 'Creating cubic B-spline with spancontrolinput file'
        write(nopen,*) 'Creating cubic B-spline with spancontrolinput file'
    endif
    call close_log_file(nopen, file_open)



    !
    ! Generate span_fine array
    !
    do i = 1, np_fine
        span_fine(i) = (i-1.)/(np_fine-1)*(span(na) - span(1)) + span(1)
    enddo



    !
    ! Create spanwise cubic spline for curvature
    ! Spanwise distribution of "Span" control points
    !
    do j = 1,nsl
        bspline_chord_curv(j,1) = span(j)
    end do

    ! Spanwise distribution of "u" and "cur" control points
    do i = 2,ncp_chord_curv

        call cubicspline(cp_chord_curv(:,i),cp_chord_curv(:,1),ncp_span_curv,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
        call cubicbspline_intersec(y_spl_end,xc,yc,ncp1,span,intersec,na,xbs,ybs)

        do j = 1,na
            bspline_chord_curv(j,i) = intersec(j)
        end do

    end do  ! i = 2,ncp_chord_curv

    ! Write curvature spanwise spline data to a file, if command line option "dev" is used
    if (isdev) then

        print *, 'Writing spanwise curvature variation data to file'
        call log_file_exists(log_file, nopen, file_open)
        write(nopen,*) 'Writing spanwise curvature variation data to file'
        call close_log_file(nopen, file_open)

        curvature_file_name = 'curvature_span_variation.'//trim(casename)//'.dat'
        inquire(file = curvature_file_name, exist=file_exist)

        if (file_exist) then
            open(97, file = curvature_file_name, status = 'old', action = 'write', form = 'formatted')
        else
            open(97, file = curvature_file_name, status = 'new', action = 'write', form = 'formatted')
        end if

        do i = 1,nsl

            write(97, '(20F20.16)') bspline_chord_curv(i,:)

        end do
        close(97)

    end if  ! isdev



    !
    ! Create spanwise cubic spline for thickness
    ! Generate cubic splines for quartic spline thickness distribution or direct thickness distribution
    !
    if (thick /= 0 .or. thick_distr == 3) then
        
        ! Spanwise distribution of "Span" control points
        do j = 1,na
            bspline_thk(j,1) = span(j)
        end do

        ! Spanwise distribution of "u" and "thk" control points
        do i = 2,ncp_chord_thk

            call cubicspline(cp_chord_thk(:,i),cp_chord_thk(:,1),ncp_span_thk,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
            call cubicbspline_intersec(y_spl_end,xc,yc,ncp1,span,intersec,na,xbs,ybs)
            
            do j = 1,na
                bspline_thk(j,i) = intersec(j)
            end do

        end do  ! i = 2,ncp_chord_thk
        
    !
    ! Generate cubic splines for exact thickness distribution
    !
    else if (thick_distr == 4) then
        
        ! Spanwise distribution of "Span" control points
        do j = 1,na
            bspline_thk(j,1) = span(j)
        enddo

        ! Spanwise distribution of "u" and "thk" control points
        do i = 1,ncp_thickness

            write(ind, '(i2)') i
            call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), cp_chord_thk(:,i+ncp_thickness+1), ncp_span_thk, span, na, &
                                           1, out_coord_v)
            intersec_v(1:na) = out_coord_v(:, 2)
            call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), cp_chord_thk(:,i+1), ncp_span_thk, span, na, 1, out_coord_u)
            intersec_u(1:na) = out_coord_u(:, 2)
            
            ! Compute with span_fine if command line option "dev" is passed to T-Blade3
            if (isdev) then

                call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), cp_chord_thk(:,i+1), ncp_span_thk, span_fine, np_fine, &
                                               1, out_coord_u_fine)

                call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), cp_chord_thk(:,i+ncp_thickness+1), ncp_span_thk,       &
                                               span_fine, np_fine, 1, out_coord_v_fine)

            endif   ! isdev
           
            ! Store "u" and "thk" spanwise distribution in the spanwise thickness array 
            do j = 1, na
                bspline_thk(j, 2*i) = intersec_u(j)
                bspline_thk(j, 2*i+1) = 0.5*intersec_v(j)
            enddo

        enddo   ! i = 1,ncp_thickness

        !
        ! Spanwise distribution of "tetht"
        ! Store "tetht" spanwise distribution in exact thickness spanwise TE angle array
        ! Write spline data to file
        !
        call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), te_angle_cp, ncp_span_thk, span, na, 1, out_coord_v)
        te_angle_all(1:na) = out_coord_v(:, 2)
        open (unit = 81, file = 'te_angle_span_dist.' // trim(casename) // '.dat')
        write (81, '(12F30.12)') (out_coord_v(k, :), k = 1, na)
        close (81)
        
        ! Compute with span_fine if command line option "dev" is passed to T-Blade3
        if (isdev) then

            call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), te_angle_cp, ncp_span_thk, span_fine, np_fine, 1, out_coord_v_fine)

            ! Write spline data to file
            open (unit = 81, file = 'te_angle_span_dist_fine.' // trim(casename) // '.dat')
            write (81, '(12F30.12)') (out_coord_v_fine(k, :), k = 1, np_fine)
            close (81)

        endif   ! isdev

        !
        ! Spanwise distribution of "letht"
        ! Store "letht" spanwise distribution in exact thickness spanwise LE angle array
        ! Write spline data to file
        !
        call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), le_angle_cp, ncp_span_thk, span, na, 1, out_coord_v)
        le_angle_all(1:na) = out_coord_v(:, 2)
        open (unit = 81, file = 'le_angle_span_dist.' // trim(casename) // '.dat')
        write (81, '(12F30.12)') (out_coord_v(k, :), k = 1, na)
        close (81)

        ! Compute with span_fine if command line option "dev" is passed to T-Blade3
        if (isdev) then

            call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), le_angle_cp, ncp_span_thk, span_fine, np_fine, 1, out_coord_v_fine)

            ! Write spline data to file
            open (unit = 81, file = 'le_angle_span_dist_fine.' // trim(casename) // '.dat')
            write (81, '(12F30.12)') (out_coord_v_fine(k, :), k = 1, np_fine)
            close (81)

        endif   ! isdev

    !
    ! Generate cubic splines for modified NACA four-digit thickness distribution
    !
    else if (thick_distr == 5) then

        if (allocated(bspline_thk)) deallocate(bspline_thk)
        allocate(bspline_thk(nsl,size(cp_chord_thk,2)))

        ! Spanwise distribution of "Span" control points
        bspline_thk(:,1) = span(1:nsl)

        ! Spanwise distribution of "LE_radius", "u_max", "t_max" and "t_TE"
        do i = 2,size(cp_chord_thk,2)

            call cubicspline(cp_chord_thk(:,i),cp_chord_thk(:,1),ncp_span_thk,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
            call cubicbspline_intersec(y_spl_end,xc,yc,ncp1,span,intersec_u,na,xbs,ybs)

            ! For "t_max" and "t_TE", store as half thickness
            if (i == 4 .or. i == 5) then
                bspline_thk(:,i) = 0.5*intersec_u(1:nsl)
            else
                bspline_thk(:,i) = intersec_u(1:nsl)
            end if

        end do  ! i = 2,5

    end if  ! thick_distr



    !
    ! Write thickness spanwise spline data to a file, if command line option "dev" is used
    !
    if (isdev) then
        
        print *, 'Writing spanwise thickness variation data to file'
        call log_file_exists(log_file, nopen, file_open)
        write(nopen,*) 'Writing spanwise thickness variation data to file'
        call close_log_file(nopen, file_open)

        thickness_file_name = 'thickness_span_variation.'//trim(casename)//'.dat'
        inquire(file = thickness_file_name, exist=file_exist)

        if (file_exist) then
            open(97, file = thickness_file_name, status = 'old', action = 'write', form = 'formatted')
        else
            open(97, file = thickness_file_name, status = 'new', action = 'write', form = 'formatted')
        end if

        do i = 1,nsl

            write(97, '(20F20.16)') bspline_thk(i,:)

        end do
        close(97)

    end if  ! isdev



    !
    ! Create spanwise cubic splines for LE 
    !
    if(LE .ne. 0) then

        ! Spanwise distribution of "Span"
        do j = 1,na
            bspline_LE(j,1)=span(j)
        end do

        !
        ! Spanwise distributions of: 
        ! "lethk", "tethk", "s", "ee", "C_le_x_top", "C_le_x_bot", "C_le_y_top", "C_le_y_bot", 
        ! "LE_vertex_ang", "LE_vertex_dis", "sting_l", "sting_h_top", "sting_h_bot"
        !
        do i = 2,ncp_LE

            call cubicspline(cp_LE(:,i),cp_LE(:,1),ncp_span_LE,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
            call cubicbspline_intersec(y_spl_end,xc,yc,ncp1,span,intersec,na,xbs,ybs)

            do j = 1,na
                bspline_LE(j,i)=intersec(j)
            end do

        end do  ! i = 2,ncp_LE

        ! Write LE spanwise spline data to a file, if command line option "dev" is used
        if (isdev) then

            print *, 'Writing spanwise LE variation data to file'
            call log_file_exists(log_file, nopen, file_open)
            write(nopen,*) 'Writing spanwise LE variation data to file'
            call close_log_file(nopen, file_open)

            LE_file_name = 'LE_span_variation.'//trim(casename)//'.dat'
            inquire(file = LE_file_name, exist=file_exist)

            if (file_exist) then
                open(97, file = LE_file_name, status = 'old', action = 'write', form = 'formatted')
            else
                open(97, file = LE_file_name, status = 'new', action = 'write', form = 'formatted')
            end if

            do i = 1,nsl

                write(97, '(20F20.16)') bspline_LE(i,:)

            end do
            close(97)

        end if  ! isdev

    endif   ! LE



    !
    ! Print message to screen and write to log file
    !
    call log_file_exists(log_file, nopen, file_open)
    if (thick_distr == 4) then
        print*, 'Spanwise curvature Bspline created successfully'
        print*, 'Spanwise thickness and TE angle distributions created successfully:'
        print*, 'Files prefixed with thk_span_dist and te_angle_span_dist created'
        write(nopen,*) 'Spanwise curvature Bspline created successfully'
        write(nopen,*) 'Spanwise thickness and TE angle distributions created successfully:'
        write(nopen,*) 'Files prefixed with thk_span_dist and te_angle_span_dist created'
    else
        print*, 'B-spline created successfully'
        write(nopen,*) 'B-spline created successfully'
    endif
    call close_log_file(nopen, file_open)


    
    !
    ! Call spanwise output subroutine for creating curv_cp, thk_cp and LE arrays
    !
    call span_output_2()


end subroutine span_variation
!---------------------------------------------------------------------------------------------------------------------------




















