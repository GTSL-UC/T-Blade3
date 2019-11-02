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
    use file_operations
    use errors
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
        if (using_cur1) then
            cp_start                   = 2 
        else
            curv_cp(2,2*k)             = 0.0
            cp_start                   = 3
        end if
        
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
    ! write_curv_cp in file_operations
    ! TODO: Determine if needed
    !call write_curv_cp(na,curv_cp)


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
    real                            :: intersec_u(nspan)
    character(:),   allocatable     :: log_file
    logical                         :: file_open, isquiet_local


    !
    ! Determine if command line argument 'isquiet' is being used
    !
    call get_quiet_status(isquiet_local)



    !
    ! Allocate spanwise curvature arrays
    !
    if (allocated(bspline_chord_curv)) deallocate(bspline_chord_curv)
    allocate(bspline_chord_curv(nsl,ncp_chord_curv))
   
    

    !
    ! Allocate spanwise thickness arrays
    !
    if (allocated(bspline_thk)) deallocate(bspline_thk)
    allocate(bspline_thk(nsl, ncp_chord_thk))



    !
    ! Allocate spanwise LE spline arrays
    !
    if (allocated(bspline_LE)) deallocate(bspline_LE)
    allocate(bspline_LE(nsl, ncp_LE + 1))


    
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
    if (.not. isquiet_local) print*, 'Creating cubic B-spline with spancontrolinput file'
    write(nopen,*) 'Creating cubic B-spline with spancontrolinput file'
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

        if (.not. isquiet_local) print *, 'Writing spanwise curvature variation data to file'
        call log_file_exists(log_file, nopen, file_open)
        write(nopen,*) 'Writing spanwise curvature variation data to file'
        call close_log_file(nopen, file_open)
        
        ! write_span_curv in file_operations
        call write_span_curv(nsl,ncp_chord_curv,casename,bspline_chord_curv)

    end if  ! isdev



    !
    ! Create spanwise cubic spline for thickness
    ! Generate cubic splines for quartic spline thickness distribution or direct thickness distribution
    !
    if (thick /= 0) then
        
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
    if (isdev .and. (thick /= 0 .or. thick_distr == 5)) then
        
        if (.not. isquiet_local) print *, 'Writing spanwise thickness variation data to file'
        call log_file_exists(log_file, nopen, file_open)
        write(nopen,*) 'Writing spanwise thickness variation data to file'
        call close_log_file(nopen, file_open)

        ! write_span_thk in file_operations
        if (thick_distr == 5) then
            call write_span_thk(nsl,size(cp_chord_thk,2),casename,bspline_thk,thick_distr)
        else
            call write_span_thk(nsl,ncp_chord_thk,casename,bspline_thk,thick_distr)
        end if


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

            if (.not. isquiet_local) print *, 'Writing spanwise LE variation data to file'
            call log_file_exists(log_file, nopen, file_open)
            write(nopen,*) 'Writing spanwise LE variation data to file'
            call close_log_file(nopen, file_open)

            ! write_span_LE in file_operations
            call write_span_LE(nsl,ncp_LE,casename,bspline_LE)

        end if  ! isdev

    endif   ! LE



    !
    ! Print message to screen and write to log file
    !
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet_local) print*, 'B-spline created successfully'
    write(nopen,*) 'B-spline created successfully'
    call close_log_file(nopen, file_open)


    
    !
    ! Call spanwise output subroutine for creating curv_cp, thk_cp and LE arrays
    !
    call span_output_2()


end subroutine span_variation
!---------------------------------------------------------------------------------------------------------------------------




















