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

    integer                         :: np_fine, i_local, nopen
    real,           allocatable     :: span_fine(:), out_coord_u_fine(:,:), out_coord_v_fine(:,:)
    real                            :: out_coord_u(na, 12), out_coord_v(na, 12), intersec_u(nspan), &
                                       intersec_v(nspan)
    character(20)                   :: ind
    character(:),   allocatable     :: log_file, thickness_file_name
    logical                         :: file_open, file_exist


    ! Allocate spanwise curvature arrays
    if (allocated(bspline_chord_curv)) deallocate(bspline_chord_curv)
    allocate(bspline_chord_curv(nsl,ncp_chord_curv))
    
    ! Allocate spanwise thickness arrays
    if (allocated(bspline_thk)) deallocate(bspline_thk)
    if (thick_distr == 4) then
        allocate(bspline_thk(nsl, 2*ncp_thickness + 1))
    else if (thick_distr == 5) then
        allocate(bspline_thk(nsl, 5))
    else
        allocate(bspline_thk(nsl, ncp_chord_thk))
    end if

    ! Allocate spanwise LE spline arrays
    if (allocated(bspline_LE)) deallocate(bspline_LE)
    allocate(bspline_LE(nsl, ncp_LE + 1))

    ! Allocate exact thickness spanwise LE_angle and TE_angle arrays
    if (allocated(le_angle_all)) deallocate(le_angle_all)
    allocate(le_angle_all(na))
    if (allocated(te_angle_all)) deallocate(te_angle_all)
    allocate(te_angle_all(na))
    
    ! Allocate exact thickness arrays
    np_fine = 1000
    if (allocated(span_fine)) deallocate(span_fine)
    allocate(span_fine(np_fine))
    if (allocated(out_coord_u_fine)) deallocate(out_coord_u_fine)
    allocate(out_coord_u_fine(np_fine, 12))
    if (allocated(out_coord_v_fine)) deallocate(out_coord_v_fine)
    allocate(out_coord_v_fine(np_fine, 12))


    ! Print message to screen and write to log file
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


    ! Generate span_fine array
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


    !
    ! Create spanwise cubic spline for thickness
    ! Generate cubic splines for quartic spline thickness distribution or direct thickness distribution
    !
    if(thick /= 0 .or. thick_distr == 3) then
        
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

    ! Generate cubic splines for exact thickness distribution
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
            
            ! Write spline data to files
            open (unit = 81, file = 'thk_span_dist_v.' // trim(adjustl(ind)) // '.' // trim(casename) // '.dat')
            write (81, '(12F30.12)') (out_coord_v(k, :), k = 1, na)
            close (81)
            open (unit = 81, file = 'thk_span_dist_u.' // trim(adjustl(ind)) // '.' // trim(casename) // '.dat')
            write (81, '(12F30.12)') (out_coord_u(k, :), k = 1, na)
            close (81)

            ! Compute with span_fine if command line option "dev" is passed to T-Blade3
            if (isdev) then

                call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), cp_chord_thk(:,i+1), ncp_span_thk, span_fine, np_fine, &
                                               1, out_coord_u_fine)

                ! Write spline data to file
                open (unit = 81, file = 'thk_span_dist_u_fine.' // trim(adjustl(ind)) // '.' // trim(casename) // '.dat')
                write (81, '(12F30.12)') (out_coord_u_fine(k, :), k = 1, np_fine)
                close (81)

                call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), cp_chord_thk(:,i+ncp_thickness+1), ncp_span_thk,       &
                                               span_fine, np_fine, 1, out_coord_v_fine)

                ! Write spline data to file
                open (unit = 81, file = 'thk_span_dist_v_fine.' // trim(adjustl(ind)) // '.' // trim(casename) // '.dat')
                write (81, '(12F30.12)') (out_coord_v_fine(k, :), k = 1, np_fine)
                close (81)
            
            endif   ! isdev
           
            ! Store "u" and "thk" spanwise distribution in the spanwise thickness array 
            do j = 1, na
                bspline_thk(j, 2*i) = intersec_u(j)
                bspline_thk(j, 2*i+1) = intersec_v(j)
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

    ! Generate cubic splines for modified NACA four-digit thickness distribution
    else if (thick_distr == 5) then

        ! Spanwise distribution of "Span" control points
        bspline_thk(:,1) = span(1:nsl)

        ! Spanwise distribution of "u" and "thk" control points
        do i = 2,5

            call cubicspline(cp_chord_thk(:,i),cp_chord_thk(:,1),ncp_span_thk,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
            call cubicbspline_intersec(y_spl_end,xc,yc,ncp1,span,intersec_u,na,xbs,ybs)
            bspline_thk(:,i) = intersec_u(1:nsl)

        end do  ! i = 2,5

        ! Write spline data to a file
        if (isdev) then

            print *, 'Writing spanwise thickness variation data to file'
            call log_file_exists(log_file, nopen, file_open)
            write(nopen,*) "Writing spanwise thickness variation data to file"
            call close_log_file(nopen, file_open)

            thickness_file_name = 'thickness_span_variation.'//trim(casename)//'.dat'
            inquire(file = thickness_file_name, exist=file_exist)
        
            if (file_exist) then
                open(97, file = thickness_file_name, status = 'old', action = 'write', form = 'formatted')
            else
                open(97, file = thickness_file_name, status = 'new', action = 'write', form = 'formatted')
            end if
            do i = 1,nsl

                write(97, '(5F20.16)') bspline_thk(i,:)
            
            end do
            close(97)

        end if  ! isdev

    end if  ! thick_distr


    ! Create spanwise cubic splines for LE 
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

    endif   ! LE


    ! Print message to screen and write to log file
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


    ! Call spanwise output subroutine for creating curv_cp, thk_cp and LE arrays
    call span_output()


end subroutine span_variation
!---------------------------------------------------------------------------------------------------------------------------




















