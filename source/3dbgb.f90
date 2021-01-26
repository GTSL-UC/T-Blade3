!
! Main T-Blade3 driver subroutine
! Input parameters: fname_in - T-Blade3 main input filename
!                   aux_in   - T-Blade3 auxiliary input filename
!                   arg2     - second command line argument
!                   arg3     - third command line argument
!                   arg4     - fourth command line argument
!
!----------------------------------------------------------------------------------------------------------
subroutine bgb3d_sub(fname_in, aux_in, arg2, arg3, arg4) 
    use globvar
    use file_operations
    use errors
    use funcNsubs
    implicit none
    
    character(*)                :: fname_in
    character(*)                :: aux_in
    character(*)                :: arg2
    character(*)                :: arg3
    character(*)                :: arg4

    ! Local variables
    integer                     :: nopen, nopen_error, n_ext = 21, n_ext_mean
    real(kind = 8)              :: spl_eval, dspl_eval, xdiff, temp_1, temp_2
    real(kind = 8), allocatable :: um_spl(:), spanwise_thk(:), mhub_inf(:), &
                                   thhub_inf(:), xhub_inf(:), yhub_inf(:), zhub_inf(:), mtip_inf(:), &
                                   thtip_inf(:), xtip_inf(:), ytip_inf(:), ztip_inf(:),              &
                                   m_ext_mean(:,:), th_ext_mean(:,:), mt_umax(:,:), dim_thick(:,:)
    character(256)              :: fname, temp, row_type, path
    character(:),   allocatable :: log_file, error_file, auxinput_filename, error_msg
    logical                     :: axial_TE, radial_TE, file_open, file_exist, write_csv, &
                                   initial, open_error  !axial_LE, radial_LE


    !
    ! Initiallze logical variables and flags
    !
    axial_LE                                = .False.
    radial_LE                               = .False.
    axial_TE                                = .False.
    radial_TE                               = .False.
    is_xyzstreamlines                       = .False.
    is2d                                    = .False.
    isquiet                                 = .false.
    initial                                 = .true.
    wing_flag                               = 0
   
    

    !
    ! Compute constants
    !
    pi                                      = 4.*atan(1.0)
    dtor                                    = pi/180.
    radius_tolerance                        = 1e-05
    abs_zero                                = 0.0000000000000000



    ! 
    ! Create error log file for each new T-Blade3 run
    ! If no errors or warnings are raised, the log file will be empty
    !
    call error_file_exists(error_file, nopen_error, open_error, initial)
    write(nopen_error,'(A)') 'T-Blade3 ERROR LOG FILE'
    call close_error_file(nopen_error, open_error)




    ! Types of 2nd argument
    if (trim(arg2) == 'dev') then
        isdev                               = .true.
    else if (trim(arg2) == 'xygrid') then 
        error_msg                           = 'Command line option "xygrid" is no longer &
                                              &available with T-Blade3'
        call fatal_error(error_msg)
    else if (trim(arg2) == 'xyzstreamlines') then 
        is_xyzstreamlines                   = .true.
    else if ((trim(arg2) == '2d') .or. (trim(arg2) == '2D')) then
        is2d                                = .true.
    else if ((trim(arg2) == 'v0') .or. (trim(arg2) == 'V0')) then
        error_msg                           = 'Command line option "v0" is no longer &
                                              &available with T-Blade3'
        call fatal_error(error_msg)
    else if (trim(arg2) == 'quiet') then
        isquiet                             = .true.
    end if

    ! Types of 3rd argument
    if (trim(arg3) == 'dev') then
        isdev                               = .true.
    else if (trim(arg3) == 'xygrid') then 
        error_msg                           = 'Command line option "xygrid" is no longer &
                                              &available with T-Blade3'
        call fatal_error(error_msg)
    else if (trim(arg3) == 'xyzstreamlines') then 
        is_xyzstreamlines                   = .true.
    else if ((trim(arg3) == '2d') .or. (trim(arg3) == '2D')) then
        is2d                                = .true.
    else if ((trim(arg3) == 'v0') .or. (trim(arg3) == 'V0')) then
        error_msg                           = 'Command line option "v0" is no longer &
                                              &available with T-Blade3'
    else if (trim(arg3) == 'quiet') then
        isquiet                             = .true.
    end if

    ! Types of 4th argument
    if (trim(arg4) == 'dev') then
        isdev                               = .true.
    else if (trim(arg4) == 'xygrid') then 
        error_msg                           = 'Command line option "xygrid" is no longer &
                                              &available with T-Blade3'
        call fatal_error(error_msg)
    else if (trim(arg4) == 'xyzstreamlines') then 
        is_xyzstreamlines                   = .true.
    else if ((trim(arg4) == '2d') .or. (trim(arg4) == '2D')) then
        is2d                                = .true.
    else if ((trim(arg4) == 'v0') .or. (trim(arg4) == 'V0')) then
        error_msg                           = 'Command line option "v0" is no longer &
                                              &available with T-Blade3'
        call fatal_error(error_msg)
    else if (trim(arg4) == 'quiet') then
        isquiet                             = .true.
    end if



    ! 
    ! Display the welcome message and some info about the code capabilities
    ! displayMessage() in funcNsubs.f90
    !
    call displayMessage
    
    ! Input file name
    fname                                   = fname_in



    !
    ! Print command-line argument related data to screen and write to file
    ! log_file_exists() in file_operations.f90
    !
    call log_file_exists(log_file, nopen, file_open)
    
    ! Print 2nd argument
    select case(trim(arg2))
        case('dev')
            if (.not. isquiet) print *, '2nd Argument: develop'
            write(nopen,*) '2nd Argument: develop'
        case('xyzstreamlines')
            if (.not. isquiet) print *, '2nd Argument: xyzstreamlines'
            write(nopen,*) '2nd Argument: xyzstreamlines'
        case('2d','2D')
            if (.not. isquiet) print *, '2nd Argument: 2D'
            write(nopen,*) '2nd Argument: 2D'
        case('quiet')
            write(nopen,*) '2nd Argument: quiet'
    end select

    ! Print 3rd argument
    select case(trim(arg3))
        case('dev')
            if (.not. isquiet) print *, '3rd Argument: develop'
            write(nopen,*) '3rd Argument: develop'
        case('xyzstreamlines')
            if (.not. isquiet) print *, '3rd Argument: xyzstreamlines'
            write(nopen,*) '3rd Argument: xyzstreamlines'
        case('2d','2D')
             if (.not. isquiet) print *, '3rd Argument: 2D'
            write(nopen,*) '3rd Argument: 2D'
        case('quiet')
            write(nopen,*) '3rd Argument: quiet'
    end select
   
    ! Print 4th argument 
    select case(trim(arg4))
        case('dev')
            if (.not. isquiet) print *, '4th Argument: develop'
            write(nopen,*) '4th Argument: develop'
        case('xyzstreamlines')
            if (.not. isquiet) print *, '4th Argument: xyzstreamlines'
            write(nopen,*) '4th Argument: xyzstreamlines'
        case('2d','2D')
            if (.not. isquiet) print *, '4th Argument: 2D'
            write(nopen,*) '4th Argument: 2D'
        case('quiet')
            write(nopen,*) '4th Argument: quiet'
    end select


    ! close_log_file() in file_operation.f90
    call close_log_file(nopen,file_open)



    !
    ! Locate path of current working directory
    !
    control_inp_flag                        = 0
    j                                       = -1
    do i = len(trim(fname)), 1, -1
        if ((fname(i:i)  ==  '/') .or. (fname(i:i)  ==  '\')) then
            j                               = i
            exit
        end if
    end do
    
    if (j == -1) then
        path                                = ''
    else
        path                                = fname(1:j)
    end if


    
    !
    ! Determine row number and blade type from the main input filename
    ! Print to screen and write to main log file
    !
    k                                       = 0
    
    do i = len(trim(fname)), 1, -1
        if (fname(i:i)  ==  '.') then
            k                               = k + 1
            if (k == 1) then
                j                           = i - 1
            end if
            if (k == 2) then
                k                           = i + 1
                exit
            end if
        end if
    end do
    
    row_type                                = fname(k:j)

    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) print *, 'Row number and blade type is ', row_type
    write(nopen,*) 'Row number and blade  type is ', row_type
    call close_log_file(nopen, file_open)


   
    ! 
    ! Read the main input file 
    ! readinput() in readinput.f90
    !
    call readinput(fname)
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) then
        write(*,*)
        write(*,*) 'Reading inputs from file : ', fname
    end if
    write(nopen,*) ''
    write(nopen,*) 'Reading inputs from file: ', fname
    call close_log_file(nopen, file_open)



    ! 
    ! Determine if auxiliary input file is needed depending on main input file switches
    ! Set control_inp_flag depending on whether spanwise spline switch is engaged
    !
    if ((curv /= 0 .or. thick /= 0 .or. LE /= 0 .or.thick_distr /= 0) &
        .and.trim(spanwise_spline) /= 'spanwise_spline')then
        control_inp_flag                    = 1
    else if ((curv /= 0 .or. thick /= 0 .or. LE /= 0 .or. thick_distr /= 0) & 
           .and.trim(spanwise_spline) == 'spanwise_spline')then
        control_inp_flag                    = 2
    end if

    ! Determine which auxiliary input file should be read
    call log_file_exists(log_file, nopen, file_open)



    ! Read old controlinputs file (added by Nemnem)
    ! TODO: To be removed
    if (control_inp_flag  ==  1) then
        if (.not. isquiet) then
            write(*, *)
            print *, 'Reading the controlinput file ....'
            write(*, *)
        end if
        write(nopen,*) ''
        write(nopen,*) 'Reading the controlinput file ....'
        write(nopen,*) ''

        ! If auxiliary input file doesn't exist, warn and quit
        ! readcontrolinputs() in readinput.f90
        auxinput_filename                   = trim(path)//'controlinputs.'//trim(row_type)//'.dat'
        inquire(file = auxinput_filename, exist=file_exist)
        if (file_exist) then
            call readcontrolinput(row_type, path)
        else
            error_msg                       = 'Auxiliary input file '//auxinput_filename//' does not exist'
            call fatal_error(error_msg)
        end if

    ! Read old spancontrolinputs file (added by Syed)
    else if (control_inp_flag  ==  2) then 
        if (.not. isquiet) then
            write(*, *)
            print *, 'Reading the spanwise_input file ....'
            write(*, *)
        end if
        write(nopen,*) ''
        write(nopen,*) 'Reading the spanwise_input file ....'
        write(nopen,*) ''

        ! If auxiliary input file doesn't exist, warn and quit
        ! read_spanwise_NACA_input() and read_spanwise_input() in readinput.f90
        if (thick_distr == 5) then
            auxinput_filename               = trim(path)//'spancontrolinputs.'//trim(row_type)//'.dat'
            inquire(file = auxinput_filename, exist=file_exist)
            if (file_exist) then
                call read_spanwise_NACA_input(row_type, path)
            else
                error_msg                   = 'Auxiliary input file '//auxinput_filename//' does not exist'
                call fatal_error(error_msg)
            end if
        else
            auxinput_filename               = trim(path)//'spancontrolinputs.'//trim(row_type)//'.dat'
            inquire(file = auxinput_filename, exist=file_exist)
            if (file_exist) then
                call read_spanwise_input(row_type, path)
            else
                error_msg                   = 'Auxiliary input file '//auxinput_filename//' does not exist'
                call fatal_error(error_msg)
            end if
        end if

    end if  ! control_inp_flag

    call close_log_file(nopen, file_open)



    !
    ! Print basic case data to the screen and write to main log file
    !
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) then
        write(*, *)
        write(*, *) 'case:', fext
        write(*, *) 'bladerow #:', ibrow
        write(*, *) ibrowc
        write(*, *)
        write(*, *) 'Number of blades in this row:', nbls
        write(*, *) 'bsf:', scf
        write(*, *)
        write(*, *) 'Number of streamlines:', nsl
        write(*, *)
    end if

    write(nopen, *)
    write(nopen, *) 'case:', fext
    write(nopen, *) 'bladerow #:', ibrow
    write(nopen, *) ibrowc
    write(nopen, *)
    write(nopen, *) 'Number of blades in this row:', nbls
    write(nopen, *) 'bsf:', scf
    write(nopen, *)
    write(nopen, *) 'Number of streamlines:', nsl
    write(nopen, *)
    call close_log_file(nopen, file_open)



    !
    ! If input angle switch in the main input file is not engaged
    ! print data to screen and write to main log file
    !
    if (.not. spanwise_angle_spline) then
        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) print *, '   in_betaZ*    out_betaZ*'
        write(nopen,*) '   in_betaZ*    out_betaZ*'
        do js = 1, nspn
            if (.not. isquiet) print *, in_beta(js), out_beta(js)
            write(nopen,*) in_beta(js), out_beta(js)
        end do
        call close_log_file(nopen, file_open)
    end if



    ! Cubic spline for the LE and TE coordinates from the input file
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) then
        write(*, *)
        write(*, *)'LE/TE defined by a curve with no. of points as:', npoints
        write(*, *)'xLE    rLE     xTE     rTE'

        write(nopen, *)
        write(nopen, *)'LE/TE defined by a curve with no. of points as:', npoints
        write(nopen, *)'xLE    rLE     xTE     rTE'
    end if

    ! arclength() and spline() in spline.f90
    do i = 1, npoints

        if (.not. isquiet) print *, xle(i), rle(i), xte(i), rte(i)
        write(nopen,*) xle(i), rle(i), xte(i), rte(i)
        
        ! Spline LE curve
        call arclength(npoints, xle(1), rle(1), sle(1))
        call spline(npoints, xle(1), xles(1), sle(1), 999.0, -999.0)
        call spline(npoints, rle(1), rles(1), sle(1), 999.0, -999.0)
        
        ! Spline TE curve
        call arclength(npoints, xte(1), rte(1), ste(1))
        call spline(npoints, xte(1), xtes(1), ste(1), 999.0, -999.0)
        call spline(npoints, rte(1), rtes(1), ste(1), 999.0, -999.0)
    
    end do
    call close_log_file(nopen, file_open)



    !
    ! LE spline options data
    !
    call log_file_exists(log_file, nopen, file_open)
    write(nopen,*) ''
    
    ! If leading edge switch is engaged in the main input file
    if (LE /= 0) then 
        
        do js = 1, nspn
            if (.not. isquiet) print *, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), jcellblade_all(js), etawidth_all(js), &
                                       &BGgrid_all(js)
            write(nopen,*) airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), jcellblade_all(js),            &
                           etawidth_all(js), BGgrid_all(js)
        end do
    
    ! For elliptical LE
    else if (LE == 0) then

        do js = 1, nspn
            if (.not. isquiet) print *, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), lethk_all(js), tethk_all(js),         &
                                       &jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
            write(nopen,*) airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), lethk_all(js), tethk_all(js),  &
                           jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
                           
        end do
    
    end if   ! LE
    call close_log_file(nopen, file_open)

    
    
    ! 
    ! Print sweep options and write to main log file
    ! Differentiate between true and axial sweep
    !
    call log_file_exists(log_file, nopen, file_open)
    if (trim(trueleansweep) /= '')then
        if (.not. isquiet) then
            print *, 'Sweep along the chord (1 = yes): ', chrdsweep
            write(*, *)
        end if
        write(nopen,*) ''
        write(nopen,*) 'Sweep along the chord (1 = yes): ', chrdsweep
        write(nopen,*) ''
    else
        if (.not. isquiet) then
            print *, 'Sweep in the axial direction (m-prime).'
            write(*, *)
        end if
        write(nopen,*) ''
        write(nopen,*) 'Sweep in the axial direction (m-prime).'
        write(nopen,*) ''
    end if

    ! 
    ! Print lean options and write to main log file
    ! Differentiate between true and axial lean
    !
    if (trim(trueleansweep) /= '')then
        if (.not. isquiet) then
            print *, 'Lean normal to the chord (1 = yes): ', chrdlean
            write(*, *)
        end if
        write(nopen,*) 'Lean normal to the chord (1 = yes): ', chrdlean
        write(nopen,*)
    else
        if (.not. isquiet) then
            print *, 'Lean in the tangential direction (theta).'
            write(*, *)
        end if
        write(nopen,*) 'Lean in the tangential direction (theta).'
        write(nopen,*) ''
    end if
    call close_log_file(nopen, file_open)



    !
    ! Write dimensional hub and casing streamlines 
    ! hubTipStreamline() in funcNsubs.f90
    !
    do i = 1, nsp(nsl)
        xt(i, 1)                            = xm(i, nsl)
        rt(i, 1)                            = rm(i, nsl)
    end do
    call hubTipStreamline(nsp(1), nsp(nsl), xm(1, 1), rm(1, 1), xt, rt)


    
    !
    ! Calculate m-prime coordinates using (x,r) coordinates as the 
    ! input for streamlines
    !
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) then
        write(*, *)
        print *, 'Using x_s and r_s coordinates for streamline from the input file...'
        write(*, *)
        print *, 'Calculating the m_prime coordinates for each streamline...'
        write(*, *)
    end if

    write(nopen, *)
    write(nopen, *) 'Using x_s and r_s coordinates for streamline from the input file...'
    write(nopen, *)
    write(nopen, *) 'Calculating the m_prime coordinates for each streamline...'
    write(nopen, *)
    call close_log_file(nopen, file_open)



    !
    ! Set flags for radial machines
    !
    do ia = 1, nsl
        rad_in_flag(ia)                     = 1
        rad_out_flag(ia)                    = 1
        
        do i = 2, nsp(ia)
            
            if (xm(i-1, ia) /= xm(i, ia)) then
                rad_in_flag(ia)             = 0
                rad_out_flag(ia)            = 0
                exit
            end if

        end do
    end do   ! ia

    do ia = 1, nsl

        if (rm(1, ia) > xm(nsp(ia), ia)) then
            rad_out_flag(ia)                = 0
        else if (rm(1, ia) < xm(nsp(ia), ia)) then
            rad_in_flag(ia)                 = 0
        end if

    end do   ! ia
        
    

    !
    ! TODO: What is happening here?
    !
    do ia = 1, nsl

        mp(1, ia)                           = 0.0
        k                                   = 0

        call log_file_exists(log_file, nopen, file_open)
        do i = 2, nsp(ia)

            if (rm(i, ia) < radius_tolerance) then

                k = k + 1   
                if (.not. isquiet) print *, 'Radius less than', radius_tolerance, ', excluding point number', k
                write(nopen,*) 'Radius less than', radius_tolerance, ', excluding point number', k
                xm(1, ia)                   = xm(i, ia)  
                rm(1, ia)                   = rm(i, ia)
                if (.not. isquiet) print *, 'xm(1, ia)', xm(1, ia), 'rm(1, ia)', rm(1, ia)
                write(nopen,*) 'xm(1, ia)', xm(1, ia), 'rm(1, ia)', rm(1, ia)

            else

                mp(i - k, ia)               = mp(i - k - 1, ia) + 2.0*sqrt((rm(i, ia) - rm(i - 1, ia))**2 + (xm(i, ia) - &
                                              xm(i - 1, ia))**2)/(rm(i, ia) + rm(i - 1, ia))
                xm(i - k, ia)               = xm(i, ia)   
                rm(i - k, ia)               = rm(i, ia)   

            end if

        end do  ! i = 2, nsp(ia)
        call close_log_file(nopen, file_open)

        call log_file_exists(log_file, nopen, file_open)
        if (k /= 0) then

            nsp_hub                         = nsp(ia)       
            nsp(ia)                         = i - k - 1         
            if (.not. isquiet) print *, 'nsp for streamline', ia, 'changed from', nsp_hub, 'to', nsp(ia)
            write(nopen,*) 'nsp for streamline', ia, 'changed from', nsp_hub, 'to', nsp(ia)

        end if  ! if (k /= 0)
        call close_log_file(nopen, file_open)

        ! Write dimensional streamline data to a file
        ! streamlines in file_operations.f90
        if (is_xyzstreamlines) then

            if ((ia >= 1) .and. (ia <= nsl)) then
                call streamlines(nsp(ia), xm(:, ia), rm(:, ia), ia) 
            end if

        end if  ! is_xyzstreamlines

    end do  ! ia = 1, nsl



    !
    ! Debugging: Create an interpolated xm rm for nonoffset hub streamline for plotting 
    !
    if (hub /= 0) then

        n_inter_intervals                   = 3  
        k                                   = 1
        xm_nonoffset_hub(k)                 = xm(1, 1)
        rm_nonoffset_hub(k)                 = rm(1, 1)

        if (allocated(hub_slope)) deallocate(hub_slope)
        allocate(hub_slope(nsp(1) - 1))

        ! Interpolate hub streamline xm rm
        do i = 1, nsp(1) - 1 

            hub_slope(i)                    = (rm(i + 1, 1) - rm(i, 1))/(xm(i + 1, 1) - xm(i, 1))

            do j = 1, n_inter_intervals

                k                           = j + (i - 1)*n_inter_intervals
                xm_nonoffset_hub(k + 1)     = xm_nonoffset_hub(k) + ((xm(i + 1, 1) - xm(i, 1))/(n_inter_intervals))
                rm_nonoffset_hub(k + 1)     = rm_nonoffset_hub(k) + (xm_nonoffset_hub(k + 1) - xm_nonoffset_hub(k))*hub_slope(i)
                nsp_interpolated_hub        = k + 1 

            end do  ! j = 1, n_inter_intervals

        end do  ! i = 1, nsp(1) - 1
            
        ! Calculate mp_nonoffset_hub
        mp_nonoffset_hub(1)                 = 0.0
        do i = 2, nsp_interpolated_hub
            mp_nonoffset_hub(i)             = mp_nonoffset_hub(i - 1) + 2.0*sqrt((rm_nonoffset_hub(i) - &
                                              rm_nonoffset_hub(i - 1))**2 + (xm_nonoffset_hub(i) -      &
                                              xm_nonoffset_hub(i - 1))**2)/(rm_nonoffset_hub(i) + rm_nonoffset_hub(i - 1))
        end do
            
        ! Splining
        ! spline in spline.f90
        call spline(nsp_interpolated_hub, xm_nonoffset_hub, xms_nonoffset_hub, mp_nonoffset_hub, 999.0, -999.0)
        call spline(nsp_interpolated_hub, rm_nonoffset_hub, rms_nonoffset_hub, mp_nonoffset_hub, 999.0, -999.0)
            
        ! Offseting the interpolated hub spline
        ! huboffset in funcNsubs.f90
        temp                                = 'interpolated'
        call huboffset(mp_nonoffset_hub, xm_nonoffset_hub, rm_nonoffset_hub, xms_nonoffset_hub, &
                       rms_nonoffset_hub, hub, nsp_interpolated_hub, scf, temp)
            
    end if  ! if (hub /= 0)
    


    !
    ! Splining the x, r coordinates of the streamlines (construction lines)
    ! to create mprime spline coefficients
    ! spl_discjoint in spline.f90
    !
    na                                      = nsl
    do ia = 1, na
        call spl_discjoint(xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia), 999.0, -999.0)
        call spl_discjoint(rm(1, ia), rms(1, ia), mp(1, ia), nsp(ia), 999.0, -999.0)
    end do



    !
    ! Calculating offsets for the streamlines if the offset is given as input
    !
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) then
        write(*, *)
        print *, 'hub offset:', hub
        print *, 'tip offset:', tip
        write(*, *)
    end if

    write(nopen,*) ''
    write(nopen,*) 'hub offset:', hub
    write(nopen,*) 'tip offset:', tip
    write(nopen,*)
    call close_log_file(nopen, file_open)

    ! Hub offset
    ! huboffset in funcNsubs.f90
    if (hub /= 0) then

        call huboffset(mphub(1, 1), xm(1, 1), rm(1, 1), xms(1, 1), rms(1, 1), hub, nsp(1), scf, casename)
        
        ! Offset mprime coordinates updated to the mprime array at hub and casing.
        do i = 1, nsp(1)
            mp(i, 1)                        = mphub(i, 1)
        end do

    end if  ! if (hub /= 0)


    ! Tip offset
    if (tip /= 0) then

        do i = 1, nsp(na)
            xt(i, 1)                        = xm(i, na)
            rt(i, 1)                        = rm(i, na)
            dxn(i, 1)                       = xms(i, na)
            drn(i, 1)                       = rms(i, na)
        end do  
       
        ! tipoffset in funcNsubs.f90 
        call tipoffset(mptip, xm(1, nsl), rm(1, nsl), xms(1, nsl), rms(1, nsl), tip, nsp(nsl), scf, nsl, casename)
        
        ! Offset mprime coordinates updated to the mprime array at hub and casing.
        do i = 1, nsp(na)
            mp(i, na)                       = mptip(i, 1)
        end do

    end if  ! if (tip /= 0)



    !
    ! Obtaining LE/TE x, r values on each streamline using the LE/TE curve and
    ! the streamline curve intersection 
    !
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) write(*, *)'xLE    rLE     xTE     rTE'
    write(nopen,*) 'xLE    rLE     xTE     rTE'
    write(nopen,*) ''
    if (.not. isquiet) print *, 'Calculating LE x, r points... '
    write(nopen,*) 'Calculating LE x, r points... '
    call close_log_file(nopen, file_open)

    ! LE curve intersection with the streamline curve
    do ia = 1, na

        s1le(ia)                            = mp(2, ia)
        s2le(1)                             = rle(1)
        
        if (ia == 1) then
            s2le(ia)                        = s2le(1)
        else
            s2le(ia)                        = s2le(ia - 1)
        end if

        ! spl_intersect in spline.f90
        call spl_intersect(ia, s1le(ia), s2le(ia), xm(1, ia), xms(1, ia), rm(1, ia), &
                           rms(1, ia), mp(1, ia), nsp(ia), xle(1), xles(1), rle(1),  &
                           rles(1), sle(1), npoints)

        ! spl_eval in spline.f90
        x_le(ia)                            = spl_eval(nsp(ia), s1le(ia), xm(1, ia), xms(1, ia), mp(1, ia))
        r_le(ia)                            = spl_eval(nsp(ia), s1le(ia), rm(1, ia), rms(1, ia), mp(1, ia))

    end do  ! ia = 1, na

    ! Print output to screen and write to log file
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) print *, 'Calculating TE x, r points...'
    write(nopen,*) ''
    write(nopen,*) 'Calculating TE x, r points...'
    call close_log_file(nopen, file_open)

    ! TE curve intersection with the streamline curve
    do ia = 1, na

        s1te(ia)                            = s1le(ia)
        s2te(1)                             = rte(1)
        
        if (ia == 1)then
            s2te(ia)                        = s2te(1)
        else
            s2te(ia)                        = s2te(ia - 1)
        end if

        ! spl_intersect in spline.f90
        call spl_intersect(ia,s1te(ia), s2te(ia), xm(1, ia), xms(1, ia), rm(1, ia), &
                           rms(1, ia), mp(1, ia), nsp(ia), xte(1), xtes(1), rte(1), &
                           rtes(1), ste(1), npoints)

        ! spl_eval in spline.f90
        x_te(ia)                            = spl_eval(nsp(ia), s1te(ia), xm(1, ia), xms(1, ia), mp(1, ia))
        r_te(ia)                            = spl_eval(nsp(ia), s1te(ia), rm(1, ia), rms(1, ia), mp(1, ia))

    end do  ! ia = 1, na

    ! write_LE_TE_intersection in file_operations.f90
    call write_LE_TE_intersection

    ! Store spanwise section radii at LE and TE in separate arrays
    if (allocated(sec_radius)) deallocate(sec_radius)
    allocate(sec_radius(nsl, 2))

    do ia = 1, na
        sec_radius(ia, 1) = r_le(ia)  
        sec_radius(ia, 2) = r_te(ia) 
    end do



    !
    ! Checking if r_slope > x_slope for non-axial machines
    !
    call log_file_exists(log_file, nopen, file_open)
    write(nopen,*) ''
    do ia = 1, na

        i_slope                             = 0
        
        do i = 1, nsp(ia)
            xm_slope                        = abs(xms(i, ia))
            rm_slope                        = abs(rms(i, ia))
            if (rm_slope >= xm_slope .and. i_slope == 0) &
                i_slope                     = i  
        end do  ! i = 1, nsp(ia)

        if (.not. isquiet) print *, 'i_slope', i_slope
        write(nopen,*) 'i_slope', i_slope

    end do  ! ia = 1, na
    call close_log_file(nopen, file_open)



    !
    ! Calculating msLE and msTE using LE, TE (x, r) and determining axial/radial/mixed flow
    !
    call log_file_exists(log_file, nopen, file_open)
    write(nopen,*) ''
    
    ! Obtaining initial guesss values for msle and mste to calculate the correct values
    do ia = 1, na

        xsle                                = x_le(ia)
        xste                                = x_te(ia)
        rsle                                = r_le(ia)
        rste                                = r_te(ia)
        ile                                 = 0
        ite                                 = 0

        ! Leading edge index 
        do i = 1, nsp(ia) - 1

            ii                              = i
            xi                              = xm(i, ia)
            ri                              = rm(i, ia)
            xi1                             = xm(i + 1, ia)
            ri1                             = rm(i + 1, ia)
            x1hub                           = xm(1, 1)
            x1tip                           = xm(1, na)
            r1hub                           = rm(1, 1)
            r1tip                           = rm(1, na)      

            ! Purely axial flow
            if (i_slope == 0) then   

                if ((xi1 >= xsle .and. xi <= xsle) .and. ile == 0) &
                    ile                     = i       

            ! Not purely axial flow
            else if (i_slope /= 0) then 

                ! Flow at LE
                if (ii <= i_slope) then 

                    ! Axial flow at LE
                    if ((r1tip > r1hub) .or. (r1tip < r1hub)) then

                        axial_LE            = .True.
                        if ((xi1 >= xsle .and. xi <= xsle) .and. ile == 0) &
                            ile             = i 

                    ! Radial flow at LE
                    else if ((x1hub < x1tip) .or. (x1hub > x1tip)) then 

                        radial_LE           = .True.                   
                        if (ri1 >= rsle .and. ri <= rsle .and. ile == 0) &
                            ile             = i  

                    end if

                end if  ! if (ii <= i_slope)      

            end if  ! i_slope   

        end do  ! i = 1, nsp(ia) - 1

        ! Print LE index to screen and write to log file
        if (.not. isquiet) print *, 'ile:', ile
        write(nopen,*) 'ile:', ile
        

        ! Trailing edge index 
        do i = 1, nsp(ia) - 1

            ii                              = i
            xi                              = xm(i, ia)
            ri                              = rm(i, ia)
            xi1                             = xm(i + 1, ia)
            ri1                             = rm(i + 1, ia)

            ! Purely axial flow
            if (i_slope == 0) then  

                if ((xi1 >= xste .and. xi <= xste) .and. ite == 0) &
                    ite                     = i      

            ! Not purely axial flow
            else if (i_slope /= 0) then 

                ! Flow at TE
                if (ii >= i_slope) then

                    ! Radial flow at TE
                    if ((r1tip > r1hub) .or. (r1tip < r1hub)) then 

                        radial_TE           = .True.
                        if ((ri1 >= rste .and. ri <= rste) .and. ite == 0) &
                            ite             = i

                    ! Axial flow at TE
                    else if ((x1hub < x1tip) .or. (x1hub > x1tip)) then 

                        axial_TE            = .True.
                        if ((xi1 >= xste .and. xi <= xste) .and. ite == 0) &
                            ite             = i              

                    end if

                end if  ! if (ii >= i_slope)       

            end if  ! i_slope

        end do  ! i = 1, nsp(ia) - 1

        ! Print TE index to screen and write to log file
        if (.not. isquiet) print *, 'ite:', ite
        write(nopen,*) 'ite:', ite

    end do  ! ia = 1, na
    call close_log_file(nopen, file_open)



    !
    ! Determining purely axial, purely radial, mixed flow
    !
    call log_file_exists(log_file, nopen, file_open)
    write(nopen,*)
    
    do ia = 1, na

        msle(ia)                            = s1le(ia)
        mste(ia)                            = s1te(ia)

        ! Purely axial flow
        if (i_slope == 0) then 

            if (.not. isquiet) then
                print *, 'Using x values for msLE due to a purely axial flow.'
                print *, 'Using x values for msTE due to a purely axial flow.'
            end if
            write(nopen,*) 'Using x values for msLE due to a purely axial flow.'
            write(nopen,*) 'Using x values for msTE due to a purely axial flow.'
         
         
            ! Evaluating span for an axial blade
            lref                            = abs(r_le(na) - r_le(1))
            span(ia)                        = abs(r_le(ia) - r_le(1))/real(lref)    

       else if (i_slope /= 0) then

            ! Axial flow at LE
            if (axial_LE) then 
         
                if (.not. isquiet) print *, 'Using x values for msLE due to axial flow at LE.'
                write(nopen,*) 'Using x values for msLE due to axial flow at LE.'
           
                ! Evaluating span for an axial blade
                lref                        = abs(r_le(na) - r_le(1))
                span(ia)                    = abs(r_le(ia) - r_le(1))/real(lref)
          
            ! Non-axial flow at LE 
            else if (radial_LE) then! non-axial flow at LE
         
                if (.not. isquiet) print *, 'Using r values for msLE due to non-axial flow at LE.'
                write(nopen,*) 'Using r values for msLE due to non-axial flow at LE.'
           
                ! Evaluating span for a non-axial blade
                xdiff                       = abs(x_le(na) - x_le(1))
                lref                        = xdiff
                span(ia)                    = abs(x_le(ia) - x_le(1))/real(lref)
           
            end if ! axial_LE/radial_LE

            ! Axial flow at TE
            if (axial_TE) then 
         
                if (.not. isquiet) print *, 'Using x values for msTE due to axial flow at TE.'
                write(nopen,*) 'Using x values for msTE due to axial flow at TE.'
    
            ! Non-axial flow at TE 
            else if (radial_TE) then 
         
                if (.not. isquiet) print *, 'Using r values for msTE due to non-axial flow at TE.'
                write(nopen,*) 'Using r values for msTE due to non-axial flow at TE.'

            end if ! axial_TE/radial_TE
         
        end if ! i_slope

        ! Non-dimensional chord
        chordm(ia)                           = abs(mste(ia) - msle(ia))

        if (.not. isquiet) then
            print *, 'msle:', msle(ia)
            print *, 'mste:', mste(ia)
            print *, 'chordm:', chordm(ia)
        end if

        write(nopen,*) 'msle:', msle(ia)
        write(nopen,*) 'mste:', mste(ia)
        write(nopen,*) 'chordm:',  chordm(ia)

    end do  ! ia = 1, na
    
    call close_log_file(nopen, file_open)



    !
    ! Calculating phi to adjust for BetaZ conversion
    !
    call log_file_exists(log_file, nopen, file_open)
    write(nopen,*) ''

    do ia = 1, na

        xmsle(ia)                            = 0.0
        rmsle(ia)                            = 0.0

        ! Calculating dx/dm' and dr/dm' value at LE of each streamline
        ! dspl_eval in spline.f90
        xmsle(ia)                            = dspl_eval(msle(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia))
        rmsle(ia)                            = dspl_eval(msle(ia), rm(1, ia), rms(1, ia), mp(1, ia), nsp(ia))

        ! Calculating dphi_s_in = dr/dx = (dr/dm')/(dx/dm') 
        phi_s_in(ia)                         = atan(rmsle(ia)/xmsle(ia))
        if (.not. isquiet) print *, 'phi_s_in(ia)', phi_s_in(ia)/dtor
        write(nopen,*) 'phi_s_in(ia)', phi_s_in(ia)/dtor

    end do  ! ia = 1, na

    do ia = 1, na

        xmste(ia)                           = 0.0
        rmste(ia)                           = 0.0

       ! Calculating dx/dm' and dr/dm' value at TE of each streamline
       ! dspl_eval in spline.f90
       xmste(ia)                            = dspl_eval(mste(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia))
       rmste(ia)                            = dspl_eval(mste(ia), rm(1, ia), rms(1, ia), mp(1, ia), nsp(ia))

       ! Calculating dphi_s_out = dr/dx = (dr/dm')/(dx/dm') 
       phi_s_out(ia)                        = atan(rmste(ia)/xmste(ia))
       if (.not. isquiet) print *, 'phi_s_out(ia)', phi_s_out(ia)/dtor
       write(nopen,*) 'phi_s_out(ia)', phi_s_out(ia)/dtor

    end do  ! ia = 1, na

    write(nopen,*) ''
    call close_log_file(nopen, file_open)



    !
    ! Create spanwise splines for inputs from spancontrolinputs file
    !
    if (control_inp_flag  ==  2) then
        call span_variation(nsl)
    end if



    !
    ! Inputs for 2D airfoil generation
    !
    nspn                                    = na
    do js = 1, nspn
       in_beta(js)                          = in_beta(js)
       out_beta(js)                         = out_beta(js)
       thk_c(js)                            = thk_c(js)
    end do



    !
    ! Control points for spanwise inlet Betaz
    !
    call log_file_exists(log_file, nopen, file_open)

    ! Inlet BetaZ defined as inlet flow angle
    if ((trim(anglespline) == 'inletspline') .or. (trim(anglespline) == 'inoutspline')) then

        if (.not. isquiet) then
            write(*, *)
            write(*, *)' Inlet Beta defined spanwise by a cubic B-spline using control points.'
            write(*, *)'   span         in_Beta (spline)'
        end if
        write(nopen,*) ''
        write(nopen,*) ' Inlet Beta defined spanwise by a cubic B-spline using control points.'
        write(nopen,*) '   span         in_Beta (spline)'

        ! Generate spanwise spline for inlet BetaZ
        ! cubicspline and cubicbspline_intersec in cubicspline.f90
        call cubicspline(cpinbeta, xcpinbeta, spaninbeta, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
        call cubicbspline_intersec(ncp1, xc, yc, na, span, inbeta_s, xbs, ybs, y_spl_end)

        ! Store in global array
        do ia = 1, na

            if (.not. isquiet) print *, span(ia), inbeta_s(ia)
            write(nopen,*) span(ia), inbeta_s(ia)
            in_beta(ia)                     = inbeta_s(ia)

        end do  ! ia = 1, na

    ! Inlet BetaZ defined as incidence
    else if (trim(anglespline) == 'inci_dev_spline') then

        if (.not. isquiet) then
            write(*, *)
            write(*, *)' Inlet Beta incidence defined spanwise by a cubic B-spline using control points.'
            write(*, *)'   span         in_Beta (spline)'
        end if
        write(nopen, *)
        write(nopen, *)' Inlet Beta incidence defined spanwise by a cubic B-spline using control points.'
        write(nopen, *)'   span         in_Beta (spline)'

        ! Generate spanwise spline for inlet BetaZ
        ! cubicspline and cubicbspline_intersec in cubicspline.f90
        call cubicspline(cpinbeta, xcpinbeta, spaninbeta, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
        call cubicbspline_intersec(ncp1, xc, yc, na, span, inci_s, xbs, ybs, y_spl_end)

        ! Store in global array
        do ia = 1, na

            temp_1                          = in_beta(ia)
            temp_2                          = inci_s(ia)
            in_beta(ia)                     = inBetaInci(in_beta(ia), inci_s(ia))
            if (.not. isquiet) print *, span(ia), in_beta(ia)
            write(nopen,*) span(ia), temp_1, temp_2, in_beta(ia)

        end do  ! ia = 1, na

    end if  ! anglespline
    call close_log_file(nopen, file_open)




    !
    ! Control points for panwise outlet BetaZ
    !
    call log_file_exists(log_file, nopen, file_open)

    ! Outlet BetaZ defined as outlet flow angle
    if ((trim(anglespline) == 'outletspline').or.(trim(anglespline) == 'inoutspline')) then

        if (.not. isquiet) then
            write(*, *)
            write(*, *)' Outlet Beta defined spanwise by a cubic B-spline using control points.'
            write(*, *)'   span        out_Beta (spline)'
        end if

        write(nopen, *)
        write(nopen, *)' Outlet Beta defined spanwise by a cubic B-spline using control points.'
        write(nopen, *)'   span        out_Beta (spline)'

        ! Spanwise spline for outlet BetaZ
        ! cubicspline and cubicbspline_intersec in spline.f90
        call cubicspline(cpoutbeta, xcpoutbeta, spanoutbeta, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
        call cubicbspline_intersec(ncp1, xc, yc, na, span, outbeta_s, xbs, ybs, y_spl_end)

        ! Store in global array
        do ia = 1, na
            if (.not. isquiet) print *, span(ia), outbeta_s(ia)
            write(nopen,*) span(ia),  outbeta_s(ia)
            out_beta(ia)                    = outbeta_s(ia)     
        end do  ! ia = 1, na

    ! Outlet BetaZ defined as deviation
    else if (trim(anglespline) == 'inci_dev_spline') then

        if (.not. isquiet) then
            write(*, *)
            write(*, *)' Outlet Beta deviation defined spanwise by a cubic B-spline using control points.'
            write(*, *)'   span        out_Beta (spline)'
        end if

        write(nopen, *)
        write(nopen, *)' Outlet Beta deviation defined spanwise by a cubic B-spline using control points.'
        write(nopen, *)'   span        out_Beta (spline)'

        ! Spanwise spline for outlet BetaZ
        ! cubicspline and cubicbspline_intersec in spline.f90
        call cubicspline(cpoutbeta, xcpoutbeta, spanoutbeta, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
        call cubicbspline_intersec(ncp1, xc, yc, na, span, dev_s, xbs, ybs, y_spl_end)

        ! Store in global array
        do ia = 1, na

            temp_1                          = out_beta(ia)
            temp_2                          = dev_s(ia)
            out_beta(ia)                    = outBetaDevn(in_beta(ia), out_beta(ia), dev_s(ia))
            if (.not. isquiet) print *, span(ia), out_beta(ia)
            write(nopen,*) span(ia), temp_1, temp_2, out_beta(ia)

        end do  ! ia = 1, na

    end if  ! anglespline
    call close_log_file(nopen, file_open)




    !
    ! Control points for chord multiplier
    !
    if (chord_switch == 2) then   

        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) then
            write(*, *)
            write(*, *)'   span        chord_multipliers'
        end if
        write(nopen,*) ''
        write(nopen,*) '   span        chord_multipliers'

        ! Spanwise spline for chord multiplier
        ! cubicspline and cubicbspline_intersec in spline.f90
        call cubicspline(cpchord, xcpchord, spanchord, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
        call cubicbspline_intersec(ncp1, xc, yc, na, span, chords, xbs, ybs, y_spl_end)
        
        ! Print spline data to screen and write to file
        do ia = 1, na
            if (.not.isquiet) print *, span(ia), chords(ia)
            write(nopen,*) span(ia), chords(ia)
        end do  
        call close_log_file(nopen, file_open)

    end if  ! chord_switch



    !
    ! Control points for stagger
    !
    staggspline                             = in_beta(1)
    if (staggspline == 999.0) then

        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) then
            write(*, *)
            write(*, *)' Stagger defined spanwise by a cubic B-spline using control points.'
            write(*, *)'   span        stagger'
        end if

        write(nopen, *)
        write(nopen, *)' Stagger defined spanwise by a cubic B-spline using control points.'
        write(nopen, *)'   span        stagger'

        ! Spanwise spline for stagger
        ! cubicspline and cubicbspline_intersec in spline.f90
        call cubicspline(cpinbeta, xcpinbeta, spaninbeta, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
        call cubicbspline_intersec(ncp1, xc, yc, na, span, inbeta_s, xbs, ybs, y_spl_end)

        ! Print spline data to screen and write to file
        do ia = 1, na
           if (.not. isquiet) print *, span(ia), inbeta_s(ia)
           write(nopen,*) span(ia), inbeta_s(ia)
        end do
        call close_log_file(nopen, file_open)

    end if  ! staggspline



    !
    ! Control points for tm/c spline multiplier
    !
    if (tm_c_spline)then

        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) then
            write(*, *)
            write(*, *)' tm/c thickness ratio defined radially with 2D spline using control points.'
        end if
        write(nopen,*) ''
        write(nopen,*) 'tm/c thickness ratio defined radially with 2D spline using control points.'

        ! Spanwise spline for thickness multiplier
        ! cubicspline and cubicbspline_intersec in spline.f90
        call cubicspline(cptm_c, xcptm_c, spantm_c, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
        call cubicbspline_intersec(ncp1, xc, yc, na, span, thk_tm_c_spl, xbs, ybs, y_spl_end)

        ! Print spline data to screen and write to file
        do ia = 1, na
           if (.not. isquiet) print *, span(ia), thk_tm_c_spl(ia)
           write(nopen,*) span(ia), thk_tm_c_spl(ia)
        end do

    end if  ! tm_c_spline



    !
    ! Control points for max thickness location (umxthk_all) spline
    !
    if (u_max_spline) then
        if (allocated(um_spl)) deallocate(um_spl)
        allocate(um_spl(na))

        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) then
            write(*,*) '' 
            write(*,*) 'Max thickness location defined spanwise by a cubic B-spline using control points'
        end if
        write(nopen,*) ''
        write(nopen,*) 'Max thickness location defined spanwise by a cubic B-spline using control points'

        ! Spanwise spline for max thickness location
        ! cubicspline and cubicbspline_intersec in spline.f90
        call cubicspline(cptm_c, xcpumax, spantm_c, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
        call cubicbspline_intersec(ncp1, xc, yc, na, span, um_spl, xbs, ybs, y_spl_end)
        umxthk_all                          = um_spl

        ! Print spline data to screen and write to file
        do ia = 1, na
            if (.not. isquiet) print *, span(ia), umxthk_all(ia)
            write(nopen,*) span(ia), umxthk_all(ia)
        end do
        call close_log_file(nopen, file_open)

    end if  ! u_max_spline



    !
    ! Allocating variables for further calculations
    !
    if (allocated(bladedata     )) deallocate(bladedata     )
    allocate(bladedata(amount_data, nsl))
    if (allocated(bladedata_before)) deallocate(bladedata_before)
    allocate(bladedata_before(amount_data, nsl))
    if (allocated(intersec_coord)) deallocate(intersec_coord)
    allocate(intersec_coord(12, nsl))
    if (allocated(throat_3D     )) deallocate(throat_3D     )
    allocate(throat_3D(nsl))
    if (allocated(mouth_3D      )) deallocate(mouth_3D      )
    allocate(mouth_3D(nsl))
    if (allocated(exit_3D       )) deallocate(exit_3D       )
    allocate(exit_3D(nsl))
    if (allocated(throat_pos    )) deallocate(throat_pos    )
    allocate(throat_pos(nsl))
    if (allocated(throat_index  )) deallocate(throat_index  )
    allocate(throat_index(nsl))

    

    !
    ! Array to store spanwise thickness data for the Wennerstrom thickness distribution
    if (allocated(spanwise_thk)) deallocate(spanwise_thk)
    allocate(spanwise_thk(nspn))
    spanwise_thk                            = 0.0



    !
    ! Array to store (m',theta) coordinates of points associated
    ! with maximum thickness location for all spanwise sections
    !
    if (allocated(mt_umax)) deallocate(mt_umax)
    allocate(mt_umax(nspn, 4))
    mt_umax                                 = 0.0



    !
    ! 2D airfoil generation
    !
    do js = 1, nspn

        mles                                = msle(js)
        mtes                                = mste(js)
        mr1                                 = mrel1(js)
        stak_u                              = stk_u(js)
        stak_v                              = stk_v(js)



        !
        ! betaZ-betaM conversion
        !
        ! All axial: Converting Beta_z to Beta_m ;tanBetaM = tanBetaz* cos(phi)
        if (beta_switch == 0) then 

            sinl                            = tan(in_beta(js)*dtor)*cos(phi_s_in(js))
            sext                            = tan(out_beta(js)*dtor)*cos(phi_s_out(js))

        ! All radial: Converting Beta_r to Beta_m {tanBetaM = tanBetaR*sin(phi)}
        else if (beta_switch == 1) then 

            sinl                            = tan(in_beta(js)*dtor)*sin(phi_s_in(js))
            sext                            = tan(out_beta(js)*dtor)*sin(phi_s_out(js))

        ! AxailIN-RadialOUT: Converting Beta_r to Beta_m {tanBetaM = tanBetaR*sin(phi)}
        else if (beta_switch == 2) then 

            sinl                            = tan(in_beta(js)*dtor)*cos(phi_s_in(js))
            sext                            = tan(out_beta(js)*dtor)*sin(phi_s_out(js))
        
        ! RadialIN-AxialOUT: Converting Beta_r to Beta_m {tanBetaM = tanBetaR*sin(phi)}
        else if (beta_switch == 3) then 
            sinl                            = tan(in_beta(js)*dtor)*sin(phi_s_in(js))
            sext                            = tan(out_beta(js)*dtor)*cos(phi_s_out(js))

        end if  ! beta_switch     



        !
        ! Chord switches
        !
        call log_file_exists(log_file, nopen, file_open)
        write(nopen,*) ''

        ! Non-dimensional actual chord
        if (chord_switch == 1) then

            if (.not. isquiet) print *, 'Non-dimensional chord from the input...'
            write(nopen,*) 'Non-dimensional chord from the input...'
            chrdx                           = chord(js)
            axchrd(js)                      = chord(js)

        ! Internal chord
        else if (chord_switch == 0) then

            if (.not. isquiet) print *, 'Internally calculated chord...'
            write(nopen,*) 'Internally calculated chord...'
            chrdx                           = chordm(js)
            axchrd(js)                      = chord(js)

        ! Chord with spanwise chord multiplier
        else if (chord_switch == 2) then

            if (.not. isquiet) print *, 'Chord multiplier calculated using spline control points...'
            write(nopen,*) 'Chord multiplier calculated using spline control points...'
            chrdx                           = chordm(js) * chords(js)
            axchrd(js)                      = chord(js)

        end if  ! chord_switch 
        call close_log_file(nopen, file_open)



        !
        ! Stagger switches
        !
        call log_file_exists(log_file, nopen, file_open)

        ! Stagger from spline control table
        if (staggspline == 999.)then 
            stgr                            = inbeta_s(js)

        ! Stagger from the angles table
        else if (chord_switch /= 0)then
            stgr                            = in_beta(js) 

        ! Zero stagger
        else
            stgr                            = 0.
            if (.not. isquiet) print *, 'Stagger calculated from the inlet and exit angles...'
            write(nopen,*) 'Stagger calculated from the inlet and exit angles...'

        end if 
        call close_log_file(nopen, file_open)



        ! LE input
        if (allocated(sting_l_all)) deallocate(sting_l_all)
        if (LE /= 2) then
            if (allocated(sting_l_all)) deallocate(sting_l_all)
            allocate(sting_l_all(nsl))
            sting_l_all(1:nsl) = 0.
        end if
      


        !
        ! Thickness multiplier switches
        !
        call log_file_exists(log_file, nopen, file_open)
        if (tm_c_spline .and. thick_distr /= 0) then
            if (.not. isquiet) print *, 'Thickness t/c will be multiplied by tm/c 2D spline definition...'
            write(nopen,*) 'Thickness t/c will be multiplied by tm/c 2D spline definition...'
            thkc                            = thk_c(js)*thk_tm_c_spl(js)
        else
            thkc                            = thk_c(js)
        end if
        call close_log_file(nopen, file_open)



        !
        ! Array definitions for global grid variables
        !
        if (js == 1) then
             
            if (allocated(mblade_grid)) deallocate(mblade_grid)
            allocate(mblade_grid(nspn,500))
            if (allocated(thblade_grid)) deallocate(thblade_grid)
            allocate(thblade_grid(nspn,500))

            if (allocated(m_ext_mean)) deallocate(m_ext_mean)
            allocate(m_ext_mean(nspn, 200))
            if (allocated(th_ext_mean)) deallocate(th_ext_mean)
            allocate(th_ext_mean(nspn, 200))

        end if



        !
        ! 2D blade section generation routine
        ! bladegen in bladegen.f90
        !
        call bladegen(nspn,thkc,mr1,sinl,sext,chrdx,js,blext(js),xcen,ycen,airfoil(js),stgr,stack,chord_switch,    &
                      stak_u,stak_v,xb_stk,yb_stk,stack_switch,clustering_switch,clustering_parameter,nsl,nbls,    &
                      curv,thick,LE,np,ncp_curv,ncp_thk,curv_cp,thk_cp, wing_flag, lethk_all,tethk_all,s_all,      &
                      ee_all,thick_distr,umxthk_all, C_le_x_top_all,C_le_x_bot_all,C_le_y_top_all,C_le_y_bot_all,  &
                      LE_vertex_ang_all,LE_vertex_dis_all,sting_l_all,sting_h_all,LEdegree,no_LE_segments,         &
                      sec_radius,bladedata,amount_data,scf,intersec_coord,throat_index,n_normal_distance,casename, &
                      develop,mble,mbte,mles,mtes,i_slope,jcellblade_all,etawidth_all,BGgrid_all,thk_tm_c_spl,     &
                      theta_offset,TE_der_actual,TE_der_norm, mblade_grid(js,:),thblade_grid(js,:),spanwise_thk,   &
                      n_ext,m_ext_mean(js,:),th_ext_mean(js,:),mt_umax(js,:))



        ! Store 2D quantities in global variables
        mprime_ble(js)                      = mble
        mprime_bte(js)                      = mbte
        xcg(js)                             = xcen
        ycg(js)                             = ycen
        xb_stack(js)                        = xb_stk
        yb_stack(js)                        = yb_stk

        if (.not. isquiet) write(*, *)
        
        stagger(js)                         = stgr

        ! Deallocate LE input array
        if (curv == 0 .and. LE /= 2) then
          deallocate(sting_l_all)
        end if

    end do  ! js = 1, nspn

    !if (is2d) then
    !    !goto 1001
    !end if



    !
    ! Number of points along the (m',theta) extended meanlines
    ! computed in bladegen
    !
    n_ext_mean                              = ((np + 1)/2) + (2 * (n_ext - 1))


   
    !
    ! Stacking 2D blade sections to create 3D blade
    !
    bladedata_before                        = bladedata

    ! Allocate and initialize array to store the dimensional
    ! TE, maximum and LE thicknesses
    if (allocated(dim_thick)) deallocate(dim_thick)
    allocate(dim_thick(nspn,3))
    dim_thick                               = 0.0

    do js = 1, nrow 

        nsec                                = nspn

        ! bladestack in bladestack.f90
        call bladestack(nspn, x_le, x_te, r_le, r_te, nsec, scf, msle, np, stack,                         &
                        cpdeltam, spanmp, xcpdelm, cpdeltheta, spantheta, xcpdeltheta,                    &
                        cpinbeta, spaninbeta, xcpinbeta, cpoutbeta, spanoutbeta, xcpoutbeta,              &
                        xm, rm, xms, rms, mp, nsp, bladedata, amount_data, intersec_coord,                &
                        throat_3D, mouth_3D, exit_3D, casename, nbls, LE, axchrd, mprime_ble,             &
                        mprime_bte, units, stagger, chrdsweep, chrdlean, axial_LE, radial_LE,thick_distr, &
                        transpose(mblade_grid(:,1:np)), transpose(thblade_grid(:,1:np)), n_ext_mean,      &
                        m_ext_mean(:,1:n_ext_mean), th_ext_mean(:,1:n_ext_mean), mt_umax,                 &
                        clustering_switch, clustering_parameter, dim_thick)

    end do  ! js = 1, nrow



    !
    ! Inflate hub section
    ! Required for enabling flends in ESP
    !
    if (hub_inflate .and. hub_inf_offset > 10E-8) then
        
        if (allocated(mhub_inf) .and. allocated(thhub_inf)) &
            deallocate(mhub_inf, thhub_inf)
        allocate(mhub_inf(np), thhub_inf(np))

        if (allocated(xhub_inf) .and. allocated(yhub_inf) .and. allocated(zhub_inf)) &
            deallocate(xhub_inf, yhub_inf, zhub_inf)
        allocate(xhub_inf(np), yhub_inf(np), zhub_inf(np))

        ! inflate_mprime_theta_section and get_inflated_3D_section 
        ! in funcNsubs.f90
        call inflate_mprime_theta_section(1, mhub_inf, thhub_inf)
        call get_inflated_3D_section(1, mhub_inf, thhub_inf, xhub_inf, yhub_inf, zhub_inf)

    end if



    !
    ! Inflate tip section
    ! Required for enabling flends in ESP
    !
    if (tip_inflate .and. tip_inf_offset > 10E-8) then
        
        if (allocated(mtip_inf) .and. allocated(thtip_inf)) &
            deallocate(mtip_inf, thtip_inf)
        allocate(mtip_inf(np), thtip_inf(np))

        if (allocated(xtip_inf) .and. allocated(ytip_inf) .and. allocated(ztip_inf)) &
            deallocate(xtip_inf, ytip_inf, ztip_inf)
        allocate(xtip_inf(np), ytip_inf(np), ztip_inf(np))

        ! inflate_mprime_theta_section and get_inflated_3D_section 
        ! in funcNsubs.f90
        call inflate_mprime_theta_section(21, mtip_inf, thtip_inf)
        call get_inflated_3D_section(21, mtip_inf, thtip_inf, xtip_inf, ytip_inf, ztip_inf)

    end if

    

    !
    ! Blade volume calculation
    ! Approximate method to calculate blade volume: strip average area * strip height
    !
    do i = 1, nsl - 1
         bladedata(amount_data, i)          = (bladedata(9, i) + bladedata(9, i + 1) + sqrt(bladedata(9, i) *  &
                                               bladedata(9, i + 1))) * (scf*((sum(sec_radius(i + 1, :))/2.0) - &
                                               (sum(sec_radius(i, :))/2.0))/3.0)
    end do
    bladedata(amount_data, nsl)             = sum(bladedata(amount_data, 1:nsl - 1))



    call log_file_exists(log_file, nopen, file_open)
    write(nopen,*) ''
    if (.not. isquiet) print *, (np+1)/2
    write(nopen,*) (np + 1)/2
    call close_log_file(nopen, file_open)



    !
    ! Writing the 3D nondimensional throat, span, in_beta & out_beta
    !
    do js = 1, nsl

        ! Add some variables to bladedata array
        bladedata(1, js)                    = span(js)
        bladedata(2, js)                    = in_beta(js)
        bladedata(3, js)                    = out_beta(js)

        ! throatindex in funcNsubs.f90
        call throatindex(throat_pos, throat_index, n_normal_distance, js, nsl, thick_distr)

        ! Dimensional throat
        bladedata(12, js) = throat_3D(js)*scf 

    end do



    !
    ! Write the bladedata containing throat and other info to a file
    ! outputfiledata in file_operations.f90
    !
    write_csv = .false.
    if (thick_distr == 5) then
        if (clustering_switch == 4) then
            write_csv = .true.
        end if
    end if
    call outputfiledata(casename, units, nsl, amount_data, bladedata, throat_pos, chordm, write_csv, dim_thick)

    !
    ! Write dimensional 3D blade coordinates to separate files
    ! write_3D_section_files in file_operations.f90
    !
    call write_3D_section_files()!(scf, fext, ibrowc, nbls, casename)



    !
    ! Writing pitch and chord non-dimensional values to a file
    ! cascade_nondim_file in file_operations.f90
    !
    pitch                                   = 2*pi/nbls
    call cascade_nondim_file(nsl, ibrow, msle, mste, mprime_ble, mprime_bte, chordm, pitch, casename)


end subroutine bgb3d_sub
!----------------------------------------------------------------------------------------------------------





















