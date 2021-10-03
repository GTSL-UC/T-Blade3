!
! This subroutine reads the main input file (3dbgbinput.bladerow.dat)
! Stores input in several global variables defined in globvar.f90
!
! Input parameters: fname - name of main input file
! 
!------------------------------------------------------------------------------------------------------------
subroutine readinput(fname)
    use globvar
    use file_operations
    use errors
    use funcNsubs
    implicit none

    character(256),                 intent(in)          :: fname
    
    ! Local variables
    character(256)                                      :: temp, temp1, temp2, beta_switch_2
    character(:),   allocatable                         :: log_file, error_msg, warning_msg, warning_msg_1, dev_msg, temp_str, &
                                                           master_ID, develop_ID
    integer                                             :: er, stat, n_temp1, n_temp2, &
                                                           nopen, nopen1, itm_c, iumax
    real                                                :: temp_offsets(2), temp_hub_offset, &
                                                           temp_tip_offset
    real,           allocatable                         :: temp_in(:)
    real,           parameter                           :: tol = 1E-8
    logical                                             :: equal, beta_value(5), ang_spl_value(5), file_open, &
                                                           file_open_1, read_tm_c_then_u_max = .false., &
                                                           read_u_max_then_tm_c = .false., read_tm_c_only = .false., &
                                                           read_u_max_only = .false., zero_u_max = .false., &
                                                           grid_gen_present = .false., isquiet_local


    !
    ! Allocate variables
    !
    if (allocated(x_le)) deallocate(x_le)
    allocate(x_le(nspan))
    if (allocated(x_te)) deallocate(x_te)
    allocate(x_te(nspan))
    if (allocated(r_le)) deallocate(r_le)
    allocate(r_le(nspan))
    if (allocated(r_te)) deallocate(r_te)
    allocate(r_te(nspan))
    if (allocated(in_beta)) deallocate(in_beta)
    allocate(in_beta(nspan)) 
    if (allocated(out_beta)) deallocate(out_beta)
    allocate(out_beta(nspan))
    if (allocated(mrel1)) deallocate(mrel1)
    allocate(mrel1(nspan))
    if (allocated(chord)) deallocate(chord)
    allocate(chord(nspan))
    if (allocated(thk_c)) deallocate(thk_c)
    allocate(thk_c(nspan))
    if (allocated(inci)) deallocate(inci)
    allocate(inci(nspan))
    if (allocated(devn)) deallocate(devn)
    allocate(devn(nspan))
    if (allocated(sec_flow_ang)) deallocate(sec_flow_ang)
    allocate(sec_flow_ang(nspan))
    if (allocated(phi_s_in)) deallocate(phi_s_in)
    allocate(phi_s_in(nspan))
    if (allocated(phi_s_out)) deallocate(phi_s_out)
    allocate(phi_s_out(nspan))
    if (allocated(stagger)) deallocate(stagger)
    allocate(stagger(nspan))
    if (allocated(chordm)) deallocate(chordm)
    allocate(chordm(nspan))
    if (allocated(msle)) deallocate(msle)
    allocate(msle(nspan))
    if (allocated(s1le)) deallocate(s1le)
    allocate(s1le(nspan))
    if (allocated(s2le)) deallocate(s2le)
    allocate(s2le(nspan))
    if (allocated(s1te)) deallocate(s1te)
    allocate(s1te(nspan))
    if (allocated(s2te)) deallocate(s2te)
    allocate(s2te(nspan))
    if (allocated(sang)) deallocate(sang)
    allocate(sang(nspan))
    if (allocated(stk_u)) deallocate(stk_u)
    allocate(stk_u(nspan))
    if (allocated(stk_v)) deallocate(stk_v)
    allocate(stk_v(nspan))
    if (allocated(total_camber)) deallocate(total_camber)
    allocate(total_camber(nspan))
    if (allocated(mprime_ble)) deallocate(mprime_ble)
    allocate(mprime_ble(nspan))
    if (allocated(mprime_bte)) deallocate(mprime_bte)
    allocate(mprime_bte(nspan))
    if (allocated(BGgrid_all)) deallocate(BGgrid_all)
    allocate(BGgrid_all(nspan))
    if (allocated(jcellblade_all)) deallocate(jcellblade_all)
    allocate(jcellblade_all(nspan))
    if (allocated(etawidth_all)) deallocate(etawidth_all)
    allocate(etawidth_all(nspan))
    if (allocated(axchrd)) deallocate(axchrd)
    allocate(axchrd(nspan))



    !
    ! Determine if 'quiet' command line argument is being used
    !
    call get_quiet_status(isquiet_local)



    !
    ! Open main input file and start reading
    ! 
    open(1, file = fname, status = 'unknown')
    rewind(1)



    ! log_file_exists() in file_operation.f90
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet_local) print*, fname
    write(nopen,*) ''
    write(nopen,*) fname

    ! close_log_file() in file_operations.f90
    call close_log_file(nopen, file_open)



    !
    ! Start writing a log file based on the main input file
    ! open_maininput_log_file() in file_operations.f90
    !
    call open_maininput_log_file(trim(adjustl(fname)), nopen1, file_open_1)


   
    ! 
    ! temp used to read descriptor lines in main input file
    !
    read(1, '(A)')temp
    write(nopen1,'(A)') trim(temp)


   
    ! 
    ! Read the casename
    !
    read(1, *)fext
    backspace(1)
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    casename = trim(fext)
    
    read(1, '(A)')temp
    write(nopen1,'(A)') trim(temp)


   
    ! 
    ! Read bladerow type
    !
    read(1, *)ibrow
    backspace(1)
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    write(ibrowc, '(i3)')ibrow
    
    read(1, '(A)')temp
    write(nopen1,'(A)') trim(temp)


   
    !
    ! Read number of blades
    !
    read(1, *) nbls 
    backspace(1)
    read(1,'(A)') temp
    write(nopen1, '(A)') trim(temp)


    
    !
    ! Read units of blade scaling factor
    !
    read(1, '(A)') temp
    write(nopen1,'(A)') trim(temp)
    units = temp(24:25)

    ! Invalid unit for the blade scaling factor
    if (units /= 'mm' .and. units /= 'cm' .and. units /= 'm)' .and. units /= 'm ') then
        error_msg   = 'Incorrect units for blade scaling factor'
        warning_msg = 'Only mm, cm or m can be specified'
        dev_msg     = 'Check subroutine readinput in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)
    end if



    !
    ! Read blade scaling factor
    !
    read(1, *)scf, temp
    write(nopen1,*) scf
    temp = adjustl(trim(temp))
    read(temp, *, iostat = er) theta_offset
    if (er /= 0) then
        theta_offset = 0.
        rewind(1)
        do i = 1, 8
            read(1, *) temp
        enddo
    endif


   
    ! 
    ! Read number of streamlines
    !
    read(1, '(A)')temp
    read(1, *)nsl
    write(nopen1, '(A)') trim(temp)
    write(nopen1, *) nsl
    
    read(1, '(A)')temp
    write(nopen1, '(A)') trim(temp)



    !
    ! Initialize secondary argument for angle spanwise splines
    !
    spanwise_angle_spline = .False.
    spanwise_inci_dev_spline = .False.

    !
    ! Read the input file angle switch 
    ! Also read secondary argument for angle spanwise splines if present 
    !
    read(1, '(A)') beta_switch_2
    write(nopen1, '(A)') trim(beta_switch_2)

    !
    ! All possible valid inputs are stored as logical variables in an array
    !
    beta_value  = [index(beta_switch_2, '0') /= 0, index(beta_switch_2, '1') /= 0, index(beta_switch_2, '2') /= 0, &
                   index(beta_switch_2, '3') /= 0, index(beta_switch_2, '4') /= 0]

    !
    ! Check for all possible valid inputs of the input angle switch
    !
    ! Case 1 - All AXIAL angles
    n_temp1     = 0
    if (beta_value(1) .and. .not. beta_value(2) .and. .not. beta_value(3) .and. .not. beta_value(4) .and. .not. beta_value(5)) then
        n_temp1 = index(beta_switch_2, '0')
        read(beta_switch_2(n_temp1:n_temp1 + 1),*,iostat=stat) beta_switch

    ! Case 2 - All RADIAL angles    
    elseif (.not. beta_value(1) .and. beta_value(2) .and. .not. beta_value(3) .and. .not. beta_value(4) .and. .not. beta_value(5)) then
        n_temp1 = index(beta_switch_2, '1')
        read(beta_switch_2(n_temp1:n_temp1 + 1),*,iostat=stat) beta_switch

    ! Case 3 - AXIAL inlet angles and RADIAL outlet angles
    elseif (.not. beta_value(1) .and. .not. beta_value(2) .and. beta_value(3) .and. .not. beta_value(4) .and. .not. beta_value(5)) then
        n_temp1 = index(beta_switch_2, '2')
        read(beta_switch_2(n_temp1:n_temp1 + 1),*,iostat=stat) beta_switch

    ! Case 4 - RADIAL inlet angles and AXIAL outlet angles
    elseif (.not. beta_value(1) .and. .not. beta_value(2) .and. .not. beta_value(3) .and. beta_value(4) .and. .not. beta_value(5)) then
        n_temp1 = index(beta_switch_2, '3')
        read(beta_switch_2(n_temp1:n_temp1 + 1),*,iostat=stat) beta_switch

    ! Case 5 - Wing flag is turned on
    elseif (.not. beta_value(1) .and. .not. beta_value(2) .and. .not. beta_value(3) .and. .not. beta_value(4) .and. beta_value(5)) then
        beta_switch = 0
        wing_flag   = 1

    ! Case 6 - Invalid input for the input angle switch
    !          Warn user and stop execution
    elseif (.not. beta_value(1) .and. .not. beta_value(2) .and. .not. beta_value(3) .and. .not. beta_value(4) .and. .not. beta_value(5)) then

        error_msg   = 'Invalid argument for beta_switch'
        warning_msg = 'Valid arguments are 0, 1, 2, 3 and 4 (refer to T-Blade3 documentation)'
        dev_msg     = 'Check subroutine readinput in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)

    endif

    !
    ! All possible valid secondary inputs are stored as logical variables in an array
    !
    ang_spl_value   = [len(beta_switch_2(:n_temp1)) == len(trim(beta_switch_2)), &
                       index(beta_switch_2, 'inletspline') /= 0, index(beta_switch_2, 'outletspline') /= 0, &
                       index(beta_switch_2, 'inoutspline') /= 0, index(beta_switch_2, 'inci_dev_spline') /= 0]


    call log_file_exists(log_file, nopen, file_open)

    !
    ! Check for all possible valid inputs of the secondary argument
    !
    !
    ! Case 1 - No splining required
    if (ang_spl_value(1) .and. .not. ang_spl_value(2) .and. .not. ang_spl_value(3) .and. .not. ang_spl_value(4) .and.     &
        .not. ang_spl_value(5)) then
        read(1,'(A)') temp

    ! Case 2 - spline inlet angles only   
    elseif (.not. ang_spl_value(1) .and. ang_spl_value(2) .and. .not. ang_spl_value(3) .and. .not. ang_spl_value(4) .and. &
            .not. ang_spl_value(5)) then

        n_temp2 = index(beta_switch_2, 'inletspline')
        anglespline = trim(beta_switch_2(n_temp2:))
        spanwise_angle_spline = .true.

        if (.not. isquiet_local) then
            print *, ''
            print *, 'Angles defined spanwise as a B-spline using control points'
            print *, ''
            print *, trim(anglespline)
        end if
        write(nopen,*) ''
        write(nopen,*) 'Angles defined spanwise as a B-spline using control points'
        write(nopen,*) ''
        write(nopen,*) trim(anglespline)
        read(1,'(A)') temp

    ! Case 3 - spline outlet angles only
    elseif (.not. ang_spl_value(1) .and. .not. ang_spl_value(2) .and. ang_spl_value(3) .and. .not. ang_spl_value(4) .and. &
            .not. ang_spl_value(5)) then

        n_temp2 = index(beta_switch_2, 'outletspline')
        anglespline = trim(beta_switch_2(n_temp2:))
        spanwise_angle_spline = .true.

        if (.not. isquiet_local) then
            print *, ''
            print *, 'Angles defined spanwise as a B-spline using control points'
            print *, ''
            print *, trim(anglespline)
        end if
        write(nopen,*) ''
        write(nopen,*) 'Angles defined spanwise as a B-spline using control points'
        write(nopen,*) ''
        write(nopen,*) trim(anglespline)
        read(1,'(A)') temp

    ! Case 4 - spline inlet and outlet angles
    elseif (.not. ang_spl_value(1) .and. .not. ang_spl_value(2) .and. .not. ang_spl_value(3) .and. ang_spl_value(4) .and. &
            .not. ang_spl_value(5)) then

        n_temp2 = index(beta_switch_2, 'inoutspline')
        anglespline = trim(beta_switch_2(n_temp2:))
        spanwise_angle_spline = .true.

        if (.not. isquiet_local) then
            print *, ''
            print *, 'Angles defined spanwise as a B-spline using control points'
            print *, ''
            print *, trim(anglespline)
        end if
        write(nopen,*) ''
        write(nopen,*) 'Angles defined spanwise as a B-spline using control points'
        write(nopen,*) ''
        write(nopen,*) trim(anglespline)
        read(1,'(A)') temp

    ! Case 5 - spline incidence and deviation
    elseif (.not. ang_spl_value(1) .and. .not. ang_spl_value(2) .and. .not. ang_spl_value(3) .and. .not. ang_spl_value(4) &
            .and. ang_spl_value(5)) then

        n_temp2 = index(beta_switch_2, 'inci_dev_spline')
        anglespline = trim(beta_switch_2(n_temp2:))
        spanwise_inci_dev_spline = .true.

        if (.not. isquiet_local) then
            print *, ''
            print *, 'Incidence and Deviation defined spanwise as a B-spline using control points'
            print *, ''
            print *, trim(anglespline)
        end if
        write(nopen,*) ''
        write(nopen,*) 'Incidence and Deviation defined spanwise as a B-spline using control points'
        write(nopen,*) ''
        write(nopen,*) trim(anglespline)
        read(1,'(A)') temp

    ! Case 6 - invalid input
    !          warn user and stop execution
    elseif (.not. ang_spl_value(1) .and. .not. ang_spl_value(2) .and. .not. ang_spl_value(3) .and. .not. ang_spl_value(4) &
            .and. .not. ang_spl_value(5)) then

        error_msg   = 'Invalid argument for anglespline'
        warning_msg = 'Valid arguments are "inletspline", "outletspline", "inoutspline" or "inci_dev_spline"'
        dev_msg     = 'Check subroutine readinput in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)

    end if

    call close_log_file(nopen, file_open)
    write(nopen1, '(A)') trim(temp)



    ! 
    ! Read curvature control switch
    !
    read(1,*)curv, spanwise_spline  
    if (trim(spanwise_spline) == 'spanwise_spline') then
        backspace(1)
        read(1,'(A)') temp
        write(nopen1, '(A)') trim(temp)
    else
        backspace(1)
        backspace(1)
        read(1,'(A)') temp
        write(nopen1, '(A)') trim(temp)
        read(1,'(A)') temp
        write(nopen1, '(A)') trim(temp)
    end if

    ! Invalid input for the camber definition switch
    ! Warn user and stop execution 
    if (curv /= 0 .and. curv /= 1) then

        error_msg   = 'Invalid argument for camber definition switch'
        warning_msg = 'Valid arguments are 0 or 1 (refer to T-Blade3 documentation)'
        dev_msg     = 'Check subroutine readinput in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)

    end if

    ! Invalid input for curvature control switch
    ! Warn user and stop execution
    if (trim(spanwise_spline) /= 'spanwise_spline' .and. trim(spanwise_spline) /= 'Airfoil') then

        error_msg   = 'Invalid argument for curvature control switch'
        warning_msg = 'Valid argument for using spancontrolinputs is "spanwise_spline" (refer to T-Blade3 documentation)'
        dev_msg     = 'Check subroutine readinput in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)

    end if 


    
    !
    ! Read next line in the input file if spanwise_spline has been specified
    !
    if (trim(spanwise_spline)=='spanwise_spline')then
        read(1,'(A)')temp
        write(nopen1,'(A)') trim(temp)
    endif


    
    ! 
    ! Read thickness distribution switch
    !
    read(1, *)thick_distr, temp2
    backspace(1)
    read(1,'(A)') temp
    write(nopen1, *) thick_distr
    write(nopen1,'(A)') trim(temp)
    
    ! If there is an invalid input for the thickness distribution switch
    ! Warn user and stop execution
    if (thick_distr == 3 .or. thick_distr == 4) then

        error_msg   = 'Invalid argument for thickness distribution switch'
        warning_msg = 'Direct and Exact thickness distributions are no longer available with T-Blade3'
        dev_msg     = 'Check subroutine readinput in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)

    else if (thick_distr /= 0 .and. thick_distr /= 1 .and. thick_distr /= 2 .and. thick_distr /= 5) then

        error_msg   = 'Invalid argument for thickness distribution switch'
        warning_msg = 'Valid arguments are 0, 1, 2, 3, 4 or 5 (refer to T-Blade3 documentation)'
        dev_msg     = 'Check subroutine readinput in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)

    end if



    !
    ! Read thickness multiplier switch
    !
    read(1, *)thick       
    write(nopen1,*) thick
    
    ! If there is an invalid input for the thickness multiplier switch
    ! Warn user and stop execution
    if (thick /= 0 .and. thick /= 1) then

        error_msg   = 'Invalid argument for thickness multiplier switch'
        warning_msg = 'Valid arguments are 0 or 1 (refer to T-Blade3 documentation)'
        dev_msg     = 'Check subroutine readinput in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)

    end if



    !
    ! Read LE spline control switch
    !
    read(1, '(A)')temp
    read(1, *)LE         
    write(nopen1,'(A)') trim(temp)
    write(nopen1,*) LE
   
    ! Raise fatal error for activating LE spline control switch
    if (LE /= 0) then
        
        call current_version(master_ID, develop_ID)
        error_msg   = 'LE spline control is not available in T-Blade3 version '//develop_ID
        dev_msg     = 'Check subroutine readinput in readinput.f90'
        call fatal_error(error_msg, dev_msg = dev_msg)

    end if


     
    !
    ! Read non-dimensional actual chord control switch
    !
    read(1, '(A)')temp
    read(1, *)chord_switch 
    write(nopen1,'(A)') trim(temp)
    write(nopen1,*) chord_switch
    
    ! If there is an invalid input for the non-dimensional actual chord switch
    ! Warn user and stop execution
    if (chord_switch /= 0 .and. chord_switch /= 1 .and. chord_switch /= 2) then

        error_msg   = 'Invalid argument for non-dimensional actual chord switch'
        warning_msg = 'Valid arguments are 0, 1 or 2 (refer to T-Blade3 documentation)'
        dev_msg     = 'Check subroutine readinput in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)

    end if



    !
    ! True lean and sweep switch
    !
    read(1, '(A)')temp
    read(1, *)leansweep_switch 
    write(nopen1,'(A)') trim(temp)
    write(nopen1,*) leansweep_switch
    
    ! If there is an invalid input for the true lean and sweep switch
    ! Warn user and stop execution
    if (leansweep_switch == 0) then
        trueleansweep = ''
    else if (leansweep_switch == 1) then
        trueleansweep = '1'
    else
        error_msg   = 'Invalid argument for leansweep_switch'
        warning_msg = 'Valid arguments are 0 or 1 (refer to T-Blade3 documentation)'
        dev_msg     = 'Check subroutine readinput in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)
    end if



    !
    ! Read clustering distribution switch
    !
    read(1, '(A)')temp
    write(nopen1,'(A)') trim(temp)
    read(1, *)clustering_switch, clustering_parameter
    backspace(1)
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    
    ! If there is an invalid input for the clustering distribution switch
    ! Warn user and stop execution
    if (clustering_switch /= 0 .and. clustering_switch /= 1 .and. clustering_switch /= 2 .and. &
        clustering_switch /= 3 .and. clustering_switch /= 4) then

        error_msg   = 'Invalid argument for clustering_switch'
        warning_msg = 'Valid arguments are 0, 1, 2 or 3 (refer to T-Blade3 documentation)'
        dev_msg     = 'Check subroutine readinput in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)

    end if



    !
    ! Read next lines
    !
    read(1, '(A)')temp
    write(nopen1, '(A)') trim(temp)
    read(1, '(A)')temp
    write(nopen1, '(A)') trim(temp)


    
    !
    ! Determine blade section file names
    !
    do i = 1, nsl
        write(ibrowc1, '(i3)')i
        blext(i) = trim(adjustl(ibrowc1))//'.'//trim(adjustl(ibrowc))//'.'//fext
    enddo



    !
    ! Read sectionwise properties table
    !
    nspn = nsl
    do js = 1, nspn

        ! Not reading inBeta and outBeta
        ! inBeta and outBeta are splined spanwise
        if (spanwise_angle_spline)then 
            read(1, *, end = 35)tempr, tempr, tempr, &
            mrel1(js), chord(js), thk_c(js), inci(js), devn(js), sec_flow_ang(js)
            backspace(1)
            read(1,'(A)') temp
            write(nopen1, '(A)') trim(temp)

        ! Not reading inci and devn
        ! inci and devn are splined spawnise
        elseif (spanwise_inci_dev_spline) then 
            read(1, *, end = 35)tempr, in_beta(js), out_beta(js), mrel1(js), chord(js), thk_c(js), inci(js), devn(js), sec_flow_ang(js)
            backspace(1)
            read(1,'(A)') temp
            write(nopen1, '(A)') trim(temp)

        ! Read all entries in the table
        else 
            read(1, *, end = 35)tempr, in_beta(js), out_beta(js), mrel1(js), chord(js), thk_c(js), inci(js), devn(js), sec_flow_ang(js)
            backspace(1)
            read(1,'(A)') temp
            write(nopen1, '(A)') trim(temp)
        end if

    end do   ! js

    ! If no angles are splined spanwise, set angles
    if (.not.spanwise_angle_spline .and. .not.spanwise_inci_dev_spline) then
       do js = 1, nspn
          in_beta( js) =  inBetaInci(in_beta(js),               inci(js))
          out_beta(js) = outBetaDevn(in_beta(js), out_beta(js), devn(js))
       enddo
    endif



    !
    ! Read the LE / TE curve definition
    !
    read(1, '(A)')temp
    write(nopen1,'(A)') trim(temp)
    read(1, '(A)')temp
    write(nopen1,'(A)') trim(temp) 
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1, *)npoints
    write(nopen1,*) npoints
    read(1, '(A)')temp
    write(nopen1,'(A)') trim(temp)
    do i = 1, npoints
        read(1, *)xle(i), rle(i), xte(i), rte(i)
        backspace(1)
        read(1,'(A)') temp
        write(nopen1,'(A)') trim(temp)
    enddo


   
    ! 
    ! Read set of descriptor lines
    !
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)



    ! Read variable radial stacking switch
    read(1,*)stack_switch
    write(nopen1,*) stack_switch
    read(1,'(A)')temp

    ! Raise warning if grid generation parameters are still present in main input file
    ! TODO: Will be made a fatal error in future commits
    if ((index(trim(temp), 'Jcells') /= 0) .or. (index(trim(temp), 'eta_ofst') /= 0) &
        .or. (index(trim(temp), 'BGgrid') /= 0)) then
        warning_msg     = 'Background grid generation is no longer available with T-Blade3'
        warning_msg_1   = 'Background grid generation parameters should be removed from main input file'
        dev_msg         = 'Check subroutine readinput in readinput.f90'
        call warning(warning_msg, warning_msg_1, dev_msg)
        grid_gen_present = .true.
    end if
    write(nopen1,'(A)') trim(temp)



    !
    ! Read airfoil type, stacking values, chord location of max. thickness, LE thickness
    !      TE thickness and grid arguments
    !
    if (allocated(umxthk_all)) deallocate(umxthk_all)
    allocate(umxthk_all(nsl))

    ! If spline LE is being used, don't store LE and TE thickness
    ! TODO: grid_gen_present will be removed in future commits
    if (LE/=0) then

        do js = 1, nspn
            if (grid_gen_present) then
                read(1, *)tempr, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), tempr, tempr, &
                          jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
            else
                read(1, *)tempr, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), tempr, tempr
            end if
            backspace(1)
            read(1,'(A)') temp
            write(nopen1,'(A)') trim(temp)
        enddo

    ! If elliptical LE is being used, read LE and TE thickness
    ! TODO: grid_gen_present will be removed in future commits
    elseif (LE == 0) then
        
        if (allocated(lethk_all)) deallocate(lethk_all)
        allocate(lethk_all(nsl))
        if (allocated(tethk_all)) deallocate(tethk_all)
        allocate(tethk_all(nsl))
        
        do js = 1, nspn
            if (grid_gen_present) then
                read(1, *)tempr, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), lethk_all(js), &
                          tethk_all(js), jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
            else
                read(1, *)tempr, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), lethk_all(js), &
                          tethk_all(js)
            end if
            backspace(1)
            read(1,'(A)') temp
            write(nopen1,'(A)') trim(temp)
        enddo

    endif   ! LE



    !
    ! Read descriptor lines
    !
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)



    !
    ! Read control table for blending section variable
    !
    read(1, *)cpbsv, bsv1, bsv2
    backspace(1)
    read(1,'(A)') temp
    write(nopen1,*) trim(temp)
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)
    do i = 1, cpbsv
        read(1, *)spanbsv(i), bf1(i), bf2(i)
        backspace(1)
        read(1,'(A)') temp
        write(nopen1,'(A)') trim(temp)
    enddo



    !
    ! Read stacking axis location
    !
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1, *)stack
    write(nopen1,*) stack



    !
    ! Read sweep spline control points 
    !
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)')temp 
    write(nopen1,'(A)') trim(temp)
    read(temp(12:12), *)cpdeltam

    ! True sweep
    if(trim(trueleansweep)/='')then
        
        chrdsweep = 1
        read(1,'(A)')temp
        write(nopen1,'(A)') trim(temp)
        do i = 1, cpdeltam
            read(1, *)spanmp(i), xcpdelm(i)
            backspace(1)
            read(1,'(A)') temp
            write(nopen1,'(A)') trim(temp)
        enddo

    ! Axial sweep
    else
        
        read(1,'(A)')temp
        write(nopen1,'(A)') trim(temp)
        do i = 1, cpdeltam
            read(1, *)spanmp(i), xcpdelm(i)
            backspace(1)
            read(1,'(A)') temp
            write(nopen1,'(A)') trim(temp)
        enddo

    endif   ! trim(trueleansweep)

    !
    ! Call ESP sweep override subroutines
    !
    if (allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cpdeltam))
    
    !
    ! override_span_del_m_ctrl() in 3dbgb.f90
    ! Callback to override_span_del_m_ctrl_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    temp_in = spanmp(1:cpdeltam)
#ifdef FROM_ESP
    call override_span_del_m_ctrl(cpdeltam, temp_in)
#endif
    spanmp(1:cpdeltam) = temp_in

    !
    ! override_span_del_m() in 3dbgb.f90
    ! Callback to override_span_del_m_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    if (allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cpdeltam))
    temp_in = xcpdelm(1:cpdeltam)
#ifdef FROM_ESP
    call override_span_del_m(cpdeltam, temp_in)
#endif
    xcpdelm(1:cpdeltam) = temp_in



    !
    ! Read lean spline control points
    !
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)')temp 
    write(nopen1,'(A)') trim(temp)
    read(temp(12:12), *)cpdeltheta

    ! True lean
    if(trim(trueleansweep)/='')then
        
        chrdlean = 1
        read(1,'(A)')temp
        write(nopen1,'(A)') trim(temp)
        do i = 1, cpdeltheta
            read(1, *)spantheta(i), xcpdeltheta(i)
            backspace(1)
            read(1,'(A)') temp
            write(nopen1,'(A)') trim(temp)
        enddo

    ! Tangential lean
    else
        
        read(1,'(A)')temp
        write(nopen1,'(A)') trim(temp)
        do i = 1, cpdeltheta
            read(1, *)spantheta(i), xcpdeltheta(i)
            backspace(1)
            read(1,'(A)') temp
            write(nopen1,'(A)') trim(temp)
        enddo
    
    endif

    ! Call ESP lean override subroutines
    if (allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cpdeltheta))

    ! 
    ! override_span_del_theta_ctrl() in 3dbgb.f90
    ! Callback to override_span_del_theta_ctrl_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    temp_in = spantheta(1:cpdeltheta)
#ifdef FROM_ESP
    call override_span_del_theta_ctrl(cpdeltheta, temp_in)
#endif
    spantheta(1:cpdeltheta) = temp_in
     
    !
    ! override_span_del_theta() in 3dbgb.f90
    ! Callback to override_span_det_theta_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    if (allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cpdeltheta))
    temp_in = xcpdeltheta(1:cpdeltheta) 
#ifdef FROM_ESP
    call override_span_del_theta(cpdeltheta, temp_in)
#endif
    xcpdeltheta(1:cpdeltheta) = temp_in



    !
    ! Read inBeta* spline control points
    !
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)
    read(1, *)cpinbeta 
    write(nopen1,*) cpinbeta
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)
    do i = 1, cpinbeta
        read(1, *)spaninbeta(i), xcpinbeta(i)
        backspace(1)
        read(1,'(A)') temp
        write(nopen1,'(A)') trim(temp)
    enddo

    !
    ! Call ESP inlet angle override subroutines
    ! override_span_in_beta_ctrl() in 3dbgb.f90
    ! Callback to override_span_in_beta_ctrl_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    if (allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cpinbeta))
    temp_in = spaninbeta(1:cpinbeta)
#ifdef FROM_ESP
    call override_span_in_beta_ctrl(cpinbeta, temp_in)
#endif
    spaninbeta(1:cpinbeta) = temp_in

    ! 
    ! override_span_out_beta() in 3dbgb.f90
    ! Callback to override_span_in_beta_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    if (allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cpinbeta))
    temp_in = xcpinbeta(1:cpinbeta)
#ifdef FROM_ESP
    call override_span_in_beta(cpinbeta, temp_in)
#endif
    xcpinbeta(1:cpinbeta) = temp_in



    !
    ! Read outBeta* spline control points
    !
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1, *)cpoutbeta
    write(nopen1,*) cpoutbeta
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    do i = 1, cpoutbeta
        read(1, *)spanoutbeta(i), xcpoutbeta(i)
        backspace(1)
        read(1,'(A)') temp
        write(nopen1,'(A)') trim(temp)
    enddo

    !
    ! Call ESP exit angle override subroutines
    ! override_span_out_beta_ctrl() in 3dbgb.f90
    ! Callback to override_span_out_beta_ctrl_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    if (allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cpoutbeta))
    temp_in = spanoutbeta(1:cpoutbeta)
#ifdef FROM_ESP
    call override_span_out_beta_ctrl(cpoutbeta, temp_in)
#endif
    spanoutbeta(1:cpoutbeta) = temp_in

    !
    ! override_span_out_beta() in 3dbgb.f90
    ! Callback to override_span_out_beta_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    if (allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cpoutbeta))
    temp_in = xcpoutbeta(1:cpoutbeta)
#ifdef FROM_ESP
    call override_span_out_beta(cpoutbeta, temp_in)
#endif
    xcpoutbeta(1:cpoutbeta) = temp_in



    !
    ! Read chord multiplier spline control points
    ! TODO: chord_multiplier > 1?
    !
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1, *)cpchord
    write(nopen1,*) cpchord
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    do i = 1, cpchord
        read(1, *)spanchord(i), xcpchord(i)
        backspace(1)
        read(1,'(A)') temp
        write(nopen1,'(A)') trim(temp)
        xcpchord(i) = xcpchord(i) + 1.0
    enddo

    ! 
    ! Call ESP chord multiplier override subroutines
    ! override_span_chord_ctrl() in 3dbgb.f90
    ! Callback to override_span_chord_ctrl_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    if (allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cpchord))
    temp_in = spanchord(1:cpchord)
#ifdef FROM_ESP
    call override_span_chord_ctrl(cpchord, temp_in)
#endif
    spanchord(1:cpchord) = temp_in

    !
    ! override_span_chord() in 3dbgb.f90
    ! Callback to override_span_chord_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    if (allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cpchord))
    temp_in = xcpchord(1:cpchord)
#ifdef FROM_ESP
    call override_span_chord(cpchord, temp_in)
#endif
    
    ! If xcpchord values have been overridden, add 1
    equal   = .true.
    do i = 1,cpchord
        equal = (abs(xcpchord(i) - temp_in(i)) .le. tol)
        if (.not. equal) exit
    end do
    if (.not. equal) then
        xcpchord(1:cpchord) = temp_in + 1
    else
        xcpchord(1:cpchord) = temp_in
    end if



    ! 
    ! Read tm/c spline control points 
    ! TODO: tm/c > 1?
    !
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)
    read(1, *)cptm_c ! control points for tm/c
    write(nopen1,*) cptm_c
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)

    ! Find index of 'tm/c' and 'u_max'
    iumax   = index(trim(temp), 'u_max')
    itm_c   = index(trim(temp), 'tm/c')

    ! Set reading options based on the indices of 'tm/c' and 'u_max'
    if (itm_c < iumax .and. itm_c /= 0) read_tm_c_then_u_max    = .True.
    if (itm_c > iumax .and. iumax /= 0) read_u_max_then_tm_c    = .True.
    if (itm_c /= 0 .and. iumax == 0)    read_tm_c_only          = .True.
    if (itm_c == 0 .and. iumax /= 0)    read_u_max_only         = .True.

    ! Next line to always use the thickness tm/c as it is a multiplier (default = 1):
    if ((thick /= 1) .and. (.not. is2d) .and. (read_tm_c_then_u_max .or. &
       &read_u_max_then_tm_c .or. read_tm_c_only) .and. (thick_distr /= 5)) then
        tm_c_spline = .True.
    else
        tm_c_spline = .False.
    endif  

    ! Read inputs in appropriate order
    do i = 1,cptm_c

        if (read_tm_c_then_u_max) then
            read(1,*) spantm_c(i), xcptm_c(i), xcpumax(i)
            xcptm_c(i) = xcptm_c(i)! + 1.0
            if (thick_distr == 1 .or. thick_distr == 2) &
                xcptm_c(i) = xcptm_c(i) + 1.0
        else if (read_u_max_then_tm_c) then
            read(1,*) spantm_c(i), xcpumax(i), xcptm_c(i)
            xcptm_c(i) = xcptm_c(i)! + 1.0
            if (thick_distr == 1 .or. thick_distr == 2) &
                xcptm_c(i) = xcptm_c(i) + 1.0
        else if (read_tm_c_only) then
            read(1,*) spantm_c(i), xcptm_c(i)
            xcptm_c(i) = xcptm_c(i)! + 1.0
            if (thick_distr == 1 .or. thick_distr == 2) &
                xcptm_c(i) = xcptm_c(i) + 1.0
        else if (read_u_max_only) then
            read(1,*) spantm_c(i), xcpumax(i)
        end if

        backspace(1)
        read(1,'(A)') temp
        write(nopen1,'(A)') trim(temp)

        ! If u_max is being read from file and is zero, activate flag
        ! TODO: Add warning?
        if ((.not. read_tm_c_only) .and. (xcpumax(i) < tol) .and. (.not. zero_u_max)) then
            zero_u_max = .true.
        end if 

    end do

    ! Determine if u_max spline needs to be used or not
    if ((thick_distr == 0 .or. thick_distr == 1 .or. thick_distr == 2) .and.  &
       &(read_tm_c_then_u_max .or. read_u_max_then_tm_c .or. read_u_max_only) &
       &.and. (.not. zero_u_max)) then

       u_max_spline = .true.

    end if 

    ! Warning message for u_max spline zero values
    if (zero_u_max) then
        warning_msg     = "Zero value for u_max spline control points detected, reverting to umxthk values"
        warning_msg_1   = "If you want to use u_max spline, please use non-zero values otherwise ignore this warning"
        dev_msg         = "Check subroutine readinput in readinput.f90"
        call warning(warning_msg, warning_msg_1, dev_msg)
    end if

    !
    ! Call ESP tm/c override subroutines
    ! override_span_thk_c_ctrl() in 3dbgb.f90
    ! Callback to override_span_thk_ctrl_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    if (allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cptm_c))
    temp_in = spantm_c(1:cptm_c)
#ifdef FROM_ESP
    call override_span_thk_c_ctrl(cptm_c, temp_in)
#endif
    spantm_c(1:cptm_c) = temp_in

    !
    ! override_span_thk_c() in 3dbgb.f90
    ! Callback to override_span_thk_c() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    if(allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cptm_c))
    temp_in = xcptm_c(1:cptm_c)
#ifdef FROM_ESP
    call override_span_thk_c(cptm_c, temp_in)
#endif
    
    ! If xcptm_c values have been overridden, add 1
    if (thick_distr == 1 .or. thick_distr == 2) then
        do i = 1,cptm_c
            equal = (abs(xcptm_c(i) - temp_in(i)) .le. tol)
            if (.not. equal) exit
        end do
        if (.not. equal) then
            xcptm_c(1:cptm_c) = temp_in + 1
        else
        xcptm_c(1:cptm_c) = temp_in
        end if
    else if (thick_distr == 0) then
        xcptm_c(1:cptm_c) = temp_in
    end if

    !
    ! override_span_u_max() in 3dbgb.f90
    ! Callback to override_span_u_max_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    if (allocated(temp_in)) deallocate(temp_in)
    allocate(temp_in(cptm_c))
    temp_in = xcpumax(1:cptm_c)
#ifdef FROM_ESP
    call override_span_u_max(cptm_c, temp_in)
#endif
    xcpumax(1:cptm_c) = temp_in



    ! 
    ! Read hub and tip offsets and call ESP override subroutine
    !
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)') temp 
    write(nopen1,'(A)') trim(temp)

    ! Read hub related offsets as a string
    read(1,'(A)') temp

    ! If only hub offset is present
    if (index(trim(adjustl(temp)), ' ', back = .true.) == 0) then

        n_temp1 = len(trim(adjustl(temp)))
        temp_str    = trim(adjustl(temp))
        read(temp_str(1:n_temp1), *) hub

    ! If hub offset and hub inflation offset have been specified
    else if (index(trim(adjustl(temp)), ' ', back = .true.) /= 0) then

        hub_inflate = .true.
        temp_str    = trim(adjustl(temp))
        n_temp1     = index(temp_str, ' ')
        n_temp2     = index(temp_str, ' ', back = .true.)
        read(temp_str(1:n_temp1 - 1), *) hub
        read(temp_str(n_temp2:), *) hub_inf_offset

        ! Check for zero inflation offset
        if (hub_inf_offset > 10E-8) hub_inflate = .true.

    end if  ! index(trim(adjustl(temp)))

    write(nopen1, '(A)') temp
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)

    ! Read tip related offsets as a string
    read(1,'(A)') temp

    ! If only tip offset is present
    if (index(trim(adjustl(temp)), ' ', back = .true.) == 0) then
        
        n_temp1     = len(trim(adjustl(temp)))
        temp_str    = trim(adjustl(temp))
        read(temp_str(1:n_temp1), *) tip

    ! If tip offset and tip inflation offset have been specified
    else if (index(trim(adjustl(temp)), ' ', back = .true.) /= 0) then

        tip_inflate = .true.        
        temp_str    = trim(adjustl(temp))
        n_temp1     = index(temp_str, ' ')
        n_temp2     = index(temp_str, ' ', back = .true.)
        read(temp_str(1:n_temp1 - 1), *) tip
        read(temp_str(n_temp2:), *) tip_inf_offset

    end if  ! index(trim(adjustl(temp)))

    write(nopen1, '(A)') temp

    !
    ! Call ESP offset override subroutines
    ! override_offsets() in 3dbgb.f90
    ! Callback to override_offsets_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    temp_offsets(1) = hub
    temp_offsets(2) = tip
#ifdef FROM_ESP
    call override_offsets(temp_offsets)
#endif
    hub = temp_offsets(1)
    tip = temp_offsets(2)

    !
    ! Call ESP inflation offset overrride subroutines
    ! override_hub_inf_offset and override_tip_inf_offset
    ! in 3dbgb_driver.f90
    ! Callback to override_hub_inf_offset_() and 
    ! override_tip_inf_offset_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
    !
    if (hub_inflate) then
        temp_hub_offset     = hub_inf_offset
#ifdef FROM_ESP
        call override_hub_inf_offset(temp_hub_offset)
#endif
        hub_inf_offset      = temp_hub_offset
    end if

    if (tip_inflate) then
        temp_tip_offset     = tip_inf_offset
#ifdef FROM_ESP
        call override_tip_inf_offset(temp_tip_offset)
#endif
        tip_inf_offset      = temp_tip_offset
    end if



    !
    ! Read streamline data
    !
    read(1,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read(1,'(A)')temp
    write(nopen1,'(A)') trim(temp)
    do while(temp/='x_s')
        read(1, *)temp
        backspace(1)
        read(1,'(A)') temp1
        write(nopen1,'(A)') trim(temp1)
    enddo



    !
    ! Calculating m-prime coordinate using x, r coordinates 
    ! as the input for streamlines
    !
    do ia = 1, nsl
        
        nsp(ia) = 0
        do while (.true.)
            
            read(1, *)trarray(1), trarray(2)
            backspace(1)
            read(1,'(A)') temp
            write(nopen1,'(A)') trim(temp)
            
            if (trarray(2)/=0)then
                nsp(ia) = nsp(ia) + 1
                xm(nsp(ia), ia) = trarray(1)
                rm(nsp(ia), ia) = trarray(2)
                else
                exit
            end if

        end do   ! do while (.true.)
    
    end do   ! ia



    ! Close input file
    35 close(1)
    call close_maininput_log_file(nopen1, file_open_1)
    return


end subroutine readinput
!------------------------------------------------------------------------------------------------------------






!
! This subroutine reads the old auxiliary input file (controlinputs.bladerow.dat)
! Stores input in several global variables defined in globvar.f90
!
! Input parameter: row_type - T-Blade3 bladerow type
!                  path     - path of the directory where auxiliary input file is
!
!------------------------------------------------------------------------------------------------------------
subroutine readcontrolinput(row_type, path)
    use globvar
    use file_operations
    use funcNsubs
    implicit none

    character(256),                 intent(in)      :: row_type
    character(*),                   intent(in)      :: path
    
    ! Local variables
    character(256)                                  :: temp, fname4, fname5
    character(:),   allocatable                     :: log_file
    integer                                         :: phantom_n, nopen, nopen1
    logical                                         :: file_open, file_open_1, isquiet_local


    !
    ! Determine if 'isquiet' command line argument is being used
    !
    call get_quiet_status(isquiet_local)

    
    
    if (.not. isquiet_local) print *, ''
    call log_file_exists(log_file, nopen, file_open)
    write(nopen,*) ''
    call close_log_file(nopen, file_open)
    
    
    
    !
    ! Determine auxiliary input file name
    !
    fname5 = trim(path)//'controlinputs.'//trim(row_type)//'.dat'



    !
    ! Start writing log file based on auxiliary input file
    !    
    call open_auxinput_log_file(trim(adjustl(fname5)), nopen1, file_open_1)
    
    
    
    !
    ! Open auxiliary input file and start reading
    !
    open(11, file = fname5)
    rewind(11)
    
    
    
    !
    ! Allocate curvature arrays
    !
    if (allocated(ncp_curv)) deallocate(ncp_curv)
    allocate(ncp_curv(nsl))
    if (allocated(curv_cp )) deallocate(curv_cp )
    allocate(curv_cp(20, 2*nsl))



    !
    ! temp is used to read descriptor lines
    !
    read (11,'(A)') temp   
    write(nopen1,'(A)') trim(temp)
    read (11,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    
    
    
    !
    ! Read curvature, thickness and LE control points for all sections
    !
    current = 0
    do i = 1, nsl
        
        ! Read descriptor lines
        read (11,'(A)') temp
        write(nopen1,'(A)') trim(temp)
        read (11,'(A)') temp
        write(nopen1,'(A)') trim(temp)

        ! Read number of chordwise control points 
        read (11, *) ncp_curv(i)
        backspace(11)
        read(11,'(A)') temp
        write(nopen1,'(A)') trim(temp)

        ! Accommodate for fixed control points
        ncp_curv(i) = ncp_curv(i) + 2
        if (allocated(xcp)) deallocate(xcp)
        allocate(xcp(ncp_curv(i)))
        if (allocated(ycp)) deallocate(ycp)
        allocate(ycp(ncp_curv(i)))

        ! Update counter
        current = i 
        read (11,'(A)') temp
        write(nopen1,'(A)') trim(temp)
        write(radialsec, *)current 
        
        ! Read curvature control points for current section
        do j = 1, (ncp_curv(i)-2)
            read(11, *) xcp(j+1), ycp(j+1)
            backspace(11)
            read(11,'(A)') temp
            write(nopen1,'(A)') temp
        end do

        ! If command line option "dev" is used, write curvature control points to a file
        ! TODO: Move to file_operations
        if(isdev) then

            fname4 = 'curvature_ctrl_pts.'//trim(adjustl(radialsec))//'.'//trim(casename)//'.txt'
            open(12, file = fname4)
            write(12, *)trim(casename)
            write(12, *)'Curvature Control points for camber'
            write(12, *)'u      v '   
            do j = 1, (ncp_curv(i)-2)
                write(12, *)xcp(j+1), ycp(j+1)
            end do
            close(12)

        end if   ! isdev 

        ! Fixed control points (leading and trailing edges)
        xcp(1) = 2*xcp(2)-xcp(3)
        ycp(1) = 2*ycp(2)-ycp(3)
        xcp(ncp_curv(i)) = 2*xcp(ncp_curv(i)-1)-xcp(ncp_curv(i)-2)
        ycp(ncp_curv(i)) = 2*ycp(ncp_curv(i)-1)-ycp(ncp_curv(i)-2) 
        do k = 1, ncp_curv(i)
            curv_cp(k, 2*i-1) = xcp(k)
            curv_cp(k, 2*i) = ycp(k)
        end do

    enddo   ! i = 1,nsl

    
    
    ! Allocate thickness arrays
    if (allocated(ncp_thk)) deallocate(ncp_thk)
    if (allocated(thk_cp )) deallocate(thk_cp )
    Allocate(ncp_thk(nsl))
    Allocate(thk_cp(20, 2*nsl))
    
    ! Phantom points
    phantom_n = 4
    
    ! Read descriptor lines
    read(11,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    read (11,'(A)') temp
    write(nopen1,'(A)') trim(temp)
    
    do i = 1, nsl

        ! Read descriptor lines
        read (11,'(A)') temp
        write(nopen1,'(A)') trim(temp)
        read (11,'(A)') temp
        write(nopen1,'(A)') trim(temp)

        ! Read number of chordwise thickness control points
        read (11, *) ncp_thk(i)
        backspace(11)
        read (11,'(A)') temp
        write(nopen1,'(A)') trim(temp)

        ! Accommodate phantom points
        ncp_thk(i) = ncp_thk(i) + phantom_n

        ! Allocate thickness control points arrays
        if (allocated(xcp)) deallocate(xcp)
        if (allocated(ycp)) deallocate(ycp)
        Allocate(xcp(ncp_thk(i)))
        Allocate(ycp(ncp_thk(i)))

        read (11,'(A)') temp
        write(nopen1,'(A)') trim(temp)
        
        ! Read thickness control points for the current section
        do j = 1, (ncp_thk(i)-phantom_n)
            read(11, *) xcp(j+(phantom_n/2)), ycp(j+(phantom_n/2))
            backspace(11)
            read(11,'(A)') temp
            write(nopen1,'(A)') trim(temp)
        end do
            
        xcp(1)              = 2*xcp(3) - xcp(5)    
        ycp(1)              = 2*ycp(3) - ycp(5)
        xcp(2)              = 2*xcp(3) - xcp(4)    
        ycp(2)              = 2*ycp(3) - ycp(4)
        xcp(ncp_thk(i))     = 2*xcp(ncp_thk(i) - 2) - xcp(ncp_thk(i) - 4) 
        ycp(ncp_thk(i))     = 2*ycp(ncp_thk(i) - 2) - ycp(ncp_thk(i) - 4)
        xcp(ncp_thk(i) - 1) = 2*xcp(ncp_thk(i) - 2) - xcp(ncp_thk(i) - 3)
        ycp(ncp_thk(i) - 1) = 2*ycp(ncp_thk(i) - 2) - ycp(ncp_thk(i) - 3)

        ! Store in thickness control point array
        do k = 1, ncp_thk(i)
            thk_cp(k, 2*i-1) = xcp(k)
            thk_cp(k, 2*i) = ycp(k)
        enddo

    enddo   ! i = 1,nsl

    
    
    if (allocated(sting_l_all)) deallocate(sting_l_all)
    allocate(sting_l_all(nsl))
    
    ! Allocate LE control parameters arrays for spline LE
    ! TODO: To be removed
    !if(LE /= 0) then

    !    if (allocated(lethk_all)) deallocate(lethk_all)
    !    allocate(lethk_all(nsl))
    !    if (allocated(tethk_all)) deallocate(tethk_all)
    !    allocate(tethk_all(nsl))
    !    if (allocated(s_all)) deallocate(s_all)
    !    allocate(s_all(nsl))
    !    if (allocated(ee_all)) deallocate(ee_all)
    !    allocate(ee_all(nsl))
    !    if (allocated(C_le_x_top_all)) deallocate(C_le_x_top_all)
    !    allocate(C_le_x_top_all(nsl))
    !    if (allocated(C_le_x_bot_all)) deallocate(C_le_x_bot_all)
    !    allocate(C_le_x_bot_all(nsl))
    !    if (allocated(C_le_y_top_all)) deallocate(C_le_y_top_all)
    !    allocate(C_le_y_top_all(nsl))
    !    if (allocated(C_le_y_bot_all)) deallocate(C_le_y_bot_all)
    !    allocate(C_le_y_bot_all(nsl))
    !    if (allocated(LE_vertex_ang_all)) deallocate(LE_vertex_ang_all)
    !    allocate(LE_vertex_ang_all(nsl))
    !    if (allocated(LE_vertex_dis_all)) deallocate(LE_vertex_dis_all)
    !    allocate(LE_vertex_dis_all(nsl))
    !    if (allocated(sting_h_all)) deallocate(sting_h_all)
    !    allocate(sting_h_all(nsl,2))
    !   
    !    ! Read descriptor lines 
    !    read (11,'(A)') temp
    !    write(nopen1,'(A)') trim(temp)
    !    read (11,'(A)') temp
    !    write(nopen1,'(A)') trim(temp)
    !    read (11,'(A)') temp
    !    write(nopen1,'(A)') trim(temp)

    !    ! Read LE spline degree and no. of LE spline segments
    !    read (11, *) LEdegree, no_LE_segments
    !    backspace(11)
    !    read (11,'(A)') temp
    !    write(nopen1,'(A)') trim(temp)

    !    call log_file_exists(log_file, nopen, file_open)
    !    if (.not. isquiet_local) print*, 'LEdegree = ', LEdegree, 'no_LE_segments = ', no_LE_segments
    !    write(nopen,*) 'LEdegree = ', LEdegree, 'no_LE_segments = ', no_LE_segments
    !    call close_log_file(nopen, file_open)
    !    
    !    ! Read LE control parameters
    !    do i = 1, nsl
    !        read (11,'(A)') temp
    !        write(nopen1,'(A)') trim(temp)
    !        read (11,'(A)') temp
    !        write(nopen1,'(A)') trim(temp)
    !        read (11, *) lethk_all(i), tethk_all(i), s_all(i), ee_all(i), C_le_x_top_all(i), C_le_x_bot_all(i), &
    !        C_le_y_top_all(i), C_le_y_bot_all(i), LE_vertex_ang_all(i), LE_vertex_dis_all(i), &
    !        sting_l_all(i), sting_h_all(i, 1), sting_h_all(i, 2)
    !        backspace(11)
    !        read (11,'(A)') temp
    !        write(nopen1,'(A)') trim(temp)
    !    
    !    end do
    !
    !end if  ! LE
   
    
    
    !
    ! Close auxiliary input file and auxiliary log file
    ! 
    close(11)
    call close_auxinput_log_file(nopen1, file_open_1)
    return


end subroutine readcontrolinput
!------------------------------------------------------------------------------------------------------------






!
! This subroutine reads the auxiliary input file (spancontrolinputs.bladerow.dat)
! Stores input in several global variables defined in globvar.f90
!
! Input parameters: row_type - T-Blade3 blade row type
!                   path     - path of the directory where the auxiliary input file is
!
!------------------------------------------------------------------------------------------------------------
subroutine read_spanwise_input(row_type, path)
    use globvar
    use file_operations
    use errors
    use funcNsubs
    implicit none
    
    character(256),                 intent(in)      :: row_type
    character(*),                   intent(in)      :: path

    ! Local variables
    character(256)                                  :: temps, file_name
    character(:),   allocatable                     :: log_file, error_msg, dev_msg!warning_msg
    real,           allocatable                     :: temp(:)
    real                                            :: span_dum
    integer                                         :: jj, nopen, nopen1
    logical                                         :: file_open, file_open_1, deprecated, isquiet_local



    !
    ! Determine auxiliary input file name
    !
    file_name = trim(path)//'spancontrolinputs.'//trim(row_type)//'.dat'
    
    
    
    !
    ! Start writing log file based on auxiliary input file
    !
    call open_auxinput_log_file(trim(adjustl(file_name)), nopen1, file_open_1)
    
    
    
    !
    ! Start reading auxiliary input file
    !
    open(10, file = file_name)
    rewind(10)

    
    
    !
    ! temps is used to read descriptor lines
    !
    read(10,'(A)') temps
    write(nopen1,'(A)') trim(temps)
    read(10,'(A)') temps
    write(nopen1,'(A)') trim(temps)
    read(10,'(A)') temps
    write(nopen1,'(A)') trim(temps)
    read(10,'(A)') temps
    write(nopen1,'(A)') trim(temps)
    read(10,'(A)') temps
    write(nopen1,'(A)') trim(temps)

    
    
    !
    ! Read number of spanwise control points and number of chordwise control points
    !
    read(10, *) ncp_span_curv, ncp_chord
    backspace(10)
    read(10,'(A)') temps
    write(nopen1,'(A)') trim(temps)
    if(control_inp_flag == 1 .and. ncp_span_curv /= nsl) then
        error_msg   = ' In auxiliary file inputs, number of spanwise curvature specifications'//&
                      ' must equal number of streamlines if spanwise spline is not used.'
        dev_msg     = 'Check subroutine read_spanwise_input in readinput.f90'
        call fatal_error(error_msg, dev_msg = dev_msg)
    endif
    
    
    
    ! Number of curvature control points should equal number of chordwise control points
    ncp_curvature = ncp_chord



    !
    ! Allocate and initialize curvature control points arrays
    !
    if (allocated(ncp_curv)) deallocate(ncp_curv)
    allocate(ncp_curv(nsl))
    if (allocated(curv_cp )) deallocate(curv_cp )
    if(control_inp_flag == 1) then
        allocate(curv_cp(20, 2*nsl))
    endif
    do i = 1, nsl
        ncp_curv(i) = ncp_curvature + 2
    enddo

    ! Including phantom points spanwise						
    ncp_span_curv1 = ncp_span_curv+2



    read(10,'(A)') temps
    
    ! If control table for cur1 is present, assign ncp_chord_curv accordingly
    ! TODO: Shouldn't be here once everyone moves on to NACA file
    if (index(trim(temps), 'cur1') /= 0) then
    !    error_msg   = 'Incorrect auxiliary file format'
    !    warning_msg = 'This is a newer spancontrolinputs file format compatible with modified NACA thickness distribution only'
    !    dev_msg     = 'Check subroutine read_spanwise_input in readinput.f90'
    !    call fatal_error(error_msg, warning_msg, dev_msg)
        ncp_chord_curv = ncp_chord - 2 + ncp_curvature + 1
        deprecated  = .true.
        using_cur1  = .true.
    else
        ncp_chord_curv = ncp_chord - 2 + ncp_curvature + 1 - 1
        deprecated  = .false.
        using_cur1  = .false.
    end if

    !
    ! Allocate array to store curvature control points table read from auxiliary input file
    !
    if (allocated(cp_chord_curv)) deallocate(cp_chord_curv)
    allocate(cp_chord_curv(ncp_span_curv,ncp_chord_curv))

    write(nopen1,'(A)') temps
    


    !
    ! Read curvature control points table
    !
    do i = 1, ncp_span_curv

        read(10, *) cp_chord_curv(i, 1:ncp_chord_curv)
        backspace(10)
        read(10,'(A)') temps
        write(nopen1,'(A)') temps
        
        !
        ! If spanwise splining is not required, store curvature control points
        ! in appropriate global varaiable
        ! Spanwise splines are constructed in spanwise_variation.f90 and
        ! spanwise_output.f90
        !
        if(control_inp_flag == 1) then
            
            ! Allocate 1D control points arrays
            if (allocated(xcp)) deallocate(xcp)
            if (allocated(ycp)) deallocate(ycp)
            Allocate(xcp(ncp_curv(i)))
            Allocate(ycp(ncp_curv(i)))

            span_dum = cp_chord_curv(i, 1)
            xcp(3:ncp_curv(i)-2) = cp_chord_curv(i, 2:ncp_chord-1)
            ycp(2:ncp_curv(i)-1) = cp_chord_curv(i, ncp_chord:ncp_chord_curv)
            
            ! Store phantom points
            xcp(1) = 2*xcp(2)-xcp(3)
            xcp(2) = 0.
            ycp(1) = 2*ycp(2)-ycp(3)
            xcp(ncp_curv(i)-1) = 1.
            xcp(ncp_curv(i)) = 2*xcp(ncp_curv(i)-1)-xcp(ncp_curv(i)-2)
            ycp(ncp_curv(i)) = 2*ycp(ncp_curv(i)-1)-ycp(ncp_curv(i)-2)

            ! Store in global variable curv_cp
            do k = 1, ncp_curv(i)
                curv_cp(k, 2*i-1) = xcp(k)
                curv_cp(k, 2*i) = ycp(k)
            end do

        end if   ! control_inp_flag

    end do   ! i = 1,ncp_span_curv



    !
    ! ESP curvature override subroutines if spanwise splining is required
    !
    if (control_inp_flag == 2) then
        
        if (allocated(temp)) deallocate(temp)
        allocate(temp(ncp_span_curv))

        !
        ! Override "Span" control points
        ! override_span_curv_ctrl() in 3dbgb.f90
        ! Callback to override_span_curv_ctrl_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        jj = 1
        do i = 1, ncp_span_curv
           temp(i) = cp_chord_curv(i,jj)
        end do
#ifdef FROM_ESP
        call override_span_curv_ctrl(ncp_span_curv, temp)
#endif
        do i = 1, ncp_span_curv
           cp_chord_curv(i,jj) = temp(i)
        end do

        !
        ! Override cur1 if needed
        ! override_cur1() in 3dbgb.f90
        ! Callback to override_cur1_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        ! TODO: Will be removed once everyone moves onto NACA auxiliary file
        !
        if (deprecated) then

            jj = 1 + ncp_chord - 2 + 1
            do i = 1,ncp_span_curv
                temp(i) = cp_chord_curv(i,jj)
            end do
#ifdef FROM_ESP
            call override_cur1(ncp_span_curv, temp)
#endif
            do i = 1,ncp_span_curv
                cp_chord_curv(i,jj) = temp(i)
            end do

        end if
        
        !
        ! Override cur2 if needed
        ! override_cur2() in 3dbgb.f90
        ! Callback to override_cur2_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        ! 
        if (ncp_curvature >= 1) then
           
            if (deprecated) then
                jj = 1 + ncp_chord - 2 + 2
            else 
                jj = 1 + ncp_chord - 2 + 2 - 1
            end if
            
            do i = 1, ncp_span_curv
               temp(i) = cp_chord_curv(i,jj)
            end do
#ifdef FROM_ESP
            call override_cur2(ncp_span_curv, temp)
#endif
            do i = 1, ncp_span_curv
               cp_chord_curv(i,jj) = temp(i)
            end do

        end if

        !
        ! Override cur3 if needed
        ! override_cur3() in 3dbgb.f90
        ! Callback to override_cur3_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_curvature >= 2) then
            
            if (deprecated) then
                jj = 1 + ncp_chord - 2 + 3
            else
                jj = 1 + ncp_chord - 2 + 3 - 1
            end if
            
            do i = 1, ncp_span_curv
               temp(i) = cp_chord_curv(i,jj)
            end do
#ifdef FROM_ESP
            call override_cur3(ncp_span_curv, temp)
#endif
            do i = 1, ncp_span_curv
               cp_chord_curv(i,jj) = temp(i)
            end do

        end if

        !
        ! Override cur4 if needed
        ! override_cur4() in 3dbgb.f90
        ! Callback to override_cur4_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_curvature >= 3) then
           
            if (deprecated) then
                jj = 1 + ncp_chord - 2 + 4
            else 
                jj = 1 + ncp_chord - 2 + 4 - 1
            end if

            do i = 1, ncp_span_curv
               temp(i) = cp_chord_curv(i,jj)
            end do
#ifdef FROM_ESP
            call override_cur4(ncp_span_curv, temp)
#endif
            do i = 1, ncp_span_curv
               cp_chord_curv(i,jj) = temp(i)
            end do

        end if

        !
        ! Override cur5 if needed
        ! override_cur5() in 3dbgb.f90
        ! Callback to override_cur5_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_curvature >= 4) then
           
            if (deprecated) then
                jj = 1 + ncp_chord - 2 + 5
            else 
                jj = 1 + ncp_chord - 2 + 5 - 1
            end if
            
            do i = 1, ncp_span_curv
               temp(i) = cp_chord_curv(i,jj)
            end do
#ifdef FROM_ESP
            call override_cur5(ncp_span_curv, temp)
#endif
            do i = 1, ncp_span_curv
               cp_chord_curv(i,jj) = temp(i)
            end do

        end if

        !
        ! Override cur6 if needed
        ! override_cur6() in 3dbgb.f90
        ! Callback to override_cur6_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_curvature >= 5) then
           
            if (deprecated) then
                jj = 1 + ncp_chord - 2 + 6
            else 
                jj = 1 + ncp_chord - 2 + 6 - 1
            end if
            
            do i = 1, ncp_span_curv
               temp(i) = cp_chord_curv(i,jj)
            end do
#ifdef FROM_ESP
            call override_cur6(ncp_span_curv, temp)
#endif
            do i = 1, ncp_span_curv
               cp_chord_curv(i,jj) = temp(i)
            end do

        end if

        !
        ! Override cur7 if needed
        ! override_cur7() in 3dbgb.f90
        ! Callback to override_cur7_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_curvature >= 6) then
           
            if (deprecated) then
                jj = 1 + ncp_chord - 2 + 7
            else 
                jj = 1 + ncp_chord - 2 + 7 - 1 
            end if

            do i = 1, ncp_span_curv
               temp(i) = cp_chord_curv(i,jj)
            end do
#ifdef FROM_ESP
            call override_cur7(ncp_span_curv, temp)
#endif
            do i = 1, ncp_span_curv
               cp_chord_curv(i,jj) = temp(i)
            end do

        end if

        !
        ! Override u2 if needed
        ! override_u2() in 3dbgb.f90
        ! Callback to override_u2_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_chord >= 1) then
            
            jj = 1 + 2-1
            do i = 1, ncp_span_curv
               temp(i) = cp_chord_curv(i,jj)
            end do
#ifdef FROM_ESP
            call override_u2(ncp_span_curv, temp)
#endif
            do i = 1, ncp_span_curv
               cp_chord_curv(i,jj) = temp(i)
            end do

        end if

        !
        ! Override u3 if needed
        ! override_u3() in 3dbgb.f90
        ! Callback to override_u3_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_chord >= 2) then
            
            jj = 1 + 3-1
            do i = 1, ncp_span_curv
               temp(i) = cp_chord_curv(i,jj)
            end do
#ifdef FROM_ESP
            call override_u3(ncp_span_curv, temp)
#endif
            do i = 1, ncp_span_curv
               cp_chord_curv(i,jj) = temp(i)
            end do

        end if

        !
        ! Override u4 if needed
        ! override_u4() in 3dbgb.f90
        ! Callback to override_u4_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_chord >= 3) then
            
            jj = 1 + 4-1
            do i = 1, ncp_span_curv
               temp(i) = cp_chord_curv(i,jj)
            end do
#ifdef FROM_ESP
            call override_u4(ncp_span_curv, temp)
#endif
            do i = 1, ncp_span_curv
               cp_chord_curv(i,jj) = temp(i)
            end do

        end if
        
        !
        ! Override u5 if needed
        ! override_u5() in 3dbgb.f90
        ! Callback to override_u5_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_chord >= 4) then
            
            jj = 1 +5-1
            do i = 1, ncp_span_curv
               temp(i) = cp_chord_curv(i,jj)
            end do
#ifdef FROM_ESP
            call override_u5(ncp_span_curv, temp)
#endif
            do i = 1, ncp_span_curv
               cp_chord_curv(i,jj) = temp(i)
            end do

        end if

        !
        ! Override u6 if needed
        ! override_u6() in 3dbgb.f90
        ! Callback to override_u6_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_chord >= 5) then
            
            jj = 1 +6-1
            do i = 1, ncp_span_curv
               temp(i) = cp_chord_curv(i,jj)
            end do
#ifdef FROM_ESP
            call override_u6(ncp_span_curv, temp)
#endif
            do i = 1, ncp_span_curv
               cp_chord_curv(i,jj) = temp(i)
            end do

         endif

    endif   ! control_inp_flag



    !
    ! Read thickness control table
    !
    if (thick /= 0 .or. LE /= 0) then !.or. thick_distr == 3  .or. thick_distr == 4) then
        
        read(10,'(A)') temps
        write(nopen1,'(A)') trim(temps)
        read(10,'(A)') temps
        write(nopen1,'(A)') trim(temps)
       


        !
        ! Read number of spanwise and chordwise control points
        !
        read(10, *)ncp_span_thk, ncp_chord_thickness
        backspace(10)
        read(10,'(A)') temps
        write(nopen1,'(A)') temps

        !end if
        
        if(control_inp_flag == 1 .and. ncp_span_thk /= nsl) then
            error_msg   = 'In auxiliary file inputs, number of spanwise thickness specifications'//&
                          ' must equal number of streamlines if spanwise spline is not used.'
            dev_msg     = 'Check subroutine read_spanwise_input in readinput.f90'
            call fatal_error(error_msg, dev_msg = dev_msg)
        end if
       
       
        
        !
        ! Number of chord and thickness control points will always be the same
        !
        ncp_thickness = ncp_chord_thickness
        ncp_span_thk1 = ncp_span_thk + 2



        !
        ! Determine number of control points along the chord
        !
        ncp_chord_thk = ncp_chord_thickness - 2 + ncp_thickness - 2 + 1

        
        
        !
        ! Allocate and initialize thickness control points arrays
        !
        if (allocated(ncp_thk)) deallocate(ncp_thk)
        allocate(ncp_thk(nsl))
        if (allocated(thk_cp)) deallocate(thk_cp)
        allocate(thk_cp(20, 2*nsl))
        
        do i = 1, nsl
            ncp_thk(i) = ncp_thickness + 4
        end do



        !
        ! Allocate array to store thickness control points table read from auxiliary input file
        !
        if (allocated(cp_chord_thk)) deallocate(cp_chord_thk)
        allocate(cp_chord_thk(ncp_span_thk, ncp_chord_thk))

        !
        ! Allocate array to store leading edge angle control points read from auxiliary input file
        !
        if (allocated(le_angle_cp)) deallocate(le_angle_cp)
        allocate(le_angle_cp(ncp_span_thk))
        
        !
        ! Allocate array to store trailing edge angle control points read from auxiliary input file
        !
        if (allocated(te_angle_cp)) deallocate(te_angle_cp)
        allocate(te_angle_cp(ncp_span_thk))



        !
        ! Read thickness control points table
        !
        read(10,'(A)') temps
        write(nopen1,'(A)') temps
        
        do i = 1, ncp_span_thk
            
            ! Allocate 1D thickness control points arrays
            if (allocated(xcp)) deallocate(xcp)
            allocate(xcp(ncp_thk(i)))
            if (allocated(ycp)) deallocate(ycp)
            allocate(ycp(ncp_thk(i)))
            
            ! Using quartic spline thickness distribution
            read(10, *) cp_chord_thk(i, 1:ncp_chord_thickness)
            backspace(10)
            read(10,'(A)') temps
            write(nopen1,'(A)') temps
            
            ! If spanwise splining is not required, store in 1D arrays
            if(control_inp_flag == 1) then

                xcp(1) = 2*xcp(3)-xcp(5)
                ycp(1) = 2*ycp(3)-ycp(5)
                xcp(2) = 2*xcp(3)-xcp(4)
                ycp(2) = 2*ycp(3)-ycp(4)
                xcp(ncp_thk(i)) = 2*xcp(ncp_thk(i)-2)-xcp(ncp_thk(i)-4) 
                ycp(ncp_thk(i)) = 2*ycp(ncp_thk(i)-2)-ycp(ncp_thk(i)-4)
                xcp(ncp_thk(i)-1) = 2*xcp(ncp_thk(i)-2)-xcp(ncp_thk(i)-3)
                ycp(ncp_thk(i)-1) = 2*ycp(ncp_thk(i)-2)-ycp(ncp_thk(i)-3)

            end if
            
            ! Store in global variable thk_cp
            do k = 1, ncp_thk(i)
                thk_cp(k, 2*i - 1) = xcp(k)
                thk_cp(k, 2*i)     = ycp(k)
            end do
        
        end do  ! i = 1,ncp_span_thk
        
        
        
        !
        ! Read LE control parameters if spline LE is being used
        ! TODO: To be removed
        !
        !if (LE /= 0) then

        !    ! Read descriptor lines
        !    read(10,'(A)') temps
        !    write(nopen1,'(A)') temps
        !    read(10,'(A)') temps
        !    write(nopen1,'(A)') temps
        !    read(10,'(A)') temps
        !    write(nopen1,'(A)') temps

        !    ! Read LE spline degree and no. of LE spline segments
        !    read(10, *)LE_deg, LE_seg
        !    backspace(10)
        !    read(10,'(A)') temps

        !    ! Read descriptor lines
        !    write(nopen1,'(A)') temps
        !    read(10,'(A)') temps
        !    write(nopen1,'(A)') temps

        !    ! Read number of spanwise spline LE control points
        !    read(10, *)ncp_span_LE
        !    backspace(10)
        !    read(10,'(A)') temps
        !    write(nopen1,'(A)') temps

        !    ! Number of spline LE parameters = 13
        !    ncp_LE = 13 
        !    
        !    ! Giving the same variable name as in controlinputs
        !    LEdegree       = LE_deg
        !    no_LE_segments = LE_seg

        !    ! Account for phantom points
        !    ncp_span_LE1 = ncp_span_LE + 2

        !    ! Allocate array to store spline LE control points read from auxiliary input file
        !    if (allocated(cp_LE)) deallocate(cp_LE)
        !    allocate(cp_LE(ncp_span_LE, ncp_LE+1))

        !    ! Read spline LE control points table
        !    read(10,'(A)') temps
        !    write(nopen1,'(A)') temps
        !    do i = 1, ncp_span_LE

        !        read(10, *)cp_LE(i, 1:ncp_LE+1)
        !        backspace(10)
        !        read(10,'(A)') temps
        !        write(nopen1,'(A)') temps

        !    end do
        !end if  ! if (LE /= 0)
    
    end if  ! if (thick /= 0 .or. LE /= 0)



    !
    ! Close auxiliary input file and auxiliary log file
    !
    close(10)
    call close_auxinput_log_file(nopen1, file_open_1)



    !
    ! Determine if 'isquiet' command line argument is being used
    !
    call get_quiet_status(isquiet_local)



    !
    ! Print message to screen and write to run log file
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet_local) print*, 'Spanwise input file read successfully'
    write(nopen,*) 'Spanwise input file read successfully'
    call close_log_file(nopen, file_open)


end subroutine read_spanwise_input
!------------------------------------------------------------------------------------------------------------






!
! This subroutine reads the auxiliary input file (spancontrolinputs_NACA_bladerow.dat)
! Only called when using modified NACA four-digit thickness distribution
! Stores input in several global variables defined in globvar.f90
!
! Input parameters: row_type - T-Blade3 bladerow type
!                   path     - path of the directory where the auxiliary input file is
! 
!--------------------------------------------------------------------------------------------------
subroutine read_spanwise_NACA_input(row_type,path)
    use globvar
    use file_operations
    use errors
    use funcNsubs
    implicit none

    character(256),                 intent(in)      :: row_type
    character(*),                   intent(in)      :: path

    ! Local variables
    character(:),   allocatable                     :: file_name, log_file, error_msg, warning_msg, dev_msg
    character(256)                                  :: temps, warning_arg_1, warning_arg_2, warning_arg_3
    integer                                         :: nopen_aux = 10, nopen, nopen1, kk, n_temp, n_temp_1
    real                                            :: span_dum
    real,           allocatable                     :: temp(:)
    logical                                         :: file_open, file_open_1, isquiet_local



    !
    ! Read auxiliary input file name
    !
    file_name       = trim(path)//'spancontrolinputs.'//trim(row_type)//'.dat'



    !
    ! Open auxiliary input file and auxiliary input log file
    !
    call open_auxinput_log_file(trim(adjustl(file_name)), nopen1, file_open_1)
    open(nopen_aux, file = file_name)
    rewind(nopen_aux)


    !
    ! Read casename and blade row number
    !
    do i = 1,5
        read(nopen_aux,'(A)') temps
        write(nopen1,'(A)') trim(temps)
    end do



    !
    ! Read number of curvature and chord control points
    !
    read(nopen_aux,*) ncp_span_curv, ncp_chord
    backspace(nopen_aux)
    read(nopen_aux,'(A)') temps
    write(nopen1,'(A)') trim(temps)



    !
    ! Number of chord and curvature control points will always be the same
    !
    ncp_curvature                   = ncp_chord



    !
    ! Allocate and initialize curvature control points arrays
    ! 
    if (allocated(ncp_curv)) deallocate(ncp_curv)
    allocate(ncp_curv(nsl))
    do i = 1,nsl
        ncp_curv(i)                 = ncp_curvature + 2
    end do

    if (allocated(curv_cp)) deallocate(curv_cp)
    if (control_inp_flag == 1) allocate(curv_cp(20, 2*nsl))



    !
    ! Compute total size of a curvature and chord control points row
    ! Allocate curvature control points array
    !
    ncp_chord_curv                  = ncp_chord - 2 + ncp_curvature + 1
    if (allocated(cp_chord_curv)) deallocate(cp_chord_curv)
    allocate(cp_chord_curv(ncp_span_curv, ncp_chord_curv))

    read(nopen_aux,'(A)') temps

    ! If control table for cur1 is not found, raise a fatal error
    if (index(trim(temps), 'cur1') == 0) then
        error_msg   = 'Incorrect auxiliary file format'
        warning_msg = 'This is an older format of the spancontrolinputs file'
        dev_msg     = 'Check subroutine read_spanwise_NACA_input in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)
    end if
    using_cur1  = .true.
    write(nopen1,'(A)') trim(temps)



    !
    ! Read curvature control points
    !
    do i = 1,ncp_span_curv
        
        read(nopen_aux,*) cp_chord_curv(i,:)
        backspace(nopen_aux)
        read(nopen_aux,'(A)') temps
        write(nopen1,'(A)') temps

        ! Check for monotonicity of curvature control points
        do j = 2, ncp_chord - 2

            if (cp_chord_curv(i, j + 1) < cp_chord_curv(i, j)) then

                ! Error message
                error_msg       = 'Non-monotonic distribution of control points for &
                                  &mean-line second derivative in spancontrolinputs file'

                ! Warning message showing where non-monotonicity was encountered
                write (warning_arg_1, '(i0)') j + 1
                write (warning_arg_2, '(i0)') j
                write (warning_arg_3, '(i0)') i
                warning_msg     = 'u'//trim(warning_arg_1)//' < u'//trim(warning_arg_2)//&
                                 &' for spanwise location '//trim(warning_arg_3)

                ! Developer message
                dev_msg         = 'Check subroutine read_spanwise_NACA_input in readinput.f90'

                ! Raise fatal error if control points are non-monotonic
                call fatal_error (error_msg, warning_msg, dev_msg)

            end if

        end do

        ! If spanwise splining is not required, store in arrays
        if (control_inp_flag == 1) then
            
            ! Allocate 1D curvature control points arrays
            if (allocated(xcp) .and. allocated(ycp)) deallocate(xcp,ycp)
            allocate(xcp(ncp_curv(i)), ycp(ncp_curv(i)))

            span_dum                = cp_chord_curv(i,1)
            xcp(3:ncp_curv(i) - 2)  = cp_chord_curv(i, 2:ncp_chord - 1)
            ycp(2:ncp_curv(i) - 1)  = cp_chord_curv(i, ncp_chord:ncp_chord_curv)
            xcp(1)                  = 2*xcp(2) - xcp(3)
            xcp(2)                  = 0.0
            ycp(1)                  = 2*ycp(2) - ycp(3)
            xcp(ncp_curv(i) - 1)    = 1.0
            xcp(ncp_curv(i))        = 2*xcp(ncp_curv(i) - 1) - xcp(ncp_curv(i) - 2)
            ycp(ncp_curv(i))        = 2*ycp(ncp_curv(i) - 1) - ycp(ncp_curv(i) - 2)

            ! Store in global variable curv_vp
            do k = 1,ncp_curv(i)
                curv_cp(k, 2*i - 1) = xcp(k)
                curv_cp(k, 2*i)     = ycp(k)
            end do ! k

        end if  ! control_inp_flag

    end do  ! ncp_span_curv



    !
    ! ESP override subroutines for curvature control
    !
    if (control_inp_flag == 2) then
        if (allocated(temp)) deallocate(temp)
        allocate(temp(ncp_span_curv))

        !
        ! Override span_curv_ctrl
        ! override_span_curv_ctrl() in 3dbgb.f90
        ! Callback to override_span_curv_ctrl_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        kk                          = 1
        do ii   = 1,ncp_span_curv
            temp(ii)                = cp_chord_curv(ii,kk)
        end do  
#ifdef FROM_ESP
        call override_span_curv_ctrl(ncp_span_curv,temp)
#endif
        do ii   = 1,ncp_span_curv
            cp_chord_curv(ii,kk)    = temp(ii)
        end do

        !
        ! Override u2 if needed
        ! override_u2() in 3dbgb.f90
        ! Callback to override_u2_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_chord >= 1) then
            
            kk                      = 2
            do ii = 1,ncp_span_curv
                temp(ii)            = cp_chord_curv(ii,kk)
            end do
#ifdef FROM_ESP
            call override_u2(ncp_span_curv,temp)
#endif
            do ii = 1,ncp_span_curv
                cp_chord_curv(ii,kk)= temp(ii)
            end do

        end if

        !
        ! Override u3 if needed
        ! override_u3() in 3dbgb.f90
        ! Callback to override_u3_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_chord >= 2) then
            
            kk                      = 3
            do ii = 1,ncp_span_curv
                temp(ii)            = cp_chord_curv(ii,kk)
            end do
#ifdef FROM_ESP
            call override_u3(ncp_span_curv,temp)
#endif
            do ii = 1,ncp_span_curv
                cp_chord_curv(ii,kk)= temp(ii)
            end do

        end if

        !
        ! Override u4 if needed
        ! override_u4() in 3dbgb.f90
        ! Callback to override_u4_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_chord >= 3) then

            kk                      = 4
            do ii = 1,ncp_span_curv
                temp(ii)            = cp_chord_curv(ii,kk)
            end do
#ifdef FROM_ESP
            call override_u4(ncp_span_curv,temp)
#endif
            do ii = 1,ncp_span_curv
                cp_chord_curv(ii,kk)= temp(ii)
            end do

        end if

        !
        ! Override u5 if needed
        ! override_u5() in 3dbgb.f90
        ! Callback to override_u5_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_chord >= 4) then

            kk                      = 5
            do ii = 1,ncp_span_curv
                temp(ii)            = cp_chord_curv(ii,kk)
            end do
#ifdef FROM_ESP
            call override_u5(ncp_span_curv,temp)
#endif
            do ii = 1,ncp_span_curv
                cp_chord_curv(ii,kk)= temp(ii)
            end do

        end if
         
        !       
        ! Override u6 if needed
        ! override_exact_u6() in 3dbgb.f90
        ! Callback to override_exact_u6_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_chord >= 5) then

            kk                      = 6
            do ii = 1,ncp_span_curv
                temp(ii)            = cp_chord_curv(ii,kk)
            end do
#ifdef FROM_ESP
            call override_u6(ncp_span_curv,temp)
#endif
            do ii = 1,ncp_span_curv
                cp_chord_curv(ii,kk)= temp(ii)
            end do

        end if

        !
        ! Override cur1 if needed
        ! override_cur1() in 3dbgb.f90
        ! Callback to override_cur1_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_curvature >= 1) then

            kk                      = ncp_chord 
            do ii = 1,ncp_span_curv
                temp(ii)            = cp_chord_curv(ii,kk)
            end do
#ifdef FROM_ESP
            call override_cur1(ncp_span_curv,temp)
#endif
            do ii = 1,ncp_span_curv
                cp_chord_curv(ii,kk)= temp(ii)
            end do

        end if
        
        !
        ! Override cur2 if needed
        ! override_cur2() in 3dbgb.f90
        ! Callback to override_cur2_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_curvature >= 2) then

            kk                      = ncp_chord + 1
            do ii = 1,ncp_span_curv
                temp(ii)            = cp_chord_curv(ii,kk)
            end do
#ifdef FROM_ESP
            call override_cur2(ncp_span_curv,temp)
#endif
            do ii = 1,ncp_span_curv
                cp_chord_curv(ii,kk)= temp(ii)
            end do

        end if
        
        !
        ! Override cur3 if needed
        ! override_cur3() in 3dbgb.f90
        ! Callback to override_cur3_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_curvature >= 3) then

            kk                      = ncp_chord + 2
            do ii = 1,ncp_span_curv
                temp(ii)            = cp_chord_curv(ii,kk)
            end do
#ifdef FROM_ESP
            call override_cur3(ncp_span_curv,temp)
#endif
            do ii = 1,ncp_span_curv
                cp_chord_curv(ii,kk)= temp(ii)
            end do

        end if

        !
        ! Override cur4 if needed
        ! override_cur4() in 3dbgb.f90
        ! Callback to override_cur4_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_curvature >= 4) then

            kk                      = ncp_chord + 3
            do ii = 1,ncp_span_curv
                temp(ii)            = cp_chord_curv(ii,kk)
            end do
#ifdef FROM_ESP
            call override_cur4(ncp_span_curv,temp)
#endif
            do ii = 1,ncp_span_curv
                cp_chord_curv(ii,kk)= temp(ii)
            end do

        end if

        !
        ! Override cur5 if needed
        ! override_cur5() in 3dbgb.f90
        ! Callback to override_cur5_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_curvature >= 5) then

            kk                      = ncp_chord + 4
            do ii = 1,ncp_span_curv
                temp(ii)            = cp_chord_curv(ii,kk)
            end do
#ifdef FROM_ESP
            call override_cur5(ncp_span_curv,temp)
#endif
            do ii = 1,ncp_span_curv
                cp_chord_curv(ii,kk)= temp(ii)
            end do

        end if

        !
        ! Override cur6 if needed
        ! override_cur6() in 3dbgb.f90
        ! Callback to override_cur6_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_curvature >= 6) then

            kk                      = ncp_chord + 5
            do ii = 1,ncp_span_curv
                temp(ii)            = cp_chord_curv(ii,kk)
            end do
#ifdef FROM_ESP
            call override_cur6(ncp_span_curv,temp)
#endif
            do ii = 1,ncp_span_curv
                cp_chord_curv(ii,kk)= temp(ii)
            end do

        end if

        !
        ! Override cur7 if needed
        ! override_cur7() in 3dbgb.f90
        ! Callback to override_cur7_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        if (ncp_curvature >= 7) then

            kk                      = ncp_chord + 6
            do ii = 1,ncp_span_curv
                temp(ii)            = cp_chord_curv(ii,kk)
            end do
#ifdef FROM_ESP
            call override_cur6(ncp_span_curv,temp)
#endif
            do ii = 1,ncp_span_curv
                cp_chord_curv(ii,kk)= temp(ii)
            end do

        end if

    end if  ! control_inp_flag



    ! 
    ! Read the thickness part of the auxiliary input file
    ! Only applies for the modified NACA thickness distribution
    !
    if (thick_distr /= 5) then
        error_msg   = "Incorrect auxiliary file format"
        warning_msg = "Refer to T-Blade3 documentation"
        dev_msg     = 'Check read_spanwise_NACA_input in readinput.f90'
        call fatal_error(error_msg, warning_msg, dev_msg)
    else
        do i = 1,2
            read(nopen_aux,'(A)') temps
            write(nopen1,'(A)') trim(temps)
        end do

        ! Read no. of thickness control points along span and spline switch 
        read(nopen_aux,*) ncp_span_thk, spline_switch
        backspace(nopen_aux) 
        read(nopen_aux,'(A)') temps
        write(nopen1,'(A)') trim(temps)

        ! Read thickness control points array descriptor
        read(nopen_aux,'(A)') temps
        write(nopen1,'(A)') trim(temps)
        
        ! Search for substring to determine how the TE angle has
        ! been specified
        n_temp   = index(trim(temps), 'dy_dx_TE')
        n_temp_1 = index(trim(temps), 'dy_dx_norm')

        ! Allocate thickness control points array
        if (allocated(cp_chord_thk)) deallocate(cp_chord_thk)

        ! Default TE derivative definition
        if (n_temp == 0 .and. n_temp_1 == 0) then

            TE_der_actual  = .false.
            TE_der_norm    = .false.
            ncp_chord_thk = 5
            allocate( cp_chord_thk(ncp_span_thk, ncp_chord_thk) )

        ! Actual TE derivative definition
        else if (n_temp /= 0 .and. n_temp_1 == 0) then

            TE_der_actual  = .true.
            TE_der_norm    = .false.
            ncp_chord_thk = 6
            allocate( cp_chord_thk(ncp_span_thk, ncp_chord_thk) )

        ! Normalized TE derivative definition
        else if (n_temp == 0 .and. n_temp_1 /= 0) then

            TE_der_actual  = .false.
            TE_der_norm    = .true.
            ncp_chord_thk = 6
            allocate ( cp_chord_thk(ncp_span_thk, ncp_chord_thk) )

        ! Raise an error if TE derivative is defined both directly
        ! and as a normalized quantity
        else if ( n_temp /= 0 .and. n_temp_1 /= 0) then

            error_msg   = 'Conflicting definitions of TE derivative found.'
            warning_msg = 'Refer to T-Blade3 documentation'
            dev_msg     = 'Check subroutine read_spanwise_NACA_input in readinput.f90'
            call fatal_error (error_msg, warning_msg, dev_msg)

        end if

        ! Read thickness control points
        do i = 1,ncp_span_thk

            read(nopen_aux,*) cp_chord_thk(i,:)
            !do j = 4,5
            !    cp_chord_thk(i,j)   = 0.5*cp_chord_thk(i,j)
            !end do
            backspace(nopen_aux)
            read(nopen_aux,'(A)') temps
            write(nopen1,'(A)') trim(temps)

        end do  ! ncp_span_thk

    end if  ! thick_distr



    !
    ! Assign array ncp_thk for bladegen subroutine
    !
    if (allocated(ncp_thk)) deallocate(ncp_thk)
    allocate (ncp_thk(nsl))
    do i = 1, nsl
        ncp_thk(i) = size(cp_chord_thk, 2)
    end do



    !
    ! Close auxiliary input file and auxiliary input log file
    !
    close(nopen1)
    close(nopen_aux)



    !
    ! ESP override subroutines for thickness control
    !
    if (control_inp_flag == 2) then
        if (allocated(temp)) deallocate(temp)
        allocate(temp(ncp_span_thk))

        !
        ! Override span_thk_ctrl
        ! override_span_thk_ctrl() in 3dbgb.f90
        ! Callback to override_span_thk_ctrl_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        kk                      = 1
        do ii = 1,ncp_span_thk
            temp(ii)            = cp_chord_thk(ii,kk)
        end do
#ifdef FROM_ESP
        call override_span_thk_ctrl(ncp_span_thk,temp)
#endif
        do ii = 1,ncp_span_thk
            cp_chord_thk(ii,kk) = temp(ii)
        end do

        !
        ! Override naca_le_radius
        ! override_naca_le_radius() in 3dbgb.f90
        ! Callback to override_naca_le_radius_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        kk                      = 2
        do ii = 1,ncp_span_thk
            temp(ii)            = cp_chord_thk(ii,kk)
        end do
#ifdef FROM_ESP
        call override_naca_le_radius(ncp_span_thk,temp)
#endif
        do ii = 1,ncp_span_thk
            cp_chord_thk(ii,kk) = temp(ii)
        end do

        !
        ! Override naca_u_max
        ! override_naca_u_max() in 3dbgb.f90
        ! Callback to override_naca_u_max_() in udpTblade.c, udpHubWedge.c and  udpBladeVolume.c
        !
        kk                      = 3
        do ii = 1,ncp_span_thk
            temp(ii)            = cp_chord_thk(ii,kk)
        end do
#ifdef FROM_ESP
        call override_naca_u_max(ncp_span_thk,temp)
#endif
        do ii = 1,ncp_span_thk
            cp_chord_thk(ii,kk) = temp(ii)
        end do

        !
        ! Override naca_t_max
        ! override_naca_t_max() in 3dbgb.f90
        ! Callback to override_naca_t_max_() in 3dbgb.f90
        !
        kk                      = 4
        do ii = 1,ncp_span_thk
            temp(ii)            = cp_chord_thk(ii,kk)
        end do
#ifdef FROM_ESP
        call override_naca_t_max(ncp_span_thk,temp)
#endif
        do ii = 1,ncp_span_thk
            cp_chord_thk(ii,kk) = temp(ii)
        end do

        !
        ! Override naca_t_te
        ! override_naca_t_te() in 3dbgb.f90
        ! Callback to override_naca_t_te_() in udpTblade.c, udpHubWedge.c and udpBladeVolume.c
        !
        kk                      = 5
        do ii = 1,ncp_span_thk
            temp(ii)            = cp_chord_thk(ii,kk)
        end do
#ifdef FROM_ESP
        call override_naca_t_te(ncp_span_thk,temp)
#endif
        do ii = 1,ncp_span_thk
            cp_chord_thk(ii,kk) = temp(ii)
        end do

    end if  ! control_inp_flag



    !
    ! Determine if 'isquiet' command line argument is being used
    !
    call get_quiet_status(isquiet_local)



    !
    ! Print message to screen and write to log file
    !
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet_local) print *, "NACA spanwise input file read successfully"
    write(nopen,*) "NACA spanwise input file read successfully"
    call close_log_file(nopen, file_open)


end subroutine read_spanwise_NACA_input
!--------------------------------------------------------------------------------------------------





















