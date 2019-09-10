module file_operations
    implicit none



    contains



    !
    ! Subroutine for checking if the log file exists or not
    ! If exists, open as a file to append
    ! If doesn't exists, open as a new file
    !
    !---------------------------------------------------------------------------
    subroutine log_file_exists(log_file, nopen, file_open, initial)
        
        character(len = :), allocatable,    intent(inout)   :: log_file
        integer,                            intent(inout)   :: nopen
        logical,                            intent(inout)   :: file_open
        logical,    optional,               intent(in)      :: initial

        ! Local variables
        logical             :: exist
        integer             :: ierr


        log_file    = 'T-Blade3_run.log'
        nopen       = 101

        inquire(file = log_file, exist=exist)
        if (exist) then
            if (present(initial) .and. initial .eqv. .true.) then
                open(nopen, file = log_file, status = 'old', action = 'write')
            else 
                open(nopen, file = log_file, status = 'old', iostat = ierr, position = 'append', action = 'write')
            end if
        else
            open(nopen, file = log_file, status = 'new', iostat = ierr, action = 'write')
        end if

        if (ierr == 0) then
            file_open = .true.  
        else
            file_open = .false.
        end if


    end subroutine log_file_exists
    !---------------------------------------------------------------------------






    !
    ! Close log file if it is open
    !
    ! Input parameters: nopen       - unit number for closing file
    !                   file_open   - close the file only if it is open
    !
    !---------------------------------------------------------------------------
    subroutine close_log_file(nopen, file_open)

        integer,        intent(in)      :: nopen
        logical,        intent(in)      :: file_open


        ! If the log file is open, close it
        if (file_open) close(nopen)


    end subroutine close_log_file
    !---------------------------------------------------------------------------






    !
    ! Subroutine for checking if the input log file exists or not
    ! If exists, open as a file to append
    ! If doesn't exist, open as a new file
    !
    !---------------------------------------------------------------------------
    subroutine open_maininput_log_file(input_file, nopen, file_open)
        
        character(*),                   intent(in)          :: input_file
        integer,                        intent(inout)       :: nopen
        logical,                        intent(inout)       :: file_open

        ! Local variables
        character(100)   :: maininput_log_file
        logical         :: exist
        integer         :: ierr, index1


        nopen               = 102
        index1              = index(input_file, 'dat')
        maininput_log_file  = input_file(:index1 - 1)//'log'

        inquire(file = trim(maininput_log_file), exist=exist)
        if (exist) then
            open(nopen, file = trim(maininput_log_file), status = 'old', iostat = ierr, action = 'write')
        else
            open(nopen, file = trim(maininput_log_file), status = 'new', iostat = ierr, action = 'write')
        end if

        if (ierr == 0) then
            file_open = .true.
        else
            file_open = .false.
        end if


    end subroutine open_maininput_log_file
    !---------------------------------------------------------------------------






    !
    ! Close input log file if open
    !
    ! Input parameters: nopen       - unit number for closing the file
    !                   file_open   - only close file if it is open
    !
    !---------------------------------------------------------------------------
    subroutine close_maininput_log_file(nopen, file_open)

        integer,                    intent(in)              :: nopen
        logical,                    intent(in)              :: file_open

        ! If the file is open, close it
        if (file_open) close(nopen)


    end subroutine close_maininput_log_file
    !---------------------------------------------------------------------------






    !
    ! Subroutine for checking if the input log file exists or not
    ! If exists, open as a file to append
    ! If doesn't exist, open as a new file
    !
    !---------------------------------------------------------------------------
    subroutine open_auxinput_log_file(input_file, nopen, file_open)
        
        character(*),               intent(in)              :: input_file
        integer,                    intent(inout)           :: nopen
        logical,                    intent(inout)           :: file_open
        
        ! Local variables
        character(100)       :: auxinput_log_file
        logical             :: exist
        integer             :: ierr, index1
        
        
        nopen               = 102
        index1              = index(input_file, 'dat')
        auxinput_log_file   = input_file(:index1 - 1)//'log'
        
        inquire(file = trim(auxinput_log_file), exist=exist)
        if (exist) then
            open(nopen, file = trim(auxinput_log_file), status = 'old', iostat = ierr, action = 'write')
        else
            open(nopen, file = trim(auxinput_log_file), status = 'new', iostat = ierr, action = 'write')
        end if

        if (ierr == 0) then
            file_open = .true.
        else
            file_open = .false.
        end if


    end subroutine open_auxinput_log_file
    !---------------------------------------------------------------------------






    !
    ! Close input log file if open
    !
    ! Input parameters: nopen       - unit number for closing the file
    !                   file_open   - only close file if it is open
    !
    !---------------------------------------------------------------------------
    subroutine close_auxinput_log_file(nopen, file_open)

        integer,                intent(in)              :: nopen
        logical,                intent(in)              :: file_open


        ! If the file is open, close it
        if (file_open) close(nopen)


    end subroutine close_auxinput_log_file
    !---------------------------------------------------------------------------




    
    
    !
    ! Subroutine for checking if the error log file exists or not
    ! If exists, open as a file to append
    ! If doesn't exists, open as a new file
    !
    !---------------------------------------------------------------------------
    subroutine error_file_exists(error_file, nopen, file_open, initial)
        
        character(:),   allocatable,        intent(inout)   :: error_file
        integer,                            intent(inout)   :: nopen
        logical,                            intent(inout)   :: file_open
        logical,    optional,               intent(in)      :: initial

        ! Local variables
        logical                                             :: exist
        integer                                             :: ierr


        error_file  = 'error.log'
        nopen       = 953

        inquire(file = error_file, exist=exist)
        if (exist) then
            if (present(initial) .and. initial .eqv. .true.) then
                open(nopen, file = error_file, status = 'old', iostat = ierr, action = 'write')
            else
                open(nopen, file = error_file, status = 'old', iostat = ierr, position = 'append', action = 'write')
            end if
        else
            open(nopen, file =  error_file, status = 'new', iostat = ierr, action = 'write')
        end if

        if (ierr == 0) then
            file_open = .true.
        else
            file_open = .false.
        end if


    end subroutine error_file_exists
    !---------------------------------------------------------------------------




    
    
    !
    ! Close error log file if it is open
    !
    ! Input parameters: nopen       - unit number for closing the file
    !                   file_open   - only close the file if it is open
    !
    !---------------------------------------------------------------------------
    subroutine close_error_file(nopen, file_open)

        integer,        intent(in)      :: nopen
        logical,        intent(in)      :: file_open


        ! If the log file is open, close it
        if (file_open) close(nopen)


    end subroutine close_error_file
    !---------------------------------------------------------------------------




    
    
    !
    ! Write 2D array in matrix form to a file
    ! Specified for (x,y) grid coordinates 
    !
    ! Input parameters: fname   - file name
    !                   nx      - number of x points
    !                   ny      - number of y points
    !                   X       - 2D array of x-coordinates
    !                   Y       - 2D array  of y-coordinates
    !
    !---------------------------------------------------------------------------
    subroutine file_write_matrix(fname,X,Y,nx,ny)
    
        character(32),          intent(in)              :: fname
        integer,                intent(in)              :: nx
        integer,                intent(in)              :: ny
        real,                   intent(in)              :: X(nx,ny)
        real,                   intent(in)              :: Y(nx,ny)

        ! Local variables
        integer                                         :: i, funit = 1


        !
        ! Open specified file and start writing to file
        !
        open(funit, file = fname, status = 'unknown')
        
        write(funit,*) nx, ny
        
        ! Write x coordinates
        write(funit,*) 'X coordinates'
        do i = 1,nx
            write(funit,*) X(i,:)
        end do
        write(funit,*) ''

        ! Write y coordinates
        write(funit,*) 'Y coordinates'
        do i = 1,nx
            write(funit,*) Y(i,:)
        end do
        
        close(funit)


    end subroutine file_write_matrix
    !---------------------------------------------------------------------------




    
    
    !
    ! Write 1D arrays to a file
    !
    ! Input parameters: fname   - file name
    !                   nx      - number of x points
    !                   X       - 1D array of x points
    !                   Y       - 1D array of y points
    !
    !---------------------------------------------------------------------------
    subroutine file_write_1D(fname,X,Y,nx)
    
        character(32),          intent(in)              :: fname
        integer,                intent(in)              :: nx
        real,                   intent(in)              :: X(nx)
        real,                   intent(in)              :: Y(nx)

        ! Local variables
        integer                                         :: i, funit = 1


        !
        ! Open specified file and start writing to file
        !
        open(funit, file = fname, status = 'unknown')
        do i = 1,nx
            write(funit,'(2F20.16)') X(i), Y(i)
        end do
        close(funit)


    end subroutine file_write_1D
    !---------------------------------------------------------------------------






    !
    ! Write the dimensional hub and casing streamline data to a file
    !
    !---------------------------------------------------------------------------
    subroutine hubTipStreamline(xhub,rhub,nphub,xtip,rtip,nptip,nsl,scf,casename)

        integer,                intent(in)              :: nphub, nptip
        real,                   intent(in)              :: xhub(nphub,1), rhub(nphub,1), &
                                                           xtip(nptip,1), rtip(nptip,1)
        integer,                intent(in)              :: nsl
        real,                   intent(in)              :: scf
        character(32),          intent(in)              :: casename

        ! Local variables
        integer                                         :: i, nopen
        character(80)                                   :: fname1, fname2
        character(:),   allocatable                     :: log_file
        logical                                         :: file_open, isquiet_local


        !
        ! Determine if command line argument 'isquiet' is being used
        !
        call get_quiet_status(isquiet_local)

        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet_local) print *, 'Writing dimensional hub and casing streamlines'
        write(nopen,*) 'Writing dimensional hub and casing streamlines'
        call close_log_file(nopen, file_open)

        ! Write hub streamline data
        fname1  = 'hub.'//trim(casename)//'.sldcrv'
        open(1, file = fname1, status = 'unknown')
        do i = 1,nphub
            write(1,*) scf*xhub(i,1), 0.0, scf*rhub(i,1)
        end do
        close(1)

        ! Write casing streamline data
        fname2  = 'casing.'//trim(casename)//'.sldcrv'
        open(2, file = fname2, status = 'unknown')
        do i = 1,nptip
            write(2,*) scf*xtip(i,1), 0.0, scf*rtip(i,1)
        end do
        close(2)


    end subroutine hubTipStreamline
    !---------------------------------------------------------------------------






    !
    ! Write dimensional streamline data (without hub or casing) to a file
    !
    !---------------------------------------------------------------------------
    subroutine streamlines(xml,rml,np,scf,casename,ia)

        integer,                    intent(in)          :: np
        real,                       intent(in)          :: xml(np), rml(np)
        real,                       intent(in)          :: scf
        character(32),              intent(in)          :: casename
        integer,                    intent(in)          :: ia

        ! Local variables
        integer                                         :: i, nopen
        character(32)                                   :: temp
        character(80)                                   :: fname
        character(:),   allocatable                     :: log_file
        logical                                         :: file_open, isquiet_local


        ! Get isquiet status
        call get_quiet_status(isquiet_local)

        write(temp,*) ia
        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet_local) print *, 'Writing dimensional streamline '//trim(adjustl(temp))
        write(nopen,*) 'Writing dimensional streamline '//trim(adjustl(temp))
        call close_log_file(nopen, file_open)

        ! Write data to file
        fname   = 'streamlines'//trim(adjustl(temp))//'.'//trim(casename)//'.sldcrv'
        open(1, file = fname, status = 'unknown')
        do i = 1,np
            write(1,*) scf*xml(i), 0.0, scf*rml(i)
        end do
        close(1)


    end subroutine streamlines
    !---------------------------------------------------------------------------
    





    !
    ! Write section properties to a file
    !
    !---------------------------------------------------------------------------
    subroutine outputfiledata(bladedata,nsl,amount_data,throat_pos,casename,units)

        integer,                    intent(in)          :: nsl, amount_data
        real,                       intent(in)          :: bladedata(amount_data,nsl)
        character(20),              intent(in)          :: throat_pos(nsl)
        character(32),              intent(in)          :: casename
        character(2),               intent(in)          :: units

        integer                                         :: js
        character(80)                                   :: file1


        file1 = 'blade_section_data.'//trim(casename)//'.dat'
        
        open(unit= 100, file = file1, form = "formatted")
        write(100,*)trim(casename)
        write(100,*)'Blade sections Data:'
        write(100,*)'---------------------'
        write(100,*) 

        do js = 1,nsl
            if (js == 1) then
              
                ! If blade scaling factor is specified in mm  
                if (units == 'mm') then
                  if (throat_pos(nsl) == 'le') then
                    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                                  'chord [mm]','sweep','lean','area[mm^2]','lethk [mm]','tethk [mm]','throat [mm]', &
                                  '(r*delta_theta)LE','Geom Zweifel','le/te/btween/none'
                  else if (throat_pos(nsl) == 'te') then
                    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                                  'chord [mm]','sweep','lean','area[mm^2]','lethk [mm]','tethk [mm]','throat [mm]', &
                                  '(r*delta_theta)TE','Geom Zweifel','le/te/btween/none'
                  else
                    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                                  'chord [mm]','sweep','lean','area[mm^2]','lethk [mm]','tethk [mm]','throat [mm]', &
                                  '(r*delta_theta)WT','Geom Zweifel','le/te/btween/none'
                  end if
              
               ! If blade scaling factor is specified in cm 
               else if (units == 'cm') then
                  if (throat_pos(nsl) == 'le') then
                    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                                  'chord [cm]','sweep','lean','area[cm^2]','lethk [cm]','tethk [cm]','throat [cm]', &
                                  '(r*delta_theta)LE','Geom Zweifel','le/te/btween/none'
                  else if (throat_pos(nsl) == 'te') then
                    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                                  'chord [cm]','sweep','lean','area[cm^2]','lethk [cm]','tethk [cm]','throat [cm]', &
                                  '(r*delta_theta)TE','Geom Zweifel','le/te/btween/none'
                  else
                    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                                  'chord [cm]','sweep','lean','area[cm^2]','lethk [cm]','tethk [cm]','throat [cm]', &
                                  '(r*delta_theta)WT','Geom Zweifel','le/te/btween/none'
                  end if

                ! If blade scaling factor is specified in m
                else if ((units == 'm ').or.(units == 'm)')) then
                  if (throat_pos(nsl) == 'le') then
                    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                                  'chord [ m]','sweep','lean','area[ m^2]','lethk [ m]','tethk [ m]','throat [ m]', &
                                  '(r*delta_theta)LE','Geom Zweifel','le/te/btween/none'
                  else if (throat_pos(nsl) == 'te') then
                    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                                  'chord [ m]','sweep','lean','area[ m^2]','lethk [ m]','tethk [ m]','throat [ m]', &
                                  '(r*delta_theta)TE','Geom Zweifel','le/te/btween/none'
                  else
                    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                                  'chord [ m]','sweep','lean','area[ m^2]','lethk [ m]','tethk [ m]','throat [ m]', &
                                  '(r*delta_theta)WT','Geom Zweifel','le/te/btween/none'
                  end if
                
                end if  ! units

            end if  ! if (js == 1)
            
            write(100,201) js,bladedata(1:(amount_data-1),js),throat_pos(js)

        end do  ! do js = 1,nsl

        write(100,*) 'Total Volume of the blade =',bladedata(amount_data,nsl),'[m^3]' 
        close(100)

        202 format((A7,2x),(A4,11x),4(A14,3x),(A13,1x),2(A6,5x),(A13,2x),(A10,2x),(A10,1x),(A11),2x,A17,2x,A12,2x,A17)
        201 format(1x,I3,4x,(f11.8,4x),4(sf14.8,3x),3x,8(f11.8,1x),7x,f11.8,4x,A5)

        
    end subroutine outputfiledata
    !---------------------------------------------------------------------------






    !
    ! Write non-dimensional cascade file
    !
    !---------------------------------------------------------------------------
    subroutine cascade_nondim_file(msle,mste,mprime_ble,mprime_bte,chordm,pitch, &
                                   nsl,ibrow,casename)

        integer,                    intent(in)      :: nsl, ibrow
        real,                       intent(in)      :: msle(nsl), mste(nsl), mprime_ble(nsl), &
                                                       mprime_bte(nsl), chordm(nsl), pitch
        character(32),              intent(in)      :: casename

        ! Local variables
        integer                                     :: ia
        character(80)                               :: file1    


        file1   = 'cascade_nondim.'//trim(casename)//'.dat'
        open(13, file = file1, status = 'unknown')
        write(13,*) trim(casename)
        write(13,*) 'Blade row: ', ibrow
        write(13,*) 'Non-dimensional quantities: '
        write(13,*) "section    m'sLE           m'sTE           m'blade_LE          m'blade_TE &
                                &chord           pitch      "

        do ia = 1,nsl
            write(13,101) ia, msle(ia), mste(ia), mprime_ble(ia), mprime_bte(ia), chordm(ia), pitch
        end do

        close(13)

        101 format(I2,2X,6(f19.16,1x))


    end subroutine cascade_nondim_file
    !---------------------------------------------------------------------------






    !
    ! Write (m',theta) blade section coordinates to a file
    !
    !---------------------------------------------------------------------------
    subroutine write_blade_files(np,nx,ii,fext,sinls,sexts,chrdd,pitch,xb,yb)

        integer,                    intent(in)      :: np, nx, ii
        character(32),              intent(in)      :: fext
        real,                       intent(in)      :: sinls, sexts, chrdd, pitch, xb(nx), yb(nx)

        ! Local variables
        character(:),   allocatable                 :: fname
        integer                                     :: funit = 2, i


        fname   = 'blade.'//trim(fext)

        open(funit, file = fname, status = 'unknown')
        write(funit,'(a)') trim(fext)
        write(funit,'(5(f19.16,1x))') sinls, sexts, 0.5*chrdd, chrdd, pitch
        do i = ii + 1,np - ii
            write(funit,*) xb(i), yb(i)
        end do
        close(funit)


    end subroutine write_blade_files
    !---------------------------------------------------------------------------






    !
    ! Write 2D throat data to a file
    !
    !---------------------------------------------------------------------------
    subroutine write_2D_throat_data(js,np,np_sidee,n_normal_distance,casename,u,camber,  &
                                    camber_upper,pitch_line,u1_top,v1_top,u2_bot,v2_bot, &
                                    inter_coord,min_throat_2D,throat_index)

        integer,                    intent(in)      :: js, np, np_sidee, n_normal_distance
        character(*),               intent(in)      :: casename
        real                                        :: camber((np + 1)/2), camber_upper((np + 1)/2),    &
                                                       pitch_line((np + 1)/2), u1_top((np + 1)/2),      &
                                                       v1_top((np + 1)/2), u2_bot((np + 1)/2),          &
                                                       v2_bot((np + 1)/2), inter_coord(4,((np + 1)/2)), &
                                                       min_throat_2D, u((np + 1)/2)
        integer,                    intent(in)      :: throat_index

        ! Local variables
        integer                                     :: funit = 85, i, k


        write(funit,*) 'section ', js
        write(funit,*) 'u       camber      upper_camber'
        do i = 1,np_sidee
            write(funit,*) u(i), camber(i), camber_upper(i)
        end do

        write(funit,*) 'u       pitch_line'
        do i = 1,np_sidee
            write(funit,*) u(i), pitch_line(i)
        end do

        write(funit,*) 'u1_top      v1_top      u2_bot      v2_bot'
        do i = 1,np_sidee
            write(funit,*) u1_top(i), v1_top(i)
            write(funit,*) u2_bot(i), v2_bot(i)
        end do

        write(funit,*) 'intersection points'
        write(funit,*) '    u                   v                   counter'
        do k = 1,n_normal_distance
            write(funit,*) inter_coord(1:2,k)
            write(funit,*) inter_coord(3:4,k), k
        end do

        write(funit,*) 'min_throat_2D ', min_throat_2D
        write(funit,*) 'throat_index ', throat_index


    end subroutine write_2D_throat_data
    !---------------------------------------------------------------------------






    !
    ! Write 3D meanline coordinates to a file
    !
    !---------------------------------------------------------------------------
    subroutine write_3D_meanline(ia,uplmt,casename,nx,nax,xmeanline,ymeanline,zmeanline)

        integer,                    intent(in)      :: ia, uplmt, nx, nax
        character(32),              intent(in)      :: casename
        real,                       intent(in)      :: xmeanline(nx,nax), ymeanline(nx,nax), &
                                                       zmeanline(nx,nax)

        ! Local variables
        character(:),   allocatable                 :: fname2
        character(32)                               :: temp
        integer                                     :: funit = 4, i

        
        write(temp,*) ia
        fname2  = 'meanline.sec'//trim(adjustl(temp))//'.'//trim(casename)//'.dat'
        open(funit, file = fname2, form = 'formatted')
        do i = 1,uplmt + 6
            write(funit,*) xmeanline(i,ia), ymeanline(i,ia), zmeanline(i,ia)
        end do
        close(funit)


    end subroutine write_3D_meanline
    !---------------------------------------------------------------------------






    !
    ! Write constant slope meanline coordinates to a file
    !
    !---------------------------------------------------------------------------
    subroutine write_constantslope_meanline(ia,uplmt,npts,casename,nx,nax,xmeanline,ymeanline, &
                                            zmeanline)

        integer,                    intent(in)      :: ia, uplmt, npts, nx, nax
        character(32),              intent(in)      :: casename
        real,                       intent(in)      :: xmeanline(nx,nax), ymeanline(nx,nax), &
                                                       zmeanline(nx,nax)

        ! Local variables
        character(:),   allocatable                 :: fname2
        character(32)                               :: temp
        integer                                     :: funit = 4, i
        

        write(temp,'(i2)') ia
        fname2  = 'meanline.sec'//trim(adjustl(temp))//'.'//trim(casename)//'.dat'
        open(funit, file = fname2, form = 'formatted')
        do i = 1,uplmt + npts
            write(funit,*) xmeanline(i,ia), ymeanline(i,ia), zmeanline(i,ia)
        end do
        close(funit)


    end subroutine write_constantslope_meanline
    !---------------------------------------------------------------------------






    !
    ! Write thickness multiplier control points to a file
    !
    !---------------------------------------------------------------------------
    subroutine write_thick_multi_cp(sec,casename,ncp,xcp_thk,ycp_thk)

        character(20),              intent(in)      :: sec
        character(*),               intent(in)      :: casename
        integer,                    intent(in)      :: ncp
        real,                       intent(in)      :: xcp_thk(ncp), ycp_thk(ncp)

        ! Local variables
        character(:),   allocatable                 :: file2
        integer                                     :: funit = 81, i


        file2   = 'thick_Multi_cp.'//trim(adjustl(sec))//'.'//trim(casename)//'.txt'
        open(funit, file = file2, status = 'unknown', action = 'write', form = 'formatted')
        write(funit,*) 'xcp_thk     ycp_thk'
        do i = 1,ncp
            write(funit,*) xcp_thk(i), '    ', ycp_thk(i)
        end do
        close(funit)


    end subroutine write_thick_multi_cp
    !---------------------------------------------------------------------------






    !
    ! Write NACA thickness distribution for a section to a file
    !
    !---------------------------------------------------------------------------
    subroutine write_NACA_thickness(sec,casename,np,u,thickness_data)

        character(20),              intent(in)      :: sec
        character(*),               intent(in)      :: casename
        integer,                    intent(in)      :: np
        real,                       intent(in)      :: u(np), thickness_data(np,3)

        ! Local variables
        character(:),   allocatable                 :: thickness_file_name
        integer                                     :: funit = 11, i
        logical                                     :: file_exist


        thickness_file_name = 'thickness_data.'//trim(adjustl(sec))//'.'//trim(casename)
        inquire(file = thickness_file_name, exist=file_exist)
        if (file_exist) then
            open(funit, file = thickness_file_name, status = 'old', action = 'write', form = 'formatted')
        else
            open(funit, file = thickness_file_name, status = 'new', action = 'write', form = 'formatted')
        end if
        do i = 1,np

            write(funit,'(4F40.16)') u(i), 2.0*thickness_data(i,1), thickness_data(i,2), thickness_data(i,3)

        end do
        close(funit)


    end subroutine write_NACA_thickness
    !---------------------------------------------------------------------------
   





    !
    ! Write Wennerstrom thickness distribution for a section to a file
    !
    !---------------------------------------------------------------------------
    subroutine write_Wennerstrom_thickness(sec,casename,np,u,thickness)
        
        character(20),              intent(in)          :: sec
        character(*),               intent(in)          :: casename
        integer,                    intent(in)          :: np
        real,                       intent(in)          :: u(np), thickness(np)

        ! Local variables
        character(:),   allocatable                     :: thickness_file_name
        integer                                         :: funit = 11, i
        logical                                         :: file_exist


        thickness_file_name = 'thickness_data.'//trim(adjustl(sec))//'.'//trim(casename)
        inquire(file = thickness_file_name, exist=file_exist)
        if (file_exist) then
            open(funit, file = thickness_file_name, status = 'old', action = 'write', form = 'formatted')
        else
            open(funit, file = thickness_file_name, status = 'new', action = 'write', form = 'formatted')
        end if
        do i = 1,np

            write(funit, '(2F20.16)') u(i), thickness(i)

        end do
        close(funit)


    end subroutine write_Wennerstrom_thickness
    !---------------------------------------------------------------------------
    





    !
    ! Write Wennerstrom thickness distribution for a section to a file
    !
    !---------------------------------------------------------------------------
    subroutine write_Wennerstrom_thickness_variation(casename,nspn,umxthk_all,spanwise_thk)
        
        character(*),               intent(in)          :: casename
        integer,                    intent(in)          :: nspn
        real,                       intent(in)          :: umxthk_all(nspn)
        real,                       intent(in)          :: spanwise_thk(nspn)

        ! Local variables
        character(:),   allocatable                     :: thickness_file_name
        integer                                         :: funit = 11, i
        logical                                         :: file_exist


        thickness_file_name     = 'thickness_span_variation.'//trim(casename)//'.dat'
        inquire (file = thickness_file_name, exist = file_exist)
        if (file_exist) then
            open(funit, file = thickness_file_name, status = 'old', action = 'write', form = 'formatted')
        else
            open(funit, file = thickness_file_name, status = 'new', action = 'write', form = 'formatted')
        end if
        do i = 1, nspn
            
            write(funit, '(2F20.16)') umxthk_all(i), spanwise_thk(i)

        end do
        close(funit)


    end subroutine write_Wennerstrom_thickness_variation
    !---------------------------------------------------------------------------
   





    !
    ! Write meanline u,v data to a file
    !
    ! Input parameters: np      - number of points along the chord
    !                   sec     - string representing the section index
    !                   u       - aarray of points along the chord
    !                   camber  - array of the camber for the 'sec' blade section
    !                   slope   - array of the camber slope for the 'sec' blade section
    !
    !---------------------------------------------------------------------------
    subroutine meanline_u_v_file(np,sec,u,camber,slope)

        integer,                intent(in)              :: np
        character(*),           intent(in)              :: sec
        real,                   intent(in)              :: u(np)
        real,                   intent(in)              :: camber(np)
        real,                   intent(in)              :: slope(np)

        ! Logical
        character(:),   allocatable                     :: meanline_file
        integer                                         :: nopen = 831, i
        logical                                         :: exist


        ! 
        ! Inquire if the meanline (u,v) file exists
        ! If it exists, overwrite 
        !
        meanline_file   = 'meanline_uv.'//trim(adjustl(sec))//'.dat'
        inquire(file = meanline_file, exist=exist)
        if (exist) then
            open(nopen, file = trim(meanline_file), status = 'old', action = 'write', form = 'formatted')
        else
            open(nopen, file = trim(meanline_file), status = 'new', action = 'write', form = 'formatted')
        end if
        
        do i = 1,np
            write(nopen,'(3F30.16)') u(i), camber(i), slope(i)
        end do
        
        close(nopen)


    end subroutine meanline_u_v_file
    !---------------------------------------------------------------------------






    !
    ! Write curvature spanwise spline data to a file, if using isdev
    !
    !---------------------------------------------------------------------------
    subroutine write_span_curv(nsl,ncp_chord_curv,casename,bspline_chord_curv)

        integer,                intent(in)              :: nsl, ncp_chord_curv
        character(*),           intent(in)              :: casename
        real,                   intent(in)              :: bspline_chord_curv(nsl,ncp_chord_curv)

        ! Local variables
        character(:),   allocatable                     :: curvature_file_name
        integer                                         :: funit = 97, i
        logical                                         :: file_exist


        curvature_file_name = 'curvature_span_variation.'//trim(casename)//'.dat'
        inquire(file = curvature_file_name, exist=file_exist)
        if (file_exist) then
            open(funit, file = curvature_file_name, status = 'old', action = 'write', form = 'formatted')
        else
            open(funit, file = curvature_file_name, status = 'new', action = 'write', form = 'formatted')
        end if
        do i = 1,nsl

            write(funit, '(20F20.16)') bspline_chord_curv(i,:)

        end do
        close(funit)


    end subroutine write_span_curv
    !---------------------------------------------------------------------------






    !
    ! Write thickness spanwise spline data to a file, if using isdev
    !
    !---------------------------------------------------------------------------
    subroutine write_span_thk(nsl,ncp_chord_thk,casename,bspline_thk,thick_distr)

        integer,                intent(in)              :: nsl, ncp_chord_thk
        character(*),           intent(in)              :: casename
        real,                   intent(in)              :: bspline_thk(nsl,ncp_chord_thk)
        integer,                intent(in)              :: thick_distr

        ! Local variables
        real,           allocatable                     :: bspline_thk_local(:,:)
        character(:),   allocatable                     :: thickness_file_name
        integer                                         :: funit = 97, i
        logical                                         :: file_exist


        thickness_file_name = 'thickness_span_variation.'//trim(casename)//'.dat'
        inquire(file = thickness_file_name, exist=file_exist)
        if (file_exist) then
            open(funit, file = thickness_file_name, status = 'old', action = 'write', form = 'formatted')
        else
            open(funit, file = thickness_file_name, status = 'new', action = 'write', form = 'formatted')
        end if
        do i = 1,nsl

            if (thick_distr == 5) then
                bspline_thk_local       = bspline_thk
                bspline_thk_local(:,4)  = 2.0*bspline_thk_local(:,4)
                bspline_thk_local(:,5)  = 2.0*bspline_thk_local(:,5)
                write(funit, '(20F20.16)') bspline_thk_local(i,:)
            else
                write(funit, '(20F20.16)') bspline_thk(i,:)
            end if

        end do
        close(funit)


    end subroutine write_span_thk
    !---------------------------------------------------------------------------






    !
    ! Write thickness spanwise spline data to a file, if using isdev
    !
    !---------------------------------------------------------------------------
    subroutine write_span_LE(nsl,ncp_LE,casename,bspline_LE)

        integer,                intent(in)              :: nsl, ncp_LE
        character(*),           intent(in)              :: casename
        real,                   intent(in)              :: bspline_LE(nsl,ncp_LE + 1)

        ! Local variables
        character(:),   allocatable                     :: LE_file_name
        integer                                         :: funit = 97, i
        logical                                         :: file_exist


        LE_file_name = 'LE_span_variation.'//trim(casename)//'.dat'
        inquire(file = LE_file_name, exist=file_exist)
        if (file_exist) then
            open(funit, file = LE_file_name, status = 'old', action = 'write', form = 'formatted')
        else
            open(funit, file = LE_file_name, status = 'new', action = 'write', form = 'formatted')
        end if
        do i = 1,nsl

            write(funit, '(20F20.16)') bspline_LE(i,:)

        end do
        close(funit)


    end subroutine write_span_LE
    !---------------------------------------------------------------------------






    !
    ! Write curvature control points to a file
    !
    !---------------------------------------------------------------------------
    subroutine write_curv_cp(na,curv_cp)

        integer,                intent(in)              :: na
        real,                   intent(in)              :: curv_cp(20,2*na)

        ! Local variables
        character(:),   allocatable                     :: curv_cp_file
        integer                                         :: funit = 150, i


        curv_cp_file    = 'curv_cp.dat'
        open(funit, file = curv_cp_file)
        do i = 1,20
            write(funit,'(22F15.2)') curv_cp(i,1:2*na)
        end do
        close(funit)


    end subroutine write_curv_cp
    !---------------------------------------------------------------------------






    !
    ! Write 3D section files (x,y,z)
    ! Moved from b3d2sec.f90
    !
    !---------------------------------------------------------------------------
    subroutine write_3D_section_files(scf,fext,ibrowc,nbls,casename)

        real,                   intent(in)              :: scf
        character(32),          intent(in)              :: fext
        character(10),          intent(in)              :: ibrowc
        integer,                intent(in)              :: nbls
        character(32),          intent(in)              :: casename

        ! Local variables
        real                                            :: xs,ys,zs,xsav,ysav,zsav
        character(80)                                   :: fname,fname1,temp
        integer                                         :: i,j,np,ns,nopen
        character(:),   allocatable                     :: log_file
        logical                                         :: file_open, isquiet


        ! Get the value of isquiet
        call get_quiet_status(isquiet)

        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) then
            write(*,*)
            print *, 'casename:',trim(fext)
        end if
        write(nopen,*) ''
        write(nopen,*) 'casename:', trim(fext)

        ! File name for combined section data file
        fname               = 'blade3d.'//trim(casename)//'.dat'

        if (.not.isquiet) then
            write(*,*)
            print *, 'fname: ', trim(fname)
            write(*,*)
            write(*,*) 'Creating section data files for CAD system'
        end if
        write(nopen,*) ''
        write(nopen,*) 'fname: ', trim(fname)
        write(nopen,*) ''
        write(nopen,*) 'Creating section data files for CAD system'

        ! Open  combined section data file
        open(2, file = fname, status = 'unknown')
        read(2,*) np,  ns
        if (.not. isquiet) then
            write(*,*) ''
            write(*,*) 'Number of points: ', np
            write(*,*) ''
            write(*,*) 'Number of sections: ', ns
            write(*,*) ''
            write(*,*) 'bsf: ', scf
            write(*,*) 'Number of blades in this row: ', nbls
            write(*,*)
        end if
        write(nopen,*) ''
        write(nopen,*) 'Number of points: ', np
        write(nopen,*) ''
        write(nopen,*) 'Number of sections: ', ns
        write(nopen,*) ''
        write(nopen,*) 'bsf: ', scf
        write(nopen,*) 'Number of blades in this row: ', nbls
        write(nopen,*)

        do j = 1,ns
            
            write(temp,*) j
            fname1          = 'sec'//trim(adjustl(temp))//'.'//trim(casename)//'.dat'
            open(1, file = fname1, form = 'formatted')
            
            ! Write section data
            do i = 1,np
                read(2,*) xs, ys, zs
                if (i == 1) then
                    xsav    = xs
                    ysav    = ys
                    zsav    = zs
                end if
                write(1,12) xs, ys, zs
            end do  ! i = 1,np

            close(1)

        end do  ! j = 1,ns

        ! Close combined section data file
        close(2)

        if (.not. isquiet) write(*,*) ''
        write(nopen,*) ''
        call close_log_file(nopen, file_open)

        12 format((f25.16),1x,(f25.16),1x,(f25.16))
        return


    end subroutine write_3D_section_files
    !---------------------------------------------------------------------------




















end module file_operations
