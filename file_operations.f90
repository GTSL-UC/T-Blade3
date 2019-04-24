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
        character(50)   :: maininput_log_file
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
        character(50)       :: auxinput_log_file
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
                open(nopen, file = error_file, status = 'old', action = 'write')
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
        logical                                         :: file_open


        call log_file_exists(log_file, nopen, file_open)
        write(*,*)
        print *, 'casename:',trim(fext)
        write(nopen,*) ''
        write(nopen,*) 'casename:', trim(fext)

        ! File name for combined section data file
        fname               = 'blade3d.'//trim(casename)//'.dat'

        write(*,*)
        print *, 'fname: ', trim(fname)
        write(*,*)
        write(*,*) 'Creating section data files for CAD system'
        write(nopen,*) ''
        write(nopen,*) 'fname: ', trim(fname)
        write(nopen,*) ''
        write(nopen,*) 'Creating section data files for CAD system'

        ! Open  combined section data file
        open(2, file = fname, status = 'unknown')
        read(2,*) np,  ns
        write(*,*) ''
        write(*,*) 'Number of points: ', np
        write(*,*) ''
        write(*,*) 'Number of sections: ', ns
        write(*,*) ''
        write(*,*) 'bsf: ', scf
        write(*,*) 'Number of blades in this row: ', nbls
        write(*,*)
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

        write(*,*) ''
        write(nopen,*) ''
        call close_log_file(nopen, file_open)

        12 format((f25.16),1x,(f25.16),1x,(f25.16))
        return


    end subroutine write_3D_section_files
    !---------------------------------------------------------------------------




















end module file_operations
