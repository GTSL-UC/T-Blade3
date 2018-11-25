module file_operations
    implicit none



    contains



    !
    ! Subroutine for checking if the log file exists or not
    ! If exists, open as a file to append
    ! If doesn't exists, open as a new file
    !
    ! Mayank Sharma (@UC) - 24/11/18
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
    ! Mayank Sharma (@UC) - 24/11/18
    !
    !---------------------------------------------------------------------------
    subroutine close_log_file(nopen, file_open)

        integer,        intent(in)      :: nopen
        logical,        intent(in)      :: file_open


        ! If the log file is open, close it
        if (file_open) close(nopen)


    end subroutine close_log_file
    !---------------------------------------------------------------------------








end module file_operations
