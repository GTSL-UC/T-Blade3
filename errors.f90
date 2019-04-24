module errors
    use file_operations
    implicit none



    contains



    !
    ! Fatal error subroutine stops execution of the code
    ! Shows a warning message to the user
    !
    ! Input paramaters: error_msg   - mandatory message showing where the fatal error has occurred
    !                   warning_msg - optional message to warn user about why the error has occurred
    !
    !--------------------------------------------------------------------------------------------------
    subroutine fatal_error(error_msg, warning_msg)
       
        character(:),   allocatable,                intent(in)  :: error_msg
        character(:),   allocatable,    optional,   intent(in)  :: warning_msg

        ! Local variables
        character(:),   allocatable                             :: error_file
        integer                                                 :: nopen
        logical                                                 :: file_open


        ! Print the messages to the screen and stop execution
        print *, ''
        print *, 'FATAL ERROR: '//error_msg
        if (present(warning_msg)) &
            print *, warning_msg
        print *, ''

        !
        ! Write the error messages to the error log file
        ! error_file_exists and close_error_file in file_operations.f90
        !
        call error_file_exists(error_file, nopen, file_open)
        write(nopen,*) 'FATAL ERROR: '//error_msg
        if (present(warning_msg)) &
            write(nopen,*) warning_msg
        call close_error_file(nopen, file_open)

        stop


    end subroutine fatal_error
    !--------------------------------------------------------------------------------------------------



    !
    ! Error subroutine quits subroutine when error flag is used
    !
    ! Input paramaters: error_msg   - mandatory message showing where the error has occurred
    !
    !--------------------------------------------------------------------------------------------------
    subroutine error(error_msg)
        use file_operations

        character(:),   allocatable,                intent(in)  :: error_msg

        ! Local variables
        character(:),   allocatable                             :: error_file
        integer                                                 :: nopen
        logical                                                 :: file_open


        ! Print the error message to the screen and exit
        print *, ''
        print *, 'ERROR: '//error_msg
        print *, ''

        !
        ! Write the error messages to the error log file
        ! error_file_exists and close_error_file in file_operations.f90
        !
        call error_file_exists(error_file, nopen, file_open)
        write(nopen,*) ''
        write(nopen,*) 'ERROR: '//error_msg
        write(nopen,*) ''
        call close_error_file(nopen, file_open)


    end subroutine error
    !--------------------------------------------------------------------------------------------------
    
    
    
    !
    ! Subroutine which shows a warning message when warnings are issued
    !
    ! Input paramaters: warning_msg     - mandatory message showing where the warning has occurred
    !                   warning_msg_1   - optional warning message 
    !--------------------------------------------------------------------------------------------------
    subroutine warning(warning_msg,warning_msg_1)
        use file_operations

        character(:),   allocatable,                intent(in)  :: warning_msg
        character(:),   allocatable,    optional,   intent(in)  :: warning_msg_1

        ! Local variables
        character(:),   allocatable                             :: error_file
        integer                                                 :: nopen
        logical                                                 :: file_open


        ! Print the warning message to the screen
        print *, ''
        print *, 'WARNING: '// warning_msg
        if (present(warning_msg_1)) &
            print *, 'WARNING: '//warning_msg_1
        print *, ''

        !
        ! Write the error messages to the error log file
        ! error_file_exists and close_error_file in file_operations.f90
        !
        call error_file_exists(error_file, nopen, file_open)
        write(nopen,*) ''
        write(nopen,*) 'WARNING: '//warning_msg
        if (present(warning_msg_1)) &
            write(nopen,*) 'WARNING: '//warning_msg_1
        write(nopen,*) ''
        call close_error_file(nopen, file_open)


    end subroutine warning
    !--------------------------------------------------------------------------------------------------



















end module
