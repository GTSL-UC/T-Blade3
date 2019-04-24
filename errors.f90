module errors
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
        use file_operations
       
        character(:),   allocatable,                intent(in)  :: error_msg
        character(:),   allocatable,    optional,   intent(in)  :: warning_msg


        ! Print the messages to the screen and stop execution
        print *, ''
        print *, 'FATAL ERROR: '//error_msg
        if (present(warning_msg)) &
            print *, warning_msg
        print *, ''
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


        ! Print the error message to the screen and exit
        print *, ''
        print *, 'ERROR: '//error_msg
        print *, ''


    end subroutine error
    !--------------------------------------------------------------------------------------------------
    
    
    
    !
    ! Subroutine which shows a warning message when warnings are issued
    !
    ! Input paramaters: warning_msg - mandatory message showing where the warning has occurred
    !
    !--------------------------------------------------------------------------------------------------
    subroutine warning(warning_msg,warning_msg_1)
        use file_operations

        character(:),   allocatable,                intent(in)  :: warning_msg
        character(:),   allocatable,    optional,   intent(in)  :: warning_msg_1


        ! Print the warning message to the screen
        print *, ''
        print *, 'WARNING: '// warning_msg
        if (present(warning_msg_1)) &
            print *, 'WARNING: '//warning_msg_1
        print *, ''


    end subroutine warning
    !--------------------------------------------------------------------------------------------------



















end module
