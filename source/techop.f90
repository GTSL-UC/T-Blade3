!
! Program to chop the TE of a 2D (m'theta) airfoil
! Required inputs are the blade file name and the number of points
! to be chopped
! The TE chopped blade coordinates are written to the same file
!
!------------------------------------------------------------------------------------------------
program TEchop
    use file_operations
    use errors
    implicit none

    integer                                                 :: i, points, nlines, stat
    real,           allocatable                             :: xb(:), yb(:)
    real                                                    :: sinl, sext, chrd1, chrd2, pitch
    character(32)                                           :: fname, fext, npoints
    character(:),   allocatable                             :: error_msg, warning_msg, dev_msg


    !
    ! Read the first command line argument
    ! Input filename
    !
    call getarg(1,fname)
    
    if ( ( fname == 'help' ) .or. ( fname == '' ) ) then

         error_msg      = 'techop command line error'
         warning_msg    = 'Usage: techop blade.#.#.casename npoints'
         dev_msg        = 'Check techop.f90'
         call fatal_error(error_msg, warning_msg, dev_msg)

    end if
   
    ! 
    ! Read second command line argument
    ! Number of points to be chopped
    !
    call getarg(2,npoints)
    read (npoints, *) points
    


    !
    ! Get number of blade points in the blade section file
    ! nlines - number of lines in the file
    !
    open (1, file = trim(adjustl(fname)), status = 'unknown')
   
    nlines                                                  = 0 
    do 
        read(1, *, iostat = stat)
        if (is_iostat_end(stat)) exit
        nlines = nlines + 1
    end do

    close(1)


    
    !
    ! Allocate blade point arrays
    !
    if (allocated(xb)) deallocate(xb)
    allocate(xb(nlines - 2))
    if (allocated(yb)) deallocate(yb)
    allocate(yb(nlines - 2))



    !
    ! Read the blade coordinates
    !
    open (1, file = trim(adjustl(fname)), status = 'unknown')
    
    ! Print output to screen
    print *, ''
    print *, 'Reading input file: ', trim(adjustl(fname))
    print *, ''

    ! Read blade section properties
    read (1, *) fext
    read (1, *) sinl, sext, chrd1, chrd2, pitch

    ! Read blade section coordinates
    do i = 1, nlines - 2
        read(1, *) xb(i), yb(i)
    end do

    close(1)



    !
    ! Print output to screen
    !
    print *, 'Original number of coordinates: ', nlines - 2
    print *, ''
    print *, 'Number of coordinates chopped from the TE: ', points
    print *, ''
    print *, 'Final number of coordinates: ', nlines - 2 - points 
    print *, ''
    print *, 'Overwriting chopped TE blade coordinates to the SAME file'
    print *, ''
    


    !
    ! Write the chopped TE blade coordinates to file
    !
    open (2, file = fname, status = 'unknown')

    ! Write blade section properties
    write (2, '(a)') fext
    write (2, '(5f20.16)') sinl, sext, chrd1, chrd2, pitch

    ! Write new blade coordinates to file
    do i = points + 1, nlines - 2-points
        write (2, *) xb(i), yb(i)
    end do

    close(2)




















end program
