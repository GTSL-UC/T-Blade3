!
! Quartic spline based thickness distribution
!
!--------------------------------------------------------------------------------------------------
subroutine splinethick(js, np, np_side, i_le, i_te, thickness, u, lethk, umxthk, mxthk, tethk, uin_le, thick_distr, &
                       ucp_top, vcp_top, ucp_bot, vcp_bot, casename, develop, isdev, spline_data, splinedata)
    use errors 
    use file_operations
    implicit none

    ! Local constants
    integer,    parameter                   :: degree           = 4 
    integer,    parameter                   :: side_segments    = 7
    integer,    parameter                   :: ncp_side         = side_segments + degree 
    integer,    parameter                   :: ncp              = (2*side_segments) + degree
    integer,    parameter                   :: xrhs             = ncp + 6
    integer,    parameter                   :: ix_tete_float    = xrhs - 5
    integer,    parameter                   :: ix_te_float      = xrhs - 4
    integer,    parameter                   :: ix_le_float      = xrhs - 3
    integer,    parameter                   :: ile_ee           = xrhs - 2
    integer,    parameter                   :: ite_ee           = xrhs - 1
    integer,    parameter                   :: yrhs             = ncp + 4
    integer,    parameter                   :: iy_tete_float    = yrhs - 5
    integer,    parameter                   :: iy_te_float      = yrhs - 4
    integer,    parameter                   :: iy_le_float      = yrhs - 3

    ! Input parameters
    integer,                intent(in)      :: js, np, np_side
    integer,                intent(inout)   :: i_le, i_te
    real,                   intent(inout)   :: thickness(np)
    real,                   intent(in)      :: u(np)
    real,                   intent(in)      :: lethk, umxthk, mxthk, tethk
    real,                   intent(inout)   :: uin_le
    integer,                intent(in)      :: thick_distr
    real,                   intent(inout)   :: ucp_top(ncp_side), vcp_top(ncp_side), &
                                               ucp_bot(ncp_side), vcp_bot(ncp_side)
    character(*),           intent(in)      :: casename, develop
    logical,                intent(in)      :: isdev
    integer,                intent(in)      :: spline_data
    real,                   intent(inout)   :: splinedata(6, np_side)

    ! Local variables
    integer                                 :: i, irow, ixrow, iyrow, iLE, iTE, ix_le, imxthk, &
                                               ix_te, ix_tete, info
    real                                    :: Ax(xrhs - 1, xrhs), Ay(yrhs - 1, yrhs), top_thickness(np), &
                                               bot_thickness(np), arclength(side_segments + 1), t, pi,    &
                                               te_angle, te_thk_x, dtor, uin_te
    character(80)                           :: sec
    character(:),   allocatable             :: error_msg, dev_msg
    logical                                 :: isquiet


    ! Determine isquiet status
    call get_quiet_status(isquiet)

    Ax                                  = 0
    Ay                                  = 0

    !Constants
    pi                                  = 4.*atan(1.0)
    dtor                                = pi/180.0
    


    write(sec, '(i2)') js
    do i = 1, ncp - degree + 1

        if(degree == 3) then
            Ax(i,i:i + 2)               = [1/6.0, 2/3.0, 1/6.0]
        else if( degree == 4 ) then
            Ax(i,i:i + 3)               = [1/24.0, 11/24.0, 11/24.0, 1/24.0]
        end if
        Ay(i,i:i + degree)              = Ax(i,i:i + degree)    

    end do  ! i = 1, ncp - degree + 1



    !
    ! TE point  
    ! xcp(1)*(1/6) + xcp(2)*(2/3) + xcp(3)*(1/6) = 1
    ! ycp(1)*(1/6) + ycp(2)*(2/3) + ycp(3)*(1/6) = te_s*tethk/2 = 0
    !
    irow                                = 1
    iTE                                 = irow
    Ax(irow,xrhs)                       = 1
    Ay(irow,yrhs)                       = 0



    !
    ! TE thickness top  
    ! xcp(2)*(1/6) + xcp(3)*(2/3) + xcp(4)*(1/6) + tethk/2*te_ee = 1 ( == 1-tethk/2*te_ee )
    ! ycp(2)*(1/6) + ycp(3)*(2/3) + ycp(4)*(1/6) = tethk/2
    !
    irow                                = irow + 1
    Ax(irow,ite_ee)                     = -1  
    Ax(irow,xrhs)                       = 0 
    Ay(irow,yrhs)                       = tethk/2.0



    !
    ! MAX thickness derivative TE close float top 
    ! xcp(3)*(1/6) + xcp(4)*(2/3) + xcp(5)*(1/6) - x_float = 0
    ! ycp(3)*(1/6) + ycp(4)*(2/3) + ycp(5)*(1/6) - y_float = 0
    !
    if (ix_tete_float > ncp) then

        irow                            = irow + 1
        ix_tete                         = irow
        Ax(irow,ix_tete_float)          = -1  
        Ax(irow,xrhs)                   = 0
        Ay(irow,iy_tete_float)          = -1  
        Ay(irow,yrhs)                   = 0

    end if



    !
    ! MAX thickness derivative TE float top  
    ! xcp(3)*(1/6) + xcp(4)*(2/3) + xcp(5)*(1/6) - x_float = 0
    ! ycp(3)*(1/6) + ycp(4)*(2/3) + ycp(5)*(1/6) - y_float = 0
    !
    irow                                = irow + 1
    ix_te                               = irow
    Ax(irow,ix_te_float)                = -1  
    Ax(irow,xrhs)                       = 0
    Ay(irow,iy_te_float)                = -1  
    Ay(irow,yrhs)                       = 0



    !
    ! MAX thickness top  
    ! xcp(4)*(1/6) + xcp(5)*(2/3) + xcp(6)*(1/6) = umxthk
    ! ycp(4)*(1/6) + ycp(5)*(2/3) + ycp(6)*(1/6) = mxthk/2
    !
    irow                                = irow + 1
    imxthk                              = irow
    Ax(irow,xrhs)                       = umxthk
    Ay(irow,yrhs)                       = mxthk/2.0



    !
    ! MAX thickness derivative LE float top  
    ! xcp(3)*(1/6) + xcp(4)*(2/3) + xcp(5)*(1/6) - x_float = 0
    ! ycp(3)*(1/6) + ycp(4)*(2/3) + ycp(5)*(1/6) - y_float = 0
    !
    irow                                = irow + 1
    ix_le                               = irow
    Ax(irow,ix_le_float)                = -1  
    Ax(irow,xrhs)                       = 0
    Ay(irow,iy_le_float)                = -1  
    Ay(irow,yrhs)                       = 0



    !
    ! LE thickness top  
    ! xcp(5)*(1/6) + xcp(6)*(2/3) + xcp(7)*(1/6) - lethk/2*le_ee = 0
    ! ycp(5)*(1/6) + ycp(6)*(2/3) + ycp(7)*(1/6) = lethk/2
    !
    irow                                = irow + 1
    Ax(irow,ile_ee)                     = -1  
    Ax(irow,xrhs)                       = 0
    Ay(irow,yrhs)                       = lethk/2.0



    !
    ! LE point  
    ! xcp(6)*(1/6) + xcp(7)*(2/3) + xcp(8)*(1/6) = 0
    ! ycp(6)*(1/6) + ycp(7)*(2/3) + ycp(8)*(1/6) = le_s*lethk/2
    !
    irow                                = irow + 1
    iLE                                 = irow
    Ax(irow,xrhs)                       = 0
    Ay(irow,yrhs)                       = 0 



    !
    ! LE thickness bot  
    ! xcp(7)*(1/6) + xcp(8)*(2/3) + xcp(9)*(1/6) - lethk/2*le_ee = 0
    ! ycp(7)*(1/6) + ycp(8)*(2/3) + ycp(9)*(1/6) = -lethk/2
    !
    irow                                = irow + 1
    Ax(irow,ile_ee)                     = -1  
    Ax(irow,xrhs)                       = 0
    Ay(irow,yrhs)                       = -lethk/2.0



    !
    ! MAX thickness derivative LE bot  
    ! xcp(7)*(1/6) + xcp(8)*(2/3) + xcp(9)*(1/6) - 1 = 0
    ! ycp(7)*(1/6) + ycp(8)*(2/3) + ycp(9)*(1/6) + 1 = 0
    !
    irow                                = irow + 1
    Ax(irow,ix_le_float)                = -1  
    Ax(irow,xrhs)                       = 0
    Ay(irow,iy_le_float)                = 1  
    Ay(irow,yrhs)                       = 0



    !
    ! MAX thickness bot  
    ! xcp(7)*(1/6) + xcp(8)*(2/3) + xcp(9)*(1/6) = umxthk
    ! ycp(7)*(1/6) + ycp(8)*(2/3) + ycp(9)*(1/6) = -mxthk/2
    !
    irow                                = irow + 1
    Ax(irow,xrhs)                       = umxthk
    Ay(irow,yrhs)                       = -mxthk/2.0



    !
    ! MAX thickness derivative TE bot  
    ! xcp(7)*(1/6) + xcp(8)*(2/3) + xcp(9)*(1/6) - 1 = 0
    ! ycp(7)*(1/6) + ycp(8)*(2/3) + ycp(9)*(1/6) + 1 = 0
    !
    irow                                = irow + 1
    Ax(irow,ix_te_float)                = -1  
    Ax(irow,xrhs)                       = 0
    Ay(irow,iy_te_float)                = 1  
    Ay(irow,yrhs)                       = 0



    !
    ! MAX thickness derivative TE bot 
    ! xcp(7)*(1/6) + xcp(8)*(2/3) + xcp(9)*(1/6) - 1 = 0
    ! ycp(7)*(1/6) + ycp(8)*(2/3) + ycp(9)*(1/6) + 1 = 0
    !
    if (ix_tete_float > ncp) then

        irow                            = irow + 1
        Ax(irow,ix_tete_float)          = -1  
        Ax(irow,xrhs)                   = 0
        Ay(irow,iy_tete_float)          = 1  
        Ay(irow,yrhs)                   = 0

    end if


    
    !
    ! TE thickness bot  
    ! xcp(8)*(1/6) + xcp(9)*(2/3) + xcp(10)*(1/6) + tethk/2*te_ee = 1 ( == 1-tethk/2*te_ee )
    ! ycp(8)*(1/6) + ycp(9)*(2/3) + ycp(10)*(1/6) = -tethk/2
    !
    irow                                = irow + 1
    Ax(irow,ite_ee)                     = -1  
    Ax(irow,xrhs)                       = 0
    Ay(irow,yrhs)                       = -tethk/2.0



    !
    ! TE point  
    ! xcp(9)*(1/6) + xcp(10)*(2/3) + xcp(11)*(1/6) = 1
    ! ycp(9)*(1/6) + ycp(10)*(2/3) + ycp(11)*(1/6) = te_s*tethk/2 = 0
    !
    irow                                = irow + 1
    Ax(irow,xrhs)                       = 1
    Ay(irow,yrhs)                       = 0

    if (.not. isquiet) print *, 'irow', irow
    


    !
    ! TE point Periodic contidions  
    ! xcp(1)*(-1/2) + xcp(3)*(1/2)  -  xcp(9)*(-1/2) + xcp(11)*(1/2) ) = 0
    ! ycp(1)*(-1/2) + ycp(3)*(1/2)  -  ycp(9)*(-1/2) + ycp(11)*(1/2) ) = 0
    ! xcp(1)*(1) + xcp(2)*(-2) + xcp(3)*(1)  -  xcp(9)*(1) + xcp(10)*(-2) + xcp(11)*(1) ) = 0
    ! ycp(1)*(1) + ycp(2)*(-2) + ycp(3)*(1)  -  ycp(9)*(1) + ycp(10)*(-2) + ycp(11)*(1) ) = 0
    !
    irow                                = irow + 1
    if (degree == 3) then

        Ax(irow,1)                      = -1/2.0 
        Ax(irow,3)                      = 1/2.0  
        Ax(irow,ncp - 2)                = 1/2.0 
        Ax(irow,ncp)                    = -1/2.0
        Ax(irow,xrhs)                   = 0

    else if (degree == 4) then

        Ax(irow,1)                      = -1/6.0
        Ax(irow,2)                      = -1/2.0 
        Ax(irow,3)                      = 1/2.0 
        Ax(irow,4)                      = 1/6.0   
        Ax(irow,ncp - 3)                = 1/6.0
        Ax(irow,ncp - 2)                = 1/2.0 
        Ax(irow,ncp - 1)                = -1/2.0  
        Ax(irow,ncp)                    = -1/6.0   
        Ax(irow,xrhs)                   = 0

    end if



    ! Spline thickness with blunt TE
    if (thick_distr == 1) then 

        if (.not. isquiet) print *, 'thick_distr: ', thick_distr, 'blunt TE'
        Ay(irow,1:ncp)                  = Ax(irow,1:ncp) 
        Ay(irow,yrhs)                   = 0.0

    ! Spline thickness with sharp TE
    else if (thick_distr == 2) then 
        
        if (.not. isquiet) print*, 'thick_distr: ', thick_distr
        if (.not. isquiet) print*, 'Generating sharp TE'
        write(*, *)

        if (degree == 3) then

            Ax(irow,:)                  = 0.0
            Ax(irow,2)                  = 1 
            Ax(irow, 3)                 = -2 
            Ax(irow, 4)                 = 1  
            Ax(irow,xrhs)               = 0.0  
            Ay(irow,1:ncp)              = Ax(irow,1:ncp)

        else if (degree == 4) then

            Ax(irow,:)                  = 0.0
            Ax(irow,2)                  = 1/2.0 
            Ax(irow,3)                  = -1/2.0 
            Ax(irow,4)                  = -1/2.0   
            Ax(irow,5)                  = 1/2.0  
            Ax(irow,xrhs)               = 0.0 
            Ay(irow,1:ncp)              = Ax(irow,1:ncp)

        end if  ! degree

    end if  ! thick_distr

    irow                                = irow + 1



    ! The first derivative at the trailing edge upper and lower sides of the section is equal
    if (degree == 3) then

        Ax(irow,1)                      = 1 
        Ax(irow,2)                      = -2 
        Ax(irow,3)                      = 1  
        Ax(irow,ncp - 2)                = -1 
        Ax(irow,ncp - 1)                = 2
        Ax(irow,ncp)                    = -1 
        Ax(irow,xrhs)                   = 0 

    else if (degree == 4) then

        Ax(irow,1)                      = 1/2.0 
        Ax(irow,2)                      = -1/2.0 
        Ax(irow,3)                      = -1/2.0 
        Ax(irow,4)                      = 1/2.0  
        Ax(irow,ncp - 3)                = -1/2.0
        Ax(irow,ncp - 2)                = 1/2.0 
        Ax(irow,ncp - 1)                = 1/2.0 
        Ax(irow,ncp)                    = -1/2.0   
        Ax(irow,xrhs)                   = 0 

    end if  ! degree



    ! Spline thickness with blunt TE
    if (thick_distr == 1) then 

        if (.not. isquiet) print *, 'thick_distr: ', thick_distr, 'blunt TE'
        Ay(irow,1:ncp)                  = Ax(irow,1:ncp) 
        Ay(irow,yrhs)                   = 0.0

    ! Spline thickness with sharp TE
    else if (thick_distr == 2) then 

        if (.not. isquiet) print *, 'thick_distr: ', thick_distr
        if (.not. isquiet) print *, 'Generating sharp TE'
        write(*, *)

        if (degree == 3) then

            Ax(irow,:)                  = 0.0
            Ax(irow,ncp - 3)            = 1 
            Ax(irow,ncp - 2)            = -2 
            Ax(irow,ncp - 1)            = 1 
            Ax(irow,xrhs)               = 0.0  
            Ay(irow,1:ncp)              = Ax(irow,1:ncp)

        else if (degree == 4) then

            Ax(irow,:)                  = 0.0
            Ax(irow,ncp - 4)            = 1/2.0 
            Ax(irow,ncp - 3)            = -1/2.0  
            Ax(irow,ncp - 2)            = -1/2.0  
            Ax(irow,ncp - 1)            = 1/2.0
            Ax(irow,xrhs)               = 0.0 
            Ay(irow,1:ncp)              = Ax(irow,1:ncp)

        end if  ! degree

    end if  ! thick_distr



    if (degree == 4) then

        irow                            = irow + 1
        Ax(irow, 1)                     = -1  
        Ax(irow, 2)                     = 3 
        Ax(irow, 3)                     = -3  
        Ax(irow, 4)                     = 1 
        Ax(irow, ncp - 3)               = -1  
        Ax(irow, ncp - 2)               = 3
        Ax(irow, ncp - 1)               = -3 
        Ax(irow, ncp)                   = 1  
        Ax(irow, xrhs)                  = 0 
        Ay(irow, 1:ncp)                 = Ax(irow, 1:ncp) 
        Ay(irow, yrhs)                  = 0

    end if  ! if (degree = 4)



    ! User input constraints
    ixrow = irow
    iyrow = irow



    ! Specify thickness locations 
    if (degree == 3) then

        ixrow                           = ixrow + 1 
        Ax(ixrow, iLE)                  = 1 
        Ax(ixrow, iLE + 1)              = -2 
        Ax(ixrow, iLE + 2)              = 1 
        Ax(ixrow, ile_ee)               = 0
        Ax(ixrow, xrhs)                 = 0.03 !LE dx2/dt2

    else if (degree == 4) then

        ixrow                           = ixrow + 1 
        Ax(ixrow, iLE)                  = 1/2.0 
        Ax(ixrow, iLE + 1)              = -1/2.0 
        Ax(ixrow, iLE + 2)              = -1/2.0  
        Ax(ixrow, iLE + 3)              = 1/2.0
        Ax(ixrow, ile_ee)               = 0 
        Ax(ixrow, xrhs)                 = 0.04 !LE dx2/dt2

    end if  ! degree



    ! Spline thickness with blunt TE
    ! This specifies thickness location using curvature at the TE
    if (thick_distr == 1) then 
        
        if (degree == 3) then   

            ixrow                       = ixrow + 1 
            Ax(ixrow, iTE)              = 1 
            Ax(ixrow, iTE + 1)          = -2 
            Ax(ixrow, iTE + 2)          = 1  
            Ax(ixrow, ite_ee)           = 0
            Ax(ixrow, xrhs)             = -0.0 !TE dx2/dt2

        else if (degree == 4) then

            ixrow                       = ixrow + 1 
            Ax(ixrow, iTE)              = 1/2.0 
            Ax(ixrow, iTE + 1)          = -1/2.0 
            Ax(ixrow, iTE + 2)          = -1/2.0
            Ax(ixrow, iTE + 3)          = 1/2.0  
            Ax(ixrow, ite_ee)           = 0  
            Ax(ixrow, xrhs)             = -0.00 !TE dx2/dt2 

        end if  ! degree

    ! Spline thickness with sharp TE
    ! Specifies the thickness location using a sharpness angle for sharp TE
    else if (thick_distr == 2) then 

        te_angle                        = 2.5*dtor 
        te_thk_x                        = 1 - ((tethk/2)/tan(te_angle))
        if (.not. isquiet) print *, 'Thickness location sharp TE: ', te_thk_x     
        ixrow                           = ixrow + 1 
        Ax(ixrow, ite_ee)               = 1 
        Ax(ixrow, xrhs)                 = te_thk_x

    end if  ! thick_distr



    !Max thickness control by LE
    if (degree == 3) then

        ixrow                           = ixrow + 1 
        Ax(ixrow, ix_le)                = -1 
        Ax(ixrow, ix_le + 1)            = 3 
        Ax(ixrow, ix_le + 2)            = -3  
        Ax(ixrow, ix_le + 3)            = 1
        Ax(ixrow, ix_le_float)          = 0  
        Ax(ixrow, xrhs)                 = 0.0 !Max thickness dx3/dt3 by LE

        iyrow                           = iyrow + 1 
        Ay(iyrow, ix_le)                = -1  
        Ay(iyrow, ix_le + 1)            = 3 
        Ay(iyrow, ix_le + 2)            = -3  
        Ay(iyrow, ix_le + 3)            = 1
        Ay(iyrow, iy_le_float)          = 0 
        Ay(iyrow, yrhs)                 = 0.0 !Max thickness dy3/dt3 by LE

    else if (degree == 4) then

        ixrow                           = ixrow + 1 
        Ax(ixrow, ix_le)                = -1 
        Ax(ixrow, ix_le + 1)            = 3 
        Ax(ixrow, ix_le + 2)            = -3   
        Ax(ixrow, ix_le + 3)            = 1
        Ax(ixrow, ix_le_float)          = 0  
        Ax(ixrow, xrhs)                 = 0 !Max thickness dx3/dt3 by LE

        iyrow                           = iyrow + 1 
        Ay(iyrow, ix_le)                = 1 
        Ay(iyrow, ix_le + 1)            = -4 
        Ay(iyrow, ix_le + 2)            = 6  
        Ay(iyrow, ix_le + 3)            = -4
        Ay(iyrow, ix_le + 4)            = 1 
        Ay(iyrow, iy_le_float)          = 0 
        Ay(iyrow, yrhs)                 = 0 !Max thickness dy4/dt4 by LE

    end if  ! degree



    !Max thickness control by TE
    if (degree == 3) then

        ixrow                           = ixrow + 1 
        Ax(ixrow, ix_te )               = -1  
        Ax(ixrow, ix_te + 1)            = 3  
        Ax(ixrow, ix_te + 2)            = -3    
        Ax(ixrow, ix_te + 3)            = 1
        Ax(ixrow, ix_te_float)          = 0 
        Ax(ixrow, xrhs)                 = 0.0 !Max thickness dx3/dt3 by TE

        iyrow                           = iyrow + 1 
        Ay(iyrow, imxthk)               = 0.5 
        Ay(iyrow, imxthk + 1)           = 0  
        Ay(iyrow, imxthk + 2)           = -0.5 
        Ay(iyrow, iy_te_float)          = 0
        Ay(iyrow, yrhs)                 = 0.0 !Max thickness dy/dt

    else if (degree == 4) then

        ixrow                           = ixrow + 1  
        Ax(ixrow, ix_te)                = 1    
        Ax(ixrow, ix_te + 1)            = -4 
        Ax(ixrow, ix_te + 2)            = 6  
        Ax(ixrow, ix_te + 3)            = -4
        Ax(ixrow, ix_te + 4)            = 1 
        Ax(ixrow, ix_te_float)          = 0 
        Ax(ixrow, xrhs)                 = 0 !Max thickness dx4/dt4 by TE

        iyrow                           = iyrow + 1 
        Ay(iyrow, imxthk)               = -1/6.0 
        Ay(iyrow, imxthk + 1)           = -1/2.0 
        Ay(iyrow, imxthk + 2)           = 1/2.0
        Ay(iyrow, imxthk + 3)           = 1/6.0 
        Ay(iyrow, iy_te_float)          = 0 
        Ay(iyrow, yrhs)                 = 0 !Max thickness dy/dt

    end if  ! degree



    !Max thickness control by TE 
    if (ix_tete_float > ncp) then

        if (degree == 3) then

            ixrow                       = ixrow + 1 
            Ax(ixrow, ix_tete)          = -1  
            Ax(ixrow, ix_tete + 1)      = 3 
            Ax(ixrow, ix_tete + 2 )     = -3
            Ax(ixrow, ix_tete + 3)      = 1  
            Ax(ixrow, ix_tete_float)    = 0 
            Ax(ixrow, xrhs)             = 0.0 !Max thickness dx3/dt3 by TE

            iyrow                       = iyrow + 1 
            Ay(iyrow, ix_tete)          = -1 
            Ay(iyrow, ix_tete + 1)      = 3 
            Ay(iyrow, ix_tete + 2)      = -3
            Ay(iyrow, ix_tete + 3)      = 1 
            Ay(iyrow, iy_tete_float)    = 0 
            Ay(iyrow, yrhs)             = 0.0 !Max thickness dy3/dt3 by TE

        else if (degree == 4) then

            ixrow                       = ixrow + 1 
            Ax(ixrow, ix_tete)          = 1    
            Ax(ixrow, ix_tete + 1)      = -4 
            Ax(ixrow, ix_tete + 2)      = 6 
            Ax(ixrow, ix_tete + 3)      = -4 
            Ax(ixrow, ix_tete + 4)      = 1 
            Ax(ixrow, ix_tete_float)    = 0 
            Ax(ixrow, xrhs)             = 0 !Max thickness dx4/dt4 by TE TE

            iyrow                       = iyrow + 1  
            Ay(iyrow, ix_tete)          = -1.0  
            Ay(iyrow, ix_tete + 1)      = 3.0  
            Ay(iyrow, ix_tete + 2)      = -3
            Ay(iyrow, ix_tete + 3)      = 1.0  
            Ay(iyrow, iy_tete_float)    = 0  
            Ay(iyrow, yrhs)             = 0 !Max thickness dy3/dt3

        end if  ! degree

    end if  ! if (ix_tete_float > ncp)



    if (ixrow /= xrhs - 1) then
        error_msg                       = 'ixrow not equal to xrhs - 1 in splinethick.f90'
        dev_msg                         = 'Check subroutine splinethick in splinethick.f90'
        call fatal_error(error_msg, dev_msg = dev_msg)
    end if

    if (iyrow /= yrhs-1) then
        error_msg                       = 'iyrow not equal to yrhs - 1 in splinethick.f90'
        dev_msg                         = 'Check subroutine splinethick in splinethick.f90'
        call fatal_error(error_msg, dev_msg = dev_msg)
    end if



    call gauss_jordan(xrhs - 1, 1, Ax, info)
    if(info.ne.0)then
        error_msg                       = 'Singular matrix encountered in solving the thickness distribution'
        dev_msg                         = 'Check subroutine splinethick in splinethick.f90'
        call fatal_error(error_msg, dev_msg = dev_msg)
    end if

    call gauss_jordan(yrhs - 1, 1, Ay, info)
    if(info.ne.0)then
        error_msg                       = 'Singular matrix encountered in solving the thickness distribution'
        dev_msg                         = 'Check subroutine splinethick in splinethick.f90'
        call fatal_error(error_msg, dev_msg = dev_msg)
    end if



    ! Generate top thickness
    ucp_top                     = Ax(ncp_side:1:-1, xrhs)
    vcp_top                     = Ay(ncp_side:1:-1, yrhs)

    call bspline_y_of_x(ncp_side, degree, ucp_top, vcp_top, np, u, top_thickness) 



    ! Generate bottom thickness
    ucp_bot                     = Ax(ncp-ncp_side+1:ncp, xrhs)
    vcp_bot                     = Ay(ncp-ncp_side+1:ncp, yrhs)

    call bspline_y_of_x(ncp_side, degree, ucp_bot, vcp_bot, np, u,  bot_thickness) 


    
    ! Compute thickness
    thickness                   = top_thickness - bot_thickness
    thickness                   = thickness/2.0

    call bspline_arclength(ncp_side, degree, ucp_bot, vcp_bot, arclength)



    do i = 1, np     
        t                       = real(i - 1)/real(np)
        splinedata(5, i)        = thickness(i)
    end do 



    ! 
    ! write_quartic_spline_thickness in file_operations.f90
    !
    call write_quartic_spline_thickness(isdev, sec, casename, np, u, top_thickness, bot_thickness)



    if (.not. isquiet) print *, 'lethk/2 = ', lethk/2.0
    if (.not. isquiet) print *, 'tethk/2 = ', tethk/2.0

    i_le                        = 0
    i_te                        = 0

    do i = 1, np - 1

        if ((thickness(i) < (lethk/2.0)) .and. (thickness(i + 1) > (lethk/2.0))) then
            i_le                = i
            uin_le              = u(i_le)
        else if ((thickness(i) > tethk/2.0) .and. (thickness(i + 1) < tethk/2.0)) then
            i_te                = i
            uin_te              = u(i_te)
        end if

    end do  ! i = 1, np - 1
     

end subroutine splinethick
!--------------------------------------------------------------------------------------------------






!
! UNUSED SUBROUTINES
!
!--------------------------------------------------------------------------------------------------
!subroutine splinethickmult(u, splthick, xcp, ycp, ncp)
!!xcp, ycp: thickness control points
!!xbs, ybs: spline points
!!t: parameter value ( 0<t<1)
!!ncp: number of control points
!!"Ahmed Nemnem" 
!
!implicit none
!
!integer i, j, k, ncp
!integer, parameter :: np = 100
!real*8, allocatable, dimension(:) :: xbs, ybs
!real*8 xs(np), ys(np), u(np), xcp(ncp), ycp(ncp)
!real*8 t(np), B1(np), B2(np), B3(np), B4(np)
!real*8 splthick(np), tt, tt_0
!real*8 d1_B11, B11, d1_B22, B22, d1_B33, B33, d1_B44, B44
!real*8 xs_0, d1_xs_0
!real*8 x_spl_end(ncp-2)
!
!
!! xs = x1(control point)*B1+x2(control point)*B2+x3(control point)
!!                                         *B3+x4(control point)*B4
!do i = 1, np
!    t(i) = (1.0/(np-1))*(i-1)
!    B1(i) = ((-t(i)**3) + (3*t(i)**2) - (3*t(i)) + 1)/6
!    B2(i) = ((3*t(i)**3) - (6*t(i)**2) + 4)/6
!    B3(i) = ((-3*t(i)**3) + (3*t(i)**2) + (3*t(i)) + 1)/6
!    B4(i) = (t(i)**3)/6
!end do
!
!!print*, "xbs ybs"
!if (allocated(xbs)) deallocate(xbs)
!if (allocated(ybs)) deallocate(ybs)
!Allocate (xbs((np)*(ncp-3)))
!Allocate (ybs((np)*(ncp-3)))
!!5   
!do j = 1, ncp-3
!    do i = 1, np
!        xs(i) = (xcp(j)*B1(i)) + (xcp(j+1)*B2(i)) + (xcp(j+2)*B3(i))&
!        + (xcp(j+3)*B4(i))
!        ys(i) = (ycp(j)*B1(i)) + (ycp(j+1)*B2(i)) + (ycp(j+2)*B3(i))&
!        + (ycp(j+3)*B4(i))
!        k = i+(j-1)*(np)
!        xbs(k) = xs(i)
!        ybs(k) = ys(i)
!        !print*, xbs(k), ybs(k)
!    end do
!    !000000000000000000000000000000000000000000000000000000
!    !picking up the endpoints of each spline for Newton interpolation
!    if (j == 1) then
!        x_spl_end(1) = xs(1)
!    end if           
!     x_spl_end(j+1) = xs(np)
!    !00000000000000000000000000000000000000000000000000000
! end do
!
!
!! Newton method to find ybs corresponding to u(i) 
!! for u(i) == 0
!splthick(1) = ybs(1)
!! for u(i) == 1
!splthick(np) = ybs((np)*(ncp-3))
!
!do j = 1, ncp-3
!    do i = 2, np-1
!        if ((u(i) > x_spl_end(j)).and.(u(i) < x_spl_end(j+1)))then
!            tt_0 = u(i)
!            do k = 1, 100
!                ! Basic functions:
!                B11 = ((-tt_0**3) + (3*tt_0**2) - (3*tt_0) + 1)/6
!                B22 = ((3*tt_0**3) - (6*tt_0**2) + 4)/6
!                B33 = ((-3*tt_0**3) + (3*tt_0**2) + (3*tt_0) + 1)/6
!                B44 = (tt_0**3)/6              
!                ! first derivative:
!                d1_B11 = ((-3*tt_0**2) + (6*tt_0) - (3))/6
!                d1_B22 = ((9*tt_0**2) - (12*tt_0))/6
!                d1_B33 = ((-9*tt_0**2) + (6*tt_0) + (3))/6
!                d1_B44 = (3*tt_0**2)/6
!                ! xs(t_0)
!                  xs_0 = xcp(j)*B11+xcp(j+1)*B22+xcp(j+2)*B33+xcp(j+3)*B44
!                ! d1_xs(t_0)
!                d1_xs_0 = xcp(j)*d1_B11+xcp(j+1)*d1_B22+xcp(j+2)*d1_B33+xcp(j+3)*d1_B44
!                ! Newton's interpolation:
!                 tt = tt_0 + (u(i)-xs_0)/d1_xs_0
!                 if (abs(tt-tt_0)<1e-16) then
!                    ! basic function at u(i):
!                    B11 = ((-tt**3) + (3*tt**2) - (3*tt) + 1)/6
!                    B22 = ((3*tt**3) - (6*tt**2) + 4)/6
!                    B33 = ((-3*tt**3) + (3*tt**2) + (3*tt) + 1)/6
!                    B44 = (tt**3)/6
!                    goto 15
!                 end if
!                 tt_0 = tt
!            end do
!            15  splthick(i) = ycp(j)*B11+ycp(j+1)*B22+ycp(j+2)*B33+ycp(j+3)*B44
!        end if
!    end do
!end do
!
!! write the thickness values in a file:
!! TODO: Move to file_operations
!open(unit = 61, file = "thickness_multip.txt", form = "formatted")
!write(61, *) "ui splthick"     
!do i = 1, np     
!    ! write results in a file:
!    write(61, *) u(i), "    ", splthick(i)
!end do
!close(61)         
!
!return
!
!end subroutine splinethickmult
!--------------------------------------------------------------------------------------------------

!*******************************************************************************************

!subroutine splinethickcontrol(umxthk, thkc, ncp, xcp_thk, ycp_thk, np, u, thk, thick_distr_3_flag)
!
!! Added by Karthik Balasubramanian
!
!implicit none
!
!integer :: ncp, np, info, i, i_cp, dB_max_i, bisect_err
!real :: umxthk, thkc, x_spl_end, bspline4, t, dB_ycp_prod_sum
!real :: xcp_thk(ncp), ycp_thk(ncp), thk(np), u(np), dB_root(5), dB_ycp_prod(5), ddB(5), &
!    ucp_thk_mat(2, 3), ce(0:4), t_roots
!character (len = 16) :: thick_distr_3_flag
!
!write (*, '(/, A)') 'Executing subroutine splinethickcontrol in splinethick.f90'
!write (*, '(A)') 'Implementing blunt TE and LE'
!
!!! Blunt LE and TE Constraints
!ucp_thk_mat = reshape((/1., 1./3., 11., 1., -(11.*xcp_thk(3))-xcp_thk(4), xcp_thk(3)+(xcp_thk(4)/3.) /), (/2, 3/))
!call gauss_jordan(2, 1, ucp_thk_mat, info)
!xcp_thk(1:2) = ucp_thk_mat(:, 3)
!ycp_thk(1) = -(11.*ycp_thk(2))-(11.*ycp_thk(3))-ycp_thk(4)
!ucp_thk_mat = reshape((/11., 1., 1., 1./3., 24.-xcp_thk(ncp-3)-(11.*xcp_thk(ncp-2)), (xcp_thk(ncp-3)/3.) + &
!                     xcp_thk(ncp-2) /), (/2, 3/))
!call gauss_jordan(2, 1, ucp_thk_mat, info)
!xcp_thk(ncp-1:ncp) = ucp_thk_mat(:, 3)
!ycp_thk(ncp) = -ycp_thk(ncp-3)-(11.*ycp_thk(ncp-2))-(11.*ycp_thk(ncp-1))
!write (*, '(A)') 'Thickness control points including internally generated points after blunt LE and TE constraints : '
!write (*, '(2F20.16)') (xcp_thk(i), ycp_thk(i), i = 1, ncp)
!
!!! Maximum Thickness Implementation
!if (thick_distr_3_flag(:1) .eq. '1') then
!    write (*, '(A)') 'Constraining max thickness location'
!    do i = 1, ncp-4
!        x_spl_end = bspline4(xcp_thk(i:i+4), 1.)
!        if (umxthk <= x_spl_end) exit
!    end do
!    i_cp = i
!    write (*, '(A, F5.3, ":", F5.3, I2, ":", I2)') 'End points of segment containing maximum thickness : ', &
!                                                   bspline4(xcp_thk(i:i+4), 0.), x_spl_end
!    write (*, '(A, I2, ":", I2)') 'Control points containing maximum thickness : ', i, i+4
!    ! Coefficients of Quartic t equation
!    ce(0) = xcp_thk(i)+(11.*xcp_thk(i+1))+(11.*xcp_thk(i+2))+xcp_thk(i+3)-(24.*umxthk)
!    ce(1) = (-4.*xcp_thk(i))-(12.*xcp_thk(i+1))+(12.*xcp_thk(i+2))+(4.*xcp_thk(i+3))
!    ce(2) = (6.*xcp_thk(i))-(6.*xcp_thk(i+1))-(6.*xcp_thk(i+2))+(6.*xcp_thk(i+3))
!    ce(3) = (-4.*xcp_thk(i))+(12.*xcp_thk(i+1))-(12.*xcp_thk(i+2))+(4.*xcp_thk(i+3))
!    ce(4) = xcp_thk(i)-(4.*xcp_thk(i+1))+(6.*xcp_thk(i+2))-(4.*xcp_thk(i+3))+xcp_thk(i+4)
!
!    if (abs(ce(4)) > 1e-13) then
!        write(*, '(A)') 'Equation in t is : Quartic'
!        call poly_solve_bisect(4, ce, 0., 1., bisect_err, t_roots)
!    else if (abs(ce(3)) > 1e-13) then
!        write(*, '(A)') 'Equation in t is : Cubic'
!        call poly_solve_bisect(3, ce, 0., 1., bisect_err, t_roots)
!    else if (abs(ce(2)) > 1e-13) then
!        write(*, '(A)') 'Equation in t is : Quadratic'
!        call poly_solve_bisect(2, ce, 0., 1., bisect_err, t_roots)
!    else if (abs(ce(1)) > 1e-13) then
!        write(*, '(A)') 'Equation in t is : Linear'
!        t_roots = -ce(0)/ce(1)
!    end if
!
!    write (*, '(A, F20.16, /, A, F20.16)') 'User-defined max thickness location is :', umxthk, &
!     'Max thickness location determined using calculated t is :', bspline4(xcp_thk(i_cp:i_cp+4), t)
!
!    call d_bspline4_Beval(t, dB_root)
!    dB_max_i = maxloc(abs(dB_root), 1)
!    write (*, '(A, I5)') 'Modified control point to implement maximum thickness : ', i_cp+dB_max_i-1
!    dB_ycp_prod = dB_root*ycp_thk(i_cp:i_cp+4)
!    dB_ycp_prod_sum = sum(dB_ycp_prod, mask = abs(dB_root).lt.maxval(abs(dB_root)))
!    ycp_thk(i_cp+dB_max_i-1) = -dB_ycp_prod_sum/dB_root(dB_max_i)
!    write (*, '(A)') 'Thickness control points including internally generated points after max thickness location implemented : '
!    write (*, '(2F20.16)') (xcp_thk(i), ycp_thk(i), i = 1, ncp)
!    call dd_Bspline4_Beval(t, ddB)
!    write (*, '(A, F20.16)') 'Second derivative of thickness at position of maximum thickness : ', sum(ddB*ycp_thk(i_cp:i_cp+4))
!end if
!
!write (*, '(//)')
!call bspline_y_of_x( thk, u, np, xcp_thk, ycp_thk, ncp, 4 )
!thk = thk*thkc/2./maxval(thk)
!
!end subroutine splinethickcontrol





















