!
! Subroutine to display the welcome message 
!
!------------------------------------------------------------------------------------------------------
subroutine displayMessage
    use file_operations

    character(len = :), allocatable :: log_file
    integer                         :: nopen
    logical                         :: file_open, initial, isquiet_local


    !
    ! Determine if 'isquiet' command line argument is being used
    !
    call get_quiet_status(isquiet_local)


    if (.not. isquiet_local) then
        write(*,*)
        write(*,*)'************************************************************'
        write(*,*)'************************************************************'
        write(*,*)'****  T-BLADE3:Turbomachinery BLADE 3D Geometry Builder ****'
        write(*,*)'****                                                    ****' 
        write(*,*)'****  Master Version 1.2	                               ****' 
        write(*,*)'****                                                    ****'
        write(*,*)'****  ...was also called as below till Aug 2016...      ****' 
        write(*,*)'****  3DBGB: 3 Dimensional Blade Geometry Builder       ****'
        write(*,*)'****                                                    ****'  
        write(*,*)'****  Develop Version 1.2                               ****' 
        write(*,*)'****                                                    ****'
        write(*,*)'****  This software comes with ABSOLUTELY NO WARRANTY   ****'
        write(*,*)'****                                                    ****'
        write(*,*)'****  This is a program which generates a 3D blade...   ****' 
        write(*,*)'****  ...shape and outputs 3D blade section files.      ****'
        write(*,*)'****                                                    ****'
        write(*,*)'****  Inputs: LE and TE curve(x,r), inlet angle,        ****' 
        write(*,*)'****          exit angle, chord, tm/c, incidence,       ****'
        write(*,*)'****          deviation, secondary flow angles,         ****'
        write(*,*)'****          streamline coordinates:(x,r)              ****'   
        write(*,*)'****          control points for sweep, lean,           ****'
        write(*,*)'****          blade scaling factor.                     ****'
        write(*,*)'****                                                    ****'
        write(*,*)'****  Outputs: 3D blade sections (x,y,z),               ****'
        write(*,*)'****           2D airfoils (mprime,theta).              ****'
        write(*,*)'****                                                    ****'
        write(*,*)'****  ---------------by Kiran Siddappaji         ----   ****'
        write(*,*)'****  ---------------by Mark G. Turner           ----   ****'
        write(*,*)'****  ------------------- turnermr@ucmail.uc.edu ----   ****'
        write(*,*)'****  ---------------by Karthik Balasubramanian  ----   ****'
        write(*,*)'****  ---------------by Syed Moez Hussain Mahmood----   ****'
        write(*,*)'****  ---------------by Ahmed Nemnem             ----   ****'
        write(*,*)'****  ---------------by Marshall C. Galbraith    ----   ****'
        write(*,*)'************************************************************'
        write(*,*)'************************************************************'
        write(*,*)
        write(*,*) 'T-Blade3 Copyright (C) 2017 University of Cincinnati, developed by Kiran Siddappaji,' 
        write(*,*) 'Dr. Mark G. Turner, Karthik  Balasubramanian, Syed Moez Hussain, Ahmed Farid Nemnem '
        write(*,*) ' and Marshall C. Galbraith.'
        write(*,*)
        write(*,*) 'This program is free software; you can redistribute it and/or modify it under the '
        write(*,*) 'terms of the GNU General Public License as published by the Free Software Foundation; '
        write(*,*) 'either version 2 of the License, or (at your option) any later version.'
        write(*,*)
        write(*,*) 'This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;'
        write(*,*) 'without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. '
        write(*,*) 'See the GNU General Public License for more details.  For the complete terms of the '
        write(*,*) 'GNU General Public License, please see this URL: http://www.gnu.org/licenses/gpl-2.0.html'
        write(*,*)
        write(*,*)'************************************************************'
        write(*,*)
    end if



    !
    ! Check if log file exists or not
    ! Write screen output to the log file
    !
    initial = .true.
    call log_file_exists(log_file, nopen, file_open, initial)

    write(nopen,*)
    write(nopen,*)'************************************************************'
    write(nopen,*)'************************************************************'
    write(nopen,*)'****  T-BLADE3:Turbomachinery BLADE 3D Geometry Builder ****'
    write(nopen,*)'****                                                    ****' 
    write(nopen,*)'****  Master Version 1.13                               ****' 
    write(nopen,*)'****                                                    ****'
    write(nopen,*)'****  ...was also called as below till Aug 2016...      ****' 
    write(nopen,*)'****  3DBGB: 3 Dimensional Blade Geometry Builder       ****'
    write(nopen,*)'****                                                    ****'  
    write(nopen,*)'****  Develop Version 1.2                               ****' 
    write(nopen,*)'****                                                    ****'
    write(nopen,*)'****  This software comes with ABSOLUTELY NO WARRANTY   ****'
    write(nopen,*)'****                                                    ****'
    write(nopen,*)'****  This is a program which generates a 3D blade...   ****' 
    write(nopen,*)'****  ...shape and outputs 3D blade section files.      ****'
    write(nopen,*)'****                                                    ****'
    write(nopen,*)'****  Inputs: LE and TE curve(x,r), inlet angle,        ****' 
    write(nopen,*)'****          exit angle, chord, tm/c, incidence,       ****'
    write(nopen,*)'****          deviation, secondary flow angles,         ****'
    write(nopen,*)'****          streamline coordinates:(x,r)              ****'   
    write(nopen,*)'****          control points for sweep, lean,           ****'
    write(nopen,*)'****          blade scaling factor.                     ****'
    write(nopen,*)'****                                                    ****'
    write(nopen,*)'****  Outputs: 3D blade sections (x,y,z),               ****'
    write(nopen,*)'****           2D airfoils (mprime,theta).              ****'
    write(nopen,*)'****                                                    ****'
    write(nopen,*)'****  ---------------by Kiran Siddappaji         ----   ****'
    write(nopen,*)'****  ---------------by Mark G. Turner           ----   ****'
    write(nopen,*)'****  ------------------- turnermr@ucmail.uc.edu ----   ****'
    write(nopen,*)'****  ---------------by Karthik Balasubramanian  ----   ****'
    write(nopen,*)'****  ---------------by Syed Moez Hussain Mahmood----   ****'
    write(nopen,*)'****  ---------------by Ahmed Nemnem             ----   ****'
    write(nopen,*)'****  ---------------by Marshall C. Galbraith    ----   ****'
    write(nopen,*)'************************************************************'
    write(nopen,*)'************************************************************'
    write(nopen,*)
    write(nopen,*) 'T-Blade3 Copyright (C) 2017 University of Cincinnati, developed by Kiran Siddappaji,' 
    write(nopen,*) 'Dr. Mark G. Turner, Karthik  Balasubramanian, Syed Moez Hussain, Ahmed Farid Nemnem '
    write(nopen,*) ' and Marshall C. Galbraith.'
    write(nopen,*)
    write(nopen,*) 'This program is free software; you can redistribute it and/or modify it under the '
    write(nopen,*) 'terms of the GNU General Public License as published by the Free Software Foundation; '
    write(nopen,*) 'either version 2 of the License, or (at your option) any later version.'
    write(nopen,*)
    write(nopen,*) 'This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;'
    write(nopen,*) 'without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. '
    write(nopen,*) 'See the GNU General Public License for more details.  For the complete terms of the '
    write(nopen,*) 'GNU General Public License, please see this URL: http://www.gnu.org/licenses/gpl-2.0.html'
    write(nopen,*)
    write(nopen,*)'************************************************************'
    write(nopen,*)

    ! Close the log file if it is open
    call close_log_file(nopen, file_open)

    return


end subroutine displayMessage
!------------------------------------------------------------------------------------------------------






!
! Function to calculate the inlet angle including incidence
!
!------------------------------------------------------------------------------------------------------
real function inBetaInci(inbeta,inci)
    implicit none
    
    real                    :: inbeta(1),inci(1)

    !Positive inlet angle
    if (inbeta(1) >= 0.) then
      inBetaInci = inbeta(1) - inci(1)
    else
      !Negative inlet angle
      inBetaInci = inbeta(1) + inci(1) 
    end if


end function 
!------------------------------------------------------------------------------------------------------






!
! Function to calculate the exit angle including deviation
!
!------------------------------------------------------------------------------------------------------
real*8 function outBetaDevn(inbeta,outbeta,devn)
    implicit none
    
    real                    :: inbeta(1),outbeta(1),camber(1), devn(1)

    !calculating camber 
    camber(1) = outbeta(1) - inbeta(1)

    !negative camber
    if (camber(1) <= 0.) then
      outBetaDevn = outbeta(1) - devn(1)
    else
      !positive camber
      outBetaDevn = outbeta(1) + devn(1)
    end if


end function
!------------------------------------------------------------------------------------------------------






!
! Subroutine calculating hub offset
!
!------------------------------------------------------------------------------------------------------
subroutine huboffset(mphub,x,r,dxds,drds,hub,nphub,scf,casename)
    use file_operations
    implicit none

    integer,                    intent(in)          :: nphub
    real,                       intent(inout)       :: mphub(nphub,1), x(nphub,1), r(nphub,1), &
                                                       dxds(nphub,1), drds(nphub,1)
    real,                       intent(in)          :: hub, scf
    character(32),              intent(in)          :: casename

    ! Local variables
    integer                                         :: i, nopen
    real                                            :: xhub(nphub,1),rhub(nphub,1),xms_hub(nphub,1), &
                                                       rms_hub(nphub,1),deltan,b,xnorm(nphub,1),     &
                                                       rnorm(nphub,1),dxn(nphub,1),drn(nphub,1)
    character(80)                                   :: fname1
    character(:),   allocatable                     :: log_file
    logical                                         :: file_open, isquiet_local, from_gridgen = .false.


    ! Calculating the normal and offset coordinates
    do i = 1, nphub   
       xnorm(i,1)   = drds(i,1)
       rnorm(i,1)   = -dxds(i,1)
       deltan       = hub*1.
       b            = deltan/((xnorm(i,1)**2 + rnorm(i,1)**2)**0.5)
       dxn(i,1)     = b*(xnorm(i,1))
       drn(i,1)     = b*(rnorm(i,1))
       xhub(i,1)    = x(i,1) + dxn(i,1)
       rhub(i,1)    = r(i,1) + drn(i,1)
    end do  

    ! Get isquiet status
    call get_quiet_status(isquiet_local)

    write(*,*) ''

    ! Writing the offset coordinates to a file
    fname1 = 'hub-offset.'//trim(casename)//'.dat'
    open(1,file = fname1, status = 'unknown')
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet_local) then
        write(*,*)'Calculating hub-offset streamline coordinates...'
        write(*,*)'Writing the coordinates to hub-offset.dat file'
    end if

    write(nopen,*) 'Calculating hub-offset streamline coordinates...'
    write(nopen,*) 'Writing the coordinates to hub-offset.dat file'
    call close_log_file(nopen, file_open)

    ! Writing the dimensionless offset coordinates to a file
    fname1 = 'hub-offset-dimlss.'//trim(casename)//'.dat'
    open(2,file = fname1, status = 'unknown')

    !Calculating the mprime coordinates of the offset streamline/construction line.
    mphub(1,1)= 0.
    write(2,*)xhub(1,1),rhub(1,1)
    do i = 2, nphub
       mphub(i,1) = mphub(i-1,1) + 2.*sqrt((rhub(i,1)-rhub(i-1,1))**2+(xhub(i,1)-xhub(i-1,1))**2)/(rhub(i,1)+rhub(i-1,1)) 
       write(1,*)scf*xhub(i,1),0.0,scf*rhub(i,1)
       write(2,*)xhub(i,1),rhub(i,1)
    end do

    ! Splining the offset xhub, rhub:
    call spline(xhub(1,1),xms_hub(1,1),mphub(1,1),nphub, 999.0, -999.0, from_gridgen)
    call spline(rhub(1,1),rms_hub(1,1),mphub(1,1),nphub, 999.0, -999.0, from_gridgen)
    
    ! Over writing the xm, rm values with hub spline coefficients:
    x    = xhub
    r    = rhub
    dxds = xms_hub
    drds = rms_hub

    ! Close files
    close(2)
    close(1)

    
end subroutine huboffset
!------------------------------------------------------------------------------------------------------






!
! Subroutine calculating tip offset
!
!------------------------------------------------------------------------------------------------------
subroutine tipoffset(mptip,x,r,dxds,drds,tip,nptip,scf,nsl,casename)
    use file_operations
    implicit none

    integer,                    intent(in)          :: nptip
    real,                       intent(inout)       :: mptip(nptip,1), x(nptip,1), r(nptip,1), &
                                                       dxds(nptip,1), drds(nptip,1) 
    real,                       intent(in)          :: tip, scf
    integer,                    intent(in)          :: nsl
    character(32),              intent(in)          :: casename

    ! Local variables
    integer                                         :: i,nopen
    real                                            :: xtip(nptip,1),rtip(nptip,1),xms_tip(nptip,1),     &
                                                       rms_tip(nptip,1),b,xnorm(nptip,1),rnorm(nptip,1), &
                                                       dxn(nptip,1),drn(nptip,1),deltan
    character(80)                                   :: fname1
    character(:),   allocatable                     :: log_file
    logical                                         :: file_open, isquiet_local, from_gridgen = .false.
    

    ! Calculating the normal and offset coordinates
    do i = 1, nptip   
       xnorm(i,1)   = drds(i,1)
       rnorm(i,1)   = -dxds(i,1)
       deltan       = tip*1.
       b            = deltan/((xnorm(i,1)**2 + rnorm(i,1)**2)**0.5)
       dxn(i,1)     = b*(xnorm(i,1))
       drn(i,1)     = b*(rnorm(i,1))
       xtip(i,1)    = x(i,1) - dxn(i,1)
       rtip(i,1)    = r(i,1) - drn(i,1)
    end do  

    ! Get isquiet status
    call get_quiet_status(isquiet_local)

    if (.not. isquiet_local) write(*,*)

    ! Writing the offset coordinates to a file
    fname1 = 'tip-offset.'//trim(casename)//'.dat'
    open(1,file = fname1, status = 'unknown')
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet_local) then
        write(*,*)'Calculating tip-offset streamline coordinates...'
        write(*,*)'Writing the coordinates to tip-offset.dat file'
    end if

    write(nopen,*) 'Calculating tip-offset streamline coordinates...'
    write(nopen,*) 'Writing the coordinates to tip-offset.dat file'
    call close_log_file(nopen, file_open)

    ! Writing the dimenstionless offset coordinates to a file
    fname1 = 'tip-offset-dimlss.'//trim(casename)//'.dat'
    open(2,file = fname1, status = 'unknown')
    write(2,*)xtip(1,1),rtip(1,1)

    ! Calculating the mprime coordinates of the offset streamline/construction line.
    mptip(1,1)= 0.
    do i = 2, nptip
       mptip(i,1) = mptip(i-1,1) + 2.*sqrt((rtip(i,1)-rtip(i-1,1))**2+(xtip(i,1)-xtip(i-1,1))**2)/(rtip(i,1)+rtip(i-1,1)) 
       write(1,*)scf*xtip(i,1),0.0,scf*rtip(i,1)
       write(2,*)xtip(i,1),rtip(i,1)
    end do

    ! Splining the offset xtip, rtip:
    call spline(xtip(1,1),xms_tip(1,1),mptip(1,1),nptip, 999.0, -999.0, from_gridgen)
    call spline(rtip(1,1),rms_tip(1,1),mptip(1,1),nptip, 999.0, -999.0, from_gridgen)
    
    ! Overwriting xm, rm with new tip spline coefficients:
    x = xtip
    r = rtip
    dxds = xms_tip
    drds = rms_tip
      
    ! Close all files 
    close(2)
    close(1)

    
end subroutine tipoffset
!------------------------------------------------------------------------------------------------------






!
! Compute intersection points
!
!------------------------------------------------------------------------------------------------------
subroutine interp(xa,ya,xb,yb,xc,yc,xd,yd,xint,yint)
    implicit none
     
    real,                   intent(in)      :: xa,ya,xb,yb,xc,yc,xd,yd
    real,                   intent(inout)   :: xint,yint
    

    xint=(xa*(yb-ya)+yc*(xb-xa)-((yd-yc)/(xd-xc))*xc*(xb-xa)-ya*(xb-xa))/((yb-ya)-(((yd-yc)*(xb-xa))/(xd-xc)))
    yint = yc+((xint-xc)/(xd-xc))*(yd-yc)
     
    
end subroutine interp
!------------------------------------------------------------------------------------------------------






!
! Find the position of the throat for a section
!
!------------------------------------------------------------------------------------------------------
subroutine throatindex(throat_pos,throat_index,n_normal_distance,js,nsl,thick_distr)
    use file_operations
    use errors
    implicit none
   
    integer,                    intent(in)          :: nsl, throat_index(nsl), n_normal_distance, js, &
                                                       thick_distr 
    character(20),              intent(inout)       :: throat_pos(nsl)

    ! Local variables
    integer                             :: nopen
    character(:),   allocatable         :: log_file, warning_msg, dev_msg
    character(10)                       :: warning_arg
    logical                             :: file_open, isquiet


    ! Get the value of isquiet
    call get_quiet_status(isquiet)

    call log_file_exists(log_file, nopen, file_open)

    if (throat_index(js) == 0) then
        throat_pos(js) = 'none'
        write(warning_arg,'(I2)') js
        warning_msg   = 'No Throat found for section '//trim(adjustl(warning_arg))
        dev_msg       = 'Check subroutine throatindex in funcNsubs.f90'
        call warning(warning_msg, dev_msg = dev_msg)
        write(nopen,*) 'No Throat Found'

    else if (throat_index(js) < 0.25*n_normal_distance) then
        throat_pos(js) = 'le'
        if (.not. isquiet) print*,'throat_index',js,throat_index(js)
        write(nopen,*) 'throat_index', js, throat_index(js)

    else if (throat_index(js) > 0.75*n_normal_distance) then
        throat_pos(js) = 'te' 
        if (.not. isquiet) print*,'throat_index',js,throat_index(js)
        write(nopen,*) 'throat_index', js, throat_index(js)

    else
        throat_pos(js) = 'btween'
        if (.not. isquiet) print*,'throat_index',js,throat_index(js)
        write(nopen,*) 'throat_index', js, throat_index(js)

    end if  ! throat_index(js)

    call close_log_file(nopen, file_open)


end subroutine throatindex
!------------------------------------------------------------------------------------------------------






!
! Calculate geometric properties of blade section
! Computes centroid location, principal moments of inertia and principal-axis angles
!
!------------------------------------------------------------------------------------------------------
subroutine aecalc(n,x,y,t, itype, area,xcen,ycen,ei11,ei22,apx1,apx2)
    dimension x(n),y(n),t(n)
    

    ! Initialize variables
    sint  = 0.0
    aint  = 0.0
    xint  = 0.0
    yint  = 0.0
    xxint = 0.0
    xyint = 0.0
    yyint = 0.0
    
    do 10 io = 1, n
    if (io == n) then
        ip = 1
    else
        ip = io + 1
    end if
    
    dx =  x(io) - x(ip)
    dy =  y(io) - y(ip)
    xa = (x(io) + x(ip))*0.50
    ya = (y(io) + y(ip))*0.50
    ta = (t(io) + t(ip))*0.50
    
    ds = sqrt(dx*dx + dy*dy)
    sint = sint + ds


    !
    ! Integrate over airfoil cross-section
    !
    if (itype == 1) then
        da    = ya*dx
        aint  = aint  +       da
        xint  = xint  + xa   *da
        yint  = yint  + ya   *da/2.0
        xxint = xxint + xa*xa*da
        xyint = xyint + xa*ya*da/2.0
        yyint = yyint + ya*ya*da/3.0

    !
    ! Integrate over skin thickness
    !
    else
        da    = ta*ds
        aint  = aint  +       da
        xint  = xint  + xa   *da
        yint  = yint  + ya   *da
        xxint = xxint + xa*xa*da
        xyint = xyint + xa*ya*da
        yyint = yyint + ya*ya*da

    end if
    
    10   continue
    
    area = aint
    if (aint == 0.0) then
        xcen  = 0.0
        ycen  = 0.0
        ei11  = 0.0
        ei22  = 0.0
        apx1  = 0.0
        apx2  = atan2(1.0,0.0)
        return
    end if
    

    !
    ! Calculate centroid location
    !
    xcen = xint/aint
    ycen = yint/aint
    
    !
    ! Calculate inertias
    !
    eixx = yyint - ycen*ycen*aint
    eixy = xyint - xcen*ycen*aint
    eiyy = xxint - xcen*xcen*aint
    
    !
    ! Set principal-axis inertias 
    ! ei11 is closest to "up-down" bending inertia
    !
    eisq  = 0.25*(eixx  - eiyy )**2  + eixy**2
    sgn = sign( 1.0 , eiyy-eixx )
    ei11 = 0.5*(eixx + eiyy) - sgn*sqrt(eisq)
    ei22 = 0.5*(eixx + eiyy) + sgn*sqrt(eisq)
    
    ! Rotationally-invariant section
    if (eisq/(ei11*ei22) < (0.001*sint)**4) then
        apx1 = 0.0
        apx2 = atan2(1.0,0.0)
    else
        c1   = eixy
        s1   = eixx - ei11
        c2   = eixy
        s2   = eixx - ei22
        sgn1 = sign( 1.0 , c1 )
        sgn2 = sign( 1.0 , c2 )
        apx1 = atan2(sgn1*s1,sgn1*c1)
        apx2 = atan2(sgn2*s2,sgn2*c2)
    end if


end subroutine aecalc
!------------------------------------------------------------------------------------------------------






!
! Stack (m',theta) airfoils
!
!------------------------------------------------------------------------------------------------------
subroutine stacking(xb,yb,xbot,ybot,xtop,ytop,js,np,stack_switch,stack,stk_u,stk_v,area,LE)
    use file_operations
    implicit none

    integer,                        intent(in)      :: js, np, LE, stack, stack_switch
    real,                           intent(in)      :: stk_u(1), stk_v(1), area
    real,                           intent(inout)   :: xb(np), yb(np), xbot((np + 1)/2), ybot((np + 1)/2), &
                                                       xtop((np + 1)/2), ytop((np + 1)/2)

    ! Local variables
    integer                                         :: i, j, k, np_side, nopen
    integer,    parameter                           :: nx = 1000, ny = 300
    real                                            :: umin, umax, sb(nx), u_stack, v_stack, vtop_stack, vbot_stack,  &
                                                       v_zero_stack, const_stk_u, const_stk_v, stku, stkv, ucen,vcen, &
                                                       ei11, ei22, apx1, apx2
    character(:),    allocatable                    :: log_file
    logical                                         :: file_open, isquiet


    ! Get isquiet status
    call get_quiet_status(isquiet)


    ! Calculating the area centroid cordinates of the airfoil ucen,vcen
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) print*,np
    write(nopen,*) np
    call aecalc(np,xb,yb,sb,1,area,ucen,vcen,ei11,ei22,apx1,apx2)
    
    ! Number of points along the bottom and top curves
    if (mod(np,2) == 0) then
        np_side = np/2 
    else
        np_side = (np + 1)/2
    end if   

    ! Constant radial stacking 
    if (stack_switch == 0) then 
        const_stk_u = floor(abs(real(stack))/1000)
        stku = const_stk_u
        if (stack >= 0) then
          const_stk_v = real(stack) - (const_stk_u*1000)
          stkv = const_stk_v
        else
          const_stk_v = real(stack) + (const_stk_u*1000)
          stkv = const_stk_v
        end if

    ! Variable radial stacking
    else if (stack_switch == 1) then 
        stku = stk_u(js)
        stkv = stk_v(js) 
    end if

    ! Print stacking position information to the screen and write to log file
    if (.not. isquiet) then
        write(*,*)
        print*,'Stacking position on the chord as % of chord:',stku
        print*,'Stacking position in % above(+ve) or below(-ve) meanline :',stkv
    end if
    write(nopen,*) ''
    write(nopen,*) 'Stacking position on the chord as % of chord:', stku
    write(nopen,*) 'Stacking position in % above(+ve) or below(-ve) meanline :', stkv

    ! 200 is for stacking at the area centroid
    if (stku == 200.) then 
        u_stack = ucen
        v_stack = vcen
        if (.not. isquiet) print*,'Stacking at the area centroid of the airfoil...'
        write(nopen,*) 'Stacking at the area centroid of the airfoil...'
        goto 16

    ! Calculating u_stack using the % chord stacking information
    else
        if (LE == 2) then 
          if (.not. isquiet) print*,'LE with sting.'
          write(nopen,*) 'LE with sting.'
        end if
        u_stack = xb(1)*(real(stku)/100)
    end if
    
    ! Stacking on/above/below meanline
    ! Calculating vtop_stack and vbot_stack for v_stack
    j = 1
    do i = 1, np_side - 1 
       umin = xbot(i)
       umax = xbot(i + 1)
       if (u_stack >= umin.and.u_stack <= umax) then
         j = i
         exit
       end if
    end do

    if (.not. isquiet) print*,'top curve points index range for % chord stacking:',j,j+1
    write(nopen,*) 'top curve points index range for % chord stacking:',j,j+1
    vbot_stack =  ybot(j) + ((u_stack - xbot(j))*(ybot(j+1) - ybot(j))/(xbot(j+1) - xbot(j))) 
    
    k = 1
    do i = 1, np_side - 1
       umin = xtop(i)
       umax = xtop(i + 1)
       if (u_stack >= umin.and.u_stack <= umax) then
         k = i
         exit
       end if
    end do

    if (.not. isquiet) print*,'bottom curve points index range for % chord stacking:',k,k+1
    write(nopen,*) 'bottom curve points index range for % chord stacking:',k,k+1
    vtop_stack =  ytop(k) + ((u_stack - xtop(k))*(ytop(k+1) - ytop(k))/(xtop(k+1) - xtop(k))) 
    
    !
    ! Stacking on meanline: v_stack = 0
    ! v_zero_stack = average of yb(istack) and yb(1+np - istack)  
    ! Above meanline stack: vstack= v_zero_stack - %age(stack)*(distance between meanline and 100% ABOVE meanline @ istack)
    ! Below meanline stack: vstack= v_zero_stack - %age(stack)*(distance between meanline and 100% BELOW meanline @ istack)
    !
    v_zero_stack = (vtop_stack + vbot_stack)/2
    
    if (.not. isquiet) print*, "vtop_stack  vbot_stack", vtop_stack, vbot_stack
    write(nopen,*) 'vtop_stack vbot_stack', vtop_stack, vbot_stack

    ! Above meanline stacking
    if (stku /= 200 .and. stkv > 0) then
      v_stack = v_zero_stack + (real(stkv)/100)*(abs(vtop_stack - v_zero_stack))
      if (.not. isquiet) print*,'+ve stack v_stack',v_stack,(real(stkv)/100)
      write(nopen,*) '+ve stack v_stack', v_stack, (real(stkv)/100)

    ! Meanline stacking
    else if (stku /= 200 .and. stkv == 0) then
      v_stack = 0.

    ! Below meanline stacking
    else if (stku /= 200 .and. stkv < 0) then 
      v_stack = v_zero_stack + (real(stkv)/100)*(abs(vbot_stack - v_zero_stack))

      if (.not. isquiet) print*,'v_stack',v_stack,(real(stkv)/100)
      write(nopen,*) 'v_stack', v_stack, (real(stkv)/100)

    end if

    if (.not. isquiet) then
        write(*,*)
        print*,'u_stack  v_stack', u_stack, v_stack
        write(*,*)
    end if
    write(nopen,*) ''
    write(nopen,*) 'u_stack  v_stack', u_stack, v_stack
    write(nopen,*) ''
    call close_log_file(nopen, file_open)
    
    ! Stacked coordinates
    16 do i = 1,np
          xb(i) = xb(i) - u_stack
          yb(i) = yb(i) - v_stack
       end do

 
end subroutine stacking
!------------------------------------------------------------------------------------------------------






!
! Rotate 2D sections
! Returns rotated coordinates as an array
!
!------------------------------------------------------------------------------------------------------
subroutine rotate(xb,yb,x,y,angle)
    implicit none

    real,                   intent(in)      :: x,y,angle
    real,                   intent(inout)   :: xb(1),yb(1)


    xb(1) = x*cos(-angle) + y*sin(-angle)
    yb(1) = y*cos(-angle) - x*sin(-angle)


end subroutine rotate
!------------------------------------------------------------------------------------------------------






!
! Rotate 2D sections
! Returns rotated coordinates as a floating point number
!
!------------------------------------------------------------------------------------------------------
subroutine rotate2(xb,yb,x,y,angle)
    implicit none

    real,               intent(in)      :: x,y,angle
    real,               intent(inout)   :: xb,yb


    xb = x*cos(-angle) + y*sin(-angle)
    yb = y*cos(-angle) - x*sin(-angle)


end subroutine rotate2
!------------------------------------------------------------------------------------------------------






!
! Scale blade section
!
!------------------------------------------------------------------------------------------------------
real function scaled(x,scalefactor)
    implicit none

    real,               intent(in)      :: x,scalefactor


    scaled = x*scalefactor


end function scaled
!------------------------------------------------------------------------------------------------------






!
! Create m'-theta blade section and write to file
!
!------------------------------------------------------------------------------------------------------
subroutine bladesection(xb,yb,np,nbls,TE_del,sinls,sexts,chrdd,fext,js,pitch,mble,mbte,airfoil)
    use file_operations
    implicit none

    integer,    parameter                       :: nx = 1000
    integer,                intent(in)          :: np, nbls, TE_del, js
    real,                   intent(in)          :: sinls, sexts, chrdd
    real,                   intent(inout)       :: xb(nx), yb(nx), mble, mbte, pitch
    character(*),           intent(in)          :: fext, airfoil

    ! Local variables
    integer                                     :: ii,np_side
    real                                        :: x1,y1,x2,y2
    real,   parameter                           :: pi = 4.0*atan(1.0)


    ! Number of points for bottom and top curve
    if (mod(np,2) == 0) then
      np_side = np/2 
    else
      np_side = (np+1)/2
    end if

    ! Initializing ii
    ii  = 0

    ! Bottom and top TE points
    x1 = xb(1)
    y1 = yb(1)
    x2 = xb(np)
    y2 = yb(np)

    ! If the TE points do not coincide
    ! Four digit NACA airfoils with closed sharp TE
    if (len_trim(airfoil) == 4) then 
      xb(1)  = 0.5*(x1 + x2)
      yb(1)  = 0.5*(y1 + y2)
      xb(np) = xb(1)
      yb(np) = yb(1)
    end if

    ! If the TE points do not coincide
    ! Take average of bottom and top TE points for new TE
    if((x1.ne.x2).and.(y1.ne.y2))then 
      xb(1)  = 0.5*(xb(np) + xb(1))
      yb(1)  = 0.5*(yb(np) + yb(1))
      xb(np) = xb(1)
      yb(np) = yb(1)
    end if 

    ! Calculation of the pitch between adjacent blades
    pitch = 2*pi/nbls 

    ! Switch for deleting the TE coordinates for each blade
    ! ii is the number of points to be deleted from the TE
    if (TE_del == 1) then
      ii = 18   
    else if (TE_del == 0) then
      ii = 0
    end if
    
    !Assiging the LE and TE to variables
    mble = xb(np_side)
    mbte = xb(np)
    
    ! Write the blade section coordinates to a file
    ! write_blade_files in file_operations
    call write_blade_files(np,nx,ii,fext,sinls,sexts,chrdd,pitch,xb,yb)

    
end subroutine bladesection
!------------------------------------------------------------------------------------------------------






!
! Find straight line intersection points
!
!------------------------------------------------------------------------------------------------------
subroutine st_line_intersection(xa,ya,xb,yb,xc,yc,xd,yd,xint,yint)
    implicit none

    real,                   intent(in)      :: xa,ya,xb,yb,xc,yc,xd,yd
    real,                   intent(out)     :: xint,yint


    xint = (xa*(yb - ya)+yc*(xb - xa) - ((yd - yc)/(xd - xc))*xc*(xb - xa) - ya*(xb - xa))/ &
           ((yb - ya) - (((yd - yc)*(xb - xa))/(xd - xc)))
    yint = yc + ((xint - xc)/(xd - xc))*(yd - yc)


end subroutine st_line_intersection
!------------------------------------------------------------------------------------------------------






!
! Calculate the 2D throat between nondimensional blade sections.
! Find the perpendicular distance on the pitch line (camber at half pitch) between adjacent blades
! calculating the throat from bottom point on the blade 
!
!------------------------------------------------------------------------------------------------------
subroutine throat_calc_pitch_line(xb,yb,np,camber,angle,sang,u,pi,pitch,throat_coord, mouth_coord,exit_coord, &
                                  min_throat_2D,throat_index,n_normal_distance,casename,js,nsl,develop)
    use file_operations
    use errors
    implicit none

    integer,                    intent(in)          :: np, js, nsl
    integer,                    intent(inout)       :: throat_index, n_normal_distance
    real,                       intent(in)          :: xb(np), yb(np), camber((np + 1)/2), angle((np + 1)/2), &
                                                       sang, u((np + 1)/2), pi, pitch
    real,                       intent(inout)       :: throat_coord(4), mouth_coord(4), exit_coord(4), min_throat_2D
    character(*),               intent(in)          :: casename, develop

    ! Local variables
    integer                                         :: i, j, k, np_sidee, i_interup1, i_interup2, i_interdwn1, i_interdwn2, nopen
    real                                            :: v1_top((np+1)/2), u1_top((np+1)/2), v2_bot((np+1)/2), u2_bot((np+1)/2),         &
                                                       yb_upper(np), xint_up, yint_up, xint_dwn, yint_dwn, inter_coord(4,((np+1)/2)),  &
                                                       pitch_line((np+1)/2), camber_upper((np+1)/2), x_interup, y_interup, x_interdwn, &
                                                       y_interdwn
    real,           allocatable                     :: throat(:)
    character(80)                                   :: file4
    character(:),   allocatable                     :: log_file, warning_msg, dev_msg
    character(10)                                   :: warning_arg
    logical                                         :: file_open, isdev, isquiet


    ! Get the value of isdev and isquiet
    call get_dev_status(isdev)
    call get_quiet_status(isquiet)

    ! Initializing variables
    x_interup   = 0.0
    y_interup   = 0.0
    x_interdwn  = 0.0
    y_interdwn  = 0.0

    ! Number of points along bottom and top curves
    if (mod(np,2) == 0) then
      np_sidee = np/2 
    else
      np_sidee = (np+1)/2
    end if
    
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) print*,'np_sidee',np_sidee
    write(nopen,*) 'np_sidee', np_sidee
    
    ! Intialize variables
    n_normal_distance   = 0
    throat_index        = 0
    inter_coord         = 0.
    throat_coord        = 0.
    mouth_coord         = 0.
    exit_coord          = 0.

    ! Calculating the pitch line mid way between 2 blades:
    pitch_line = camber + (0.5*pitch)

    ! Create the upper blade:
    yb_upper     = yb + pitch
    camber_upper = camber + pitch

    ! Finding the upper point using the point and angle:
    ! After rotation by stagger, 'sang' should be taking into consideration,
    do i = 1, np_sidee
       v1_top(i) = pitch_line(i)+ 0.5*pitch
       u1_top(i) = 1/tan(angle(i)+sang+pi/2.)*(v1_top(i)-pitch_line(i))+u(i)
       v2_bot(i) = pitch_line(i)- 0.5*pitch
       u2_bot(i) = 1/tan(angle(i)+sang-pi/2.)*(v2_bot(i)-pitch_line(i))+u(i)
    end do

    ! Identify the throat by intersection point with upper surface of the blade:
    k = 1
    do j = 1, np_sidee
        i_interup1 = 0
        i_interdwn1 = 0

        ! Intersection with upper airfoil
        do i = np_sidee,np - 1  
            call st_line_intersection(u(j),pitch_line(j),u1_top(j),v1_top(j),xb(i),yb_upper(i),xb(i + 1), &
                                      yb_upper(i+1),xint_up,yint_up)
            if ((xint_up >= xb(i) ) .and. (xint_up <= xb(i+1))) then
              x_interup  = xint_up 
              y_interup  = yint_up
              i_interup1 = i 
              i_interup2 = i+1
            end if
        end do      

        ! Intersection with lower airfoil 
        do i = 1,np_sidee - 1       
            call st_line_intersection(u(j),pitch_line(j),u2_bot(j),v2_bot(j),xb(i),yb(i),xb(i + 1),yb(i + 1), &
                                      xint_dwn,yint_dwn)
            if ((xint_dwn <= xb(i) ).and.(xint_dwn >= xb(i+1))) then
              x_interdwn = xint_dwn ;y_interdwn = yint_dwn
              i_interdwn1 = i ;i_interdwn2 = i+1
            end if
        end do

        if ((i_interup1 /= 0) .and. (i_interdwn1 /= 0)) then

            ! First intersection point
            inter_coord(1,k) = x_interup
            inter_coord(2,k) = y_interup

            ! Second intersection point
            inter_coord(3,k) = x_interdwn 
            inter_coord(4,k) = y_interdwn 
            
            ! Number of throats for each section
            n_normal_distance = k  
            k = k + 1
        end if

    end do  ! j = 1,np_sidee

    ! Warn if no throat is found
    if (.not. isquiet) print*, 'n_normal_distance =',n_normal_distance
    write(nopen,*) 'n_normal_distance = ', n_normal_distance
    if(n_normal_distance == 0) then
      write(warning_arg,'(I2)') js
      warning_msg   = 'No throats found because of low number of blades for section '//trim(adjustl(warning_arg)) 
      dev_msg       = 'Check subroutine throat_calc_pitch_line in funcNsubs.f90'
      call warning(warning_msg, dev_msg = dev_msg)
      return
    end if

    ! Writing the mouth and exit areas:
    mouth_coord = inter_coord(:,1)
    exit_coord = inter_coord(:,n_normal_distance)

    if (.not. isquiet) print*,'number of intersection points (k) =',k-1,'from np_side of',np_sidee
    write(nopen,*) 'Number of intersection points (k) = ',k-1,'from np_side of',np_sidee

    if (allocated(throat)) deallocate(throat)
    Allocate (throat(n_normal_distance))
    
    ! Calculation of the throat
    throat(1)     = sqrt((inter_coord(1,1)-inter_coord(3,1))**2+(inter_coord(2,1)-inter_coord(4,1))**2) 
    min_throat_2D = throat(1)
    throat_index  = 1
    do k = 2,n_normal_distance

       throat(k) = sqrt((inter_coord(1,k)-inter_coord(3,k))**2+(inter_coord(2,k)-inter_coord(4,k))**2) 
       if (throat(k) < min_throat_2D) then
          min_throat_2D = throat(k)
          throat_index  = k
          throat_coord  = inter_coord(:,k)
       end if

    end do

    ! Writing to a file for debugging
    if (isdev) then   
        if (.not. isquiet) then          
            print*,""
            print*,'Writing non-dimensional throat points to a file for debugging...'
            print*,""
        end if
        write(nopen,*) ''
        write(nopen,*) 'Writing non-dimensional throat points to a file for debugging...'
        write(nopen,*) ''
        
        if (js == 1) then 
          file4 = 'throat_points.'//trim(casename)//'.txt'
          open(unit = 85, file = file4, form = "formatted")
          write(85,*)'pitch',pitch
        end if
    
        ! write_2D_throat_data in file_operations
        call write_2D_throat_data(js,np,np_sidee,n_normal_distance,casename,u,camber,camber_upper,  &
                                  pitch_line,u1_top,v1_top,u2_bot,v2_bot,inter_coord,min_throat_2D, &
                                  throat_index)
        if (js == nsl) close(85)

    end if   

    call close_log_file(nopen, file_open)

    
end subroutine throat_calc_pitch_line
!------------------------------------------------------------------------------------------------------






!
! Calculate average camber for blade section
!
!------------------------------------------------------------------------------------------------------
subroutine averaged_camber(xb,yb,np,u,camber,angle,sinl)
    implicit none

    integer,                        intent(in)      :: np
    real,                           intent(in)      :: xb(np), yb(np), sinl
    real,                           intent(inout)   :: u((np + 1)/2), camber((np + 1)/2), angle((np + 1)/2)

    ! Local variables
    integer                                         :: i
    real                                            :: u_i((np + 1)/2), camber_i((np + 1)/2)
    logical                                         :: isquiet


    ! Determine isquiet status
    call get_quiet_status(isquiet)

    ! Initialize variables
    camber_i= 0
    u_i     = 0
    camber  = 0
    angle   = 0

    if (.not. isquiet) print*, 'np',np

    ! Calculating the average:
    do i = 1,(np + 1)/2
       u_i(i)      = (xb(i) + xb(np + 1 -i))/2
       camber_i(i) = (yb(i) + yb(np + 1 -i))/2
    end do

    ! Arrange the u and camber
    do i = 1,(np + 1)/2
       u(i)      = u_i(((np + 1)/2) - i + 1)
       camber(i) = camber_i(((np + 1)/2) - i + 1)
    end do

    ! Calculate the camber line angles (slope):
    angle(1)        = atan(sinl)
    do i = 1,((np + 1)/2) - 1
       angle(i + 1) = atan((camber(i + 1) - camber(i))/(u(i + 1) - u(i)))
    end do

   
end subroutine averaged_camber
!------------------------------------------------------------------------------------------------------






!
!
!
!------------------------------------------------------------------------------------------------------
subroutine askr(prompt,rinput)
    character(*),                   intent(in)      :: prompt
    real,                           intent(inout)   :: rinput


    np = index(prompt,'^') - 1
    if (np == 0) np = len(prompt)
    10 write(*,1000) prompt(1:np)
    read (*,*,err=10) rinput
    return
    1000 format(/a,'   r>  ',$)


end subroutine askr
!------------------------------------------------------------------------------------------------------






!
! Create meanline coordinates and periodic walls for both top and bottom surfaces
!
!------------------------------------------------------------------------------------------------------
subroutine meanline3DNperiodicwall(xb,yb,zb,xposlean,yposlean,zposlean,xneglean,yneglean,zneglean,iap, &
                                   nsec,uplmt,scf,casename)
    use file_operations
    implicit none

    integer,                        intent(in)      :: iap, nsec, uplmt
    real,                           intent(in)      :: xb(iap,nsec), yb(iap,nsec), zb(iap,nsec), xposlean(iap,nsec), &
                                                       yposlean(iap,nsec), zposlean(iap,nsec), xneglean(iap,nsec),   &
                                                       yneglean(iap,nsec), zneglean(iap,nsec), scf
    character(32),                  intent(in)      :: casename

    ! Local variables
    integer                                         :: i,ia
    integer,    parameter                           :: nx = 1000, nax = 1000


    real                                            :: xprd_top(nx,nax), yprd_top(nx,nax), zprd_top(nx,nax), &
                                                       xprd_bot(nx,nax), yprd_bot(nx,nax), zprd_bot(nx,nax), &
                                                       xmeanline(nx,nax),ymeanline(nx,nax),zmeanline(nx,nax)


    !
    ! Adding the upstream and downstream curves of constant theta with 6 points on both sides
    !
    do ia = 1, nsec

        ! Upstream coordinates
        xmeanline(1,ia) = -scf*0.20 + scf*xb(uplmt + 1,ia)
        ymeanline(1,ia) = scf*yb(uplmt + 1,ia)
        zmeanline(1,ia) = scf*zb(uplmt + 1,ia)

        ! Top periodic wall coordinates
        xprd_top(1,ia) = -scf*0.20 + scf*xposlean(uplmt + 1,ia)
        yprd_top(1,ia) = scf*yposlean(uplmt + 1,ia)
        zprd_top(1,ia) = scf*zposlean(uplmt + 1,ia)

        ! Bottom periodic wall coordinates
        xprd_bot(1,ia) = -scf*0.20 + scf*xneglean(uplmt + 1,ia)
        yprd_bot(1,ia) = scf*yneglean(uplmt + 1,ia)
        zprd_bot(1,ia) = scf*zneglean(uplmt + 1,ia)
        
        xmeanline(2,ia) = -scf*0.18 + scf*xb(uplmt + 1,ia)
        ymeanline(2,ia) = scf*yb(uplmt + 1,ia)
        zmeanline(2,ia) = scf*zb(uplmt + 1,ia) 
        
        ! Top periodic wall coordinates
        xprd_top(2,ia) = -scf*0.18 + scf*xposlean(uplmt + 1,ia)
        yprd_top(2,ia) = scf*yposlean(uplmt + 1,ia)
        zprd_top(2,ia) = scf*zposlean(uplmt + 1,ia)
        
        ! Bottom periodic wall coordinates
        xprd_bot(2,ia) = -scf*0.18 + scf*xneglean(uplmt + 1,ia)
        yprd_bot(2,ia) = scf*yneglean(uplmt + 1,ia)
        zprd_bot(2,ia) = scf*zneglean(uplmt + 1,ia)
       
        xmeanline(3,ia) = -scf*0.16 + scf*xb(uplmt + 1,ia)
        ymeanline(3,ia) = scf*yb(uplmt + 1,ia)
        zmeanline(3,ia) = scf*zb(uplmt + 1,ia) 
        
        ! Top periodic wall coordinates
        xprd_top(3,ia) = -scf*0.16 + scf*xposlean(uplmt + 1,ia)
        yprd_top(3,ia) = scf*yposlean(uplmt + 1,ia)
        zprd_top(3,ia) = scf*zposlean(uplmt + 1,ia)

        ! Bottom periodic wall coordinates
        xprd_bot(3,ia) = -scf*0.16 + scf*xneglean(uplmt + 1,ia)
        yprd_bot(3,ia) = scf*yneglean(uplmt + 1,ia)
        zprd_bot(3,ia) = scf*zneglean(uplmt + 1,ia)
           
        xmeanline(4,ia) = -scf*0.12 + scf*xb(uplmt + 1,ia)
        ymeanline(4,ia) = scf*yb(uplmt + 1,ia)
        zmeanline(4,ia) = scf*zb(uplmt + 1,ia)
        
        ! Top periodic wall coordinates
        xprd_top(4,ia) = -scf*0.12 + scf*xposlean(uplmt + 1,ia)
        yprd_top(4,ia) = scf*yposlean(uplmt+1,ia)
        zprd_top(4,ia) = scf*zposlean(uplmt+1,ia)
        ! Bottom periodic wall coordinates
        xprd_bot(4,ia) = -scf*0.12 + scf*xneglean(uplmt + 1,ia)
        yprd_bot(4,ia) = scf*yneglean(uplmt + 1,ia)
        zprd_bot(4,ia) = scf*zneglean(uplmt + 1,ia)
        
        xmeanline(5,ia) = -scf*0.10 + scf*xb(uplmt + 1,ia)
        ymeanline(5,ia) = scf*yb(uplmt + 1,ia)
        zmeanline(5,ia) = scf*zb(uplmt + 1,ia)
        
        ! Top periodic wall coordinates
        xprd_top(5,ia) = -scf*0.10 + scf*xposlean(uplmt + 1,ia)
        yprd_top(5,ia) = scf*yposlean(uplmt + 1,ia)
        zprd_top(5,ia) = scf*zposlean(uplmt + 1,ia)
        
        ! Bottom periodic wall coordinates
        xprd_bot(5,ia) = -scf*0.10 + scf*xneglean(uplmt + 1,ia)
        yprd_bot(5,ia) = scf*yneglean(uplmt + 1,ia)
        zprd_bot(5,ia) = scf*zneglean(uplmt + 1,ia)
        
        xmeanline(6,ia) = -scf*0.06 + scf*xb(uplmt + 1,ia)
        ymeanline(6,ia) = scf*yb(uplmt + 1,ia)
        zmeanline(6,ia) = scf*zb(uplmt + 1,ia)
        
        ! Top periodic wall coordinates
        xprd_top(6,ia) = -scf*0.06 + scf*xposlean(uplmt + 1,ia)
        yprd_top(6,ia) = scf*yposlean(uplmt + 1,ia)
        zprd_top(6,ia) = scf*zposlean(uplmt + 1,ia)
        
        ! Bottom periodic wall coordinates
        xprd_bot(6,ia) = -scf*0.06 + scf*xneglean(uplmt + 1,ia)
        yprd_bot(6,ia) = scf*yneglean(uplmt + 1,ia)
        zprd_bot(6,ia) = scf*zneglean(uplmt + 1,ia)
    
        ! 
        ! Average of the top and bottom curves
        !
        do i = 7,uplmt

           xmeanline(i,ia) = scf*(xb((uplmt + 1) - (i - 1),ia) + xb((uplmt + 1) + (i - 1),ia))*0.5
           ymeanline(i,ia) = scf*(yb((uplmt + 1) - (i - 1),ia) + yb((uplmt + 1) + (i - 1),ia))*0.5
           zmeanline(i,ia) = scf*(zb((uplmt + 1) - (i - 1),ia) + zb((uplmt + 1) + (i - 1),ia))*0.5 
           
           ! Top periodic wall coordinates
           xprd_top(i,ia) = scf*(xposlean((uplmt + 1) - (i - 1),ia) + xposlean((uplmt + 1) + (i - 1),ia))*0.5
           yprd_top(i,ia) = scf*(yposlean((uplmt + 1) - (i - 1),ia) + yposlean((uplmt + 1) + (i - 1),ia))*0.5
           zprd_top(i,ia) = scf*(zposlean((uplmt + 1) - (i - 1),ia) + zposlean((uplmt + 1) + (i - 1),ia))*0.5
           
           ! Bottom periodic wall coordinates
           xprd_bot(i,ia) = scf*(xneglean((uplmt + 1) - (i - 1),ia) + xneglean((uplmt + 1) + (i - 1),ia))*0.5
           yprd_bot(i,ia) = scf*(yneglean((uplmt + 1) - (i - 1),ia) + yneglean((uplmt + 1) + (i - 1),ia))*0.5
           zprd_bot(i,ia) = scf*(zneglean((uplmt + 1) - (i - 1),ia) + zneglean((uplmt + 1) + (i - 1),ia))*0.5 

        end do
        
        ! Downstream coordinates
        xmeanline(uplmt+1,ia) = scf*0.06 + scf*xb(iap,ia)
        ymeanline(uplmt+1,ia) = scf*yb(iap,ia)
        zmeanline(uplmt+1,ia) = scf*zb(iap,ia)
        
        ! Top periodic wall coordinates
        xprd_top(uplmt+1,ia) = scf*0.06 + scf*xposlean(iap,ia)
        yprd_top(uplmt+1,ia) = scf*yposlean(iap,ia)
        zprd_top(uplmt+1,ia) = scf*zposlean(iap,ia)
        
        ! Bottom periodic wall coordinates
        xprd_bot(uplmt+1,ia) = scf*0.06 + scf*xneglean(iap,ia)
        yprd_bot(uplmt+1,ia) = scf*yneglean(iap,ia)
        zprd_bot(uplmt+1,ia) = scf*zneglean(iap,ia)
       
        xmeanline(uplmt+2,ia) = scf*0.10 + scf*xb(iap,ia)
        ymeanline(uplmt+2,ia) = scf*yb(iap,ia)
        zmeanline(uplmt+2,ia) = scf*zb(iap,ia) 
        
        ! Top periodic wall coordinates
        xprd_top(uplmt+2,ia) = scf*0.10 + scf*xposlean(iap,ia)
        yprd_top(uplmt+2,ia) = scf*yposlean(iap,ia)
        zprd_top(uplmt+2,ia) = scf*zposlean(iap,ia)
        
        ! Bottom periodic wall coordinates
        xprd_bot(uplmt+2,ia) = scf*0.10 + scf*xneglean(iap,ia)
        yprd_bot(uplmt+2,ia) = scf*yneglean(iap,ia)
        zprd_bot(uplmt+2,ia) = scf*zneglean(iap,ia)
        
        xmeanline(uplmt+3,ia) = scf*0.12 + scf*xb(iap,ia)
        ymeanline(uplmt+3,ia) = scf*yb(iap,ia)
        zmeanline(uplmt+3,ia) = scf*zb(iap,ia) 
        
        ! Top periodic wall coordinates
        xprd_top(uplmt+3,ia) = scf*0.12 + scf*xposlean(iap,ia)
        yprd_top(uplmt+3,ia) = scf*yposlean(iap,ia)
        zprd_top(uplmt+3,ia) = scf*zposlean(iap,ia)
        
        ! Bottom periodic wall coordinates
        xprd_bot(uplmt+3,ia) = scf*0.12 + scf*xneglean(iap,ia)
        yprd_bot(uplmt+3,ia) = scf*yneglean(iap,ia)
        zprd_bot(uplmt+3,ia) = scf*zneglean(iap,ia)
           
        xmeanline(uplmt+4,ia) = scf*0.16 + scf*xb(iap,ia)
        ymeanline(uplmt+4,ia) = scf*yb(iap,ia)
        zmeanline(uplmt+4,ia) = scf*zb(iap,ia)
     
        ! Top periodic wall coordinates
        xprd_top(uplmt+4,ia) = scf*0.16 + scf*xposlean(iap,ia)
        yprd_top(uplmt+4,ia) = scf*yposlean(iap,ia)
        zprd_top(uplmt+4,ia) = scf*zposlean(iap,ia)
      
        ! Bottom periodic wall coordinates
        xprd_bot(uplmt+4,ia) = scf*0.16 + scf*xneglean(iap,ia)
        yprd_bot(uplmt+4,ia) = scf*yneglean(iap,ia)
        zprd_bot(uplmt+4,ia) = scf*zneglean(iap,ia)
        
        xmeanline(uplmt+5,ia) = scf*0.18+ scf*xb(iap,ia)
        ymeanline(uplmt+5,ia) = scf*yb(iap,ia)
        zmeanline(uplmt+5,ia) = scf*zb(iap,ia)
       
        ! Top periodic wall coordinates
        xprd_top(uplmt+5,ia) = scf*0.18 + scf*xposlean(iap,ia)
        yprd_top(uplmt+5,ia) = scf*yposlean(iap,ia)
        zprd_top(uplmt+5,ia) = scf*zposlean(iap,ia)
        
        ! Bottom periodic wall coordinates
        xprd_bot(uplmt+5,ia) = scf*0.18 + scf*xneglean(iap,ia)
        yprd_bot(uplmt+5,ia) = scf*yneglean(iap,ia)
        zprd_bot(uplmt+5,ia) = scf*zneglean(iap,ia)
        
        xmeanline(uplmt+6,ia) = scf*0.20 + scf*xb(iap,ia)
        ymeanline(uplmt+6,ia) = scf*yb(iap,ia)
        zmeanline(uplmt+6,ia) = scf*zb(iap,ia)
        
        ! Top periodic wall coordinates
        xprd_top(uplmt+6,ia) = scf*0.20 + scf*xposlean(iap,ia)
        yprd_top(uplmt+6,ia) = scf*yposlean(iap,ia)
        zprd_top(uplmt+6,ia) = scf*zposlean(iap,ia)
       
        ! Bottom periodic wall coordinates
        xprd_bot(uplmt+6,ia) = scf*0.20 + scf*xneglean(iap,ia)
        yprd_bot(uplmt+6,ia) = scf*yneglean(iap,ia)
        zprd_bot(uplmt+6,ia) = scf*zneglean(iap,ia)
       
        ! 
        ! Writing meanline coordinates to a file
        ! write_3D_meanline in file_operations
        !
        call write_3D_meanline(ia,uplmt,casename,nx,nax,xmeanline,ymeanline,zmeanline)
    
    end do  ! do ia = 1,nsec

    write(*,*)


end subroutine meanline3DNperiodicwall
!------------------------------------------------------------------------------------------------------






!
! Offset function
!
!------------------------------------------------------------------------------------------------------
real function numoffset(num,delta)
    implicit none
    
    real,                       intent(in)      :: delta, num(1)


    numoffset = num(1) + delta


end function
!------------------------------------------------------------------------------------------------------






!
! Creates constant slope meanlines and periodic walls for both top and bottom surfaces
!
!------------------------------------------------------------------------------------------------------
subroutine constantslopemeanline3D(xb,yb,zb,xposlean,yposlean,zposlean,xneglean,yneglean,zneglean,iap, &
                                   nsec,uplmt,scf,casename)
    use file_operations
    implicit none

    integer,                        intent(in)      :: iap, nsec, uplmt
    real,                           intent(in)      :: xb(iap,nsec), yb(iap,nsec), zb(iap,nsec), scf,              &
                                                       xposlean(iap,nsec), yposlean(iap,nsec), zposlean(iap,nsec), &
                                                       xneglean(iap,nsec), yneglean(iap,nsec), zneglean(iap,nsec)
    character(32),                  intent(in)      :: casename
    
    ! Local variables
    integer                                         :: i, ia, j, npts
    integer,    parameter                           :: nx = 1000, nax = 1000
    real                                            :: xprd_top(nx,nax), yprd_top(nx,nax), zprd_top(nx,nax),     &
                                                       xprd_bot(nx,nax), yprd_bot(nx,nax), zprd_bot(nx,nax),     &
                                                       xmeanline(nx,nax), ymeanline(nx,nax), zmeanline(nx,nax),  &
                                                       phi_xz_up,phi_xz_dwn,phi_xy_up,phi_xy_dwn,coefficient(6), &
                                                       mag_coff
    logical                                         :: isquiet


    ! Get the value of isquiet
    call get_quiet_status(isquiet)

    ! Number of points added
    npts = 4 

    ! Magnification factor for the periodic boundaries 
    mag_coff = 1 

    !
    ! Average of the top and bottom curves (meanline calculation)
    !
    do ia = 1, nsec
       
        do i = npts+1,uplmt

            xmeanline(i,ia) = scf*(xb((uplmt + 1) - (i - 1),ia) + xb((uplmt + 1) + (i - 1),ia))*0.5
            ymeanline(i,ia) = scf*(yb((uplmt + 1) - (i - 1),ia) + yb((uplmt + 1) + (i - 1),ia))*0.5
            zmeanline(i,ia) = scf*(zb((uplmt + 1) - (i - 1),ia) + zb((uplmt + 1) + (i - 1),ia))*0.5 
            
            ! Top periodic wall coordinates
            xprd_top(i,ia)  = scf*(xposlean((uplmt + 1) - (i - 1),ia) + xposlean((uplmt + 1) + (i - 1),ia))*0.5
            yprd_top(i,ia)  = scf*(yposlean((uplmt + 1) - (i - 1),ia) + yposlean((uplmt + 1) + (i - 1),ia))*0.5
            zprd_top(i,ia)  = scf*(zposlean((uplmt + 1) - (i - 1),ia) + zposlean((uplmt + 1) + (i - 1),ia))*0.5
            
            ! Bottom periodic wall coordinates
            xprd_bot(i,ia)  = scf*(xneglean((uplmt + 1) - (i - 1),ia) + xneglean((uplmt + 1) + (i - 1),ia))*0.5
            yprd_bot(i,ia)  = scf*(yneglean((uplmt + 1) - (i - 1),ia) + yneglean((uplmt + 1) + (i - 1),ia))*0.5
            zprd_bot(i,ia)  = scf*(zneglean((uplmt + 1) - (i - 1),ia) + zneglean((uplmt + 1) + (i - 1),ia))*0.5 

        end do 
        
        ! Calculating the slope angles in xz and xy planes from meanline coordinates
        phi_xz_up  = atan(real(zmeanline(npts + 2,ia) - zmeanline(npts + 1,ia))/(xmeanline(npts + 2,ia) &
                     - xmeanline(npts + 1,ia)))
        phi_xy_up  = atan(real(ymeanline(npts + 2,ia) - ymeanline(npts + 1,ia))/(xmeanline(npts + 2,ia) &
                     - xmeanline(npts + 1,ia)))
        phi_xz_dwn = atan(real(zmeanline(uplmt,ia) - zmeanline(uplmt - 1,ia))/(xmeanline(uplmt,ia)      &
                     - xmeanline(uplmt - 1,ia)))
        phi_xy_dwn = atan(real(ymeanline(uplmt,ia) - ymeanline(uplmt - 1,ia))/(xmeanline(uplmt,ia)      &
                     - xmeanline(uplmt - 1,ia)))  
        
        coefficient = [ 0.04, 0.05, 0.06, 0.07, 0.08, 0.1 ]* mag_coff
        
        ! Constant slope meanline (upstream coordinates)
        do j = npts, 1, -1

            xmeanline(j,ia) = -scf*coefficient(npts + 1 - j) + scf*xb(uplmt + 1,ia)
            ymeanline(j,ia) = (xmeanline(j,ia) - xmeanline(j + 1,ia))*tan(phi_xy_up) + ymeanline(j + 1,ia)
            zmeanline(j,ia) = (xmeanline(j,ia) - xmeanline(j + 1,ia))*tan(phi_xz_up) + zmeanline(j + 1,ia)
            
            ! Top periodic wall coordinates
            xprd_top(j,ia)  = -scf*coefficient(npts + 1 - j) + scf*xposlean(uplmt + 1,ia)
            yprd_top(j,ia)  = (xprd_top(j,ia) - xprd_top(j + 1,ia))*tan(phi_xy_up) + scf*yprd_top(j + 1,ia)
            zprd_top(j,ia)  = (xprd_top(j,ia) - xprd_top(j + 1,ia))*tan(phi_xz_up) + scf*zprd_top(j + 1,ia)
            
            ! Bottom periodic wall coordinates
            xprd_bot(j,ia)  = -scf*coefficient(npts + 1 - j) + scf*xneglean(uplmt + 1,ia)
            yprd_bot(j,ia)  = (xprd_bot(j,ia) - xprd_bot(j + 1,ia))*tan(phi_xy_up) + scf*yprd_bot(j + 1,ia)
            zprd_bot(j,ia)  = (xprd_bot(j,ia) - xprd_bot(j + 1,ia))*tan(phi_xz_up) + scf*zprd_bot(j + 1,ia)

        end do
       
        ! Constant slope meanline (downstream coordinates)
        do j = 1 , npts
           
           xmeanline(uplmt + j,ia) = scf*coefficient(j) + scf*xb(iap,ia)
           ymeanline(uplmt + j,ia) = (xmeanline(uplmt + j,ia) - xmeanline(uplmt + j - 1,ia))*tan(phi_xy_dwn) + &
                                     ymeanline(uplmt + j - 1,ia)
           zmeanline(uplmt + j,ia) = (xmeanline(uplmt + j,ia) - xmeanline(uplmt + j - 1,ia))*tan(phi_xz_dwn) + &
                                     zmeanline(uplmt + j - 1,ia)
           
           ! Top periodic wall coordinates
           xprd_top(uplmt + j,ia) = scf*coefficient(j) + scf*xposlean(iap,ia)
           yprd_top(uplmt + j,ia) = (xprd_top(uplmt + j,ia) - xprd_top(uplmt + j - 1,ia))*tan(phi_xy_dwn) + &
                                    yprd_top(uplmt + j - 1,ia)
           zprd_top(uplmt + j,ia) = (xprd_top(uplmt + j,ia) - xprd_top(uplmt + j - 1,ia))*tan(phi_xz_dwn) + &
                                    zprd_top(uplmt + j - 1,ia)
           
           ! Bottom periodic wall coordinates
           xprd_bot(uplmt + j,ia) = scf*coefficient(j) + scf*xneglean(iap,ia)
           yprd_bot(uplmt + j,ia) = (xprd_bot(uplmt + j,ia) - xprd_bot(uplmt + j - 1,ia))*tan(phi_xy_dwn) + &
                                    yprd_bot(uplmt + j - 1,ia)
           zprd_bot(uplmt + j,ia) = (xprd_bot(uplmt + j,ia) - xprd_bot(uplmt + j - 1,ia))*tan(phi_xz_dwn) + &
                                    zprd_bot(uplmt + j - 1,ia)

        end do
       
        ! Write meanline coordinates to a file
        ! write_constantslope_meanline in file_operations
        call write_constantslope_meanline(ia,uplmt,npts,casename,nx,nax,xmeanline,ymeanline,zmeanline)

    end do  ! do ia = 1,nsec

    if (.not. isquiet) write(*,*)


end subroutine constantslopemeanline3D
!------------------------------------------------------------------------------------------------------






!
! Get value of isdev from globvar
!
!------------------------------------------------------------------------------------------------------
subroutine get_dev_status(isdev_local)
    use globvar
    implicit none

    logical,                    intent(inout)       :: isdev_local


    isdev_local = isdev


end subroutine get_dev_status
!------------------------------------------------------------------------------------------------------






!
! Get value of isquiet from globvar
!
!------------------------------------------------------------------------------------------------------
subroutine get_quiet_status(isquiet_local)
    use globvar
    implicit none

    logical,                    intent(inout)       :: isquiet_local


    isquiet_local = isquiet


end subroutine get_quiet_status
!------------------------------------------------------------------------------------------------------






!
! Get current section number from globvar
!
!------------------------------------------------------------------------------------------------------
subroutine get_sec_number(js_local)
    use globvar
    implicit none

    integer,                    intent(inout)       :: js_local


    js_local = js


end subroutine get_sec_number
!------------------------------------------------------------------------------------------------------






!
! Get thickness multiplier switch value from globvar
!
!------------------------------------------------------------------------------------------------------
subroutine get_thick_status(thick_local)
    use globvar
    implicit none

    integer,                    intent(inout)       :: thick_local


    thick_local = thick


end subroutine get_thick_status
!------------------------------------------------------------------------------------------------------






!
! Subroutine to solve a tridiagonal linear system
!
! Input parameters: n   - size of incoming square matrix
!                   d   - elements along the diagonal
!                   ld  - elements along the sub-diagonal
!                   ud  - elements along the super diagonal
!
!------------------------------------------------------------------------------------------------------
subroutine tridiag_solve(d, ld, ud, r, n)
    use errors
    implicit none

    integer,                    intent(in)          :: n
    real,                       intent(inout)       :: d(n), ld(n), ud(n), r(n)

    ! Local variables
    integer                                         :: k
    real                                            :: m, tol = 10E-10
    character(:),   allocatable                     :: error_msg, dev_msg


    do k = 2,n

        ! Check for zero diagonal elements
        if (abs(d(k - 1)) .le. tol) then
            error_msg   = 'tridiag_solve failed - zero diagonal element'
            dev_msg     = 'Check subroutine tridiag_solve in spline.f90'
            call fatal_error(error_msg, dev_msg = dev_msg)
        end if

        m       = ld(k)/d(k - 1)
        d(k)    = d(k) - (m*ud(k - 1))
        r(k)    = r(k) - (m*r(k - 1))

    end do

    ! Check for zero element along the diagonal
    if (abs(d(n)) .le. tol) then
        error_msg   = 'tridiag_solve failed - zero diagonal element'
        dev_msg     = 'Check subroutine tridiag_solve in spline.f90'
        call fatal_error(error_msg, dev_msg = dev_msg)
    end if

    r(n)    = r(n)/d(n)

    do k = n - 1, 1, -1
        r(k)    = (r(k) - (ud(k)*r(k + 1)))/d(k)
    end do


end subroutine tridiag_solve
!------------------------------------------------------------------------------------------------------






!
! Subroutine for Gauss-Jordan elimination to solve a nxn linear system by converting the 
! coefficient matrix to its reduced row echelon form
! Row pivoting is implemented
!
! Input parameters: n           - size of incoming square matrix
!                   nrhs        - no. of columns of RHS vector
!
!------------------------------------------------------------------------------------------------------
subroutine gauss_jordan(n, nrhs, a, fail_flag)
    use errors
    implicit none

    integer,                    intent(in)          :: n
    integer,                    intent(in)          :: nrhs
    real,                       intent(inout)       :: a(n, n + nrhs)
    integer,                    intent(inout)       :: fail_flag

    ! Local variables
    integer                                         :: i, j, c, ipvt
    real                                            :: pvt, temp(n + nrhs)
    real,   parameter                               :: eps = 10e-16
    character(:),   allocatable                     :: error_msg, dev_msg


    ! Set number of columns
    c           = n + nrhs

    ! Initialize flag
    fail_flag   = 0

    do i = 1,n

        ! Determining pivot row and coefficient
        ipvt                    = i
        pvt                     = a(i,i)
        
        do j = i + 1,n
            if (abs(pvt) < abs(a(j,i))) then
                pvt             = a(j,i)
                ipvt            = j
            end if
        end do

        ! If all pivot column elements are zero, return fail
        if (abs(pvt) < eps) then
            error_msg   = 'gauss_jordan - zero pivot term'
            dev_msg     = 'Check subroutine gauss_jordan in funcNsubs.f90'
            call error(error_msg, dev_msg)
            fail_flag           = 1
            return
        end if

        ! Interchange current row and pivot row
        temp                    = a(ipvt,:)
        a(ipvt,:)               = a(i,:)
        a(i,:)                  = temp

        ! Eliminate coefficients below and above pivot
        ! Explicit back substitution not required
        a(i,i)                  = 1.0
        a(i,i + 1:c)            = a(i,i + 1:c)/pvt
        
        do j = 1,n
            if (j /= i) then
                a(j,i + 1:c)    = a(j,i + 1:c) - a(j,i) * a(i,i + 1:c)
                a(j,i)          = 0.0
            end if
        end do

    end do  ! i = 1,n


end subroutine gauss_jordan
!------------------------------------------------------------------------------------------------------






!
! Input parameters: x   - coordinate at which polynomial has to be evaluated
!                   cf  - array of polynomial coefficients
!                   ord - order of the polynomial to be computed
!
!------------------------------------------------------------------------------------------------------
real function feval(x,cf,ord)
    implicit none

    integer,                    intent(in)          :: ord
    real,                       intent(in)          :: x, cf(ord)

    ! Local variables
    integer                                         :: i

    feval       = cf(1)

    do i = 2,ord + 1
       feval    = feval + (cf(i)*(x**(i - 1))) 
    end do


end function feval
!------------------------------------------------------------------------------------------------------






!
! Input parameters: ord     - order of polynomial
!                   cf      - array of coefficients
!                   a_bound - lower bound of bounding interval
!                   b_bound - upper bound of bounding interval
!                   
!------------------------------------------------------------------------------------------------------
subroutine poly_solve_bisect(ord,cf,a_bound,b_bound,er,x)
    implicit none

    integer,                    intent(in)          :: ord
    real,                       intent(in)          :: cf(ord)
    real,                       intent(in)          :: a_bound
    real,                       intent(in)          :: b_bound
    integer,                    intent(inout)       :: er
    real,                       intent(inout)       :: x(4)

    ! Local variables
    integer                                         :: i, maxiter = 200
    real                                            :: feval, tol = 1e-6, a, b, m, ya, yb, ym, bnd


    a       = a_bound
    ya      = feval(a,cf,ord)
    b       = b_bound
    yb      = feval(b,cf,ord)

    ! Error if bounding interval doesn't contain roots
    if (ya*yb > 0) then
        er  = 1
        return
    end if

    ! Bisection method
    do i = 1,maxiter
        
        m   = (a + b)/2.0
        ym  = feval(m,cf,ord)
        bnd = (b - a)/2.0

        if (abs(ym) < tol) exit

        if (ym*ya < 0) then
            b   = m
            yb  = ym
        else
            a   = m
            ya  = ym
        end if
    
    end do

    x           = ym

    ! Error if number of iterations exceeds maximum iterations
    if (i == maxiter) then
        er      = 2
    end if


end subroutine poly_solve_bisect
!------------------------------------------------------------------------------------------------------






!
! Input parameters: ceff    - array of polynomial coefficients
!
! Solves a quadratic polynomial
!
!------------------------------------------------------------------------------------------------------
subroutine quadratic_roots(ceff, root)
    implicit none

    ! Precision variables
    integer,    parameter                           :: sp = kind(0.0), dp = kind(0.0D0)

    real(kind = dp),            intent(in)          :: ceff(3) 
    complex(kind = dp),         intent(inout)       :: root(2) 
    
    ! Local variables
    real(kind = dp),parameter:: eps = epsilon(0.0D0)
    real(dp):: d, r, w, x, y


    ! If equation is linear
    if(ceff(1) == 0.0) then
        root(1)         = (0.D0,0.D0)
        root(2)         = cmplx(-ceff(2)/ceff(3), 0.0D0,dp)
        return
    end if

    ! Compute discriminant
    d                   = ceff(2)*ceff(2) - 4.0D0*ceff(1)*ceff(3)             
    if (abs(d) <= 2.0D0*eps*ceff(2)*ceff(2)) then
        root(1)         = cmplx(-0.5D0*ceff(2)/ceff(3), 0.0D0, dp) 
        root(2)         = root(1)
        return
    end if

    ! Complex roots if discriminant is negative
    r = sqrt(abs(d))
    if (d < 0.0D0) then
        x               = -0.5D0*ceff(2)/ceff(3)        
        y               = abs(0.5D0*r/ceff(3))
        root(1)         = cmplx(x, y, dp)
        root(2)         = cmplx(x,-y, dp)   
        return
    end if

    ! Numerical Recipes, sec. 5.5
    if (ceff(2) /= 0.0D0) then              
        w               = -(ceff(2) + sign(r,ceff(2)))
        root(1)         = cmplx(2.0D0*ceff(1)/w,  0.0D0, dp)
        root(2)         = cmplx(0.5D0*w/ceff(3), 0.0D0, dp)
        return
    end if

    ! If equation is of the form (x^2 - 1)
    x                   = abs(0.5D0*r/ceff(3))   
    root(1)             = cmplx( x, 0.0D0, dp)
    root(2)             = cmplx(-x, 0.0D0, dp)
    return


end subroutine quadratic_roots
!------------------------------------------------------------------------------------------------------






!
! Input parameters: ceff    - array of polynomial coefficients
!
! Solves a cubic polynomial
!
!------------------------------------------------------------------------------------------------------
subroutine cubic_roots(ceff, root)
    implicit none

    ! Precision variables
    integer,    parameter                           :: sp = kind(0.0), dp = kind(1.0D0)

    real(kind = dp),            intent(in)          :: ceff(4)
    complex(kind = dp),         intent(inout)       :: root(3)

    ! Local variables
    real(kind = dp),    parameter                   :: rt3 = 1.7320508075689D0, eps = epsilon(0.0D0)
    real(kind = dp)                                 :: aq(3), arg, c, cf, d, p, p1, q, q1, r, ra, rb,  &
                                                       rq, rt, r1, s, sf, sq, sum1, t, tol, t1, w, w1, &
                                                       w2, x, x1, x2, x3, y, y1, y2, y3


    ! If equation is quadratic, call previous subroutine
    if (ceff(1) == 0.0) then
        root(1)                             = (0.D0,0.D0)
        call quadratic_roots(ceff(2:4), root(2:3))
        return
    end if

    p                                       = ceff(3)/(3.0D0*ceff(4))
    q                                       = ceff(2)/ceff(4)
    r                                       = ceff(1)/ceff(4)
    tol                                     = 4.0D0*eps

    c                                       = 0.0D0
    t                                       = ceff(2) - p*ceff(3)
    if (abs(t) > tol*abs(ceff(2))) &
        c                                   = t/ceff(4)

    t                                       = 2.0D0*p*p - q
    if (abs(t) <= tol*abs(q)) &
        t                                   = 0.0D0
    d                                       = r + p*t

    if (abs(d) <= tol*abs(r)) then
        
        root(1)                             = cmplx(-p, 0.0D0,dp)
        w                                   = sqrt(abs(c))
        if (c < 0.0D0) then
            if (p /= 0.0D0) then
                x                           = -(p + sign(w,p))
                root(3)                     = cmplx(x, 0.0D0,dp)
                t                           = 3.0D0*ceff(1)/(ceff(3)*x)
                if (abs(p) > abs(t)) then
                    root(2)                 = root(1)
                    root(1)                 = cmplx(t, 0.0D0,dp)
                else
                    root(2)                 = cmplx(t, 0.0D0,dp)
                end if
            else
                root(2)                     = cmplx(w, 0.0D0,dp)
                root(3)                     = cmplx(-w, 0.0D0,dp)
            end if

        else
            root(2)                         = cmplx(-p, w,dp)
            root(3)                         = cmplx(-p,-w,dp)
        end if
        
        return

    end if

    s                                       = max(abs(ceff(1)), abs(ceff(2)), &
                                                  abs(ceff(3)))
    p1                                      = ceff(3)/(3.0D0*s)
    q1                                      = ceff(2)/s
    r1                                      = ceff(1)/s

    t1                                      = q - 2.25D0*p*p
    if (abs(t1) <= tol*abs(q)) &
        t1                                  = 0.0D0
    w                                       = 0.25D0*r1*r1
    w1                                      = 0.5D0*p1*r1*t
    w2                                      = q1*q1*t1/27.0D0

    if (w1 >= 0.0D0) then
        w                                   = w + w1
        sq                                  = w + w2
    else if (w2 < 0.0D0) then
        sq                                  = w + (w1 + w2)
    else
        w                                   = w + w2
        sq                                  = w + w1
    end if

    if (abs(sq) <= tol*w) &
        sq                                  = 0.0D0
    rq                                      = abs(s/ceff(4))*sqrt(abs(sq))
    
    if (sq >= 0.0D0) then
        
        ra                                  = (-0.5D0*d - sign(rq,d))
        if (ra > 0.0D0) then
            ra                              = ra**(1.0D0/3.0D0)
        else if (ra < 0.0D0) then
            ra                              = -1.0D0*(-1.0D0*ra)**(1.0D0/3.0D0)
        else
            ra                              = 0.0D0
        end if
        
        rb                                  = -c/(3.0D0*ra)
        t                                   = ra + rb
        w                                   = -p
        x                                   = -p
        
        if (abs(t) > tol*abs(ra)) then
            w                               = t - p
            x                               = -0.5D0*t - p
            if (abs(x) <= tol*abs(p)) &
                x                           = 0.0D0
        end if

        t                                   = abs(ra - rb)
        y                                   = 0.5D0*rt3*t
        if (t <= tol*abs(ra)) then
            
            if (abs(x) < abs(w)) then
                if (abs(w) < 0.1D0*abs(x)) &
                    w                       = - (r/x)/x
                root(1)                     = cmplx(w, 0.0D0,dp)
                root(2)                     = cmplx(x, 0.0D0,dp)
                root(3)                     = root(2)
                return
            else
                if (abs(x) < 0.1D0*abs(w)) then
                else
                    root(1)                 = cmplx(x, 0.0D0,dp)
                    root(2)                 = root(1)
                    root(3)                 = cmplx(w, 0.0D0,dp)
                    return
                end if
            end if

        else
            
            if (abs(x) < abs(y)) then
                s                           = abs(y)
                t                           = x/y
            else
                s                           = abs(x)
                t                           = y/x
            end if
            
            if (s < 0.1D0*abs(w)) then
            else
                w1                          = w/s
                sum1                        = 1.0D0 + t*t
                if (w1*w1 < 0.01D0*sum1) &
                    w                       = - ((r/sum1)/s)/s
                root(1)                     = cmplx(w, 0.0D0,dp)
                root(2)                     = cmplx(x, y,dp)
                root(3)                     = cmplx(x,-y,dp)
                return
            end if

        end if

    else
        
        arg                                 = atan2(rq, -0.5D0*d)
        cf                                  = cos(arg/3.0D0)
        sf                                  = sin(arg/3.0D0)
        rt                                  = sqrt(-c/3.0D0)
        y1                                  = 2.0D0*rt*cf
        y2                                  = -rt*(cf + rt3*sf)
        y3                                  = -(d/y1)/y2
        x1                                  = y1 - p
        x2                                  = y2 - p
        x3                                  = y3 - p
        if (abs(x1) > abs(x2)) then
            t                               = x1 
            x1                              = x2 
            x2                              = t
        end if
        if (abs(x2) > abs(x3)) then
            t                               = x2
            x2                              = x3
            x3                              = t
        end if
        if (abs(x1) > abs(x2)) then
            t                               = x1
            x1                              = x2
            x2                              = t
        end if
        w                                   = x3

        if (abs(x2) < 0.1D0*abs(x3)) then
        else if (abs(x1) < 0.1D0*abs(x2)) then
            x1                              = - (r/x3)/x2
            root(1)                         = cmplx(x1, 0.0D0,dp)
            root(2)                         = cmplx(x2, 0.0D0,dp)
            root(3)                         = cmplx(x3, 0.0D0,dp)
            return
        end if

    end if

    aq(1)                                   = ceff(1)
    aq(2)                                   = ceff(2) + ceff(1)/w
    aq(3)                                   = -ceff(4)*w
    call quadratic_roots(aq, root)
    root(3)                                 = cmplx(w, 0.0D0,dp)
    if (aimag(root(1)) == 0.0D0) return
    root(3)                                 = root(2)
    root(2)                                 = root(1)
    root(1)                                 = cmplx(w, 0.0D0,dp)
    return


end subroutine cubic_roots
!------------------------------------------------------------------------------------------------------






!
! Input parameters: ceff    - array of polynomial coefficients
!
! Solves a quartic polynomial
!
!------------------------------------------------------------------------------------------------------
subroutine quartic_roots (ceff, er, root)
    use errors
    implicit none

    ! Precision variables
    integer,    parameter                           :: sp = kind(0.0), dp = kind(0.0D0)

    real(kind = dp),            intent(in)          :: ceff(5)
    integer,                    intent(inout)       :: er
    complex(kind = dp),         intent(inout)       :: root(4)
    
    ! Local variables
    real(kind = dp)                                 :: p, q, r, t, v1, v2, x, y, u, h, v, &
                                                       x1, x2, x3, b, c, d, e, temp(4)
    complex(kind = dp)                              :: w
    integer                                         :: i, j
    character(:),   allocatable                     :: counter, warning_msg, dev_msg


    ! Initialize counter
    counter = ''

    ! If equation is cubic, use the previous subroutine
    if (ceff(1) == 0.0) then
        root(1)                             = (0.D0,0.D0)
        call cubic_roots(ceff(2:), root(2:))
        return
    end if

    b                                       = ceff(4)/(4.0D0*ceff(5))
    c                                       = ceff(3)/ceff(5)
    d                                       = ceff(2)/ceff(5)
    e                                       = ceff(1)/ceff(5)

    p                                       = 0.5D0*(c - 6.0D0*b*b)
    q                                       = d - 2.0D0*b*(c - 4.0D0*b*b)
    r                                       = b*b*(c - 3.0D0*b*b) - b*d + e
    temp(1)                                 = -q*q/64.0D0
    temp(2)                                 = 0.25D0*(p*p - r)
    temp(3)                                 =  p
    temp(4)                                 = 1.0D0

    call cubic_roots(temp, root)
    do i = 1, 3
        if (root(i) /= root(i)) then
            write(counter, '(i2)') i
            warning_msg = 'cubic_roots subroutine failed: '//counter//'th root undefined'
            dev_msg     = 'Check subroutine quartic_roots in funcNsubs.f90'
            call warning(warning_msg, dev_msg = dev_msg)
        end if
    end do

    if (aimag(root(2)) == 0.0D0) then
        
        x1                                  = dble(root(1))
        x2                                  = dble(root(2))
        x3                                  = dble(root(3))
        if (x1 > x2) then
            t                               = x1 
            x1                              = x2
            x2                              = t
        end if
        if (x2 > x3) then
            t                               = x2 
            x2                              = x3 
            x3                              = t
        end if
        if (x1 > x2) then
            t                               = x1 
            x1                              = x2 
            x2                              = t
        end if
        u                                   = 0.0D0
        if (x3 > 0.0D0) &
            u                               = sqrt(x3)

        if (x2 <= 0.0D0) then
            v1                              = sqrt(abs(x1))
            v2                              = sqrt(abs(x2))
            if (q < 0.0D0) &
                u                           = -u
            x                               = -u - b
            y                               = v1 - v2
            root(1)                         = cmplx(x, y, dp)
            root(2)                         = cmplx(x,-y, dp)
            x                               =  u - b
            y                               = v1 + v2
            root(3)                         = cmplx(x, y, dp)
            root(4)                         = cmplx(x,-y, dp)
            return
        else if (x1 >= 0.0D0) then
            x1                              = sqrt(x1)
            x2                              = sqrt(x2)
            if (q > 0.0D0) &
                x1                          = -x1
            temp(1)                         = x1 + x2 + u - b
            temp(2)                         = -x1 - x2 + u - b
            temp(3)                         = x1 - x2 - u - b
            temp(4)                         = -x1 + x2 - u - b
            do i = 1, 3
                do j = i, 4
                    if (temp(j) == minval(temp(i:4))) then
                        t                   = temp(j) 
                        temp(j)             = temp(i)
                        temp(i)             = t
                        exit
                    end if
                end do
            end do

            if (abs(temp(1)) < 0.1D0*abs(temp(4))) then
                t                           = temp(2)*temp(3)*temp(4)
                if (t /= 0.0D0) &
                    temp(1)                 = e/t
            end if
            
            root(1)                         = cmplx(temp(1), 0.0D0, dp)
            root(2)                         = cmplx(temp(2), 0.0D0, dp)
            root(3)                         = cmplx(temp(3), 0.0D0, dp)
            root(4)                         = cmplx(temp(4), 0.0D0, dp)
            return

        else if (abs(x1) > x2) then
            v1                              = sqrt(abs(x1))
            v2                              = 0.0D0
            x                               = -u - b
            y                               = v1 - v2
            root(1)                         = cmplx(x, y, dp)
            root(2)                         = cmplx(x,-y, dp)
            x                               =  u - b
            y                               = v1 + v2
            root(3)                         = cmplx(x, y, dp)
            root(4)                         = cmplx(x,-y, dp)
            return
        end if

    else

        t                                   = dble(root(1))
        x                                   = 0.0D0
        if (t > 0.0D0) then
            x                               = sqrt(t)
            if (q > 0.0D0) &
                x                           = -x
            w                               = sqrt(root(2))
            u                               = 2.0D0*dble(w)
            v                               = 2.0D0*abs(aimag(w))
            t                               =  x - b
            x1                              = t + u
            x2                              = t - u
            if (abs(x1) > abs(x2)) then
                t                           = x1 
                x1                          = x2 
                x2                          = t
            end if
            u                               = -x - b
            h                               = u*u + v*v
            if (x1*x1 < 0.01D0*min(x2*x2,h)) &
                x1                          = e/(x2*h)
            root(1)                         = cmplx(x1, 0.0D0, dp)
            root(2)                         = cmplx(x2, 0.0D0, dp)
            root(3)                         = cmplx(u, v, dp)
            root(4)                         = cmplx(u,-v, dp)

        else if (t < 0.0D0) then
            h                               = abs(dble(root(2))) + abs(aimag(root(2)))
            if (abs(t) > h) then
                w                           = sqrt(root(2))
                u                           = 2.0D0*dble(w)
                v                           = 2.0D0*abs(aimag(w))
                t                           =  x - b
                x1                          = t + u
                x2                          = t - u
                if (abs(x1) > abs(x2)) then
                    t                       = x1 
                    x1                      = x2 
                    x2                      = t
                end if
                u                           = -x - b
                h                           = u*u + v*v
                if (x1*x1 < 0.01D0*min(x2*x2,h)) &
                    x1                      = e/(x2*h)
                root(1)                     = cmplx(x1, 0.0D0, dp)
                root(2)                     = cmplx(x2, 0.0D0, dp)
                root(3)                     = cmplx(u, v, dp)
                root(4)                     = cmplx(u,-v, dp)
            else
                v                           = sqrt(abs(t))
                root(1)                     = cmplx(-b, v, dp)
                root(2)                     = cmplx(-b,-v, dp)
                root(3)                     = root(1)
                root(4)                     = root(2)
            end if

        else if (t == 0.0D0) then
            w                               = sqrt(root(2))
            u                               = 2.0D0*dble(w)
            v                               = 2.0D0*abs(aimag(w))
            t                               =  x - b
            x1                              = t + u
            x2                              = t - u
            if (abs(x1) > abs(x2)) then
                t                           = x1 
                x1                          = x2 
                x2                          = t
            end if
            u                               = -x - b
            h                               = u*u + v*v
            if (x1*x1 < 0.01D0*min(x2*x2,h)) &
                x1                          = e/(x2*h)
            root(1)                         = cmplx(x1, 0.0D0, dp)
            root(2)                         = cmplx(x2, 0.0D0, dp)
            root(3)                         = cmplx(u, v, dp)
            root(4)                         = cmplx(u,-v, dp)
        end if

    end  if


end subroutine quartic_roots
!------------------------------------------------------------------------------------------------------






!
! Input parameters: np  - number of points along chord
!
! Subroutine for a uniform clustering of u before starting blade generation
!
!------------------------------------------------------------------------------------------------------
subroutine uniform_clustering(np,u)
    use file_operations
    implicit none

    integer,                    intent(in)          :: np
    real(kind = 8),             intent(inout)       :: u(*)

    ! Local variables
    integer                                         :: i, nopen
    character(len = :), allocatable                 :: log_file
    logical                                         :: file_open, isquiet


    ! Determine isquiet status
    call get_quiet_status(isquiet)

    ! Print output to screen and write to log file
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) then
        print *, 'Using uniform clustering distribution'
        print *, ''
    end if
    write(nopen,*) 'Using uniform clustering distribution'
    write(nopen,*) ''
    call close_log_file(nopen, file_open)


    ! Uniform clustering of u from u = 0.0 to u = 1.0
    do i = 1,np
        u(i)        = real(i - 1,8)/real(np - 1,8)
    end do


end subroutine uniform_clustering
!------------------------------------------------------------------------------------------------------






!
! Input parameters: np  - number of points along chord
!
! Subroutine for a sine function based clustering of u before starting blade generation
!
!------------------------------------------------------------------------------------------------------
subroutine sine_clustering(np,u,clustering_parameter)
    use file_operations
    implicit none

    integer,                    intent(in)          :: np 
    real(kind = 8),             intent(inout)       :: u(*)
    real(kind = 8),             intent(inout)       :: clustering_parameter

    ! Local variables
    integer                                         :: i, nopen
    real(kind = 8)                                  :: ui, du, pi
    character(len = :),     allocatable             :: log_file
    logical                                         :: file_open, isquiet


    ! Determine isquiet status
    call get_quiet_status(isquiet)

    ! Print output to screen and write to log file
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) then
        print *, 'Using sine function based clustering distribution'
        print *, ''
    end if
    write(nopen,*) 'Using sine function based clustering distribution'
    write(nopen,*) ''
    call close_log_file(nopen, file_open)


    ! First element of u is set to zero
    u(1)            = 0.0
    pi              = real(4.0*atan(1.0),8)
    
    ! Cluster u    
    do i = 2,np
        
        ui          = real(i - 1)/real(np)
        du          = (sin(pi*ui))**clustering_parameter
        u(i)        = u(i - 1) + du

    end do


end subroutine sine_clustering
!------------------------------------------------------------------------------------------------------






!
! Input parameters: np  - number of points along chord
!
! Subroutine for exponential clustering of u before starting blade generation 
!
!------------------------------------------------------------------------------------------------------
subroutine exponential_clustering(np,u,clustering_parameter)
    use file_operations
    implicit none

    integer,                    intent(in)          :: np
    real(kind = 8),             intent(inout)       :: u(*)
    real(kind = 8),             intent(inout)       :: clustering_parameter

    ! Local variables
    integer                                         :: np1, i, j, nopen
    real(kind = 8), allocatable                     :: xi(:), u_temp(:)
    character(len = :), allocatable                 :: log_file
    logical                                         :: file_open, isquiet


    ! Determine isquiet status
    call get_quiet_status(isquiet)

    ! Print output to screen and write to log file
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) then
        print *, 'Using exponential function based clustering distribution'
        print *, ''
    end if
    write(nopen,*) 'Using exponential function based clustering distribution'
    write(nopen,*) ''
    call close_log_file(nopen, file_open)


    ! np1 is the size of the stretched arrays from 0.0 to 0.5 and 0.5 to 1.0
    np1                 = (np + 1)/2

    !
    ! Generate stretching coordinate xi with a uniform distribution from
    ! xi = 0.0 to xi = 1.0
    !
    if (allocated(xi)) deallocate(xi)
    allocate(xi(np1))

    do i = 1,np1
        xi(i)           = real(i - 1,8)/real(np1 - 1,8)
    end do

    !
    ! Generate temporary cluster from u = 0.0 to u = 0.5
    ! This cluster will be reversed and used to generate cluster from u = 0.5 to u = 1.0
    !
    if (allocated(u_temp)) deallocate(u_temp)
    allocate(u_temp(np1))

    do i = 1,np1
        u_temp(i)       = (exp(clustering_parameter*xi(i)) - 1.0)/(exp(clustering_parameter) - 1.0)
        u_temp(i)       = 0.5*u_temp(i)
    end do

    !
    ! Populate u from u = 0.0 to u = 1.0
    !
    do i = 1,np1
        u(i)            = u_temp(i)
    end do
    do i = 1,np1 - 1
        j               = np1 + i
        u(j)            = 1.0 - u_temp(np1 - i)
    end do


end subroutine exponential_clustering
!------------------------------------------------------------------------------------------------------






!
! Input parameters: np  - number of points along chord
!
! Subroutine for hyperbolic tangent clustering of u before starting blade generation
!
!------------------------------------------------------------------------------------------------------
subroutine hyperbolic_tan_clustering(np,u,clustering_parameter)
    use file_operations
    implicit none

    integer,                    intent(in)          :: np
    real(kind = 8),             intent(inout)       :: u(*)
    real(kind = 8),             intent(inout)       :: clustering_parameter

    ! Local variables
    integer                                         :: np1, i, j, nopen
    real(kind = 8), allocatable                     :: xi(:), u_temp(:), temp(:)
    character(len = :), allocatable                 :: log_file
    logical                                         :: file_open, isquiet


    ! Determine isquiet status
    call get_quiet_status(isquiet)

    ! Print output to screen and write to log file
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) then
        print *, 'Using hyperbolic tangent function based clustering'
        print *, ''
    end if
    write(nopen,*) 'Using hyperbolic tangent function based clustering'
    write(nopen,*) ''
    call close_log_file(nopen, file_open)


    ! np1 is the size of the stretched arrays from 0.0 to 0.5 and 0.5 to 1.0
    np1                 = (np + 1)/2

    !
    ! Generate stretching coordinate xi with a uniform distribution from
    ! xi = 0.0 to xi = 1.0
    !
    if (allocated(xi)) deallocate(xi)
    allocate(xi(np1))

    do i = 1,np1
        xi(i)           = real(i - 1,8)/real(np1 - 1,8)
    end do

    !
    ! Generate temporary cluster from u = 0.0 to u = 0.5
    ! This cluster will be reversed and used to generate cluster from u = 0.5 to u = 1.0
    !
    if (allocated(u_temp)) deallocate(u_temp)
    allocate(u_temp(np1))
    if (allocated(temp)) deallocate(temp)
    allocate(temp(np1))

    do i = 1,np1

        temp(i)         = 1.0 + (tanh(0.5*clustering_parameter*xi(i))/tanh(0.5*clustering_parameter))
        temp(i)         = 0.5*temp(i)

    end do

    ! Generate cluster from u = 0.0 to u = 0.5
    do i = 1,np1
        u_temp(i)       = 1.0 - temp(np1 + 1 - i)
    end do

    !
    ! Populate u from u = 0.0 to u = 1.0
    !
    do i = 1,np1
        u(i)            = u_temp(i)
    end do
    do i = 1,np1 - 1
        j               = np1 + i
        u(j)            = 1.0 - u_temp(np1 - i)
    end do


end subroutine hyperbolic_tan_clustering
!------------------------------------------------------------------------------------------------------






!
! Generate LE ellipse
!
! Input parameters: np          - number of points along the chord
!                   cp_LE_ellip - control points for generating LE clustering ellipse
!                   np_cluster  - number of points for clustering LE
!
!------------------------------------------------------------------------------------------------------
subroutine LE_ellipse(np,cp_LE_ellip,np_cluster,x_ellip_LE,y_ellip_LE)
    implicit none

    integer,                    intent(in)          :: np
    real,                       intent(in)          :: cp_LE_ellip(4,2)
    integer,                    intent(in)          :: np_cluster
    real,                       intent(inout)       :: x_ellip_LE(*), y_ellip_LE(*)
    
    ! Local variables
    real                                            :: x_1_LE,y_1_LE,x_2_LE,y_2_LE,x_3_LE,  &
                                                       y_3_LE,x_center_LE,y_center_LE,a_LE, &
                                                       b_LE
    real,       allocatable                         :: t_LE(:)
    real,       parameter                           :: pi = 4.0*atan(1.0)
    integer                                         :: i


    !
    ! Define ellipse end points and center for the LE
    !
    ! 1 - top endpoint of the minor axis of the LE ellipse (y = +b)
    ! 2 - bottom endpoint of the minor axis of the LE ellipse (y = -b)
    ! 3 - left endpoint of the major axis of the LE ellipse (x = -a)
    ! center - center of the LE ellipse
    !
    x_1_LE                      = cp_LE_ellip(1,1)!xcp_thk(1)
    y_1_LE                      = cp_LE_ellip(1,2)!ycp_thk(1)
    x_2_LE                      = cp_LE_ellip(2,1)!xcp_thk(1)
    y_2_LE                      = cp_LE_ellip(2,2)!-ycp_thk(1)
    x_3_LE                      = cp_LE_ellip(3,1)!0.0
    y_3_LE                      = cp_LE_ellip(3,2)!0.0
    x_center_LE                 = cp_LE_ellip(4,1)!xcp_thk(1)
    y_center_LE                 = cp_LE_ellip(4,2)!0.0


    ! Define major and minor axis of the LE ellipse
    a_LE                        = sqrt((x_center_LE - x_3_LE)**2 + (y_center_LE - y_3_LE)**2)
    b_LE                        = sqrt((x_center_LE - x_1_LE)**2 + (y_center_LE - y_1_LE)**2)


    !
    ! Define parameter space for the LE ellipse
    !
    if (allocated(t_LE)) deallocate(t_LE)
    allocate(t_LE(2*np_cluster - 1))
    do i = 1,size(t_LE)

        t_LE(i)                 = (pi*(real(i - 1)/real(size(t_LE) - 1))) + (0.5*pi)

    end do

    ! Generate LE ellipse
    do i = 1,size(t_LE)

        x_ellip_LE(i)           = a_LE*(1.0 + cos(t_LE(i)))
        y_ellip_LE(i)           = b_LE*sin(t_LE(i))

    end do


    !
    ! Ensure matching of ellipse endpoints with generated ellipse
    ! Floating point operations cause slight difference in endpoints
    !
    x_ellip_LE(1)               = x_1_LE;       y_ellip_LE(1)               = y_1_LE
    x_ellip_LE(np_cluster)      = x_3_LE;       y_ellip_LE(np_cluster)      = y_3_LE
    x_ellip_LE(size(t_LE))      = x_2_LE;       y_ellip_LE(size(t_LE))      = y_2_LE


end subroutine LE_ellipse
!------------------------------------------------------------------------------------------------------






!
! Generate TE ellipse
!
! Input parameters: np          - number of points along the chord
!                   cp_TE_ellip - control points for generating TE clustering ellipse
!                   np_cluster  - number of points for clustering TE
!
!------------------------------------------------------------------------------------------------------
subroutine TE_ellipse(np,cp_TE_ellip,np_cluster,x_ellip_TE,y_ellip_TE)
    implicit none

    integer,                    intent(in)          :: np
    real,                       intent(in)          :: cp_TE_ellip(4,2)
    integer,                    intent(in)          :: np_cluster
    real,                       intent(inout)       :: x_ellip_TE(*), y_ellip_TE(*)

    ! Local variables
    real                                            :: x_1_TE,y_1_TE,x_2_TE,y_2_TE,x_3_TE,  &
                                                       y_3_TE,x_center_TE,y_center_TE,a_TE, &
                                                       b_TE
    real,       allocatable                         :: t_TE(:)
    real,       parameter                           :: pi = 4.0*atan(1.0)
    integer                                         :: i


    ! 
    ! Define ellipse endpoints and center for the TE ellipse
    !
    ! 1 - top endpoint of the minor axis of the TE ellipse (y = +b)
    ! 2 - bottom endpoint of the minor axis of the TE ellipse (y = -b)
    ! 3 - rght endpoint of the major axis of the TE ellipse (x = +a)
    ! center - center of the TE ellipse
    !
    x_1_TE                      = cp_TE_ellip(1,1)!xcp_thk(ncp)
    y_1_TE                      = cp_TE_ellip(1,2)!ycp_thk(ncp)
    x_2_TE                      = cp_TE_ellip(2,1)!xcp_thk(ncp)
    y_2_TE                      = cp_TE_ellip(2,2)!-ycp_thk(ncp)
    x_3_TE                      = cp_TE_ellip(3,1)!1.0
    y_3_TE                      = cp_TE_ellip(3,2)!0.0
    x_center_TE                 = cp_TE_ellip(4,1)!xcp_thk(ncp)
    y_center_TE                 = cp_TE_ellip(4,2)!0.0


    ! Define major and minor axis of the TE ellipse
    a_TE                        = sqrt((x_center_TE - x_3_TE)**2 + (y_center_TE - y_3_TE)**2)
    b_TE                        = sqrt((x_center_TE - x_1_TE)**2 + (y_center_TE - y_1_TE)**2)


    !
    ! Define parameter space for the TE ellipse
    !
    if (allocated(t_TE)) deallocate(t_TE)
    allocate(t_TE(2*np_cluster - 1))
    do i = 1,size(t_TE)

        t_TE(i)                 = (pi*(real(size(t_TE) - 1 - i + 1)/real(size(t_TE) - 1))) - 0.5*pi

    end do


    ! Generate TE ellipse
    do i = 1,size(t_TE)

        x_ellip_TE(i)           = x_center_TE + (a_TE*cos(t_TE(i)))
        y_ellip_TE(i)           = b_TE*sin(t_TE(i))

    end do


    !
    ! Ensure matching of ellipse endpoints with the generated ellipse
    ! Floating point operations cause slight differences in endpoints
    !
    x_ellip_TE(1)               = x_1_TE;   y_ellip_TE(1)               = y_1_TE
    x_ellip_TE(np_cluster)      = x_3_TE;   y_ellip_TE(np_cluster)      = y_3_TE
    x_ellip_TE(size(t_TE))      = x_2_TE;   y_ellip_TE(size(t_TE))      = y_2_TE


end subroutine TE_ellipse
!------------------------------------------------------------------------------------------------------






!
! Cluster the part of the blade between the LE and the TE
!
! Input parameters: u_begin - starting location of the middle part of the blade section
!                   u_end   - ending location of the middle part of the blade section
!                   np_mid  - number of points in the middle part of the blade section
!
!------------------------------------------------------------------------------------------------------
subroutine cluster_mid(u_begin,u_end,np_mid,u_mid)
    implicit none

    real,                       intent(in)          :: u_begin, u_end
    integer,                    intent(in)          :: np_mid
    real,                       intent(inout)       :: u_mid(np_mid)

    ! Local variables
    integer                                         :: i
    real                                            :: delta_u


    delta_u         = abs(u_end - u_begin)

    do i =  1,np_mid

        u_mid(i)    = u_begin + (delta_u*(real(i - 1)/real(np_mid - 1)))

    end do

    u_mid(1)        = u_begin
    u_mid(np_mid)   = u_end


end subroutine cluster_mid
!------------------------------------------------------------------------------------------------------






!
! Define hyperbolic clustering function for LE side mid clustering
! This function is used in the bisection method when solving for the LE side
! clustering parameter
!
! Input parameters: K               - equation constant for the LE clustering parameter equation
!                   xi              - reference coordinate system location
!                   func_coordinate - bisection solver guess
!
!------------------------------------------------------------------------------------------------------
real function LE_clustering_parameter_func(K,xi,func_coordinate) result(func)
    implicit none

    real,                       intent(in)          :: K
    real,                       intent(in)          :: xi
    real,                       intent(in)          :: func_coordinate

    
    func            = K + ((tanh(0.5*func_coordinate*(xi - 1.0)))/(tanh(0.5*func_coordinate)))


end function LE_clustering_parameter_func
!------------------------------------------------------------------------------------------------------






!
! Define hyperbolic clustering function for TE side mid clustering
! This function is used in the bisection method when solving for the TE side
! clustering parameter
!
! Input parameters: K               - equation constant for the TE clustering parameter equation
!                   xi              - reference coordinate system location
!                   func_coordinate - bisection solver guess
!
!------------------------------------------------------------------------------------------------------
real function TE_clustering_parameter_func(K,xi,func_coordinate) result(func)
    implicit none

    real,                       intent(in)          :: K
    real,                       intent(in)          :: xi
    real,                       intent(in)          :: func_coordinate


    func            = K - ((tanh(0.5*func_coordinate*xi))/(tanh(0.5*func_coordinate)))


end function TE_clustering_parameter_func
!------------------------------------------------------------------------------------------------------






!
! Bisection solver for the LE side clustering parameter
!
! Input parameters: xi  - reference coordinate system location
!                   K   - equation constant for the LE clustering parameter equation
!
!------------------------------------------------------------------------------------------------------
subroutine LE_clustering_parameter_solver(xi,K,delta,solver_flag)
    use file_operations
    use errors
    implicit none

    real,                       intent(in)          :: xi
    real,                       intent(in)          :: K
    real,                       intent(inout)       :: delta
    logical,                    intent(inout)       :: solver_flag

    ! Local variables
    real                                            :: a, b, c, f1, f2, f3, &
                                                       tol = 10E-6
    integer                                         :: nopen, niter, js
    character(:),   allocatable                     :: log_file, warning_msg, warning_msg_1, &
                                                       dev_msg
    character(10)                                   :: warning_arg
    logical                                         :: file_open
    interface LE_clustering_parameter_func
        real function LE_clustering_parameter_func(Kf,xif,func_coordinate) 
            real,               intent(in)          :: Kf
            real,               intent(in)          :: xif
            real,               intent(in)          :: func_coordinate
        end function LE_clustering_parameter_func
    end interface


    ! Get section number
    call get_sec_number(js)

    
    ! Define initial bisection interval "[a,b]" and interval midpoint "c"
    ! Compute function values at a, b and c
    a               = 0.05
    b               = 10.0
    c               = 0.5*(a + b)
    f1              = LE_clustering_parameter_func(K,xi,a)
    f2              = LE_clustering_parameter_func(K,xi,b)
    f3              = LE_clustering_parameter_func(K,xi,c)
   
    call log_file_exists(log_file, nopen, file_open)

    ! Bisection interval should contain a zero
    if ((f1 < 0 .and. f2 > 0) .or. (f1 > 0 .and. f2 < 0)) then

        ! Iteration counter
        niter       = 0    
        do while (niter .le. 40)
        
            ! Determine whether sign(a) = sign(c) or
            !                   sign(b) = sign(c) and
            ! reinterpret bisection interval
            if ((f1 < 0 .and. f3 < 0) .or. (f1 > 0 .and. f3 > 0)) then
                a   = c
            else if ((f2 < 0 .and. f3 < 0) .or. (f2 > 0 .and. f3 > 0)) then
                b   = c
            end if
      
            ! Compute new midpoint c and new function values 
            c       = 0.5*(a + b)
            f1      = LE_clustering_parameter_func(K,xi,a)
            f2      = LE_clustering_parameter_func(K,xi,b)
            f3      = LE_clustering_parameter_func(K,xi,c)

            ! Update iteration counter
            niter   = niter + 1

            ! Exit condition
            if (abs(f3) < tol) exit
        
        end do

    else
        write(warning_arg, '(I2)') js
        warning_msg     = "Could not find initial guesses for the LE clustering_parameter bisection solver for &
                          &section "//trim(adjustl(warning_arg))
        warning_msg_1   = "Returning to uniform midchord clustering"
        dev_msg         = 'Check subroutine LE_clustering_parameter_solver in funcNsubs.f90'
        call warning(warning_msg, warning_msg_1, dev_msg)
        delta       = 0.0
        solver_flag = .false.
    end if
    call close_log_file(nopen, file_open)

    ! Set clustering_parameter
    delta       = c
    solver_flag = .true.


end subroutine LE_clustering_parameter_solver
!------------------------------------------------------------------------------------------------------






!
! Bisection solver for TE side clustering parameter
!
! Input parameters: xi  - reference coordinate system location
!                   K   - equation constant for the TE clustering parameter equation
!
!------------------------------------------------------------------------------------------------------
subroutine TE_clustering_parameter_solver(xi,K,delta,solver_flag)
    use file_operations
    use errors
    implicit none

    real,                       intent(in)          :: xi
    real,                       intent(in)          :: K
    real,                       intent(inout)       :: delta
    logical,                    intent(inout)       :: solver_flag

    ! Local variables
    real                                            :: a, b, c, f1, f2, f3, &
                                                       tol = 10E-6
    integer                                         :: nopen, niter, js
    character(:),   allocatable                     :: log_file, warning_msg, warning_msg_1, &
                                                       dev_msg
    character(10)                                   :: warning_arg
    logical                                         :: file_open
    interface TE_clustering_parameter_func
        real function TE_clustering_parameter_func(Kf,xif,func_coordinate)
            real,               intent(in)          :: Kf
            real,               intent(in)          :: xif
            real,               intent(in)          :: func_coordinate
        end function TE_clustering_parameter_func
    end interface
   

    ! Get current section number 
    call get_sec_number(js)

    
    ! Define initial bisection interval "[a,b]" and interval midpoint "c"
    ! Compute function values at a, b and c
    a               = 0.05
    b               = 10.0
    c               = 0.5*(a + b)
    f1              = TE_clustering_parameter_func(K,xi,a)
    f2              = TE_clustering_parameter_func(K,xi,b)
    f3              = TE_clustering_parameter_func(K,xi,c)

    call log_file_exists(log_file, nopen, file_open)
    
    ! Bisection interval should contain a zero    
    if ((f1 < 0. .and. f2 > 0.) .or. (f1 > 0. .and. f2 < 0.)) then

        ! Iteration counter
        niter       = 0
        do while (niter .le. 40) 

            ! Determine whether sign(a) = sign(c) or
            !                   sign(b) = sign(c) and
            ! reinterpret bisection interval
            if ((f1 < 0 .and. f3 < 0) .or. (f1 > 0 .and. f3 > 0)) then
                a   = c
            else if ((f2 < 0 .and. f3 < 0) .or. (f2 > 0 .and. f3 > 0)) then
                b   = c
            end if

            ! Compute new midpoint c and new function values
            c       = 0.5*(a + b)
            f1      = TE_clustering_parameter_func(K,xi,a)
            f2      = TE_clustering_parameter_func(K,xi,b)
            f3      = TE_clustering_parameter_func(K,xi,c)

            ! Update iteration counter
            niter   = niter + 1
            
            ! Exit condition
            if (abs(f3) < tol) exit
                
        end do 

    else
        write(warning_arg,'(I2)') js
        warning_msg     = 'Could not find initial guesses for the TE clustering_parameter bisection solver for &
                          &section '//trim(adjustl(warning_arg))
        warning_msg_1   = 'Returning to uniform midchord clustering'
        dev_msg         = 'Check subroutine TE_clustering_parameter_solver in funcNsubs.f90'
        call warning(warning_msg, warning_msg_1, dev_msg)
        delta       = 0.0
        solver_flag = .false.
    end if
    call close_log_file(nopen, file_open)

    ! Set clustering parameter
    delta           = c
    solver_flag     = .true.


end subroutine TE_clustering_parameter_solver
!------------------------------------------------------------------------------------------------------






!
! Add hyperbolic clustering for LE side middle part
!
! Input parameters: np_cluster  - number of clustered points along LE and TE
!                   np_mid      - number of points in the middle part of the blade section
!                   u_LE        - clustered points along LE
!                   u_TE        - clustered points along TE
!
!------------------------------------------------------------------------------------------------------
subroutine mid_hyperbolic_clustering(np_cluster,np_mid,u_LE,u_TE,u_mid)
    use file_operations
    implicit none

    integer,                    intent(in)          :: np_cluster
    integer,                    intent(in)          :: np_mid
    real,                       intent(in)          :: u_LE(np_cluster)
    real,                       intent(in)          :: u_TE(np_cluster)
    real,                       intent(inout)       :: u_mid(np_mid)

    ! Local variables
    integer                                         :: i, np_mid_LE, np_mid_TE, nopen
    real,   allocatable                             :: xi(:), u_mid_LE(:), u_mid_TE(:)
    real                                            :: du_LE, u_mid_pt, du_mid_LE, du_mid_TE, K_LE, delta_LE, &
                                                       du_TE, K_TE, delta_TE
    character(:),   allocatable                     :: log_file
    logical                                         :: solver_flag_LE, solver_flag_TE, file_open, &
                                                       isquiet


    ! Determine isquiet status
    call get_quiet_status(isquiet)

    ! Calculate array sizes
    np_mid_LE   = (np_mid + 1)/2
    np_mid_TE   = np_mid_LE


    ! du_LE     - distance between the last two LE points
    ! du_mid_LE - distance between blade mid point and LE
    du_LE       = abs(u_LE(np_cluster) - u_LE(np_cluster - 1))
    du_TE       = abs(u_TE(2) - u_TE(1))
    u_mid_pt    = 0.5*(u_LE(np_cluster) + u_TE(1))
    du_mid_LE   = abs(u_mid_pt - u_LE(np_cluster))
    du_mid_TE   = abs(u_TE(1) - u_mid_pt)


    ! Compute the uniform reference space xi
    ! The second xi value is used for the clustering parameter equation
    ! Equation defined in LE_clustering_parameter_func
    if (allocated(xi)) deallocate(xi)
    allocate(xi(np_mid_LE))
    do i = 1,np_mid_LE
        xi(i)   = real(i - 1,8)/real(np_mid_LE - 1,8)
    end do


    ! Compute the equation constant for the clustering parameter equation
    ! Equation defined in LE_clustering_parameter_func
    K_LE        = 1.0 - (du_LE/du_mid_LE)
    K_TE        = 1.0 - (du_TE/du_mid_TE)


    ! Solve the clustering parameter equation
    ! TODO: Add Newton's solver
    call LE_clustering_parameter_solver(xi(2),K_LE,delta_LE,solver_flag_LE)
    call TE_clustering_parameter_solver(xi(np_mid_LE - 1),K_TE,delta_TE,solver_flag_TE)


    if (solver_flag_LE .and. solver_flag_TE) then

        call log_file_exists(log_file, nopen, file_open)
        if (.not. isquiet) then
            print *, 'Hyperbolic midchord clustering with delta_LE = ', delta_LE
            print *, 'Hyperbolic midchord clustering with delta_TE = ', delta_TE
        end if
        write(nopen,*) 'Hyperbolic midchord clustering with delta_TE = ', delta_LE 
        write(nopen,*) 'Hyperbolic midchord clustering with delta_TE = ', delta_TE
        call close_log_file(nopen, file_open)
               
        ! Cluster u_LE_mid
        if (allocated(u_mid_LE)) deallocate(u_mid_LE)
        allocate(u_mid_LE(np_mid_LE))
        u_mid_LE(1) = u_LE(np_cluster)
        do i = 2,np_mid_LE

            u_mid_LE(i) = u_mid_LE(1) + (du_mid_LE*(1.0 + ((tanh(0.5*delta_LE*(xi(i) - 1.0)))/(tanh(0.5*delta_LE)))))

        end do
        u_mid_LE(np_mid_LE) = u_mid_pt


        ! Cluster u_TE_mid
        if (allocated(u_mid_TE)) deallocate(u_mid_TE)
        allocate(u_mid_TE(np_mid_TE))
        u_mid_TE(1) = u_mid_pt
        do i = 2,np_mid_TE

            u_mid_TE(i) = u_mid_TE(1) + (du_mid_TE*((tanh(0.5*delta_TE*xi(i)))/(tanh(0.5*delta_TE))))

        end do
        u_mid_TE(np_mid_TE) = u_TE(1)


        ! Generate u_mid
        do i = 1,np_mid_LE

            u_mid(i)    = u_mid_LE(i)

        end do
        do i = 2,np_mid_TE

            u_mid(np_mid_LE + i - 1)    = u_mid_TE(i)

        end do

    else
        
        ! If clustering parameter for midchord hyperbolic clustering is not found
        ! use uniform clustering
        call cluster_mid(u_LE(np_cluster),u_TE(1),np_mid,u_mid)

    end if


end subroutine mid_hyperbolic_clustering
!------------------------------------------------------------------------------------------------------






!
! Add elliptical clustering for the LE and TE
!
! Input parameters: np          - number of points along the chord
!                   np_cluster  - number of points along the clustered LE and TE
!                   cp_LE_ellip - control points for the LE clustering ellipse
!                   cp_TE_ellip - control points for the TE clustering ellipse
!
!------------------------------------------------------------------------------------------------------
subroutine elliptical_clustering(np,np_cluster,cp_LE_ellip,cp_TE_ellip,u_new)
    use file_operations
    implicit none

    integer,                    intent(in)          :: np
    integer,                    intent(in)          :: np_cluster
    real,                       intent(in)          :: cp_LE_ellip(4,2), cp_TE_ellip(4,2)
    real,                       intent(inout)       :: u_new(np)

    ! Local variables
    integer                                         :: i, j, np_mid, nopen
    real,           allocatable                     :: x_ellip_LE(:), y_ellip_LE(:), &
                                                       x_ellip_TE(:), y_ellip_TE(:), &
                                                       u_LE(:), u_TE(:), u_mid(:)
    character(:),   allocatable                     :: log_file
    logical                                         :: file_open, isquiet


    !Determine isquiet status
    call get_quiet_status(isquiet)

    ! Print to screen and write to log file
    if (.not. isquiet) then
        print *, 'Using ellipse based clustering function'
        print *, ''
    end if
    call log_file_exists(log_file, nopen, file_open)
    write(nopen,*) 'Using ellipse based clustering function'
    write(nopen,*) ''
    call close_log_file(nopen, file_open)


    ! Generate LE and TE ellipses
    if (allocated(x_ellip_LE) .and. allocated(y_ellip_LE)) deallocate(x_ellip_LE,y_ellip_LE)
    allocate(x_ellip_LE(2*np_cluster - 1), y_ellip_LE(2*np_cluster - 1))
    call LE_ellipse(np,cp_LE_ellip,np_cluster,x_ellip_LE,y_ellip_LE)

    if (allocated(x_ellip_TE) .and. allocated(y_ellip_TE)) deallocate(x_ellip_TE,y_ellip_TE)
    allocate(x_ellip_TE(2*np_cluster - 1), y_ellip_TE(2*np_cluster - 1))
    call TE_ellipse(np,cp_TE_ellip,np_cluster,x_ellip_TE,y_ellip_TE)


    ! Store u values for LE and TE
    if (allocated(u_LE) .and. allocated(u_TE)) deallocate(u_LE,u_TE)
    allocate(u_LE(np_cluster), u_TE(np_cluster))
    do i = 1,np_cluster

        u_LE(i)     = x_ellip_LE(np_cluster - i + 1)
        u_TE(i)     = x_ellip_TE(i)

    end do
    

    ! Cluster the middle part of the blade section using
    ! Use hyperbolic clustering
    np_mid  = np - (2*np_cluster) + 2
    if (allocated(u_mid)) deallocate(u_mid)
    allocate(u_mid(np_mid))
    call mid_hyperbolic_clustering(np_cluster,np_mid,u_LE,u_TE,u_mid)

    ! Initializing 'j'
    j   = 0

    ! Generate u_new by combining u_LE, u_mid and u_TE
    do i = 1,np_cluster

        u_new(i)                 = u_LE(i)

    end do
    do i = 1,np_mid - 2

        u_new(np_cluster + i)    = u_mid(i + 1)
        j                        = np_cluster + i

    end do
    do i = 1,np_cluster

        u_new(j + i)             = u_TE(i)

    end do


end subroutine elliptical_clustering
!------------------------------------------------------------------------------------------------------






!
! Interpolate trailing edge angle
!
! Input parameters: u_max - chordwise location of max. thickness for the blade section
!
! Reference: Abbott, I.H., van Doenhoff, A.E., "Families of Wing Sections", Theory of Wing
!            Sections, Dover Publications, New York, 1999, pp. 116-118
!
!------------------------------------------------------------------------------------------------------
subroutine compute_TE_angle(u_max,trail_angle)
    use file_operations
    implicit none

    real,                       intent(in)      :: u_max
    real,                       intent(inout)   :: trail_angle


    trail_angle = 0.775 + (2.51666667*u_max) + (-13.625*(u_max**2)) + (35.83333333*(u_max**3)) &
                  + (-12.5*(u_max**4))


end subroutine compute_TE_angle
!------------------------------------------------------------------------------------------------------






!
! Compute coefficients of modified NACA four-digit thickness
!
! For u < u_max: y_t = a_0*sqrt(u) + a_1*u + a_2*(u**2) + a_3*(u**3)
! For u > u_max: y_t = d_0 + d_1*(1 - u) + d_2*((1 - u)**2) + d_3*((1 - u)**3)
!
! Input parameters: t_max    - half max. thickness for the blade section in fraction chord
!                   u_max    - chordwise location of max. thickness for the blade section
!                   TE_thk   - thickness at the TE location
!                   dy_dx_TE - trailing edge angle
!                   I        - integer parameter governing roundedness of the leading edge
!                              (default = 6, sharp LE = 0)
!
! Reference: Abbott, I.H, von Doenhoff, A.E., "Families of Wing Sections", Theory of Wing
!            Sections, Dover Publications, New York, 1999, pp. 116-118
!
!------------------------------------------------------------------------------------------------------
subroutine modified_NACA_four_digit_thickness_coeffs(t_max,u_max,TE_thk,dy_dx_TE,I,a,d)
    use file_operations
    implicit none

    real,                       intent(in)      :: t_max
    real,                       intent(in)      :: u_max
    real,                       intent(in)      :: TE_thk
    real,                       intent(in)      :: dy_dx_TE
    real,                       intent(in)      :: I
    real,                       intent(inout)   :: a(4)
    real,                       intent(inout)   :: d(4)

    ! Local variables
    integer                                     :: fail_flag
    real                                        :: temp
    real,           allocatable                 :: aug_matrix(:,:)


    !
    ! Compute d_0 and d_1
    !
    d(1)            = TE_thk!0.02*t_max
    d(2)            = 2.0*dy_dx_TE*t_max
    
    !
    ! Compute d_2 and d_3
    ! Enforce the following conditions: yd_t(u_max)     = t_max
    !                                   yd'_t(u_max)    = 0
    ! Solve the linear system:          A_d_coeffs*d_23 = b_d_coeffs 
    ! with                              d_23            = [d_2, d_3]
    !  
    if (allocated(aug_matrix)) deallocate(aug_matrix)
    allocate(aug_matrix(2,3))

    aug_matrix(1,:) = [(1.0 - u_max)**2 , (1.0 - u_max)**3       , t_max - d(1) - (d(2)*(1.0 - u_max))]
    aug_matrix(2,:) = [2.0*(u_max - 1.0), -3.0*((u_max - 1.0)**2), d(2)                               ]

    call gauss_jordan(2,1,aug_matrix,fail_flag)

    ! Compute d_2 and d_3
    d(3:)           = aug_matrix(:,3)
    

    !
    ! Compute a_0
    !
    a(1)            = sqrt(2.2038)*(2.0*t_max*(real(I,8)/6.0))

    !
    ! Compute a_1, a_2 and a_3
    ! Enforce the following conditions: ya_t(u_max)         = t_max
    !                                   ya'_t(u_max)        = 0
    !                                   ya''_t(u_max)       = yd''_t(u_max)
    ! Solve the linear system:          A_a_coeffs*a_123    = b_a_coeffs
    ! with                              a_123               = [a_1, a_2, a_3]
    !
    if (allocated(aug_matrix)) deallocate(aug_matrix)
    allocate(aug_matrix(3,4))

    temp            = (2.0*d(3)) + (6.0*(1.0 - u_max)*d(4)) + (0.25*a(1)/(u_max*sqrt(u_max)))
    
    aug_matrix(1,:) = [u_max, u_max**2 , u_max**3      , t_max - (a(1)*sqrt(u_max))]
    aug_matrix(2,:) = [1.0  , 2.0*u_max, 3.0*(u_max**2), (-0.5*a(1)/sqrt(u_max))   ]
    aug_matrix(3,:) = [0.0  , 2.0      , 6.0*u_max     , temp                      ]

    call gauss_jordan(3,1,aug_matrix,fail_flag)

    ! Compute a_1, a_2 and a_3
    a(2:)           = aug_matrix(:,4)


end subroutine modified_NACA_four_digit_thickness_coeffs
!------------------------------------------------------------------------------------------------------






!
! Compute coefficients of modified NACA four-digit thickness
!
! This subroutine solves for the coefficients with an elliptical TE
!
! For u < u_max: y_t = a_0*sqrt(u) + a_1*u + a_2*(u**2) + a_3*(u**3)
! For u > u_max: y_t = d_0*sqrt*(1 - u) + d_1*(1 - u) + d_2*((1 - u)**2) + d_3*((1 - u)**3)
!
! Input parameters: t_max     - half max. thickness for the blade section 
!                   u_max     - chordwise location of max. thickness for the blade section
!                   TE_thk    - half thickness at the trailing edge
!                   u_TE      - chordwise location of trailing edge
!                   dy_dx_TE  - trailiing edge angle
!                   LE_radius - radius of the leading edge
!
! Reference: Abbott, I.H., von Doenhoff, A.E., "Families of Wing Sections", Theory of Wing
!            Sections, Dover Publications, New York, 1999, pp. 116-118
!
!------------------------------------------------------------------------------------------------------
!subroutine modified_NACA_four_digit_thickness_coeffs_2(t_max,u_max,t_TE,u_TE,dy_dx_TE, &
!                                                       LE_radius, a, d)
!    use file_operations
!    implicit none
!
!    real,                   intent(in)      :: t_max
!    real,                   intent(in)      :: u_max
!    real,                   intent(in)      :: t_TE
!    real,                   intent(in)      :: u_TE
!    real,                   intent(in)      :: dy_dx_TE
!    real,                   intent(in)      :: LE_radius
!    real,                   intent(inout)   :: a(4)
!    real,                   intent(inout)   :: d(4)
!
!    ! Local variables
!    integer                                 :: i, j, k, nopen, fail_flag
!    real                                    :: temp
!    real,           allocatable             :: aug_matrix(:,:)
!    character(:),   allocatable             :: log_file
!    logical                                 :: file_open
!
!
!    !
!    ! Compute d_0, d_1, d_2 and d_3
!    ! Enforce the following conditions: yd_t(u_max)     = t_max
!    !                                   yd_t(u_TE)      = t_TE
!    !                                   yd'_t(u_max)    = 0.0
!    !                                   yd'_t(u_TE)     = dy_dx_TE
!    !
!    ! Gauss Jordan method used to solve the resulting linear system
!    !
!    d(1)            = sqrt(2.0*t_TE)
!
!    if (allocated(aug_matrix)) deallocate(aug_matrix)
!    allocate(aug_matrix(3,4))
!    
!    aug_matrix(1,:) = [1.0 - u_max, (1.0 - u_max)**2 , (1.0 - u_max)**3       , t_max - (d(1)*sqrt(1.0 - u_max))]
!    !aug_matrix(2,:) = [1.0 - u_TE , (1.0 - u_TE)**2  , (1.0 - u_TE)**3        , t_TE - (d(1)*sqrt(1.0 - u_TE))]
!    aug_matrix(2,:) = [-1.0       , 2.0*(u_max - 1.0), -3.0*((1.0 - u_max)**2), (0.5*d(1)/sqrt(1.0 - u_max))    ]
!    !aug_matrix(3,:) = [-1.0       , 2.0*(u_TE - 1.0) , -3.0*((1.0 - u_TE)**2) , (-2.0*dy_dx_TE*t_max) + (0.5*d(1)/sqrt(1.0 - u_TE))]
!    aug_matrix(3,:) = [0.0        , 0.0              , 0.0                    , 0.0                             ]
!
!    call gauss_jordan(3,1,aug_matrix,fail_flag)
!    d(2:)           = aug_matrix(:,4)
!
!
!    ! Compute a_0
!    a(1)            = sqrt(2.2038)*(2.0*t_max*LE_radius/6.0)
!
!    !
!    ! Compute a_1, a_2 and a_3
!    ! Enforce the following conditions: ya_t(u_max)     = t_max
!    !                                   ya'_t(u_max)    = 0.0
!    !                                   ya''_t(u_max)   = yd''_t(u_max)
!    !
!    ! Gauss Jordan method used to solve the resulting linear system
!    !
!    if (allocated(aug_matrix)) deallocate(aug_matrix)
!    allocate(aug_matrix(3,4)) 
!    
!    temp            = (-0.25*d(1)/((sqrt(1.0 - u_max))**3)) + (2.0*d(3)) + (6.0*(1.0 - u_max)*d(4)) + &
!                      (0.25*a(1)/(u_max*sqrt(u_max)))
!
!    aug_matrix(1,:) = [u_max, u_max**2 , u_max**3      , t_max - (a(1)*sqrt(u_max))]
!    aug_matrix(2,:) = [1.0  , 2.0*u_max, 3.0*(u_max**2), (-0.5*a(1)/sqrt(u_max))   ]
!    aug_matrix(3,:) = [0.0  , 2.0      , 6.0*u_max     , temp                      ]
!
!    call gauss_jordan(3,1,aug_matrix,fail_flag)
!
!    a(2:)           = aug_matrix(:,4)
!    
!     
!end subroutine modified_NACA_four_digit_thickness_coeffs_2
!------------------------------------------------------------------------------------------------------






!
! Compute coefficients of modified NACA four-digit thickness
!
! For u < u_max: y_t = a_0*sqrt(u) + a_1*u + a_2*(u**2) + a_3*(u**3)
! For u > u_max: y_t = d_0 + d_1*(1 - u) + d_2*((1 - u)**2) + d_3*((1 - u)**3)
!
! Input parameters: t_max    - half max. thickness for the blade section in fraction chord
!                   u_max    - chordwise location of max. thickness for the blade section
!                   t_TE     - half thickness for the blade section at the trailing edge
!                   dy_dx_TE - trailing edige angle
!                   I        - integer parameter governing roundedness of the leading edge
!                              (default = 6, sharp LE = 0)
!
! Reference: Abbott, I.H, von Doenhoff, A.E., "Families of Wing Sections", Theory of Wing
!            Sections, Dover Publications, New York, 1999, pp. 116-118
!
!------------------------------------------------------------------------------------------------------
subroutine modified_NACA_four_digit_thickness_coeffs_2(t_max,u_max,t_TE,dy_dx_TE,I,a,d)
    implicit none

    real,                       intent(in)      :: t_max
    real,                       intent(in)      :: u_max
    real,                       intent(in)      :: t_TE
    real,                       intent(in)      :: dy_dx_TE
    real,                       intent(in)      :: I
    real,                       intent(inout)   :: a(4)
    real,                       intent(inout)   :: d(4)

    ! Local variables
    integer                                     :: fail_flag
    real                                        :: u_TE, temp
    real,           allocatable                 :: aug_matrix(:,:)



    ! Define near trailing edge chordwise location
    u_TE            = 1.0 - (t_TE)


    !
    ! Compute d_0, d_1, d_2 and d_3
    ! Enforce the following conditions: yd_t(u_max)     = t_max
    !                                   yd_t(u_TE)      = t_TE
    !                                   yd'_t(u_max)    = 0.0
    !                                   yd'_t(u_TE)     = -2.0*dy_dx_TE*t_max
    !
    if (allocated(aug_matrix)) deallocate(aug_matrix)
    allocate(aug_matrix(4,5))

    aug_matrix(1,:) = [1.0, 1.0 - u_max, (1.0 - u_max)**2  , (1.0 - u_max)**3       , t_max   ]
    aug_matrix(2,:) = [1.0, 1.0 - u_TE , (1.0 - u_TE)**2   , (1.0 - u_TE)**3        , t_TE    ]
    aug_matrix(3,:) = [0.0, -1.0       , -2.0*(1.0 - u_max), -3.0*((1.0 - u_max)**2), 0.0     ]
    aug_matrix(4,:) = [0.0, -1.0       , -2.0*(1.0 - u_TE) , -3.0*((1.0 - u_TE)**2) , dy_dx_TE]

    call gauss_jordan(4,1,aug_matrix,fail_flag)

    ! Store d_0, d_1, d_2 and d_3
    d               = aug_matrix(:,5)


    !
    ! Compute a_0
    !
    a(1)            = sqrt(2.2038)*(2.0*t_max*(I/6.0))

    !
    ! Compute a_1, a_2 and a_3
    ! Enforce the following conditions: ya_t(u_max)     = yd_t(u_max)
    !                                   ya'_t(u_max)    = yd'_t(u_max)
    !                                   ya''_t(u_max)   = yd''_t(u_max)
    !
    if (allocated(aug_matrix)) deallocate(aug_matrix)
    allocate(aug_matrix(3,4))

    temp            = (2.0*d(3)) + (6.0*d(4)*(1.0 - u_max)) + (0.25*a(1)/((sqrt(u_max))**3))

    aug_matrix(1,:) = [u_max, u_max**2 , u_max**3      , t_max - (a(1)*sqrt(u_max))]
    aug_matrix(2,:) = [1.0  , 2.0*u_max, 3.0*(u_max**2), (-0.5*a(1)/sqrt(u_max))   ]
    aug_matrix(3,:) = [0.0  , 2.0      , 6.0*u_max     , temp                      ]

    call gauss_jordan(3,1,aug_matrix,fail_flag)

    ! Compute a_1, a_2 and a_3
    a(2:)           = aug_matrix(:,4)


end subroutine modified_NACA_four_digit_thickness_coeffs_2
!------------------------------------------------------------------------------------------------------






!
! Obtain thickness distribution with the computed coefficients a_i and d_i
!
! Input parameters: np    - number of points along the blade section meanline
!                   u     - points along chord
!                   u_max - chordwise location of max. thickness for the blade section
!                   t_max - half max. thickness for the blade section in fraction chord
!                   a     - thickness coefficients obtained in modified_NACA_four_digit_thickness_coeffs
!                   d     - thickness coefficients obtained in modified_NACA_four_digit_thickness_coeffs
!           
!------------------------------------------------------------------------------------------------------
subroutine modified_NACA_four_digit_thickness(np,u,u_max,t_max,a,d,thk_data)
    use file_operations
    implicit none

    integer,                intent(in)      :: np
    real,                   intent(in)      :: u(np)
    real,                   intent(in)      :: u_max
    real,                   intent(in)      :: t_max
    real,                   intent(in)      :: a(4)
    real,                   intent(in)      :: d(4)
    real,                   intent(inout)   :: thk_data(np,3)
    !real,                   intent(inout)   :: thk_der(np)

    ! Local variables
    integer                                 :: i
    real                                    :: tol = 10E-8


    ! Compute thickness distribution
    do i = 1,np

        if (abs(u(i) - u_max) .le. tol) then
            thk_data(i,1)   = t_max
        else if (u(i) .lt. u_max) then
            thk_data(i,1)   = (a(1)*sqrt(u(i))) + (a(2)*u(i)) + (a(3)*(u(i)**2)) + (a(4)*(u(i)**3))
        else if (u(i) .gt. u_max) then
            thk_data(i,1)   = d(1) + (d(2)*(1.0 - u(i))) + (d(3)*((1.0 - u(i))**2)) + (d(4)*((1.0 - u(i))**3))
        end if

    end do


    ! Compute first and second derivatives of thickness distribution
    do i = 1,np

        if (abs(u(i) - u_max) .le. tol) then
            thk_data(i,2)   = 0.0
            thk_data(i,3)   = (2.0*d(3)) + (6.0*d(4)*(1.0 - u_max))
        else if (abs(u(i)) .le. tol) then
            thk_data(i,2)   = (0.5*a(1)/sqrt(tol)) + a(2) + (2.0*a(3)*tol) + (3.0*a(4)*(tol**2))
            thk_data(i,3)   = (-0.25*a(1)/((sqrt(tol))**3)) + (2.0*a(3)) + (6.0*a(4)*tol)
        else if (u(i) .lt. u_max) then
            thk_data(i,2)   = (0.5*a(1)/sqrt(u(i))) + a(2) + (2.0*a(3)*u(i)) + (3.0*a(4)*(u(i)**2))
            thk_data(i,3)   = (-0.25*a(1)/((sqrt(u(i)))**3)) + (2.0*a(3)) + (6.0*a(4)*u(i))
        else if (u(i) .gt. u_max) then
            thk_data(i,2)   = -d(2) + (2.0*(u(i) - 1.0)*d(3)) - (3.0*((u(i) - 1.0)**2)*d(4))
            thk_data(i,3)   = (2.0*d(3)) + (6.0*d(4)*(1.0 - u(i)))
        end if

    end do


end subroutine modified_NACA_four_digit_thickness
!------------------------------------------------------------------------------------------------------






!
! Obtain thickness distribution with the computed coefficients a_i and d_i
!
! Input parameters: np    - number of points along the blade section meanline
!                   u     - points along chord
!                   u_max - chordwise location of max. thickness for the blade section
!                   t_max - half max. thickness for the blade section in fraction chord
!                   a     - thickness coefficients obtained in modified_NACA_four_digit_thickness_coeffs
!                   d     - thickness coefficients obtained in modified_NACA_four_digit_thickness_coeffs
!           
!------------------------------------------------------------------------------------------------------
subroutine modified_NACA_four_digit_thickness_2(np,u,u_max,t_max,t_TE,a,d,thk_data)
    use file_operations
    implicit none

    integer,                intent(in)      :: np
    real,                   intent(in)      :: u(np)
    real,                   intent(in)      :: u_max
    real,                   intent(in)      :: t_max
    real,                   intent(in)      :: t_TE
    real,                   intent(in)      :: a(4)
    real,                   intent(in)      :: d(4)
    real,                   intent(inout)   :: thk_data(np,3)

    ! Local variables
    integer                                 :: i, counter
    real                                    :: u_TE, tol = 10E-8



    !
    ! Find point closest to u_TE
    !
    u_TE                = 1.0 - t_TE
    counter             = 0
    do i = 1,np
        
        if (u(i) .le. u_TE) then
            counter     = counter + 1
        else
            exit
        end if

    end do

    ! Initialize thickness array as zero-valued
    thk_data            = 0.0


    ! 
    ! Compute NACA thickness distribution
    ! Also compute first and second derivative
    !
    do i = 1,counter

        if (abs(u(i) - u_max) .le. tol) then
            thk_data(i,1)   = t_max
            thk_data(i,2)   = 0.0  
            thk_data(i,3)   = (2.0*d(3)) + (6.0*d(4)*(1.0 - u_max))
        else if (abs(u(i)) .le. tol) then
            thk_data(i,1)   = 0.0
            thk_data(i,2)   = (0.5*a(1)/sqrt(tol)) + a(2) + (2.0*a(3)*tol) + (3.0*a(4)*(tol**2))    
            thk_data(i,3)   = (-0.25*a(1)/((sqrt(tol))**3)) + (2.0*a(3)) + (6.0*a(4)*tol)
        else if (u(i) < u_max) then
            thk_data(i,1)   = (a(1)*sqrt(u(i))) + (a(2)*u(i)) + (a(3)*(u(i)**2)) + (a(4)*(u(i)**3)) 
            thk_data(i,2)   = (0.5*a(1)/sqrt(u(i))) + a(2) + (2.0*a(3)*u(i)) + (3.0*a(4)*(u(i)**2))
            thk_data(i,3)   = (-0.25*a(1)/((sqrt(u(i)))**3)) + (2.0*a(3)) + (6.0*a(4)*u(i))
        else if (u(i) > u_max) then
            thk_data(i,1)   = d(1) + (d(2)*(1.0 - u(i))) + (d(3)*((1.0 - u(i))**2)) + (d(4)*((1.0 - u(i))**3))
            thk_data(i,2)   = -d(2)  - (2.0*d(3)*(1.0 - u(i))) - (3.0*d(4)*((1.0 - u(i))**2))
            thk_data(i,3)   = (2.0*d(3)) + (6.0*d(4)*(1.0 - u(i)))
        end if

    end do


    !
    ! Add circular TE thickness
    !
    do i = counter + 1,np - 1
        thk_data(i,1)       = sqrt(t_TE**2 - ((u(i) - u_TE)**2)) 
    end do
    thk_data(np,1)          = 0.0



end subroutine modified_NACA_four_digit_thickness_2
!------------------------------------------------------------------------------------------------------







!
! Collect modified NACA four digit thickness computation in one subroutine
! To enable calling in a loop for numerical runtime checks
!
!------------------------------------------------------------------------------------------------------
subroutine modified_NACA_four_digit_thickness_all(js,np,u,u_max,t_max,t_TE,a,d,thk_data,monotonic,write_to_file)
    use errors
    use file_operations
    implicit none

    integer,                    intent(in)          :: js
    integer,                    intent(in)          :: np
    real,                       intent(in)          :: u(np)
    real,                       intent(in)          :: u_max
    real,                       intent(in)          :: t_max
    real,                       intent(in)          :: t_TE
    real,                       intent(in)          :: a(4)
    real,                       intent(in)          :: d(4)
    real,                       intent(inout)       :: thk_data(np,3)
    logical,                    intent(inout)       :: monotonic
    logical,                    intent(in)          :: write_to_file

    ! Local variables
    character(:),   allocatable                     :: error_msg, dev_msg, warning_msg
    character(10)                                   :: sec
    integer                                         :: i
    logical                                         :: thk_der(np)


    !
    ! Compute the thickness distribution
    !
    call modified_NACA_four_digit_thickness_2(np,u,u_max,t_max,t_TE,a,d,thk_data)


    !
    ! Convert section number into a string
    !
    write(sec, '(i2)') js


    !
    ! Check for negative thickness
    !
    do i = 1,np
    
        if (thk_data(i,1) < 0) then
            if (write_to_file) then
                error_msg   = 'Negative thickness encountered for blade section '//trim(adjustl(sec))
                dev_msg     = 'Check subroutine bladegen in bladegen.f90'
                call fatal_error(error_msg, dev_msg = dev_msg)
            end if
        end if

    end do
   

    !
    ! Check for monotonicity
    ! 
    do i = 1,np

        thk_der(i)      = (thk_data(i,3) > 0.0)
        if (thk_der(i)) then

            if (write_to_file) then
                warning_msg = 'Thickness distribution for blade section '//trim(adjustl(sec))//" isn't monotonic"
                dev_msg     = 'Check subroutine bladegen in bladegen.f90'
                call warning(warning_msg, dev_msg = dev_msg)
            end if
            monotonic   = .false.
            exit

        end if

    end do


end subroutine modified_NACA_four_digit_thickness_all
!------------------------------------------------------------------------------------------------------





















