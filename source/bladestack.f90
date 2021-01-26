subroutine bladestack(nspn,X_le,X_te,R_le,R_te,nsec,scf,msle,np,stack,cpdeltam,spanmp,xcpdelm,cpdeltheta,    &
                      spantheta,xcpdeltheta,cpinbeta,spaninbeta,xcpinbeta,cpoutbeta,spanoutbeta,xcpoutbeta,  &
                      xm,rm,xms,rms,mp,nsp,bladedata,amount_data,intersec_coord,throat_3D,mouth_3D,exit_3D,  &
                      casename,nbls,LE,axchrd,mble,mbte,units,stagger,chrdsweep,chrdlean,axial_LE,radial_LE, &
                      thick_distr,x_in,y_in,nmeanline,xmeanline,ymeanline,mt_umax,clustering_switch,         &
                      clustering_parameter,dim_thick)

    use file_operations
    use funcNsubs
    implicit none

    ! Integer parameters
    integer,    parameter                               :: nspan = 200, nx = 500, nxx = 1000, nax = 50, nbx = 500, nby = 100,   &
                                                           nrow = 1

    ! Input parameters
    integer,                            intent(in)      :: nspn, nsec, np, stack, cpdeltam, cpdeltheta, cpinbeta, cpoutbeta,    &
                                                           nsp(nspan), amount_data, nbls, chrdsweep, chrdlean, thick_distr,     &
                                                           LE, nmeanline, clustering_switch
    real,                               intent(in)      :: X_le(nspan), X_te(nspan), R_le(nspan), R_te(nspan), xm(nx,nax),      &
                                                           rm(nx,nax), xms(nx,nax), rms(nx,nax), mp(nx,nax), clustering_parameter
    real,                               intent(in)      :: scf, msle(nspan), spanmp(100), xcpdelm(100), spantheta(100),         &
                                                           xcpdeltheta(100), spaninbeta(100), xcpinbeta(100), spanoutbeta(100), &
                                                           xcpoutbeta(100), intersec_coord(12,nspn), axchrd(nspan), mble(nspan),&
                                                           mbte(nspan), stagger(nspan)
    character(32),                      intent(in)      :: casename
    character(2),                       intent(in)      :: units
    logical,                            intent(in)      :: axial_LE, radial_LE
    real,                               intent(inout)   :: bladedata(amount_data,nspn), throat_3D(nspn), mouth_3D(nspn),        &
                                                           exit_3D(nspn)
    real,                               intent(inout)   :: x_in(np,nspn), y_in(np,nspn), xmeanline(nspn,nmeanline),             &
                                                           ymeanline(nspn,nmeanline), mt_umax(nspn,6), dim_thick(nspn,3)

    ! Local variables
    character(80)                                       :: fname1
    character(20)                                       :: temp
    integer                                             :: na, i, ia, iap, k, ile, uplmt, nap(nspan), i_slope, ncp1, nspline,   &
                                                           nopen, ntemp, nmid, temp1, temp2
    real,           allocatable                         :: xa(:,:), ya(:,:), xb(:,:), rb(:,:), yb(:,:), zb(:,:), xposlean(:,:), &
                                                           yposlean(:,:), zposlean(:,:), xneglean(:,:), yneglean(:,:),          &
                                                           zneglean(:,:)
    real                                                :: chord_actual(100), mps(nxx,nax), demp, spl_eval, xxa, yya, pi, dtor, &
                                                           dmp(nspan), mp_stack(nspan), xm_slope, rm_slope, mps_inter,          &
                                                           inter_xb(6,nspn), inter_rb(6,nspn), inter_yb(6,nspn),                &
                                                           inter_zb(6,nspn), lref, delmp(nspan), span(nspan), mpxc(nspan),      &
                                                           delta_theta(nspan), y_spl_end(nx), xbs(nx), ybs(nx), xc(nx), yc(nx), &
                                                           mp3D_meanline(nspn,nmeanline), xem(nspn,nmeanline),                  &
                                                           rem(nspn,nmeanline), yem(nspn,nmeanline), zem(nspn,nmeanline),       &
                                                           xrt_umax(nspn,6), xyz_umax(nspn,6)  !stingl(nspan)
    character(:),   allocatable                         :: log_file
    logical                                             :: file_open, isquiet
    common / BladeSectionPoints /xxa(nxx,nax),yya(nxx,nax)


    ! Initialize variables
    ile = 0
    xyz_umax = 0.0



    !
    ! Get the value of isquiet
    !
    call get_quiet_status(isquiet)



    ! Constants
    pi   = 4.*atan(1.0)
    dtor = pi/180.


    !
    ! Allocation of variables
    !
    if (allocated(xa)) deallocate(xa)
    allocate(xa(np,nspn))
    if (allocated(ya)) deallocate(ya)
    allocate(ya(np,nspn))
    if (allocated(xb)) deallocate(xb)
    allocate(xb(np,nspn))
    if (allocated(rb)) deallocate(rb)
    allocate(rb(np,nspn))
    if (allocated(yb)) deallocate(yb)
    allocate(yb(np,nspn))
    if (allocated(zb)) deallocate(zb)
    allocate(zb(np,nspn))



    !
    ! Sting LE option - m' Offset for obtaining the chord
    ! Subtracted from the m' coordinates for this option.
    ! TODO: Will be removed
    !
    !if (LE == 2) then
    !    do ia = 1, nspn
    !        stingl(ia) = mbte(ia) - mble(ia) - axchrd(ia)
    !    end do
    !end if



    !
    ! Read in m',theta airfoil coordinates 
    !
    na = nspn
    
    call log_file_exists(log_file, nopen, file_open)
    
    if (.not. isquiet) print*,'Number of airfoil coordinates:',np
    write(nopen,*) 'Number of airfoil coordinates:', np
   
    ! In a normal T-Blade3 run, x_in = m'
    !                           y_in = theta 
    do ia = 1, na
        nap(ia) = np
        iap = nap(ia)
        do i = 1, iap
            xa(i,ia) = x_in(i,ia)
            ya(i,ia) = y_in(i,ia)
        end do         
    end do



    !
    ! Write stacking axis location to log file and print to screen
    ! 
    if (.not. isquiet) then
        write(*,*)
        print*,'Stacking Axis location:',stack 
        write(*,*)
    end if
    write(nopen,*) ''
    write(nopen,*) 'Stacking Axis location:', stack
    write(nopen,*) ''
    do ia = 1,na
        if (.not. isquiet) print*,'Section #',ia
        write(nopen,*) 'Section #', ia
    end do
    if (.not. isquiet) write(*,*) 



    !
    ! Checking if r_slope > x_slope for non-axial machines
    !
    do ia = 1,na
        i_slope = 0
        do i = 1,nsp(ia)
            xm_slope = abs(xms(i,ia))
            rm_slope = abs(rms(i,ia))
            if (rm_slope >= xm_slope .and. i_slope == 0) i_slope = i
        end do
        if (.not. isquiet) print*,'i_slope',i_slope
        write(nopen,*) 'i_slope', i_slope
    end do
    call close_log_file(nopen, file_open)



    !
    ! Span calculation for axial and non axial machines.
    ! For radial flow span is difference in x coordinates.
    !
    do ia = 1,na
        
        ! Evaluating span for axial blade
        if (i_slope == 0) then  
   
            lref = abs(R_le(na) - R_le(1))
            span(ia) = abs(R_le(ia) - R_le(1))/lref
   
        ! Evaluating span for non-axial flow blade  
        else if (i_slope /= 0) then 
  
            ! Axial flow at LE 
            if (axial_LE) then 
                
                lref = abs(R_le(na) - R_le(1))
                span(ia) = abs(R_le(ia) - R_le(1))/lref
      
            ! Non-axial flow at LE 
            else if (radial_LE) then
    
                ! spl_inv in subroutine spline.f90 
                call spl_inv(mpxc(ia),R_le(1),rm(1,ia),rms(1,ia),mp(1,ia),nsp(ia))
                lref = abs(X_le(na) - X_le(1))
                span(ia) = abs(X_le(ia) - X_le(1))/lref
       
            end if ! axial_LE/radial_LE
        
        end if ! i_slope

    end do



    !
    ! Cubic spline for sweep control points
    ! cubicspline and cubicbspline_intersec in cubicspline.f90
    !
    call cubicspline(cpdeltam,xcpdelm,spanmp,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
    call cubicbspline_intersec(ncp1,xc,yc,na,span,delmp,xbs,ybs,y_spl_end)



    !
    ! Cubic spline for lean control points
    ! cubicspline and cubicbspline_intersec in cubicspline.f90
    !
    call cubicspline(cpdeltheta,xcpdeltheta,spantheta,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
    call cubicbspline_intersec(ncp1,xc,yc,na,span,delta_theta,xbs,ybs,y_spl_end)


    
    !
    ! Sweep definition (delta_m control points)
    !
    call log_file_exists(log_file, nopen, file_open)
    write(nopen,*) ''

    ! True sweep
    if (chrdsweep == 1) then 
        
        if (.not. isquiet) write(*,*)'span          true_sweep'
        write(nopen,*)'span          true_sweep'
        do ia = 1, na
            delmp(ia) = (delmp(ia)*abs(cos(stagger(ia))))
            if (.not. isquiet) print*,span(ia),delmp(ia)
            write(nopen,*) span(ia), delmp(ia)
        end do

    ! True lean
    else if (chrdlean == 1) then 
        
        if (.not. isquiet) write(*,*)'span          delta_m for true_lean'
        write(nopen,*)'span          delta_m for true_lean'
        do ia = 1, na
            delmp(ia) = (delta_theta(ia)*abs(sin(stagger(ia))))
            if (.not. isquiet) print*,span(ia),delmp(ia)
            write(nopen,*) span(ia), delmp(ia)
        end do

    ! Axial sweep
    else 
        
        if (.not. isquiet) write(*,*)'span          delta_m'
        write(nopen,*) 'span          delta_m'
        do ia = 1, na       
            if (.not. isquiet) print*,span(ia),delmp(ia)
            write(nopen,*) span(ia), delmp(ia)
        end do

    end if 
   
    ! Store delta_m control points in blade data array 
    do ia = 1, na
       bladedata(7,ia) = delmp(ia)
    end do
    
    call close_log_file(nopen, file_open)



    !
    ! Lean definition (delta_theta control points)
    !
    call log_file_exists(log_file, nopen, file_open)
    if (.not. isquiet) write(*,*)
    write(nopen,*) ''
    
    ! True lean
    if (chrdlean == 1) then 
        
        if (.not. isquiet) write(*,*)'span         true_lean'
        write(nopen,*)'span         true_lean'
        do ia = 1, na
            delta_theta(ia) = (delta_theta(ia)*abs(cos(stagger(ia))))
            if (.not. isquiet) print*,span(ia),delta_theta(ia)
            write(nopen,*) span(ia), delta_theta(ia)
        end do

    ! True sweep
    else if (chrdsweep == 1) then 
        
        if (.not. isquiet) write(*,*)'span         delta_theta for true_sweep'
        write(nopen,*)'span         delta_theta for true_sweep'
        do ia = 1, na
            delta_theta(ia) = (delmp(ia)*abs(sin(stagger(ia))))
            if (.not. isquiet) print*,span(ia),delta_theta(ia)
            write(nopen,*) span(ia), delta_theta(ia)
        end do

    ! Tangential lean
    else 
      
        if (.not. isquiet) write(*,*)'span         delta_theta'
        write(nopen,*)'span         delta_theta'
        do ia = 1, na      
            if (.not. isquiet) print*,span(ia),delta_theta(ia)
            write(nopen,*) span(ia), delta_theta(ia)
        end do

    end if  ! chrdlean
    
    ! Store delta_theta control points in blade data array
    do ia = 1, na
       bladedata(8,ia) = delta_theta(ia)
    end do
    
    if (.not. isquiet) write(*,*)
    write(nopen,*) ''
    
    call close_log_file(nopen, file_open)



    !
    ! Calculating streamwise m' values for mapping of airfoils
    ! Offset delta_m' = m'_s,LE - m'_b,LE
    ! m'_3D = m'_blade + offset + sweep
    !
    do ia = 1,na
       
        write(temp,*) ia
        ile                     = (iap + 1)/2
        mp_stack(ia)            = msle(ia)
        dmp(ia)                 = mp_stack(ia) - xa(ile,ia)
       
        do i = 1,iap
            demp                = delmp(ia)
            mps(i,ia)           = xa(i,ia) + dmp(ia) + delmp(ia)
        end do

        ! Compute m'_3D for extended meanlines
        do i = 1, nmeanline
            mp3D_meanline(ia,i) = xmeanline(ia,i) + dmp(ia) + delmp(ia)
        end do

        if (thick_distr == 5) then

            ! Compute m'_3D for maximum thickness points
            mt_umax(ia,1)           = mt_umax(ia,1) + dmp(ia) + delmp(ia)
            mt_umax(ia,3)           = mt_umax(ia,3) + dmp(ia) + delmp(ia)

            ! Theta coordinates for cylindrical version of
            ! maximum thickness points
            xrt_umax(ia,3)          = mt_umax(ia,2)
            xrt_umax(ia,6)          = mt_umax(ia,4)

        end if

    end do  ! ia = 1,na



    !
    ! Calculating streamwise x and r coordinates
    !
    do ia = 1,na

        ! For blade section
        do i = 1,iap
            xb(i,ia)    = spl_eval(nsp(ia),mps(i,ia),xm(1,ia),xms(1,ia),mp(1,ia))
            rb(i,ia)    = spl_eval(nsp(ia),mps(i,ia),rm(1,ia),rms(1,ia),mp(1,ia))
        end do

        ! For extended meanlines
        do i = 1, nmeanline
           xem(ia,i)    = spl_eval(nsp(ia), mp3D_meanline(ia,i), xm(1,ia), xms(1,ia), mp(1,ia))
           rem(ia,i)    = spl_eval(nsp(ia), mp3D_meanline(ia,i), rm(1,ia), rms(1,ia), mp(1,ia))
        end do

        ! For maximum thickness points
        if (thick_distr == 5) then
            xrt_umax(ia,1)  = spl_eval(nsp(ia), mt_umax(ia,1), xm(1,ia), xms(1,ia), mp(1,ia))
            xrt_umax(ia,2)  = spl_eval(nsp(ia), mt_umax(ia,1), rm(1,ia), rms(1,ia), mp(1,ia))
            xrt_umax(ia,4)  = spl_eval(nsp(ia), mt_umax(ia,3), xm(1,ia), xms(1,ia), mp(1,ia))
            xrt_umax(ia,5)  = spl_eval(nsp(ia), mt_umax(ia,3), rm(1,ia), rms(1,ia), mp(1,ia))
        end if

    end do


    
    !
    ! Get the throat intersection points mapping (xb,rb) values
    !
    do ia = 1,na
        do i = 1,6   
            mps_inter = intersec_coord(2*i-1,ia) + dmp(ia) + delmp(ia)
            inter_xb(i,ia) = spl_eval(nsp(ia),mps_inter,xm(1,ia),xms(1,ia),mp(1,ia))
            inter_rb(i,ia) = spl_eval(nsp(ia),mps_inter,rm(1,ia),rms(1,ia),mp(1,ia))
        end do
    end do
    if (.not. isquiet) print*,''


   
    !
    ! Allocate arrays for coordinates for the half pitch leaned blade for 
    ! periodic walls
    !
    if (allocated(xposlean)) deallocate(xposlean)
    allocate(xposlean(iap,na))
    if (allocated(yposlean)) deallocate(yposlean)
    allocate(yposlean(iap,na))
    if (allocated(zposlean)) deallocate(zposlean)
    allocate(zposlean(iap,na))
    if (allocated(xneglean)) deallocate(xneglean)
    allocate(xneglean(iap,na))
    if (allocated(yneglean)) deallocate(yneglean)
    allocate(yneglean(iap,na))
    if (allocated(zneglean)) deallocate(zneglean)
    allocate(zneglean(iap,na))

    !
    ! Converting cylindrical coordinates to cartesian coordinates
    ! x = x
    ! y = r*sin(theta + delta_theta)
    ! z = r*cos(theta + delta_theta)
    !
    do ia = 1,na        

        do i = 1,iap     
            
            !if (LE == 2) then
            !    xb(i,ia)     = xb(i,ia) - stingl(ia)
            !else
            xb(i,ia)     = xb(i,ia)
            !end if
            yb(i,ia)         = rb(i,ia)*sin(ya(i,ia) + delta_theta(ia)) 
            zb(i,ia)         = rb(i,ia)*cos(ya(i,ia) + delta_theta(ia)) 
            
            ! Positive half pitch leaned coordinates 
            xposlean(i,ia)   = xb(i,ia) 
            yposlean(i,ia)   = rb(i,ia)*sin(ya(i,ia) + (pi/nbls)) 
            zposlean(i,ia)   = rb(i,ia)*cos(ya(i,ia) + (pi/nbls)) 
            
            ! Negative half pitch leaned coordinates 
            xneglean(i,ia)   = xb(i,ia) 
            yneglean(i,ia)   = rb(i,ia)*sin(ya(i,ia) - (pi/nbls)) 
            zneglean(i,ia)   = rb(i,ia)*cos(ya(i,ia) - (pi/nbls)) 

        end do   ! i = 1,iap


        ! Convert extended meanlines to Cartesian coordinates
        do i = 1, nmeanline
            yem(ia, i)      = rem(ia, i) * sin (ymeanline(ia, i) + delta_theta(ia))
            zem(ia, i)      = rem(ia, i) * cos (ymeanline(ia, i) + delta_theta(ia))
        end do

        ! Convert maximum thickness points to Cartesian coordinates
        if (thick_distr == 5) then
            xyz_umax(ia,1)      = scf * xrt_umax(ia,1)
            xyz_umax(ia,2)      = scf * xrt_umax(ia,2) * sin(xrt_umax(ia,3) + delta_theta(ia))
            xyz_umax(ia,3)      = scf * xrt_umax(ia,2) * cos(xrt_umax(ia,3) + delta_theta(ia))
            xyz_umax(ia,4)      = scf * xrt_umax(ia,4)
            xyz_umax(ia,5)      = scf * xrt_umax(ia,5) * sin(xrt_umax(ia,6) + delta_theta(ia))
            xyz_umax(ia,6)      = scf * xrt_umax(ia,5) * cos(xrt_umax(ia,6) + delta_theta(ia))
        end if

        ! Get (x,y,z) for the intersection point
        do k = 1, 6      
            
            !if (LE == 2) then
            !    inter_xb(k,ia) = inter_xb(k,ia) - stingl(ia)
            !else
            inter_xb(k,ia) = inter_xb(k,ia)
            !end if
            inter_yb(k,ia)   = inter_rb(k,ia)*sin(intersec_coord(2*k,ia) + delta_theta(ia))
            inter_zb(k,ia)   = inter_rb(k,ia)*cos(intersec_coord(2*k,ia) + delta_theta(ia))

        end do  ! k = 1, 6

    end do  ! ia = 1, na



    !
    ! Calculation of the 3D throat length
    !
    call log_file_exists(log_file, nopen, file_open)
    
    do ia = 1,na
        
        throat_3D(ia) = sqrt((inter_xb(1,ia) - inter_xb(2,ia))**2 + &
                        (inter_yb(1,ia) - inter_yb(2,ia))**2 +      &
                        (inter_zb(1,ia) - inter_zb(2,ia))**2)
        mouth_3D(ia)  = sqrt((inter_xb(3,ia) - inter_xb(4,ia))**2 + &
                        (inter_yb(3,ia) - inter_yb(4,ia))**2 +      &
                        (inter_zb(3,ia)-inter_zb(4,ia))**2)
        exit_3D(ia)   = sqrt((inter_xb(5,ia) - inter_xb(6,ia))**2 + &
                        (inter_yb(5,ia) - inter_yb(6,ia))**2 +      &
                        (inter_zb(5,ia) - inter_zb(6,ia))**2)
       
        ! Write 3D throat parameters to log file and print to screen
        if (throat_3D(ia) /= 0) then 
            if (.not. isquiet) then
                print*,'section(',ia,')'
                print*,'3D throat line [',units,'] =',throat_3D(ia)*scf
                print*,'3D mouth line [',units,'] =',mouth_3D(ia)*scf
                print*,'3D exit line [',units,'] =',exit_3D(ia)*scf
            end if
            write(nopen,*)'section(',ia,')'
            write(nopen,*) '3D throat line [',units,'] =',throat_3D(ia)*scf
            write(nopen,*) '3D mouth line [',units,'] =',mouth_3D(ia)*scf
            write(nopen,*) '3D exit line [',units,'] =',exit_3D(ia)*scf
        end if

    end do  ! ia = 1, na



    !
    ! Compute dimensional thicknesses to write out
    ! to .csv file
    ! TODO: Only for NACA thickness with ellipse-based clustering
    !
    ntemp = int(clustering_parameter)
    nmid = (np + 1)/2
    if (thick_distr == 5) then
        if (clustering_switch == 4) then
            do ia = 1, na

                ! Dimensional TE thickness for all sections
                temp1 = ntemp
                temp2 = np - ntemp + 1
                dim_thick(ia,1) = scf * sqrt((xb(temp1,ia) - xb(temp2,ia))**2 + (yb(temp1,ia) - yb(temp2,ia))**2 + &
                                             (zb(temp1,ia) - zb(temp2,ia))**2)

                ! Dimensional maximum thickness for all sections
                dim_thick(ia,2) = sqrt((xyz_umax(ia,1) - xyz_umax(ia,4))**2 + (xyz_umax(ia,2) - xyz_umax(ia,5))**2 + &
                                       (xyz_umax(ia,3) - xyz_umax(ia,6))**2)

                ! Dimensional LE thickness for all sections
                temp1 = nmid - ntemp + 1
                temp2 = nmid + ntemp - 1
                dim_thick(ia,3) = scf * sqrt((xb(temp1,ia) - xb(temp2,ia))**2 + (yb(temp1,ia) - yb(temp2,ia))**2 + &
                                             (zb(temp1,ia) - zb(temp2,ia))**2)
            end do
        end if
    end if



    !
    ! Writing dimensional coordinates into a single file 'blade3D.casename.dat'.
    !
    if (.not. isquiet) write(*,*)"Number of radial sections:",nsec
    write(nopen,*) 'Number of radial sections:', nsec
    
    ! TODO: Move to file_operations
    fname1 = 'blade3d.'//trim(casename)//'.dat'
    open(3,file=fname1,status='unknown')
    if (.not. isquiet) then
        write(*,*)
        write(*,*) 'Writing 3D blade geometry ...'
        write(*,*)
    end if
    write(nopen,*) ''
    write(nopen,*) 'Writing 3D blade geometry ...'
    write(nopen,*) ''
    write(3,*) iap,nsec


    
    !
    ! Store blade (x,y,z) coordinates 
    !
    !xbi = scf*transpose(xb)
    !ybi = scf*transpose(yb)
    !zbi = scf*transpose(zb)



    !
    ! Dimensional chord calculation
    !
    do ia = 1,nsec
       
        do i = 1,iap
            write(3,10) scf*xb(i,ia),scf*yb(i,ia),scf*zb(i,ia)
        end do

        chord_actual(ia) = scf*sqrt((xb(ile,ia) - xb(iap,ia))**2 + (yb(ile,ia) - yb(iap,ia))**2 + &
                           (zb(ile,ia) - zb(iap,ia))**2)
        if (.not. isquiet) print*,'chord_actual(',units,'):',chord_actual(ia)
        write(nopen,*) 'chord_actual(',units,'):',chord_actual(ia)
        bladedata(6,ia)= chord_actual(ia) 

    end do  ! ia = 1, nsec

    ! Close file 'blade3D.casename.dat'
    close(3)



    !
    ! Calculating the meanline by taking the average of PS and SS curves.
    !
    if (.not. isquiet) then
        write(*,*)
        print*,'Calculating the meanline by taking the average of PS and SS in 3D...'
        print*,'Writing the meanline in 3D to meanline.sec#.dat files...'
        write(*,*)
        print*,'Writing the top and bottom periodic wall coordinates...'
    end if
    write(nopen,*) ''
    write(nopen,*) 'Calculating the meanline by taking the average of PS ans SS in 3D...'
    write(nopen,*) 'Writing the meanline in 3D to meanline.sec#.dat files...'
    write(nopen,*) ''
    write(nopen,*) 'Writing the top and bottom periodic wall coordinates...'
    
    call close_log_file(nopen, file_open)
    
    uplmt = ((iap + 1)/2) - 1 

    ! Calculate constant slope meanline
    ! constantslopemeanline3D in funcNsubs.f90
    !call constantslopemeanline3D(xb,yb,zb,xposlean,yposlean,zposlean,xneglean,yneglean,zneglean,iap,nsec, &
    !                                 uplmt,scf,casename)
    call write_extended_meanlines (casename, nspn, nmeanline, scf*xem, scf*yem, scf*zem)

    ! format statements
    10 format(3(f25.16,1x))
    return


end subroutine bladestack





















