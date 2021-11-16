subroutine bladestack(nspn,X_le,X_te,R_le,R_te,nsec,scf,msle,np,stack,cpdeltam,spanmp,xcpdelm,cpdeltheta,    &
                      spantheta,xcpdeltheta,cpinbeta,spaninbeta,xcpinbeta,cpoutbeta,spanoutbeta,xcpoutbeta,  &
                      xm,rm,xms,rms,mp,nsp,bladedata,amount_data,intersec_coord,throat_3D,mouth_3D,exit_3D,  &
                      casename,nbls,LE,axchrd,mble,mbte,units,stagger,chrdsweep,chrdlean,axial_LE,radial_LE, &
                      thick_distr,x_in,y_in,nmeanline,xmeanline,ymeanline,mt_umax,clustering_switch,         &
                      clustering_parameter,dim_thick,dm_dcurv,dth_dcurv,dm_dthk,dth_dthk,dm_dinbeta,         &
                      dth_dinbeta,dm_doutbeta,dth_doutbeta,dm_dcm,dth_dcm)

    use globvar,                only: delmp_xcp_ders, delmp_ycp_ders, delta_theta_xcp_ders, delta_theta_ycp_ders, &
                                      ncp_chord_curv, ncp_span_curv, ncp_span_thk, ncp_chord_thk, cpchord
    use file_operations
    use errors
    use funcNsubs
    use derivatives,            only: compute_spanwise_xcp_ders, compute_spanwise_ycp_ders, spl_eval_ders, &
                                      yz_sweep_ders, yz_lean_ders, yz_param_ders
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
    real,                               intent(in)      :: dm_dcurv(nspn, np, ncp_chord_curv - 1, ncp_span_curv),               &
                                                           dth_dcurv(nspn, np, ncp_chord_curv - 1, ncp_span_curv),              &
                                                           dm_dthk(nspn, np, ncp_chord_thk - 1, ncp_span_thk),                  &
                                                           dth_dthk(nspn, np, ncp_chord_thk - 1, ncp_span_thk),                 &
                                                           dm_dinbeta(nspn,np,2,cpinbeta), dth_dinbeta(nspn, np, 2, cpinbeta),  &
                                                           dm_doutbeta(nspn,np,2,cpoutbeta),                                    &
                                                           dth_doutbeta(nspn, np, 2, cpoutbeta), dm_dcm(nspn, np, 2, cpchord),  &
                                                           dth_dcm(nspn, np, 2, cpchord)
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
    integer,        allocatable                         :: segment_info(:,:), cp_pos(:,:)
    real,           allocatable                         :: xa(:,:), ya(:,:), xb(:,:), rb(:,:), yb(:,:), zb(:,:), xposlean(:,:), &
                                                           yposlean(:,:), zposlean(:,:), xneglean(:,:), yneglean(:,:),          &
                                                           zneglean(:,:), spline_params(:), dcpall(:,:)
    real                                                :: chord_actual(100), mps(nxx,nax), demp, spl_eval, pi, dtor,           &
                                                           dmp(nspan), mp_stack(nspan), xm_slope, rm_slope, mps_inter,          &
                                                           inter_xb(6,nspn), inter_rb(6,nspn), inter_yb(6,nspn),                &
                                                           inter_zb(6,nspn), lref, delmp(nspan), span(nspan), mpxc(nspan),      &
                                                           delta_theta(nspan), y_spl_end(nx), xbs(nx), ybs(nx), xc(nx), yc(nx), &
                                                           mp3D_meanline(nspn,nmeanline), xem(nspn,nmeanline),                  &
                                                           rem(nspn,nmeanline), yem(nspn,nmeanline), zem(nspn,nmeanline),       &
                                                           xrt_umax(nspn,6), xyz_umax(nspn,6)  !stingl(nspan)
    real                                                :: dm3D_dcurv(nspn, np, ncp_chord_curv - 1, ncp_span_curv),             &
                                                           dm3D_dthk(nspn, np, ncp_chord_thk - 1, ncp_span_thk),                &
                                                           dm3D_dinbeta(nspn, np, 2, cpinbeta),                                 &
                                                           dm3D_doutbeta(nspn, np, 2, cpoutbeta),                               &
                                                           dm3D_dcm(nspn, np, 2, cpchord), dm3D_dsweep(nspn, np, 2, cpdeltam)
    real                                                :: dx_dcurv(nspn, np, ncp_chord_curv - 1, ncp_span_curv),               &
                                                           dr_dcurv(nspn, np, ncp_chord_curv - 1, ncp_span_curv),               &
                                                           dx_dthk(nspn, np, ncp_chord_thk - 1, ncp_span_thk),                  &
                                                           dr_dthk(nspn, np, ncp_chord_thk - 1, ncp_span_thk),                  &
                                                           dx_dinbeta(nspn,np,2,cpinbeta),                                      &
                                                           dr_dinbeta(nspn, np, 2, cpinbeta), dx_doutbeta(nspn,np,2,cpoutbeta), &
                                                           dr_doutbeta(nspn, np, 2, cpoutbeta), dx_dcm(nspn, np, 2, cpchord),   &
                                                           dr_dcm(nspn, np, 2, cpchord), dx_dsweep(nspn, np, 2, cpdeltam),      &
                                                           dr_dsweep(nspn, np, 2, cpdeltam), dx_dlean(nspn, np, 2, cpdeltheta)
    real                                                :: dy_dcurv(nspn, np, ncp_chord_curv - 1, ncp_span_curv),               &
                                                           dz_dcurv(nspn, np, ncp_chord_curv - 1, ncp_span_curv),               &
                                                           dy_dthk(nspn, np, ncp_chord_thk - 1, ncp_span_thk),                  &
                                                           dz_dthk(nspn, np, ncp_chord_thk - 1, ncp_span_thk),                  &
                                                           dy_dinbeta(nspn,np,2,cpinbeta), dz_dinbeta(nspn, np, 2, cpinbeta),   &
                                                           dy_doutbeta(nspn,np,2,cpoutbeta),                                    &
                                                           dz_doutbeta(nspn, np, 2, cpoutbeta), dy_dcm(nspn, np, 2, cpchord),   &
                                                           dz_dcm(nspn, np, 2, cpchord), dy_dsweep(nspn, np, 2, cpdeltam),      &
                                                           dz_dsweep(nspn, np, 2, cpdeltam), dy_dlean(nspn, np, 2, cpdeltheta), &
                                                           dz_dlean(nspn, np, 2, cpdeltheta)
    character(:),   allocatable                         :: log_file, msg_1, msg_2, msg_3
    logical                                             :: file_open, isquiet
    !common / BladeSectionPoints /xxa(nxx,nax),yya(nxx,nax)



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
    ! Allocate segment_info and spline_params
    !
    if (allocated(segment_info)) deallocate(segment_info)
    allocate(segment_info(na, 4))
    if (allocated(spline_params)) deallocate(spline_params)
    allocate(spline_params(na))

    !
    ! Allocate dcpall for sweep control points
    !
    if (allocated(dcpall)) deallocate(dcpall)
    allocate(dcpall(cpdeltam, cpdeltam + 2))

    ! Allocate cp_pos for sweep control points
    if (allocated(cp_pos)) deallocate(cp_pos)
    allocate(cp_pos(na, cpdeltam))

    ! Allocate arrays for derivatives of delmp wrt
    ! the control points of Span and delta_m
    if (allocated(delmp_xcp_ders)) deallocate (delmp_xcp_ders)
    allocate (delmp_xcp_ders(na, cpdeltam))
    if (allocated(delmp_ycp_ders)) deallocate (delmp_ycp_ders)
    allocate (delmp_ycp_ders(na, cpdeltam))

    !
    ! Initialize to zero if axial sweep option is not
    ! being used
    ! TODO: Extend to true lean and sweep after correction?
    !
    delmp_xcp_ders                      = 0.0
    delmp_ycp_ders                      = 0.0

    !
    ! Cubic spline for sweep control points
    ! cubicspline and cubicbspline_intersec in cubicspline.f90
    !
    call cubicspline(cpdeltam,xcpdelm,spanmp,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1,.true.,dcpall)
    call cubicbspline_intersec(ncp1,xc,yc,na,span,delmp,xbs,ybs,y_spl_end,.true.,segment_info,spline_params,cp_pos)

    !
    ! Compute derivatives of delmp wrt the control
    ! points of Span and delta_m
    !
    if (chrdsweep == 0) then    ! axial sweep

        call compute_spanwise_xcp_ders (na, ncp1, yc, xc, cp_pos, segment_info, spline_params, delmp_xcp_ders)
        call compute_spanwise_ycp_ders (na, ncp1, segment_info, dcpall, spline_params, delmp_ycp_ders)

    else    ! true sweep (show warning for now)

        msg_1 = "Derivatives wrt sweep control points set to 0."
        msg_2 = "Sweep derivatives aren't computed when using true sweep option."
        msg_3 = "See spanwise spline creation using sweep control points in bladestack.f90"

        ! warning defined in errors.f90
        call warning (warning_msg = msg_1, warning_msg_1 = msg_2, dev_msg = msg_3)

    end if



    !
    ! Allocate dcpall for lean control points
    !
    if (allocated(dcpall)) deallocate(dcpall)
    allocate(dcpall(cpdeltheta, cpdeltheta + 2))

    ! Allocate cp_pos for sweep control points
    if (allocated(cp_pos)) deallocate(cp_pos)
    allocate(cp_pos(na, cpdeltheta))

    ! Allocate arrays for derivatives of delta_theta wrt
    ! the control points of Span and delta_theta
    if (allocated(delta_theta_xcp_ders)) deallocate (delta_theta_xcp_ders)
    allocate (delta_theta_xcp_ders(na, cpdeltheta))
    if (allocated(delta_theta_ycp_ders)) deallocate (delta_theta_ycp_ders)
    allocate (delta_theta_ycp_ders(na, cpdeltheta))

    !
    ! Initialize to zero if tangential lean option is
    ! not being used
    ! TODO: Extend to true lean after correction?
    !
    delta_theta_xcp_ders                = 0.0
    delta_theta_ycp_ders                = 0.0

    !
    ! Cubic spline for lean control points
    ! cubicspline and cubicbspline_intersec in cubicspline.f90
    !
    call cubicspline(cpdeltheta,xcpdeltheta,spantheta,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1,.true.,dcpall)
    call cubicbspline_intersec(ncp1,xc,yc,na,span,delta_theta,xbs,ybs,y_spl_end,.true.,segment_info,spline_params,cp_pos)

    !
    ! Compute derivatives of delmp wrt the control
    ! points of Span and delta_theta
    !
    if (chrdlean == 0) then ! tangential lean

        call compute_spanwise_xcp_ders (na, ncp1, yc, xc, cp_pos, segment_info, spline_params, delta_theta_xcp_ders)
        call compute_spanwise_ycp_ders (na, ncp1, segment_info, dcpall, spline_params, delta_theta_ycp_ders)

    else    ! true lean (show warning for now)

        msg_1   = "Derivatives with respect to lean control points set to 0."
        msg_2   = "Lean derivatives aren't computed when using true lean options."
        msg_3   = "See spanwise spline creation using lean control points in bladestack.f90"

        ! warning defined in errors.f90
        call warning (warning_msg = msg_1, warning_msg_1 = msg_2, dev_msg = msg_3)

    end if


    
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
        ile                             = (iap + 1)/2
        mp_stack(ia)                    = msle(ia)
        dmp(ia)                         = mp_stack(ia) - xa(ile,ia)
       
        do i = 1,iap

            demp                        = delmp(ia)
            mps(i,ia)                   = xa(i,ia) + dmp(ia) + delmp(ia)

            ! Compute derivatives of mps wrt input parameters
            dm3D_dcurv(ia, i, :, :)     = dm_dcurv(ia, i, :, :) - dm_dcurv(ia, ile, :, :)       ! mean-line 2nd derivative
            dm3D_dthk(ia, i, :, :)      = dm_dthk(ia, i, :, :) - dm_dthk(ia, ile, :, :)         ! NACA thickness
            dm3D_dinbeta(ia, i, :, :)   = dm_dinbeta(ia, i, :, :) - dm_dinbeta(ia, ile, :, :)   ! in_beta*
            dm3D_doutbeta(ia, i, :, :)  = dm_doutbeta(ia, i, :, :) - dm_doutbeta(ia, ile, :, :) ! out_beta*
            dm3D_dcm(ia, i, :, :)       = dm_dcm(ia, i, :, :) - dm_dcm(ia, ile, :, :)           ! chord_multiplier

            ! Sweep control points
            dm3D_dsweep(ia, i, 1, :)    = delmp_xcp_ders(ia, :)
            dm3D_dsweep(ia, i, 2, :)    = delmp_ycp_ders(ia, :)

        end do

        ! Compute m'_3D for extended meanlines
        do i = 1, nmeanline
            mp3D_meanline(ia,i)         = xmeanline(ia,i) + dmp(ia) + delmp(ia)
        end do

        if (thick_distr == 5) then

            ! Compute m'_3D for maximum thickness points
            mt_umax(ia,1)               = mt_umax(ia,1) + dmp(ia) + delmp(ia)
            mt_umax(ia,3)               = mt_umax(ia,3) + dmp(ia) + delmp(ia)

            ! Theta coordinates for cylindrical version of
            ! maximum thickness points
            xrt_umax(ia,3)              = mt_umax(ia,2)
            xrt_umax(ia,6)              = mt_umax(ia,4)

        end if

    end do  ! ia = 1,na



    !
    ! Calculating streamwise x and r coordinates
    !
    do ia = 1,na

        ! For blade section
        do i = 1,iap

            xb(i,ia)    = spl_eval(nsp(ia),mps(i,ia),xm(1,ia),xms(1,ia),mp(1,ia),.false.)
            rb(i,ia)    = spl_eval(nsp(ia),mps(i,ia),rm(1,ia),rms(1,ia),mp(1,ia),.false.)


            !
            ! Compute derivatives of (x,r) wrt input parameters
            !
            ! Sweep
            call spl_eval_ders (nsp(ia), mps(i,ia), xm(1,ia), xms(1,ia), mp(1,ia), &
                                dm3D_dsweep(ia,i,:,:), dx_dsweep(ia,i,:,:))
            call spl_eval_ders (nsp(ia), mps(i,ia), rm(1,ia), rms(1,ia), mp(1,ia), &
                                dm3D_dsweep(ia,i,:,:), dr_dsweep(ia,i,:,:))

            ! in_beta*
            call spl_eval_ders (nsp(ia), mps(i,ia), xm(1,ia), xms(1,ia), mp(1,ia), &
                                dm3D_dinbeta(ia,i,:,:), dx_dinbeta(ia,i,:,:))
            call spl_eval_ders (nsp(ia), mps(i,ia), rm(1,ia), rms(1,ia), mp(1,ia), &
                                dm3D_dinbeta(ia,i,:,:), dr_dinbeta(ia,i,:,:))

            ! out_beta*
            call spl_eval_ders (nsp(ia), mps(i,ia), xm(1,ia), xms(1,ia), mp(1,ia), &
                                dm3D_doutbeta(ia,i,:,:), dx_doutbeta(ia,i,:,:))
            call spl_eval_ders (nsp(ia), mps(i,ia), rm(1,ia), rms(1,ia), mp(1,ia), &
                                dm3D_doutbeta(ia,i,:,:), dr_doutbeta(ia,i,:,:))

            ! Chord multiplier
            call spl_eval_ders (nsp(ia), mps(i,ia), xm(1,ia), xms(1,ia), mp(1,ia), &
                                dm3D_dcm(ia,i,:,:), dx_dcm(ia,i,:,:))
            call spl_eval_ders (nsp(ia), mps(i,ia), rm(1,ia), rms(1,ia), mp(1,ia), &
                                dm3D_dcm(ia,i,:,:), dr_dcm(ia,i,:,:))

            ! Mean-line 2nd derivative
            call spl_eval_ders (nsp(ia), mps(i,ia), xm(1,ia), xms(1,ia), mp(1,ia), &
                                dm3D_dcurv(ia,i,:,:), dx_dcurv(ia,i,:,:))
            call spl_eval_ders (nsp(ia), mps(i,ia), rm(1,ia), rms(1,ia), mp(1,ia), &
                                dm3D_dcurv(ia,i,:,:), dr_dcurv(ia,i,:,:))

            ! NACA thickness
            call spl_eval_ders (nsp(ia), mps(i,ia), xm(1,ia), xms(1,ia), mp(1,ia), &
                                dm3D_dthk(ia,i,:,:), dx_dthk(ia,i,:,:))
            call spl_eval_ders (nsp(ia), mps(i,ia), rm(1,ia), rms(1,ia), mp(1,ia), &
                                dm3D_dthk(ia,i,:,:), dr_dthk(ia,i,:,:))

        end do  ! i = 1, iap

        ! For extended meanlines
        do i = 1, nmeanline
           xem(ia,i)    = spl_eval(nsp(ia), mp3D_meanline(ia,i), xm(1,ia), xms(1,ia), mp(1,ia), .false.)
           rem(ia,i)    = spl_eval(nsp(ia), mp3D_meanline(ia,i), rm(1,ia), rms(1,ia), mp(1,ia), .false.)
        end do

        ! For maximum thickness points
        if (thick_distr == 5) then
            xrt_umax(ia,1)  = spl_eval(nsp(ia), mt_umax(ia,1), xm(1,ia), xms(1,ia), mp(1,ia), .false.)
            xrt_umax(ia,2)  = spl_eval(nsp(ia), mt_umax(ia,1), rm(1,ia), rms(1,ia), mp(1,ia), .false.)
            xrt_umax(ia,4)  = spl_eval(nsp(ia), mt_umax(ia,3), xm(1,ia), xms(1,ia), mp(1,ia), .false.)
            xrt_umax(ia,5)  = spl_eval(nsp(ia), mt_umax(ia,3), rm(1,ia), rms(1,ia), mp(1,ia), .false.)
        end if

    end do


    
    !
    ! Get the throat intersection points mapping (xb,rb) values
    !
    do ia = 1,na
        do i = 1,6   
            mps_inter = intersec_coord(2*i-1,ia) + dmp(ia) + delmp(ia)
            inter_xb(i,ia) = spl_eval(nsp(ia),mps_inter,xm(1,ia),xms(1,ia),mp(1,ia),.false.)
            inter_rb(i,ia) = spl_eval(nsp(ia),mps_inter,rm(1,ia),rms(1,ia),mp(1,ia),.false.)
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
            xb(i,ia)        = xb(i,ia)
            !end if
            yb(i,ia)        = rb(i,ia)*sin(ya(i,ia) + delta_theta(ia))
            zb(i,ia)        = rb(i,ia)*cos(ya(i,ia) + delta_theta(ia))


            !
            ! Compute (y, z) derivatives wrt all parameter control points
            !
            ! Sweep
            !
            ! TODO: Include effect of stagger and delta_theta on delmp ders
            !       when using true sweep/true lean options
            !
            call yz_sweep_ders (ya(i, ia), delta_theta(ia), dr_dsweep(ia, i, :, :), &
                                dy_dsweep(ia, i, :, :), dz_dsweep(ia, i, :, :))

            !
            ! Lean
            !
            ! TODO: Include effect of stagger and delmp on delta_theta ders
            !       when using true lean/true sweep options
            !
            dx_dlean        = 0.0
            call yz_lean_ders (rb(i, ia), ya(i, ia), delta_theta(ia), delta_theta_xcp_ders(ia, :), &
                               delta_theta_ycp_ders(ia, :), dy_dlean(ia, i, :, :), dz_dlean(ia, i, :, :))

            ! in_beta*
            call yz_param_ders (rb(i, ia), ya(i, ia), delta_theta(ia), dr_dinbeta(ia, i, :, :), &
                                dth_dinbeta(ia, i, :, :), dy_dinbeta(ia, i, :, :), dz_dinbeta(ia, i, :, :))

            ! out_beta*
            call yz_param_ders (rb(i, ia), ya(i, ia), delta_theta(ia), dr_doutbeta(ia, i, :, :), &
                                dth_doutbeta(ia, i, :, :), dy_doutbeta(ia, i, :, :), dz_doutbeta(ia, i, :, :))

            ! chord multiplier
            call yz_param_ders (rb(i, ia), ya(i, ia), delta_theta(ia), dr_dcm(ia, i, :, :), &
                                dth_dcm(ia, i, :, :), dy_dcm(ia, i, :, :), dz_dcm(ia, i, :, :))

            ! mean-line 2nd derivative
            call yz_param_ders (rb(i, ia), ya(i, ia), delta_theta(ia), dr_dcurv(ia, i, :, :), &
                                dth_dcurv(ia, i, :, :), dy_dcurv(ia, i, :, :), dz_dcurv(ia, i, :, :))

            ! NACA thickness
            call yz_param_ders (rb(i, ia), ya(i, ia), delta_theta(ia), dr_dthk(ia, i, :, :), &
                                dth_dthk(ia, i, :, :), dy_dthk(ia, i, :, :), dz_dthk(ia, i, :, :))

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



    ! Write derivatives of all (x, y, z) spanwise sections
    ! TODO: Add if-else conditions
    call write_xyz_der_files (1, dx_dsweep, dy_dsweep, dz_dsweep)
    call write_xyz_der_files (2, dx_dlean, dy_dlean, dz_dlean)
    call write_xyz_der_files (3, dx_dinbeta, dy_dinbeta, dz_dinbeta)
    call write_xyz_der_files (4, dx_doutbeta, dy_doutbeta, dz_doutbeta)
    call write_xyz_der_files (5, dx_dcm, dy_dcm, dz_dcm)
    call write_xyz_der_files (6, dx_dcurv, dy_dcurv, dz_dcurv)
    call write_xyz_der_files (7, dx_dthk, dy_dthk, dz_dthk)



    ! format statements
    10 format(3(f25.16,1x))
    return


end subroutine bladestack





















