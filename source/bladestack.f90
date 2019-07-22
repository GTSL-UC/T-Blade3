subroutine bladestack(nspn,X_le,X_te,R_le,R_te,nsec,scf,msle,np,stack,               &
                    cpdeltam,spanmp,xcpdelm,cpdeltheta,spantheta,xcpdeltheta,             &
                    cpinbeta,spaninbeta,xcpinbeta,cpoutbeta,spanoutbeta,xcpoutbeta,       &
                    xm,rm,xms,rms,mp,nsp,bladedata,amount_data,intersec_coord,    &
                    throat_3D,mouth_3D,exit_3D,casename,nbls,LE,axchrd,mble,              &
                    mbte,units,stagger,chrdsweep,chrdlean,axial_LE,radial_LE,thick_distr, &
                    x_in,y_in,xbi,ybi,zbi,from_gridgen)

!******************************************************************************************
! subroutine bladestack: Creates Mapping of streamlines to 2D airfoils and generates...
!                        ... 3D airfoils. Adds lean/sweep and performs...
!                        ... stacking of 3D airfoils.
!
! inputs: 2D airfoils (m',theta); streamlines (m',xm,rm); lean and sweep values; scale.
! output: sec#.dat, sec#.csv files for 3D blade dimensional coordinates.
!         meanline.casename.dat : meanline coordinates.
!         top_periodic.sec.dat : top periodic wall coordinates.
!         bot_periodic.sec.dat : bot periodic wall coordinates.
!******************************************************************************************
! CODE STRUCTURE : ORDER OF OPERATION
!******************************************************************************************
! 01. Start subroutine with variable declaration, constants and allocaltion.

! 02. Read in m',theta airfoil coordinates.

! 03. SPAN calculation for different flow types.

! 04. SWEEP DEFINITION using control points for splining delta_m.

! 05. LEAN DEFINITION using control points for splining detal_theta.

! 06. Calculating the streamwise m'3D values for mapping of airfoils and streamlines.

! 07. Calculating streamwise x3D and r3D coordinates.

! 08. Converting cylindrical coordinates to cartesian coordinates.

! 09. Calculation of the 3D Throat length.

! 10. Dimensional Chord and writing dimensional coordinates into a single file 'blade3D.casename.dat'.

! 11. Calculating the meanline by taking the average of PS and SS curves.

! 12. Deallocation of variables.

! 13. FORMAT statements.

! 14. End Subroutine.
!******************************************************************************************

!******************************************************************************************
! Variables Declaration.
!******************************************************************************************
use file_operations
implicit none

character*80 fname1
character*20 temp
character*32 casename
character(len=2) :: units

integer na,i,ia,iap,stack
integer k,ile
integer nsec
integer nspn,nrow,nspan,nbls
integer nx,nax,nbx,nby,nxx,LE
integer cpdeltam,cpdeltheta,cpinbeta,cpoutbeta,uplmt,thick_distr

parameter (nspan=200,nx=500,nxx=1000,nax=50,nbx=500,nby=100,nrow=1)

integer nap(nspan),nsp(nspan),i_slope,ncp1
integer amount_data,chrdsweep,chrdlean

real chord_actual(100)
real mps(nxx,nax)
real scf,demp
real spl_eval,stagger(nspan)
real, allocatable, dimension(:,:) :: xa,ya,xb,yb,zb,rb
real xm(nx,nax),rm(nx,nax),mp(nx,nax)
real xms(nx,nax),rms(nx,nax)
real xxa,yya,pi,dtor
real X_le(nspan),X_te(nspan),msle(nspan),dmp(nspan)
real R_le(nspan),R_te(nspan)
real mp_stack(nspan)
real xm_slope,rm_slope
real bladedata(amount_data,nspn)
real intersec_coord(12,nspn),mps_inter
real inter_xb(6,nspn),inter_rb(6,nspn),inter_yb(6,nspn),inter_zb(6,nspn)
real throat_3D(nspn),mouth_3D(nspn),exit_3D(nspn)
real lref
real*8 delmp(nspan)
real*8 span(nspan),mble(nspan),mbte(nspan),axchrd(nspan),stingl(nspan)
real mpxc(nspan)
real*8 delta_theta(nspan)
real*8 y_spl_end(nx)
real*8 xcpdelm(100),xcpdeltheta(100),xcpinbeta(100),xcpoutbeta(100)
real*8 spanmp(100),spaninbeta(100),spanoutbeta(100)
real*8 spantheta(100), xbs(nx), ybs(nx)
real*8 xc(nx),yc(nx)
real*8, allocatable, dimension(:,:):: xposlean,yposlean,zposlean
real*8, allocatable, dimension(:,:):: xneglean,yneglean,zneglean

real,   intent(inout)                   :: xbi(nspn,np), ybi(nspn,np), zbi(nspn,np)
real,   intent(inout)                   :: x_in(np,nspn), y_in(np,nspn)

integer np,nspline
logical axial_LE,radial_LE
logical,    intent(in)                  :: from_gridgen
integer                                 :: nopen
character(:),   allocatable             :: log_file
logical                                 :: file_open, isquiet
common / BladeSectionPoints /xxa(nxx,nax),yya(nxx,nax)

! Initialize ile
ile = 0

! Get the value of isquiet
call get_quiet_status(isquiet)

! -----------------------------------------------------------------------------
! --------Constants---------( Pi value)----------------------------------------
pi   = 4.*atan(1.0)
dtor = pi/180.
! -----------------------------------------------------------------------------

!*******************************************************************************************
! Allocation of variables
!*******************************************************************************************
if (allocated(xa)) deallocate(xa)
if (allocated(ya)) deallocate(ya)
if (allocated(xb)) deallocate(xb)
if (allocated(yb)) deallocate(yb)
if (allocated(zb)) deallocate(zb)
if (allocated(rb)) deallocate(rb)
allocate(xa(np,nspn),ya(np,nspn))
allocate(xb(np,nspn),yb(np,nspn),zb(np,nspn),rb(np,nspn))

!  print*,xcg
!  print*,ycg
! do ia = 1,nspn
   ! ycg(ia) = ycg(ia)
   ! !print*,'chord in bladestack',chrd(ia)
! enddo

!*******************************************************************************************
! Sting LE option: m' Offset for obtaining the chord
!*******************************************************************************************
! Subtract this from the x coordinates for this option.
if(LE.eq.2)then
  do ia = 1, nspn
    stingl(ia) = mbte(ia) - mble(ia) - axchrd(ia)
  enddo
endif

!*******************************************************************************************
! Read in m',theta airfoil coordinates 
! For more stream files (if defined) to be read increase na upper bound
!*******************************************************************************************
na = nspn
call log_file_exists(log_file, nopen, file_open)
if (.not. isquiet) print*,'Number of airfoil coordinates:',np
write(nopen,*) 'Number of airfoil coordinates:', np
do ia = 1, na
   nap(ia) = np!199
   iap = nap(ia)
   do i = 1, iap
      xa(i,ia) = x_in(i,ia)!xxa(i,ia)! airfoil coordinates (m')
      ya(i,ia) = y_in(i,ia)!yya(i,ia)! airfoil coordinates (theta)
   enddo         
enddo
!do i = 1,iap
!    print *, xa(i,1), ya(i,1)
!end do
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
enddo
if (.not. isquiet) write(*,*) 

!*******************************************************************************************
! SPAN calculation
!*******************************************************************************************
!--------------------------------------------------------------------
! Checking if r_slope > x_slope for non-axial machines
!--------------------------------------------------------------------
do ia = 1,na
   i_slope = 0
   do i = 1,nsp(ia)
      xm_slope = abs(xms(i,ia))
      rm_slope = abs(rms(i,ia))
      if(rm_slope.ge.xm_slope.and.i_slope.eq.0) i_slope = i
   enddo
   !write(*,*)
   if (.not. isquiet) print*,'i_slope',i_slope
   write(nopen,*) 'i_slope', i_slope
enddo
call close_log_file(nopen, file_open)

!--------------------------------------------------------------------
! Span calculation for axial and non axial machines.
! For Radial flow span is difference in x coordinates.
!--------------------------------------------------------------------
do ia = 1,na
   if(i_slope.eq.0)then  ! purely axial flow
   
     !---Evaluating span  for an axial blade
     lref = abs(R_le(na) - R_le(1))
     span(ia) = abs(R_le(ia) - R_le(1))/lref
     !print*,'span:',span(ia)
     
   elseif(i_slope.ne.0)then ! non-axial flow
   
    if(axial_LE)then ! axial flow at LE
     
       !---Evaluating span  for an axial blade
       lref = abs(R_le(na) - R_le(1))
       span(ia) = abs(R_le(ia) - R_le(1))/lref
       !print*,'span:',span(ia)
       
    elseif(radial_LE)then! non-axial flow at LE 
     
       call spl_inv(mpxc(ia),R_le(1),rm(1,ia),rms(1,ia),mp(1,ia),nsp(ia))
       !---Evaluating span for a non-axial blade
       lref = abs(X_le(na) - X_le(1))
       span(ia) = abs(X_le(ia) - X_le(1))/lref
       !print*,'span:',span(ia)
       
    endif ! end if for flow type at LE
     
   endif ! end if for flow type for the machine   
enddo
!-------------------------------------------------------------------- 
! Splining SWEEP control points
!-------------------------------------------------------------------- 
call cubicspline(xcpdelm,spanmp,cpdeltam,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
call cubicbspline_intersec(y_spl_end,xc,yc,ncp1,span,delmp,na,xbs,ybs)
!-------------------------------------------------------------------- 
! Splining LEAN control points
!-------------------------------------------------------------------- 
call cubicspline(xcpdeltheta,spantheta,cpdeltheta,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
call cubicbspline_intersec(y_spl_end,xc,yc,ncp1,span,delta_theta,na,xbs,ybs)
!-------------------------------------------------------------------- 

!*******************************************************************************************
! SWEEP DEFINITION
!-Control points for delta m ----
!******************************************************************************************* 

! write(*,*)
! print*,'Stagger in bladestack: ',stagger(1:na)
call log_file_exists(log_file, nopen, file_open)
write(nopen,*) ''
if(chrdsweep.eq.1)then ! sweep along the blade chord; true sweep
  if (.not. isquiet) write(*,*)'span          true_sweep'
  write(nopen,*)'span          true_sweep'
  do ia = 1, na
     delmp(ia) = (delmp(ia)*abs(cos(stagger(ia))))! + (delmp(ia)*abs(sin(stagger(ia))))
     if (.not. isquiet) print*,span(ia),delmp(ia)
    write(nopen,*) span(ia), delmp(ia)
  enddo 
elseif(chrdlean.eq.1)then ! true lean
  if (.not. isquiet) write(*,*)'span          delta_m for true_lean'
  write(nopen,*)'span          delta_m for true_lean'
  do ia = 1, na
     delmp(ia) = (delta_theta(ia)*abs(sin(stagger(ia))))
     if (.not. isquiet) print*,span(ia),delmp(ia)
     write(nopen,*) span(ia), delmp(ia)
  enddo
else !axial sweep
  if (.not. isquiet) write(*,*)'span          delta_m'
  write(nopen,*) 'span          delta_m'
  do ia = 1, na       
     if (.not. isquiet) print*,span(ia),delmp(ia)
     write(nopen,*) span(ia), delmp(ia)
  enddo 
endif 
do ia = 1, na
   bladedata(7,ia)= delmp(ia)
enddo
call close_log_file(nopen, file_open)

!*******************************************************************************************
! LEAN DEFINITION 
!-Control points for delta theta----
!******************************************************************************************* 

call log_file_exists(log_file, nopen, file_open)
if (.not. isquiet) write(*,*)
write(nopen,*) ''
if(chrdlean.eq.1)then ! lean normal to the blade chord; true lean.
  if (.not. isquiet) write(*,*)'span         true_lean'
  write(nopen,*)'span         true_lean'
  do ia = 1, na
     delta_theta(ia) = (delta_theta(ia)*abs(cos(stagger(ia))))! + (delta_theta(ia)*abs(sin(stagger(ia))))
     if (.not. isquiet) print*,span(ia),delta_theta(ia)
     write(nopen,*) span(ia), delta_theta(ia)
  enddo
elseif(chrdsweep.eq.1)then ! true sweep
  if (.not. isquiet) write(*,*)'span         delta_theta for true_sweep'
  write(nopen,*)'span         delta_theta for true_sweep'
  do ia = 1, na
     delta_theta(ia) = (delmp(ia)*abs(sin(stagger(ia))))
     if (.not. isquiet) print*,span(ia),delta_theta(ia)
     write(nopen,*) span(ia), delta_theta(ia)
  enddo
else ! tangential lean
  if (.not. isquiet) write(*,*)'span         delta_theta'
  write(nopen,*)'span         delta_theta'
  do ia = 1, na      
     if (.not. isquiet) print*,span(ia),delta_theta(ia)
     write(nopen,*) span(ia), delta_theta(ia)
  enddo
endif
do ia = 1, na
   bladedata(8,ia)= delta_theta(ia)
enddo
if (.not. isquiet) write(*,*)
write(nopen,*) ''
call close_log_file(nopen, file_open)

!*******************************************************************************************
! Calculating the streamwise m' values for mapping of airfoils and streamlines ----
!*******************************************************************************************
!--- mp_stack(ia) = msle(ia) + (real(stk_u)/100)*chrd(ia)
!---Offset calculation for each section: m'_strm and m'_blade-----
!---delm' = m's_stack - m'b_stack-----
!---m's_stack = m's_LE + (%chordstack)*chord-----
!---chrd = m's_TE - m's_LE
!---m'_3D = m'_blade + m'_offset + delm'_ctrlpoints@LE + m'_ctrlpoints@LE
!-------------------------------------------------------------------- 
!-------------------------------------------------------------------- 
do ia = 1,na
   write(temp,*)ia
   ile = (iap+1)/2 ! blade LE index
   mp_stack(ia) = msle(ia) !+ (real(stk_u)/100)*chord(ia)
   dmp(ia) = mp_stack(ia) - xa(ile,ia)
   !print*,'chord',chrd(ia)
   do i = 1,iap   
      demp = delmp(ia)
      mps(i,ia) = xa(i,ia) + dmp(ia) + delmp(ia) !! no need of >>+ mpxc(ia)! 12/18/12 4/1/13
      ! write(50,*)mps(i,ia),ya(i,ia)
   enddo
   ! close(50)
enddo 

!*******************************************************************************************
! Calculating streamwise x and r coordinates-------
!*******************************************************************************************
do ia = 1,na
   do i = 1,iap
      xb(i,ia) = spl_eval(mps(i,ia),xm(1,ia),xms(1,ia),mp(1,ia),nsp(ia))      
      rb(i,ia) = spl_eval(mps(i,ia),rm(1,ia),rms(1,ia),mp(1,ia),nsp(ia))
   enddo 
enddo
! get the throat intersection points mapping xb,rb values: Nemnem 9 16 2013
do ia = 1,na
   do i = 1,6   ! for LE, TE ... ,x value
      mps_inter = intersec_coord(2*i-1,ia) + dmp(ia) + delmp(ia)
      inter_xb(i,ia) = spl_eval(mps_inter,xm(1,ia),xms(1,ia),mp(1,ia),nsp(ia))
      inter_rb(i,ia) = spl_eval(mps_inter,rm(1,ia),rms(1,ia),mp(1,ia),nsp(ia))
   enddo
enddo
if (.not. isquiet) print*,''

!*******************************************************************************************
!---Converting cylindrical coordinates to cartesian coordinates
!*******************************************************************************************
!---------  x = x           ----------------------------
!---------  y = r sin(theta + delta(theta)) --------------
!---------  z = r cos(theta + delta(theta))--------------
!*******************************************************************************************
if (allocated(xposlean)) deallocate(xposlean)
if (allocated(yposlean)) deallocate(yposlean)
if (allocated(zposlean)) deallocate(zposlean)
if (allocated(xneglean)) deallocate(xneglean)
if (allocated(yneglean)) deallocate(yneglean)
if (allocated(zneglean)) deallocate(zneglean)
allocate(xposlean(iap,na),yposlean(iap,na),zposlean(iap,na))
allocate(xneglean(iap,na),yneglean(iap,na),zneglean(iap,na))

do ia = 1,na        ! number of stream lines
   do i = 1,iap     ! number of points
      if(LE.eq.2)then
        xb(i,ia) = xb(i,ia) - stingl(ia)
      else
        xb(i,ia) = xb(i,ia)
      endif
      yb(i,ia) = rb(i,ia)*sin(ya(i,ia) + delta_theta(ia)) 
      zb(i,ia) = rb(i,ia)*cos(ya(i,ia) + delta_theta(ia)) 
      ! Coordinates for the half pitch leaned blade for the periodic walls
      ! positive half pitch leaned coordinates 
      xposlean(i,ia) = xb(i,ia) 
      yposlean(i,ia) = rb(i,ia)*sin(ya(i,ia) + (pi/nbls)) 
      zposlean(i,ia) = rb(i,ia)*cos(ya(i,ia) + (pi/nbls)) 
      ! negative half pitch leaned coordinates 
      xneglean(i,ia) = xb(i,ia) 
      yneglean(i,ia) = rb(i,ia)*sin(ya(i,ia) - (pi/nbls)) 
      zneglean(i,ia) = rb(i,ia)*cos(ya(i,ia) - (pi/nbls)) 
   enddo
   ! get x-y-z for the intersection point: Nemnem 9 16 2013
   do k = 1 ,6      ! LE, TE ... values
      if(LE.eq.2)then
        inter_xb(k,ia) = inter_xb(k,ia) - stingl(ia)
      else
        inter_xb(k,ia) = inter_xb(k,ia)
      endif
        inter_yb(k,ia) = inter_rb(k,ia)*sin(intersec_coord(2*k,ia) + delta_theta(ia))
        inter_zb(k,ia) = inter_rb(k,ia)*cos(intersec_coord(2*k,ia) + delta_theta(ia))
   enddo
enddo

!*******************************************************************************************
! Calculation of the 3D Throat length:  Nemnem 9 17 2013
!*******************************************************************************************
call log_file_exists(log_file, nopen, file_open)
do ia = 1,na
   throat_3D(ia) = sqrt((inter_xb(1,ia)-inter_xb(2,ia))**2+&
                (inter_yb(1,ia)-inter_yb(2,ia))**2+ &
        (inter_zb(1,ia)-inter_zb(2,ia))**2)! 3D throat
   mouth_3D(ia) = sqrt((inter_xb(3,ia)-inter_xb(4,ia))**2+&
                (inter_yb(3,ia)-inter_yb(4,ia))**2+ &
        (inter_zb(3,ia)-inter_zb(4,ia))**2)
   exit_3D(ia) = sqrt((inter_xb(5,ia)-inter_xb(6,ia))**2+&
                (inter_yb(5,ia)-inter_yb(6,ia))**2+ &
        (inter_zb(5,ia)-inter_zb(6,ia))**2)
   if(throat_3D(ia).ne.0) then 
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
   endif
enddo

!*******************************************************************************************
! Dimensional Chord calculation and writing dimensional coordinates into a single file 'blade3D.casename.dat'.
!*******************************************************************************************
if (.not. isquiet) write(*,*)"Number of radial sections:",nsec
write(nopen,*) 'Number of radial sections:', nsec
!---- output ...
! TODO: Move to file_operations
fname1 = 'blade3d.'//trim(casename)//'.dat'
if (.not. from_gridgen) open(3,file=fname1,status='unknown')
if (.not. isquiet) then
    write(*,*)
    write(*,*) 'Writing 3D blade geometry ...'
    write(*,*)
end if
write(nopen,*) ''
write(nopen,*) 'Writing 3D blade geometry ...'
write(nopen,*) ''
if (.not. from_gridgen) write(3,*) iap,nsec
!scaled output

! Store blade (x,y,z) coordinates
xbi = scf*transpose(xb)
ybi = scf*transpose(yb)
zbi = scf*transpose(zb)

do ia = 1,nsec
   do i = 1,iap
        if (.not. from_gridgen) write(3,10) scf*xb(i,ia),scf*yb(i,ia),scf*zb(i,ia)       
   enddo
   chord_actual(ia) = scf*sqrt((xb(ile,ia)-xb(iap,ia))**2 + (yb(ile,ia)-yb(iap,ia))**2 + (zb(ile,ia)-zb(iap,ia))**2)
   if (.not. isquiet) print*,'chord_actual(',units,'):',chord_actual(ia)
   write(nopen,*) 'chord_actual(',units,'):',chord_actual(ia)
   bladedata(6,ia)= chord_actual(ia) ! in input file units
enddo
if (.not. from_gridgen) close(3)

!*******************************************************************************************
! Calculating the meanline by taking the average of PS and SS curves.
!*******************************************************************************************
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
uplmt = ((iap+1)/2)-1 !uplmt+1 is 100 for 199 as iap.

! call meanline3DNperiodicwall(xb,yb,zb,xposlean,yposlean,zposlean, &
                                   ! xneglean,yneglean,zneglean,iap,nsec, &
                                   ! uplmt,scf,casename)
if (.not. from_gridgen) &
    call constantslopemeanline3D(xb,yb,zb,xposlean,yposlean,zposlean, &
                                   xneglean,yneglean,zneglean,iap,nsec, &
                                   uplmt,scf,casename)

!*******************************************************************************************
! Deallocation of variables
!*******************************************************************************************                              
deallocate(xposlean,yposlean,zposlean,xneglean,yneglean,zneglean)

!*******************************************************************************************
! FORMAT statements
!*******************************************************************************************
10 format(3(f25.16,1x))
!11 format((f20.16),A,(f20.16),A,(f20.16))
return
end subroutine bladestack

!*******************************************************************************************
!*******************************************************************************************
!*******************************************************************************************


