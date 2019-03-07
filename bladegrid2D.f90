!***************************************************
subroutine bladegrid2D(xb,yb,np,nbls,chrd,thkc,fext,LE,le_pos,thick_distr, &
                       casename,msle,mste,mble,mbte,js,nspn,np_side, &
                       curv_camber,stingl,jcellblade,etawidth,BGgrid,develop,isdev)
!---------------------------------------------------------------------------                       
! creates a grid for the background and the blade in 2D.
! writes a plot3d file
! Input:
!  xblade,yblade coordinates, chord, mble,mbte,msle
!  etawidth: height of the eta grid for the blade grid
!  BGgrid: switch whether to create a background grid or not
!  jcellblade: number of high order cells in the blade grid (4n+1)
!  LE : LE switch can be 0 = elliptical, 1 = spline, 2 = sting
!  thick_dist: can be 0 = wennerstrom, 1 = spline with blunt TE, 2 = spline with sharp TE        
!---------------------------------------------------------------------------
!******************************************************************************************
! CODE STRUCTURE : ORDER OF OPERATION
!******************************************************************************************
! 01. Start subroutine with variable declarations and Constants.

! 02. Calculation of Pitch and max index of the meanline array.

! 03. Translating the blade in m' direction to map to the streamline.

! 04. Condition to skip creating background meshes.

! 05. Creating the BACKGROUND grid points and logic option for curved mesh.
      ! BGgrid = 2 is for curved background mesh.
      
! 06. Creates linear or curved background meanline.

! 07. Fills up n points above and below the meanline to create imax x jmax grid.

! 08. Stores it in the variables to be written to the grid file.

! 09. Creating the INFLATED BLADE grid points.
      !a) O or C grid for the blade
      !b) if LE == 2 then sting LE blade grid (C-grid)
      !c) Offsetting the blade coordinates to fill in eta direction
          ! Currently uniformly spaced cells in eta direction
          ! Options are hyperbolic, simlpe stretching hard coded.
      !d) C grid blades if thick_dist==2
          ! C grid mesh refinement   
          
!*10. LE mesh refinement for spline LE blade grids (* INCOMPLETE)

! 11. Blade grid translation if it is outside the background grid.

! 12. Elliptical grid smoothing for the background grids
      !a) Smoothens the boundary grid lines before smoothing the inner points.
      
! 13. Writing the grid coordinates to a plot2D file (grid.x).   
 
! 14. Deallocation of variables.

! 15. End Subroutine.  
!******************************************************************************************

!******************************************************************************************
! Variables Declaration.
!******************************************************************************************
use file_operations
use gridvar
implicit none
integer i,j,k,t,np,nbls,uplmt,j1,j2,cgrid,js,nspn
integer BGgrid,np_side,le_pos,ximax,m
integer imax,jmax,ngrid,imax1,jmax1,LE,imax2,imax_org,imaxnew
integer curv_camber
real*8 x1,y1,x2,y2,y3
real*8, dimension(np) :: xb,yb
real*8, allocatable, dimension(:):: xrot,yrot
real*8, allocatable, dimension(:,:) :: Xg,Yg,Xx,Yy,Xx1,Yy1,Xbg,Ybg,Xbg2,Ybg2
real*8, allocatable, dimension(:,:) :: Xg1,Yg1,Xg11,Yg11
real*8, allocatable, dimension(:) :: cluster,phi
real*8, allocatable, dimension(:) :: xblade,yblade,xbnew,ybnew,xbase,ybase
real*8 deltac,distance
real*8 dtor
real*8 d_offset,increment,numoffset
real*8 pitch,pi,chrd

real*8 cellwidth,xte_norm
real*8 subdivide2D
real*8 yte_norm,te_norm_mag,msle,mste,mble,mbte
real*8 jcellblade,err

real*8 thkc,thick_distr,etawidth,stingl

logical curvemesh, ellipsmooth, translateUp, translateDown, isdev
integer                                     :: nopen
character(:),   allocatable                 :: log_file
logical                                     :: file_open

character*32 fname1,fname2,fext,temp,casename,develop


!Constants
pi = 4.*atan(1.0)
dtor = PI/180.
err = 0.000001
!
call log_file_exists(log_file, nopen, file_open)
print*,'Creating the grid...'
write(nopen,*) 'Creating the grid...'
if (allocated(xblade)) deallocate(xblade)
if (allocated(yblade)) deallocate(yblade)
allocate(xblade(np),yblade(np))
do i = 1, np
 xblade(i) = xb(i)
 yblade(i) = yb(i)
enddo

!---------------------------------------------------
! Calculation of the pitch between adjacent blades:
!---------------------------------------------------
pitch = 2*pi/nbls 
print*,'pitch: ',pitch
write(*,*)
write(nopen,*) 'pitch: ', pitch
write(nopen,*) ''
call close_log_file(nopen, file_open)
!---------------------------------------------------------------------------------------
!Calculating the meanline coordinates' max index for both even and odd number of points.
!---------------------------------------------------------------------------------------
if(mod(np,2).eq.0)then
 np = np - 1 ! For even no. of points LE = np/2. So, the current formula (np+1)/2 ==> np/2 with this change.
endif
uplmt = (np+1)/2 !for example, uplmt is 100 for 199 as np.
!print*,'mble: ',mble
imax_org = np

!---------------------------------------------------
! Translating the blade in m' direction
!---------------------------------------------------
!print*,'Le before translation: ',xblade(uplmt)
xblade(uplmt) = mble
!if((js.ne.1).or.(js.ne.nspn))then
 do i = 1, np
  xblade(i) = xblade(i) + msle - mble!- xblade(uplmt)
 enddo
!print*,'Le after translation: ',xblade(uplmt)
!endif

!!!call ellipdata(xellip,yellip,xb,yb,np,uplmt)
!*******************************************************
! Cases without background grids
!*******************************************************
if(BGgrid.eq.0)then
 goto 50 ! no background grid needed 
endif

!*******************************************************
!Creating the BACKGROUND grid points.
!*******************************************************
! Number of grids to be writtien to grid file
! 1. Blade grid
! 2. Background grid
ngrid = 2

!---------------------------------------------------
!Option for curved or linear background grid
!---------------------------------------------------
! Curved grid and linear grid are the 2 options
!Logics
if(BGgrid == 2) then ! curved background mesh
 curvemesh = .true.
elseif((BGgrid == 1).or.(BGgrid == 0))then ! linear or no background mesh
 curvemesh = .false.
endif
!
ellipsmooth = .true.
!
if(curvemesh)then

  !---------------------------------------------------
  ! Curved background grid
  !---------------------------------------------------
  call curveBG(imax,jmax,xblade,yblade, &
              uplmt,np,casename,develop,msle,mble,chrd,pitch)
              
else ! linear background grid

  !--------------------------------------------------
  ! Linear background grid
  !--------------------------------------------------
  call linearBG(imax,jmax,xblade,yblade, &
             uplmt,np,casename,develop,msle,mble,chrd,pitch)

endif  ! endif for background grid option to create upstream + meanline + downstream
!-----------------------------------------------------------------

!-----------------------------------------------------------------
! Creating the background grid points by offsetting the gridline obtained.
! gridline = upstream line + meanline (curved or linear) + downstream line
!-----------------------------------------------------------------
if (allocated(Xbg )) deallocate(Xbg )
if (allocated(Ybg )) deallocate(Ybg )
if (allocated(Xbg2)) deallocate(Xbg2)
if (allocated(Ybg2)) deallocate(Ybg2)
Allocate(Xbg(imax,jmax),Ybg(imax,jmax))
Allocate(Xbg2(imax,jmax),Ybg2(imax,jmax))
!-----------------------------------------------------------------
! Offsetting the gridline by increments ABOVE it upto half pitch.
!-----------------------------------------------------------------
 j1 = int((jmax-1)*0.5)
 j2 = j1 + 1
 !
 do j = 1, j1
     do i = 1, imax
        Xbg(i,j)  = xline(i) 
        increment = ((0.5/j1)*j)*pitch 
        Ybg(i,j)  = numoffset(yline(i),increment)
     enddo
 enddo
!-----------------------------------------------------------------
! Offsetting the gridline by increments BELOW it upto half pitch.
!-----------------------------------------------------------------
 do j =1, j1
  do i = 1, imax
   Xbg2(i,j) = xline(i)
   increment = -((0.5/j1)*j)*pitch 
   Ybg2(i,j) = numoffset(yline(i),increment)
  enddo 
 enddo
 call log_file_exists(log_file, nopen, file_open)
 print*,'Filling the background grid points...'
 write(nopen,*) 'Filling the background grid points...'
 call close_log_file(nopen, file_open)
!-----------------------------------------------------------------
!Filling the arrays to the background grid points
!-----------------------------------------------------------------
if (allocated(Xg1)) deallocate(Xg1)
if (allocated(Yg1)) deallocate(Yg1)
ALLOCATE( Xg1(imax,jmax), Yg1(imax,jmax))
do j = 1, j1  ! starting from the bottom offset curve, growing upwards.
  do i = 1, imax
    Xg1(i,j) = Xbg2(i,j1 + 1 - j)
    Yg1(i,j) = Ybg2(i,j1 + 1 - j)
  enddo
enddo
do i = 1, imax
 Xg1(i,j1+1) =  xline(i)
 Yg1(i,j1+1) =  yline(i)
enddo
do j = 1, j1
  do i = 1, imax
    Xg1(i,j1+1+j) = Xbg(i,j)
    Yg1(i,j1+1+j) = Ybg(i,j)
!    write(*,*)Xg1(i,j1+1+j),Yg1(i,j1+1+j)
  enddo 
enddo

!-----------------------------------------------------------------
!writin BG grid for debug
if(isdev)then
    fname1 = 'BGgrid.'//trim(fext)
    call fileWriteMatrix(fname1,Xg1,Yg1,imax,jmax)
    ! open(33,file=fname1,status='unknown')
    ! write(33,*)'skip'
    ! write(33,*)imax,jmax
    ! !do j = 1, jmax
       ! do i = 1, imax
          ! write(33,*)Xg1(i,:)
       ! enddo
    ! print*,""
    ! write(33,*)'Y coordinates'
    ! do i = 1, imax
       ! write(33,*)Yg1(i,:)
    ! enddo
    ! !enddo
    ! close(33)
    ! write(*,*)
endif
!-----------------------------------------------------------------
!
!-----------------------------------------------------------------
! LE sting option
!-----------------------------------------------------------------
!For blade with sting LE the upstream domain should not exist.
! The background grid should not contain j1 upstream grid points.
if(LE.eq.2)then
  imaxnew = imax - j1
  if(curvemesh)then
      if(mod(imaxnew,4).eq.0)then ! Conditions to follow 4n+1 rule for 4n cells
       imaxnew = imaxnew + 1
      elseif(mod(imaxnew,4).eq.1)then
       imaxnew = imaxnew
      elseif(mod(imaxnew,4).eq.2)then
       imaxnew = imaxnew - 1
      elseif(mod(imaxnew,4).eq.3)then
       imaxnew = imaxnew - 2
      endif
  endif
  if (allocated(Xg11)) deallocate(Xg11)
  if (allocated(Yg11)) deallocate(Yg11)
  ALLOCATE( Xg11(imaxnew,jmax), Yg11(imaxnew,jmax))
  do j = 1, jmax
    do i = 1, imaxnew
      Xg11(i,j) = Xg1(j1+i,j)
      Yg11(i,j) = Yg1(j1+i,j)
    enddo    
  enddo
endif
!-----------------------------------------------------------------
!-----------------------------------------------------------------
50 continue ! jumps to here if the Background grid is not needed.
!-----------------------------------------------------------------
!
!*******************************************************
! O & C grid for the blade
!*******************************************************
!-------------------------------------------------------- 
! Creating the INFLATED BLADE grid points
!-------------------------------------------------------- 
call log_file_exists(log_file, nopen, file_open)
if(thick_distr == 2)then
  jcells = 10!8!33
else
  jcells = int(jcellblade)!33
  print*,'jcells: ',jcells
  write(nopen,*) 'jcells: ', jcells
endif
call close_log_file(nopen, file_open)
jmax1 = (4*jcells)+1!33
!-----------------------------------------------------------
! sting LE blade grid (C-grid)
!-----------------------------------------------------------
if(LE.eq.2)then ! LE sting   
    
  call stingLEgrid(xb,yb,np,uplmt,stingl,le_pos,thkc,thick_distr, &
                   mble,msle,mbte,chrd,jmax1,casename,fext,develop,isdev,etawidth)
                    
endif
!-----------------------------------------------------------
!Sting LE grid points calculated above.
imax1 = imax_org
np = imax_org
!
do i = 1, np
   xblade(i) = xb(i)+ msle - mble
   yblade(i) = yb(i)
enddo
! Memory allocation
if (allocated(Xg)) deallocate(Xg)
if (allocated(Yg)) deallocate(Yg)
if (allocated(Xx)) deallocate(Xx)
if (allocated(Yy)) deallocate(Yy)
ALLOCATE( Xg(imax1,jmax1), Yg(imax1,jmax1))
ALLOCATE( Xx(imax1,jmax1), Yy(imax1,jmax1))
!ALLOCATE( Xxrot(imax1,jmax1), Yyrot(imax1,jmax1))
!-------------------------------------------------------- 
! Filling the inner boundary grid points with Blade coordinates
!-------------------------------------------------------- 
if (allocated(xbase)) deallocate(xbase)
if (allocated(ybase)) deallocate(ybase)
allocate(xbase(imax1),ybase(imax1))
do i = 1, imax1
    Xg(i,1) = xblade(imax1+1-i)
    Yg(i,1) = yblade(imax1+1-i)
    Xx(i,1) = xblade(i)
    Yy(i,1) = yblade(i)
    xbase(i) = Xx(i,1)
    ybase(i) = yy(i,1)
enddo
!-------------------------------------------------------- 
! OFFSET coordinates for the blade
!-------------------------------------------------------- 
!Splining the blade coordinates
call arclength(xblade(1),yblade(1),s(1),np)
! Calculating x'(s) and y'(s)
call spline(xblade(1),dxds(1),s(1),np, 999.0, -999.0)
call spline(yblade(1),dyds(1),s(1),np, 999.0, -999.0)
!
d_offset = (thkc)/jmax1! 100% 0f max thk
cellwidth = 2.*chrd/(50)
do k = 1, jmax1-1
   write(temp,*)k
   if(thick_distr == 2)then
    
     offset = 1 - tanh(pi*(1 - real(k)/(jmax1-1)))/tanh(pi) ! hyperbolic stretching
     offset = offset*cellwidth*2. !hyperbolic stretching. Scaling it.
     !---------------------------------
     ! Calculating offset coordinates
     call offset_points(xblade,yblade,xbase,ybase,dxds,dyds,offset, &
                         imax1,k,uplmt,le_pos,LE)
    
   else
      
     offset = d_offset*(real(etawidth)/100)!*0.1!0.08 !uniform spacing in the normal direction for Euler runs
     !offset = (d_offset*k)**2
     ! offset = 1 - tanh((pi/1.)*(1 - real(k)/(jmax1-1)))/tanh(pi/1.) ! hyperbolic stretching
     ! offset = offset*d_offset*0.5!cellwidth*0.25 !hyperbolic stretching. Scaling it.
     !---------------------------------
     ! Calculating offset coordinates
     !print*,'k before offset: ',k
     call offset_points(xblade,yblade,xblade,yblade,dxds,dyds,offset, &
                         imax1,k,uplmt,le_pos,LE)
   endif
   !
   !if TE points do not coincide, an average of the TE points is taken.
   !if(thick_distr == 1)then ! blunt TE
   x1 = xblade(1)
   y1 = yblade(1)
   x2 = xblade(imax1)
   y2 = yblade(imax1)
   if((x1.ne.x2).and.(y1.ne.y2))then
     
     xblade(imax1)  = 0.5*(xblade(imax1) + xblade(1))
     yblade(imax1)  = 0.5*(yblade(imax1) + yblade(1))
     xblade(1) = xblade(imax1)
     yblade(1) = yblade(imax1)
     
   endif 
   !-------------------------------------------------------------------------------------
   ! Filling the arrays clockwise from TE to TE through LE to make the grid RIGHT HANDED.
   !------------------------------------------------------------------------------------- 
   do i = 1, imax1
      Xg(i,k+1) = xblade(imax1+1-i)
      Yg(i,k+1) = yblade(imax1+1-i)
      Xx(i,k+1) = xblade(i)
      Yy(i,k+1) = yblade(i) 
   enddo
enddo
!------------------------------------
!for STING LE case
!------------------------------------
if(LE.eq.2)then
 
  do j = 1, jmax1
     do i = 1, uplmt-le_pos
        Xgstingblade(njoint+i,j) = Xg(uplmt+le_pos-1+i,j)
        Ygstingblade(njoint+i,j) = Yg(uplmt+le_pos-1+i,j)
     enddo
     !
     do i = 2, uplmt-le_pos
        Xgstingblade(njoint+uplmt-le_pos-1+i,j) = Xg(i,j)
        Ygstingblade(njoint+uplmt-le_pos-1+i,j) = Yg(i,j)
     enddo
  enddo  

endif
!
!******************************************************* 
! C grid for the blade
!******************************************************* 
if (allocated(xbnew)) deallocate(xbnew)
if (allocated(ybnew)) deallocate(ybnew)
allocate(xbnew(imax1 + 2*cgrid),ybnew(imax1 + 2*cgrid))
cgrid = 40 ! nodes for the linear grid at the start and end of the C grid.
! New imax for the Grid.
imax2 = imax1 + 2*cgrid
!Creating a simple stretched grid from the start of C grid to TE.
if(thick_distr == 2)then ! sharp TE
  !Calculating unifrom cell width for the linear part of the C grid.
  !  distance = 2*sqrt((Xg(uplmt,1) - Xg(uplmt,jmax1))**2 + (Yg(uplmt,1) - Yg(uplmt,jmax1))**2)
  distance = sqrt((Xx(2,jmax1) - Xx(imax1-1,jmax1))**2 + (Yy(2,jmax1) - Yy(imax1-1,jmax1))**2)
  deltac = 0.5*distance/cgrid
  !  offset = 1 - tanh(pi*(1 - real(k)/(jmax1-1)))/tanh(pi) ! hyperbolic stretching
  !  offset = offset*cellwidth*2.
  xbnew(cgrid) = Xg(1,jmax1)
  ybnew(cgrid) = Yg(1,jmax1)
  do i = 1, imax1
     xbnew(cgrid + i) = Xg(imax1+1-i,1)  ! blade geometry coordinates for C grid.
     ybnew(cgrid + i) = Yg(imax1+1-i,1)  ! blade geometry coordinates for C grid.
  enddo
  !Calculating the normal at the TE as the angle bisection vector...
  !of the 2 normals at 1st and last point of the blade.
  ! The bisecting vector
  xte_norm = (xnorm(1) + xnorm(imax1))*0.5
  yte_norm = (ynorm(1) + ynorm(imax1))*0.5
  te_norm_mag = sqrt((xte_norm)**2 + (yte_norm)**2)
  !  
  do i = cgrid-1,1,-1 ! TE to start of C grid.
   !    deltac = offset*cellwidth*2.
     deltac = 1 - tanh(pi*(1 - real(cgrid-i)/(cgrid)))/tanh(pi) ! hyperbolic stretching
     deltac = deltac*distance*0.05
     !   cluster(cgrid-i) = deltac
     xbnew(i) = xbnew(i+1) + (deltac)*(xte_norm/te_norm_mag)
     ybnew(i) = ybnew(i+1) + (deltac)*(yte_norm/te_norm_mag)
  enddo
  xbnew(cgrid+imax1+1) = Xg(imax1,jmax1)
  ybnew(cgrid+imax1+1) = Yg(imax1,jmax1)
  do i = cgrid+imax1+2, imax2 ! TE to end of C grid.
     deltac = 1 - tanh(pi*(1 - real(i-cgrid-imax1-1)/(cgrid)))/tanh(pi) ! hyperbolic stretching
     deltac = deltac*distance*0.05
     !   cluster(i-cgrid-imax1-1) = deltac
     xbnew(i) = xbnew(i-1) + (deltac)*(xte_norm/te_norm_mag)!**2
     ybnew(i) = ybnew(i-1) + (deltac)*(yte_norm/te_norm_mag)!**2
  enddo
  DEALLOCATE(Xg,Yg) ! deallocating the memory from blunt TE allocation
  ALLOCATE( Xg(imax2,jmax1), Yg(imax2,jmax1))
  if (allocated(Xx1)) deallocate(Xx1)
  if (allocated(Yy1)) deallocate(Yy1)
  ALLOCATE( Xx1(imax2,jmax1), Yy1(imax2,jmax1))
  !
  do i = 1, imax2
     Xg(i,1) = xbnew(imax2+1-i) ! Making it go clockwise
     Yg(i,1) = ybnew(imax2+1-i)
     Xx1(i,1) = xbnew(i)
     Yy1(i,1) = ybnew(i)
  enddo
  !
  !-------------------------------------------------------- 
  ! OFFSET coordinates for the blade
  !-------------------------------------------------------- 
  !Splining the blade coordinates
  call arclength(xbnew(1),ybnew(1),s(1),imax2)
  ! Calculating x'(s) and y'(s)
  call spline(xbnew(1),dxds(1),s(1),imax2, 999.0, -999.0)
  call spline(ybnew(1),dyds(1),s(1),imax2, 999.0, -999.0)
  if (allocated(phi )) deallocate(phi )
  if (allocated(xrot)) deallocate(xrot)
  if (allocated(yrot)) deallocate(yrot)
  Allocate (phi(2),xrot(1),yrot(1))
  do k = 1, jmax1-1
     write(temp,*)k
     offset = 1 - tanh(pi*(1 - real(k)/(jmax1-1)))/tanh(pi) ! hyperbolic stretching
     offset = offset*distance*0.5
     !	offset = k*distance*0.5/jmax1 ! uniform cellwidth

    ! Calculating offset coordinates
    !    call offset_points(xbnew,ybnew,Xx1(1,1),Yy1(1,1),dxds(1),dyds(1),offset,imax2)
    ! Calculating offset coordinates
    !-------------------------------------------------------- 
    ! Calculating the normals and offset coordinates.
    ! Xnorm = y'(s)/ sqrt((y'(s))^2 + (x'(s))^2)
    ! Ynorm = x'(s)/ sqrt((y'(s))^2 + (x'(s))^2)
    !-------------------------------------------------------- 
    do i = 1, imax2
       Dr = ((dyds(i))**2 + (dxds(i))**2)**0.5
       xnorm(i) =  dyds(i)/Dr ! unit normal vector in x.
       ynorm(i) = -dxds(i)/Dr ! unit normal vector in y.
       if(i.eq.cgrid+2)then !2nd point from TE counterclockwise.
         xn1 = xnorm(i)
         yn1 = ynorm(i)
       elseif(i.eq.cgrid+imax1-1)then ! last but one point from TE counterclockwise.
         xn2 = xnorm(i)
         yn2 = ynorm(i)
       endif
       xbnew(i) = Xx1(i,1) + offset*xnorm(i) 
       ybnew(i) = Yy1(i,1) + offset*ynorm(i) 
    enddo  
    ! Refining the TE grid.
    ! 1st grid line with the normal of the 2nd point from TE counterclockwise. 
    xbnew(cgrid+1) = Xx1(cgrid+1,1) + offset*xn1 
    ybnew(cgrid+1) = Yy1(cgrid+1,1) + offset*yn1
    !(np)th grid line with the normal of the (np-1)th point from TE counterclockwise.
    xbnew(cgrid+imax1) = Xx1(cgrid+imax1,1) + offset*xn2 
    ybnew(cgrid+imax1) = Yy1(cgrid+imax1,1) + offset*yn2
    ! Filling the grid coordinates
    do i = 1, imax2
       Xg(i,k+1) = xbnew(imax2+1-i)
       Yg(i,k+1) = ybnew(imax2+1-i)
    enddo
  enddo
  !---------------------------
  !C GRID MESH Refinement :
  !---------------------------
  ! ! Translating the 1st and last eta grid lines in the C grid section
  ! !...on the extruded line from TE.  
  ! Clustering for the C grid in Xi direction
  if (allocated(cluster)) deallocate(cluster)
  Allocate (cluster(cgrid))
  do t = 1,cgrid
     deltac = 1 - tanh((pi/4.0)*(1 - real(t)/(cgrid)))/tanh(pi/4.0) ! hyperbolic stretching
     cluster(t) = deltac*distance*0.025
     !   deltac = distance/cgrid
     !   cluster(t) = deltac*distance*20. ! uniform cell width
  enddo
  !------------------------------------
  !Translating the TOP side grid lines
  !------------------------------------
  do t = 1, cgrid!cgrid+imax1, imax2-1
     do j = 1, jmax1
        Xg(cgrid+imax1+t,j) = Xg(cgrid+imax1+t-1,j) + cluster(t)*(xte_norm/te_norm_mag)
        Yg(cgrid+imax1+t,j) = Yg(cgrid+imax1+t-1,j) + cluster(t)*(yte_norm/te_norm_mag)
     enddo
  enddo  
  !------------------------------------
  !Translating the BOTTOM side grid lines
  !------------------------------------
  do t = cgrid, 1, -1
     do j = 1, jmax1
        Xg(t,j) = Xg(t+1,j) + cluster(cgrid+1-t)*(xte_norm/te_norm_mag)
        Yg(t,j) = Yg(t+1,j) + cluster(cgrid+1-t)*(yte_norm/te_norm_mag)
     enddo
  enddo 
  !-----------------------------------------------------------------
  !writin BG grid for debug
  if(isdev)then
  
    fname1 = 'bladegrid.'//trim(fext)
    call fileWriteMatrix(fname1,Xg,Yg,imax2,jmax1)

  endif
  !-----------------------------------------------------------------
endif ! endif for C grid at TE 

!----------------------------------------------------------------------------
! LE Mesh Refinement for the grid attaching from LE to the point on top curve
!---------------------------------------------------------------------------- 
! The grid points are in clockwise direction from TE to LE to TE.
!
if(LE.eq.1)then ! IF the spline LE is used instead of the default elliptical LE.
  m = uplmt + le_pos
  do k = 2, jmax1
     if(k.ge.3)then
       !Top curve LE attached to the top curve
       ! 2 points after LE attachment to top curve and 6 points before that.
        ! X coordinates
        Xg(m-2,k) = subdivide2D(Xg(m-6,k),Xg(m+2,k),0.50)
        Xg(m-4,k) = subdivide2D(Xg(m-6,k),Xg(m-2,k),0.50)
        Xg(m-5,k) = subdivide2D(Xg(m-6,k),Xg(m-4,k),0.50)
        Xg(m-3,k) = subdivide2D(Xg(m-4,k),Xg(m-2,k),0.50)
        Xg(m  ,k) = subdivide2D(Xg(m-2,k),Xg(m+2,k),0.50)
        Xg(m-1,k) = subdivide2D(Xg(m-2,k),Xg(m  ,k),0.50)
        Xg(m+1,k) = subdivide2D(Xg(m  ,k),Xg(m+2,k),0.50)
        ! Y coordinates
        Yg(m-2,k) = subdivide2D(Yg(m-6,k),Yg(m+2,k),0.50)
        Yg(m-4,k) = subdivide2D(Yg(m-6,k),Yg(m-2,k),0.50)
        Yg(m-5,k) = subdivide2D(Yg(m-6,k),Yg(m-4,k),0.50)
        Yg(m-3,k) = subdivide2D(Yg(m-4,k),Yg(m-2,k),0.50)
        Yg(m  ,k) = subdivide2D(Yg(m-2,k),Yg(m+2,k),0.50)
        Yg(m-1,k) = subdivide2D(Yg(m-2,k),Yg(m  ,k),0.50)
        Yg(m+1,k) = subdivide2D(Yg(m  ,k),Yg(m+2,k),0.50)
       !
       !Bot curve LE attached to the bot curve
       ! Bot curve is adjusted by correcting the normal vectors.
     endif
  enddo
endif
!-----------------------------------------------------------
!
!**************************************************************
!Blade Y grid translation if it is outside the background grid 
!**************************************************************
if((BGgrid.ne.0).and.(LE.ne.2))then

  if(thick_distr == 2)then
    ximax = imax2
  elseif(LE.eq.2)then
    ximax = nsting1 + uplmt-le_pos + uplmt-le_pos-1 + nsting2
  else
    ximax = imax1
  endif
  !  
  do i = 1, ximax
     do j = 1, imax
        y1 = Yg1(j,jmax) ! top background grid
        y3 = Yg1(j,1) ! bottom background grid
        y2 = Yg(i,jmax1) ! outer blade grid boundary
        if(y2 .ge. y1) then 
          translateDown = .true.
        elseif(y2 .le. y3 ) then
          translateUp = .true.
        endif
     enddo
  enddo
  ! Translate the blade grid by 3 cellwidths downwards/upwards.
  if(translateDown)then
    call log_file_exists(log_file, nopen, file_open)
    write(*,*)
    print*,'-----------------------------------------------------------'
    print*,'Grid Error: '
    print*,'-----------------------------------------------------------'
    print*,'Blade grid outside the top background grid !'
    print*,'Grid correction: Translating the blade grid by 3*cellwidth.'
    print*,'-----------------------------------------------------------'
    write(nopen,*) ''
    write(nopen,*)'-----------------------------------------------------------'
    write(nopen,*)'Grid Error: '
    write(nopen,*)'-----------------------------------------------------------'
    write(nopen,*)'Blade grid outside the top background grid !'
    write(nopen,*)'Grid correction: Translating the blade grid by 3*cellwidth.'
    write(nopen,*)'-----------------------------------------------------------'
    call close_log_file(nopen, file_open)
    Yg = Yg - 3*cellwidth  
  elseif(translateUp)then
    call log_file_exists(log_file, nopen, file_open)
    write(*,*)
    print*,'-----------------------------------------------------------'
    print*,'Grid Error: '
    print*,'-----------------------------------------------------------'
    print*,'Blade grid outside the bottom background grid !'
    print*,'Grid correction: Translating the blade grid by 3*cellwidth.'
    print*,'-----------------------------------------------------------'
    write(nopen,*) ''
    write(nopen,*)'-----------------------------------------------------------'
    write(nopen,*) 'Grid Error: '
    write(nopen,*) '-----------------------------------------------------------'
    write(nopen,*) 'Blade grid outside the bottom background grid !'
    write(nopen,*) 'Grid correction: Translating the blade grid by 3*cellwidth.'
    write(nopen,*) '-----------------------------------------------------------'
    call close_log_file(nopen, file_open)
    Yg = Yg + 3*cellwidth  
  endif

endif
!-------------------------------------------------
!Writing Blade grid for debug
!-------------------------------------------------
if((isdev).and.(thick_distr .ne. 2).and.(LE .ne. 2))then
  fname1 = 'bladegrid.'//trim(fext)
  call fileWriteMatrix(fname1,Xg,Yg,imax1,jmax1)
endif
!-----------------------------------------------------------
!
!*******************************************************
!Elliptical grid smoothing
!******************************************************* 
!! Background grid
if(BGgrid.eq.2)then
  call log_file_exists(log_file, nopen, file_open)
  print*,'Smoothing the background grid...'
  write(nopen,*) 'Smoothing the background grid...'
  call close_log_file(nopen, file_open)
  call gauseI2D( imax, jmax, Xg1, Yg1, err, curvemesh, ellipsmooth )
endif
!
!*******************************************************
!Writing the plot3D file containing the grid points 
!******************************************************* 
fname2 = "2Dbladegrid."//trim(adjustl(fext))//".x"
OPEN(unit=2,file=fname2,status='unknown')

call log_file_exists(log_file, nopen, file_open)
write(*,*)
write(*,*)"Writing the 2D grid to a plot3D file..."
write(*,*)
write(nopen,*) ''
write(nopen,*) 'Writing the 2D grid to a plot3D file...'
write(nopen,*) ''
call close_log_file(nopen, file_open)

if(BGgrid.eq.0.)then ! no background grid for splitter blades.

  call log_file_exists(log_file, nopen, file_open)
  print*,trim(casename)
  print*,' Writing the grid file for the splitter blade...'
  write(*,*)
  write(nopen,*) trim(casename)
  write(nopen,*) 'Writing the grid file for the splitter blade...'
  write(nopen,*) ''
  call close_log_file(nopen, file_open)
  write(2,*) 1 ! ngrid=1
  !-------------------------------------------------------- 
  !Writing the inflated blade grid coordinates
  !-------------------------------------------------------- 
  if(thick_distr == 2)then! C grid for sharp TE 
  
    ! Elliptical grid smoothing 
    !call gauseI2D( imax2, jmax1, Xg, Yg, err, curvemesh, ellipsmooth )
    !    
    write(2,*) imax2,jmax1
    do j = 1, jmax1
     do i = 1, imax2
       write(2,*)Xg(i,j) !+ msle - mble ! Writing all X coordinates.
     enddo
    enddo
    !----------------------
    do j = 1, jmax1
     do i = 1, imax2
       write(2,*)Yg(i,j) ! Writing all Y coordinates
     enddo
    enddo
    !----------------------------------------------------------
    
  elseif(LE.eq.2)then ! sting blade case
  
    write(2,*)nsting1 + uplmt-le_pos + uplmt-le_pos-1 + nsting2,jmax1
    print*,nsting1,'+',uplmt-le_pos,'+',uplmt-le_pos-1,'+',nsting2
    print*,''
        ! Writing all X coordinates.
    ! Top Sting
     do j = 1, jmax1
       do i = 1, nsting1!Top sting LE to Blade
        write(2,*)Xgstingblade(i,j) -stingl
        ! write(20,*)Xgstingblade(i,j) -stingl
       enddo
       do i = 1, uplmt-le_pos!Blade top curve
        write(2,*)Xgstingblade(njoint+i,j) -stingl
        ! write(20,*)Xgstingblade(njoint+i,j) -stingl
       enddo
    ! Bottom Sting 
       do i = 2, uplmt-le_pos!, 1, -1!Blade bottom curve to LE bottom sting
        write(2,*)Xg(i,j) -stingl
        ! write(25,*)Xg(i,j) -stingl
       enddo 
       do i = 1, nsting2!, 1 ,-1!Blade bottom curve
        write(2,*)Xgstingdash(i,j) -stingl 
        ! write(25,*)Xgstingdash(i,j) -stingl
       enddo    
    enddo
    !------------------------------------------
    ! Writing all Y coordinates
    ! Top Sting
    do j = 1, jmax1
       do i = 1, nsting1!grid
        write(2,*)Ygstingblade(i,j) 
        ! write(20,*)Ygstingblade(i,j)
       enddo
       do i = 1, uplmt-le_pos!grid
        write(2,*)Ygstingblade(njoint+i,j)
        ! write(20,*)Ygstingblade(njoint+i,j)
       enddo
    ! Bottom Sting 
       do i = 2, uplmt-le_pos!, 1, -1!grid
        write(2,*)Yg(i,j) 
        ! write(25,*)Yg(i,j)
       enddo
       do i = 1, nsting2!, 1, -1!grid
        write(2,*)Ygstingdash(i,j)!Ygstingblade(nstinggrid+1-i,j) 
        ! write(25,*)Ygstingdash(i,j)
       enddo
    enddo 
    !----------------------------------------------
    
  else ! O grid for blunt TE
  
    ! Elliptical grid smoothing 
    if(LE.eq.1)then
      call log_file_exists(log_file, nopen, file_open)
      print*,'Smoothing the background grid...'
      write(nopen,*) 'Smoothing the background grid...'
      call close_log_file(nopen, file_open)
      call gauseI2Dblade( imax1, jmax1, Xg, Yg, err, curvemesh, ellipsmooth, LE )
    endif
    !-------------------------------------------------
    write(2,*) imax1,jmax1
    do j = 1, jmax1
     do i = 1, imax1
       write(2,*)Xg(i,j) !+ msle - mble ! Writing all X coordinates.
     enddo
    enddo
    !----------------------
    do j = 1, jmax1
     do i = 1, imax1
       write(2,*)Yg(i,j) ! Writing all Y coordinates
     enddo
    enddo
    
  endif
  !-------------------------------------------------------- 
  !Writing the inflated blade grid coordinates
  !-------------------------------------------------------- 
  !write(2,*) Xg(:,:),Yg(:,:) 
  !--------------------------------------------------------
  !close(2)
  !  
  
else ! rotor or stator(vane) blades with background grid.

  WRITE(2,*) ngrid
  ! WRITE(20,*) ngrid
  ! WRITE(25,*) ngrid
  if(thick_distr == 2)then! C grid for sharp TE 
  
    write(2,*) imax2,jmax1
    
  elseif(LE.eq.2)then ! C grid for LE sting option
  
    write(2,*)nsting1 + uplmt-le_pos + uplmt-le_pos-1 + nsting2,jmax1
    print*,nsting1,'+',uplmt-le_pos,'+',uplmt-le_pos-1,'+',nsting2
    print*,''
    ! write(20,*)nsting1 + uplmt-le_pos,jmax1
    ! write(25,*)nsting2 + uplmt-le_pos,jmax1
    imax = imaxnew ! imax without the upstream grid.
    
  else ! default blade
    !
    write(2,*) imax1,jmax1
    
  endif
  !jmax = j2+2!jmax-j1
  WRITE(2,*) imax,jmax ! background grid
  !-------------------------------------------------------- 
  !Writing the inflated blade grid coordinates
  !-------------------------------------------------------- 
  !Translating the grid
  !Xg(:,:) = Xg(:,:) + msle - mble
  !WRITE(2,*) Xg(:,:),Yg(:,:) 
  if(thick_distr == 2)then ! C grid for sharp TE
  
    ! Elliptical grid smoothing 
    !call gauseI2D( imax2, jmax1, Xg, Yg, err, curvemesh, ellipsmooth )
    do j = 1, jmax1
     do i = 1, imax2
       write(2,*)Xg(i,j) ! Writing all X coordinates.
     enddo
    enddo
    !---------------------
    do j = 1, jmax1
     do i = 1, imax2
       write(2,*)Yg(i,j) ! Writing all Y coordinates
     enddo
    enddo
    !------------------------------------
    
  elseif(LE.eq.2)then ! C grid for LE sting option
  
    ! Writing all X coordinates.
    ! Top Sting
     do j = 1, jmax1
       do i = 1, nsting1!Top sting LE to Blade
        write(2,*)Xgstingblade(i,j) -stingl
        ! write(20,*)Xgstingblade(i,j) -stingl
       enddo
       do i = 1, uplmt-le_pos!Blade top curve
        write(2,*)Xgstingblade(njoint+i,j) -stingl
        ! write(20,*)Xgstingblade(njoint+i,j) -stingl
       enddo
    ! Bottom Sting 
       do i = 2, uplmt-le_pos!, 1, -1!Blade bottom curve to LE bottom sting
        write(2,*)Xg(i,j) -stingl
        ! write(25,*)Xg(i,j) -stingl
       enddo 
       do i = 1, nsting2!, 1 ,-1!Blade bottom curve
        write(2,*)Xgstingdash(i,j) -stingl 
        ! write(25,*)Xgstingdash(i,j) -stingl
       enddo    
    enddo
    !---------------------
    ! Writing all Y coordinates
    ! Top Sting
    do j = 1, jmax1
       do i = 1, nsting1!grid
        write(2,*)Ygstingblade(i,j) 
        ! write(20,*)Ygstingblade(i,j)
       enddo
       do i = 1, uplmt-le_pos!grid
        write(2,*)Ygstingblade(njoint+i,j)
        ! write(20,*)Ygstingblade(njoint+i,j)
       enddo
    ! Bottom Sting 
       do i = 2, uplmt-le_pos!, 1, -1!grid
        write(2,*)Yg(i,j) 
        ! write(25,*)Yg(i,j)
       enddo
       do i = 1, nsting2!, 1, -1!grid
        write(2,*)Ygstingdash(i,j)!Ygstingblade(nstinggrid+1-i,j) 
        ! write(25,*)Ygstingdash(i,j)
       enddo
    enddo 
    !------------------------------------
    
  else ! O grid for blunt TE
  
    ! Elliptical grid smoothing 
    if(LE.eq.1)then
      print*,'Smoothing the background grid...'
      call gauseI2Dblade( imax1, jmax1, Xg, Yg, err, curvemesh, ellipsmooth, LE )
    endif
    !
    do j = 1, jmax1
     do i = 1, imax1
       write(2,*)Xg(i,j) ! Writing all X coordinates.
     enddo
    enddo
    !---------------------
    do j = 1, jmax1
     do i = 1, imax1
       write(2,*)Yg(i,j) ! Writing all Y coordinates
     enddo
    enddo
    
  endif
  !-------------------------------------------------------- 
  ! Writing the background grid points
  !-------------------------------------------------------- 
  !Translating the grid
  !Xg1(:,:) = Xg1(:,:) + msle - mble
  !WRITE(2,*) Xg1(:,:),Yg1(:,:)
  if(LE.eq.2)then ! LE sting case
  
    !X coordinates for complete Sting case
    do j = 1, jmax
       do i = 1, imax
        write(2,*)Xg11(i,j) -stingl
       enddo
    enddo
    ! Y coordinates for complete sting case
    do j = 1, jmax
       do i = 1, imax
        write(2,*)Yg11(i,j) 
       enddo
    enddo
    !-------------------------------------------
    
  else ! other LE options
  
    do j = 1, jmax
     do i = 1, imax
       write(2,*)Xg1(i,j) ! Writing all X coordinates.
     enddo
    enddo
    !---------------------
    do j = 1, jmax
     do i = 1, imax
      write(2,*)Yg1(i,j) ! Writing all Y coordinates
     enddo
    enddo
    
  endif 
  !-------------------------------------------------------- 
  
endif
CLOSE(2)


if(BGgrid.eq.1.)then
 DEALLOCATE( Xg, Yg, Xg1, Yg1,xline,yline,Xbg,Ybg,Xbg2,Ybg2)
 if(LE.eq.2)then
  deallocate(Xg11,Yg11)
 endif
else
 DEALLOCATE( Xg, Yg, xblade, yblade)
endif 

return
end subroutine
!**************************************************	

