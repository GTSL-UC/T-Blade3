subroutine gauseI2D( ni, nj, x, y, err, curvemesh, ellipsmooth )
!-----------------------------------------------------
!Calculates the smoothed grid points.
! Solves for the laplace equation.
! Uses Guass-Siedel iterations.
! Inputs: X,Y grid points, imax,jmax
!         error for G-S iteration convergence
!         curvemesh, ellipsmooth are logical variables
! Outputs: X,Y grid points which are smoothed.
!-----------------------------------------------------
integer ni, nj, i, j, k,kmax, info, order, istart,ilast,nsmooth
real*8, intent(inout):: x(ni,nj), y(ni,nj), err
real*8 xtemp, ytemp, err1
real*8 g11, g12, g22
real*8 x1,x2,x3,x4,x5,x6,x7
real*8 y1,y2,y3,y4,y5,y6,y7,mat(4,5),A,B,C,D
logical curvemesh,ellipsmooth

if(ellipsmooth)then
  !Smoothing the top and bottom periodic bounday grid curve.
  !--------------------------------------------------------
  !BOTTOM curve
  !--------------------------------------------------------
  istart = 0
  ilast = 0
  !upstream to meanline smoothing
  y1 = 0.
  y2 = 0.
  !
  do i = 2 , ni
     y1 = y(i-1,1)
     y2 = y(i,1)
     if((y2 - y1).ne.0.)then
       istart = i
       exit
     endif
       !print*,'i: ',i
  enddo
  print*,'istart bottom curve: ',istart  
  !Smoothing the corner by taking midpoints of    
  !...5 points before and after istart...
  !...where istart is the point where the upstream ends...
  !...and the periodic wall begins. start with istart first
  nsmooth = 5 ! smoothing nsmooth times
  do j = 1, nsmooth
     ! @ istart
     x(istart    ,1) = 0.5*(x(istart - 1,1) + x(istart + 1,1) )
     !5 X points before istart
     x(istart - 1,1) = 0.5*(x(istart - 2,1) + x(istart    ,1) )
     x(istart - 2,1) = 0.5*(x(istart - 3,1) + x(istart - 1,1) )
     x(istart - 3,1) = 0.5*(x(istart - 4,1) + x(istart - 2,1) )
     x(istart - 4,1) = 0.5*(x(istart - 5,1) + x(istart - 3,1) )
     x(istart - 5,1) = 0.5*(x(istart - 6,1) + x(istart - 4,1) )
     !5 X points after istart
     x(istart + 1,1) = 0.5*(x(istart + 2,1) + x(istart    ,1) )
     x(istart + 2,1) = 0.5*(x(istart + 3,1) + x(istart + 1,1) )
     x(istart + 3,1) = 0.5*(x(istart + 4,1) + x(istart + 2,1) )
     x(istart + 4,1) = 0.5*(x(istart + 5,1) + x(istart + 3,1) )
     x(istart + 5,1) = 0.5*(x(istart + 6,1) + x(istart + 4,1) )
     !!--------------------------------------------------------
     ! @ istart
     y(istart    ,1) = 0.5*(y(istart - 1,1) + y(istart + 1,1) )
     !5 Y points before istart
     y(istart - 1,1) = 0.5*(y(istart - 2,1) + y(istart    ,1) )
     y(istart - 2,1) = 0.5*(y(istart - 3,1) + y(istart - 1,1) )
     y(istart - 3,1) = 0.5*(y(istart - 4,1) + y(istart - 2,1) )
     y(istart - 4,1) = 0.5*(y(istart - 5,1) + y(istart - 3,1) )
     y(istart - 5,1) = 0.5*(y(istart - 6,1) + y(istart - 4,1) )
     !5 Y points after istart
     y(istart + 1,1) = 0.5*(y(istart + 2,1) + y(istart    ,1) )
     y(istart + 2,1) = 0.5*(y(istart + 3,1) + y(istart + 1,1) )
     y(istart + 3,1) = 0.5*(y(istart + 4,1) + y(istart + 2,1) )
     y(istart + 4,1) = 0.5*(y(istart + 5,1) + y(istart + 3,1) )
     y(istart + 5,1) = 0.5*(y(istart + 6,1) + y(istart + 4,1) )
  enddo
  
  !meanline to downstream smoothing
  y1 = 0.
  y2 = 0.
  do i = ni, 2, -1
     y1 = y(i,1)
     y2 = y(i-1,1)
     if((y1 - y2).ne.0.)then
       !print*,y1,y2,i
       ilast = i
       exit
     endif
  enddo
  print*,'ilast bottom curve: ',ilast
  !
  !Smoothing the corner by taking midpoints of    
  !...5 points before and after ilast...
  !...where ilast is the point where the periodic wall ends...
  !...and the downstream begins. start with ilast first
  do j = 1, nsmooth
     ! @ ilast
     x(ilast    ,1) = 0.5*(x(ilast - 1,1) + x(ilast + 1,1) )
     !5 X points before ilast
     x(ilast - 1,1) = 0.5*(x(ilast - 2,1) + x(ilast    ,1) )
     x(ilast - 2,1) = 0.5*(x(ilast - 3,1) + x(ilast - 1,1) )
     x(ilast - 3,1) = 0.5*(x(ilast - 4,1) + x(ilast - 2,1) )
     x(ilast - 4,1) = 0.5*(x(ilast - 5,1) + x(ilast - 3,1) )
     x(ilast - 5,1) = 0.5*(x(ilast - 6,1) + x(ilast - 4,1) )
     !5 X points after ilast
     x(ilast + 1,1) = 0.5*(x(ilast + 2,1) + x(ilast    ,1) )
     x(ilast + 2,1) = 0.5*(x(ilast + 3,1) + x(ilast + 1,1) )
     x(ilast + 3,1) = 0.5*(x(ilast + 4,1) + x(ilast + 2,1) )
     x(ilast + 4,1) = 0.5*(x(ilast + 5,1) + x(ilast + 3,1) )
     x(ilast + 5,1) = 0.5*(x(ilast + 6,1) + x(ilast + 4,1) )
     !!--------------------------------------------------------
     ! @ ilast
     y(ilast    ,1) = 0.5*(y(ilast - 1,1) + y(ilast + 1,1) )
     !5 Y points before ilast
     y(ilast - 1,1) = 0.5*(y(ilast - 2,1) + y(ilast    ,1) )
     y(ilast - 2,1) = 0.5*(y(ilast - 3,1) + y(ilast - 1,1) )
     y(ilast - 3,1) = 0.5*(y(ilast - 4,1) + y(ilast - 2,1) )
     y(ilast - 4,1) = 0.5*(y(ilast - 5,1) + y(ilast - 3,1) )
     y(ilast - 5,1) = 0.5*(y(ilast - 6,1) + y(ilast - 4,1) )
     !5 Y points after ilast
     y(ilast + 1,1) = 0.5*(y(ilast + 2,1) + y(ilast    ,1) )
     y(ilast + 2,1) = 0.5*(y(ilast + 3,1) + y(ilast + 1,1) )
     y(ilast + 3,1) = 0.5*(y(ilast + 4,1) + y(ilast + 2,1) )
     y(ilast + 4,1) = 0.5*(y(ilast + 5,1) + y(ilast + 3,1) )
     y(ilast + 5,1) = 0.5*(y(ilast + 6,1) + y(ilast + 4,1) )
  enddo
  !
  !--------------------------------------------------------
  !TOP curve
  !--------------------------------------------------------
  istart = 0
  ilast = 0
  !upstream to meanline smoothing
  y1 = 0.
  y2 = 0.
  !
  do i = 2 , ni
     y1 = y(i-1,nj)
     y2 = y(i,nj)
     if((y2 - y1).ne.0.)then
       istart = i
       exit
     endif
       !print*,'i: ',i
  enddo
  print*,'istart top curve: ',istart  
  !Smoothing the corner by taking midpoints of    
  !...5 points before and after istart...
  !...where istart is the point where the upstream ends...
  !...and the periodic wall begins. start with istart first
  do j = 1, nsmooth
     ! @ istart
     x(istart    ,nj) = 0.5*(x(istart - 1,nj) + x(istart + 1,nj) )
     !5 X points before istart 
     x(istart - 1,nj) = 0.5*(x(istart - 2,nj) + x(istart    ,nj) )
     x(istart - 2,nj) = 0.5*(x(istart - 3,nj) + x(istart - 1,nj) )
     x(istart - 3,nj) = 0.5*(x(istart - 4,nj) + x(istart - 2,nj) )
     x(istart - 4,nj) = 0.5*(x(istart - 5,nj) + x(istart - 3,nj) )
     x(istart - 5,nj) = 0.5*(x(istart - 6,nj) + x(istart - 4,nj) )
     !5 X points after istart
     x(istart + 1,nj) = 0.5*(x(istart + 2,nj) + x(istart    ,nj) )
     x(istart + 2,nj) = 0.5*(x(istart + 3,nj) + x(istart + 1,nj) )
     x(istart + 3,nj) = 0.5*(x(istart + 4,nj) + x(istart + 2,nj) )
     x(istart + 4,nj) = 0.5*(x(istart + 5,nj) + x(istart + 3,nj) )
     x(istart + 5,nj) = 0.5*(x(istart + 6,nj) + x(istart + 4,nj) )
     !!--------------------------------------------------------
     ! @ istart
     y(istart    ,nj) = 0.5*(y(istart - 1,nj) + y(istart + 1,nj) )
     !5 Y points before istart
     y(istart - 1,nj) = 0.5*(y(istart - 2,nj) + y(istart    ,nj) )
     y(istart - 2,nj) = 0.5*(y(istart - 3,nj) + y(istart - 1,nj) )
     y(istart - 3,nj) = 0.5*(y(istart - 4,nj) + y(istart - 2,nj) )
     y(istart - 4,nj) = 0.5*(y(istart - 5,nj) + y(istart - 3,nj) )
     y(istart - 5,nj) = 0.5*(y(istart - 6,nj) + y(istart - 4,nj) )
     !5 Y points after istart
     y(istart + 1,nj) = 0.5*(y(istart + 2,nj) + y(istart    ,nj) )
     y(istart + 2,nj) = 0.5*(y(istart + 3,nj) + y(istart + 1,nj) )
     y(istart + 3,nj) = 0.5*(y(istart + 4,nj) + y(istart + 2,nj) )
     y(istart + 4,nj) = 0.5*(y(istart + 5,nj) + y(istart + 3,nj) )
     y(istart + 5,nj) = 0.5*(y(istart + 6,nj) + y(istart + 4,nj) )
  enddo
  
  !meanline to downstream smoothing
  y1 = 0.
  y2 = 0.
  do i = ni, 2, -1
     y1 = y(i,nj)
     y2 = y(i-1,nj)
     if((y1 - y2).ne.0.)then
       !print*,y1,y2,i
       ilast = i
       exit
     endif
  enddo
  print*,'ilast top curve: ',ilast
  !
  !Smoothing the corner by taking midpoints of    
  !...5 points before and after ilast...
  !...where ilast is the point where the periodic wall ends...
  !...and the downstream begins. start with ilast first
  do j = 1, nsmooth
    ! @ ilast
     x(ilast    ,nj) = 0.5*(x(ilast - 1,nj) + x(ilast + 1,nj) )
     !5 X points before ilast
     x(ilast - 1,nj) = 0.5*(x(ilast - 2,nj) + x(ilast    ,nj) )
     x(ilast - 2,nj) = 0.5*(x(ilast - 3,nj) + x(ilast - 1,nj) )
     x(ilast - 3,nj) = 0.5*(x(ilast - 4,nj) + x(ilast - 2,nj) )
     x(ilast - 4,nj) = 0.5*(x(ilast - 5,nj) + x(ilast - 3,nj) )
     x(ilast - 5,nj) = 0.5*(x(ilast - 6,nj) + x(ilast - 4,nj) )
     !5 X points after ilast
     x(ilast + 1,nj) = 0.5*(x(ilast + 2,nj) + x(ilast    ,nj) )
     x(ilast + 2,nj) = 0.5*(x(ilast + 3,nj) + x(ilast + 1,nj) )
     x(ilast + 3,nj) = 0.5*(x(ilast + 4,nj) + x(ilast + 2,nj) )
     x(ilast + 4,nj) = 0.5*(x(ilast + 5,nj) + x(ilast + 3,nj) )
     x(ilast + 5,nj) = 0.5*(x(ilast + 6,nj) + x(ilast + 4,nj) )
     !!--------------------------------------------------------
     ! @ ilast
     y(ilast    ,nj) = 0.5*(y(ilast - 1,nj) + y(ilast + 1,nj) )
     !5 Y points before ilast
     y(ilast - 1,nj) = 0.5*(y(ilast - 2,nj) + y(ilast    ,nj) )
     y(ilast - 2,nj) = 0.5*(y(ilast - 3,nj) + y(ilast - 1,nj) )
     y(ilast - 3,nj) = 0.5*(y(ilast - 4,nj) + y(ilast - 2,nj) )
     y(ilast - 4,nj) = 0.5*(y(ilast - 5,nj) + y(ilast - 3,nj) )
     y(ilast - 5,nj) = 0.5*(y(ilast - 6,nj) + y(ilast - 4,nj) )
     !5 Y points after ilast
     y(ilast + 1,nj) = 0.5*(y(ilast + 2,nj) + y(ilast    ,nj) )
     y(ilast + 2,nj) = 0.5*(y(ilast + 3,nj) + y(ilast + 1,nj) )
     y(ilast + 3,nj) = 0.5*(y(ilast + 4,nj) + y(ilast + 2,nj) )
     y(ilast + 4,nj) = 0.5*(y(ilast + 5,nj) + y(ilast + 3,nj) )
     y(ilast + 5,nj) = 0.5*(y(ilast + 6,nj) + y(ilast + 4,nj) )
  enddo
  !
endif
  !---------------------------------------------------------------
  !---------------------------------------------------------------
if(curvemesh)then
  print*,"Elliptic grid smoothing..."
  !print*,'Error coming in :',err
  !k = 0
  kmax = 2000
    do k = 1, kmax!while(abs(err1) > err)
        err1 = 0
        do j = 2, nj-1
           do i = 2, ni-1
           
              g11 =((x(i+1,j)-x(i-1,j) )**2 + (y(i+1,j)-y(i-1,j))**2 )/4
              g22 =((x(i,j+1)-x(i,j-1) )**2 + (y(i,j+1)-y(i,j-1))**2 )/4
              g12 =(x(i+1,j)-x(i-1,j))*(x(i,j+1)-x(i,j-1))/4 + &
                   (y(i+1,j)-y(i-1,j))*(y(i,j+1)-y(i,j-1))/4

              xtemp = 1/(2*(g11+g22))*( &
                  g22*x(i+1,j) - 0.5*g12*x(i+1,j+1) + 0.5*g12*x(i+1,j-1)+ &
                  g11*x(i,j+1) + g11*x(i,j-1) + &
                  g22*x(i-1,j) - 0.5*g12*x(i-1,j-1) + 0.5*g12*x(i-1,j+1) )

              ytemp = 1/(2*(g11+g22))*( &
                  g22*y(i+1,j) - 0.5*g12*y(i+1,j+1) +0.5*g12*y(i+1,j-1)+ &
                  g11*y(i,j+1) + g11*y(i,j-1) + &
                  g22*y(i-1,j) - 0.5*g12*y(i-1,j-1) +0.5*g12*y(i-1,j+1) )
                  
              err1 = err1 + (x(i,j)-xtemp)**2+(y(i,j)-ytemp)**2

              x(i,j) = xtemp
              y(i,j) = ytemp
            enddo
        enddo
        err1 = SQRT( err1/((nj-2)*(ni-2)) )
        !print*,'error: ',k,err1
        !print*,'iteration,error,inputerror : ',k,err1,err
        if(abs(err1) < err)then
           print*,'Smoothing converged at: ',k,err,err1
           return
        elseif (k == kmax)then
           print*,'Reached the max iteration,error,givenerror: ',k,err,err1
        endif
    !    k = k+1
    enddo
    !print*,'Smoothing converged with error: ',k
endif

return
end subroutine gauseI2D
!*******************************************************************

!*******************************************************************
!*******************************************************************
subroutine gauseI2Dblade( ni, nj, x, y, err, curvemesh, ellipsmooth,LE )
!-----------------------------------------------------
!Calculates the smoothed grid points of blade.
! Solves for the laplace equation.
! Uses Guass-Siedel iterations.
! Inputs: X,Y grid points, imax,jmax
!         error for G-S iteration convergence
!         curvemesh, ellipsmooth are logical variables
! Outputs: X,Y grid points which are smoothed.
!-----------------------------------------------------
integer ni, nj, i, j, k,kmax, info, order, istart,ilast,nsmooth,LE
real*8, intent(inout):: x(ni,nj), y(ni,nj), err
real*8 xtemp, ytemp, err1
real*8 g11, g12, g22
real*8 x1,x2,x3,x4,x5,x6,x7
real*8 y1,y2,y3,y4,y5,y6,y7,mat(4,5),A,B,C,D
logical curvemesh,ellipsmooth

!---------------------------------------------------------------
!---------------------------------------------------------------
if(curvemesh.or.(LE.eq.1))then
  print*,"Elliptic grid smoothing..."
  !print*,'Error coming in :',err
  !k = 0
  kmax = 2000
    do k = 1, kmax!while(abs(err1) > err)
        err1 = 0
        do j = 2, nj-1
           do i = 2, ni-1
           
              g11 =((x(i+1,j)-x(i-1,j) )**2 + (y(i+1,j)-y(i-1,j))**2 )/4
              g22 =((x(i,j+1)-x(i,j-1) )**2 + (y(i,j+1)-y(i,j-1))**2 )/4
              g12 =(x(i+1,j)-x(i-1,j))*(x(i,j+1)-x(i,j-1))/4 + &
                   (y(i+1,j)-y(i-1,j))*(y(i,j+1)-y(i,j-1))/4

              xtemp = 1/(2*(g11+g22))*( &
                  g22*x(i+1,j) - 0.5*g12*x(i+1,j+1) + 0.5*g12*x(i+1,j-1)+ &
                  g11*x(i,j+1) + g11*x(i,j-1) + &
                  g22*x(i-1,j) - 0.5*g12*x(i-1,j-1) + 0.5*g12*x(i-1,j+1) )

              ytemp = 1/(2*(g11+g22))*( &
                  g22*y(i+1,j) - 0.5*g12*y(i+1,j+1) +0.5*g12*y(i+1,j-1)+ &
                  g11*y(i,j+1) + g11*y(i,j-1) + &
                  g22*y(i-1,j) - 0.5*g12*y(i-1,j-1) +0.5*g12*y(i-1,j+1) )
                  
              err1 = err1 + (x(i,j)-xtemp)**2+(y(i,j)-ytemp)**2

              x(i,j) = xtemp
              y(i,j) = ytemp
            enddo
        enddo
        err1 = SQRT( err1/((nj-2)*(ni-2)) )
        !print*,'error: ',k,err1
        !print*,'iteration,error,inputerror : ',k,err1,err
        if(abs(err1) < err)then
           print*,'Smoothing converged at: ',k,err,err1
           return
        elseif (k == kmax)then
           print*,'Reached the max iteration,error,givenerror: ',k,err,err1
        endif
    !    k = k+1
    enddo
    !print*,'Smoothing converged with error: ',k
endif

return
end subroutine gauseI2Dblade
!**************************************************



!*******************************************************
!**************************************************
subroutine curveBG(imax,jmax,xblade,yblade, &
                     uplmt,np,casename,develop,isdev,msle,mble,chrd,pitch)
!-----------------------------------------------------
! Subroutine creates the curved mesh for the Background
! Creates a uniformly distributed meanline curve.
! Reduces the points on the meanline (LE-TE) to half.
! Inputs: xblade,yblade np coordinates of the blade section
!         uplmt is the LE position
!         chrd is the chord for calculating cellwidth
!         pitch for calculating jmax
!         msle,mble for translating the grid in m' space.
! Outputs: imax,jmax are the max indices of the grid
!          xline,yline is the upstream+meanline+downstream coordinates
!          It is defined in the module 'gridvar'
!-----------------------------------------------------
use gridvar
implicit none
integer i,j,k,dwnstrm,j1,j2,np_side
integer, intent(in) :: uplmt,np
integer, intent(out) :: imax,jmax

real*8, intent(in) :: xblade(np),yblade(np),msle,mble,chrd,pitch
!real*8, allocatable, dimension(:), intent(inout) :: xline,yline
real*8 cellwidth, dbmean
character*32 fname,fname1,fname2,fname3,fname4
character*32 fname5,fname6,fext,temp,casename,file1,develop
logical isdev

!allocate(xline(10),yline(10))
np_side = uplmt
  !using half the np_side points for sparse grid
  if(mod(np_side,2).eq.0)then
    np_side = np_side - 1 
  endif
  ! Using half the points as np_side.
  np_side = (0.5*(np_side+1))
  print*,'half np_side for curvemesh: ',np_side  
  !-----------------------------------------------------
  !Meanline with uniform clustering.
  !-----------------------------------------------------
  if (allocated(xbmean)) deallocate(xbmean)
  if (allocated(ybmean)) deallocate(ybmean)
  allocate(xbmean(np_side))
  allocate(ybmean(np_side))
  xbmean = 0.;ybmean = 0.
  xbmean(1) = xblade(uplmt)
  xbmean(np_side) = xblade(1)
  dbmean = abs(xbmean(1) - xbmean(np_side))/(np_side - 1) ! instead of uplmt
  do i = 2, np_side
     xbmean(i) = xbmean(i-1) + dbmean   !
  enddo
  !-----------------------------------------------------
  !Forcing the end points to be the Le,TE x points.
  !-----------------------------------------------------
  xbmean(1) = xblade(uplmt)
  xbmean(np_side) = xblade(1)
  if (allocated(xb1   )) deallocate(xb1   )
  if (allocated(yb1   )) deallocate(yb1   )
  if (allocated(xb2   )) deallocate(xb2   )
  if (allocated(yb2   )) deallocate(yb2   )
  if (allocated(xbnew1)) deallocate(xbnew1)
  if (allocated(ybnew1)) deallocate(ybnew1)
  if (allocated(xbnew2)) deallocate(xbnew2)
  if (allocated(ybnew2)) deallocate(ybnew2)
  allocate(xb1(np_side),yb1(np_side),xb2(np_side),yb2(np_side))
  allocate(xbnew1(np_side),ybnew1(np_side),xbnew2(np_side),ybnew2(np_side))
  xb1 = 0.; yb1 = 0.; xb2 = 0.; yb2 = 0.; 
  xbnew1 = 0.; ybnew1 = 0.; xbnew2 = 0.;ybnew2 = 0.
  !-----------------------------------------------------
  ! Splitting the blade curve into top and bottom curves.
  !-----------------------------------------------------
  !Bottom curve
  !-----------------------------------------------------
  xb1(1) = xblade(uplmt)
  yb1(1) = yblade(uplmt)
  do i = 2, np_side ! using only half the points on the curve
     xb1(i) = xblade(uplmt-2+2*i)
     yb1(i) = yblade(uplmt-2+2*i)
     !print*,xb1(i),yb1(i)
  enddo
  !-----------------------------------------------------
  ! Forcing the end point to be the TE point.
  !-----------------------------------------------------
  xb1(np_side) = xblade(1)
  yb1(np_side) = yblade(1)
  !-----------------------------------------------------
  ! Interpolating to get equidistant y points
  !-----------------------------------------------------
  call curv_line_inters(yb1,xb1,np_side,xbmean,ybnew1,np_side)
  !
  if(isdev)then
    ! Bottom curve
    fname1 = 'botcurve.'//trim(fext)
    open(331,file=fname1,status='unknown')
    write(331,*)'skip'
    write(331,*)'skip'
    do i = 1, np_side
       write(331,*)xb1(i)- msle + mble,yb1(i)
    enddo
    close(331)
    write(*,*)
    ! Bottom curve uniform points
    fname1 = 'uniformbotcurve.'//trim(fext)
    open(332,file=fname1,status='unknown')
    write(332,*)'skip'
    write(332,*)'skip'
    print*,'Bottom curve: xmean, y'
    do i = 1, np_side
       write(332,*)xbmean(i)- msle + mble,ybnew1(i)
    enddo
    close(332)
  endif
  write(*,*)
  !
  !-----------------------------------------------------
  !Top curve
  !-----------------------------------------------------
  xb2(1) = xblade(uplmt)
  yb2(1) = yblade(uplmt)
  do i = np_side, 2, -1 ! using only half the points on the curve
     xb2(i) = xblade(uplmt+2-2*i)
     yb2(i) = yblade(uplmt+2-2*i)
  enddo
  !-----------------------------------------------------
  ! Interpolating to get equidistant y points 
  !-----------------------------------------------------
  call curv_line_inters(yb2,xb2,np_side,xbmean,ybnew2,np_side)
  !
  !Calculating ybmean values
  do i = 1, np_side
     ybmean(i) = 0.5*(ybnew1(i) + ybnew2(i))
  enddo
  !
  if(isdev)then
    ! Top curve
    fname1 = 'topcurve.'//trim(fext)
    open(333,file=fname1,status='unknown')
    write(333,*)'skip'
    write(333,*)'skip'
    do i = 1, np_side
       write(333,*)xb2(i)- msle + mble,yb2(i)
    enddo
    close(333)
    write(*,*)
    !Top curve uniform points
    print*,'Top curve: xmean, y'
    fname1 = 'uniformtopcurve.'//trim(fext)
    open(334,file=fname1,status='unknown')
    write(334,*)'skip'
    write(334,*)'skip'
    do i = 1, np_side
       write(334,*)xbmean(i)- msle + mble,ybnew2(i)
    enddo
    close(334)
    write(*,*)
    ! Meanline
    print*,'Uniform meanline: xmean, ymean'
    fname1 = 'uni_meanline.'//trim(fext)
    open(335,file=fname1,status='unknown')
    write(335,*)'skip'
    write(335,*)'skip'
    do i = 1, np_side
       write(335,*)xbmean(i)- msle + mble,ybmean(i)
    enddo
    close(335)
  endif
  write(*,*)
  !
  imax = 21  ! Change the 2nd value here and every related indices below will get updated.  
  dwnstrm = 4 ! Multiplier to increase the number of cells downstream
  !----------------------------------------------
  ! Cell width definition
  !----------------------------------------------
  ! cell width = (total length of the domain)/no. of total cells
  cellwidth = 2.*chrd/(imax-1) !(xline(uplmt+ 2*j1) - xline(1))/(imax-1)
  !
  jmax = 4*(int(pitch/cellwidth)) 
  if(mod(jmax,2).eq.0)then
    jmax = jmax + 1
  else
    jmax = jmax 
  endif
  !
  !J(normal direction) Indices for writing meanline coordinates and their offsets
  j1 = int((jmax-1)*0.5)
  j2 = j1 + 1
  !
  print*,'imax x jmax begin: ',imax, jmax
  print*,'np_side,j1,j2 :',np_side,j1,j2
  !print*,'j1 + np_side + (dwnstrm*j1)',j1 + np_side + (dwnstrm*j1)
  if (allocated(xmeanline)) deallocate(xmeanline)
  if (allocated(ymeanline)) deallocate(ymeanline)
  Allocate(xmeanline(j1 + np_side + (dwnstrm*j1)),ymeanline(j1 + np_side + (dwnstrm*j1)))
  if (allocated(xline)) deallocate(xline)
  if (allocated(yline)) deallocate(yline)
  Allocate(xline(j1 + np_side + (dwnstrm*j1)),yline(j1 + np_side + (dwnstrm*j1)))
  print*,'j1 + np_side + (dwnstrm*j1)',j1 + np_side + (dwnstrm*j1)
  !---------------------------------------
  ! coordinates upstream to the meanline.
  !---------------------------------------
  do i = 1, j2  
     xline(i) =  xblade(uplmt) - 0.025*chrd*((j2-i))
     yline(i) =  yblade(uplmt)
     !xline(i) = xmeanline(i)
     !yline(i) = ymeanline(i)
  enddo 
  !---------------------------------------------------------------
  !meanline coordinates as average of top and bottom curve points. 
  !---------------------------------------------------------------
  do i = j2, np_side+j1
     xline(i) = xbmean(i-j1)
     yline(i) = ybmean(i-j1)
  enddo
  !---------------------------------------
  ! coordinates downstream to the meanline
  !---------------------------------------
  do i = np_side + j2, (j1 + np_side + (dwnstrm*j1))
     xline(i) = xblade(np) + 0.025*chrd*((i-(np_side+j1))) !0.0125
     yline(i) = yblade(np)
     !xline(i) = xmeanline(i)
     !yline(i) = ymeanline(i)   
  enddo
  imax = j1 + np_side + (dwnstrm*j1)!j2 + uplmt + dwnstrm*j1 - 2
  if(mod(imax,4).eq.0)then ! Conditions to follow 4n+1 rule for 4n cells
    imax = imax + 1
  elseif(mod(imax,4).eq.1)then
    imax = imax
  elseif(mod(imax,4).eq.2)then
    imax = imax - 1
  elseif(mod(imax,4).eq.3)then
    imax = imax - 2
  endif
  print*,'imax x jmax final: ',imax,'x',jmax
  write(*,*)
    ! print*,'xline inside curveBG subroutine'
  ! do i = 1, imax
     ! print*,xline(i),yline(i)
  ! enddo

deallocate(xbmean,ybmean,xb1,yb1,xb2,yb2)
deallocate(xbnew1,ybnew1,xbnew2,ybnew2,xmeanline,ymeanline)
  
return
end subroutine curveBG
!************************************************************************



!*************************************************************************
!*************************************************************************
subroutine linearBG(imax,jmax,xblade,yblade, &
             uplmt,np,casename,develop,msle,mble,chrd,pitch)
!-----------------------------------------------------------------
! Creates a linear uniform background grid . Converts the curved meanline
! ...into a straight-line with a peak at the max curvature.
! 4 blocks: 1st is the upstream
!           2nd is the block from LE to max/min camber
!           3rd is the block from max/min camber to TE
!           4th is the downstream
! Inputs: xblade,yblade np coordinates of the blade section
!         uplmt is the LE position
!         chrd is the chord for calculating cellwidth
!         pitch for calculating jmax
!         msle,mble for translating the grid in m' space.
! Outputs: imax,jmax are the max indices of the grid
!          xline,yline is the upstream+meanline+downstream coordinates
!          It is defined in the module 'gridvar'
!-----------------------------------------------------------------
use gridvar
implicit none
integer i,j,k,dwnstrm,j1,j2,np_side
integer, intent(in) :: uplmt,np
integer, intent(out) :: imax,jmax

real*8, intent(in) :: xblade(np),yblade(np),msle,mble,chrd,pitch
real*8 cellwidth
character*32 fname,fname1,fname2,fname3,fname4
character*32 fname5,fname6,fext,temp,casename,file1,develop

  imax = 51
  dwnstrm = 4
  !----------------------------------------------
  ! Cell width definition
  !----------------------------------------------
  ! cell width = (total length of the domain)/no. of total cells
  cellwidth = 2.*chrd/(imax-1) !(xline(uplmt+ 2*j1) - xline(1))/(imax-1)
  !
  jmax = int(pitch/cellwidth)
  !
  if(mod(jmax,2).eq.0)then
    jmax = jmax + 1
  else
    jmax = jmax 
  endif
  !J(normal direction) Indices for writing meanline coordinates and their offsets
  j1 = int((jmax-1)*0.5)
  j2 = j1 + 1
  !
  print*,'uplmt,j1,j2 :',uplmt,j1,j2
  !
  if(allocated(xline).and.allocated(yline))then
    deallocate(xline,yline)
  endif
  if (allocated(xmeanline)) deallocate(xmeanline)
  if (allocated(ymeanline)) deallocate(ymeanline)
  if (allocated(xline    )) deallocate(xline    )
  if (allocated(yline    )) deallocate(yline    )
  Allocate(xmeanline(j1 + uplmt + (dwnstrm*j1)),ymeanline(j1 + uplmt + (dwnstrm*j1)))
  Allocate(xline(j1 + uplmt + (dwnstrm*j1)),yline(j1 + uplmt + (dwnstrm*j1)))
  !---------------------------------------
  ! coordinates upstream to the meanline.
  !---------------------------------------
  do i = 1, j2
     xmeanline(i) =  xblade(uplmt) - 1.*chrd*((j2-i))
     ymeanline(i) =  yblade(uplmt)
     xline(i) = xmeanline(i)
     yline(i) = ymeanline(i)
  enddo 
  !---------------------------------------------------------------
  !meanline coordinates as average of top and bottom curve points. 
  !---------------------------------------------------------------
  do i = j2, uplmt+j1!21,uplmt+20
     xmeanline(i) = 0.5*(xblade(uplmt - (j2+1-i)) + xblade(uplmt + (j2+1-i)))
     ymeanline(i) = 0.5*(yblade(uplmt - (j2+1-i)) + yblade(uplmt + (j2+1-i)))
  enddo   
  !---------------------------------------
  ! coordinates downstream to the meanline
  !---------------------------------------
  do i = uplmt+j2, (j1 + uplmt + (dwnstrm*j1))!(2*j1)!uplmt+21, uplmt+40 
     xmeanline(i) = xblade(np) + 1.*chrd*((i-(uplmt+j1)))
     ymeanline(i) = yblade(np)
     xline(i) = xmeanline(i)
     yline(i) = ymeanline(i)
     ! print*,'i,xline,yline :',i,xline(i),yline(i)
  enddo 
  !-----------------------------------------------
  !Writing the 2D meanline coordinates to a file.
  !-----------------------------------------------
  ! fname4= '2Dmeanline.'//trim(fext)
  ! print*,'meanline fname:', fname4
  ! write(*,*)
  ! open(33,file=fname4,status='unknown')
  ! write(33,*)'skip'
  ! write(33,*)'skip'
  ! do i = 1, uplmt + (2*j1)
  ! write(33,*)xmeanline(i),ymeanline(i) 
  ! enddo 
  ! close(33) 
  !
  Deallocate(xline,yline)
  !--------------------------------------------------------------------------------
  !Creating a straight line of the meanline curve with uniform point distribution.
  !--------------------------------------------------------------------------------
  !Finding the theta @ LE, TE and the maxtheta coordinate.
  theta_LE = ymeanline(j1 + 1)
  theta_TE = ymeanline(j1 + uplmt)

  ! Probing the camber / curvature at 5 points
  ! LE,25%chord,maxval,75%chord,TE
  ! probe indices at 25% and 75% chord
  p2 = int((j1 + 1) + 0.05*((j1 + uplmt) - (j1 + 1))) + 1
  p4 = int((j1 + 1) + 0.85*((j1 + uplmt) - (j1 + 1))) + 1
  !
  value1   = (theta_LE) ! @ LE
  value2   = (ymeanline(p2)) ! @ 25% chord
  valueMax = (maxval(ymeanline, DIM = 1)) ! @ 25% chord
  value4   = (ymeanline(p4)) ! @ 75% chord
  value5   = (theta_TE) ! @ TE
  
  if((valueMax.lt.value5).or.(value2.lt.value4))then ! Negative curvature
   
    print*,'Negative Curvature.'
    print*,''
    mincurv = minval(ymeanline, DIM = 1)
    i_loc(1) = minloc(ymeanline,1)
    print*,'minloc:', minloc(ymeanline,1)
    i_thetamax = i_loc(1) 
    !stop
    if(i_thetamax.eq.1)then
      i_thetamax = i_thetamax + j1
    endif  
    theta_max = ymeanline(i_thetamax)  

  elseif((value2.gt.value5).and.(valueMax.gt.value2))then ! Positive curvature
  
    print*,'Positive Curvature.'
    print*,''
    maxcurv = maxval(ymeanline, DIM = 1)
    i_loc(1) = maxloc(ymeanline,1)
    print*,'maxloc:', maxloc(ymeanline,1)
    i_thetamax = i_loc(1) 
    ! stop
    if(i_thetamax.eq.1)then
      i_thetamax = i_thetamax + j1
    endif
    theta_max = ymeanline(i_thetamax)  
    
  else ! error
  
    write(*,*)
    print*,'ERROR finding the max curvature for linear background grid !'
    stop  
    
  endif ! endif for finding the max/min location in BG grid
    
  print*,'itheta_max,theta_max:',i_thetamax,theta_max
  ! Finding the mprime @ LE, TE and max mprime coordinate.
  mprime_LE = xmeanline(j1 + 1)
  mprime_TE = xmeanline(j1 + uplmt)
  mprime_thetamax = xmeanline(i_thetamax)
  print*,'mprime_LE',mprime_LE
  !
  !Region 1= upstream background grid
  !Region 2 = LE to max theta location grid
  !Region 2 cell count
  R2cell = int((mprime_thetamax - mprime_LE)/(cellwidth))
  print*,'R2cell: ',R2cell
  !Region 3 = max theta location to TE grid
  !Region 3 cell count
  R3cell = int((mprime_TE - mprime_thetamax)/(cellwidth))
  print*,'R3cell: ',R3cell
  !Region 4 = downstream background grid

  !!Redefining cellwidth
  !! cell width = (total length of the domain)/no. of total cells
  !! cellwidth = 2.*chrd/(imax-1) !(xline(uplmt+ 2*j1) - xline(1))/(imax-1)
  !! cellwidth = 0.5*cellwidth
  !!print*,'redifened cell width:',cellwidth
  !!jmax = int(pitch/cellwidth)+1

  if(R2cell.ne.0)then ! maxcurvature or mincurvature is not at LE
    !Updated imax
    imax = j1 + (R2cell+1) + (R3cell+1) + (dwnstrm*j1)!j1
    print*,'j1 + (R2cell+1) + (R3cell+1) + (dwnstrm*j1)',j1,(R2cell+1),(R3cell+1),(dwnstrm*j1)
    print*,'new imax: ',imax
    !-----------------------------------------
    if (allocated(xline)) deallocate(xline)
    if (allocated(yline)) deallocate(yline)
    Allocate(xline(imax),yline(imax))
    !Allocate(Xbg(imax,jmax),Ybg(imax,jmax))
    !Allocate(Xbg2(imax,jmax),Ybg2(imax,jmax))
    !-----------------------------------------
    dmprime1 = real((mprime_thetamax - mprime_LE)/(R2cell))
    dtheta1 = real((theta_max - theta_LE)/(R2cell))
    do i = 1, R2cell+1
       xline(j1+i) = mprime_LE + (i-1)*dmprime1
       yline(j1+i) = theta_LE + (i-1)*dtheta1 
    enddo
    !
    dmprime1 = real((mprime_TE- mprime_thetamax)/(R3cell))
    dtheta1 = real((theta_max - theta_TE)/(R3cell)) 
    do i = 1, R3cell
       xline(j1 + R2cell+1 + i) = mprime_thetamax + (i)*dmprime1
       yline(j1 + R2cell+1 + i) = theta_max - (i)*dtheta1
       !print*,'i,xline,yline :',i_thetamax + i-1,xline(i),yline(i)
    enddo
    !---------------------------------------
    ! coordinates upstream to the meanline with new imax.
    !---------------------------------------
    do i = 1, j1!20 
       xline(i) =  xblade(uplmt) - cellwidth*((j2-i))
       yline(i) =  yblade(uplmt)
    enddo  
    !
    !---------------------------------------
    ! coordinates downstream to the meanline with new imax
    !---------------------------------------
    do i = (j1+(R2cell+1) + (R3cell+1)), imax!(uplmt+(2*j1))!uplmt+21, uplmt+40 
       xline(i) = xblade(np) + 1.*cellwidth*((i-(j1 + (R2cell+1) + (R3cell+1) - 1)))
       yline(i) = yblade(np)
    enddo 
  elseif(R2cell.eq.0)then !  maxcurvature or mincurvature is at LE   
    !Updated imax
    imax = j1 + (R3cell+1) + 1 + (dwnstrm*j1)!j1
    print*,'j1 + (R3cell+1) + 1 + (dwnstrm*j1)',j1,(R3cell+1),(dwnstrm*j1)
    print*,'new imax: ',imax
    !-----------------------------------------
    if (allocated(xline)) deallocate(xline)
    if (allocated(yline)) deallocate(yline)
    Allocate(xline(imax),yline(imax))
    !Allocate(Xbg(imax,jmax),Ybg(imax,jmax))
    !Allocate(Xbg2(imax,jmax),Ybg2(imax,jmax))
    !-----------------------------------------
    dmprime1 = real(abs(mprime_TE - mprime_thetamax)/(R3cell-1))
    dtheta1 = real(abs(theta_max - theta_TE)/(R3cell-1))
    do i = 1, R3cell
       xline(j1+i) = mprime_LE + (i-1)*dmprime1
       yline(j1+i) = theta_LE - (i-1)*dtheta1  ! need to decrement from theta_max
    enddo
    !---------------------------------------
    ! coordinates upstream to the meanline with new imax.
    !---------------------------------------
    do i = 1, j1!20 
       xline(i) =  xblade(uplmt) - cellwidth*((j2-i))
       yline(i) =  yblade(uplmt)
    enddo  
    !
    !---------------------------------------
    ! coordinates downstream to the meanline with new imax
    !---------------------------------------
    do i = (j1 + (R3cell+1)), imax!(uplmt+(2*j1))!uplmt+21, uplmt+40 
       xline(i) = xblade(np) + 1.*cellwidth*((i-(j1 + (R3cell+1) - 1)))
       yline(i) = yblade(np)
    enddo     
  endif                         
  print*,'imax x jmax final: ',imax,'x',jmax
  write(*,*)      
      
  deallocate(xmeanline,ymeanline)
  
return
end subroutine linearBG
!************************************************************************



!*************************************************************************
!*************************************************************************
subroutine stingLEgrid(xb,yb,np,uplmt,stingl,le_pos,thkc,thick_distr, &
                       mble,msle,mbte,chrd,jmax1,casename,fext,develop,isdev,etawidth)
!-----------------------------------------------------------------
! Creates a blade grid with sting at LE.
! Creates a C-grid.
!-----------------------------------------------------------------
use gridvar
implicit none

integer i,ii,j,k,np,uplmt,jmax1,le_pos
real*8 stingl,thkc,thick_distr,pi,cellwidth,d_offset
real*8, intent(in) :: xb(np),yb(np),msle,mble,mbte,chrd,etawidth
real*8, allocatable, dimension(:) :: xnew,ynew,xblade,yblade
character*32 fname5,fname6,casename,develop,fext
logical isdev

!Constants
pi = 4.*atan(1.0)
!
if (allocated(xblade)) deallocate(xblade)
if (allocated(yblade)) deallocate(yblade)
allocate(xblade(np),yblade(np))
do i = 1, np
 xblade(i) = xb(i)
 yblade(i) = yb(i)
enddo
!---------------------------------------------------
! Translating the blade in m' direction
!---------------------------------------------------
print*,'Le before translation: ',xblade(uplmt)
xblade(uplmt) = mble
!if((js.ne.1).or.(js.ne.nspn))then
 do i = 1, np
  xblade(i) = xblade(i) + msle - mble!- xblade(uplmt)
 enddo
!----------------------------------------------------- 
 !if(BGgrid.eq.0)then
 cellwidth = 2.*chrd/(50)
 !endif
 stingl = (mbte - mble - chrd)
 print*,'Sting Length: ',stingl
 ! Adding more points on the STING for gridding 
 ! Subtracting 2 nodes on topsting to follow 4n+1 rule for high order grid.
 ! Subtracting 1 node on botsting so that the complete balde follows 2n+1 grid rule.
 nstingtop = int((xblade(uplmt-2) - xblade(uplmt))/(cellwidth*0.25)) + 1 - 1
 nstingbot = int((xblade(uplmt+2) - xblade(uplmt))/(cellwidth*0.25)) + 1 - 1 
 !     
 nstinggrid = nstingtop + 2*(le_pos-3) + uplmt-le_pos + uplmt-le_pos-1 + nstingbot
 ! Correction to follow 4n+1 rule for cells
 if(mod(nstinggrid,4).eq.0)then
   nstingbot = nstingbot + 1
   nstinggrid = nstingtop + 2*(le_pos-3) + uplmt-le_pos + uplmt-le_pos-1 + nstingbot 
   print*,'Total points(4n+1): ',nstinggrid
 elseif(mod(nstinggrid,4).eq.1)then
   nstinggrid = nstinggrid
   print*,'Total points(4n+1): ',nstinggrid
 elseif(mod(nstinggrid,4).eq.2)then
   nstingbot = nstingbot - 1
   nstinggrid = nstingtop + 2*(le_pos-3) + uplmt-le_pos + uplmt-le_pos-1 + nstingbot
   print*,'Total points(4n+1): ',nstinggrid      
 elseif(mod(nstinggrid,4).eq.3)then
   nstingtop = nstingtop - 1
   nstingbot = nstingbot - 1
   nstinggrid = nstingtop + 2*(le_pos-3) + uplmt-le_pos + uplmt-le_pos-1 + nstingbot  
   print*,'Total points(4n+1): ',nstinggrid      
 endif 
 ! Cellwidth for bith stings.    
 stingtop_cellsize = (xblade(uplmt-2) - xblade(uplmt-1))/(nstingtop-1)
 stingbot_cellsize = (xblade(uplmt+2) - xblade(uplmt-1))/(nstingbot-1)
 !new np
 np = np + nstingtop + nstingbot
 nsting1 = nstingtop+le_pos-3
 nsting2 = nstingbot+le_pos-3
 !
 !-------------------------------------------------------------
 ! Deallocation of allocated variables
 !-------------------------------------------------------------
 deallocate(xblade,yblade)
 !
 if(allocated(Xgstingblade).and.allocated(Ygstingblade))then
    deallocate(Xgstingblade,Ygstingblade)
 endif
 !
 if(allocated(xbstingtop).and.allocated(ybstingtop))then
    deallocate(xbstingtop,ybstingtop)
 endif
  !
 if(allocated(xbstingbot).and.allocated(ybstingbot))then
    deallocate(xbstingbot,ybstingbot)
 endif 
  !
 if(allocated(Xgstingtop).and.allocated(Ygstingtop))then
    deallocate(Xgstingtop,Ygstingtop)
 endif 
 !
 if(allocated(xgstingbot).and.allocated(ygstingbot))then
    deallocate(xgstingbot,ygstingbot)
 endif 
 !
 if(allocated(xgstingdash).and.allocated(ygstingdash))then
    deallocate(xgstingdash,ygstingdash)
 endif 
 !
 if(allocated(xbot).and.allocated(ybot))then
    deallocate(xbot,ybot)
 endif
   !
 if(allocated(xst).and.allocated(yst))then
    deallocate(xst,yst)
 endif
 !-----------------------------------------------------------------
 !-----------------------------------------------------------------
 ! Allocation of variables
 !-----------------------------------------------------------------
 allocate(Xgstingblade(nstinggrid,jmax1),Ygstingblade(nstinggrid,jmax1))
 allocate(xbstingtop(nsting1),ybstingtop(nsting1))
 allocate(xbstingbot(nsting2),ybstingbot(nsting2))
 allocate(xbot(uplmt-2),ybot(uplmt-2))
 allocate(xblade(np),yblade(np))
 do i = 1, uplmt-3
    xbot(i) = xb(uplmt+2+i)
    ybot(i) = yb(uplmt+2+i)
 enddo
 xbot(uplmt-2) = xb(1)
 ybot(uplmt-2) = yb(1)
 !slopes of the lines defining the STING
 slopetop = (yb(uplmt-2) - yb(uplmt-1))/(xb(uplmt-2) - xb(uplmt-1))
 slopebot = (yb(uplmt+2) - yb(uplmt+1))/(xb(uplmt+2) - xb(uplmt+1))
 !Y-intercept of the lines defining the sting
 ctop = yb(uplmt-1) - slopetop*xb(uplmt-1)
 cbot = yb(uplmt+1) - slopebot*xb(uplmt+1)
 ! To obtain points on this line Do y(i) = mx(i) + c 
 do i = 1, nstingtop
    xbstingtop(i) = xb(uplmt-1) + (i-1)*stingtop_cellsize
    ybstingtop(i) = slopetop*xbstingtop(i) + ctop
 enddo
 do i = 1, nstingbot     !
    xbstingbot(i) = xb(uplmt+1) + (i-1)*stingbot_cellsize
    ybstingbot(i) = slopebot*xbstingbot(i) + cbot
 enddo
 ybstingtop(nstingtop) = yb(uplmt-2)
 ybstingbot(nstingbot) = yb(uplmt+2)
 !Modified blade for debugging
 ! write(*,*)
 ! print*,'Writing the modified blade...'
 ! write(*,*)
if (allocated(xst)) deallocate(xst)
if (allocated(yst)) deallocate(yst)
 allocate(xst(le_pos-1),yst(le_pos-1))
 do i = 1,le_pos-1
    xst(i) = xb(uplmt-i)
    yst(i) = yb(uplmt-i)
 enddo
 do i = 1, le_pos-3     
    xbstingtop(nstingtop+i) = xst(i+2)
    ybstingtop(nstingtop+i) = yst(i+2)
 enddo
 do i = 1,le_pos-1
    xst(i) = xb(uplmt+i)
    yst(i) = yb(uplmt+i)
 enddo
 do i = 1, le_pos-3     
    xbstingbot(nstingbot+i) = xst(i+2)
    ybstingbot(nstingbot+i) = yst(i+2)
 enddo   
!TE to LE attachment for top curve
 do i = 1, uplmt-2
    xblade(i) = xb(i)
    yblade(i) = yb(i) 
 enddo
 ! Sting top line
 do i = 1, nstingtop
    xblade(uplmt+nstingtop-i) = xbstingtop(i)
    yblade(uplmt+nstingtop-i) = ybstingtop(i)
 enddo
 ! Sting bottom line
 do i = 1, nstingbot
    xblade(uplmt+nstingbot+i) = xbstingbot(i)
    yblade(uplmt+nstingbot+i) = ybstingbot(i)
 enddo
 !at the sting start.
 xblade(uplmt+nstingtop) = xb(uplmt)
 yblade(uplmt+nstingtop) = yb(uplmt)
 !
 xblade(uplmt+nstingbot) = xb(uplmt)
 yblade(uplmt+nstingbot) = yb(uplmt)
 !from LE attachment to TE on bottom curve
 do i = 1, uplmt-2
    xblade(uplmt+nstingbot+2+i) = xbot(i)
    yblade(uplmt+nstingbot+2+i) = ybot(i)
 enddo
 !---------------------------------------------------------------------------
 !---------------------------------------------------------------------------
 !Offsetting the coordinates of STINGTOP for the C grid
 !---------------------------------------------------------------------------
if (allocated(Xgstingtop)) deallocate(Xgstingtop)
if (allocated(Ygstingtop)) deallocate(Ygstingtop)
 allocate(Xgstingtop(nsting1,jmax1),Ygstingtop(nsting1,jmax1))
 !Splining the blade coordinates
 call arclength(xbstingtop(1),ybstingtop(1),s(1),nsting1)
 ! Calculating x'(s) and y'(s)
 call spline(xbstingtop,dxds,s,nsting1, 999.0, -999.0)
 call spline(ybstingtop,dyds,s,nsting1, 999.0, -999.0)
 do i = 1, nsting1
    Xgstingtop(i,1) = xbstingtop(i)
    Ygstingtop(i,1) = ybstingtop(i) 
 enddo
 do k = 1, jmax1-1
    !write(temp,*)k
    d_offset = (thkc)/jmax1! 100% 0f max thk
    !offset = (d_offset*k*1.)**2 ! simple stretching
    if(thick_distr == 2)then
      offset = 1 - tanh(pi*(1 - real(k)/(jmax1-1)))/tanh(pi) ! hyperbolic stretching
      offset = offset*cellwidth*2. !hyperbolic stretching. Scaling it.
    else 
      offset = d_offset*(real(etawidth)/100)!0.1!0.08 !uniform spacing in the normal direction for Euler runs
      ! offset = 1 - tanh((pi/1.)*(1 - real(k)/(jmax1-1)))/tanh(pi/1.) ! hyperbolic stretching
      ! offset = offset*d_offset*0.5!cellwidth*0.25 !hyperbolic stretching. Scaling it.
    endif
    ! Calculating offset coordinates
    !-------------------------------------------------------- 
    ! Calculating the normals and offset coordinates.
    ! Xnorm = y'(s)/ sqrt((y'(s))^2 + (x'(s))^2)
    ! Ynorm = x'(s)/ sqrt((y'(s))^2 + (x'(s))^2)
    !-------------------------------------------------------- 
    do i = 1, nsting1
       Dr = ((dyds(i))**2 + (dxds(i))**2)**0.5
       xnorm(i) =  dyds(i)/Dr ! unit normal vector in x.
       ynorm(i) = -dxds(i)/Dr ! unit normal vector in y.
       if(thick_distr == 2)then	  
         xbstingtop(i) = xbstingtop(i) - offset*xnorm(i) ! hyperbolic stretching
         ybstingtop(i) = ybstingtop(i) - offset*ynorm(i) 
       else
         xbstingtop(i) = xbstingtop(i) - offset*xnorm(i) ! uniform spacing
         ybstingtop(i) = ybstingtop(i) - offset*ynorm(i) ! uniform spacing
       endif
    enddo   
    ! Filling the arrays for the grid
    do ii = 1, nsting1
       Xgstingtop(ii,k+1) = xbstingtop(ii)
       Ygstingtop(ii,k+1) = ybstingtop(ii)   
    enddo	   
 enddo 
 !------------------------------------------
 !Grid Refinement for STINGTOP
 !------------------------------------------
 print*,'nstingtop:',nstingtop
 print*,'nstingbot:',nstingbot
 njoint = nsting1!-nstingtop+2
 print*,njoint
 if (allocated(Xgstingdash)) deallocate(Xgstingdash)
 if (allocated(Ygstingdash)) deallocate(Ygstingdash)
 if (allocated(xnew       )) deallocate(xnew       )
 if (allocated(ynew       )) deallocate(ynew       )
 allocate(Xgstingdash(njoint,jmax1),Ygstingdash(njoint,jmax1))
 allocate(xnew(1),ynew(1))
 yoffset = (Ygstingtop(nsting1,jmax1) - Ygstingtop(nsting1,1))/(jmax1-1)
 print*,'njoint-nstingtop, njoint: ',njoint-nstingtop,njoint
 do j = 1, jmax1
    xgtrans = Xgstingtop(nsting1,1) - Xgstingtop(nsting1,j)
    do i = 1, njoint !From left to right.
       Xgstingdash(i,j) = Xgstingtop(nsting1+1-i,1) - xgtrans + (msle - mble)
       Ygstingdash(i,j) = Ygstingtop(nsting1+1-i,1) + (j-1)*yoffset
    enddo    
    !Making the LE grid straight.    
    !Calculating the total x offset for each eta grid line           
    do i = 1, nstingtop
       xoffset = Xgstingdash(njoint-nstingtop+i,1) -  Xgstingdash(njoint-nstingtop+i,j)
       delx = xoffset/(nstingtop - 1)
       delx = (i-1)*delx
       Xgstingdash(njoint-nstingtop+i,j) = Xgstingdash(njoint-nstingtop+i,j) + delx
    enddo
    ! Filling the grid points
    do i = 1, njoint
       Xgstingblade(i,j) = Xgstingdash(njoint+1-i,j)
       Ygstingblade(i,j) = Ygstingdash(njoint+1-i,j)
    enddo
 enddo 
 !
 if(isdev)then
   fname6 = 'topstingcurve.'//trim(fext)//'.x'
   open(unit=18,file=fname6,status='unknown')
   write(18,*)1
   write(18,*)njoint,jmax1
   write(18,*)Xgstingdash(:,:),Ygstingdash(:,:)
   close(18)     
 endif
 !
 deallocate(Xgstingdash,Ygstingdash)
 !deallocate(xnew,ynew)
 !print*,'StingTOP grid Inclination angle: ', inc_angle/dtor, 'degrees.'
 !------------------------------------------------------------------------------
 s = 0. ; dxds = 0. ; dyds = 0. ; xnorm = 0. ; ynorm = 0.
 !---------------------------------------------------------------------------
 !Offsetting the coordinates of STINGBOT for the C grid
 !---------------------------------------------------------------------------
if (allocated(Xgstingbot)) deallocate(Xgstingbot)
if (allocated(Ygstingbot)) deallocate(Ygstingbot)
 allocate(Xgstingbot(nsting2,jmax1),Ygstingbot(nsting2,jmax1))
 !Splining the blade coordinates
 call arclength(xbstingbot(1),ybstingbot(1),s(1),nsting2)
 ! Calculating x'(s) and y'(s)
 call spline(xbstingbot,dxds,s,nsting2, 999.0, -999.0)
 call spline(ybstingbot,dyds,s,nsting2, 999.0, -999.0)
 do i = 1, nsting2
     Xgstingbot(i,1) = xbstingbot(i)
     Ygstingbot(i,1) = ybstingbot(i) 
 enddo
 do k = 1, jmax1-1
    !write(temp,*)k
    d_offset = (thkc)/jmax1! 100% 0f max thk
    !offset = (d_offset*k*1.)**2 ! simple stretching
    if(thick_distr == 2)then
      offset = 1 - tanh(pi*(1 - real(k)/(jmax1-1)))/tanh(pi) ! hyperbolic stretching
      offset = offset*cellwidth*2. !hyperbolic stretching. Scaling it.
    else 
      offset = d_offset*(real(etawidth)/100)!*0.1!0.08 !uniform spacing in the normal direction for Euler runs
      ! offset = 1 - tanh((pi/1.)*(1 - real(k)/(jmax1-1)))/tanh(pi/1.) ! hyperbolic stretching
      ! offset = offset*d_offset*0.5!cellwidth*0.25 !hyperbolic stretching. Scaling it.
    endif
    ! Calculating offset coordinates
    !-------------------------------------------------------- 
    ! Calculating the normals and offset coordinates.
    ! Xnorm = y'(s)/ sqrt((y'(s))^2 + (x'(s))^2)
    ! Ynorm = x'(s)/ sqrt((y'(s))^2 + (x'(s))^2)
    !-------------------------------------------------------- 
    do i = 1, nsting2
       Dr = ((dyds(i))**2 + (dxds(i))**2)**0.5
       xnorm(i) =  dyds(i)/Dr ! unit normal vector in x.
       ynorm(i) = -dxds(i)/Dr ! unit normal vector in y.
       if(thick_distr == 2)then	  
         xbstingbot(i) = Xgstingbot(i,1) + offset*xnorm(i) ! hyperbolic or simple stretching
         ybstingbot(i) = Ygstingbot(i,1) + offset*ynorm(i) 
       else
         xbstingbot(i) = xbstingbot(i) + offset*xnorm(i) ! uniform spacing
         ybstingbot(i) = ybstingbot(i) + offset*ynorm(i) ! uniform spacing
         ! xbstingbot(i) = Xgstingbot(i,1) + offset*xnorm(i) ! simple stretch spacing
         ! ybstingbot(i) = Ygstingbot(i,1) + offset*ynorm(i) ! simple stretch spacing
       endif
    enddo  
    ! Filling the arrays for the grid
    do ii = 1, nsting2
       Xgstingbot(ii,k+1) = xbstingbot(ii)
       Ygstingbot(ii,k+1) = ybstingbot(ii)   
    enddo    
 enddo 
 !------------------------------------------
 !Grid Refinement for STINGBOT
 !------------------------------------------
 ! print*,'nstingtop:',nstingtop
 ! print*,'nstingbot:',nstingbot
 njoint = nsting2!-nstingbot+2
 ! print*,njoint
if (allocated(Xgstingdash)) deallocate(Xgstingdash)
if (allocated(Ygstingdash)) deallocate(Ygstingdash)
 allocate(Xgstingdash(njoint,jmax1),Ygstingdash(njoint,jmax1))
 yoffset = (Ygstingbot(nsting2,jmax1) - Ygstingbot(nsting2,1))/(jmax1-1)
   do j = 1, jmax1
    xgtrans = Xgstingbot(nsting2,1) - Xgstingbot(nsting2,j)
    do i = 1, njoint
       Xgstingdash(i,j) = Xgstingbot(nsting2+1-i,1) - xgtrans + msle - mble
       Ygstingdash(i,j) = Ygstingbot(nsting2+1-i,1) + (j-1)*yoffset
    enddo
    !Calculating the total x offset for each eta grid line           
    do i = 1, nstingbot
       xoffset = Xgstingdash(njoint-nstingbot+i,1) -  Xgstingdash(njoint-nstingbot+i,j)
       delx = xoffset/(nstingbot - 1)
       delx = (i-1)*delx
       Xgstingdash(njoint-nstingbot+i,j) = Xgstingdash(njoint-nstingbot+i,j) + delx
    enddo
    ! Filling the grid points
    do i = 1, njoint
       Xgstingblade(nstinggrid+1-i,j) = Xgstingdash(i,j)
       Ygstingblade(nstinggrid+1-i,j) = Ygstingdash(i,j)
    enddo                       
 enddo
 !
 if(isdev)then
   fname6 = 'botstingcurve.'//trim(fext)//'.x'
   open(unit=19,file=fname6,status='unknown')
   write(19,*)1
   write(19,*)njoint,jmax1
   write(19,*)Xgstingdash(:,:),Ygstingdash(:,:)
   close(19)
 endif
 !
 deallocate(xblade,yblade)!,Xgstingblade,Ygstingblade)


return
end subroutine stingLEgrid
!************************************************************************




!*************************************************************************
!***************************************************************************
subroutine offset_points(xnew,ynew,x,y,dxds,dyds,offset,npoints,k,uplmt,le_pos,LE)
implicit none
integer, intent(in) :: k,uplmt,le_pos
integer i,j,nx,npoints,LE,m
parameter(nx=500)
real*8, dimension(npoints)::x,y,xnew,ynew,dxds,dyds,xnorm,ynorm
real*8 xx(npoints,1),yy(npoints,1)
real*8 Dr,offset,subdivide1D
character*80 fname

!LE spline option
if(LE.eq.1)then
 m = uplmt+le_pos
endif
!-------------------------------------------------------- 
! Calculating the normals and offset coordinates.
! Xnorm = y'(s)/ sqrt((y'(s))^2 + (x'(s))^2)
! Ynorm = x'(s)/ sqrt((y'(s))^2 + (x'(s))^2)
!-------------------------------------------------------- 
do i = 1, npoints
 Dr = ((dyds(i))**2 + (dxds(i))**2)**0.5
 !print*,'Dr',Dr
 ! normals are calculated counter clockwise from TE-LE-TE
 xnorm(i) =  dyds(i)/Dr ! unit normal vector in x.
 ynorm(i) = -dxds(i)/Dr ! unit normal vector in y.
 if((LE.eq.1).and.(k.ge.3))then
     ! Normal vectors on the bottom curve near LE
     ! 2 points after LE attachment to bot curve and 6 points before that.
        ! X coordinates
        xnorm(m-2) = subdivide1D(xnorm(m-6),xnorm(m+2),0.50)
        xnorm(m-4) = subdivide1D(xnorm(m-6),xnorm(m-2),0.50)
        xnorm(m-5) = subdivide1D(xnorm(m-6),xnorm(m-4),0.50)
        xnorm(m-3) = subdivide1D(xnorm(m-4),xnorm(m-2),0.50)
        xnorm(m  ) = subdivide1D(xnorm(m-2),xnorm(m+2),0.50)
        xnorm(m-1) = subdivide1D(xnorm(m-2),xnorm(m  ),0.50)
        xnorm(m+1) = subdivide1D(xnorm(m  ),xnorm(m+2),0.50)
        ! Y coordinates
        ynorm(m-2) = subdivide1D(ynorm(m-6),ynorm(m+2),0.50)
        ynorm(m-4) = subdivide1D(ynorm(m-6),ynorm(m-2),0.50)
        ynorm(m-5) = subdivide1D(ynorm(m-6),ynorm(m-4),0.50)
        ynorm(m-3) = subdivide1D(ynorm(m-4),ynorm(m-2),0.50)
        ynorm(m  ) = subdivide1D(ynorm(m-2),ynorm(m+2),0.50)
        ynorm(m-1) = subdivide1D(ynorm(m-2),ynorm(m  ),0.50)
        ynorm(m+1) = subdivide1D(ynorm(m  ),ynorm(m+2),0.50)
 endif
 xnew(i) = x(i) + offset*xnorm(i) 
 ynew(i) = y(i) + offset*ynorm(i) 
enddo
 ! fname = 'normalxy.x'
 ! if(k.eq.5)then
   ! !print*,'k in offset: ',k
   ! !call fileWrite1D(fname,xnorm,ynorm,npoints)
   ! do i = 1, npoints
      ! xx(i,1) = xnorm(i)
      ! yy(i,1) = ynorm(i)
   ! enddo
   ! call plot2D(fname,xx,yy,npoints,1)
 ! endif

return
end subroutine
!*************************************************************************

!*************************************************************************
!*************************************************************************
real*8 function subdivide1D(num1,num2,ratio)
implicit none
real*8 num1(1),num2(1), ratio

subdivide1D = ratio*(num1(1) + num2(1))

end
!*************************************************************************

!*************************************************************************
!*************************************************************************
real*8 function subdivide2D(num1,num2,ratio)
implicit none
real*8 num1(1,1),num2(1,1),ratio

subdivide2D = ratio*(num1(1,1) + num2(1,1))

end
!*************************************************************************

!*************************************************************************
!*************************************************************************
subroutine fileWriteMatrix(fname,X,Y,nx,ny)
!-------------------------------------------------------
! Writes X(:,:), Y(:,:) coordinates to a specified file.
! Writes it in the matrix form. First X coordinates
! Second y coordinates
!-------------------------------------------------------
implicit none
character*32, intent(in) :: fname
integer, intent(in) :: nx,ny
integer i
real*8, dimension(nx,ny), intent(in):: X,Y

open(1,file=fname,status='unknown')
write(1,*)'skip'
write(1,*)nx,ny
!do j = 1, jmax
   do i = 1, nx
      write(1,*)X(i,:)
   enddo
print*,""
write(1,*)'Y coordinates'
do i = 1, nx
   write(1,*)Y(i,:)
enddo
!enddo
close(1)
write(*,*)

return
end subroutine
!*************************************************************************

!*************************************************************************
!*************************************************************************
subroutine fileWrite1D(fname,X,Y,n)
!-------------------------------------------------------
! Writes X(:,:), Y(:,:) coordinates to a specified file.
! Writes it in the matrix form. First X coordinates
! Second y coordinates
!-------------------------------------------------------
implicit none
character*80, intent(in) :: fname
integer, intent(in) :: n
integer i
real*8, dimension(n), intent(in):: X,Y

open(1,file=fname,status='unknown')
!write(1,*)'skip'
!write(1,*)'skip'
do i = 1, n
   write(1,'(2F20.16)')X(i),Y(i)
enddo

close(1)
write(*,*)

return
end subroutine
!*************************************************************************

!*************************************************************************
!*************************************************************************
subroutine plot2D(fname,x,y,nx,ny)
implicit none
character*32, intent(in) :: fname
integer, intent(in) :: nx,ny
integer i,j
real*8, dimension(nx,ny), intent(in) :: x,y

open(1,file=fname,status='unknown')
write(1,*)1
write(1,*)nx,ny
write(1,*)x(:,:),y(:,:)
close(1)

return
end subroutine
!*************************************************************************

!*************************************************************************
!*************************************************************************
subroutine ellipdata(xellip,yellip,xb,yb,np,uplmt)
implicit none
integer i,j,k,np,uplmt
real*8 aelp,belp,help,kelp,delta
real*8, intent(in):: xb(np),yb(np)
real*8, intent(out):: xellip(np),yellip(np)
character*80 fname

xellip = xb
aelp = 0.; belp = 0.; help = 0.; kelp = 0.
!Major axis length
aelp = 0.5!0.5*sqrt((xb(np) - xb(uplmt))**2 + ((yb(np) - yb(uplmt))**2))
belp = aelp
help = 0.5*(xb(np) + xb(uplmt))
kelp = 0.5*(yb(np) + yb(uplmt))
!
delta = (xb(np) - xb(uplmt))/(uplmt-1)
do i = 1, uplmt
xellip(i) = xb(1) - (i-1)*delta
enddo
yellip = kelp + sqrt((belp)**2 - ((xellip - help)*(belp/aelp))**2)
do i = 1, uplmt
   yellip(i) = - yellip(i)
enddo
fname = "ellipblade.dat"
call fileWrite1D(fname,xellip,yellip,np)

return
end subroutine
!*************************************************************************

!*************************************************************************
!*************************************************************************
real*8 function ycircle(x,y,radius)
implicit none
real*8, intent(in)::x(1),y(1),radius

!x = 

end 

!*************************************************************************
 ! ! do j = 2, jmax1
     ! ! ispline = j!jmax1-1
     ! ! aellp = 0.; bellp = 0.; hellp = 0.; kellp = 0.
     ! ! !Replacing the LE grid with an ellipse shaped grid
     ! ! !Major axis length
     ! ! bellp = 0.5*(sqrt((Xg(uplmt+le_pos-1,ispline) - Xg(uplmt-le_pos+1,ispline))**2 + (Yg(uplmt+le_pos-1,ispline) - Yg(uplmt-le_pos+1,ispline))**2))
     ! ! !
     ! ! !center of the ellipse
     ! ! hellp = 0.5*(Xg(uplmt+le_pos,ispline) + Xg(uplmt-le_pos,ispline))
     ! ! kellp = 0.5*(Yg(uplmt+le_pos,ispline) + Yg(uplmt-le_pos,ispline))
     ! ! !
     ! ! !Minor axis length
     ! ! aellp = 1*(sqrt((Xg(uplmt,ispline) - hellp)**2 + (Yg(uplmt,ispline) - kellp)**2))
     ! ! !Circle aellp = bellp
     ! ! !aellp = bellp
     ! ! !--------------------------------------------------------------
     ! ! ! Calculating the outer grid points on an ellipse
     ! ! ! y = k + sqrt(b^2 - ((x-h)*(b/a))^2)
     ! ! delta = (Xg(uplmt+le_pos,ispline) - Xg(uplmt,ispline))/(le_pos)
     ! ! do i = 1, le_pos + 1
        ! ! Xg(uplmt - 1 + i,ispline) = Xg(uplmt,ispline) + (i-1)*(delta)
     ! ! enddo
     ! ! !
     ! ! delta = (Xg(uplmt-le_pos,ispline) - Xg(uplmt,ispline))/(le_pos)
     ! ! do i = 2, le_pos + 1
        ! ! Xg(uplmt + 1 - i,ispline) = Xg(uplmt,ispline) + (i-1)*(delta)
     ! ! enddo 
     ! ! Yg(uplmt-le_pos:uplmt+le_pos,ispline) = kellp + sqrt((bellp)**2 - ((Xg(uplmt-le_pos:uplmt+le_pos,ispline) - hellp)*(bellp/aellp))**2)
     ! ! !
     ! ! Yg(uplmt-le_pos,ispline) = 2*kellp - Yg(uplmt-le_pos,ispline)
        ! ! ! fname1 = 'LEellipshape.'//trim(fext)
        ! ! ! open(1,file=fname1,status='unknown')
        ! ! ! write(1,*)'eta:',ispline
        ! ! ! write(1,*)Xg(uplmt-le_pos-2,ispline),Yg(uplmt-le_pos-2,ispline)
        ! ! ! write(1,*)Xg(uplmt-le_pos-1,ispline),Yg(uplmt-le_pos-1,ispline)
        ! ! ! write(1,*)Xg(uplmt-le_pos,ispline),2*kellp - Yg(uplmt-le_pos,ispline)
        ! ! do i = uplmt-le_pos+1, uplmt!+le_pos+2
           ! ! ! write(1,*)Xg(i,ispline),(- Yg(i,ispline) + 2*kellp)
           ! ! Yg(i,ispline) = 2*kellp - Yg(i,ispline)
        ! ! enddo
        ! ! do i = uplmt+1, uplmt+le_pos!+le_pos+2
           ! ! ! write(1,*)Xg(i,ispline),Yg(i,ispline)!+2*kellp
        ! ! enddo
        ! ! ! write(1,*)Xg(uplmt+le_pos+1,ispline),Yg(uplmt+le_pos+1,ispline)
        ! ! ! write(1,*)Xg(uplmt+le_pos+2,ispline),Yg(uplmt+le_pos+2,ispline)
        ! ! ! close(1)
 ! ! enddo
