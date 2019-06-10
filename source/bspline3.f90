subroutine bspline3(xcp,ycp,ncp,x,y,nspan,ia)
!xcp,ycp: control points
!xb3,yb3: interpolated spline points
!t: parameter value ( 0<t<1)
!ncp: number of control points

implicit none

integer ncp,ncp1,i,j,k,np,nx,nax,nbsp,nspan,ia,ib
real*8 xcp(ncp),ycp(ncp)
!real*8 xb3(1000),yb3(1000)
real*8 xc(ncp+2),yc(ncp+2)
real*8 xs(1000),ys(1000)
!real*8 xmin(1000),xmax(1000)
real*8 xmax,xmin,xint

real*8 t(1000), T1(1000), T2(1000), T3(1000), T4(1000)


parameter(np=50,nx=1000,nax=100)
real*8 xbs(nx),ybs(nx),min,max
real*8 x(nspan),y(nspan)
!print*,' Control points for bspline curve from Input are:'
!print*,ncp
!do i=1,ncp
!   write(*,*)ycp(i),xcp(i)
!enddo
!
!nspan = na
!print*,'span:',span(ia)
!write(*,*)
t(1) = 0.
t(np) = 1.0
do i=2,np-1
  t(i) = t(i-1) + (1.0/np)
  T1(i) = ((-t(i)**3) + (3*t(i)**2) - (3*t(i)) + 1)/6
  T2(i) = ((3*t(i)**3) - (6*t(i)**2) + 4)/6
  T3(i) = ((-3*t(i)**3) + (3*t(i)**2) + (3*t(i)) + 1)/6
  T4(i) = (t(i)**3)/6
enddo
! Making the start and end points as collocation points----
! Start point: Considering the 1st CP as the mid point of the 2nd CP and the start point----------
xc(1) = 2*xcp(1) - xcp(2)
yc(1) = 2*ycp(1) - ycp(2)
! End point: Considering the last CP as the mid point of the end point and the penultimate CP ----
xc(ncp+2) = 2*xcp(ncp) - xcp(ncp-1)
yc(ncp+2) = 2*ycp(ncp) - ycp(ncp-1)
do i=1,ncp
  xc(i+1) = xcp(i)
  yc(i+1) = ycp(i)
enddo
!write(*,*)
!print*,'New control points with start and end points as collocation points:'
!do i=1,ncp+2
! print*,yc(i),xc(i)
!enddo
ncp1 = ncp + 2 ! includes the start and end points
!print*,'ncp-new:',ncp1
nbsp = (np - 2)*(ncp1 - 3)
!print*,'nbsp:',nbsp
!--------------------------------------------------------------------------------------------------
!constructing the bezier curve(cubic spline) using 4 points P0(x0,y0),P1(x1,y1),P2(x2,y2),P3(x3,y3)------
!-----B(t) =(((1-t)^3)*P0) + (3*((1-t)^2)*t*P1) + (3*(1-t)*(t^2)*P1) + ((t^3)*P2); 0.le.t.le.1 ---------
!write(*,*)
!print*,'Curve points for Bspline curve of degree 3:'
!print*,np
!write(*,*)
 xs(1) = 0.
 ys(1) = 0.
xbs(1) = xcp(1)
ybs(1) = ycp(1)
xbs(nbsp+2) = xcp(ncp)
ybs(nbsp+2) = ycp(ncp)
do j=1,ncp1-3
 do i=2,np-1
  xs(i) = (xc(j)*T1(i)) + (xc(j+1)*T2(i)) + (xc(j+2)*T3(i)) + (xc(j+3)*T4(i))
  ys(i) = (yc(j)*T1(i)) + (yc(j+1)*T2(i)) + (yc(j+2)*T3(i)) + (yc(j+3)*T4(i))
  k = i + (np - 2)*(j - 1)! Transforming from 2D array to 1D array
  xbs(k) = xs(i)
  ybs(k) = ys(i)
!  print*,ybs(k),xbs(k)
 enddo
enddo
!write(*,*) 
!Finding the intersection of the line and the 2D B-spline curve------
!print*,'span  deltatheta'
do ib = 1, nspan
! print*,'span:',span(ia)
 do i = 1, nbsp + 1
!  print*,xbs(i),xbs(i+1)
  xint = xbs(i+1) + (ybs(i+1) - x(ib))*(((xbs(i) - xbs(i+1))/(ybs(i+1) - ybs(i))))
!  print*,'xint',xint
  xmax = max(xbs(i),xbs(i+1))
  xmin = min(xbs(i),xbs(i+1))
!  print*,xmin,xmax
!  xmin1 = xmin(i)
!  xmax1 = xmax(i)
  if (xint.ge.xmin.and.xint.le.xmax)y(ib) = xint
 enddo
!write(*,*)
!print*,xmin,xmax
! print*,y(ia)
enddo 

return
end subroutine bspline3

!*********************************************************************************
        real*8 function bspline(cp,t)  
        implicit none
        real*8 ,dimension(4), intent (in) :: cp
        real*8 ,intent (in) :: t
        real*8 B1,B2,B3,B4       
       
         B1 = ((-t**3) + (3*t**2) - (3*t) + 1)/6
         B2 = ((3*t**3) - (6*t**2) + 4)/6
         B3 = ((-3*t**3) + (3*t**2) + (3*t) + 1)/6
         B4 = (t**3)/6

         bspline =cp(1)*B1+cp(2)*B2+cp(3)*B3+cp(4)*B4
        
        end function     

!----------------------------------------------------------------------
        real*8 function d_bspline(cp,t)  
        implicit none
        real*8 ,dimension(4), intent (in) :: cp
        real*8 ,intent (in) :: t
        real*8 B1,B2,B3,B4       
       
         B1 = ((-3*t**2) + (6*t) - 3)/6
         B2 = ((9*t**2) - 12*t)/6
         B3 = ((-9*t**2) + (6*t) + 3)/6
         B4 = (3*t**2)/6

         d_bspline = cp(1)*B1+cp(2)*B2+cp(3)*B3+cp(4)*B4
        
        end function
!----------------------------------------------------------------------
        real*8 function dd_bspline(cp,t)  
        implicit none
        real*8 ,dimension(4), intent (in) :: cp
        real*8 ,intent (in) :: t
        real*8 B1,B2,B3,B4       
       
         B1 = ((-6*t) + 6)/6
         B2 = ((18*t) - 12)/6
         B3 = ((-18*t) + 6)/6
         B4 = (6*t)/6

         dd_bspline = cp(1)*B1+cp(2)*B2+cp(3)*B3+cp(4)*B4
        
        end function    
        
!----------------------------------------------------------------------
        real*8 function d3_bspline(cp,t)  
        implicit none
        real*8 ,dimension(4), intent (in) :: cp
        real*8 ,intent (in) :: t
        real*8 B1,B2,B3,B4       
       
         B1 = -1
         B2 = 3
         B3 = -3
         B4 = 1

         d3_bspline = cp(1)*B1+cp(2)*B2+cp(3)*B3+cp(4)*B4
        
        end function    
!----------------------------------------------------------------------
        subroutine bspline_arclength(arclength,xcp,ycp,ncp,degree)
        implicit none
        integer, intent(in) :: ncp,degree
        real*8 ,dimension(ncp-degree+1), intent (out) :: arclength 
        real*8 ,dimension(ncp), intent (in) :: xcp,ycp
        real*8 ,dimension(29) :: GQx, GQw
        real*8 d_bspline4, d_bspline, dxdt, dydt, S
        integer i, j
       
        
        ! Initializing dydt
        dxdt    = 0.0
        dydt    = 0.0
        
        GQx = (/-0.9966794422605956, -0.9825455052614139, -0.9572855957780887, -0.9211802329530591, -0.874637804920103,  & 
                -0.8181854876152532, -0.752462851734477 , -0.6782145376026867, -0.5962817971382289, -0.5075929551242282, & 
                -0.413152888174008 , -0.3140316378676399, -0.2113522861660013, -0.1062782301326792,  0.0000000000000002, &
                 0.106278230132679 ,  0.2113522861660009,  0.3140316378676405,  0.4131528881740089,  0.5075929551242276, &
                 0.5962817971382278,  0.6782145376026861,  0.7524628517344769,  0.8181854876152522,  0.8746378049201026, &
                 0.9211802329530594,  0.9572855957780867,  0.9825455052614134,  0.9966794422605948/)
        GQw = (/ 0.0085169038787468,  0.0197320850561252,  0.0307404922020909,  0.041402062518683 ,  0.0515948269024977, &
                 0.0612030906570802,  0.0701179332550491,  0.0782383271357671,  0.0854722573661736,  0.0917377571392597, &
                 0.0969638340944074,  0.1010912737599152,  0.1040733100777303,  0.1058761550973202,  0.1064793817183146, &
                 0.1058761550973217,  0.1040733100777293,  0.1010912737599157,  0.0969638340944089,  0.0917377571392591, &
                 0.085472257366172 ,  0.0782383271357633,  0.0701179332550512,  0.06120309065708  ,  0.0515948269024958, &
                 0.0414020625186843,  0.0307404922020949,  0.0197320850561219,  0.0085169038787447/)

        GQx = 0.5*(GQx + 1)
        
        arclength(1) = 0
        do j = 1, ncp-degree
            arclength(j+1) = arclength(j)
            do i = 1, 29
                if (degree == 3) then
                  dxdt = d_bspline(xcp(j:j+degree),GQx(i))
                  dydt = d_bspline(ycp(j:j+degree),GQx(i))
        elseif (degree == 4) then
                  dxdt = d_bspline4(xcp(j:j+degree),GQx(i))
                  dydt = d_bspline4(ycp(j:j+degree),GQx(i))
        endif
                S = sqrt(dxdt**2 + dydt**2)
                arclength(j+1) = arclength(j+1) + GQw(i)*S
            enddo
        enddo
        
!        do j = 1, ncp-3
!            dxdt = bspline(xcp(j:j+3),1.)-bspline(xcp(j:j+3),0.)
!            dydt = bspline(ycp(j:j+3),1.)-bspline(ycp(j:j+3),0.)
!            S = sqrt(dxdt**2 + dydt**2)
!            print*, "S", dxdt, dydt, S
!            arclength(j+1) = arclength(j) + S
!        enddo        
        
        !Normalize the arc length
        arclength = arclength/arclength(ncp-degree+1)
        
        end subroutine
        
!-----------------------------------------------------------------------
        subroutine bspline_jt(j,t,arclength,ncp,degree,s)  
        implicit none
        integer, intent(in) :: ncp, degree
        integer, intent(out) :: j
        real*8 ,intent (out) :: t
        real*8 , dimension(ncp-degree+1) ,intent (in) :: arclength
        real*8 ,intent (in) :: s
        integer :: ns
        
        ns = ncp - degree !number of segmenst
        
        if( s <= 0 ) then
          t = 0
          j = 1
          !print*, "t, s, j", t, s, 1, arclength(j)
          return
        elseif( s >= 1 ) then
          t = 1
          j = ns
          !print*, "t, s, j", t, s, ns, arclength(j+1)
          return
        end if
                
         do j = 1, ns
            if ( (arclength(j) <= s) .and.  (s <= arclength(j+1)) ) then
             t = (s - arclength(j))/( (arclength(j+1)-arclength(j)) )
             !print*, "t, s, j", t, s, j, arclength(j), arclength(j+1)
             return
            endif
         enddo
        end subroutine    
        
!-----------------------------------------------------------------------
        real function bspline_cp(cp,arclength,ncp,degree,s)  
            implicit none
            real*8, dimension(ncp),  intent (in) :: cp
            real*8, dimension(ncp-2),intent (in) :: arclength
            integer, intent(in) :: ncp,degree
            real*8 ,intent (in) :: s
            integer :: j
            real*8 :: t, bspline, bspline4
           
            call bspline_jt(j,t,arclength,ncp,degree,s)  
            if( degree == 3) bspline_cp = bspline(cp(j:j+degree),t)
            if( degree == 4) bspline_cp = bspline4(cp(j:j+degree),t)

        end function    
    
!-----------------------------------------------------------------------
        real function d_bspline_cp(cp,arclength,ncp,degree,s)  
            implicit none
            real*8, dimension(ncp), intent (in) :: cp
            real*8 , dimension(ncp-2) ,intent (in) :: arclength
            integer, intent(in) :: ncp,degree
            real*8 ,intent (in) :: s
            integer :: j
            real*8 :: t, d_bspline, d_bspline4
            
            call bspline_jt(j,t,arclength,ncp,degree,s)  
            if( degree == 3) d_bspline_cp = d_bspline(cp(j:j+degree),t)
            if( degree == 4) d_bspline_cp = d_bspline4(cp(j:j+degree),t)

        end function   
    
!-----------------------------------------------------------------------
        real function dd_bspline_cp(cp,arclength,ncp,degree,s)  
            implicit none
            real*8, dimension(ncp), intent (in) :: cp
            real*8 , dimension(ncp-2) ,intent (in) :: arclength
            integer, intent(in) :: ncp,degree
            real*8 ,intent (in) :: s
            integer :: j
            real*8 :: t, dd_bspline, dd_bspline4
            
            call bspline_jt(j,t,arclength,ncp,degree,s)  
            if( degree == 3) dd_bspline_cp = dd_bspline(cp(j:j+degree),t)
            if( degree == 4) dd_bspline_cp = dd_bspline4(cp(j:j+degree),t)

        end function
    
!-----------------------------------------------------------------------
        real function d3_bspline_cp(cp,arclength,ncp,degree,s)  
            implicit none
            real*8, dimension(ncp), intent (in) :: cp
            real*8 , dimension(ncp-2) ,intent (in) :: arclength
            integer, intent(in) :: ncp,degree
            real*8 ,intent (in) :: s
            integer :: j
            real*8 :: t, d3_bspline, d3_bspline4
            
            call bspline_jt(j,t,arclength,ncp,degree,s)  
            if( degree == 3) d3_bspline_cp = d3_bspline(cp(j:j+degree),t)
            if( degree == 4) d3_bspline_cp = d3_bspline4(cp(j:j+degree),t)

        end function
        
!-----------------------------------------------------------------------
    real*8 function d4_bspline_cp(cp,arclength,ncp,degree,s)  
    implicit none
    real*8, dimension(ncp), intent (in) :: cp
    real*8 , dimension(ncp-2) ,intent (in) :: arclength
    integer, intent(in) :: ncp,degree
    real*8 ,intent (in) :: s
    integer :: j
    real*8 :: t, d4_bspline4
    
    call bspline_jt(j,t,arclength,ncp,degree,s)  
    if( degree == 3) d4_bspline_cp = 0
    if( degree == 4) d4_bspline_cp = d4_bspline4(cp(j:j+degree),t)

    end function
            
!-----------------------------------------------------------------------
subroutine bspline_y_of_x( y, x, np, xcp, ycp, ncp, degree ) 

implicit none

real*8, dimension(np), intent (out) :: y
real*8, dimension(np), intent (in) :: x
real*8, dimension(ncp), intent (in) :: xcp, ycp
integer, intent(in) :: np, ncp, degree

real*8 , dimension(ncp-degree+1) :: x_spl_end,y_spl_end
real*8 :: bspline, bspline_t_newton
real*8 :: bspline4, bspline4_t_newton
real*8 :: t,tolerance
integer :: i, j, seg_1, seg_end

!--------------------------------------------------------  
! computing the end points for each segment:

t = 0
if( degree == 3) then
    x_spl_end(1) = bspline(xcp(1:degree+1),t)
elseif (degree == 4) then
    x_spl_end(1) = bspline4(xcp(1:degree+1),t)
    y_spl_end(1) = bspline4(ycp(1:1+degree),t)
endif
t = 1
do j = 1,ncp-degree
   if( degree == 3) then
     x_spl_end(j+1) = bspline(xcp(j:j+degree),t)
   elseif ( degree == 4) then
     x_spl_end(j+1) = bspline4(xcp(j:j+degree),t)
     y_spl_end(j+1) = bspline4(ycp(j:j+degree),t)
   endif
enddo

if( x_spl_end(2) > x_spl_end(1) ) then
    !x is increasing
    seg_1 = 1
    seg_end = ncp-degree
else
    !x is decreasing
    seg_1 = ncp-degree
    seg_end = 1
endif

! print*, "x_spl_end", x_spl_end
! to print thickness multiplier segments
  ! open(unit= 83,file="thick_Multi_segments.txt", form="formatted")
    ! write(83,*),"x_spl_end","y_spl_end"
    ! do i=1,ncp-degree+1
        ! write(83,*), x_spl_end(i),y_spl_end(i)
    ! enddo
  ! close(83)
  ! print*, "x_spl_end", x_spl_end
  ! do i=1,ncp-degree+1
        ! print*, x_spl_end(i),y_spl_end(i)
  ! enddo

!______________________________________________________________________
 tolerance = 1e-16     
do i=1,np
    do j=1,(ncp-degree)
        !print*, "xs < x < xe ", x_spl_end(j), x(i), x_spl_end(j+1)
        if ( ( (x(i) >= x_spl_end(j)) .and. (x(i) <= x_spl_end(j+1)) ) .or. &
             ( (x(i) <= x_spl_end(j)) .and. (x(i) >= x_spl_end(j+1)) ) .or. &
             ( (i == 1) .and. (j == seg_1 ) ).or.&   
             ( (i == np) .and. (j == seg_end ) )) then 
             !print*,'i=',i
            if( degree == 3 )then
                if (((x(i)-x_spl_end(j))< tolerance).and.((x(i)-x_spl_end(j))> -tolerance)) then
                    t = 0
                elseif (((x(i)-x_spl_end(j))> tolerance).and.((x(i)-x_spl_end(j))< -tolerance)) then
                    t = 1
                else
                    t = bspline_t_newton(xcp(j:j+degree), x(i))
                endif
                y(i)= bspline(ycp(j:j+degree), t)
            else if( degree == 4 ) then
                if (((x(i)-x_spl_end(j))< tolerance).and.((x(i)-x_spl_end(j))> -tolerance)) then
                    t = 0
                elseif (((x(i)-x_spl_end(j))> tolerance).and.((x(i)-x_spl_end(j))< -tolerance)) then
                    t = 1
                else
                    t = bspline4_t_newton(xcp(j:j+degree), x(i))
                endif
                y(i)= bspline4(ycp(j:j+degree), t)
            endif
            !print*, "t, i, x, y, j", t, i, x(i), y(i), j
            exit
        end if
    enddo
enddo

end subroutine
!-----------------------------------------------------------------------
! This subroutine is to calculate the spline values for any random intermidiate 
! punch of points without constraining the last point to be at the end if the blade.
subroutine bspline_y_of_x_refine( y, x, np, xcp, ycp, ncp, degree ) 

implicit none

real*8, dimension(np), intent (out) :: y
real*8, dimension(np), intent (in) :: x
real*8, dimension(ncp), intent (in) :: xcp, ycp
integer, intent(in) :: np, ncp, degree

real*8 , dimension(ncp-degree+1) :: x_spl_end
real*8 :: bspline, bspline_t_newton
real*8 :: bspline4, bspline4_t_newton
real*8 :: t
integer :: i, j, seg_1, seg_end

!--------------------------------------------------------  
! computing the end points for each segment:
t = 0
if( degree == 3) then
    x_spl_end(1) = bspline(xcp(1:degree+1),t)
elseif (degree == 4) then
    x_spl_end(1) = bspline4(xcp(1:degree+1),t)
endif
t = 1
do j = 1,ncp-degree
   if( degree == 3) then
     x_spl_end(j+1) = bspline(xcp(j:j+degree),t)
   elseif ( degree == 4) then
     x_spl_end(j+1) = bspline4(xcp(j:j+degree),t)
   endif
enddo

if( x_spl_end(2) > x_spl_end(1) ) then
    !x is increasing
    seg_1 = 1
    seg_end = ncp-degree
else
    !x is decreasing
    seg_1 = ncp-degree
    seg_end = 1
endif

!print*, "x_spl_end", x_spl_end

!______________________________________________________________________
      
do i=1,np
    do j=1,(ncp-degree)
        !print*, "xs < x < xe ", x_spl_end(j), x(i), x_spl_end(j+1)
        if ( ( (x(i) >= x_spl_end(j)) .and. (x(i) <= x_spl_end(j+1)) ) .or. &
             ( (x(i) <= x_spl_end(j)) .and. (x(i) >= x_spl_end(j+1)) ) .or. &
             ( (i == 1) .and. (j == seg_1 ) )) then!.or.&  
             !( (i == np) .and. (j == seg_end ) )) then ! Nemnem 8/3/2013, because it overconstrain the spline to seg_end (not good for refining values)
            
            if( degree == 3 )then
                t = bspline_t_newton(xcp(j:j+degree), x(i))
                y(i)= bspline(ycp(j:j+degree), t)
            else if( degree == 4 ) then
                t = bspline4_t_newton(xcp(j:j+degree), x(i))
                y(i)= bspline4(ycp(j:j+degree), t)
            endif
            !print*, "t, i, x, y, j", t, i, x(i), y(i), j
            exit
        end if
    enddo
enddo

end subroutine       
        
!-----------------------------------------------------------------------
real*8 function bspline_t_newton(cp,u)
    use errors
    implicit none
    integer k
    real*8 ,dimension(4), intent (in) :: cp
    real*8 ,intent (in) :: u
    real*8 B1,B2,B3,B4,d1_B1,d1_B2,d1_B3,d1_B4
    real*8 tt_0, xs_0,d1_xs_0
    character(:),   allocatable :: error_msg, error_arg_1, error_arg_2, &
                                   dev_msg
    ! Newton's method       

    tt_0 = 0.5

    do k =1,100
        ! Basic functions:
        B1 = ((-tt_0**3) + (3*tt_0**2) - (3*tt_0) + 1)/6
        B2 = ((3*tt_0**3) - (6*tt_0**2) + 4)/6
        B3 = ((-3*tt_0**3) + (3*tt_0**2) + (3*tt_0) + 1)/6
        B4 = (tt_0**3)/6               
        ! first derivative:
        d1_B1 = ((-3*tt_0**2) + (6*tt_0) - (3))/6
        d1_B2 = ((9*tt_0**2) - (12*tt_0))/6
        d1_B3 = ((-9*tt_0**2) + (6*tt_0) + (3))/6
        d1_B4 = (3*tt_0**2)/6
        ! xs(t_0)
        xs_0 = cp(1)*B1+cp(2)*B2+cp(3)*B3+cp(4)*B4
        ! d1_xs(t_0)
        d1_xs_0= cp(1)*d1_B1+cp(2)*d1_B2+cp(3)*d1_B3+cp(4)*d1_B4
        ! Newton's interpolation:
        bspline_t_newton = tt_0 + (u-xs_0)/d1_xs_0

        ! Make sure we stay within the bounds 
        if( bspline_t_newton > 1 ) bspline_t_newton = 1
        if( bspline_t_newton < 0 ) bspline_t_newton = 0

        if (abs(u-xs_0)<1e-15) then 
            !print*,"abs(u-xs_0)", abs(u-xs_0), u, xs_0, bspline_t_newton
            return
        endif
        tt_0 = bspline_t_newton
    enddo

    ! Convert real numbers to strings
    write(error_arg_1, '(f20.16)') abs(u - xs_0)
    write(error_arg_2, '(f20.16)') bspline_t_newton
    error_msg   = "Cubic abs(u-xs_0) not converged - "//error_arg_1//' '//error_arg_2
    dev_msg     = 'Check function bspline_t_newton in bspline3.f90'
    call fatal_error(error_msg, dev_msg = dev_msg)

end function
!********************************************************************************
!*********************************************************************************
!************Quartic B spline functions*********************
        real*8 function bspline4(cp,t)  
        implicit none
        real*8 ,dimension(5), intent (in) :: cp
        real*8 ,intent (in) :: t
        real*8 B1,B2,B3,B4,B5      
       
         B1 = ((1-t)**4)/24
         B2 = ((-4*t**4) + (12*t**3) + (-6*t**2) + (-12*t) + 11)/24
         B3 = ((6*t**4) + (-12*t**3) + (-6*t**2) + (12*t) + 11)/24
         B4 = ((-4*t**4) + (4*t**3) + (6*t**2) + (4*t) + 1)/24
         B5 = (t**4)/24

         bspline4 =cp(1)*B1+cp(2)*B2+cp(3)*B3+cp(4)*B4+cp(5)*B5
        
        end function     

!----------------------------------------------------------------------
        real*8 function d_bspline4(cp,t)  
        implicit none
        real*8 ,dimension(5), intent (in) :: cp
        real*8 ,intent (in) :: t
        real*8 B1,B2,B3,B4,B5 
       
         B1 = t**3/6 - t**2/2 + t/2 - 1/6.
         B2 = -2*t**3/3 + 3*t**2/2 - t/2 - 1/2.
         B3 = t**3 - 3*t**2/2 - t/2 + 1/2.
         B4 = -2*t**3/3 + t**2/2 + t/2 + 1/6.
         B5 = t**3/6
         
         d_bspline4 = cp(1)*B1+cp(2)*B2+cp(3)*B3+cp(4)*B4+cp(5)*B5
        
        end function
!----------------------------------------------------------------------
        real*8 function dd_bspline4(cp,t)  
        implicit none
        real*8 ,dimension(5), intent (in) :: cp
        real*8 ,intent (in) :: t
        real*8 B1,B2,B3,B4,B5  
       
         B1 = (12*(1-t)**2)/24
         B2 = ((-48*t**2) + (72*t) + (-12))/24
         B3 = ((72*t**2) + (-72*t) + (-12))/24
         B4 = ((-48*t**2) + (24*t) + (12))/24
         B5 = (12*t**2)/24
 
         dd_bspline4 = cp(1)*B1+cp(2)*B2+cp(3)*B3+cp(4)*B4+cp(5)*B5
        
        end function    
        
!----------------------------------------------------------------------
        real*8 function d3_bspline4(cp,t)  
        implicit none
        real*8 ,dimension(5), intent (in) :: cp
        real*8 ,intent (in) :: t
        real*8 B1,B2,B3,B4,B5       
       
         B1 = (-24*(1-t))/24
         B2 = ((-96*t) + (72))/24
         B3 = ((144*t) + (-72))/24
         B4 = ((-96*t) + (24))/24
         B5 = (24*t)/24
        
         d3_bspline4 = cp(1)*B1+cp(2)*B2+cp(3)*B3+cp(4)*B4+cp(5)*B5
        end function    
!------------------------------------------------------------

        real*8 function d4_bspline4(cp,t)  
        implicit none
        real*8 ,dimension(5), intent (in) :: cp
        real*8 ,intent (in) :: t
        real*8 B1,B2,B3,B4,B5       
       
         B1 = 1
         B2 = -4
         B3 = 6
         B4 = -4
         B5 = 1
 
         d4_bspline4 = cp(1)*B1+cp(2)*B2+cp(3)*B3+cp(4)*B4+cp(5)*B5
 
        end function    
!------------------------------------------------------------
real*8 function bspline4_t_newton(cp,u)
    use errors
    implicit none
    integer k
    real*8 ,dimension(5), intent (in) :: cp
    real*8 ,intent (in) :: u
    real*8 :: bspline4, d_bspline4
    real*8 tt_0, xs_0,d1_xs_0
    character(:),   allocatable :: error_msg, error_arg_1, error_arg_2, &
                                   dev_msg
    ! Newton's method       
    tt_0 = 0.5

    do k =1,100
        ! xs(t_0)
        xs_0 = bspline4(cp,tt_0)
        ! d1_xs(t_0)
        d1_xs_0= d_bspline4(cp,tt_0)
        
        ! Newton's interpolation:
        bspline4_t_newton = tt_0 + (u-xs_0)/d1_xs_0

        ! Make sure we stay within the bounds 
        if( bspline4_t_newton > 1 ) bspline4_t_newton = 1
        if( bspline4_t_newton < 0 ) bspline4_t_newton = 0

        if (abs(u-xs_0)<1e-15) then                
           !print*,"abs(u-xs_0)", abs(u-xs_0), u, xs_0, bspline4_t_newton
           return
        endif
        tt_0 = bspline4_t_newton
    enddo

    ! Convert real numbers to strings
    write(error_arg_1, '(f20.16)') abs(u - xs_0)
    write(error_arg_2, '(f20.16)') bspline4_t_newton
    error_msg   = "Quartic abs(u-xs_0) not converged - "//error_arg_1//" "//error_arg_2
    dev_msg     = 'Check function bspline4_t_newton in bspline3.f90'
    call fatal_error(error_msg, dev_msg = dev_msg)

end function

subroutine bspline4_Beval(t, B)

real ,intent (in) :: t
real :: B(5)      

B(1) = ((1-t)**4)/24
B(2) = ((-4*t**4) + (12*t**3) + (-6*t**2) + (-12*t) + 11)/24
B(3) = ((6*t**4) + (-12*t**3) + (-6*t**2) + (12*t) + 11)/24
B(4) = ((-4*t**4) + (4*t**3) + (6*t**2) + (4*t) + 1)/24
B(5) = (t**4)/24

end subroutine bspline4_Beval

subroutine d_bspline4_Beval(t, B)

real ,intent (in) :: t
real :: B(5)      

B(1) = t**3/6 - t**2/2 + t/2 - 1/6.
B(2) = -2*t**3/3 + 3*t**2/2 - t/2 - 1/2.
B(3) = t**3 - 3*t**2/2 - t/2 + 1/2.
B(4) = -2*t**3/3 + t**2/2 + t/2 + 1/6.
B(5) = t**3/6

end subroutine d_bspline4_Beval

subroutine dd_bspline4_Beval(t, B)

real ,intent (in) :: t
real :: B(5)      

B(1) = (12*(1-t)**2)/24
B(2) = ((-48*t**2) + (72*t) + (-12))/24
B(3) = ((72*t**2) + (-72*t) + (-12))/24
B(4) = ((-48*t**2) + (24*t) + (12))/24
B(5) = (12*t**2)/24

end subroutine dd_bspline4_Beval
