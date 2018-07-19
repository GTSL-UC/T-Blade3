      subroutine lesting (xtop, ytop,xbot, ybot,dimen, &
                           camber_ang, camber_le, uin_le,le_pos, pi, x_le_spl, y_le_spl, js, nsl,&
	                       s_all, ee_all,C_le_x_top_all,C_le_x_bot_all,C_le_y_top_all,C_le_y_bot_all,sang,&
	                       xcp4_hat_all,xcp6_hat_all,ycp4_hat_all,ycp6_hat_all,ncp,degree,casename,develop, &
                           sting_l_all,sting_h_all)
       
!xcp,ycp: edge control points
!xbs,ybs: spline points
!t: parameter value ( 0<t<1)
!ncp: number of control points
! the coordinate system of the LE spline is in the u-v plane
!"Ahmed Nemnem" 

      implicit none

      integer i,j,k,np_le,n_of_iter,le_pos,js,nsl
	  integer ncp,degree,no_segments
	  integer factorial,dimen,interval,order
	  parameter (no_segments = 2) ! for 2 spine segments
	  ! degree .. is the degree of bspline (3(cubic) or 4(quartic))
	  ! plot ... is the number of section I want to see in output files 
	  real C_le_x_top,C_le_x_bot,C_le_y_top,C_le_y_bot,y_le,xcp4_hat,xcp6_hat,ycp4_hat,ycp6_hat
      real, dimension(ncp) :: xcp_top, ycp_top,xcp_bot,ycp_bot
	  real, dimension((2*(degree+no_segments)),((2*(degree+no_segments))+1)) :: a_top,a_bot
      real x_le_spl(2*le_pos-1),y_le_spl(2*le_pos-1)
	  real x_le_spl_top(le_pos-2),y_le_spl_top(le_pos-2),x_le_spl_bot(le_pos-2),y_le_spl_bot(le_pos-2)
      real u_le,thk_le,slope_le,curv_le,ee
      real ord,s,theta_us,ss,sang
      real, dimension(dimen), intent(inout) :: xtop, ytop
      real, dimension(dimen), intent(inout) :: xbot, ybot
      real xbot_le, ybot_le, slope_le_bot, curv_le_bot, slope_curv_bot
      real xtop_le, ytop_le, slope_le_top, curv_le_top, slope_curv_top
      real camber_ang(dimen),camber_le(dimen),le_camber_ang,cam_le
      real uin_le,le_thk,chrd_le
      real du1,du2,umx,pi
      real s_all(nsl),ee_all(nsl),C_le_y_top_all(nsl),C_le_y_bot_all(nsl),C_le_x_top_all(nsl),C_le_x_bot_all(nsl)
	  real xcp4_hat_all(nsl),xcp6_hat_all(nsl),ycp4_hat_all(nsl),ycp6_hat_all(nsl)
	  real sting_l_all(nsl),sting_h_all(nsl,2)
	  real x_spl_end_top(ncp-(degree-1)), y_spl_end_top(ncp-(degree-1)),x_spl_end_bot(ncp-(degree-1)), y_spl_end_bot(ncp-(degree-1))
	  real arclength(ncp-(degree-1))
	  real bspline, d_bspline, dd_bspline, bspline_cp
	  real bspline4, d_bspline4, dd_bspline4
	  real x_cp(degree+1), y_cp(degree+1)
	  real sx, sy !Inflection points on the LE
	  real xt, xtt, yt, ytt, u
	  real center, delta_x, delta_y 
      integer :: info
      real uLE_top,uLE_bot, vLE_top,vLE_bot, t
	  real ycp_3,ycp0_3,curv0top_2,d_curv0top_2
	  real ycp_5,ycp0_5,curv0bot_1,d_curv0bot_1
	  real theta_deg,u_vec(1),cam_vec(1),theta_rad
	  real camber_ang_spl((ncp-(degree-1))),camber_le_spl((ncp-(degree-1)))
	  real h,factor
	  real, allocatable, dimension(:) :: theta
	  real sting_h_top,sting_h_bot, sting_l  ! sting height and length
      
      character*32 casename,develop,sec
      character*80 file1,file2,file3,file4
	  !real, allocatable, dimension(:,:) :: xxx
	  !real, allocatable, dimension(:) :: xxxx


! inputs:
!	ss =+ve upward deflection of LE, s=-ve Downward deflection, s=0 No deflection
!   ee ...The elongation of the leading edge.
! print*,'js in lespline',js
    write(sec,'(i2)')js
    ss    = s_all(js)
    ee   = ee_all(js)
	C_le_x_top = C_le_x_top_all(js) !0.2  ! ratio to set the location of the third ycp from 0 (thin) to 1(thick)
	C_le_x_bot = C_le_x_bot_all(js) !0.2  ! ratio to set the location of the third ycp from 0 (thin) to 1(thick)
	C_le_y_top = C_le_y_top_all(js) !0.2  ! ratio to set the location of the third ycp from 0 (thin) to 1(thick)
	C_le_y_bot = C_le_y_bot_all(js) !0.2  ! ratio to set the location of the third ycp from 0 (thin) to 1(thick)
	xcp4_hat = xcp4_hat_all(js) 	! ratio to set the location of u-value 4th cp.
	xcp6_hat = xcp6_hat_all(js)		! ratio to set the location of u-value 6th cp.
	ycp4_hat = ycp4_hat_all(js)		! ratio to set the location of v-value 4th cp.
	ycp6_hat = ycp6_hat_all(js)		! ratio to set the location of v-value 6th cp.
	sting_l = sting_l_all(js)  	! percentage of the chord
	sting_h_top = sting_h_all(js,1)	! percentage of the lethk top
	sting_h_bot = sting_h_all(js,2)	! percentage of the lethk bot
	!print*,'ss',ss
	!print*,'ee',ee
	!print*,'C_le_x_top',C_le_x_top
	!print*,'C_le_x_bot',C_le_x_bot
	!print*,'C_le_y_top',C_le_y_top
	!print*,'C_le_y_bot',C_le_y_bot
	!print*,'xcp4_hat',xcp4_hat
	!print*,'xcp6_hat',xcp6_hat
	!print*,'ycp4_hat',ycp4_hat
	!print*,'ycp6_hat',ycp6_hat
	!print*,'xtop',xtop
	!print*,'ytop',ytop
	!print*,'camber_ang',camber_ang
	
! to work in the non rotated plane:*********************
   theta_rad = camber_ang(1)
   theta_deg = camber_ang(1)*180./pi
   !print*,' theta_deg used in rotation to zero camber plane =',theta_deg
   le_camber_ang = 0.

!Rotating the inputs to zero camber frame:
	call vector_rotation(xtop,ytop,dimen,-camber_ang,xtop,ytop)
	call vector_rotation(xbot,ybot,dimen,-camber_ang,xbot,ybot)
	!print*,'xtop',xtop
	!print*,'ytop',ytop

		write(90,*) 'xtop rotated',xtop
		write(90,*) 'ytop rotated',ytop
		write(90,*) 'xbot rotated',xbot
		write(90,*) 'ybot rotated',ybot
	close(90)
	
	cam_le = camber_le(1)
	! test :
        if (allocated(theta)) deallocate(theta)
	allocate(theta(1))
	 theta = -camber_ang(1)
	u_vec = uin_le; cam_vec = cam_le
	call vector_rotation(u_vec,cam_vec,1,theta,u_vec,cam_vec)
	uin_le = u_vec(1)
	cam_le =cam_vec(1)
	print*,'uin_le rotated to zero camber =',uin_le,'cam_le =',cam_le,'theta =',theta
	deallocate(theta)
	
! Calculated parameters:
    le_thk  = (ytop(1)-ybot(1)) ! Leading edge thickness ![/cos(le_camber_ang)] commented for zero camber 
    chrd_le = (le_thk/2.)*(1+ee)  ! leading edge chord length


! Leading egde tip point in the zero camber plane:(le_camber_ang =0)
     uLE_top = uin_le-chrd_le
	 uLE_bot = uin_le-chrd_le
	 vLE_top =  ss*(chrd_le)+sting_h_top*le_thk/2  ! cam_le = 0 when rotating to zero camber
	 vLE_bot =  ss*(chrd_le)-sting_h_bot*le_thk/2
    ! uLE_top = uin_le-chrd_le
	! uLE_bot = uin_le-chrd_le + sting_h_bot*le_thk*sin(theta_rad)
	! vLE_top =  ss*(chrd_le)+sting_h_top*le_thk/cos(theta_rad)/2 ! at attached line slope not equal 0
	! vLE_bot =  ss*(chrd_le)-sting_h_bot*le_thk/cos(theta_rad)/2

   !print*,'uLE =', uLE
	print*,'uLE_top and bottom =', uLE_top,uLE_bot
	print*,'vLE top and bottom =', vLE_top,vLE_bot


! calculating the control point for defining the tip:
!_______________________________________________________________
!============ Point values ( Connection of the leading edge is at the First point)

	interval = dimen-1
	
   !top LE curve ----------------------------------------
    xtop_le = xtop(1)
    ytop_le = ytop(1)

	! slope_le_top = -(3.*ytop(4)-4.*ytop(3)+ ytop(2))/(xtop(2) - xtop(4))	
    ! curv_le_top = (  ytop(4) - 2*ytop(3) + ytop(2) )/ (0.5*(xtop(2)-xtop(4)))**2 ! backward difference (F(x+2h)-2F(x-h)+F(x))/h^2+O(h)
	! slope_curv_top = -(ytop(4) - 3*ytop(3) + 3*ytop(2) - ytop(1))/ ((xtop(1)-xtop(4))/3)**3 ! backward difference o(h)
	
	! Refined finite difference:----------------------------------------
		h = (xtop(dimen) - xtop(1))/interval
		if (abs((h)-(xtop(2)-xtop(1)))>1e-05) then
			print*,'error calculating h in LE spline top...'
			stop
		endif
		!print*,'h top',h
		!print*,'xtop',xtop
		!print*,'ytop',ytop
		!print*,'xbot',xbot
		!print*,'ybot',ybot
	! 6 intervals accuracy
	slope_le_top = 	(-1/6.*ytop(dimen)+6/5.*ytop(dimen-1)-15/4.*ytop(dimen-2)+&
					20/3.*ytop(dimen-3)-15/2.*ytop(dimen-4)+6.*ytop(dimen-5)-49/20.*ytop(dimen-6))/h	!+o(h^6)
	
	! curv_le_top = (137/180.*ytop(dimen)-27/5.*ytop(dimen-1)+33/2.*ytop(dimen-2)-245/9.*ytop(dimen-3)+&
					! 117/4.*ytop(dimen-4)-87/5.*ytop(dimen-5)+203/45.*ytop(dimen-6))/h**2				!+o(h^5)
	curv_le_top = 	(-5/6.*ytop(dimen-1)+61/12.*ytop(dimen-2)-13.*ytop(dimen-3)+&
					 107/6.*ytop(dimen-4)-77/6.*ytop(dimen-5)+15/4.*ytop(dimen-6))/h**2					!+o(h^4)
    
	slope_curv_top =(-15/8.*ytop(dimen)+13.*ytop(dimen-1)-307/8.*ytop(dimen-2)+62.*ytop(dimen-3)-&
					461/8.*ytop(dimen-4)+29.*ytop(dimen-5)-49/8.*ytop(dimen-6))/h**3					!+o(h^4)

   !bottom LE curve ----------------------------------------
    xbot_le = xbot(1)
    ybot_le = ybot(1)
    
	!slope_le_bot = (3.*ybot(1)-4.*ybot(2)+ ybot(3))/(xbot(1) - xbot(3)) ! forward difference (3F(x)-4F(x-h)+F(x-2h))/2h +O(h^2)
	!curv_le_bot =  (  ybot(1) - 2*ybot(2) + ybot(3) )/ (0.5*(xbot(3)-xbot(1)))**2 ! forward difference (F(x+2h)-2F(x-h)+F(x))/h^2+O(h)
	!slope_curv_bot = -(ybot(1) - 3*ybot(2) + 3*ybot(3) - ybot(4))/ ((xbot(4)-xbot(1))/3)**3 ! forward difference o(h)

	! Refined finite difference:----------------------------------------
		h = (xbot(dimen) - xbot(1))/interval
		if (abs((h)-(xbot(2)-xbot(1)))>1e-05) then
			print*,'error calculating h in spline LE bot ...'
			stop
		endif
		!print*,'h bot',h
	! 6 intervals accuracy
	slope_le_bot = 	(-1/6.*ybot(dimen)+6/5.*ybot(dimen-1)-15/4.*ybot(dimen-2)+&
					20/3.*ybot(dimen-3)-15/2.*ybot(dimen-4)+6.*ybot(dimen-5)-49/20.*ybot(dimen-6))/h	
	
	! curv_le_bot = 	(137/180.*ybot(dimen)-27/5.*ybot(dimen-1)+33/2.*ybot(dimen-2)-245/9.*ybot(dimen-3)+&
					! 117/4.*ybot(dimen-4)-87/5.*ybot(dimen-5)+203/45.*ybot(dimen-6))/h**2
	curv_le_bot =	(-5/6.*ybot(dimen-1)+61/12.*ybot(dimen-2)-13.*ybot(dimen-3)+&
					 107/6.*ybot(dimen-4)-77/6.*ybot(dimen-5)+15/4.*ybot(dimen-6))/h**2
    
	slope_curv_bot =(-15/8.*ybot(dimen)+13.*ybot(dimen-1)-307/8.*ybot(dimen-2)+62.*ybot(dimen-3)-&
					461/8.*ybot(dimen-4)+29.*ybot(dimen-5)-49/8.*ybot(dimen-6))/h**3
					
	!0000000000000000000000000000000000000000000000000000000000000		

      print*, "slope_le_top" ,slope_le_top
      print*, "slope_le_bot" ,slope_le_bot
	  print*, "curv_le_top" ,curv_le_top
      print*, "curv_le_bot" ,curv_le_bot
	  print*, "slope_curv_top" ,slope_curv_top
	  print*, "slope_curv_bot" ,slope_curv_bot

	 
	  print*, 'LE_Degree = ',degree
	  print*,'no_segments =',no_segments

!=====================================================

    a_top = 0 ; a_bot = 0
	
	! for the 4th order bspline LE: -->(quartic b-spline)
! Equations for top and bottom 5 point cal.: _________a(18,18+1) total 9 control points
    ! Equations for top segment
    !xcp(1)*(1/24) + xcp(2)*(11/24) + xcp(3)*(11/24) + xcp(4)*(1/24)= xtop_le(2) 
    !ycp(1)*(1/24) + ycp(2)*(11/24) + ycp(3)*(11/24) + ycp(4)*(1/24)= ytop_le(2) 
    !slope_le_top(2) * ( xcp(1)*(-1/6)+xcp(2)*(-1/2)+ xcp(3)*(1/2)+xcp(4)*(1/6) ) - ycp(1)*(-1/6)-ycp(2)*(-1/2)- ycp(3)*(1/2)-ycp(4)*(1/6) = 0
    !curv_le_top * ( xcp(1)*(1/2) + xcp(2)*(-1/2) + xcp(3)*(-1/2)+ xcp(4)*(1/2) ) - ycp(1)*(1/2)-ycp(2)*(-1/2)- ycp(3)*(-1/2)-ycp(4)*(1/2) = 0
	!slope_curv_top*( xcp(1)*(-1) + xcp(2)*(3) + xcp(3)*(-3)+ xcp(4)*(1) ) - ycp(1)*(-1)-ycp(2)*(3)- ycp(3)*(-3)-ycp(4)*(1) = 0
	
    !Equation for LE "control" point
    ! xcp(5)  = uLE
    ! ycp(5)  = vLE
	
    ! xcp(3) = xcp(2)+C_le*(xcp(4)-xcp(2))   --> (-1+C_le_x)*xcp(2) + xcp(3) - C_le_x   *xcp(4) = 0 then rotated by rotation matrix
    ! ycp(3) = ycp(2)+C_le*(ycp(4)-ycp(2))   --> (-1+C_le_y)*ycp(2) + ycp(3) - C_le_y   *ycp(4) = 0  then rotated by rotation matrix
    ! xcp(7) = xcp(8)+C_le*(xcp(6)-xcp(8))   --> - C_le_x   *xcp(6) + xcp(7) +(-1+C_le_x)*xcp(8) = 0  then rotated by rotation matrix
    ! ycp(7) = ycp(8)+C_le*(ycp(6)-ycp(8))   --> - C_le_y   *ycp(6) + ycp(7) +(-1+C_le_y)*ycp(8) = 0  then rotated by rotation matrix
	
    !Equations for bottom segment
    !xcp(6)*(1/24) + xcp(7)*(11/24) + xcp(8)*(11/24) + xcp(9)*(1/24)= xtop_le(2) 
    !ycp(6)*(1/24) + ycp(7)*(11/24) + ycp(8)*(11/24) + ycp(9)*(1/24)= ytop_le(2) 
    !slope_le_top(2) * ( xcp(6)*(-1/6)+xcp(7)*(-1/2)+ xcp(8)*(1/2)+xcp(9)*(1/6) ) - ycp(6)*(-1/6)-ycp(7)*(-1/2)- ycp(8)*(1/2)-ycp(9)*(1/6) = 0
    !curv_le_top * ( xcp(6)*(1/2) + xcp(7)*(-1/2) + xcp(8)*(-1/2)+ xcp(9)*(1/2) ) - ycp(6)*(1/2)-ycp(7)*(-1/2)- ycp(8)*(-1/2)-ycp(9)*(1/2) = 0
	!slope_curv_bot*( xcp(6)*(-1) + xcp(7)*(3) + xcp(8)*(-3)+ xcp(9)*(1) ) - ycp(6)*(-1)-ycp(7)*(3)- ycp(8)*(-3)-ycp(9)*(1) = 0
	
	! Deviated control points due to ss deviation:
	! xcp(4) = xcp_hat*(uo-uLE)
	! ycp(4) = ycp_hat*(thk_LE)
	! xcp(6) = xcp_hat*(uo-uLE)
	! ycp(6) = ycp_hat*(thk_LE)
	
! Upper spline:
!_____________
                                   !xcp(1),                 xcp(2),                 xcp(3),       			xcp(4),    xcp(5),                ycp(1), ycp(2), ycp(3), ycp(4), ycp(5)    , RHS
     a_top(1,1:5)  = (/                   1/24.,                 11/24.,                 11/24.,        			 1/24.,     0. /)
     a_top(1,7:11) = (/    0.,      0.,     0.,     0.,	0. /)
     a_top(1,13)   = xtop_le
     a_top(2,1:5)  = (/                      0.,                     0.,                     0.,       	  			0., 	0. /)
     a_top(2,7:11) = (/  1/24., 11/24., 11/24.,  1/24.,  0. /)
     a_top(2,13)   = ytop_le
     a_top(3,1:5)  = (/ 	  slope_le_top*(-1/6.),	  slope_le_top*(-1/2.), 	slope_le_top*(1/2.), 	slope_le_top*(1/6.),   	0. /)
     a_top(3,7:11) = (/  1/6.,  1/2.,   -1/2.,   -1/6.,  0. /)
     a_top(3,13)   = 0.
     a_top(4,1:5)  = (/      curv_le_top*(1/2.),    curv_le_top*(-1/2.),    curv_le_top*(-1/2.), 	curv_le_top*(1/2.),     0. /)
     a_top(4,7:11) = (/  -1/2.,  1/2.,   1/2.,   -1/2.,  0. /)
     a_top(4,13)   = 0.
	 a_top(5,1:5)  = (/    slope_curv_top*(-1.),    slope_curv_top*(3.),   slope_curv_top*(-3.),    slope_curv_top*(1.),     0. /)
     a_top(5,7:11) = (/   1.,  	  -3.,    3.,     -1.,  0. /)
     a_top(5,13)   = 0.
	 

! Zero Camber equation for 3rd cp:
	 a_top(6,7:11) 	= (/     0.,    (-C_le_y_top),    	   	1.,      (-1+C_le_y_top), 		0./)

! Equations controlling the LE shape : 4th control point...	 
	 a_top(7,4)  	= 1.		; a_top(7 ,13)  =  (1-xcp4_hat)	*(uin_le - uLE_top)
	 a_top(8,10)  	= 1.		; a_top(8 ,13) 	=  ycp4_hat	*(vLE_top)
	 
!This sets the LE point at uLE, vLE_top at the end of segment 2 
                   !xcp(3), xcp(4), xcp(5), xcp(6)	xcp(7) 
     a_top(9,3:6)   = (/  1/24.,  11/24.,   11/24.,   1/24. /)   ; a_top(9,13)  = uLE_top		! x value
                   !ycp(3), ycp(4), ycp(5), ycp(6), ycp(7)
     a_top(10,9:12) = (/  1/24.,  11/24.,   11/24.,	 1/24. /)   ; a_top(10,13) = vLE_top	! y value
     !slope at the top spline end: = tan(-camber angle)
	 a_top(11,2:6)  = (/  0.,	tan(theta_rad)*(-1/6.),	tan(theta_rad)*(-1/2.),  	tan(theta_rad)*(1/2.), 	tan(theta_rad)*(1/6.) /)
     a_top(11,8:12) = (/    0.,   1/6.,  1/2.,   -1/2.,   -1/6. /)
     a_top(11,13)   = 0.
     ! curvature at top spline end: = 0
	 a_top(12,2:6)  = (/  0.,    0.*(1/2.),    0.*(-1/2.),    0.*(-1/2.), 	  0.*(1/2.) /)
     a_top(12,8:12) = (/    0.,    -1/2.,  1/2.,   1/2.,  -1/2. /)
     a_top(12,13)   = 0.
	 
! -----------------------------------------------------------
! Lower spline:
!_____________
                                   !xcp(1),                 xcp(2),                 xcp(3),       			xcp(4),    xcp(5),                ycp(1), ycp(2), ycp(3), ycp(4), ycp(5)    , RHS
     a_bot(1,1:5)  = (/                   1/24.,                 11/24.,                 11/24.,        			 1/24.,     0. /)
     a_bot(1,7:11) = (/    0.,      0.,     0.,     0.,	0. /)
     a_bot(1,13)   = xbot_le
     a_bot(2,1:5)  = (/                      0.,                     0.,                     0.,       	  			0., 	0. /)
     a_bot(2,7:11) = (/  1/24., 11/24., 11/24.,  1/24.,  0. /)
     a_bot(2,13)   = ybot_le
     a_bot(3,1:5)  = (/ 	  slope_le_bot*(-1/6.),	  slope_le_bot*(-1/2.), 	slope_le_bot*(1/2.), 	slope_le_bot*(1/6.),   	0. /)
     a_bot(3,7:11) = (/  1/6.,  1/2.,   -1/2.,   -1/6.,  0. /)
     a_bot(3,13)   = 0.
     a_bot(4,1:5)  = (/      curv_le_bot*(1/2.),    curv_le_bot*(-1/2.),    curv_le_bot*(-1/2.), 	curv_le_bot*(1/2.),     0. /)
     a_bot(4,7:11) = (/  -1/2.,  1/2.,   1/2.,   -1/2.,  0. /)
     a_bot(4,13)   = 0.
	 a_bot(5,1:5)  = (/    slope_curv_bot*(-1.),    slope_curv_bot*(3.),   slope_curv_bot*(-3.),    slope_curv_bot*(1.),     0. /)
     a_bot(5,7:11) = (/   1.,  	  -3.,    3.,     -1.,  0. /)
     a_bot(5,13)   = 0.
	 
! Zero Camber equation for 3rd cp:
	 a_bot(6,7:11) = (/     0.,    (-C_le_y_bot),    	   	1.,      (-1+C_le_y_bot), 		0./)

! Equations controlling the LE shape : 4th control point...	 
	 a_bot(7,4)    = 1.		; a_bot(7 ,13)  =  (1-xcp6_hat)	*(uin_le - uLE_bot)
	 a_bot(8,10)   = 1.		; a_bot(8 ,13) 	=  (ycp6_hat)	*(vLE_bot)
	 	 
!This sets the LE point at uLE, vLE in the middle of segment 3 
                   !xcp(3), xcp(4), xcp(5), xcp(6)	xcp(7) 
     a_bot(9,3:6)  = (/  1/24.,  11/24.,   11/24.,   1/24. /)   ; a_bot(9,13)  = uLE_bot
                   !ycp(3), ycp(4), ycp(5), ycp(6), ycp(7)
     a_bot(10,9:12)= (/  1/24.,  11/24.,   11/24.,   1/24. /)   ; a_bot(10,13)  = vLE_bot
	 !slope at the top spline end: = tan(-camber angle)
	 a_bot(11,2:6) = (/  0.,	tan(theta_rad)*(-1/6.),	tan(theta_rad)*(-1/2.),  	tan(theta_rad)*(1/2.), 	tan(theta_rad)*(1/6.) /)
     a_bot(11,8:12)= (/    0.,   1/6.,  1/2.,   -1/2.,   -1/6. /)
     a_bot(11,13)  = 0.
     ! curvature at top spline end: = 0
	 a_bot(12,2:6) = (/  0.,    0.*(1/2.),    0.*(-1/2.),    0.*(-1/2.), 	  0.*(1/2.) /)
     a_bot(12,8:12)= (/    0.,    -1/2.,  1/2.,   1/2.,  -1/2. /)
     a_bot(12,13)  = 0.

!-----------------------------------------
   !print*, "a = "
   
    !do i = 1,12
    !    print*, a_top(i,:)
	!	 print*, a_bot(i,:)
    !enddo
    ! solving for the upper spline CP:
	!------------------------------------
     call gauss_jordan ( 2*(degree+no_segments), 1, a_top, info )
	 print*, "info==0 == ", info
	 if (info.ne.0) then 
		print*,'Sting top spline Singular Matrix ...'
		stop
	 endif
     xcp_top(1:ncp) =  a_top(1			  : (degree+no_segments) 			,2*(degree+no_segments)+1)  
     ycp_top(1:ncp) =  a_top((degree+no_segments)+1 :2*(degree+no_segments)	,2*(degree+no_segments)+1)  
    ! do i = 1,ncp	 
		! print*,i,'xcp_top',xcp_top(i),' ','ycp_top',ycp_top(i)
	! end do
	
	 ! solving for the bottom spline CP:
	 !-------------------------------------
	 call gauss_jordan ( 2*(degree+no_segments), 1, a_bot, info )
	 print*, "info==0 == ", info
	 if (info.ne.0) then 
		print*,'Sting bot spline Singular Matrix ...'
		stop
	 endif
	 xcp_bot(1:ncp) =  a_bot(1			  : (degree+no_segments) 			,2*(degree+no_segments)+1)  
     ycp_bot(1:ncp) =  a_bot((degree+no_segments)+1 :2*(degree+no_segments)	,2*(degree+no_segments)+1)
	 
    ! do i = 1,ncp	 
		! print*,i,'xcp_bot',xcp_bot(i),' ','ycp_bot',ycp_bot(i)
	! end do
    print*,'========================================='

   !stop		 
!--------------------------------------------------------  
! computing the end points for each segment:
! For Upper spline:
!------------------
	   t = 0
		if (degree == 3) then
			x_spl_end_top(1) = bspline(xcp_top(1:degree+1),t)
			y_spl_end_top(1) = bspline(ycp_top(1:degree+1),t)
		elseif (degree == 4) then
			x_spl_end_top(1) = bspline4(xcp_top(1:degree+1),t)
			y_spl_end_top(1) = bspline4(ycp_top(1:degree+1),t)
		endif
       t = 1
       do j = 1,ncp-degree
           x_cp = xcp_top(j:j+degree)
		   y_cp = ycp_top(j:j+degree)
		   if (degree == 3) then
			x_spl_end_top(j+1) = bspline(x_cp,t)
			y_spl_end_top(j+1) = bspline(y_cp,t)
		   elseif (degree == 4) then
			x_spl_end_top(j+1) = bspline4(x_cp,t)
			y_spl_end_top(j+1) = bspline4(y_cp,t)
		   endif
       enddo
		 
       call bspline_arclength(arclength,xcp_top,ycp_top,ncp,degree)
       print*, 'arclength', arclength
 ! xs = x1(control point)*B1+x2(control point)*B2+x3(control point)
 !                                         *B3+x4(control point)*B4
 ! clustering the spacing of the LE spline
 	    center = 0.5
		do j=1,(le_pos-2)
			u = real(j-1)/real(le_pos-2)
			if (u < 0.5) then
				s = u !center*(sin( pi * u ))
			else
				s = u!(1-center)*(cos( pi * (u-0.5) ))
		    endif
            !s = u !Uniform spacing
            !print*, "u, s", u, s
			x_le_spl_top(j) = bspline_cp(xcp_top,arclength,ncp,degree,s)
			y_le_spl_top(j) = bspline_cp(ycp_top,arclength,ncp,degree,s)	
		enddo
	 
         if (allocated(theta)) deallocate(theta)
	 Allocate (theta(ncp-(degree-1)))
	 do i= 1, (ncp-(degree-1))
		theta(i) = theta_rad
	 enddo
	 call vector_rotation(x_spl_end_top,y_spl_end_top,(ncp-(degree-1)),theta,x_spl_end_top,y_spl_end_top)
	 deallocate (theta)
	
	 Allocate (theta(ncp))	
	 do i= 1, ncp
		theta(i) = theta_rad
	 enddo
	 call vector_rotation(xcp_top,ycp_top,ncp,theta,xcp_top,ycp_top)	 
	 deallocate (theta)


! For Bottom spline:
!------------------
	   t = 0
		if (degree == 3) then
			x_spl_end_bot(1) = bspline(xcp_bot(1:degree+1),t)
			y_spl_end_bot(1) = bspline(ycp_bot(1:degree+1),t)
		elseif (degree == 4) then
			x_spl_end_bot(1) = bspline4(xcp_bot(1:degree+1),t)
			y_spl_end_bot(1) = bspline4(ycp_bot(1:degree+1),t)
		endif
       t = 1
       do j = 1,ncp-degree
           x_cp = xcp_bot(j:j+degree)
		   y_cp = ycp_bot(j:j+degree)
		   if (degree == 3) then
			x_spl_end_bot(j+1) = bspline(x_cp,t)
			y_spl_end_bot(j+1) = bspline(y_cp,t)
		   elseif (degree == 4) then
			x_spl_end_bot(j+1) = bspline4(x_cp,t)
			y_spl_end_bot(j+1) = bspline4(y_cp,t)
		   endif
       enddo
		 
       call bspline_arclength(arclength,xcp_bot,ycp_bot,ncp,degree)
       print*, 'arclength', arclength
 ! xs = x1(control point)*B1+x2(control point)*B2+x3(control point)
 !                                         *B3+x4(control point)*B4
 ! clustering the spacing of the LE spline
 	    center = 0.5
		do j=1,(le_pos-2)
			u = real(j-1)/real(le_pos-2)
			if (u < 0.5) then
				s = u ! center*(sin( pi * u ))
			else
				s = u !1 - (1-center)*(cos( pi * (u-0.5) ))
		    endif
            !s = u !Uniform spacing
            !print*, "u, s", u, s
			x_le_spl_bot(j) = bspline_cp(xcp_bot,arclength,ncp,degree,s)
			y_le_spl_bot(j) = bspline_cp(ycp_bot,arclength,ncp,degree,s)	
		enddo
	 
         if (allocated(theta)) deallocate(theta)
	 Allocate (theta(ncp-(degree-1)))
	 do i= 1, (ncp-(degree-1))
		theta(i) = theta_rad
	 enddo
	 call vector_rotation(x_spl_end_bot,y_spl_end_bot,(ncp-(degree-1)),theta,x_spl_end_bot,y_spl_end_bot)
	 deallocate (theta)
	
	 Allocate (theta(ncp))	
	 do i= 1, ncp
		theta(i) = theta_rad
	 enddo
	 call vector_rotation(xcp_bot,ycp_bot,ncp,theta,xcp_bot,ycp_bot)	 
	 deallocate (theta)

 !---------------
	! Rotate back to the frame of camber line angle:
	 call vector_rotation(xtop,ytop,dimen,camber_ang,xtop,ytop)
	 call vector_rotation(xbot,ybot,dimen,camber_ang,xbot,ybot)
 !-------------------------------------	
! creating the whole sting LE:
		x_le_spl(1:le_pos-2)				= x_le_spl_top
		y_le_spl(1:le_pos-2)				= y_le_spl_top
		x_le_spl((2*le_pos-1):le_pos+2:-1)  = x_le_spl_bot
		y_le_spl((2*le_pos-1):le_pos+2:-1)	= y_le_spl_bot
  ! the sting tine:
        x_le_spl(le_pos-1) = (uLE_top - sting_l) ! percentage of the blade section chord. The scaling is done in bladegen.
		y_le_spl(le_pos-1) = (vLE_top*cos(theta_rad))
		x_le_spl(le_pos  ) = (uLE_top - sting_l)  ! percentage of the blade section chord.
		y_le_spl(le_pos  ) = (ss*chrd_le)
		x_le_spl(le_pos+1) = (uLE_bot - sting_l)! percentage of the blade section chord.
		y_le_spl(le_pos+1) = (vLE_bot*cos(theta_rad))
	
		
 ! sting rotation to camber 
         if (allocated(theta)) deallocate(theta)
 	 Allocate (theta(3))
	 do i= 1, 3
		theta(i) = -theta_rad-sang
	 enddo
	 call vector_rotation(x_le_spl(le_pos-1:le_pos+1), y_le_spl(le_pos-1:le_pos+1), 3, theta, x_le_spl(le_pos-1:le_pos+1), &
                          y_le_spl(le_pos-1:le_pos+1))
	 deallocate (theta)
	 
 !final LE rotation: 
	 Allocate (theta(2*le_pos-1))
	 do i= 1, (2*le_pos-1)
		theta(i) = theta_rad
	 enddo
	 call vector_rotation(x_le_spl,y_le_spl,(2*le_pos-1),theta,x_le_spl,y_le_spl)
	 deallocate (theta)
	
!-----------------------------	
	! do i=1,3
		! print*, 'x_le_spline',x_le_spl(le_pos-2+i),'y_le_spline',y_le_spl(le_pos-2+i)
	! enddo
	
	 ! write values to files:
      file1 = 'lesting_CP_top.'//trim(adjustl(sec))//'.'//trim(casename)//'.txt'
      open(unit=71,file=file1, form="formatted")
         write(71,*) 'xcp_top',"	",'ycp_top'
         do i=1,ncp
	         write(71,*) xcp_top(i),"	",ycp_top(i)
         enddo
	  close(71)
	  file1 = 'lesting_CP_bot.'//trim(adjustl(sec))//'.'//trim(casename)//'.txt'
      open(unit=72,file=file1, form="formatted")
		 write(72,*) 'xcp_bot',"	",'ycp_bot'
         do i=1,ncp
	         write(72,*) xcp_bot(i),"	",ycp_bot(i)
         enddo
      close(72)
      if(trim(develop).eq.'dev')then
	      file2 = 'le_def_sting.'//trim(adjustl(sec))//'.'//trim(casename)//'.txt'
          open(unit=73,file=file2, form="formatted")
             write(73,*) "xtop	ytop"
              do i=1,dimen
               write(73,*) xtop(i)," ",ytop(i)
             end do
             write(73,*)
             write(73,*) "xbot	ybot"
              do i=1,dimen
               write(73,*) xbot(i)," ",ybot(i)
             end do         
           close(73)
      endif
      file3 = 'le_sting.'//trim(adjustl(sec))//'.'//trim(casename)//'.txt'
      open(unit=74,file=file3, form="formatted")
         write(74,*) "x_le_spl_sting	y_le_spl_sting"
          do i=1,(2*le_pos-1)
           write(74,*) x_le_spl(i)," ",y_le_spl(i)
         end do
       close(74)
      if(trim(develop).eq.'dev')then
          file4 = 'le_sting_segments.'//trim(adjustl(sec))//'.'//trim(casename)//'.txt' 
          open(unit=75,file=file4, form="formatted")
             write(75,*) "x_spl_end_top	y_spl_end_top"
              do i=1,ncp-(degree-1)
                write(75,*) x_spl_end_top(i)," ",y_spl_end_top(i)
             end do
			 write(75,*) "x_spl_end_bot	y_spl_end_bot"
              do i=1,ncp-(degree-1)
                write(75,*) x_spl_end_bot(i)," ",y_spl_end_bot(i)
             end do
           close(75)
      endif 
!	endif 

      return
      end subroutine lesting
!*****************************************************************************
