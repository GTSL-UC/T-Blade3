subroutine lespline (xtop, ytop,xbot, ybot,dimen, &
				   camber_ang, camber_le, uin_le,le_pos, pi, x_le_spl, y_le_spl, js, nsl,&
				   s_all, ee_all,C_le_x_top_all,C_le_x_bot_all,C_le_y_top_all,C_le_y_bot_all,&
				   LE_vertex_ang_all,LE_vertex_dis_all,ncp,degree,no_LE_segments,casename,develop,isdev)
       
!xcp,ycp: edge control points
!xbs,ybs: spline points
!t: parameter value ( 0<t<1)
!ncp: number of control points
! the coordinate system of the LE spline is in the u-v plane
!"Ahmed Nemnem" 

      implicit none

      integer i,j,k,np_le,n_of_iter,le_pos,js,nsl
	  integer ncp,degree,no_LE_segments
	  integer factorial,dimen,interval,order
!      integer, parameter :: plot = 1
	  ! degree .. is the degree of bspline (3(cubic) or 4(quartic))
	  ! plot ... is the number of section I want to see in output files 
	  real C_le_x_top,C_le_x_bot,C_le_y_top,C_le_y_bot,y_le,xcp4_hat,xcp6_hat,ycp4_hat,ycp6_hat
      real, dimension(ncp) :: xcp, ycp
	  real, dimension((2*(degree+no_LE_segments)),((2*(degree+no_LE_segments))+1)) :: a
      real x_le_spl(2*le_pos-1),y_le_spl(2*le_pos-1),sum_alpha_le_spl_top
	  real le_spl_alpha_top(dimen+le_pos-1),le_spl_arc_len_top(dimen+le_pos-1),delta_alpha_le_spl_top(dimen+le_pos-2)
      real u_le,thk_le,slope_le,curv_le,ee,sum_alpha_le_spl_bot,x_le_spl_full(2*dimen+2*le_pos-1),y_le_spl_full(2*dimen*le_pos-1)
	  real le_spl_alpha_bot(dimen+le_pos-1),le_spl_arc_len_bot(dimen+le_pos-1),delta_alpha_le_spl_bot(dimen+le_pos-2)
      real ord,s,theta_us,ss
      real, dimension(dimen), intent(inout) :: xtop, ytop
      real, dimension(dimen), intent(inout) :: xbot, ybot
      real xbot_le, ybot_le, slope_le_bot, curv_le_bot, slope_curv_bot
      real xtop_le, ytop_le, slope_le_top, curv_le_top, slope_curv_top
      real camber_ang(dimen),camber_le(dimen),le_camber_ang,cam_le
      real uin_le,le_thk,chrd_le,LE_vertex_angle,LE_extens,LE_vertex_dis,LE_vertex_ang
      real du1,du2,umx,pi
      real s_all(nsl),ee_all(nsl),C_le_y_top_all(nsl),C_le_y_bot_all(nsl),C_le_x_top_all(nsl),C_le_x_bot_all(nsl)
	  real LE_vertex_ang_all(nsl),LE_vertex_dis_all(nsl)
	  real x_spl_end(ncp-(degree-1)), y_spl_end(ncp-(degree-1)), arclength(ncp-(degree-1))
	  real bspline, d_bspline, dd_bspline, bspline_cp
	  real bspline4, d_bspline4, dd_bspline4
	  real x_cp(degree+1), y_cp(degree+1)
	  real sx, sy !Inflection points on the LE
	  real xt, xtt, yt, ytt, u
	  real center, delta_x, delta_y 
      integer :: info
      real uLE, vLE, t
	  real ycp_3,ycp0_3,curv0top_2,d_curv0top_2
	  real ycp_5,ycp0_5,curv0bot_1,d_curv0bot_1
	  real theta_deg,u_vec(1),cam_vec(1),theta_rad
	  real camber_ang_spl((ncp-(degree-1))),camber_le_spl((ncp-(degree-1)))
	  real h,factor
	  real, allocatable, dimension(:) :: theta
	  !minimzation varaibles:
	  real c_le_a_top,c_le_b_top,c_le_c_top,sum_alpha_top_a,sum_alpha_top_b,sum_alpha_top_c
	  real c_le_a_bot,c_le_b_bot,c_le_c_bot,sum_alpha_bot_a,sum_alpha_bot_b,sum_alpha_bot_c
	  real t1_top,t2_top,sum_alpha_top_1,sum_alpha_top_2,golden_ratio
	  real t1_bot,t2_bot,sum_alpha_bot_1,sum_alpha_bot_2,tolerance
      
      character*32 casename,develop,sec
      character*80 file1,file2,file3,file4
	  !real, allocatable, dimension(:,:) :: xxx
	  !real, allocatable, dimension(:) :: xxxx
      logical isdev


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
	! xcp4_hat = xcp4_hat_all(js) 	! ratio to set the location of u-value 4th cp.
	! xcp6_hat = xcp6_hat_all(js)		! ratio to set the location of u-value 6th cp.
	! ycp4_hat = ycp4_hat_all(js)		! ratio to set the location of v-value 4th cp.
	! ycp6_hat = ycp6_hat_all(js)		! ratio to set the location of v-value 6th cp.
	LE_vertex_ang = LE_vertex_ang_all(js)
	LE_vertex_dis = LE_vertex_dis_all(js)
	
	x_le_spl = 0. ; y_le_spl= 0.
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

! Leading egde tip point:
   !uLE = (uin_le-chrd_le*cos(le_camber_ang)) - ss*chrd_le*sin(le_camber_ang)	 
   !vLE = (cam_le-chrd_le*sin(le_camber_ang)) + ss*chrd_le*cos(le_camber_ang)
! Leading egde tip point in the zero camber plane:(le_camber_ang =0)
    uLE = uin_le-chrd_le
	vLE =  ss*(chrd_le)  ! cam_le = 0 when rotating to zero camber

   print*,'uLE =', uLE
   print*,'vLE =', vLE


! calculating the control point for defining the tip:
!_______________________________________________________________

!============ Point values (if Connection of the leading edge is at the second point)
   ! !top LE curve:
    ! xtop_le = xtop(4)
    ! ytop_le = ytop(4)
    ! slope_le_top(2) = -(3*ytop(4)-4*0.5*(ytop(4)+ytop(3))+ ytop(3))/(xtop(3) - xtop(4)) ! backward difference (3F(x)-4F(x-h)+F(x-2h))/2h    
    ! curv_le_top(2) = ( slope_le_top(1) - slope_le_top(2))/ (0.5*(xtop(3)-xtop(4))) ! backward difference
    
   ! !bottom LE curve:
    ! xbot_le = xbot(1)
    ! ybot_le = ybot(1)
    ! slope_le_bot(1) = (3*ybot(1)-4*0.5*(ybot(1)+ybot(2))+ ybot(2))/(xbot(1) - xbot(2)) ! forward difference (3F(x)-4F(x-h)+F(x-2h))/2h
    ! curv_le_bot(1) = -( slope_le_bot(1)- slope_le_bot(2))/ (0.5*(xbot(2)-xbot(1))) ! forward difference
	
   ! Leading edge point:
    ! uLE = (uin_le-chrd_le*cos(le_camber_ang)) - s*le_thk/2*sin(le_camber_ang)		 
    ! vLE = (1.25*cam_le-chrd_le*sin(le_camber_ang)) + s*le_thk/2*cos(le_camber_ang)
	! curv_le_tip = d2Y/dX2 = Y2-2*Y3+Y4/(X2-2*X3+X4)
	
   ! Leading edge mid points:
    ! X3 = X2+C_le*(X2-X4)
	! X5 = X6+C_le*(X6-X4)
	! (Y3-Y5)/(ytop_le-ybot_le)= y_le
	
 
!============ Point values (if Connection of the leading edge is at the First point)

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

					
      print*, "slope_le_top" ,slope_le_top
      print*, "slope_le_bot" ,slope_le_bot
	  print*, "curv_le_top" ,curv_le_top
      print*, "curv_le_bot" ,curv_le_bot
	  print*, "slope_curv_top" ,slope_curv_top
	  print*, "slope_curv_bot" ,slope_curv_bot

	 
	  print*, 'LE_Degree = ',degree
	  print*,'no_LE_segments =',no_LE_segments

!===========================================================================================
!========================================================

 !Minimization of sum_alpha_le_spl: using Golden-section search:
	tolerance = 1e-05
	! the range triplet points: [a,c,b] interval, c inbetween 
	c_le_a_top = 0.0    ;c_le_b_top =  4.0    ; c_le_c_top = 1.0
	c_le_a_bot = 0.0    ;c_le_b_bot =  4.0    ; c_le_c_bot = 1.0

	! define the golden ratio:
	golden_ratio = 0.5*(3-sqrt(5.))
	
	! evaluate the functions:
	! sum_alpha_a = f(c_le_a) 
	call le_matrix_sol(x_le_spl,y_le_spl,x_spl_end,y_spl_end,c_le_a_top,c_le_a_bot,degree,no_LE_segments,dimen,slope_le_top,   &
                       slope_le_bot,curv_le_top,curv_le_bot,slope_curv_top,slope_curv_bot,xtop_le,ytop_le,xbot_le,ybot_le,uLE, &
                       vLE,le_camber_ang,C_le_x_bot,C_le_x_top,xcp,ycp,ncp,le_pos,uin_le,le_thk,pi,LE_vertex_ang,LE_vertex_dis)	
	call sum_alpha_function(xtop,ytop,xbot,ybot,dimen,le_pos,x_le_spl,y_le_spl,sum_alpha_le_spl_top,sum_alpha_le_spl_bot)
	sum_alpha_top_a = sum_alpha_le_spl_top; 	sum_alpha_bot_a = sum_alpha_le_spl_bot
	
	! sum_alpha_b =  f(c_le_b)
	call le_matrix_sol(x_le_spl,y_le_spl,x_spl_end,y_spl_end,c_le_b_top,c_le_b_bot,degree,no_LE_segments,dimen,slope_le_top,   &
                       slope_le_bot,curv_le_top,curv_le_bot,slope_curv_top,slope_curv_bot,xtop_le,ytop_le,xbot_le,ybot_le,uLE, &
                       vLE,le_camber_ang,C_le_x_bot,C_le_x_top,xcp,ycp,ncp,le_pos,uin_le,le_thk,pi,LE_vertex_ang,LE_vertex_dis)
	call sum_alpha_function(xtop,ytop,xbot,ybot,dimen,le_pos,x_le_spl,y_le_spl,sum_alpha_le_spl_top,sum_alpha_le_spl_bot)
	sum_alpha_top_b = sum_alpha_le_spl_top; 	sum_alpha_bot_b = sum_alpha_le_spl_bot
	
	! sum_alpha_c =  f(c_le_c)
	call le_matrix_sol(x_le_spl,y_le_spl,x_spl_end,y_spl_end,c_le_c_top,c_le_c_bot,degree,no_LE_segments,dimen,slope_le_top,   &
                       slope_le_bot,curv_le_top,curv_le_bot,slope_curv_top,slope_curv_bot,xtop_le,ytop_le,xbot_le,ybot_le,uLE, &
                       vLE,le_camber_ang,C_le_x_bot,C_le_x_top,xcp,ycp,ncp,le_pos,uin_le,le_thk,pi,LE_vertex_ang,LE_vertex_dis)
	call sum_alpha_function(xtop,ytop,xbot,ybot,dimen,le_pos,x_le_spl,y_le_spl,sum_alpha_le_spl_top,sum_alpha_le_spl_bot)
	sum_alpha_top_c = sum_alpha_le_spl_top; 	sum_alpha_bot_c = sum_alpha_le_spl_bot
	
	! error trap for top:
	if ((sum_alpha_top_a < sum_alpha_top_c).or.(sum_alpha_top_c > sum_alpha_top_b)) then
		print*, 'Warning, [a,c,b] interval does not bracket a min, look in lespline subroutine'
	endif
	
	! error trap for bot:
	if ((sum_alpha_bot_a < sum_alpha_bot_c).or.(sum_alpha_bot_c > sum_alpha_bot_b)) then
		print*, 'Warning, [a,c,b] interval does not bracket a min, look in lespline subroutine'
	endif
	
	!for top
	if (abs(c_le_c_top - c_le_a_top) < abs(c_le_b_top - c_le_c_top)) then
		t1_top= c_le_c_top;  t2_top = c_le_c_top + golden_ratio*(c_le_b_top - c_le_c_top)
	else
		t2_top= c_le_c_top;  t1_top = c_le_c_top - golden_ratio*(c_le_c_top - c_le_a_top)
	endif
	
	!for bottom	
	if (abs(c_le_c_bot - c_le_a_bot) < abs(c_le_b_bot - c_le_c_bot)) then
		t1_bot= c_le_c_bot;  t2_bot = c_le_c_bot + golden_ratio*(c_le_b_bot - c_le_c_bot)
	else
		t2_bot= c_le_c_bot;  t1_bot = c_le_c_bot - golden_ratio*(c_le_c_bot - c_le_a_bot)
	endif
	
	!sum_alpha_1 = f(t1)
	call le_matrix_sol(x_le_spl,y_le_spl,x_spl_end,y_spl_end,t1_top,t1_bot,degree,no_LE_segments,dimen,slope_le_top,       &
                       slope_le_bot,curv_le_top,curv_le_bot,slope_curv_top,slope_curv_bot,xtop_le,ytop_le,xbot_le,ybot_le, &
                       uLE,vLE,le_camber_ang,C_le_x_bot,C_le_x_top,xcp,ycp,ncp,le_pos,uin_le,le_thk,pi,LE_vertex_ang,      & 
                       LE_vertex_dis)	
	call sum_alpha_function(xtop,ytop,xbot,ybot,dimen,le_pos,x_le_spl,y_le_spl,sum_alpha_le_spl_top,sum_alpha_le_spl_bot)
	sum_alpha_top_1 = sum_alpha_le_spl_top; 	sum_alpha_bot_1 = sum_alpha_le_spl_bot
	
	!sum_alpha_2 = f(t2)
	call le_matrix_sol(x_le_spl,y_le_spl,x_spl_end,y_spl_end,t2_top,t2_bot,degree,no_LE_segments,dimen,slope_le_top,       &
                       slope_le_bot,curv_le_top,curv_le_bot,slope_curv_top,slope_curv_bot,xtop_le,ytop_le,xbot_le,ybot_le, &
                       uLE,vLE,le_camber_ang,C_le_x_bot,C_le_x_top,xcp,ycp,ncp,le_pos,uin_le,le_thk,pi,LE_vertex_ang,      &
                       LE_vertex_dis)	
	call sum_alpha_function(xtop,ytop,xbot,ybot,dimen,le_pos,x_le_spl,y_le_spl,sum_alpha_le_spl_top,sum_alpha_le_spl_bot)
	sum_alpha_top_2 = sum_alpha_le_spl_top; 	sum_alpha_bot_2 = sum_alpha_le_spl_bot
	
  !top spline
  !----------
	print*, 'Top LE spline optimization started ...'
	! print*, '              c_le_a_top				t1_top					t2_top					c_le_b_top				sum_alpha_top_1				sum_alpha_top_2'
	! print*, '          0', c_le_a_top, t1_top, t2_top, c_le_b_top, sum_alpha_top_1, sum_alpha_top_2
	do i = 1,50
		if (sum_alpha_top_1 < sum_alpha_top_2) then
			c_le_b_top = t2_top; 	t2_top = t1_top
			t1_top = t2_top - golden_ratio *(t2_top - c_le_a_top)
			sum_alpha_top_2 = sum_alpha_top_1
			call le_matrix_sol(x_le_spl,y_le_spl,x_spl_end,y_spl_end,t1_top,c_le_c_bot,degree,no_LE_segments,dimen,slope_le_top,   &
                               slope_le_bot,curv_le_top,curv_le_bot,slope_curv_top,slope_curv_bot,xtop_le,ytop_le,xbot_le,ybot_le, &
                               uLE,vLE,le_camber_ang,C_le_x_bot,C_le_x_top,xcp,ycp,ncp,le_pos,uin_le,le_thk,pi,LE_vertex_ang,      &
                               LE_vertex_dis)	
			call sum_alpha_function(xtop,ytop,xbot,ybot,dimen,le_pos,x_le_spl,y_le_spl,sum_alpha_le_spl_top,sum_alpha_le_spl_bot)
			sum_alpha_top_1 = sum_alpha_le_spl_top;
		else
			c_le_a_top = t1_top; t1_top = t2_top
			t2_top = t1_top + golden_ratio *(c_le_b_top - t1_top)
			sum_alpha_top_1 = sum_alpha_top_2;	
			call le_matrix_sol(x_le_spl,y_le_spl,x_spl_end,y_spl_end,t2_top,c_le_c_bot,degree,no_LE_segments,dimen,slope_le_top,   &
                               slope_le_bot,curv_le_top,curv_le_bot,slope_curv_top,slope_curv_bot,xtop_le,ytop_le,xbot_le,ybot_le, &
                               uLE,vLE,le_camber_ang,C_le_x_bot,C_le_x_top,xcp,ycp,ncp,le_pos,uin_le,le_thk,pi,LE_vertex_ang,      &
                               LE_vertex_dis)	
			call sum_alpha_function(xtop,ytop,xbot,ybot,dimen,le_pos,x_le_spl,y_le_spl,sum_alpha_le_spl_top,sum_alpha_le_spl_bot)
			sum_alpha_top_2 = sum_alpha_le_spl_top;
		endif
		! printing 
		! print*, i, c_le_a_top, t1_top, t2_top, c_le_b_top, sum_alpha_top_1, sum_alpha_top_2
		
		! converge criteria:
		if (abs(c_le_b_top - c_le_a_top) < tolerance) then
			print*, 'top le spline Golden-section seach has converged'
			exit
		endif
	enddo
	! result:
	if (sum_alpha_top_2 < sum_alpha_top_1) then
		C_le_y_top = t2_top; sum_alpha_le_spl_top = sum_alpha_top_2
	else
		C_le_y_top = t1_top; sum_alpha_le_spl_top = sum_alpha_top_1
	endif	
	
  !bottom spline
  !-----------
	! print*, 'Bottom LE spline optimization started ...'
	! print*, '              c_le_a_bot				t1_bot					t2_bot				c_le_b_bot				sum_alpha_bot_1				sum_alpha_bot_2'
	! print*, '          0', c_le_a_bot, t1_bot, t2_bot, c_le_b_bot, sum_alpha_bot_1, sum_alpha_bot_2
	do i = 1,50
		if (sum_alpha_bot_1 < sum_alpha_bot_2) then
			c_le_b_bot = t2_bot; 	t2_bot = t1_bot
			t1_bot = t2_bot - golden_ratio *(t2_bot - c_le_a_bot)
			sum_alpha_bot_2 = sum_alpha_bot_1
			call le_matrix_sol(x_le_spl,y_le_spl,x_spl_end,y_spl_end,C_le_y_top,t1_bot,degree,no_LE_segments,dimen,slope_le_top,   &
                               slope_le_bot,curv_le_top,curv_le_bot,slope_curv_top,slope_curv_bot,xtop_le,ytop_le,xbot_le,ybot_le, &
                               uLE,vLE,le_camber_ang,C_le_x_bot,C_le_x_top,xcp,ycp,ncp,le_pos,uin_le,le_thk,pi,LE_vertex_ang,      &
                               LE_vertex_dis)	
			call sum_alpha_function(xtop,ytop,xbot,ybot,dimen,le_pos,x_le_spl,y_le_spl,sum_alpha_le_spl_top,sum_alpha_le_spl_bot)
			sum_alpha_bot_1 = sum_alpha_le_spl_bot
		else
			c_le_a_bot = t1_bot; t1_bot = t2_bot
			t2_bot = t1_bot + golden_ratio *(c_le_b_bot - t1_bot)
			sum_alpha_bot_1 = sum_alpha_bot_2;	
			call le_matrix_sol(x_le_spl,y_le_spl,x_spl_end,y_spl_end,C_le_y_top,t2_bot,degree,no_LE_segments,dimen,slope_le_top,   &
                               slope_le_bot,curv_le_top,curv_le_bot,slope_curv_top,slope_curv_bot,xtop_le,ytop_le,xbot_le,ybot_le, &
                               uLE,vLE,le_camber_ang,C_le_x_bot,C_le_x_top,xcp,ycp,ncp,le_pos,uin_le,le_thk,pi,LE_vertex_ang,      &
                               LE_vertex_dis)	
			call sum_alpha_function(xtop,ytop,xbot,ybot,dimen,le_pos,x_le_spl,y_le_spl,sum_alpha_le_spl_top,sum_alpha_le_spl_bot)
			sum_alpha_bot_2 = sum_alpha_le_spl_bot
		endif
		! printing 
		! print*, i, c_le_a_bot, t1_bot, t2_bot, c_le_b_bot, sum_alpha_bot_1, sum_alpha_bot_2
		
		! converge criteria:
		if (abs(c_le_b_bot - c_le_a_bot) < tolerance) then
			print*, 'bottom le spline Golden-section seach has converged'
			exit
		endif
	enddo
	! result:	
	if (sum_alpha_bot_2 < sum_alpha_bot_1) then
		C_le_y_bot = t2_bot; sum_alpha_le_spl_bot = sum_alpha_bot_2
	else
		C_le_y_bot = t1_bot; sum_alpha_le_spl_bot = sum_alpha_bot_1
	endif
!=========================================================================================
!=========================================================================================	
	
! Evaluation of the LE control points spline values (done in subroutine for LE optimization)
 call le_matrix_sol(x_le_spl,y_le_spl,x_spl_end,y_spl_end,C_le_y_top,C_le_y_bot,degree,no_LE_segments,dimen,slope_le_top,   &
                    slope_le_bot,curv_le_top,curv_le_bot,slope_curv_top,slope_curv_bot,xtop_le,ytop_le,xbot_le,ybot_le,uLE, &
                    vLE,le_camber_ang,C_le_x_bot,C_le_x_top,xcp,ycp,ncp,le_pos,uin_le,le_thk,pi,LE_vertex_ang,LE_vertex_dis)
	
!=========================================================================================
!=========================================================================================

! Evaluation of the LE minimization function:
 ! call sum_alpha_function(xtop,ytop,xbot,ybot,dimen,le_pos,x_le_spl,y_le_spl,sum_alpha_le_spl_top,sum_alpha_le_spl_bot)
 
!=========================================================================================
!=========================================================================================
!---------------------------------

 ! Rotate back to the frame of camber line angle:
	 !print*,'camber_ang =',camber_ang
	 call vector_rotation(xtop,ytop,dimen,camber_ang,xtop,ytop)
	 call vector_rotation(xbot,ybot,dimen,camber_ang,xbot,ybot)
	 
         if (allocated(theta)) deallocate(theta)
	 Allocate (theta(ncp-(degree-1)))
	 do i= 1, (ncp-(degree-1))
		theta(i) = theta_rad
	 enddo
	 call vector_rotation(x_spl_end,y_spl_end,(ncp-(degree-1)),theta,x_spl_end,y_spl_end)
	 deallocate (theta)
	 
	 Allocate (theta(2*le_pos-1))
	 do i= 1, (2*le_pos-1)
		theta(i) = theta_rad
	 enddo
	 call vector_rotation(x_le_spl,y_le_spl,(2*le_pos-1),theta,x_le_spl,y_le_spl)
	 deallocate (theta)
	
	 Allocate (theta(ncp))	
	 do i= 1, ncp
		theta(i) = theta_rad
	 enddo
	 call vector_rotation(xcp,ycp,ncp,theta,xcp,ycp)	 
	 deallocate (theta)
	 
	 	 
	 ! write values to files:
!	if (plot == js) then 
      file1 = 'le_CP.'//trim(adjustl(sec))//'.'//trim(casename)//'.txt'
      open(unit=71,file=file1, form="formatted")
         write(71,*) 'xcp_le',"	",'ycp_le'
         do i=1,ncp
	         write(71,*) xcp(i),"	",ycp(i)
         enddo
      close(71)
      if(isdev)then
	      file2 = 'le_def.'//trim(adjustl(sec))//'.'//trim(casename)//'.txt'
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
      file3 = 'le_spline.'//trim(adjustl(sec))//'.'//trim(casename)//'.txt'
      open(unit=74,file=file3, form="formatted")
         write(74,*) "x_le_spl	y_le_spl"
          do i=1,(2*le_pos-1)
           write(74,*) x_le_spl(i)," ",y_le_spl(i)
         end do
       close(74)
      if(isdev)then
          file4 = 'le_segments.'//trim(adjustl(sec))//'.'//trim(casename)//'.txt' 
          open(unit=75,file=file4, form="formatted")
             write(75,*) "x_spl_end	y_spl_end"
              do i=1,ncp-(degree-1)
                write(75,*) x_spl_end(i)," ",y_spl_end(i)
             end do
           close(75)
      endif 
!	endif 

      return
      end subroutine lespline
!*****************************************************************************
!*****************************************************************************
subroutine vector_rotation(x,y,dimen,theta_rad,x_rot,y_rot)
 implicit none
 ! dimen ... is 1D vector dimension
 integer dimen,i
 real *8 ,dimension(dimen), intent (in) :: x,y,theta_rad 
 real *8 ,dimension(dimen), intent (out) :: x_rot,y_rot
 real *8 :: x_in, y_in

  do i= 1,dimen
	x_in = x(i)
	y_in = y(i)

	x_rot(i) = x_in*cos(theta_rad(i))-y_in*sin(theta_rad(i))
	y_rot(i) = x_in*sin(theta_rad(i))+y_in*cos(theta_rad(i))
  enddo
  return
  end subroutine
!*****************************************************

subroutine section_orientation(x,y,camber,theta_rad,dimen,x_rot,y_rot)
 implicit none
 ! dimen ... is an integer shows dimension of 1D vector  
 integer dimen,i
 real *8 ,dimension(dimen), intent (in) :: x,y,camber,theta_rad
 real *8 ,dimension(dimen), intent (out) :: x_rot,y_rot
 
		x_rot = x			-y*sin(theta_rad)
		y_rot = camber		+y*cos(theta_rad)
  return
  end subroutine
!*************************************************************************
!**************************************************** Refining the values of LE spline contact point
!**************************************************** by Ahmed Nemnem
subroutine fini_diff_refine(curv_camber,thick,thick_distr,&
					   xcp_curv,ycp_curv,ncp_curv,xcp_thk,ycp_thk,ncp_thk,&
					   u_contact,interval,&
					   ucp_top,vcp_top,ucp_bot,vcp_bot,&
					   sinl,sext,flin,flex,fmxthk,umxthk,lethk,tethk,&
					   rr1,rr2,&
					   x_spl_end,init_angles,init_cambers,camber,u,&
					   xtop,ytop,xbot,ybot)

		implicit none
		integer curv_camber,thick,thick_distr
		integer i,j,l,k,interval
		integer ncp_curv,ncp_thk,oo
		
		real u(interval+1),u_contact,delta_h
		real cam,cam_u
        real u_le,thk_le,slope_le,curv_le,uin_le,i_le
		real rr1,rr2,sang,sexts,sinls,tethk,thk,ui,umx,umxthk
		real lethk, mxthk,mr1,thkc,thkmultip
		real camber(interval+1),slope(interval+1)
		real splthick(interval+1), thickness(interval+1), angle(interval+1)
		real x_spl_end(ncp_curv-2),xcp_curv(ncp_curv),ycp_curv(ncp_curv)
		real init_angles(ncp_curv-2),init_cambers(ncp_curv-2)
		real xtop(interval+1),ytop(interval+1),xbot(interval+1),ybot(interval+1)
		real xcp_thk(ncp_thk),ycp_thk(ncp_thk)
		real flex,flin,fmxthk,sinl,sext
		real ucp_top(11),vcp_top(11),ucp_bot(11),vcp_bot(11) ! from curvature
	  
 
 ! clustering the u values for the contact points:
	u(1) = u_contact
	delta_h = 1e-06
 do i = 1 , interval
	 u(i+1) = u(i) + delta_h
 enddo

!---------------------------

if(curv_camber.eq.0)then
       do i=1,(interval+1)
        ui=u(i)        
        call cambmix(ui,cam,cam_u,sinl,sext,flin,flex)
        camber(i) = cam
        slope(i) = cam_u
       enddo
elseif(curv_camber.eq.1)then 
    call bsplinecam_refine(xcp_curv,ycp_curv,(interval+1),ncp_curv,u,slope,camber,interval,x_spl_end,init_angles,init_cambers)
endif     
!print*,'slope......',slope
!print*,'camber......',camber   
!=====================================================================               
if(thick.eq.1)then     
   call bspline_y_of_x_refine( splthick, u, (interval+1), xcp_thk, ycp_thk, ncp_thk, 4 )
else
   splthick = 0
endif
 !print*,'splthick',splthick

!---- generate airfoil thickness:===============================
if( thick_distr == 1 ) then
  call splinethick_refine(thickness, u, (interval+1), ucp_top,vcp_top,ucp_bot,vcp_bot)
  thickness = thickness*(1 + splthick)
else
  do i=1,interval+1
    ui=u(i)
    thkmultip = splthick(i)
    call thickellip(i,ui, thk, lethk, tethk, fmxthk,umxthk,rr1,rr2,thkmultip,u_le,uin_le,i_le,oo)
    thickness(i) = thk
  enddo
endif
	  
!----Creating the top and bottom curve coordinates. ---------
angle = atan(slope)
!print*, "camber", camber
!print*,'u .........',u
! generating the blade on vector bases:

xbot = u      + thickness*sin(angle)
ybot = camber - thickness*cos(angle)
xtop = u      - thickness*sin(angle)
ytop = camber + thickness*cos(angle)

Return 
end subroutine fini_diff_refine
!*************************************************
!*************************************************
subroutine bsplinecam_refine(xcp,ycp,np,ncp,u,cam_u,cam,interval,x_spl_end,init_angles,init_cambers)
      implicit none

      integer i,j,k,np,ncp
	  integer, intent(in) :: interval
	  integer, parameter :: degree = 3
      real*8 xcp(ncp),ycp(ncp)
      real*8 u(interval+1)
      real*8 curv(np), cam_u(np), cam(np)
      real*8 x_spl_end(ncp-2)
      real*8 init_angles(ncp-2),init_cambers(ncp-2)
      real*8 x_cp(degree+1),y_cp(degree+1),t,angle0,camber0
 ! the functions used:     
      real*8 bspline_t_newton,camber,angle,bspline
     
      
! Interpolate u between the adjacent two xbs values (fitting):
! newton's interpolation:
        do i=1,np
         !print*,"u(",i,") =",u(i)
            do j=1,(ncp-degree)
             if ((u(i) >= x_spl_end(j)) .and. (u(i) < x_spl_end(j+1))) then
               x_cp = xcp(j:j+degree)
               y_cp = ycp(j:j+degree)
               angle0  = init_angles(j)
               camber0 = init_cambers(j)
               t = bspline_t_newton(x_cp,u(i))
               !print*,'init_angles(',j,')',init_angles(j)
               !print*,'init_cambers(',j,')',init_cambers(j)
               curv(i)= bspline(y_cp,t)
               cam_u(i)= angle(y_cp,x_cp,angle0,t)
               cam(i)= camber(y_cp,x_cp,angle0,camber0,t)              
               goto 15
             elseif (u(i) == 0) then
               y_cp = ycp(j:j+degree)
               t = 0
               curv(i)=bspline(y_cp,t)
               cam_u(i)=init_angles(j)
               cam(i)=init_cambers(j)
               goto 15
             elseif ((u(i) == 1).and.(j==(ncp-degree))) then
               y_cp = ycp(j:j+degree)
               t = 1
               curv(i)=bspline(y_cp,t)
               cam_u(i)=init_angles(j+1)
               cam(i)=init_cambers(j+1)
             end if
          end do
15        continue
		enddo
		
      return
      end subroutine bsplinecam_refine
	  
!*********************************************************************************
subroutine splinethick_refine(thickness, u, np ,ucp_top,vcp_top,ucp_bot,vcp_bot)
!
implicit none

real*8, dimension(np), intent(out) :: thickness
real*8, dimension(np), intent(in) :: u
integer, intent(in) :: np

integer, parameter :: ncp_side = 11, degree = 4
real*8, dimension(ncp_side) :: ucp_top,vcp_top,ucp_bot,vcp_bot
real*8, dimension(np) :: top_thickness, bot_thickness

call bspline_y_of_x_refine( top_thickness, u, np, ucp_top, vcp_top, ncp_side, degree )

call bspline_y_of_x_refine( bot_thickness, u, np, ucp_bot, vcp_bot, ncp_side, degree ) 

thickness = top_thickness - bot_thickness
thickness = thickness/2.
!print*,'thickness .....',thickness

end subroutine

!*****************************************************************
!*****************************************************************
integer function factorial (imax)

implicit none
integer, intent (in):: imax
integer i

factorial = 1
if (imax /= 0) then
do i= 1,imax
	factorial = factorial * i
enddo
elseif (imax == 0) then
endif
end function

!*****************************************************************
!*****************************************************************
!*****************************************************************
subroutine le_matrix_sol(x_le_spl,y_le_spl,x_spl_end,y_spl_end,C_le_y_top,C_le_y_bot,degree,no_LE_segments,dimen,slope_le_top,   &
                         slope_le_bot,curv_le_top,curv_le_bot,slope_curv_top,slope_curv_bot,xtop_le,ytop_le,xbot_le,ybot_le,uLE, &
                         vLE,le_camber_ang,C_le_x_bot,C_le_x_top,xcp,ycp,ncp,le_pos,uin_le,le_thk,pi,LE_vertex_ang,LE_vertex_dis)
	
	  implicit none

      integer i,j,k
      real    t
	  integer,intent(in) :: le_pos
	  integer ncp,degree,no_LE_segments
	  integer factorial,dimen,interval,order
	  real,intent(out) :: xcp(ncp), ycp(ncp)
	  real,intent(in) :: C_le_x_top,C_le_x_bot,C_le_y_top,C_le_y_bot
	  real xcp4_hat,xcp6_hat,ycp4_hat,ycp6_hat 
	  real, dimension((2*(degree+no_LE_segments)),((2*(degree+no_LE_segments))+1)) :: a
      real,intent(in) :: xbot_le, ybot_le, slope_le_bot, curv_le_bot, slope_curv_bot
      real,intent(in) :: xtop_le, ytop_le, slope_le_top, curv_le_top, slope_curv_top
      real,intent(in) :: le_camber_ang
      real,intent(in) :: uin_le,le_thk
	  real LE_vertex_ang,LE_extens,LE_vertex_dis,pi, arclength(ncp-(degree-1)),LE_vertex_ang_rad
 	  real,intent(out) :: x_spl_end(ncp-(degree-1)), y_spl_end(ncp-(degree-1)),x_le_spl(2*le_pos-1),y_le_spl(2*le_pos-1)
	  real bspline, d_bspline, dd_bspline, bspline_cp
	  real bspline4, d_bspline4, dd_bspline4
	  real x_cp(degree+1), y_cp(degree+1)
	  real sx, sy !Inflection points on the LE
	  real xt, xtt, yt, ytt, u, s
	  real center, delta_x, delta_y 
      integer :: info
      real,intent(in) :: uLE, vLE

	  
!=====================================================

    a = 0
	
   if ((degree == 3).and.(no_LE_segments == 4)) then		
   ! for the 3rd order bspline LE : --> (cubic bspline)
 ! Equations for top and bottom 4 point cal. for fixed cp 3, 4 ,5: _________a(14,14+1)
    ! Equartions for top segment
    !xcp(1)*(1/6) + xcp(2)*(2/3) + xcp(3)*(1/6) = xtop_le(2) 
    !ycp(1)*(1/6) + ycp(2)*(2/3) + ycp(3)*(1/6) = ytop_le(2) 
    !slope_le_top(2) * ( xcp(1)*(-1/2) + xcp(3)*(1/2) ) - ycp(1)*(-1/2) - ycp(3)*(1/2) = 0
    !curv_le_top(2) * ( xcp(1)*(1) + xcp(2)*(-2) + xcp(3)*(1) ) -  ycp(1)*(1) - ycp(2)*(-2) - ycp(3)*(1) = 0 

	
    !Equation for LE point
    !xcp(3)*(1/6) + xcp(4)*(2/3) + xcp(5)*(1/6)  = uLE
    !ycp(3)*(1/6) + ycp(4)*(2/3) + ycp(5)*(1/6)  = vLE
	
    ! xcp(3) = xcp(2)+C_le*(xcp(4)-xcp(2))   --> (-1+C_le_x)*xcp(2) + xcp(3) - C_le_x   *xcp(4) = 0 then rotated by rotation matrix
    ! ycp(3) = ycp(2)+C_le*(ycp(4)-ycp(2))   --> (-1+C_le_y)*ycp(2) + ycp(3) - C_le_y   *ycp(4) = 0  then rotated by rotation matrix
    ! xcp(5) = xcp(6)+C_le*(xcp(4)-xcp(6))   --> - C_le_x   *xcp(4) + xcp(5) +(-1+C_le_x)*xcp(6) = 0  then rotated by rotation matrix
    ! ycp(5) = ycp(6)+C_le*(ycp(4)-ycp(6))   --> - C_le_y   *ycp(4) + ycp(5) +(-1+C_le_y)*ycp(6) = 0  then rotated by rotation matrix
	
    !Equations for bottom segment
    !xcp(5)*(1/6) + xcp(6)*(2/3) + xcp(7)*(1/6)  = xbot_le(1)
    !ycp(5)*(1/6) + ycp(6)*(2/3) + ycp(7)*(1/6)  = ybot_le(1)
    !slope_le_bot(1)*( xcp(5)*(-1/2) + xcp(7)*(1/2) )- ( ycp(5)*(-1/2) + ycp(7)*(1/2) ) = 0
    !curv_le_bot(1)*( xcp(5)*(1) + xcp(6)*(-2) + xcp(7)*(1) ) - ( ycp(6)*(1) + ycp(6)*(-2) + ycp(7)*(1) ) = 0

	!1- Fixed cp(3):   xcp(3) = uLE+C_le_x*(xtop_le(2)-uLE)  --> -xcp(1)*C_le*(1/6) - xcp(2)*C_le*(2/3) + (-C_le*(1/6)+1)* xcp(3) = (1-C_le)*uLE
	!1- Fixed cp(5):   xcp(5) = uLE+C_le_x*(xbot_le(1)-uLE)  --> xcp(5)*(1-C_le*(1/6)) - xcp(6)*C_le*(2/3) - C_le*(1/6)* xcp(7) = (1-C_le)*uLE
	

                                   !xcp(1),                 xcp(2),                xcp(3),                   xcp(4),                   ycp(1), ycp(2), ycp(3), ycp(4)    , RHS
     a(1,1:4)  = (/                    1/6.,                   2/3.,                   1/6.,                     0. /)
     a(1,8:11) = (/    0.,     0.,     0.,     0. /) ; a(1,15) = xtop_le
     a(2,1:4)  = (/                      0.,                     0.,                     0.,                     0. /)
     a(2,8:11) = (/  1/6.,   2/3.,   1/6.,     0. /) ; a(2,15) = ytop_le
     a(3,1:4)  = (/ 	  slope_le_top*(-1/2.),                     0., 	slope_le_top*(1/2.),                     0. /)
     a(3,8:11) = (/  1/2.,     0.,  -1/2.,     0. /) ; a(3,15) =0.
     a(4,1:4)  = (/             curv_le_top,         -2*curv_le_top,            	curv_le_top,                     0. /)
     a(4,8:11) = (/   -1.,     2.,    -1.,     0. /) ; a(4,15) =0.

     !This version sets the LE point at uLE, vLE  
                    !xcp(3), xcp(4), xcp(5)	 
     a(5,3:5)   = (/  1/6.,   2/3.,   1/6. /)   ; a(5,15)  = uLE
                    !ycp(3), ycp(4), ycp(5)
     a(6,10:12) = (/  1/6.,   2/3.,   1/6. /)   ; a(6,15)  = vLE
     
	 ! xcp(3),ycp(3)
	                 !xcp(1),      xcp(2),       xcp(3),        xcp(4),   				   ycp(1),       ycp(2),     ycp(3),        ycp(4),
!	 a(7, 1:4) = (/     0.,    (-1+C_le_x),        1.,        -C_le_x/) ; a(7,  8:11) = (/     0.,           0.,         0.,          0. /) 
!	 a(8, 1:4) = (/      0.,            0.,         0.,          0. /) ; a(8,  8:11) = (/     0.,    (-1+C_le_y),         1.,         -C_le_y/) 
	 !xcp(5),ycp(5)
	                 !xcp(4),     xcp(5),     xcp(6),        xcp(7),   				        ycp(4),       ycp(5),     ycp(6),        ycp(7),
!	 a(9, 4:7) = (/  (-C_le_x),      1.,  -1+C_le_x,          0. /) ; a(9, 11:14) = (/           0.,           0.,         0.,         0. /) 
!	 a(10,4:7) = (/     0.,           0.,         0.,          0. /) ; a(10,11:14) = (/   (-C_le_y),           1.,  -1+C_le_y,          0. /) 

! Rotated axes: xcp(3),ycp(3), xcp(5),ycp(5)
	 a(7, 1:4)  = (/0., (-1+C_le_x_top)*cos(le_camber_ang), cos(le_camber_ang),     -C_le_x_top*cos(le_camber_ang)/) 
	 a(7, 8:11) = (/0., (1-C_le_x_top)*sin(le_camber_ang), -sin(le_camber_ang),      C_le_x_top*sin(le_camber_ang)/)
	 a(8, 1:4)  = (/0., (-C_le_y_top)*sin(le_camber_ang),   sin(le_camber_ang), (-1+C_le_y_top)*sin(le_camber_ang)/)
	 a(8, 8:11) = (/0., (-C_le_y_top)*cos(le_camber_ang),   cos(le_camber_ang), (-1+C_le_y_top)*cos(le_camber_ang)/)

	 a(9, 4:7) 	= (/(-C_le_x_bot)*cos(le_camber_ang),   cos(le_camber_ang), (-1+C_le_x_bot)*cos(le_camber_ang),0. /) 
	 a(9, 11:14)= (/(C_le_x_bot)*sin(le_camber_ang),    -sin(le_camber_ang),(1-C_le_x_bot)*sin(le_camber_ang), 0. /) 
	 a(10,4:7) 	= (/(-1+C_le_y_bot)*sin(le_camber_ang), sin(le_camber_ang), -C_le_y_bot*sin(le_camber_ang),    0. /)
	 a(10,11:14)= (/(-1+C_le_y_bot)*cos(le_camber_ang), cos(le_camber_ang), -C_le_y_bot*cos(le_camber_ang),    0. /)

! Fixed cp3 and cp5
!	 a(7, 1:4) = (/   -C_le_x*1/6.,    -C_le_x*(2/3.),      1-C_le_x*(1/6.),           0./) ; a(7,  15) = (1-C_le_x)*uLE  							!1-
! 	 a(8, 1:4) = (/     1.,           -2.,         1.,          0. /)   ; a(8,  8:11) = (/     0.,           0.,         0.,           0./)     	!2-
!	 a(9, 4:7) = (/     0.,        1-C_le_x*(1/6.),        -C_le_x*(2/3.),  -C_le_x*(1/6.) /) ; a(9, 15) = (1-C_le_x)*uLE 							!1-
! 	 a(10,4:7) = (/     0.,          1.,         -2.,          1. /)      ; a(10,11:14) =  (/     0.,           0.,         0.,          0. /)  	!2-

!delta xcp(1,2)and(2,3) are equal and delta xcp(5,6)and(6,7) are equal
! 	 a(7, 1:4) = (/     1.,           -2.,         1.,          0. /)   ; a(7,  8:11) = (/     0.,           0.,         0.,           0./)     	!3-
! 	 a(9,4:7) = (/     0.,          1.,         -2.,          1. /)      ; a(9,11:14) =  (/     0.,           0.,         0.,          0. /)  	    !3-	 

! Letting the second and Six control point = point of contact:
!	 a(8, 2) =  1.   ; a(8, 15) = xtop_le(2)
! 	 a(10, 6) =  1.   ; a(10, 15) = xbot_le(1)	 
     
                                    !xcp(4),                 xcp(5),                 xcp(6),                  xcp(7),                    ycp(4), ycp(5), ycp(6), ycp(7)    , RHS     
     a(11,4:7)   = (/                      0.,                    1/6.,                  2/3.,                   1/6. /)
     a(11,11:14) = (/    0.,     0.,     0.,     0. /) ; a(11,15) = xbot_le
     a(12,4:7)   = (/                      0.,                      0.,                    0.,                     0. /)
     a(12,11:14) = (/    0.,   1/6.,   2/3.,   1/6. /) ; a(12,15) = ybot_le
     a(13,4:7)   = (/                      0., 	slope_le_bot*(-1/2.),                    0., 	slope_le_bot*(1/2.) /)
     a(13,11:14) = (/    0.,   1/2.,     0.,  -1/2. /) ; a(13,15) =        0.
     a(14,4:7)   = (/                      0.,             curv_le_bot,        -2*curv_le_bot,            curv_le_bot /)
     a(14,11:14) = (/    0.,    -1.,     2.,    -1. /) ; a(14,15) =        0.

!--------------------------------------------------------------------------------------------------------------------------	 
	else if ((degree == 4).and.(no_LE_segments == 4)) then
    !print*,'no_LE_segments =',no_LE_segments
	! for the 4th order bspline LE: -->(quartic b-spline)
! Equations for top and bottom 5 point cal.: _________a(16,16+1) total 8 control points
    ! Equations for top segment
    !xcp(1)*(1/24) + xcp(2)*(11/24) + xcp(3)*(11/24) + xcp(4)*(1/24)= xtop_le(2) 
    !ycp(1)*(1/24) + ycp(2)*(11/24) + ycp(3)*(11/24) + ycp(4)*(1/24)= ytop_le(2) 
    !slope_le_top(2) * ( xcp(1)*(-1/6)+xcp(2)*(-1/2)+ xcp(3)*(1/2)+xcp(4)*(1/6) ) - ycp(1)*(-1/6)-ycp(2)*(-1/2)- ycp(3)*(1/2)-ycp(4)*(1/6) = 0
    !curv_le_top * ( xcp(1)*(1/2) + xcp(2)*(-1/2) + xcp(3)*(-1/2)+ xcp(4)*(1/2) ) - ycp(1)*(1/2)-ycp(2)*(-1/2)- ycp(3)*(-1/2)-ycp(4)*(1/2) = 0
	!slope_curv_top*( xcp(1)*(-1) + xcp(2)*(3) + xcp(3)*(-3)+ xcp(4)*(1) ) - ycp(1)*(-1)-ycp(2)*(3)- ycp(3)*(-3)-ycp(4)*(1) = 0

	
    !Equation for LE point
    !xcp(3)*(1/24) + xcp(4)*(11/24) + xcp(5)*(11/24) + xcp(6)*(1/24)  = uLE
    !ycp(3)*(1/24) + ycp(4)*(11/24) + ycp(5)*(11/24) + ycp(6)*(1/24)  = vLE
	
    ! xcp(3) = xcp(2)+C_le*(xcp(4)-xcp(2))   --> (-1+C_le_x)*xcp(2) + xcp(3) - C_le_x   *xcp(4) = 0 then rotated by rotation matrix
    ! ycp(3) = ycp(2)+C_le*(ycp(4)-ycp(2))   --> (-1+C_le_y)*ycp(2) + ycp(3) - C_le_y   *ycp(4) = 0  then rotated by rotation matrix
    ! xcp(6) = xcp(7)+C_le*(xcp(5)-xcp(7))   --> - C_le_x   *xcp(5) + xcp(6) +(-1+C_le_x)*xcp(7) = 0  then rotated by rotation matrix
    ! ycp(6) = ycp(7)+C_le*(ycp(5)-ycp(7))   --> - C_le_y   *ycp(5) + ycp(6) +(-1+C_le_y)*ycp(7) = 0  then rotated by rotation matrix
	
    !Equations for bottom segment
    !xcp(5)*(1/24) + xcp(6)*(11/24) + xcp(7)*(11/24) + xcp(8)*(1/24)= xtop_le(2) 
    !ycp(5)*(1/24) + ycp(6)*(11/24) + ycp(7)*(11/24) + ycp(8)*(1/24)= ytop_le(2) 
    !slope_le_bot(2) * ( xcp(5)*(-1/6)+xcp(6)*(-1/2)+ xcp(7)*(1/2)+xcp(8)*(1/6) ) - ycp(5)*(-1/6)-ycp(6)*(-1/2)- ycp(7)*(1/2)-ycp(8)*(1/6) = 0
    !curv_le_bot * ( xcp(5)*(1/2) + xcp(6)*(-1/2) + xcp(7)*(-1/2)+ xcp(8)*(1/2) ) - ycp(5)*(1/2)-ycp(6)*(-1/2)- ycp(7)*(-1/2)-ycp(8)*(1/2) = 0
	!slope_curv_bot*( xcp(5)*(-1) + xcp(6)*(3) + xcp(7)*(-3)+ xcp(8)*(1) ) - ycp(5)*(-1)-ycp(6)*(3)- ycp(7)*(-3)-ycp(8)*(1) = 0

                                   !xcp(1),                 xcp(2),                 xcp(3),       			xcp(4),    xcp(5),                ycp(1), ycp(2), ycp(3), ycp(4), ycp(5)    , RHS
     a(1,1:5)  = (/                   1/24.,                 11/24.,                 11/24.,        			 1/24.,     0. /)
     a(1,9:13) = (/    0.,      0.,     0.,     0.,	0. /) ; a(1,17) = xtop_le
     a(2,1:5)  = (/                      0.,                     0.,                     0.,       	  			0., 	0. /)
     a(2,9:13) = (/  1/24., 11/24., 11/24.,  1/24.,  0. /) ; a(2,17) = ytop_le
     a(3,1:5)  = (/ 	  slope_le_top*(-1/6.),	  slope_le_top*(-1/2.), 	slope_le_top*(1/2.), 	slope_le_top*(1/6.),   	0. /)
     a(3,9:13) = (/  1/6.,  1/2.,   -1/2.,   -1/6.,  0. /) ; a(3,17) = 0.
     a(4,1:5)  = (/      curv_le_top*(1/2.),    curv_le_top*(-1/2.),    curv_le_top*(-1/2.), 	curv_le_top*(1/2.),     0. /)
     a(4,9:13) = (/  -1/2.,  1/2.,   1/2.,   -1/2.,  0. /) ; a(4,17) = 0.
	 a(5,1:5)  = (/    slope_curv_top*(-1.),    slope_curv_top*(3.),   slope_curv_top*(-3.),    slope_curv_top*(1.),     0. /)
     a(5,9:13) = (/   1.,  	  -3.,    3.,     -1.,  0. /) ; a(5,17) = 0.
	 
     !This version sets the LE point at uLE, vLE  
                    !xcp(3), xcp(4), xcp(5), xcp(6)
     a(6,3:6)   = (/  1/24., 11/24., 11/24.,  1/24. /)   ; a(6,17)  = uLE
                    !ycp(3), ycp(4), ycp(5), ycp(6)
     a(7,11:14) = (/  1/24., 11/24., 11/24.,  1/24. /)   ; a(7,17)  = vLE

! Rotated axes: xcp(3),ycp(3), xcp(6),ycp(6)
	 a(8, 1:5  ) = (/0., (-1+C_le_x_top)*cos(le_camber_ang),  cos(le_camber_ang),     -C_le_x_top*cos(le_camber_ang), 0./) 
	 a(8, 9:13 ) = (/0.,  (1-C_le_x_top)*sin(le_camber_ang), -sin(le_camber_ang),      C_le_x_top*sin(le_camber_ang), 0./)
	 a(9, 1:5  ) = (/0.,   (-C_le_y_top)*sin(le_camber_ang),  sin(le_camber_ang), (-1+C_le_y_top)*sin(le_camber_ang), 0./)
	 a(9, 9:13 ) = (/0.,   (-C_le_y_top)*cos(le_camber_ang),  cos(le_camber_ang), (-1+C_le_y_top)*cos(le_camber_ang), 0./)
	 a(10,4:8  ) = (/0.,   (-C_le_x_bot)*cos(le_camber_ang),  cos(le_camber_ang), (-1+C_le_x_bot)*cos(le_camber_ang), 0./) 
	 a(10,12:16) = (/0.,    (C_le_x_bot)*sin(le_camber_ang), -sin(le_camber_ang),  (1-C_le_x_bot)*sin(le_camber_ang), 0./) 
	 a(11,4:8  ) = (/0., (-1+C_le_y_bot)*sin(le_camber_ang),  sin(le_camber_ang),     -C_le_y_bot*sin(le_camber_ang), 0./)
	 a(11,12:16) = (/0., (-1+C_le_y_bot)*cos(le_camber_ang),  cos(le_camber_ang),     -C_le_y_bot*cos(le_camber_ang), 0./)
     
                                    !xcp(4),                 xcp(5),                  xcp(6),                 xcp(7),					xcp(8),                    ycp(4), ycp(5), ycp(6), ycp(7),  ycp(8)   , RHS     
     a(12,4:8)   = (/0.,                   1/24.,                 11/24.,                 11/24.,   	 		  1/24. /)
     a(12,12:16) = (/    0.,     0.,     0.,     0.,      0. /) ; a(12,17) = xbot_le 
     a(13,4:8)   = (/0.,                      0.,                     0.,            		    0.,                  0. /)
     a(13,12:16) = (/    0.,   1/24., 11/24., 11/24.,  1/24. /) ; a(13,17) = ybot_le
     a(14,4:8)   = (/0., 	  slope_le_bot*(-1/6.),	  slope_le_bot*(-1/2.),    slope_le_bot*(1/2.), slope_le_bot*(1/6.) /)
     a(14,12:16) = (/    0.,   1/6.,  1/2.,   -1/2.,   -1/6. /) ; a(14,17) = 0.
     a(15,4:8)   = (/0.,      curv_le_bot*(1/2.),    curv_le_bot*(-1/2.),    curv_le_bot*(-1/2.),    curv_le_bot*(1/2.) /)
     a(15,12:16) = (/    0.,    -1/2.,  1/2.,   1/2.,  -1/2. /) ; a(15,17) = 0.
	 a(16,4:8)   = (/0.,    slope_curv_bot*(-1.),    slope_curv_bot*(3.),   slope_curv_bot*(-3.),   slope_curv_bot*(1.) /)
     a(16,12:16) = (/    0.,  	  1., 	  -3.,     3.,    -1. /) ; a(16,17) = 0.	 

!-----------------------------------------------------------------------------------------------------------------------------   
   else if ((degree == 4).and.(no_LE_segments == 5)) then
     !print*,'no_LE_segments =',no_LE_segments   
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
    !xcp(6)*(1/24) + xcp(7)*(11/24) + xcp(8)*(11/24) + xcp(9)*(1/24)= xbot_le(2) 
    !ycp(6)*(1/24) + ycp(7)*(11/24) + ycp(8)*(11/24) + ycp(9)*(1/24)= ybot_le(2) 
    !slope_le_bot(2) * ( xcp(6)*(-1/6)+xcp(7)*(-1/2)+ xcp(8)*(1/2)+xcp(9)*(1/6) ) - ycp(6)*(-1/6)-ycp(7)*(-1/2)- ycp(8)*(1/2)-ycp(9)*(1/6) = 0
    !curv_le_bot * ( xcp(6)*(1/2) + xcp(7)*(-1/2) + xcp(8)*(-1/2)+ xcp(9)*(1/2) ) - ycp(6)*(1/2)-ycp(7)*(-1/2)- ycp(8)*(-1/2)-ycp(9)*(1/2) = 0
	!slope_curv_bot*( xcp(6)*(-1) + xcp(7)*(3) + xcp(8)*(-3)+ xcp(9)*(1) ) - ycp(6)*(-1)-ycp(7)*(3)- ycp(8)*(-3)-ycp(9)*(1) = 0
	
	! Deviated control points due to ss deviation:
	! xcp(4) = xcp_hat*(uo-uLE)
	! ycp(4) = ycp_hat*(thk_LE)
	! xcp(6) = xcp_hat*(uo-uLE)
	! ycp(6) = ycp_hat*(thk_LE)
	

                                   !xcp(1),                 xcp(2),                 xcp(3),       			xcp(4),    xcp(5),                ycp(1), ycp(2), ycp(3), ycp(4), ycp(5)    , RHS
     a(1,1:5)   = (/                   1/24.,                 11/24.,                 11/24.,        			 1/24.,     0. /)
     a(1,10:14) = (/    0.,      0.,     0.,     0.,	0. /) ; a(1,19) = xtop_le
     a(2,1:5)   = (/                      0.,                     0.,                     0.,       	  			0., 	0. /)
     a(2,10:14) = (/  1/24., 11/24., 11/24.,  1/24.,  0. /) ; a(2,19) = ytop_le
     a(3,1:5)   = (/ 	  slope_le_top*(-1/6.),	  slope_le_top*(-1/2.), 	slope_le_top*(1/2.), 	slope_le_top*(1/6.),   	0. /)
     a(3,10:14) = (/  1/6.,  1/2.,   -1/2.,   -1/6.,  0. /) ; a(3,19) = 0.
     a(4,1:5)   = (/      curv_le_top*(1/2.),    curv_le_top*(-1/2.),    curv_le_top*(-1/2.), 	curv_le_top*(1/2.),     0. /)
     a(4,10:14) = (/  -1/2.,  1/2.,   1/2.,   -1/2.,  0. /) ; a(4,19) = 0.
	 a(5,1:5)   = (/    slope_curv_top*(-1.),    slope_curv_top*(3.),   slope_curv_top*(-3.),    slope_curv_top*(1.),     0. /)
     a(5,10:14) = (/   1.,  	  -3.,    3.,     -1.,  0. /) ; a(5,19) = 0.
	 
     !This sets the LE control point at uLE, vLE  
                   ! xcp(5)	 
     !a(6,5)   = 1.   ; a(6,19)  = uLE
                   ! ycp(5)
     !a(7,14)  = 1.   ; a(7,19)  = vLE
	 
	 !This sets the LE point at uLE, vLE in the middle of segment 3 
                   !xcp(3), xcp(4), xcp(5), xcp(6)	xcp(7) 
     a(6,3:7)  = (/  1/384.,  19/96., 115/192.,	19/96.,  1/384. /)   ; a(6,19)  = uLE
                   !ycp(3), ycp(4), ycp(5), ycp(6), ycp(7)
     a(7,12:16)= (/  1/384.,  19/96., 115/192.,	19/96.,  1/384. /)   ; a(7,19)  = vLE

! Rotated axes: xcp(3),ycp(3), xcp(7),ycp(7)
	 !a(8, 	1:5) 	= (/     0.,    (-1+C_le_x_top)*cos(le_camber_ang),        cos(le_camber_ang),          -C_le_x_top*cos(le_camber_ang), 	0./) 
	  !a(8,  10:14) 	= (/     0.,     (1-C_le_x_top)*sin(le_camber_ang),       -sin(le_camber_ang),           C_le_x_top*sin(le_camber_ang), 	0./)
	 !a(9, 	1:5) 	= (/     0.,    (-C_le_y_top)*sin(le_camber_ang),   	   sin(le_camber_ang),      (-1+C_le_y_top)*sin(le_camber_ang), 	0./)
	  !a(9,  10:14) 	= (/     0.,    (-C_le_y_top)*cos(le_camber_ang),    	   cos(le_camber_ang),      (-1+C_le_y_top)*cos(le_camber_ang), 	0./)
	 !a(10, 5:9)	= (/     0.,    (-C_le_x_bot)*cos(le_camber_ang),          cos(le_camber_ang),      (-1+C_le_x_bot)*cos(le_camber_ang),   	0./) 
	  !a(10, 14:18) = (/     0.,     (C_le_x_bot)*sin(le_camber_ang),         -sin(le_camber_ang),       (1-C_le_x_bot)*sin(le_camber_ang),   	0./) 
	 !a(11,	5:9) 	= (/     0.,  (-1+C_le_y_bot)*sin(le_camber_ang),   	   sin(le_camber_ang),          -C_le_y_bot*sin(le_camber_ang),    	0./)
	  !a(11,	14:18) 	= (/     0.,  (-1+C_le_y_bot)*cos(le_camber_ang),   	   cos(le_camber_ang),          -C_le_y_bot*cos(le_camber_ang),    	0./)

! Zero Camber equations for 3rd anf 7th cp:
	 !a(8, 	1:5) 	= (/     0.,  (-1+C_le_x_top),        	1.,          -C_le_x_top, 		0./) 
	 a(9,  	10:14) 	= (/     0.,    (-C_le_y_top),    	   	1.,      (-1+C_le_y_top), 		0./)
	 !a(10, 	5:9)	= (/     0.,    (-C_le_x_bot),          1.,      (-1+C_le_x_bot),   	0./) 
	 a(11,	14:18) 	= (/     0.,  (-1+C_le_y_bot),   	   	1.,          -C_le_y_bot,    	0./)
	  
! Equations determining the derivatives at the bottom:	  
                                    !xcp(5),                 xcp(6),                  xcp(7),                 xcp(8),					xcp(9),                    ycp(4), ycp(5), ycp(6), ycp(7),  ycp(8)   , RHS     
     a(12,5:9)   = (/0.,                1/24.,               11/24.,               11/24.,   	 		   1/24. /)
     a(12,14:18) = (/    0.,     0.,     0.,     0.,      0. /) ; a(12,19) = xbot_le 
     a(13,5:9)   = (/0.,                   0.,                   0.,            		 0.,              	  0. /)
     a(13,14:18) = (/    0.,   1/24., 11/24., 11/24.,  1/24. /) ; a(13,19) = ybot_le
     a(14,5:9)   = (/0., slope_le_bot*(-1/6.), slope_le_bot*(-1/2.),  slope_le_bot*(1/2.),   slope_le_bot*(1/6.) /)
     a(14,14:18) = (/    0.,   1/6.,  1/2.,   -1/2.,   -1/6. /) ; a(14,19) = 0.
     a(15,5:9)   = (/0.,   curv_le_bot*(1/2.),  curv_le_bot*(-1/2.),  curv_le_bot*(-1/2.),    curv_le_bot*(1/2.) /)
     a(15,14:18) = (/    0.,    -1/2.,  1/2.,   1/2.,  -1/2. /) ; a(15,19) = 0.
	 a(16,5:9)   = (/0., slope_curv_bot*(-1.),  slope_curv_bot*(3.), slope_curv_bot*(-3.),   slope_curv_bot*(1.) /)
     a(16,14:18) = (/    0.,  	  1., 	  -3.,     3.,    -1. /) ; a(16,19) = 0.	 

	 !print*,'xcp4_hat',xcp4_hat
	 !print*,'xcp6_hat',xcp6_hat
! Equations to define the LE shape and droop: 4th and 6th control points manually through controlinput
	 ! a(17,4)  	= 1.		; a(17,19)  =  (1-xcp4_hat)	*(uin_le - uLE)
	 ! a(18,6)  	= 1.		; a(18,19)  =  (1-xcp6_hat)	*(uin_le - uLE)
	 ! a(8,13)  	= 1.		; a(8 ,19) 	=  ycp4_hat	*((le_thk/2.)-vLE)
	 ! a(10,15) 	= -1. 		; a(10,19) 	=  ycp6_hat *((le_thk/2.)-vLE)

! Equations controlling the LE shape: 4th and 6th control points...
 ! Using a vertex angle and length
	 LE_vertex_ang_rad = LE_vertex_ang*pi/180. !get the radian value
	 LE_extens = (LE_vertex_dis)*sqrt((uin_le - uLE)**2+(le_thk/2.-vLE)**2)
	 ! print*, "LE_vertex_dis= ------------------------------",LE_vertex_dis
	 a(17,4:5)  	= (/ 1.,    -1./)		; a(17,19) 	=  LE_extens*cos(LE_vertex_ang_rad/2.)
	 a(18,5:6) 		= (/ -1.,    1./) 		; a(18,19) 	=  LE_extens*cos(LE_vertex_ang_rad/2.)
	 a(8,13:14)  	= (/ 1.,    -1./)		; a(8 ,19) 	=  LE_extens*sin(LE_vertex_ang_rad/2.)
	 a(10,14:15) 	= (/ -1.,    1./) 		; a(10,19) 	=  -LE_extens*sin(LE_vertex_ang_rad/2.)

	 else
		print*,'Error in choosing the bspline degree or number of segments ...'
		stop
	endif

   !print*, "a = "
   
    !do i = 1,18
    !    print*, a(i,:)
    !enddo
    
     call gauss_jordan ( 2*(degree+no_LE_segments), 1, a, info )
	 ! print*, "info==0 == ", info
	 if (info.ne.0) then 
		print*,'LE spline Singular Matrix ...'
		stop
	 endif

     xcp(1:ncp) =  a(1			  : (degree+no_LE_segments) ,2*(degree+no_LE_segments)+1)  ! a(1:9,15)
     ycp(1:ncp) =  a((degree+no_LE_segments)+1 :2*(degree+no_LE_segments),2*(degree+no_LE_segments)+1)  !a(10:18,15)

    do i = 1,ncp	 
	 ! print*,i,'xcp',xcp(i),' ','ycp',ycp(i)
	end do
    ! print*,'========================================='

   ! stop	

!--------------------------------------------------------  
! computing the end points for each segment:
	   t = 0
		if (degree == 3) then
			x_spl_end(1) = bspline(xcp(1:degree+1),t)
			y_spl_end(1) = bspline(ycp(1:degree+1),t)
		elseif (degree == 4) then
			x_spl_end(1) = bspline4(xcp(1:degree+1),t)
			y_spl_end(1) = bspline4(ycp(1:degree+1),t)
		endif
       t = 1
       do j = 1,ncp-degree
           x_cp = xcp(j:j+degree)
		   y_cp = ycp(j:j+degree)
		   if (degree == 3) then
			x_spl_end(j+1) = bspline(x_cp,t)
			y_spl_end(j+1) = bspline(y_cp,t)
		   elseif (degree == 4) then
			x_spl_end(j+1) = bspline4(x_cp,t)
			y_spl_end(j+1) = bspline4(y_cp,t)
		   endif
       enddo
		 
       call bspline_arclength(arclength,xcp,ycp,ncp,degree)
       ! print*, 'arclength', arclength
 ! xs = x1(control point)*B1+x2(control point)*B2+x3(control point)
 !                                         *B3+x4(control point)*B4
 ! clustering the spacing of the LE spline
 	    center = 0.5
		do j=1,(2*le_pos-1)
			u = real(j-1)/real(2*le_pos-2)
			if (u < 0.5) then
				s = center*(sin( pi * u ))
			else
				s = 1 - (1-center)*(cos( pi * (u-0.5) ))
		    endif
            !s = u !Uniform spacing
            !print*, "u, s", u, s
			x_le_spl(j) = bspline_cp(xcp,arclength,ncp,degree,s)
			y_le_spl(j) = bspline_cp(ycp,arclength,ncp,degree,s)	
		enddo   

endsubroutine le_matrix_sol

!*****************************************************************
!*****************************************************************
!*****************************************************************
subroutine sum_alpha_function(xtop,ytop,xbot,ybot,dimen,le_pos,x_le_spl,y_le_spl,sum_alpha_le_spl_top,sum_alpha_le_spl_bot)
! Calculation of the function to be minimized, the change in spline segment angles:
	implicit none
	
	  integer j,k
	  integer, intent(in) :: dimen,le_pos
	  real, intent(in) :: x_le_spl(2*le_pos-1),y_le_spl(2*le_pos-1)
      real, intent(in) :: xtop(dimen), ytop(dimen)
      real, intent(in) :: xbot(dimen), ybot(dimen)
	  real le_spl_alpha_top(dimen+le_pos-1),le_spl_arc_len_top(dimen+le_pos-1),delta_alpha_le_spl_top(dimen+le_pos-2)
	  real x_le_spl_full(2*dimen+2*le_pos-1),y_le_spl_full(2*dimen*le_pos-1)
	  real le_spl_alpha_bot(dimen+le_pos-1),le_spl_arc_len_bot(dimen+le_pos-1),delta_alpha_le_spl_bot(dimen+le_pos-2)
      real, intent(out) :: sum_alpha_le_spl_bot,sum_alpha_le_spl_top
	  
!---------------------------------- LE optimization:
 !Calculation of the angle change between the arclength segments of the LE spline:
	! this to ensure smooth transition from blade surface ,
 	x_le_spl_full(1:dimen)=xtop
	y_le_spl_full(1:dimen)=ytop
	x_le_spl_full(dimen+1:dimen+2*le_pos-1)=x_le_spl
	y_le_spl_full(dimen+1:dimen+2*le_pos-1)=y_le_spl
	x_le_spl_full(dimen+2*le_pos:2*dimen+2*le_pos-1)=xbot(dimen:1:-1)
	y_le_spl_full(dimen+2*le_pos:2*dimen+2*le_pos-1)=ybot(dimen:1:-1)
	! printing:
	! do j=1,2*dimen+2*le_pos-1
		! print*,'x, y _le_spl_full',x_le_spl_full(j),y_le_spl_full(j)
	! enddo
	! print*,' ' 
	! do j=1,dimen
		! print*,'x, y  top',xtop(j),ytop(j)
	! enddo
	! print*,' ' 
	! do j=1,dimen
		! print*,'x, y  bot',xbot(j),ybot(j)
	! enddo

	
 ! TOP: 
	do j=1,dimen+le_pos-1 
		! print*,'1',j,dimen+le_pos-1,2*dimen+2*le_pos-1
		le_spl_alpha_top(j) = atan2((y_le_spl_full(j+1)-y_le_spl_full(j)),(x_le_spl_full(j+1)-x_le_spl_full(j)))
		! print*, 'le_spl_alpha_top',le_spl_alpha_top(j),j,dimen+le_pos-3
	enddo
	!print*, 'le_spl_alpha_top',le_spl_alpha_top
	
	!Get the difference between segment angles/ arc length :
	delta_alpha_le_spl_top(1) = abs(le_spl_alpha_top(2)-le_spl_alpha_top(1))
	do j=1,dimen+le_pos-3  ! we can do this only on one side
		delta_alpha_le_spl_top(j+1) = abs(le_spl_alpha_top(j+1)-le_spl_alpha_top(j))
	enddo
	! print*, 'delta_alpha_le_spl_top',delta_alpha_le_spl_top
	
	! Calculate the sum of alpha of the spline segments
	sum_alpha_le_spl_top = sum(delta_alpha_le_spl_top)
	! print*, 'sum_alpha_le_spl_top =',sum_alpha_le_spl_top
   !--------------
! Bottom:
 ! this to ensure smooth transition from blade surface ,
	k = 1
	do j=2*dimen+2*le_pos-1,dimen+1+le_pos,-1
		!print*, 2*dimen+2*le_pos-2,j,j-1,dimen+1+le_pos-1
		le_spl_alpha_bot(k) = atan2((y_le_spl_full(j-1)-y_le_spl_full(j)),(x_le_spl_full(j-1)-x_le_spl_full(j)))
		! print*, 'le_spl_alpha_bot',le_spl_alpha_bot(k),j,k,dimen+le_pos-3
		k=k+1
	enddo
	
	
	!Get the difference between segment angles/ arc length :
	delta_alpha_le_spl_bot(1) = abs(le_spl_alpha_bot(2)-le_spl_alpha_bot(1))
	do j=1,dimen+le_pos-3
		delta_alpha_le_spl_bot(j+1) = abs(le_spl_alpha_bot(j+1)-le_spl_alpha_bot(j))
	enddo
	! print*, 'delta_alpha_le_spl_bot',delta_alpha_le_spl_bot
		
	! Calculate the sum of alpha of the spline segments		
	sum_alpha_le_spl_bot = sum(delta_alpha_le_spl_bot)
	! print*, 'sum_alpha_le_spl_bot =',sum_alpha_le_spl_bot
	! stop
endsubroutine sum_alpha_function

!********************************************************
!********************************************************
!********************************************************
