!*************************************************************************************************
subroutine bladegen(nspn,thkc,mr1,sinl,sext,chrdx,js,fext,xcen,ycen,airfoil, &
                    stagger,stack,chord_switch,stk_u,stk_v,xb_stk,yb_stk,stack_switch, &
                    clustering_switch, clustering_parameter,nsl,nbls,curv_camber,thick,LE,np, ncp_curv,ncp_thk, &
                    curv_cp,thk_cp, wing_flag, lethk_all,tethk_all,s_all,ee_all,thick_distr,thick_distr_3_flag, &
                    umxthk_all,C_le_x_top_all,C_le_x_bot_all,C_le_y_top_all,C_le_y_bot_all,&
	                LE_vertex_ang_all,LE_vertex_dis_all,sting_l_all,sting_h_all,LEdegree,no_LE_segments,&
	                sec_radius,bladedata,amount_data,scf,intersec_coord,throat_index, &
                    n_normal_distance,casename,develop,isdev,mble,mbte,msle,mste,i_slope,jcellblade_all, &
                    etawidth_all,BGgrid_all,thk_tm_c_spl,isxygrid, theta_offset, te_flag, &
					le_opt_flag, te_opt_flag, le_angle_all, te_angle_all)

!-----------------------------------------
! Input:
!   thkc is the max thickness to chord ratio
!   mr1 is the leading edge relative Mach number. 
!   It is used for estimating some blade parameters based on Mach number
!   sinl is the leading edge blade angle (tan(beta le))
!   sext is the trailing edge blade angle (tan(beta te))
!   chrdx is the meridional (axial) chord (make 1)
!   pitch is not used except to draw the adjacent blade
! Output:
!   2D blade section coordinates in m' and theta
!------------------------------------------ 
!******************************************************************************************
! CODE STRUCTURE : ORDER OF OPERATION
!******************************************************************************************
! 01. Start Subroutine with variable declarations and Constants.

! 02. Printing Messages to Screen and assigning airfoil type.

! 03. Assigning number of coordinates on the 2D airfoils.

! 04. Allocation of variables and initialization.

! 05. Generate spacing array or clustering for the airfoil coordinates.

! 06. Set def. blade params related to thickness at LE/TE and location of max thickness.
	! a. GENERAL PROCEDURE: Airfoil definition-->Stacking point-->Stagger and Scale-->Write blade coordinates.

! 07. Curvature_Camber switches and options.
	! a. Option 1: No curvature manipulation: 
		! i ) Circle.
		! ii ) S809 for Wind Turbines.
		! iii) Propeller airfoil ClarkY.
		! iv ) NACA 4 series airfoils with circular TE.
		! v ) Default Airfoil section (sect1) with mixcamber.
		! vi ) User defined airfoil shape from input. (Refer Manual for the file format)

! b. Option 2: Curvature control for camber definition.

! 08. Thickness definition options and LE definition options.
	! a. Generate a thickness spline multiplier.
	! b. Spline thickness distr. with LE control.
	! c. LE options: Spline, Sting, Elliptical.

! 09. Store bladedata for each section.

! 10. Calculation of the 2D throat.

! 11. Writing the section properties into a single file.

! 12. 2D blade grid generation.

! 13. Calculation of Geometric Zwiefel Number.

! 14. Deallocation of variables.

! 15. Format Statements.

! 16. End Subroutine.
!******************************************************************************************

!******************************************************************************************
! Variables Declaration.
!******************************************************************************************
implicit none

integer np, np_side, i, j, l, k, js, naca, chord_switch, istack, thick_distr, nxx
integer nx, nax, nrow, nspn, nspan, npoints, nsl, nbls, ncp, le_pos, wing_flag
integer const_stk_u, const_stk_v, stack, stack_switch, clustering_switch, te_flag, le_opt_flag, te_opt_flag
integer curv_camber, thick, LE, LEdegree, n_normal_distance
integer TE_del, ii, ngrid, amount_data
integer ncp_curv(nsl), ncp_thk(nsl), i_le, i_te, oo, nb, no_LE_segments
integer interval, pt2, throat_index(nspn), spline_data, i_slope, BGgrid_all(nspn), BGgrid

parameter(nspan = 200, nx = 500, nxx = 1000, nrow = 1, nax = 50, nb = 300, spline_data = 6)
parameter(interval = 6, pt2 = 1, TE_del = 0) 	! parameter to choose the position of the second point on the blade
! interval is the number of intervals for the LE spline values refinement process
real*8 bladedata(amount_data, nsl)
real sec_radius(nsl, 2)
real*8 :: clustering_parameter
real :: sinl, sext, tempr, chrdx, chrd, stagger, pitch, radius_pitch
!real xtop(nb), ytop(nb), xbot(nb), ybot(nb), u(nb)
!real, intent(out) :: xb(nx), yb(nx)
!real splthick(nb), thickness(nb), angle(nb), camber(nb), slope(nb)
real, allocatable, dimension(:) :: xtop, ytop, xbot, ybot, u, xb, yb
real, allocatable, dimension(:) :: splthick, thickness, angle, camber, slope
real umin, umax, scaling, deltau, theta_offset
real vtop_stack, vbot_stack, xb_stk, yb_stk
real, allocatable, dimension(:, :) :: in_beta, out_beta, thickness_data
real*8, allocatable, dimension(:, :) :: splinedata
real, allocatable, dimension(:, :) :: chord, mrel1
real, allocatable, dimension(:) :: xcp, ycp
real X(nx, nx, 5), Y(nx, nx, 5)
real sb(nx), xbs, ybs, min_x, max_x
real lethk, mxthk, mr1, thkc, thkmultip
real aext, ainl, ang, apx1, apx2, area 
real cam, cam_u, dsi, dsmx, dsmn, dtor, pi, du, curv
real ei11, ei22, flex, flin, fmxthk, frat, frm, frmx
real rr1, rr2, sang, sexts, sinls, tethk, thk, ui, umx, umxthk
real xcen, ycen, xi, yi, xxa, yya, ucen, vcen
real xcg(nspan), ycg(nspan)
real u_stack, v_stack, v_zero_stack
real u_le, thk_le, slope_le, curv_le, uin_le
real u_lespline(20), thk_lespline(20), uin_lespline(20)
real slope_le_bot1, slope_le_top1, slope_le_bot2, slope_le_top2
real theta_bot1, theta_bot2, theta_top1, theta_top2, camber_le(interval+1)
real arc_bot, arc_top, curv_le_bot, curv_le_top, camber_ang(interval+1)
real curv_cp(20, 2*nsl), thk_cp(20, 2*nsl)
real lethk_all(nsl), tethk_all(nsl), s_all(nsl), ee_all(nsl), umxthk_all(nsl), thk_tm_c_spl(nsl)
real C_le_x_top_all(nsl), C_le_x_bot_all(nsl), C_le_y_top_all(nsl), C_le_y_bot_all(nsl)
real LE_vertex_ang_all(nsl), LE_vertex_dis_all(nsl), le_angle_all(nsl), te_angle_all(nsl)
real Zwiefel(nsl), sting_l_all(nsl), sting_h_all(nsl, 2)
real jcellblade_all(nspn), etawidth_all(nspn), jcellblade, etawidth
real, allocatable, dimension(:) :: xtop_refine, ytop_refine, xbot_refine, ybot_refine
real, allocatable, dimension(:) :: init_angles, init_cambers, x_spl_end_curv, cam_refine, u_refine
real ucp_top(11), vcp_top(11), ucp_bot(11), vcp_bot(11)
real le_throat, te_throat, intersec_coord(12, nsl), min_throat_2D, attach_angle
real u_translation, camber_trans
! variables for s809 profile
real x_s809ss(nx), y_s809ss(nx), x_s809ps(nx), y_s809ps(nx)
real x_ss(nx), y_ss(nx), x_ps(nx), y_ps(nx)
real xs809(nx), ys809(nx), x_s809(nx), y_s809(nx)
real x_clarky(nx), y_clarky(nx), x_file1(nx), y_file1(nx)
real xn(nx), yn(nx), b(nx), alpha(nx), xi1, yi1
real, allocatable, dimension(:) :: xcp_curv, ycp_curv, x_le_spl, y_le_spl
real, allocatable, dimension(:) :: xcp_thk, ycp_thk, curvature
!  real, allocatable, dimension(:) :: xi, yi
real le_ee, le_s, te_ee, te_s
real stk_u(1), stk_v(1)
real scaled, XINT, YINT, scf		! scf main scale factor
real u_rot, camber_rot, mble, mbte, msle, mste
! meanline variables
real t(nb), mprime(nb), theta(nb), dmp, stingl, ueqi, dueq
real, allocatable, dimension(:) :: ueq, xmean, ymean

character*120 line, temp
character*32 fext, fname, fname1, fname3, fname4, blext, casename, develop
character*80 file1, file2, file3, file4, file5, file6, file7
character*20 airfoil, sec
character*16 thick_distr_3_flag
logical error, ellip, isdev, isxygrid

common / BladeSectionPoints /xxa(nxx, nax), yya(nxx, nax) 

i_le = 0
i_te = 0

!******************************************************************************************
! Building 2D blade section using mean camber & thickness profile or other defined airfoils
!******************************************************************************************   

! -----------------------------------------------------------------------------
! --------Constants---------( Pi value)----------------------------------------
pi = 4.*atan(1.0)
dtor = PI/180.
! -----------------------------------------------------------------------------

! needed to protect write(33) if values are not otherwise defined
ucp_top = 0
vcp_top = 0

!*******************************************************************************************
! Printing Messages to Screen and assigning airfoil type
!*******************************************************************************************
write(sec, '(i2)')js ! converting the radial section number into a character to go into the filename
if(len_trim(airfoil).eq.4)then
	naca = 0
	read (airfoil, '(i4)') naca
	print*, "naca = ", naca
endif
!print*, airfoil
write(*, *)
write(*, *)"---------------------------------------------------------------------------------"
print*, 'airfoil type:', js, trim(airfoil)
print*, ' =================================================== '
!*******************************************************************************************

!*******************************************************************************************
! Assigning number of coordinates on the 2D airfoils
!*******************************************************************************************
!---- # of points
np = 121! nodes for the grid generator100
np_side = np

!*******************************************************************************************
! Allocation of variables and initialization
!*******************************************************************************************
if (allocated(splinedata)) deallocate(splinedata)
if (allocated(xtop      )) deallocate(xtop      )
if (allocated(ytop      )) deallocate(ytop      )
if (allocated(xbot      )) deallocate(xbot      )
if (allocated(ybot      )) deallocate(ybot      )
if (allocated(u         )) deallocate(u         )
if (allocated(splthick  )) deallocate(splthick  )
if (allocated(thickness )) deallocate(thickness )
if (allocated(angle     )) deallocate(angle     )
if (allocated(camber    )) deallocate(camber    )
if (allocated(slope     )) deallocate(slope     )
if (allocated(xb        )) deallocate(xb        )
if (allocated(yb        )) deallocate(yb        )
if (allocated(ueq       )) deallocate(ueq       )
if (allocated(xmean     )) deallocate(xmean     )
if (allocated(ymean     )) deallocate(ymean     )
if (allocated(thickness_data )) deallocate(thickness_data )
! Allocating all the blade data arrays:
Allocate(splinedata(spline_data, np_side)) 
Allocate(xtop(np))
Allocate(ytop(np))
Allocate(xbot(np))
Allocate(ybot(np))
Allocate(u(np))
Allocate(splthick(np))
Allocate(thickness(np))
Allocate(thickness_data(np, 12))
Allocate(angle(np))
Allocate(camber(np))
Allocate(slope(np))
Allocate(xb(2*np-1))
Allocate(yb(2*np-1))
!uniform clustering for the blade and meanline
Allocate(ueq(np_side))
Allocate(xmean(np_side))
Allocate(ymean(np_side))
!
u = 0; splthick = 0; thickness = 0;angle = 0;camber = 0; slope = 0; xb = 0; yb = 0
xtop = 0; ytop = 0; xbot = 0; ybot = 0; splinedata = 0
!
ueq = 0; xmean = 0; ymean = 0
!
! !---- spacing parameters
! du = 1.0/(np-1)
! dsmn = du
! dsmx = np*dsmn! it was 100 for np = 100 8/16/13
!*******************************************************************************************
! Generate spacing array or clustering for the airfoil coordinates ----------------
!*******************************************************************************************
! This increases the points in leading and trailing edges...Clustering
if (clustering_switch .eq. 0) then
    call uniform_clustering(np,u)
else if (clustering_switch .eq. 1) then
    call sine_clustering(np,u)
    u = u/u(np)
else if (clustering_switch .eq. 2) then
    call exponential_clustering(np,u)
else if (clustering_switch .eq. 3) then
    call hyperbolic_tan_clustering(np,u)
else
    print *, 'Invalid argument for clustering switch: Quitting!!'
    stop
end if
!u(1) = 0.0
!ueq(1) = 0.0 ! uniform clustering
!do i = 2, np
!	ui = real(i-1)/real(np) ! Marshall 8/19/13
!	du = (sin(pi*ui))**5.5
!	u(i) = u(i-1) + du 
!	!! uniform clustering
!	dueq = ui 
!	ueq(i) = ueq(i-1) + dueq ! Kiran 12/27/13
!enddo
!u = u/u(np) ! Non dimensionalizing
! do i = 2, np
! ui = du*(i-2)
! dsi = dsmn + (dsmx-dsmn)*sin(pi*ui)**2.0 
! u(i) = u(i-1) + dsi
! enddo
! umx = u(np)
! do i = 1, np
! u(i) = u(i)/umx
! enddo
!
!*******************************************************************************************
! Set def. blade params related to thickness at LE/TE and location of max thickness --------
!*******************************************************************************************
flin = 0.10
flex = 0.05   
!fmxthk = 0.1
fmxthk = thkc  !----- use input t/c thick to chord ratio
umxthk = umxthk_all(js)
! lethk = thk_tm_c_spl(js)*lethk_all(js) ! as thichness of blade changes (tm/c) the LEthk changes too nemnem
lethk = lethk_all(js)
print*, "lethk = ", lethk
tethk = tethk_all(js)   
!
!---- true for elliptical le/te
ellip = .true.
!---- default axis ratio for elliptical le/te
rr1 = 2.5
rr2 = 1.0
!
sinls = sinl
sexts = sext
!---- estimate parameters from upstream 
!....(M Changed at 11/1/2013 Noot needed for supersonic blades Ahmed Nemnem)
! if(mr1.gt.1.0) then
! frmx = 1.4
! frm = min(mr1, frmx)
! frat = (frm-1.0)/(frmx-1.0)
! flin = flin + 0.2*frat
! lethk = lethk - 0.015*frat
! tethk = tethk - 0.010*frat
! umxthk = umxthk + 0.4*frat    
! endif 

!*******************************************************************************************
! Curvature_Camber switches and options -------------------------------------------
!*******************************************************************************************
!Switch for Airfoil camber definition************

if(curv_camber.eq.0)then
	! -----------------------------------------------------------------------------
	! Case without curvature controlled camber ------------------------------------
	! -----------------------------------------------------------------------------
	print*, 'Using specified airfoil definition...'

	! Switch for using a particular type of airfoil shape--------------------------
	! -----------------------------------------------------------------------------
	! Circle shaped airfoil.
	! -----------------------------------------------------------------------------
	if(trim(airfoil).eq.'crcle')then
		call circle(xb, yb, np)
		call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)
		!stagger and scale
		do i = 1, np
			xi = xb(i)
			yi = yb(i)
			xb(i) = scaled(xi, chrdx)! using the non-dimensional actual chord from the input.
			yb(i) = scaled(yi, chrdx)
			xxa(i, js) = xb(i)
			yya(i, js) = yb(i)
		enddo
		call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrdx, fext, js, pitch, mble, mbte, airfoil)

	! -----------------------------------------------------------------------------
	! S809 airfoil wind turbine.
	! -----------------------------------------------------------------------------
	elseif(trim(airfoil).eq.'s809m')then
		stagger = stagger*dtor
		chrd = chrdx/abs(cos(stagger))
		call s809m(xb, yb, np)
		call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)
		!stagger and scale
		do i = 1, np   
			xi = xb(i)
			yi = yb(i)
			call rotate(xb(i), yb(i), xi, yi, stagger)
			xi = xb(i)
			yi = yb(i)
			xb(i) = scaled(xi, chrdx)! using the non-dimensional actual chord from the input.
			yb(i) = scaled(yi, chrdx)
			xxa(i, js) = xb(i)
			yya(i, js) = yb(i)
		enddo 
		call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrd, fext, js, pitch, mble, mbte, airfoil)

	! -----------------------------------------------------------------------------
	! Propeller airfoil ClarkY.
	! -----------------------------------------------------------------------------
	elseif(trim(airfoil).eq.'clark')then
		stagger = stagger*dtor
		chrd = chrdx/abs(cos(stagger))
		call clarky(xb, yb, np)
		call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)
		!stagger and scale
		do i = 1, np   
			xi = xb(i)
			yi = yb(i)
			call rotate(xb(i), yb(i), xi, yi, stagger)
			xi = xb(i)
			yi = yb(i)
			xb(i) = scaled(xi, chrdx)! using the non-dimensional actual chord from the input.
			yb(i) = scaled(yi, chrdx)
			xxa(i, js) = xb(i)
			yya(i, js) = yb(i)
		enddo 
		call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrd, fext, js, pitch, mble, mbte, airfoil)

	! -----------------------------------------------------------------------------
	! Counter Rotating Propeller airfoil ClarkY.
	! -----------------------------------------------------------------------------
	elseif(trim(airfoil).eq.'negclarkCR')then
		stagger = stagger*dtor
		chrd = chrdx/abs(cos(stagger))
		call negclarky(xb, yb, np)
		call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)
		!stagger and scale
		do i = 1, np   
			xi = xb(i)
			yi = yb(i)
			call rotate(xb(i), yb(i), xi, yi, stagger)
			xi = xb(i)
			yi = yb(i)
			xb(i) = scaled(xi, chrdx)! using the non-dimensional actual chord from the input.
			yb(i) = scaled(yi, chrdx)
			xxa(i, js) = xb(i)
			yya(i, js) = yb(i)
		enddo 
		call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrd, fext, js, pitch, mble, mbte, airfoil)  
	! ----------------------------------------------------------------------------- 
	! NACA 4 series airfoils with circular TE.
	! -----------------------------------------------------------------------------  
	elseif(len_trim(airfoil).eq.4)then
		write(*, *)
		stagger = stagger*dtor
		chrd = chrdx/abs(cos(stagger))
		call MakeFoil(naca, xbot, ybot, xtop, ytop, np, u)
		!Adding circular TE
		call circularTE(xbot, ybot, xtop, ytop, np)
		!---------------------------------------------------
		!---- fill in blade arrays
		do i = 1, np
			xb(i) = xtop(np-i+1)
			yb(i) = ytop(np-i+1)
		enddo
		do i = 2, np
			xb(i+np-1) = xbot(i)
			yb(i+np-1) = ybot(i)
		enddo
		!---- new # of points
		np = 2*np-1
		!u-v airfoil before stacking
		if(js.eq.1)then
			file1 = 'uvnaca.dat'
			call fileWrite1D(file1, xb, yb, np)
		endif  
		!--------------------------------------------------------
		call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)
		!stagger and scale
		do i = 1, np
			xi = xb(i)
			yi = yb(i)
			call rotate(xb(i), yb(i), xi, yi, stagger)
			xi = xb(i)
			yi = yb(i)
			if(chord_switch.eq.1)then
				xb(i) = scaled(xi, chrdx)! using the non-dimensional actual chord from the input.
				yb(i) = scaled(yi, chrdx)
			else
				xb(i) = scaled(xi, chrd)! using the value of chord calculated internally .
				yb(i) = scaled(yi, chrd)
			endif
			xxa(i, js) = xb(i)
			yya(i, js) = yb(i)
		enddo
		call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrd, fext, js, pitch, mble, mbte, airfoil)

	! -----------------------------------------------------------------------------
	! Default airfoil 'sect1' which is mixed camber. 
	! -----------------------------------------------------------------------------
	elseif(trim(airfoil).eq.'sect1')then
		print*, 'Using the default airfoil definition'
		write(*, *)
		!---- calc.stagger angle & set 0 stagger000000000000000000000000  farid
		! rotation to the axis with stagger angle to go to u-v plane:
		! First estimation of inlet metal angles:    for circular arc
		ainl = atan(sinl)
		aext = atan(sext)
		sang = 0.5*(ainl+aext)
		ainl = ainl-sang
		aext = aext-sang
		sinl = tan(ainl)
		sext = tan(aext)
		chrd = chrdx/abs(cos(sang))
		print*, 'sinl sext sang', sinl, sext, sang
		!print*, 'Using the predefined thickness...'
		do i = 1, np
			ui = u(i)    
			call cambmix(ui, cam, cam_u, sinl, sext, flin, flex)
			camber(i) = cam
			slope(i) = cam_u
		enddo

	! -----------------------------------------------------------------------------
	! Reading airfoil data from user defined file. Look at the Manual for format. 
	! -----------------------------------------------------------------------------
	else
		stagger = stagger*dtor
		chrd = chrdx/abs(cos(stagger))
		call datafile(airfoil, xb, yb, np)
		if(js.eq.1)then
			file1 = 'uvairfoil.dat'
			call fileWrite1D(file1, xb, yb, np)
		endif
		call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)
		! stagger and scale 
		do i = 1, np
			xi = xb(i)
			yi = yb(i)
			call rotate(xb(i), yb(i), xi, yi, stagger)
			xi = xb(i)
			yi = yb(i)
			xb(i) = scaled(xi, chrdx)! using the non-dimensional actual chord from the input.
			yb(i) = scaled(yi, chrdx)
			xxa(i, js) = xb(i)
			yya(i, js) = yb(i)
		enddo
		call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrd, fext, js, pitch, mble, mbte, airfoil)
	endif ! end of airfoil types for no camber curvature control 
! -----------------------------------------------------------------------------

elseif (curv_camber.eq.1) then ! using curvature control for camber instead of meanline or airfoil coordinates definition.
	! -----------------------------------------------------------------------------
	! Airfoil camber defined by curvature control through spline using control points-
	!--Karthik Balasubramanian--
	!-------- Ahmed Nemnem------
	write (*, '(/, A)') 'Using curvature control for camber definition...'
	ainl = atan(sinl)
	aext = atan(sext)
	! Reading the section control points:
	ncp = ncp_curv(js)
if (allocated(xcp_curv)) deallocate(xcp_curv)
if (allocated(ycp_curv)) deallocate(ycp_curv)
	Allocate(xcp_curv(ncp))
	Allocate(ycp_curv(ncp))
	do i = 1, ncp
		xcp_curv(i) = curv_cp(i, 2*js-1)
		ycp_curv(i) = curv_cp(i, 2*js)
	enddo
if (allocated(init_angles   )) deallocate(init_angles   )
if (allocated(init_cambers  )) deallocate(init_cambers  )
if (allocated(x_spl_end_curv)) deallocate(x_spl_end_curv)
	Allocate(init_angles(ncp-2))
	Allocate(init_cambers(ncp-2))
	Allocate(x_spl_end_curv(ncp-2))
	! get camber line from the spline curvature:
		call camline(casename, isdev, xcp_curv, ycp_curv, ncp, u, np, ainl, aext, chrdx, wing_flag, &
		sang, chrd, init_angles, init_cambers, x_spl_end_curv, splinedata)
		camber = splinedata(2, :)
		slope = splinedata(3, :)
endif ! end of camber curvature switch   
! -----------------------------------------------------------------------------

!******************************************************************************************
! Thickness definition options and LE definition options
!****************************************************************************************** 
if(trim(airfoil).eq.'sect1')then ! thickness is to be defined only for default sections       
	if(thick.eq.1)then 
		! -----------------------------------------------------------------------------
		!---- generate a spline multiplier thickness: =============================== 
		! -----------------------------------------------------------------------------  
		ncp = ncp_thk(js)
                if (allocated(xcp_thk)) deallocate(xcp_thk)
                if (allocated(ycp_thk)) deallocate(ycp_thk)
		Allocate(xcp_thk(ncp)) 
		Allocate(ycp_thk(ncp)) 
		do i = 1, ncp
			xcp_thk(i) = thk_cp(i, 2*js-1)
			ycp_thk(i) = thk_cp(i, 2*js)
		enddo
		!call splinethickmult(u, splthick, xcp_thk, ycp_thk, ncp)
		!print*, 'ncp = ', ncp
		!print*, 'xcp_thk(i), ycp_thk(i)'
		! do i = 1, ncp
		! print*, xcp_thk(i), ycp_thk(i)
		! enddo
		! fourth order thickness multiplier: Modified Nemnem 9 26 2013
		call bspline_y_of_x( splthick, u, np, xcp_thk, ycp_thk, ncp, 4 )
		!  file1 = 'thick_multi_spline.'//trim(adjustl(sec))//'.'//trim(casename)//'.txt'
		!  open(unit = 80, file = file1, form = "formatted")
		!  write(80, *), 'u', "	", 'thickness_multiplier'
		do i = 1, np
			!   write(80, *), u(i), "	", splthick(i)
			splinedata(6, i) = splthick(i)
		enddo
		!close(80)

		! -----------------------------------------------------------------------------
		! Writing a file in developer mode for debugging.
		! -----------------------------------------------------------------------------
		if(isdev)then
			file2 = 'thick_Multi_cp.'//trim(adjustl(sec))//'.'//trim(casename)//'.txt'
			open(unit = 81, file = file2, form = "formatted")
			write(81, *) 'xcp_thk', "	", 'ycp_thk'
			do i = 1, ncp
				write(81, *) xcp_thk(i), "	", ycp_thk(i)
			enddo
			close(81)	 
		endif ! end if for writing a file in developer mode  

	else ! default thickness definition.
		splthick = 0
	endif ! end if for thickness definition options
	!print*, 'splthick', splthick

	! -----------------------------------------------------------------------------
	!---- generate airfoil thickness: =============================== 
	! -----------------------------------------------------------------------------
	if (thick_distr.eq.4) then
		! Added by Karthik Balasubramanian
		write (*, '(/, A)') 'Implementing exact thickness control'
		ncp = ncp_thk(js)
		if (allocated(xcp_thk)) deallocate(xcp_thk)
		if (allocated(ycp_thk)) deallocate(ycp_thk)
		Allocate(xcp_thk(ncp)) 
		Allocate(ycp_thk(ncp)) 
		do i = 1, ncp
			xcp_thk(i) = thk_cp(i, 2*js-1)
			ycp_thk(i) = thk_cp(i, 2*js)
		enddo
		print*, 'Exact thickness points:'
		write(*, '(2F10.5)') (xcp_thk(i), ycp_thk(i), i = 1, ncp)
		print*, 'LE Angle', le_angle_all(js)		
		print*, 'TE Angle', te_angle_all(js)
		! thk_ctrl_gen_driver (uthk, thk, n, u_spl, np, te_angle_all, te_flag, out_coord)
		print*, 'TE flag', te_flag
		print*, 'LE optimization flag', le_opt_flag		
		print*, 'TE optimization flag', te_opt_flag
		call thk_ctrl_gen_driver(casename, isdev, sec, xcp_thk, ycp_thk, ncp, u, np, le_angle_all(js), &
                                 te_angle_all(js), te_flag, le_opt_flag, te_opt_flag, thickness_data)
		thickness = thickness_data(:, 2)
		! if(isdev) then
		open (unit = 81, file = 'thk_CP.' // trim(adjustl(sec)) // '.' // trim(casename) // '.dat')
		write (81, '(2F20.16)') (xcp_thk(i), ycp_thk(i), i = 1, ncp)
		close (81)
		open (unit = 81, file = 'thk_dist.' // trim(adjustl(sec)) // '.' // trim(casename) // '.dat')
		! write (81, '(2F20.16)') (u(i), thickness(i), i = 1, np)
		write (81, '(6F40.16)') (thickness_data(i, 1), thickness_data(i, 2), thickness_data(i, 3),     &
                                 thickness_data(i, 4), thickness_data(i, 5), thickness_data(i, 6), i = 1, np)
		close (81)
		! endif
	elseif (thick_distr.eq.3) then
		! Added by Karthik Balasubramanian
		write (*, '(/, A)') 'Implementing direct thickness control'
		ncp = ncp_thk(js)
                if (allocated(xcp_thk)) deallocate(xcp_thk)
                if (allocated(ycp_thk)) deallocate(ycp_thk)
		Allocate(xcp_thk(ncp)) 
		Allocate(ycp_thk(ncp)) 
		do i = 1, ncp
			xcp_thk(i) = thk_cp(i, 2*js-1)
			ycp_thk(i) = thk_cp(i, 2*js)
		enddo
		write (*, '(A)') 'User input thickness control points including internally generated dummy points : '
		write (*, '(2F20.16)') (xcp_thk(i), ycp_thk(i), i = 1, ncp)
		call splinethickcontrol(umxthk, thkc, ncp, xcp_thk, ycp_thk, np, u, thickness, thick_distr_3_flag)
		! if(isdev) then
			open (unit = 81, file = 'thk_CP.' // trim(adjustl(sec)) // '.' // trim(casename) // '.dat')
			write (81, '(2F20.16)') (xcp_thk(i), ycp_thk(i), i = 1, ncp)
			close (81)
			open (unit = 81, file = 'thk_dist.' // trim(adjustl(sec)) // '.' // trim(casename) // '.dat')
			write (81, '(2F20.16)') (u(i), thickness(i), i = 1, np)
			close (81)
		! endif
	elseif(thick_distr.ne.0) then
		! -----------------------------------------------------------------------------
		!Spline thickness distr. with LE control
		! -----------------------------------------------------------------------------
		print*, 'np = ', np
		print*, ' the thickness segment points'
		call splinethick(thickness, u, np, lethk, umxthk, fmxthk, tethk, i_le, i_te, uin_le, thick_distr, &
		ucp_top, vcp_top, ucp_bot, vcp_bot, casename, js, develop, isdev, np_side, spline_data, splinedata)
		thickness = thickness*(1 + splthick)

	elseif (thick_distr == 0)then
		do i = 1, np
			ui = u(i)
			thkmultip = splthick(i)
			call thickellip(i, ui, thk, lethk, tethk, fmxthk, umxthk, rr1, rr2, thkmultip, u_le, uin_le, i_le, oo, i_te)
			thickness(i) = thk
		enddo
	endif ! end if for thickness distribution options

	print*, 'i_le = ', i_le
	print*, 'i_te = ', i_te
	! -----------------------------------------------------------------------------
	! Creating the top and bottom curve coordinates. ---------
	! -----------------------------------------------------------------------------
	angle = atan(slope)
	!print*, "camber", camber
	! generating the blade on vector bases:
	xbot = u   + thickness*sin(angle)
	ybot = camber - thickness*cos(angle)
	xtop = u   - thickness*sin(angle)
	ytop = camber + thickness*cos(angle)

	!
	!deltau = abs(u(np) - u(1))*abs(cos(angle))
	!
	! -----------------------------------------------------------------------------
	! Writing a file in developer mode for debugging.
	! -----------------------------------------------------------------------------
	if(isdev)then
		file7 = 'topcurve.'//trim(fext)
		call fileWrite1D(file7, xtop, ytop, np)
		!
		file7 = 'botcurve.'//trim(fext)
		call fileWrite1D(file7, xbot, ybot, np)

	endif
	! -----------------------------------------------------------------------------

	! -----------------------------------------------------------------------------
	! LE spline Definition.
	! -----------------------------------------------------------------------------
	!00000000000000000000000000000000000000000000000000000 Nemnem 4/4/13
	le_pos = 1000 ! to not make confiction with the code
	if(LE.ne.0)then
		!---- generate blade curves for LE spline
		! Identifying the leading edge parameters (curv, slope, coordinates):
		le_pos = i_le
		!camber_ang = atan(camber(le_pos:le_pos+3)/u(le_pos:le_pos+3))
		!camber_le = camber(le_pos:le_pos+3)
		!do i = 1, 4
		!print*, 'u = ', u(le_pos+i-1), 'thickness = ', thickness(le_pos+i-1)
		!print*, 'camber_ang = ', camber_ang(i), 'camber_le = ', camber_le(i)
		!enddo
		print*, 'uin_le bladgen = ', uin_le
		print*, 'le_pos bladgen = ', le_pos
		!print*, 'le_camber_ang in radian = ', camber_ang(1), 'in Degree = ', 1/dtor*camber_ang(1)
		!print*, 'cam_le = ', camber_le(1)
		!00000000000000000000000000000000000000000000000000000000000000000
		if (allocated(x_le_spl   )) deallocate(x_le_spl   )
		if (allocated(y_le_spl   )) deallocate(y_le_spl   )
		if (allocated(xtop_refine)) deallocate(xtop_refine)
		if (allocated(ytop_refine)) deallocate(ytop_refine)
		if (allocated(xbot_refine)) deallocate(xbot_refine)
		if (allocated(ybot_refine)) deallocate(ybot_refine)
		if (allocated(cam_refine )) deallocate(cam_refine )
		if (allocated(u_refine   )) deallocate(u_refine   )
		Allocate(x_le_spl(2*le_pos-1))
		Allocate(y_le_spl(2*le_pos-1))  
		!---- For spline leading edge:
		Allocate(xtop_refine(interval+1))
		Allocate(ytop_refine(interval+1))
		Allocate(xbot_refine(interval+1))
		Allocate(ybot_refine(interval+1))
		Allocate(cam_refine(interval+1))
		Allocate(u_refine(interval+1))
		print*, '!! KB reached fini_diff_refine in bladegen'
		call fini_diff_refine(curv_camber, thick, thick_distr, &
							xcp_curv, ycp_curv, ncp_curv(js), xcp_thk, ycp_thk, ncp_thk(js), &
							u(le_pos), interval, ucp_top, vcp_top, ucp_bot, vcp_bot, &
							sinl, sext, flin, flex, fmxthk, umxthk, lethk, tethk, rr1, rr2, &
							x_spl_end_curv, init_angles, init_cambers, cam_refine, u_refine, &
							xtop_refine, ytop_refine, xbot_refine, ybot_refine)
		do k = 1, interval+1
			camber_ang(k) = atan(cam_refine(1)/u_refine(1))
		enddo
		camber_le = cam_refine
		!refined values:
		file3 = 'compare.'//trim(casename)//'.txt'
		open(unit = 90, file = file3, form = "formatted")
		write(90, *) 'xtop_refine', xtop_refine
		write(90, *) 'ytop_refine', ytop_refine
		write(90, *) 'xtop(le_pos)', xtop(le_pos)
		write(90, *) 'ytop(le_pos)', ytop(le_pos)
		write(90, *) 'xbot_refine', xbot_refine
		write(90, *) 'ybot_refine', ybot_refine
		write(90, *) 'xbot(le_pos)', xbot(le_pos)
		write(90, *) 'ybot(le_pos)', ybot(le_pos)
		! execute attachment of the LE spline :
		!call lespline ( xtop(le_pos+3:le_pos:-1), ytop(le_pos+3:le_pos:-1), &
		!        xbot(le_pos:le_pos+3), ybot(le_pos:le_pos+3), &    Nemnem 9/3/2013 I commented this to use the refined values

		! -----------------------------------------------------------------------------
		! Le spline definition
		! -----------------------------------------------------------------------------
		if(LE.eq.1) then
			ncp = LEdegree+no_LE_segments
			call lespline (xtop_refine,ytop_refine,xbot_refine,ybot_refine,interval+1,camber_ang,camber_le,uin_le,&
						 le_pos,pi,x_le_spl,y_le_spl,js,nsl,s_all,ee_all,C_le_x_top_all,C_le_x_bot_all,C_le_y_top_all,C_le_y_bot_all,&
						 LE_vertex_ang_all,LE_vertex_dis_all,ncp,LEdegree,no_LE_segments,casename,develop,isdev)							

		elseif(LE.eq.2) then
			! -----------------------------------------------------------------------------
			! LE sting definition
			! -----------------------------------------------------------------------------
			ncp = LEdegree+2 ! for the top and bottom let there are 2 spline segments for each.
			print*, 'ncp_sting', ncp
			call lesting (xtop_refine,ytop_refine,xbot_refine,ybot_refine,interval+1,&
						camber_ang,camber_le,uin_le,le_pos,pi,x_le_spl,y_le_spl,js,nsl, &
						s_all,ee_all,C_le_x_top_all,C_le_x_bot_all,C_le_y_top_all,C_le_y_bot_all,sang,&
						LE_vertex_ang_all,LE_vertex_dis_all,ncp,LEdegree,casename, &
						develop,sting_l_all,sting_h_all)						
		endif ! endif for LE spline options

		!---- fill in blade arrays ----------------
		do i = 1, np
			! 000000000000000000000000000000000nemnem
			if(i >= (np-le_pos+1)) then
				xb(i) = x_le_spl(i-(np-le_pos))
				yb(i) = y_le_spl(i-(np-le_pos))
			else  
				xb(i) = xtop(np-i+1)
				yb(i) = ytop(np-i+1)
			endif
			! 00000000000000000000000000000000000  
			!print*, 'i = ', i, 'xb = ', xb(i), 'yb = ', yb(i)
		enddo
		do i = 2, np
			! 000000000000000000000000000000000nemnem
			if(i <= le_pos) then
				xb(i+np-1) = x_le_spl(i+le_pos-1)
				yb(i+np-1) = y_le_spl(i+le_pos-1)
			else
				xb(i+np-1) = xbot(i)
				yb(i+np-1) = ybot(i)
			endif
			! 00000000000000000000000000000000000  
			!print*, 'i = ', i, 'xb = ', xb(i), 'yb = ', yb(i)  
		enddo     
		!---- new # of points
		np = 2*np-1

	else !if(LE.eq.0)then
		! -----------------------------------------------------------------------------
		! Elliptical LE options (default)
		! -----------------------------------------------------------------------------
		!---- fill in blade arrays ------------------
		print*, np
		do i = 1, np
			xb(i) = xtop(np-i+1)
			yb(i) = ytop(np-i+1)
		enddo
		do i = 2, np
			xb(i+np-1) = xbot(i)
			yb(i+np-1) = ybot(i)
		enddo
		!---- new # of points
		np = 2*np-1
	endif ! end if for LE spline definition

	! Stacking of airfoils ----------------------------------------------
	!print*, xtop((np+1)/2), ytop((np+1)/2)!, xb(np), yb(np), "---------------" 
	call stacking(xb, yb, xbot, ybot, xtop, ytop, js, np, stack_switch, stack, stk_u, stk_v, area, LE)
	!stagger and scale

	if(isdev) then
		file7 = 'uvblade.'//trim(fext)
		call fileWrite1D(file7, xb, yb, np)
		! open(unit = 22, file = file7, status = 'unknown')
		! write(22, *)'skip'
		! write(22, *)'skip'
		! do i = 1, np
		! write(22, *)xb(i), yb(i)
		! enddo
		! close(22)
	endif

	print*, 'chrd bladegen: ', chrd
	do i = 1, np
		!print*, xb(i), yb(i)
		xi = xb(i)
		yi = yb(i)
		call rotate(xb(i), yb(i), xi, yi, sang)
		xi = xb(i)
		yi = yb(i)
		if(chord_switch.eq.1)then
			xb(i) = scaled(xi, chrdx)! using the non-dimensional actual chord from the input.
			yb(i) = scaled(yi, chrdx)
		else
			xb(i) = scaled(xi, chrd)! using the value of chord calculated internally .
			yb(i) = scaled(yi, chrd)
		endif
		yb(i) = yb(i) + theta_offset*dtor
		xxa(i, js) = xb(i)
		yya(i, js) = yb(i)
	enddo
	call bladesection(xb, yb, np, nbls, TE_del, sinls, sexts, chrd, fext, js, pitch, mble, mbte, airfoil)
endif  ! for default airfoil section only

!******************************************************************************************
! Store bladedata for each section: Nemnem
!******************************************************************************************
! picking the data for blade data file:
bladedata(4, js) = ainl/dtor 	; bladedata(5, js) = aext/dtor
! scaling the bladedata with chord (m', theta) space
! Area is scaled with dimentional chrd^2 (2D):
bladedata(9, js) = area*(chrd*scf)**2	; bladedata(10, js) = lethk*chrd*scf 	; bladedata(11, js) = tethk*chrd*scf
bladedata(13, js) = pitch				! nondimesional r_deltatheta 
radius_pitch = pitch*sec_radius(js, 1) ! at the LE tip non-dimensional
print*, 'pitch ', pitch 

!******************************************************************************************
! Calculation of the 2D throat:	--- new approach in 9 19 2013 Nemnem ---
!******************************************************************************************
!------------------------------------------------------------------------	
! rotate, scale and translate the camber line
!------------------------------------------------------------------------
if(trim(airfoil) == 'sect1') then
	u_translation = u(1)
	camber_trans = camber(1)

	if(chord_switch.eq.1)then
	scaling = chrdx
	else
	scaling = chrd
	endif

	do i = 1, (np+1)/2
	call rotate2 (u_rot, camber_rot, u(i), camber(i), sang)
	u(i) = scaled (u_rot, scaling)
	camber(i) = scaled (camber_rot, scaling)
	u(i) = u(i) + (xb((np+1)/2)-u_translation)
	camber(i) = camber(i) + (yb((np+1)/2)-camber_trans)
	enddo
else
	! averaging top and bottom curves to obtain the camber
	call averaged_camber(xb, yb, np, u, camber, angle, sinl)
endif

! note: in intersection coordinate array, the throat, mouth and exit lines are added respectively.
call throat_calc_pitch_line(xb, yb, np, camber, angle, sang, u, pi, pitch, intersec_coord(1:4, js), &
                            intersec_coord(5:8, js), intersec_coord(9:12, js), min_throat_2D,       &
                            throat_index(js), n_normal_distance, casename, js, nsl, develop, isdev)

!******************************************************************************************            
! Writing the section properties into a single file
!******************************************************************************************
if(curv_camber.ne.0) then ! only for curvature control airfoils. 11 20 2013
	file7 = 'splinedata_section.'//trim(adjustl(sec))//'.'//trim(casename)//'.dat'
	print*, file7
	open(unit = 33, file = file7, form = "formatted")  
	write(33, *) trim(casename), ' :', ' Spline Data'
	write(33, *) 'section : ', trim(adjustl(sec))
	write(33, *)
	write(33, *) ' #     u         camber       camber_slope    camber_curvature  thick_distribution  thick_multiplier'  
	do j = 1, np_side
		write(33, 301)j, splinedata(1:(spline_data), j)
	enddo 
	write(33, *)
	write(33, *) 'Control points for the thickness distribution spline (symmetric bottom curve):'
	write(33, *) 'ucp_top    vcp_top'
	do i = 1, 11
		write(33, *) ucp_top(i), vcp_top(i)   
	enddo
	close(33)
	!---------------------------------------------------------  
        if (allocated(curvature)) deallocate(curvature)
	Allocate(curvature(np_side))
	do i = 1, np_side
		curvature(i) = splinedata(4, i)
	enddo
	stingl = sting_l_all(js)
	print*, 'stingl: ', stingl
endif

!******************************************************************************************
! 2D blade grid generation.
!******************************************************************************************
if (isxygrid) then
	print*, 'i_slope in bladgen: ', i_slope
	if((nbls.ge.5).and.(i_slope.eq.0))then ! No 2D grid files for wind turbines and cases which have less than 5 blades.
		jcellblade = jcellblade_all(js)
		etawidth = etawidth_all(js)
		BGgrid = BGgrid_all(js)

		call bladegrid2D(xb, yb, np, nbls, chrdx, thkc, fext, LE, le_pos, thick_distr, &
						casename, msle, mste, mble, mbte, js, nspn, np_side, &
						curv_camber, stingl, jcellblade, etawidth, BGgrid, develop, isdev)
	endif
endif

!******************************************************************************************
! Calculation of Geometric Zwiefel Number:
!******************************************************************************************
!------------ assuming inlet and exit velocities are equal ... 
!...from Elements of gas turbine propulsion equation (9.98 b)	 
Zwiefel(js) = 2*pitch/chrdx*((cos(aext))**2)*(tan(ainl)+tan(aext))
bladedata(14, js) = Zwiefel(js)
!------------------------

!******************************************************************************************
! Deallocation of variables
!******************************************************************************************
if(curv_camber.ne.0)then	 
	deallocate(curvature)
endif

!******************************************************************************************
! Format Statements
!******************************************************************************************
301 format(i3, 2x, 6(f19.16, 2x))
print*, '******************************************'
return
end subroutine bladegen
