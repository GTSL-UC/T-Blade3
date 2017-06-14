!****************************************************************************************
!****************************************************************************************
!****************************************************************************************
!  3 DIMENSIONAL BLADE GEOMETRY BUILDER (3DBGB)
!****************************************************************************************
!  This is a program which generates a 3d blade shape and outputs section files.
!  inputs: inlet angle, exit angle, chord, thickness, scaling factor among others.
!
!  output: 3d blade shape, 3d blade coordinates (x, y, z), section files
!---------------------------------------------------- by Kiran Siddappaji, Ahmed Nemnem
!----------------------------------------------------University of Cincinnati
!****************************************************************************************
!****************************************************************************************
!****************************************************************************************
! CODE STRUCTURE : ORDER OF OPERATION
!****************************************************************************************
! 00. Start Program.

! 01. Declare variables and display Welcome message and info about the code and Authors.

! 02. Reading the primary input file (3dbgbinput.#.dat).
    ! b. Reading from the input 'controlinput.dat' if curvature/thick/LE switches are on.
        !i  ) Reading curvature.
        !ii ) Reading thickness.
        !iii) Reading Leading edge parameters.

! 03. Print selected read data to the screen.
    !a. Splining the LE and TE coordinates from the input file.

! 04. Calculating m prime coordinates using x, r coordinates as the input for streamlines.
    !a. Writing dimensional hub and casing streamlines.

! 05. Splining the x, r coordinates of the stream/construction lines to create m' splines.
    !a. Calculating offsets for the hub-tip streamlines if the offset is given as input.

! 06. Obtaining LE/TE x, r values on each streamline using the LE/TE curve and...
      !...the streamline curve intersection.

! 07. Checking if r_slope > x_slope for non-axial flow.

! 08. Calculating msLE and msTE using LE, TE (x, r) and determining axial/radial/mixed flow.
    !a. Obtaining initial guesss values for msle and mste to calculate the correct values.
    !b. Determining purely axial, purely radial, mixed flow.
    !c. Calculating the phi to adjust for BetaZ conversion.

! 09. Passing input parameters for blade generation (2D airfoils).
    !a. Splining Control points for chord, stagger, outbeta, tm/c.

! 10. Allocating more variables for further calculations regarding throat and other data.

! 11. Calling the subroutine bladegen which performs the blade creation for various airfoils.
    !a. BetaZ-betaM conversion.
    !b. Chord switches.
    !c. Stagger switches.
    !d. calling bladegen
      ! i)Also creating 2D blade grids with background.

! 12. Stacking blade sections to create a 3d blade shape with various stacking options.

! 13. Writing relevant bladedata to an external file (volume calculation).

! 14. Writing throat-index to the screen and external file.

! 15. Writing dimensional 3D blade coordinates to separate files.

! 16. Writing pitch and chord non-dimensional values to a file.

! 17. Format statements.

! 18. Deallocation of the allocatable variables.

! 19. End Program
!****************************************************************************************

!****************************************************************************************
program bgb3d ! 3d blade geometry builder
use globvar
implicit none
!
real spl_eval, dspl_eval
real*8 xdiff, rdiff
real*8 inBetaInci, outBetaDevn
! !
character*256 :: fname, temp, tempr1, fname1, fname2, fname3, fname4, row_type
character*32 :: arg2, arg3
! !
logical axial_LE, radial_LE, axial_TE, radial_TE
axial_LE = .False.
radial_LE = .False.
axial_TE = .False.
radial_TE = .False.
is2d = .False.
wing_flag = 0
!-------constants-----------------------------------------------------------
pi = 4.*atan(1.0)
dtor = pi/180.
radius_tolerance = 1e-05
abs_zero = 0.0000000000000000

!****************************************************************************
!Displays the welcome message and some info about the code capabilities
call displayMessage
!****************************************************************************

!--------counting the number of command line arguments passed: --------------
narg = iargc()
call getarg(1, fname)
if(narg.ge.2)then
	call getarg(2, arg2)
endif
if(narg.eq.3)then
	call getarg(3, arg3)
endif

! Types of 2nd argument
if (trim(arg2).eq.'dev') then
	print*, '2nd Argument:', 'develop'
	isdev = .true.
elseif (trim(arg2).eq.'xygrid') then ! Only for the 2d grids in xy
	print*, '2nd Argument:', 'xygrid'
	isxygrid = .true.
elseif (trim(arg2).eq.'xyzstreamlines') then ! only when you want the xyz streamlines.sldcrv
	print*, '2nd Argument:', 'xyzstreamlines'
	is_xyzstreamlines = .true.
elseif ((trim(arg2).eq.'2d') .or. (trim(arg2).eq.'2D')) then
	print*, '2nd Argument:', '2D'
	is2d = .True.
endif

! Types of 3rd argument
if (trim(arg3).eq.'dev') then
	print*, '3rd Argument:', 'develop'
	isdev = .true.
elseif (trim(arg3).eq.'xygrid') then ! Only for the 2d grids in xy
	print*, '3rd Argument:', 'xygrid'
	isxygrid = .true.
elseif (trim(arg3).eq.'xyzstreamlines') then ! only when you want the xyz streamlines.sldcrv
	print*, '3rd Argument:', 'xyzstreamlines'
	is_xyzstreamlines = .true.
elseif ((trim(arg3).eq.'2d') .or. (trim(arg3).eq.'2D')) then
	print*, '3rd Argument:', '2D'
	is2d = .True.
endif

! indicating the row number and type of blade from input file name:
!-----------------------------------------------------------------
k = (index(fname, '.')+1)
do i = k, len(trim(fname))
	if (fname(i:i) == '.') then
		j = i-1
		continue
	endif
enddo
row_type = fname(k:j)
print*, 'Row number and blade type is ', row_type

!****************************************************************************
!---reading the primary input file---------------------------------------
call readinput(fname)
write(*,*)
write(*,*) 'Reading inputs from file : ', fname

if((curv.ne.0.or.thick.ne.0.or.LE.ne.0&
.or.thick_distr.ne.0).and.trim(spanwise_spline).ne.'spanwise_spline')then
	!---reading secondary input file (controlinputdat)-----------------------
	write(*, *)
	print*, 'Reading the controlinput file ....'
	write(*, *)
	call readcontrolinput(row_type)
elseif((curv.ne.0.or.thick.ne.0.or.LE.ne.0&
.or.thick_distr.ne.0).and.trim(spanwise_spline).eq.'spanwise_spline')then
	!---reading secondary input file (spancontrolinputdat)-----------------------
	write(*, *)
	print*, 'Reading the spanwise_input file ....'
	write(*, *)
	call read_spanwise_input(row_type)
endif

!****************************************************************************

!****************************************************************************
!--Messages to the screen----------------------------------------------------

write(*, *)
write(*, *)'case:', fext
write(*, *)'bladerow #:', ibrow
print*, ibrowc
write(*, *)
print*, 'Number of blades in this row:', nbls
write(*, *)'bsf:', scf
write(*, *)
print*, 'Number of streamlines:', nsl
write(*, *)
if (.not. spanwise_angle_spline) then
	print*, '   in_betaZ*    out_betaZ*'
	do js = 1, nspn
		print*, in_beta(js), out_beta(js)
	enddo
endif

!! splining the LE and TE coordinates from the input file
write(*, *)
write(*, *)'LE/TE defined by a curve with no. of points as:', npoints
write(*, *)'xLE    rLE     xTE     rTE'
do i = 1, npoints
	print*, xle(i), rle(i), xte(i), rte(i)
	!Spline LE curve
	call arclength(xle(1), rle(1), sle(1), npoints)
	call spline(xle(1), xles(1), sle(1), npoints, 999.0, -999.0)
	call spline(rle(1), rles(1), sle(1), npoints, 999.0, -999.0)
	!Spline TE curve
	call arclength(xte(1), rte(1), ste(1), npoints)
	call spline(xte(1), xtes(1), ste(1), npoints, 999.0, -999.0)
	call spline(rte(1), rtes(1), ste(1), npoints, 999.0, -999.0)
enddo
!----------------------------------------

if(LE.ne.0) then ! LE spline options
	do js = 1, nspn
		print*, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
	enddo
elseif(LE == 0) then ! elliptical LE
	do js = 1, nspn
		print*, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), lethk_all(js), tethk_all(js), jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
	enddo
endif

!****************************************************************************
!SWEEP and LEAN
! Differentiating between true and axial SWEEP
if(trim(trueleansweep).ne.'')then
	print*, 'Sweep along the chord (1 = yes): ', chrdsweep
	write(*, *)
else
	print*, 'Sweep in the axial direction (m-prime).'
	write(*, *)
endif

! Differentiating between true and axial LEAN
if(trim(trueleansweep).ne.'')then
	print*, 'Lean normal to the chord (1 = yes): ', chrdlean
	write(*, *)
else
	print*, 'Lean in the tangential direction (theta).'
	write(*, *)
endif

!--------------------------------------------------------------------------------
! ! Writing dimensional hub and casing streamlines 
!(adding hub here before eliminating zero radius points nemnem 6/25/2014)
do i = 1, nsp(nsl)
	xt(i, 1) = xm(i, nsl)
	rt(i, 1) = rm(i, nsl)
enddo
call hubTipStreamline(xm(1, 1), rm(1, 1), nsp(1), xt, rt, nsp(nsl), nsl, scf, casename)


!****************************************************************************
! Calculating m prime coordinates...
! using x, r coordinates as the input for streamlines
!****************************************************************************
write(*, *)
print*, 'Using x_s and r_s coordinates for streamline from the input file...'
write(*, *)
print*, 'Calculating the m_prime coordinates for each streamline...'
write(*, *)
!

! if (rm(1, 1) .eq. abs_zero .and.  rm(1, nsp(1)) .eq. abs_zero) then
    ! rm(1, 1) = 0.0000000000000001
    ! rm(1, nsp(1)) = 0.0000000000000001
! endif

!
do ia = 1, nsl
	mp(1, ia) = 0.
	k = 0
	! !print*, ' mp         xm         rm'
	do i = 2, nsp(ia)
		if (rm(i, ia)< radius_tolerance) then
			k = k + 1	! nemnem 6 10 2014
			print*, 'Radius less than', radius_tolerance, ', excluding point number', k
			xm(1, ia) = xm(i, ia)  ! switch to new intial value after elimination zero radius points
			rm(1, ia) = rm(i, ia)
			print*, 'xm(1, ia)', xm(1, ia), 'rm(1, ia)', rm(1, ia)
		else
			!print*, 'i-k, nsp(ia)', i-k, nsp(ia)
			mp(i-k, ia) = mp(i-k-1, ia) + 2.*sqrt((rm(i, ia)-rm(i-1, ia))**2 + (xm(i, ia)-xm(i-1, ia))**2)/(rm(i, ia)+rm(i-1, ia))
			xm(i-k, ia) = xm(i, ia)   ! Reindex the mp, xm, rm for hub
			rm(i-k, ia) = rm(i, ia)	! Reindex the mp, xm, rm for hub
		endif
	enddo
	if (k /= 0) then
		nsp_hub = nsp(ia)		! nsp_hub is used for full hub streamline extraction nemnem 6 10 2014
		nsp(ia) = i-k-1         ! Update the hub number of valid points
		print*, 'nsp for streamline', ia, 'changed from', nsp_hub, 'to', nsp(ia)
	endif
	if (is_xyzstreamlines) then
		if ((ia > 1).and.(ia < nsl)) then
			call streamlines(xm(:, ia), rm(:, ia), nsp(ia), scf, casename, ia) ! Ahmed nemnem 4 23 2014 extracting streamlines
		endif
	endif
	!print*, 'mp(:, ia)', mp(1:nsp(ia), ia)
enddo
!-------------------------------------------------------------------------------
!*************************************************************************************
! Debugging :Create an interpolated xm rm for nonoffset hub streamline for plotting : Nemnem 7 16 2014
if (hub.ne.0) then
	n_inter_intervals = 3  ! number of points = no_intervals + 1
	k = 1
	xm_nonoffset_hub(k) = xm(1, 1)
	rm_nonoffset_hub(k) = rm(1, 1)
	Allocate(hub_slope(nsp(1)-1))
	! interpolate hub streamline xm rm:
		do i = 1, nsp(1)-1 !number of hub segments
			hub_slope(i) = (rm(i+1, 1)-rm(i, 1))/(xm(i+1, 1)-xm(i, 1))
			!print*, 'slope', hub_slope(i), k
			do j = 1, n_inter_intervals
				k = j + (i-1)*n_inter_intervals
				xm_nonoffset_hub(k+1) = xm_nonoffset_hub(k)+((xm(i+1, 1)-xm(i, 1))/(n_inter_intervals))
				rm_nonoffset_hub(k+1) = rm_nonoffset_hub(k)+(xm_nonoffset_hub(k+1)-xm_nonoffset_hub(k))*hub_slope(i)
				nsp_interpolated_hub = k+1 ! this should k+1 but I put k to remove the first point
			enddo
		enddo
		
	! calculate mp_nonoffset_hub:
		mp_nonoffset_hub(1) = 0.0
		do i = 2, nsp_interpolated_hub
			mp_nonoffset_hub(i) = mp_nonoffset_hub(i-1) + 2.*sqrt((rm_nonoffset_hub(i)-rm_nonoffset_hub(i-1))**2 &
						+ (xm_nonoffset_hub(i)-xm_nonoffset_hub(i-1))**2)/(rm_nonoffset_hub(i)+rm_nonoffset_hub(i-1))
			!mp(i-k, ia) = mp(i-k-1, ia) + 2.*sqrt((rm(i, ia)-rm(i-1, ia))**2 + (xm(i, ia)-xm(i-1, ia))**2)/(rm(i, ia)+rm(i-1, ia))
		enddo
		
	! write to file:
	! fname1 = 'plot_xm_mp_nonoffset_hub.'//trim(casename)//'.dat'
	! open(12, file = fname1, status = 'unknown')
		! do i = 1, nsp_interpolated_hub
			! write(12, *) xm_nonoffset_hub(i), mp_nonoffset_hub(i), rm_nonoffset_hub(i)
		! enddo
	! close(12)
	
	! splining:
		call spline(xm_nonoffset_hub, xms_nonoffset_hub, mp_nonoffset_hub, nsp_interpolated_hub, 999.0, -999.0)
		call spline(rm_nonoffset_hub, rms_nonoffset_hub, mp_nonoffset_hub, nsp_interpolated_hub, 999.0, -999.0)
		
	! Offseting the interpolated hub spline:
		temp = 'interpolated'
		!print*, 'temp = ', temp
		call huboffset(mp_nonoffset_hub, xm_nonoffset_hub, rm_nonoffset_hub, xms_nonoffset_hub &
		, rms_nonoffset_hub, hub, nsp_interpolated_hub, scf, temp)
		
	! write to file:
	! fname2 = 'plot_xm_mp_offset_hub.'//trim(casename)//'.dat'
	! open(13, file = fname2, status = 'unknown')
		! do i = 1, nsp_interpolated_hub
			! write(13, *) xm_nonoffset_hub(i), mp_nonoffset_hub(i), rm_nonoffset_hub(i)
		! enddo
	! close(13)	
	
endif
!*************************************************************************
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
! Splining the x, r coordinates of the streamlines (construction lines)...
! to create mprime spline coeffs.
na = nsl
do ia = 1, na
	if(fext.eq.'e3c-des')then
		call spl_discjoint(xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia))
		call spl_discjoint(rm(1, ia), rms(1, ia), mp(1, ia), nsp(ia))
	else
		call spline(xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia), 999.0, -999.0)
		call spline(rm(1, ia), rms(1, ia), mp(1, ia), nsp(ia), 999.0, -999.0)
	endif
enddo
! print*, "xm(i, ia), rm(i, ia), mp(i, ia), xms(i, ia), rms(i, ia)"
! do ia = 1, na
    ! do i = 1, nsp(ia)
       ! print*, xm(i, ia), rm(i, ia), mp(i, ia), xms(i, ia), rms(i, ia)
    ! enddo
    ! print*, " "
! enddo
!
!-----------------------------------------------------------------------------

!---------------------------------------------------------------------------
!Calculating offsets for the streamlines if the offset is given as input.
!---------------------------------------------------------------------------
write(*, *)
print*, 'hub offset:', hub
print*, 'tip offset:', tip
write(*, *)

!HUB offset
if(hub.ne.0)then
	call huboffset(mphub(1, 1), xm(1, 1), rm(1, 1), xms(1, 1), rms(1, 1), hub, nsp(1), scf, casename)
	!Offset mprime coordinates updated to the mprime array at hub and casing.
	do i = 1, nsp(1)
		mp(i, 1) = mphub(i, 1)
		!print*, 'mphub', mp(i, 1)
	enddo
endif


!TIP offset
if(tip.ne.0)then
	do i = 1, nsp(na)
		xt(i, 1) = xm(i, na)
		rt(i, 1) = rm(i, na)
		dxn(i, 1) = xms(i, na)
		drn(i, 1) = rms(i, na)
	enddo  
	!call tipoffset(mptip, xt, rt, dxn, drn, tip, nsp(na), scf, nsl, casename)  ! changed by the next line Nemnem 6 10 2014
	call tipoffset(mptip, xm(1, nsl), rm(1, nsl), xms(1, nsl), rms(1, nsl), tip, nsp(nsl), scf, nsl, casename)
	!Offset mprime coordinates updated to the mprime array at hub and casing.
	do i = 1, nsp(na)
		mp(i, na) = mptip(i, 1)
		!print*, mp(i, na)
	enddo
endif
!----------------------------------------------------------------------------------
   ! taking some values for analysis:
   !------------------------------
! if (hub == 0) then! picking up some analysis values
	! fname1 = 'plot_xm_mp.unoffset.'//trim(casename)//'.dat'
	! fname2 = 'plot_rm_mp.unoffset.'//trim(casename)//'.dat'
	! fname3 = 'plot_xm_rm.unoffset.'//trim(casename)//'.dat' 
! else
	! fname1 = 'plot_xm_mp.'//trim(casename)//'.dat'
	! fname2 = 'plot_rm_mp.'//trim(casename)//'.dat'
	! fname3 = 'plot_xm_rm.'//trim(casename)//'.dat'
! endif

! open(1, file = fname3, status = 'unknown')
! open(2, file = fname1, status = 'unknown')
! open(22, file = fname2, status = 'unknown')

! do ia = 1, na  
   	! do i = 1, nsp(ia)
		! write(*, *) xm(i, ia), rm(i, ia), mp(i, ia)
	! enddo
	! write(*, *) '	0		0'
   ! do i = 1, nsp(ia)
		! write(2, *) xm(i, ia), mp(i, ia)
   ! enddo
   ! write(2, *) '		0		0'
   ! do i = 1, nsp(ia)
		! write(22, *) rm(i, ia), mp(i, ia)
   ! enddo
   ! write(22, *) '	0		0'
! enddo

! close(22)
! close(2)
! close(1)
!----------------------------------------------------------------------------------   

!----------------------------------------------------------------------------------
! Obtaining LE/TE x, r values on each streamline using the LE/TE curve and...
! ...the streamline curve intersection 3/27/12----
!----------------------------------------------------------------------------------
write(*, *)'xLE    rLE     xTE     rTE'
! LE curve intersection with the streamline curve---------
print*, 'Calculating LE x, r points... '

do ia = 1, na
	! if (npoints.eq.nsl)then ! LE, TE points same as no. of streamlines
		! x_le(ia) = xle(ia)
		! r_le(ia) = rle(ia)
	! else ! when interpolation of LE, TE coordinates is required
	   s1le(ia) = mp(2, ia)
	   s2le(1) = rle(1)
		if(ia.eq.1)then
			s2le(ia) = s2le(1)
		else
			s2le(ia) = s2le(ia-1)
		endif
		!------------------------------
		!print*, 's1le, s2le', s1le(ia), s2le(ia)
		! print*, 'inlet to spl_intersect', s1le(ia), s2le(ia), xm(1, ia), xms(1, ia), rm(1, ia), &
				   ! rms(1, ia), mp(1, ia), nsp(ia), xle(1), xles(1), rle(1), &
				   ! rles(1), sle(1), npoints 
		call spl_intersect(s1le(ia), s2le(ia), xm(1, ia), xms(1, ia), rm(1, ia), &
				   rms(1, ia), mp(1, ia), nsp(ia), xle(1), xles(1), rle(1), &
				   rles(1), sle(1), npoints)
		x_le(ia) = spl_eval(s1le(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia))
		r_le(ia) = spl_eval(s1le(ia), rm(1, ia), rms(1, ia), mp(1, ia), nsp(ia))
	! endif   
enddo

! TE curve intersection with the streamline curve---------
!if(xslope_TE.ge.rslope_TE)then
print*, 'Calculating TE x, r points...'

do ia = 1, na
	! if (npoints.eq.nsl)then ! LE, TE points same as no. of streamlines
		! x_te(ia) = xte(ia)
		! r_te(ia) = rte(ia)
	! else ! when interpolation of LE, TE coordinates is required
		s1te(ia) = s1le(ia)
		s2te(1) = rte(1)
		if(ia.eq.1)then
			s2te(ia) = s2te(1)
		else
			s2te(ia) = s2te(ia-1)
		endif
		!print*, 's1te, s2te', s1te(ia), s2te(ia)
		call spl_intersect(s1te(ia), s2te(ia), xm(1, ia), xms(1, ia), rm(1, ia), &
					rms(1, ia), mp(1, ia), nsp(ia), xte(1), xtes(1), rte(1), &
					rtes(1), ste(1), npoints)
		x_te(ia) = spl_eval(s1te(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia))
		r_te(ia) = spl_eval(s1te(ia), rm(1, ia), rms(1, ia), mp(1, ia), nsp(ia))
	! endif
	!!KB
	! write(*, *) 'Initial msle = ', s1le(ia), ' Initial mste = ', s1te(ia)
enddo

!-------------------------------------------------------
!! Assigning to separate variables to be written to a file later
fname1 = 'LE_TE_intersection.'//trim(casename)//'.dat'
open(3, file = fname1, status = 'unknown')

Allocate(sec_radius(nsl, 2))
do ia = 1, na
	! write(*, *)x_le(ia), r_le(ia), x_te(ia), r_te(ia)
	write(3, *)x_le(ia), r_le(ia), x_te(ia), r_te(ia) !Ahmed Nemnem 6/13/2014
	sec_radius(ia, 1) = r_le(ia) !for LE   Ahmed Nemnem 2/11/2014
	sec_radius(ia, 2) = r_te(ia) !for TE
enddo
close(3)
! write(*, *)

! do ia = 1, nsl
   ! print*, 'Nondimensional sec_radius for LE, TE, sec(', ia, ') = ', sec_radius(ia, :)
! enddo
! write(*, *)
! write(*, *)

!--------------------------------------------------------------------
! Checking if r_slope > x_slope for non-axial machines
!--------------------------------------------------------------------
! i_slope = 1
do ia = 1, na
	i_slope = 0
	do i = 1, nsp(ia)
		xm_slope = abs(xms(i, ia))
		rm_slope = abs(rms(i, ia))
		if(rm_slope.ge.xm_slope.and.i_slope.eq.0) i_slope = i  
	enddo
	print*, 'i_slope', i_slope
enddo

!--------------------------------------------------------------------
! Calculating msLE and msTE using LE, TE (x, r) and determining axial/radial/mixed flow
!--------------------------------------------------------------------
! Obtaining initial guesss values for msle and mste to calculate the correct values-----
do ia = 1, na
	xsle = x_le(ia)
	xste = x_te(ia)
	rsle = r_le(ia)
	rste = r_te(ia)
	ile = 0
	ite = 0

	!Leading Edge index --------------------------------------------------
	do i = 1, nsp(ia) - 1
		ii = i
		xi = xm(i, ia)
		ri = rm(i, ia)
		xi1 = xm(i+1, ia)
		ri1 = rm(i+1, ia)
		x1hub = xm(1, 1)
		x1tip = xm(1, na)
		r1hub = rm(1, 1)
		r1tip = rm(1, na)      
		if(i_slope.eq.0)then   ! purely axial flow    
		if((xi1.ge.xsle.and.xi.le.xsle).and.ile.eq.0) ile = i       
		elseif(i_slope.ne.0)then ! not purely axial flow      
		if(ii.le.i_slope)then ! flow at LE     
		  if((r1tip.gt.r1hub).or.(r1tip.lt.r1hub))then ! axial flow at LE
			axial_LE = .True.
			if((xi1.ge.xsle.and.xi.le.xsle).and.ile.eq.0) ile = i 
		  elseif((x1hub.lt.x1tip).or.(x1hub.gt.x1tip))then ! radial flow at LE
			radial_LE = .True.                   
			if(ri1.ge.rsle.and.ri.le.rsle.and.ile.eq.0) ile = i  
		  endif
		endif       
		endif     
	enddo
	print*, 'ile:', ile
	! if(ile.le.2)then
	 ! msle(ia) = 0.
	! else
	 ! msle(ia) = mp(ile-2, ia)
	! endif
	! print*, 'msle-initial guess', msle(ia)
	!--------------------------------------------------------------------
	!
	! Trailing Edge index -----------------------------------------------
	do i = 1, nsp(ia) - 1
	  ii = i
	  xi = xm(i, ia)
	  ri = rm(i, ia)
	  xi1 = xm(i+1, ia)
	  ri1 = rm(i+1, ia)
	  if(i_slope.eq.0)then   ! purely axial flow     
		if((xi1.ge.xste.and.xi.le.xste).and.ite.eq.0) ite = i      
	  elseif(i_slope.ne.0)then ! not purely axial flow    
		if(ii.ge.i_slope)then! flow at TE
			if((r1tip.gt.r1hub).or.(r1tip.lt.r1hub))then ! Radial flow at TE
			  radial_TE = .True.
			  if((ri1.ge.rste.and.ri.le.rste).and.ite.eq.0) ite = i
			elseif((x1hub.lt.x1tip).or.(x1hub.gt.x1tip))then ! Axial flow at TE
			  axial_TE = .True.
			  if((xi1.ge.xste.and.xi.le.xste).and.ite.eq.0) ite = i              
			endif
		endif       
	  endif     
	enddo
	print*, 'ite:', ite
	! mste(ia) = mp(ite+2, ia)
	! print*, 'mste-initial guess', mste(ia)
	! write(*, *) 
enddo

!-----------------------------------------------------------------------

!--------------------------------------------------------------------
!!Determining purely axial, purely radial, mixed flow
!--------------------------------------------------------------------
do ia = 1, na
	!!KB
	msle(ia) = s1le(ia)
	mste(ia) = s1te(ia)
	! print*, msle(ia), s1le(ia)
   if(i_slope.eq.0)then ! purely axial flow
     print*, 'Using x values for msLE due to a purely axial flow.'
		!print*, 'msle(ia), x_le(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia) before'
		!print*, msle(ia), x_le(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia)
     ! call spl_inv(msle(ia), x_le(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia))
		!print*, 'msle(ia), x_le(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia) after'
		!print*, msle(ia), x_le(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia)
     print*, 'Using x values for msTE due to a purely axial flow.'
		!print*, 'mste(ia), x_te(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia) before'
		!print*, mste(ia), x_te(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia)
     ! call spl_inv(mste(ia), x_te(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia))
		!print*, 'mste(ia), x_te(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia) after'
		!print*, mste(ia), x_te(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia)
     
     !---Evaluating span  for an axial blade
     lref = abs(r_le(na) - r_le(1))
     span(ia) = abs(r_le(ia) - r_le(1))/real(lref)    

   elseif(i_slope.ne.0)then

     !----Leading Edge--------------------------------------------------
     if(axial_LE)then ! axial flow at LE
     
       print*, 'Using x values for msLE due to axial flow at LE.'
       ! call spl_inv(msle(ia), x_le(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia))
       
       !---Evaluating span  for an axial blade
       lref = abs(r_le(na) - r_le(1))
       span(ia) = abs(r_le(ia) - r_le(1))/real(lref)
       
     elseif(radial_LE)then! non-axial flow at LE
     
       print*, 'Using r values for msLE due to non-axial flow at LE.'
       ! call spl_inv(msle(ia), r_le(ia), rm(1, ia), rms(1, ia), mp(1, ia), nsp(ia))
       
       !---Evaluating span for a non-axial blade
       xdiff = abs(x_le(na) - x_le(1))
       lref = xdiff
       span(ia) = abs(x_le(ia) - x_le(1))/real(lref)
       
     endif ! endif for LE

     !----Trailing Edge	------------------------------------------------
     if(axial_TE)then ! axial flow at TE
     
       print*, 'Using x values for msTE due to axial flow at TE.'
       ! call spl_inv(mste(ia), x_te(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia))
 
     elseif(radial_TE)then ! non-axial flow at TE
     
       print*, 'Using r values for msTE due to non-axial flow at TE.'
       ! call spl_inv(mste(ia), r_te(ia), rm(1, ia), rms(1, ia), mp(1, ia), nsp(ia))

     endif ! end if for TE  
     
   endif ! endif for axial/radial flow determination

   print*, 'msle:', msle(ia)
   print*, 'mste:', mste(ia)
   chordm(ia) = abs(mste(ia) - msle(ia))
   print*, 'chordm:', chordm(ia)
   ! write(*, *)
enddo

!--------------------------------------------------------------------
!Calculating the phi to adjust for BetaZ conversion
!--------------------------------------------------------------------
do ia = 1, na
   xmsle(ia) = 0.
   rmsle(ia) = 0.
   !--- Calculating dx/dm' and dr/dm' value at LE of each streamline------
   xmsle(ia) = dspl_eval(msle(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia))
   rmsle(ia) = dspl_eval(msle(ia), rm(1, ia), rms(1, ia), mp(1, ia), nsp(ia))
   !-----Calculating dphi_s_in = dr/dx = (dr/dm')/(dx/dm') -------
   phi_s_in(ia) = (atan(rmsle(ia)/xmsle(ia)))
   print*, 'phi_s_in(ia)', phi_s_in(ia)/dtor
enddo
do ia = 1, na
   xmste(ia) = 0.
   rmste(ia) = 0.
   !--- Calculating dx/dm' and dr/dm' value at TE of each streamline------
   xmste(ia) = dspl_eval(mste(ia), xm(1, ia), xms(1, ia), mp(1, ia), nsp(ia))
   rmste(ia) = dspl_eval(mste(ia), rm(1, ia), rms(1, ia), mp(1, ia), nsp(ia))
   !-----Calculating dphi_s_out = dr/dx = (dr/dm')/(dx/dm') -------
   phi_s_out(ia) = (atan(rmste(ia)/xmste(ia)))
   print*, 'phi_s_out(ia)', phi_s_out(ia)/dtor
enddo

!--------------------------------------------------------------------------------------
!**************************************************************************************
!*****Adding new inputs from spancontrolinputs(Syed Moez 03/02/2014) ********************
!**************************************************************************************

if((curv.ne.0.or.thick.ne.0.or.LE.ne.0&
.or.thick_distr.ne.0).and.trim(spanwise_spline).eq.'spanwise_spline')then
!This program is being called to create cubic bspline
!between the inputs from spancontrolinputs and
!incorporate those values into 3dbgb.
  call span_variation(nsl)
end if




!**************************************************************************************
!*****input parameters for blade generation (2D airfoils) *****************************
!**************************************************************************************
nspn = na
do js = 1, nspn
   in_beta(js) = in_beta(js)
   out_beta(js) = out_beta(js)
   thk_c(js) = thk_c(js)
enddo
!endif

!----------------------------------------------------------------------
!!-Control points for spanwise inlet Betaz------------------------------------------
if((trim(anglespline).eq.'inletspline').or.(trim(anglespline).eq.'inoutspline'))then
  write(*, *)
  write(*, *)' Inlet Beta defined spanwise by a cubic B-spline using control points.'
  write(*, *)'   span         in_Beta (spline)'
  call cubicspline(xcpinbeta, spaninbeta, cpinbeta, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
  call cubicbspline_intersec(y_spl_end, xc, yc, ncp1, span, inbeta_s, na, xbs, ybs)
  do ia = 1, na
	 print*, span(ia), inbeta_s(ia)
     in_beta(ia) = inbeta_s(ia)
  enddo
elseif (trim(anglespline).eq.'inci_dev_spline')then
  write(*, *)
  write(*, *)' Inlet Beta incidence defined spanwise by a cubic B-spline using control points.'
  write(*, *)'   span         in_Beta (spline)'
  call cubicspline(xcpinbeta, spaninbeta, cpinbeta, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
  call cubicbspline_intersec(y_spl_end, xc, yc, ncp1, span, inci_s, na, xbs, ybs)
  do ia = 1, na
     in_beta(ia) = inBetaInci(in_beta(ia), inci_s(ia))
	 print*, span(ia), in_beta(ia)
  enddo
endif
!----------------------------------------------------------------------
!!-Control points for panwise outlet BetaZ------------------------------------------
if((trim(anglespline).eq.'outletspline').or.(trim(anglespline).eq.'inoutspline'))then
  write(*, *)
  write(*, *)' Outlet Beta defined spanwise by a cubic B-spline using control points.'
  write(*, *)'   span        out_Beta (spline)'
  call cubicspline(xcpoutbeta, spanoutbeta, cpoutbeta, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
  call cubicbspline_intersec(y_spl_end, xc, yc, ncp1, span, outbeta_s, na, xbs, ybs)
  do ia = 1, na
	 print*, span(ia), outbeta_s(ia)
     out_beta(ia) = outbeta_s(ia)     
  enddo
elseif (trim(anglespline).eq.'inci_dev_spline')then
  write(*, *)
  write(*, *)' Outlet Beta deviation defined spanwise by a cubic B-spline using control points.'
  write(*, *)'   span        out_Beta (spline)'
  call cubicspline(xcpoutbeta, spanoutbeta, cpoutbeta, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
  call cubicbspline_intersec(y_spl_end, xc, yc, ncp1, span, dev_s, na, xbs, ybs)
  do ia = 1, na
	 out_beta(ia) = outBetaDevn(in_beta(ia), out_beta(ia), dev_s(ia))
	 print*, span(ia), out_beta(ia)
  enddo
endif
!----------------------------------------------------------------------
! Splining Control points for chord, stagger, outbeta, tm/c
!----------------------------------------------------------------------
!!---Control points for chord -----------------------------------------
if(chord_switch.eq.2)then   ! Changed to spline multiplier 9 3 2014 Nemnem (default = 1)
  write(*, *)
  write(*, *)'   span        chord_multipliers'
  call cubicspline(xcpchord, spanchord, cpchord, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
  call cubicbspline_intersec(y_spl_end, xc, yc, ncp1, span, chords, na, xbs, ybs)
  do ia = 1, na
     print*, span(ia), chords(ia)
  enddo
endif

!----------------------------------------------------------------------
!!-Control points for stagger------------------------------------------
staggspline = in_beta(1)
if(staggspline.eq.999.)then
  write(*, *)
  write(*, *)' Stagger defined spanwise by a cubic B-spline using control points.'
  write(*, *)'   span        stagger'
  call cubicspline(xcpinbeta, spaninbeta, cpinbeta, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
  call cubicbspline_intersec(y_spl_end, xc, yc, ncp1, span, inbeta_s, na, xbs, ybs)
  do ia = 1, na
	 print*, span(ia), inbeta_s(ia)
  enddo
endif

!----------------------------------------------------------------------
!!-Control points for tm/c spline multiplier---------------------------------------
if(tm_c_spline)then
  write(*, *)' tm/c thickness ratio defined radially with 2D spline using control points.'
  call cubicspline(xcptm_c, spantm_c, cptm_c, xbs, ybs, y_spl_end, nspline, xc, yc, ncp1)
  call cubicbspline_intersec(y_spl_end, xc, yc, ncp1, span, thk_tm_c_spl, na, xbs, ybs)
  do ia = 1, na
	 print*, span(ia), thk_tm_c_spl(ia)
  enddo
endif

!-------------------------------------------------------------
! Allocating variables for further calculations
!!-------------------------------------------------------------
!000000000000000000000000000000000000000000000000000000000000
Allocate(bladedata(amount_data, nsl))
Allocate(intersec_coord(12, nsl))
Allocate(throat_3D(nsl))
Allocate(mouth_3D(nsl))
Allocate(exit_3D(nsl))
Allocate(throat_pos(nsl))
Allocate(throat_index(nsl))
!0000000000000000000000000000000000000000000000000000000000000

!-------------------------------------------------------------
! calling bladegen routine to create 2D airfoil shapes
!!-------------------------------------------------------------
do js = 1, nspn
   mles = msle(js)
   ! msle = msle(1)
   mtes = mste(js)
   mr1 = mrel1(js)
   stak_u = stk_u(js)
   stak_v = stk_v(js)

   !----------------------------------------------------------------------
   !betaZ-betaM conversion
   !----------------------------------------------------------------------
   !print*, in_beta(js), out_beta(js)
   !  if(trim(fext).eq.'e3c')then
   if(beta_switch.eq.0)then ! All axial: Converting Beta_z to Beta_m ;tanBetaM = tanBetaz* cos(phi)
     !print*, 'Converting BetaZ to BetaM...'
     sinl = tan(in_beta(js)*dtor)*cos(phi_s_in(js))! angles used are Beta_m for axial machines
     sext = tan(out_beta(js)*dtor)*cos(phi_s_out(js))
   elseif(beta_switch.eq.1)then ! All radial: Converting Beta_r to Beta_m {tanBetaM = tanBetaR*sin(phi)}
     sinl = tan(in_beta(js)*dtor)*sin(phi_s_in(js))
     sext = tan(out_beta(js)*dtor)*sin(phi_s_out(js))
   elseif(beta_switch.eq.2)then ! AxailIN-RadialOUT: Converting Beta_r to Beta_m {tanBetaM = tanBetaR*sin(phi)}
     sinl = tan(in_beta(js)*dtor)*cos(phi_s_in(js))
     sext = tan(out_beta(js)*dtor)*sin(phi_s_out(js))
   elseif(beta_switch.eq.3)then ! RadialIN-AxialOUT: Converting Beta_r to Beta_m {tanBetaM = tanBetaR*sin(phi)}
     sinl = tan(in_beta(js)*dtor)*sin(phi_s_in(js))
     sext = tan(out_beta(js)*dtor)*cos(phi_s_out(js))
   endif     
   ! print*, 'sinl, sext', sinl, sext

   !----------------------------------------------------------------------
   !chord switches
   !----------------------------------------------------------------------
   if(chord_switch.eq.1)then
     print*, 'Non-dimensional chord from the input...'
     chrdx = chord(js)
     axchrd(js) = chord(js)
   elseif(chord_switch.eq.0)then
     print*, 'Internally calculated chord...'
     chrdx = chordm(js)
     axchrd(js) = chord(js)
   elseif(chord_switch.eq.2)then
     print*, 'Chord multiplier calculated using spline control points...'
     chrdx = chordm(js) * chords(js)
     axchrd(js) = chord(js)
   endif ! endif for chord options

   !----------------------------------------------------------------------
   !stagger switches
   !----------------------------------------------------------------------
   if(staggspline.eq.999.)then ! stagger from the spline control table
     stgr = inbeta_s(js)
   elseif(chord_switch.ne.0)then
     stgr = in_beta(js) ! stagger from the angles table
   else
     stgr = 0.
     print*, 'Stagger calculated from the inlet and exit angles...'
   endif ! endif for stagger options
   !
   !----------------------------------------------------------------------
   if(curv.ne.0.and.LE.ne.2)then
     ! Deallocate(sting_l_all)
     !Allocate(sting_l_all(nsl))
     sting_l_all(1:nsl) = 0.
     ! print*, sting_l_all
   elseif(curv.eq.0.and.LE.ne.2)then
     Allocate(sting_l_all(nsl))
     sting_l_all(1:nsl) = 0.
   endif
   
   !----------------------------------------------------------------------
   !----------------------------------------------------------------------
   !tm/c thickness spline switch
   !----------------------------------------------------------------------
   if(tm_c_spline)then
     print*, 'Thickness t/c will be multiplied by tm/c 2D spline definition...'
     thkc = thk_c(js)*thk_tm_c_spl(js)
   else
	 thkc = thk_c(js)
   endif
   
   !----------------------------------------------------------------------
				
   call bladegen(nspn,thkc,mr1,sinl,sext,chrdx,js,blext(js),xcen,ycen,airfoil(js), &
                stgr,stack,chord_switch,stak_u,stak_v,xb_stk,yb_stk,stack_switch, &
		        nsl,nbls,curv,thick,LE,np,ncp_curv,ncp_thk,curv_cp,thk_cp, wing_flag, &
                lethk_all,tethk_all,s_all,ee_all,thick_distr,thick_distr_3_flag,umxthk_all, &
		        C_le_x_top_all,C_le_x_bot_all,C_le_y_top_all,C_le_y_bot_all, &
		        LE_vertex_ang_all,LE_vertex_dis_all,sting_l_all,sting_h_all,LEdegree,no_LE_segments, &
		        sec_radius,bladedata,amount_data,scf,intersec_coord,throat_index, &
                n_normal_distance,casename,develop,isdev,mble,mbte,mles,mtes,i_slope,jcellblade_all, &
                etawidth_all,BGgrid_all,thk_tm_c_spl,isxygrid)				

   mprime_ble(js) = mble
   mprime_bte(js) = mbte
   xcg(js) = xcen
   ycg(js) = ycen
   xb_stack(js) = xb_stk
   yb_stack(js) = yb_stk
   write(*, *)
   stagger(js) = stgr
   !
   if(curv.eq.0.and.LE.ne.2)then
     Deallocate(sting_l_all)
   endif
enddo
35 close (1)

if(is2d) then
	goto 1001
endif

!
!**************************************************************************************
!************stacking blade sections to create a 3d blade shape************************
!**************************************************************************************
do js = 1, nrow ! called for each bladerow (only once since nrow = 1)
   nsec = nspn
   call bladestack(nspn, x_le, x_te, r_le, r_te, nsec, scf, xcg, ycg, msle, &
                   stk_u, stk_v, xb_stack, yb_stack, np, ile, stack, &
				   cpdeltam, spanmp, xcpdelm, cpdeltheta, spantheta, xcpdeltheta, &
				   cpinbeta, spaninbeta, xcpinbeta, cpoutbeta, spanoutbeta, xcpoutbeta, &
				   hub, tip, xm, rm, xms, rms, mp, nsp, bladedata, amount_data, intersec_coord, &
                   throat_3D, mouth_3D, exit_3D, casename, nbls, LE, axchrd, mprime_ble, &
                   mprime_bte, units, stagger, chrdsweep, chrdlean, axial_LE, radial_LE)
enddo

! --------------------------------------------------------------------------------

!**************************************************************************************
! writing the output file for the blade data: (volume calculation)
!**************************************************************************************
do i = 1, nsl-1
   ! approximate method to calculate blade volume: strip average area * strip height
!   bladedata(amount_data, i) = (bladedata(9, i)+bladedata(9, i+1))/2.*scf*((sum(sec_radius(i+1, :))/2.)-(sum(sec_radius(i, :))/2.))
   ! Volume of non uniform frustum = h/3(A1+A2+sqrt(A1.A2))
   bladedata(amount_data, i) = (bladedata(9, i)+bladedata(9, i+1)+sqrt(bladedata(9, i)*bladedata(9, i+1)))*(scf*((sum(sec_radius(i+1, :))/2.)-(sum(sec_radius(i, :))/2.))/3.)
enddo

bladedata(amount_data, nsl) = sum(bladedata(amount_data, 1:nsl-1))
print*, (np+1)/2

!----------------------------------------------------------------------------
! writing the 3D nondimensional throat, span, in_beta & out_beta:
!----------------------------------------------------------------------------
do js = 1, nsl
   ! add some variables to bladedata file:
   bladedata(1, js) = span(js)
   bladedata(2, js) = in_beta(js)
   bladedata(3, js) = out_beta(js)

   ! subroutine which prints throat_index---------------------------
   call throatindex(throat_pos, throat_index, n_normal_distance, js, nsl)

   bladedata(12, js) = throat_3D(js)*scf ! dimensional throat
enddo

!----------------------------------------------------------------------------
! calling routine to write the bladedata containing throat and other info to a file
!----------------------------------------------------------------------------
call outputfiledata (bladedata, nsl, amount_data, throat_pos, casename, units)

!**************************************************************************************
! calling a routine to write dimensional 3D blade coordinates to separate files
!**************************************************************************************
call b3d2sec(scf, fext, ibrowc, nbls, casename)

!**************************************************************************************
!writing pitch and chord non-dimensional values to a file
!**************************************************************************************
pitch = 2*pi/nbls

call cascade_nondim_file(msle, mste, mprime_ble, mprime_bte, chordm, pitch, nsl, ibrow, casename)

!----------------------------------------------------------------------------

!**************************************************************************************
!FORMAT statements
!**************************************************************************************
!101  format(I2, 2X, 6(f19.16, 1x))
106  format(f8.5)
1000 format(a)

!**************************************************************************************
! Deallocation of variables
!**************************************************************************************
1001 deallocate (x_le, x_te, r_le, r_te, in_beta, out_beta, mrel1, chord, thk_c, inci, devn, sec_flow_ang)
deallocate (phi_s_in, phi_s_out, stagger, chordm, msle, s1le, s2le, s1te)
deallocate (sang, stk_u, stk_v, total_camber, mprime_ble, mprime_bte, sec_radius)
!-------------------------------------------------------------------------------------

end program bgb3d
!**************************************************************************************