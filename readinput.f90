subroutine readinput(fname)
! reads the 3dbgbinput file
use globvar

implicit none

character*256 :: fname, temp, temp2, tempr1, fname1, fname2, fname3

real*8 inBetaInci, outBetaDevn

if (allocated(x_le          )) deallocate(x_le          )
if (allocated(x_te          )) deallocate(x_te          )
if (allocated(r_le          )) deallocate(r_le          )
if (allocated(r_te          )) deallocate(r_te          )
if (allocated(in_beta       )) deallocate(in_beta       )
if (allocated(out_beta      )) deallocate(out_beta      )
if (allocated(mrel1         )) deallocate(mrel1         )
if (allocated(chord         )) deallocate(chord         )
if (allocated(thk_c         )) deallocate(thk_c         )
if (allocated(inci          )) deallocate(inci          )
if (allocated(devn          )) deallocate(devn          )
if (allocated(sec_flow_ang  )) deallocate(sec_flow_ang  )
if (allocated(phi_s_in      )) deallocate(phi_s_in      )
if (allocated(phi_s_out     )) deallocate(phi_s_out     )
if (allocated(stagger       )) deallocate(stagger       )
if (allocated(chordm        )) deallocate(chordm        )
if (allocated(msle          )) deallocate(msle          )
if (allocated(s1le          )) deallocate(s1le          )
if (allocated(s2le          )) deallocate(s2le          )
if (allocated(s1te          )) deallocate(s1te          )
if (allocated(s2te          )) deallocate(s2te          )
if (allocated(sang          )) deallocate(sang          )
if (allocated(stk_u         )) deallocate(stk_u         )
if (allocated(stk_v         )) deallocate(stk_v         )
if (allocated(total_camber  )) deallocate(total_camber  )
if (allocated(mprime_ble    )) deallocate(mprime_ble    )
if (allocated(mprime_bte    )) deallocate(mprime_bte    )
if (allocated(BGgrid_all    )) deallocate(BGgrid_all    )
if (allocated(jcellblade_all)) deallocate(jcellblade_all)
if (allocated(etawidth_all  )) deallocate(etawidth_all  )
if (allocated(axchrd        )) deallocate(axchrd        )
allocate (x_le(nspan))
allocate (x_te(nspan))
allocate (r_le(nspan))
allocate (r_te(nspan))
allocate (in_beta(nspan))
allocate (out_beta(nspan))
allocate (mrel1(nspan))
allocate (chord(nspan))
allocate (thk_c(nspan))
allocate (inci(nspan))
allocate (devn(nspan))
allocate (sec_flow_ang(nspan))
allocate (phi_s_in(nspan))
allocate (phi_s_out(nspan))
allocate (stagger(nspan))
allocate (chordm(nspan))
allocate (msle(nspan))
allocate (s1le(nspan))
allocate (s2le(nspan))
allocate (s1te(nspan))
allocate (s2te(nspan))
allocate (sang(nspan))
allocate (stk_u(nspan))
allocate (stk_v(nspan))
allocate (total_camber(nspan))
allocate (mprime_ble(nspan))
allocate (mprime_bte(nspan))
allocate (BGgrid_all(nspan))
allocate (jcellblade_all(nspan))
allocate (etawidth_all(nspan))
allocate(axchrd(nspan))

! constants
abs_zero = 0.0000000000000000

open(1, file = fname, status = 'unknown')
rewind(1)
!write(*, *)
!write(*, *) 'Reading inputs from 3dbgbinput file'
!write(*, *)
!---reading parameters from input file----
read(1, *)temp
!reading the casename
read(1, *)fext
!write(*, *)'case:', fext
casename = trim(fext)
read(1, *)temp
read(1, *)ibrow
!write(*, *)'bladerow #:', ibrow
write(ibrowc, '(i3)')ibrow
!print*, ibrowc
!write(*, *)
read(1, *)temp
read(1, *)nbls ! number of blades in this row
!print*, 'Number of blades in this row:', nbls
read(1, '(A)'), temp
units = temp(24:25)
read(1, *)scf
!write(*, *)'bsf:', scf
!write(*, *) 
read(1, *)temp
read(1, *)nsl
!print*, 'Number of streamlines:', nsl
!write(*, *)
read(1, *)temp
spanwise_angle_spline = .False.
spanwise_inci_dev_spline = .False.
read(1, *)beta_switch, temp2  ! Input angle switch (0 = BetaZ; 1 = BetaR)
anglespline = temp2 
if ((anglespline .eq. 'inletspline').or.(anglespline .eq. 'outletspline').or.(anglespline .eq. 'inoutspline')) then
	spanwise_angle_spline = .True.
	print*, " "
	print*, "Angles defined spanwise as a B-spline using control points..."
	print*, " "
	print*, trim(anglespline)
	read(1, *)temp
elseif (anglespline .eq. 'inci_dev_spline') then
	spanwise_inci_dev_spline = .True.
	print*, " "
	print*, "Incidence and Deviation defined spanwise as a B-spline using control points..."
	print*, " "
	print*, trim(anglespline)
	read(1, *)temp	
endif
if (beta_switch.eq.4) then
	beta_switch = 0
	wing_flag = 1
endif
read(1,*)curv, spanwise_spline     !  Airfoil curvature control switch
if (trim(spanwise_spline).eq.'spanwise_spline')then
	read(1,*)temp
endif
read(1, *)thick_distr, temp2  ! Airfoil thickness distribution switch
if (len(trim(adjustl(temp2))) .eq. 3) then
	thick_distr_3_flag = trim(adjustl(temp2))
	read(1, *)temp
endif
read(1, *)thick        ! Airfoil thickness multiplier switch
read(1, *)temp
read(1, *)LE           ! Airfoil LE control switch
read(1, *)temp
read(1, *)chord_switch ! non-dimensional actual chord switch
read(1, *)temp
read(1, *)temp
!
!---- blade file names
do i = 1, nsl
	write(ibrowc1, '(i3)')i
	blext(i) = trim(adjustl(ibrowc1))//'.'//trim(adjustl(ibrowc))//'.'//fext
enddo
nspn = nsl
!print*, '   in_betaZ*    out_betaZ*'
do js = 1, nspn
	if (spanwise_angle_spline)then   ! Not reading it here since it is splined spanwise
		read(1, *, end = 35)tempr, tempr, tempr, &
		mrel1(js), chord(js), thk_c(js), inci(js), devn(js), sec_flow_ang(js)
	elseif (spanwise_inci_dev_spline) then 
		!reading inlet & outlet angles from table but not adding incidence and deviation from the table
		read(1, *, end = 35)tempr, in_beta(js), out_beta(js), mrel1(js), chord(js), thk_c(js), inci(js), devn(js), sec_flow_ang(js)
	else ! Reading the inlet and outlet angles from this table
		read(1, *, end = 35)tempr, in_beta(js), out_beta(js), mrel1(js), chord(js), thk_c(js), inci(js), devn(js), sec_flow_ang(js)
		!Adding incidence and deviation angles-------------------3/10/11
		!print*, in_beta(js), out_beta(js)   
	endif
enddo

!! override the inputs
call override_chord(nspn, chord)
call override_thk_c(nspn, thk_c)
call override_inci( nspn, inci )
call override_devn( nspn, devn )

if (.not.spanwise_angle_spline .and. .not.spanwise_inci_dev_spline) then
   do js = 1, nspn
      in_beta( js) =  inBetaInci(in_beta(js),               inci(js))
      out_beta(js) = outBetaDevn(in_beta(js), out_beta(js), devn(js))
   enddo
endif

!write(*, *)
! Reading the LE/TE curve definition---------
read(1, *)temp
read(1, *)temp 
read(1, *)npoints
read(1, *)temp
! write(*, *)'LE/TE defined by a curve with no. of points as:', npoints
! write(*, *)'xLE    rLE     xTE     rTE'
do i = 1, npoints
	read(1, *)xle(i), rle(i), xte(i), rte(i)
enddo
read(1, *)temp
read(1, *)temp
read(1, *)temp
read(1, *)temp
read(1, *)temp
read(1, *)stack_switch
read(1, *)temp
if (allocated(umxthk_all)) deallocate(umxthk_all)
Allocate(umxthk_all(nsl))
if (LE.ne.0) then
	do js = 1, nspn
		read(1, *)tempr, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), tempr, tempr, jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
		!print*, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
	enddo
elseif (LE == 0) then
        if (allocated(lethk_all)) deallocate(lethk_all)
        if (allocated(tethk_all)) deallocate(tethk_all)
	Allocate(lethk_all(nsl))
	Allocate(tethk_all(nsl))
	do js = 1, nspn
		read(1, *)tempr, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), lethk_all(js), tethk_all(js), jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
		! print*, airfoil(js), stk_u(js), stk_v(js), umxthk_all(js), lethk_all(js), tethk_all(js), jcellblade_all(js), etawidth_all(js), BGgrid_all(js)
	enddo
endif
read(1, *)temp
read(1, *)cpbsv, bsv1, bsv2
read(1, *)temp
do i = 1, cpbsv
	read(1, *)spanbsv(i), bf1(i), bf2(i)
enddo
read(1, *)temp
read(1, *)stack! Reading the stacking value
read(1, *)temp
read(1, '(A)')temp ! control points for sweep, switch for sweep along the chord.
!print*, 'temp: ', temp(12:12)
trueleansweep = temp(15:15)
read(temp(12:12), *)cpdeltam
!print*, 'cpdeltam: ', cpdeltam
!write(*, *)
if(trim(trueleansweep).ne.'')then
	read(trueleansweep, *)chrdsweep
	read(1, *)temp
	do i = 1, cpdeltam
		read(1, *)spanmp(i), tempr, xcpdelm(i)
	enddo
else
	read(1, *)temp
	do i = 1, cpdeltam
		read(1, *)spanmp(i), xcpdelm(i)
	enddo
endif
read(1, *)temp
read(1, '(A)')temp ! control points for lean, switch for lean normal to the chord.
!print*, 'temp: ', temp(12:12)
trueleansweep = temp(15:15)
read(temp(12:12), *)cpdeltheta
!print*, 'cpdeltheta: ', cpdeltheta
if(trim(trueleansweep).ne.'')then
	read(trueleansweep, *)chrdlean 
	!print*, 'Lean normal to the chord (1 = yes): ', chrdlean
	!write(*, *)
	read(1, *)temp
	do i = 1, cpdeltheta
		read(1, *)spantheta(i), tempr, xcpdeltheta(i)
		!print*, spantheta(i), xcpdeltheta(i)
	enddo
else
	!print*, 'Lean in the tangential direction (theta).'
	!write(*, *)
	read(1, *)temp
	do i = 1, cpdeltheta
		read(1, *)spantheta(i), xcpdeltheta(i)
		!print*, spantheta(i), xcpdeltheta(i)
	enddo
endif  
read(1, *)temp
read(1, *)cpinbeta ! control points for inBeta*
read(1, *)temp
do i = 1, cpinbeta
	read(1, *)spaninbeta(i), xcpinbeta(i)
enddo
read(1, *)temp
read(1, *)cpoutbeta ! control points for outBeta*
read(1, *)temp
do i = 1, cpoutbeta
	read(1, *)spanoutbeta(i), xcpoutbeta(i)
enddo
read(1, *)temp
read(1, *)cpchord ! control points for chord
read(1, *)temp
do i = 1, cpchord
	read(1, *)spanchord(i), xcpchord(i)
	xcpchord(i) = xcpchord(i) + 1.0
enddo
read(1, *)temp
read(1, *)cptm_c ! control points for tm/c
read(1, *)temp
!next line to always use the thickness tm/c as it is a multiplier (default = 1):
if ((thick_distr .ne. 0) .and. .not. is2d) then
	tm_c_spline = .True.
else
	tm_c_spline = .False.
endif    
do i = 1, cptm_c
	read(1, *)spantm_c(i), xcptm_c(i)
	xcptm_c(i) = xcptm_c(i) + 1.0
	! if (xcptm_c(i).ne.0.000000) tm_c_spline = .True. 
	!print*, cptm_c
enddo
read(1, *)temp 
read(1, *)hub
!print*, 'hub offset:', hub
read(1, *)temp
read(1, *)tip
!print*, 'tip offset:', tip
read(1, *)temp
do while(temp.ne.'x_s')
	read(1, *)temp
	!print*, 'temp = ', temp
enddo
!-----------------------------------------------------------
! Calculating m prime coordinates
!-----------------------------------------------------------
!--------------------------------------------
! using x, r coordinates as the input for streamlines
!--------------------------------------------
do ia = 1, nsl
	nsp(ia) = 0
	do while(.true.)
		read(1, *)trarray(1), trarray(2)
		!print*, trarray(1), trarray(2)
		if(trarray(2).ne.0)then
			nsp(ia) = nsp(ia) + 1
			xm(nsp(ia), ia) = trarray(1)
			rm(nsp(ia), ia) = trarray(2)
			!print*, ia, nsl
			! print*, xm(nsp(ia), ia), rm(nsp(ia), ia)
			else
			exit
		endif
	enddo
enddo

35 close(1)

return
end subroutine readinput
!**********************************************************************************
!**********************************************************************************



!**********************************************************************************
!**********************************************************************************
! subroutine readcontrolinput(fname5)
subroutine readcontrolinput(row_type, path)
!**********************************************************************************
! reads the controlinput.dat when the appropriate swittches are activated.
use globvar

implicit none

! character*(*) fname5
character*(*) path
character*256 temp, fname4, row_type
character*256 fname5
integer :: phantom_n

!if(curv.ne.0.or.thick.ne.0.or.LE.ne.0.or.thick_distr.ne.0)then
print*
!print*, 'using the controlinput file ....'
! Reading the input file of curv, thk, and LE for the bladegen:
fname5 = trim(path)//'controlinputs.'//trim(row_type)//'.dat'
print*, fname5
open(11, file = fname5)
rewind(11)
!----------------------------------------------------------------------
! Reading curvature:
!----------------------------------------------------------------------
if (allocated(ncp_curv)) deallocate(ncp_curv)
if (allocated(curv_cp )) deallocate(curv_cp )
Allocate(ncp_curv(nsl))
Allocate(curv_cp(20, 2*nsl))
read (11, *), temp	! the test case name (added 6 23 2013)
read (11, *), temp
current = 0
do i = 1, nsl
	read (11, *), temp
	read (11, *), temp
	read (11, *), ncp_curv(i)
	ncp_curv(i) = ncp_curv(i) + 2
        if (allocated(xcp)) deallocate(xcp)
        if (allocated(ycp)) deallocate(ycp)
	Allocate(xcp(ncp_curv(i)))
	Allocate(ycp(ncp_curv(i)))
	!print*, 'xcp = ', xcp
	!print*, 'ycp = ', ycp 
	current = i 
	!print*, 'current', current
	read (11, *), temp
	write(radialsec, *)current ! write out control points to a file to plot. Kiran 8/9/13
	! reading the control points: 
	do j = 1, (ncp_curv(i)-2)
		read(11, *), xcp(j+1), ycp(j+1)			
	enddo

	if(isdev) then
		fname4 = 'curvature_ctrl_pts.'//trim(adjustl(radialsec))//'.'//trim(casename)//'.txt'
		open(12, file = fname4)
		write(12, *)trim(casename)
		write(12, *)'Curvature Control points for camber'
		write(12, *)'u      v '   
		do j = 1, (ncp_curv(i)-2)
			write(12, *)xcp(j+1), ycp(j+1)
			!print*, xcp(j+3), ycp(j+3)
		enddo
		close(12)
	endif ! endif for developers

	! Fixed control points (leading and trailing edges)
	xcp(1) = 2*xcp(2)-xcp(3)	
	ycp(1) = 2*ycp(2)-ycp(3)
	xcp(ncp_curv(i)) = 2*xcp(ncp_curv(i)-1)-xcp(ncp_curv(i)-2) 	
	ycp(ncp_curv(i)) = 2*ycp(ncp_curv(i)-1)-ycp(ncp_curv(i)-2) 
	do k = 1, ncp_curv(i)
		curv_cp(k, 2*i-1) = xcp(k)
		curv_cp(k, 2*i) = ycp(k)
	enddo
	deAllocate(xcp)
	deAllocate(ycp)		  
enddo

!----------------------------------------------------------------------
! Reading Thickness
!----------------------------------------------------------------------
if (allocated(ncp_thk)) deallocate(ncp_thk)
if (allocated(thk_cp )) deallocate(thk_cp )
Allocate(ncp_thk(nsl))
Allocate(thk_cp(20, 2*nsl))
if (thick_distr.eq.3) then
	phantom_n = 2
else
	phantom_n = 4
endif
read (11, *), temp
do i = 1, nsl
	read (11, *), temp
	read (11, *), temp
	read (11, *), ncp_thk(i)
	ncp_thk(i) = ncp_thk(i) + phantom_n
        if (allocated(xcp)) deallocate(xcp)
        if (allocated(ycp)) deallocate(ycp)
	Allocate(xcp(ncp_thk(i)))
	Allocate(ycp(ncp_thk(i)))
	read (11, *), temp
	! reading the control points: 
	!do j = 1, (ncp_thk(i)-2)
	do j = 1, (ncp_thk(i)-phantom_n)
		!read(11, *), xcp(j+1), ycp(j+1)  ! for cubic spline
		read(11, *), xcp(j+(phantom_n/2)), ycp(j+(phantom_n/2))	! for quartic spline
		!print*, xcp(j+1), ycp(j+1)
	enddo
	if (thick_distr.eq.3) then
		xcp(1) = 0.
		ycp(1) = 0.
		xcp(ncp_thk(i)) = 0.
		ycp(ncp_thk(i)) = 0.
	else
		! Fixed control points (leading anf trailing edges)
		! xcp(1) = 2*xcp(2)-xcp(3)	!phantom control points cubic spline
		! ycp(1) = 2*ycp(2)-ycp(3)
		! xcp(ncp_thk(i)) = 2*xcp(ncp_thk(i)-1)-xcp(ncp_thk(i)-2) 	
		! ycp(ncp_thk(i)) = 2*ycp(ncp_thk(i)-1)-ycp(ncp_thk(i)-2)
		! 2 phantom points when dealing with quartic bspline:	 
		xcp(1) = 2*xcp(3)-xcp(5)	!phantom control points quartic spline
		ycp(1) = 2*ycp(3)-ycp(5)
		xcp(2) = 2*xcp(3)-xcp(4)	!phantom control points quartic spline
		ycp(2) = 2*ycp(3)-ycp(4)
		xcp(ncp_thk(i)) = 2*xcp(ncp_thk(i)-2)-xcp(ncp_thk(i)-4) 	
		ycp(ncp_thk(i)) = 2*ycp(ncp_thk(i)-2)-ycp(ncp_thk(i)-4)
		xcp(ncp_thk(i)-1) = 2*xcp(ncp_thk(i)-2)-xcp(ncp_thk(i)-3) 	
		ycp(ncp_thk(i)-1) = 2*ycp(ncp_thk(i)-2)-ycp(ncp_thk(i)-3)	
	endif	 
	do k = 1, ncp_thk(i)
		thk_cp(k, 2*i-1) = xcp(k)
		thk_cp(k, 2*i) = ycp(k)
	enddo
	deAllocate(xcp)
	deAllocate(ycp)
enddo

!----------------------------------------------------------------------
! Reading Leading edge parameters:
!----------------------------------------------------------------------
if (allocated(sting_l_all)) deallocate(sting_l_all)
Allocate(sting_l_all(nsl))
if(LE .ne.0) then
	if (allocated(lethk_all        )) deallocate(lethk_all        )
	if (allocated(tethk_all        )) deallocate(tethk_all        )
	if (allocated(s_all            )) deallocate(s_all            )
	if (allocated(ee_all           )) deallocate(ee_all           )
	if (allocated(C_le_x_top_all   )) deallocate(C_le_x_top_all   )
	if (allocated(C_le_x_bot_all   )) deallocate(C_le_x_bot_all   )
	if (allocated(C_le_y_top_all   )) deallocate(C_le_y_top_all   )
	if (allocated(C_le_y_bot_all   )) deallocate(C_le_y_bot_all   )
	if (allocated(LE_vertex_ang_all)) deallocate(LE_vertex_ang_all)
	if (allocated(LE_vertex_dis_all)) deallocate(LE_vertex_dis_all)
	if (allocated(sting_h_all      )) deallocate(sting_h_all      )
	Allocate(lethk_all(nsl))
	Allocate(tethk_all(nsl))
	Allocate(s_all(nsl))
	Allocate(ee_all(nsl))
	Allocate(C_le_x_top_all(nsl))
	Allocate(C_le_x_bot_all(nsl))
	Allocate(C_le_y_top_all(nsl))
	Allocate(C_le_y_bot_all(nsl))
	Allocate(LE_vertex_ang_all(nsl))
	Allocate(LE_vertex_dis_all(nsl))
	!Allocate(sting_l_all(nsl))
	Allocate(sting_h_all(nsl, 2))
	! 5 ... is the number of parameters to read
	read (11, *), temp
	read (11, *), temp
	read (11, *), LEdegree, no_LE_segments
	print*, 'LEdegree = ', LEdegree, 'no_LE_segments = ', no_LE_segments
	do i = 1, nsl
		read (11, *), temp
		read (11, *), temp
		read (11, *), lethk_all(i), tethk_all(i), s_all(i), ee_all(i), C_le_x_top_all(i), C_le_x_bot_all(i), &
		C_le_y_top_all(i), C_le_y_bot_all(i), LE_vertex_ang_all(i), LE_vertex_dis_all(i), &
		sting_l_all(i), sting_h_all(i, 1), sting_h_all(i, 2)
		! print*, 'C_le_y_top_all(i), C_le_y_bot_all(i), LE_vertex_ang_all(i), LE_vertex_dis_all(i)', C_le_y_top_all(i), C_le_y_bot_all(i), LE_vertex_ang_all(i), LE_vertex_dis_all(i)
	enddo
endif ! end if for LE spline parameters
close(11)
!endif ! end if for curvature, thickness, thickness dist and LE spline definition.

return
end subroutine readcontrolinput
!**********************************************************************************
!**********************************************************************************



!**********************************************************************************
!**********************************************************************************
! subroutine read_spanwise_input(file_name)
subroutine read_spanwise_input(row_type, path)
!Description(Syed Moez 03/02/2014):-------------------------------------------------------------------------
!This subroutine is used to used to read the input file called
!spancontrolinputs. It is triggered if any of the switches for LE, 
!thickness or curvature is activated and the word "spline" is typed
!after the curvature switch.
use globvar
implicit none
character*256 row_type
! character*(*) file_name
character*(*) path
character*256 file_name
!opening files to read inputs

real*8, allocatable, dimension(:) :: temp
integer jj

file_name = trim(path)//'spancontrolinputs.'//trim(row_type)//'.dat'
open(10, file = file_name)
rewind(10)

do i = 1, 5
	read(10, *)
end do
!--------------------------------------------------------------------------
!Reading curvature and chord control points
read(10, *) ncp_span_curv, ncp_chord
!number of chord and curvature control points will always be the same
ncp_curvature = ncp_chord
!Initializing values for variables defined by Ahmed
if (allocated(ncp_curv)) deallocate(ncp_curv)
Allocate(ncp_curv(nsl))
do i = 1, nsl
	ncp_curv(i) = ncp_curvature+2
end do

!Including phantom points												
ncp_span_curv1 = ncp_span_curv+2

ncp_chord_curv = ncp_chord-2+ncp_curvature-1+1

!Allocating arrays
if (allocated(cp_chord_curv)) deallocate(cp_chord_curv)
allocate(cp_chord_curv(ncp_span_curv, ncp_chord_curv))

!LINE 7
read(10, *)
do i = 1, ncp_span_curv
	read(10, *) cp_chord_curv(i, 1:ncp_chord_curv)
end do
allocate(temp(ncp_span_curv))

if (ncp_curvature >= 1) then
   jj = 1 + ncp_chord-2 + 2-1
   do i = 1, ncp_span_curv
      temp(i) = cp_chord_curv(i,jj)
   enddo
   call override_cur2(ncp_span_curv, temp)
   do i = 1, ncp_span_curv
      cp_chord_curv(i,jj) = temp(i)
   enddo
endif

if (ncp_curvature >= 2) then
   jj = 1 + ncp_chord-2 + 3-1
   do i = 1, ncp_span_curv
      temp(i) = cp_chord_curv(i,jj)
   enddo
   call override_cur3(ncp_span_curv, temp)
   do i = 1, ncp_span_curv
      cp_chord_curv(i,jj) = temp(i)
   enddo
endif

if (ncp_curvature >= 3) then
   jj = 1 + ncp_chord-2 + 4-1
   do i = 1, ncp_span_curv
      temp(i) = cp_chord_curv(i,jj)
   enddo
   call override_cur4(ncp_span_curv, temp)
   do i = 1, ncp_span_curv
      cp_chord_curv(i,jj) = temp(i)
   enddo
endif

if (ncp_curvature >= 4) then
   jj = 1 + ncp_chord-2 + 5-1
   do i = 1, ncp_span_curv
      temp(i) = cp_chord_curv(i,jj)
   enddo
   call override_cur5(ncp_span_curv, temp)
   do i = 1, ncp_span_curv
      cp_chord_curv(i,jj) = temp(i)
   enddo
endif

if (ncp_curvature >= 5) then
   jj = 1 + ncp_chord-2 + 6-1
   do i = 1, ncp_span_curv
      temp(i) = cp_chord_curv(i,jj)
   enddo
   call override_cur6(ncp_span_curv, temp)
   do i = 1, ncp_span_curv
      cp_chord_curv(i,jj) = temp(i)
   enddo
endif

if (ncp_curvature >= 6) then
   jj = 1 + ncp_chord-2 + 7-1
   do i = 1, ncp_span_curv
      temp(i) = cp_chord_curv(i,jj)
   enddo
   call override_cur7(ncp_span_curv, temp)
   do i = 1, ncp_span_curv
      cp_chord_curv(i,jj) = temp(i)
   enddo
endif

deallocate(temp)

if(thick .ne. 0 .or. LE .ne. 0) then
	!--------------------------------------------------------------------------
	!Reading thickness control points
	read(10, *)
	read(10, *)
	read(10, *)ncp_span_thk, ncp_chord_thickness
	!number of chord and thickness control points will always be the same
	ncp_thickness = ncp_chord_thickness
	ncp_span_thk1 = ncp_span_thk+2

	ncp_chord_thk = ncp_chord_thickness-2+ncp_thickness-2+1

	!Initializing values for variables defined by Ahmed
	if (allocated(ncp_thk)) deallocate(ncp_thk)
	Allocate(ncp_thk(nsl))
	do i = 1, nsl
		ncp_thk(i) = ncp_thickness+4
	end do

		if (allocated(cp_chord_thk)) deallocate(cp_chord_thk)
		allocate(cp_chord_thk(ncp_span_thk, ncp_chord_thk))


	read(10, *)
	print*, ncp_chord_thk, ncp_chord_thickness, ncp_thickness
	do i = 1, ncp_span_thk
		read(10, *)cp_chord_thk(i, 1:ncp_chord_thickness)
	end do
	if (LE .ne. 0) then
		!--------------------------------------------------------------------------
		!Reading LE control points
		do i = 1, 3
			read(10, *)  
		end do
		read(10, *)LE_deg, LE_seg
		read(10, *)
		read(10, *)ncp_span_LE

		ncp_LE = 13 !there are 13 different control points
		!Giving the same variable name as in controlinputs
		LEdegree = LE_deg
		no_LE_segments = LE_seg

		ncp_span_LE1 = ncp_span_LE+2

				if (allocated(cp_LE)) deallocate(cp_LE)
		
		allocate(cp_LE(ncp_span_LE, ncp_LE+1))


		read(10, *)
		do i = 1, ncp_span_LE
			read(10, *)cp_LE(i, 1:ncp_LE+1)
		end do
		close(10)
	endif
endif
!---------------------------------------------------------------------------

print*, 'spanwise input file read successfully'
!--------------------------------------------------------------------------------------------------
!This part of the program is used to check if the input data has been read correctly.
!It is not required and is therefore commented. Uncomment to create controlinupts_check.dat file 
!which should give the exact input file if this subroutine works correctly

!open(20, file = 'controlinupts_check.dat')
!write(20, *)'  span    u2     u3     u4     u5     u6    curv2  curv3  curv4  curv5  curv6  curv7'
!do i = 1, ncp_span_curv
!write(20, 10)cp_chord_curv(i, 1:ncp_chord_curv)
!end do

!write(20, *)'  span    thk2     thk3     thk4     thk5     thk6'
!do i = 1, ncp_span_thk
!write(20, 10)cp_thk(i, 1:ncp_thickness-1)
!end do

!write(20, *)
!do i = 1, ncp_span_LE
!write(20, 10)cp_LE(i, 1:ncp_LE+1)
!10 format(14f10.3)
!end do
!--------------------------------------------------------------------------------------------------

end subroutine read_spanwise_input
