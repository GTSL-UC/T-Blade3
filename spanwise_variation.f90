subroutine span_variation()
    !Description(Syed Moez 03/02/2014):-------------------------------------------------------------------------
!This subroutine is used to create a cubic bspline between variable
!taken from spancontrolinputs file and calls span_output() subroutine
!span_output selects the variables for all the sections by comparing
!its spanwise location and incorporates them into 3dbgb.
use file_operations
use globvar
implicit none

integer :: np_fine, i_local
real, allocatable, dimension(:) :: span_fine
real, allocatable, dimension(:, :) :: out_coord_u_fine, out_coord_v_fine
real :: out_coord_u(na, 12), out_coord_v(na, 12)
real :: intersec_u(nspan), intersec_v(nspan)
character*20 ind
integer ::  nopen
character(len = :), allocatable :: log_file
logical                         :: file_open

if (allocated(bspline_chord_curv)) deallocate(bspline_chord_curv)
if (allocated(bspline_thk       )) deallocate(bspline_thk       )
if (allocated(bspline_LE        )) deallocate(bspline_LE        )
allocate(bspline_chord_curv(nsl,ncp_chord_curv))
if (allocated(le_angle_all)) deallocate(le_angle_all)
Allocate(le_angle_all(na))
if (allocated(te_angle_all)) deallocate(te_angle_all)
Allocate(te_angle_all(na))
if (thick_distr .eq. 4 .or. thick_distr .eq. 5) then
	allocate(bspline_thk(nsl, 2*ncp_thickness+1))
else
	allocate(bspline_thk(nsl, ncp_chord_thk))
endif
allocate(bspline_LE(nsl,ncp_LE+1))
np_fine = 1000
if (allocated(span_fine)) deallocate(span_fine)
allocate(span_fine(np_fine))
if (allocated(out_coord_u_fine)) deallocate(out_coord_u_fine)
allocate(out_coord_u_fine(np_fine, 12))
if (allocated(out_coord_v_fine)) deallocate(out_coord_v_fine)
allocate(out_coord_v_fine(np_fine, 12))

!Calling cubicbspline subroutine to create Bspline by using the given data points
!The subroutine is called in a do loop and Bspline for each variable is created
call log_file_exists(log_file, nopen, file_open)
if (thick_distr .eq. 4) then
	print*, 'Creating spanwise thickness and TE angle distributions'
	print*, 'Creating cubic Bspline with spancontrolinput file for spanwise curvature'
    write(nopen,*) 'Creating spanwise thickness and TE angle distributions'
    write(nopen,*) 'Creating cubic Bspline with spancontrolinput file for spanwise curvature'
else
	print*, 'creating cubic bspline with spancontrolinput file'
    write(nopen,*) 'Creating cubic bspline with spancontrolinput file'
endif
call close_log_file(nopen, file_open)

do i = 1, np_fine
	span_fine(i) = (i-1.)/(np_fine-1)*(span(na) - span(1)) + span(1)
enddo
!creating cubic spline for curvature
!--------------------------------------------------------------------------------------------
do j=1,nsl
	bspline_chord_curv(j,1)=span(j)
end do

do i=2,ncp_chord_curv
	call cubicspline(cp_chord_curv(:,i),cp_chord_curv(:,1),ncp_span_curv,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
	call cubicbspline_intersec(y_spl_end,xc,yc,ncp1,span,intersec,na,xbs,ybs)
	do j=1,na
		bspline_chord_curv(j,i)=intersec(j)
	end do
end do

if(thick .ne. 0 .or. thick_distr .eq. 3) then
	!creating cubic spline for Thickness
	!-----------------------------------------------------------------------------------------------
	do j = 1,na
		bspline_thk(j,1) = span(j)
	end do

	do i=2,ncp_chord_thk
		call cubicspline(cp_chord_thk(:,i),cp_chord_thk(:,1),ncp_span_thk,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
		call cubicbspline_intersec(y_spl_end,xc,yc,ncp1,span,intersec,na,xbs,ybs)
		do j=1,na
			bspline_thk(j,i)=intersec(j)
		end do
	end do
elseif (thick_distr .eq. 4 .or. thick_distr .eq. 5) then
	do j = 1,na
		bspline_thk(j,1) = span(j)
	enddo
	do i = 1,ncp_thickness
		write(ind, '(i2)') i
		call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), cp_chord_thk(:,i+ncp_thickness+1), ncp_span_thk, span, na, &
                                       1, out_coord_v)
		intersec_v(1:na) = out_coord_v(:, 2)
		call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), cp_chord_thk(:,i+1), ncp_span_thk, span, na, 1, out_coord_u)
		intersec_u(1:na) = out_coord_u(:, 2)		
		open (unit = 81, file = 'thk_span_dist_v.' // trim(adjustl(ind)) // '.' // trim(casename) // '.dat')
		write (81, '(12F30.12)') (out_coord_v(k, :), k = 1, na)
		close (81)
		open (unit = 81, file = 'thk_span_dist_u.' // trim(adjustl(ind)) // '.' // trim(casename) // '.dat')
		write (81, '(12F30.12)') (out_coord_u(k, :), k = 1, na)
		close (81)		
		if (isdev) then
			call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), cp_chord_thk(:,i+1), ncp_span_thk, span_fine, np_fine, &
                                           1, out_coord_u_fine)		
			open (unit = 81, file = 'thk_span_dist_u_fine.' // trim(adjustl(ind)) // '.' // trim(casename) // '.dat')
			write (81, '(12F30.12)') (out_coord_u_fine(k, :), k = 1, np_fine)
			close (81)
			call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), cp_chord_thk(:,i+ncp_thickness+1), ncp_span_thk,       &
                                           span_fine, np_fine, 1, out_coord_v_fine)		
			open (unit = 81, file = 'thk_span_dist_v_fine.' // trim(adjustl(ind)) // '.' // trim(casename) // '.dat')
			write (81, '(12F30.12)') (out_coord_v_fine(k, :), k = 1, np_fine)
			close (81)			
		endif
		! do j = 2, ncp_span_thk
			! if (cp_chord_thk(j, i+1) /= cp_chord_thk(1, i+1)) then
				! print*, 'span_variation error: u coordinates of control points must be constant spanwise for this thickness option'
				! return
			! endif
		! enddo
		do j = 1, na
			bspline_thk(j, 2*i) = intersec_u(j)
			bspline_thk(j, 2*i+1) = intersec_v(j)
		enddo
	enddo
	call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), te_angle_cp, ncp_span_thk, span, na, 1, out_coord_v)
	te_angle_all(1:na) = out_coord_v(:, 2)
	open (unit = 81, file = 'te_angle_span_dist.' // trim(casename) // '.dat')
	write (81, '(12F30.12)') (out_coord_v(k, :), k = 1, na)
	close (81)
	if (isdev) then
		call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), te_angle_cp, ncp_span_thk, span_fine, np_fine, 1, out_coord_v_fine)	
		open (unit = 81, file = 'te_angle_span_dist_fine.' // trim(casename) // '.dat')
		write (81, '(12F30.12)') (out_coord_v_fine(k, :), k = 1, np_fine)
		close (81)
	endif	
	call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), le_angle_cp, ncp_span_thk, span, na, 1, out_coord_v)	
	le_angle_all(1:na) = out_coord_v(:, 2)
	open (unit = 81, file = 'le_angle_span_dist.' // trim(casename) // '.dat')
	write (81, '(12F30.12)') (out_coord_v(k, :), k = 1, na)
	close (81)	
	if (isdev) then
		call thk_ctrl_gen_driver_span (isdev, cp_chord_thk(:,1), le_angle_cp, ncp_span_thk, span_fine, np_fine, 1, out_coord_v_fine)
		open (unit = 81, file = 'le_angle_span_dist_fine.' // trim(casename) // '.dat')
		write (81, '(12F30.12)') (out_coord_v_fine(k, :), k = 1, np_fine)
		close (81)
	endif
endif
if(LE .ne. 0) then
	!creating cubic spline for LE
	!------------------------------------------------------------------------------------------------
	do j=1,na
		bspline_LE(j,1)=span(j)
	end do

	do i=2,ncp_LE
		call cubicspline(cp_LE(:,i),cp_LE(:,1),ncp_span_LE,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
		call cubicbspline_intersec(y_spl_end,xc,yc,ncp1,span,intersec,na,xbs,ybs)
		do j=1,na
			bspline_LE(j,i)=intersec(j)
		end do
	end do
endif

call log_file_exists(log_file, nopen, file_open)
if (thick_distr .eq. 4) then
	print*, 'Spanwise curvature Bspline created successfully'
	print*, 'Spanwise thickness and TE angle distributions created successfully:'
	print*, 'Files prefixed with thk_span_dist and te_angle_span_dist created'
    write(nopen,*) 'Spanwise curvature Bspline created successfully'
    write(nopen,*) 'Spanwise thickness and TE angle distributions created successfully:'
    write(nopen,*) 'Files prefixed with thk_span_dist and te_angle_span_dist created'
else
	print*, 'bspline created successfully'
    write(nopen,*) 'bspline created successfully'
endif
call close_log_file(nopen, file_open)

!calling subroutine to calculate the location of sections
!------------------------------------------------------------------------------------------------
!Adjusting new input to be integrated with 3dbgb
	call span_output()
	
!############################################
!This part of the program writes down the data points for the Bspline created using
!the control points
!-------------------------------------------------------------------------------------------------
!open(50,file='bsline_curv_check.dat')
!do i=1,na
!	write(50,100)bspline_chord_curv(i,1:ncp_chord_curv)
!end do
! if (thick_distr .eq. 4) then
	! open(60,file='bsline_thk_check.dat')
	! do i=1,na
		! write(60,100)bspline_thk(i,1:2*ncp_thickness+1)
	! end do
! else
	! open(60,file='bsline_thk_check.dat')
	! do i=1,na
		! write(60,100)bspline_thk(i,1:ncp_chord_thk)
	! end do
! endif

!open(32,file='bspline_LE.dat')
!do i=1,na
!    write(32,100)bspline_LE(i,1:ncp_LE)
!end do
100 format(14f15.8)
!------------------------------------------------------------------------------------------------
!############################################

end subroutine span_variation
