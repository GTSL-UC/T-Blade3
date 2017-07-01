subroutine span_variation()
!Description(Syed Moez 03/02/2014):-------------------------------------------------------------------------
!This subroutine is used to create a cubic bspline between variable
!taken from spancontrolinputs file and calls span_output() subroutine
!span_output selects the variables for all the sections by comparing
!its spanwise location and incorporates them into 3dbgb.

use globvar
implicit none

if (allocated(bspline_chord_curv)) deallocate(bspline_chord_curv)
if (allocated(bspline_thk       )) deallocate(bspline_thk       )
if (allocated(bspline_LE        )) deallocate(bspline_LE        )
allocate(bspline_chord_curv(nsl,ncp_chord_curv))
allocate(bspline_thk(nsl,ncp_chord_thk))
allocate(bspline_LE(nsl,ncp_LE+1))

!Calling cubicbspline subroutine to create Bspline by using the given data points
!The subroutine is called in a do loop and Bspline for each variable is created
print*, 'creating cubic bspline with spancontrolinput file'


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

if(thick .ne. 0) then
	!creating cubic spline for Thickness
	!-----------------------------------------------------------------------------------------------
	do j=1,na
		bspline_thk(j,1)=span(j)
	end do

	do i=2,ncp_chord_thk
		call cubicspline(cp_chord_thk(:,i),cp_chord_thk(:,1),ncp_span_thk,xbs,ybs,y_spl_end,nspline,xc,yc,ncp1)
		call cubicbspline_intersec(y_spl_end,xc,yc,ncp1,span,intersec,na,xbs,ybs)
		do j=1,na
			bspline_thk(j,i)=intersec(j)
		end do
	end do
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

print*, 'bspline created successfully'

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

!open(60,file='bsline_thk_check.dat')
!do i=1,na
!	write(60,100)bspline_thk(i,1:ncp_chord_thk)
!end do

!open(32,file='bspline_LE.dat')
!do i=1,na
!    write(32,100)bspline_LE(i,1:ncp_LE)
!end do
!100 format(14f15.8)
!------------------------------------------------------------------------------------------------
!############################################

end subroutine span_variation
