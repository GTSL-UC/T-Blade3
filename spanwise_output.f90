subroutine span_output ()
!Description(Syed Moez 03/02/2014):---------------------------------
!This subroutine arranges the values of the control points 
!obtained by creating a cubic B-spline into the specific  
!format for curv_cp, thk_cp and LE variables 
!These variables are being used for calculation by bladegen in creating airfoil sections.

use globvar
implicit none

if (allocated(curv_cp)) deallocate(curv_cp)
Allocate(curv_cp(20,2*na))

k=1
do i=1,na
  if (k<=na)then
    !Defining Control points for curvature chord values
	!Defining fixed control points (not varied in new input file)
	curv_cp(2,2*k-1)=0.0
	curv_cp(ncp_chord+1,2*k-1)=1.0
	
	!defining Cubic B-splined control points from the new input file
	do j=3,ncp_chord
		curv_cp(j,2*k-1)=bspline_chord_curv(i,j-1)
	end do
	
	!Adding two phantom points required for cubic Bspline
	curv_cp(1,2*k-1)=2*curv_cp(2,2*k-1)-curv_cp(3,2*k-1)
	curv_cp(ncp_chord+2,2*k-1)=2*curv_cp(ncp_chord+1,2*k-1)-curv_cp(ncp_chord,2*k-1)
	
	!Defining control points for curvature
	!Defining fixed control points (not varied in new input file)
	curv_cp(2,2*k)=0.0
	
	!defining Cubic B-splined control points from the new input file	
	do j=3,ncp_curvature+1
	curv_cp(j,2*k)=bspline_chord_curv(i,ncp_chord+j-3)
	end do
	
	!Adding two phantom points required for cubic Bspline
	curv_cp(1,2*k)=2*curv_cp(2,2*k)-curv_cp(3,2*k)
	curv_cp(ncp_curvature+2,2*k)=2*curv_cp(ncp_curvature+1,2*k)-curv_cp(ncp_curvature,2*k)
	
	k=k+1
  end if
end do

if(thick .ne. 0) then
!-------------------------------------------------------------------------------
!writing Thickness control points
 if (allocated(thk_cp)) deallocate(thk_cp)
 Allocate(thk_cp(20,2*na))
k=1
do i=1,na
  if (k<=na)then
	!Defining Control points for thickness chord values
	!Defining fixed control points (not varied in new input file)
	thk_cp(3,2*k-1)=0.0
	thk_cp(ncp_chord_thickness+2,2*k-1)=1.0
	
	!Defining Cubic B-splined control points from the new input file	
	do j=4,ncp_chord_thickness+1
	thk_cp(j,2*k-1)=bspline_thk(i,j-2)
	end do
	
	!Adding phantom points
	thk_cp(1,2*k-1)=2*thk_cp(3,2*k-1)-thk_cp(5,2*k-1)
	thk_cp(2,2*k-1)=2*thk_cp(3,2*k-1)-thk_cp(4,2*k-1)
	thk_cp(ncp_chord_thickness+4,2*k-1)=2*thk_cp(ncp_chord_thickness+2,2*k-1)-thk_cp(ncp_chord_thickness,2*k-1)
	thk_cp(ncp_chord_thickness+3,2*k-1)=2*thk_cp(ncp_chord_thickness+2,2*k-1)-thk_cp(ncp_chord_thickness+1,2*k-1)

	!Defining control points for Thickness
	!Defining fixed control points (not varied in new input file)
	thk_cp(3,2*k)=0.0
	thk_cp(ncp_thickness+2,2*k)=0.0
	
	!Defining Cubic B-splined control points from the new input file
	do j=4,ncp_thickness+1
	thk_cp(j,2*k)=bspline_thk(i,ncp_chord_thickness+j-4)
	end do
	
	!Adding four phantom points required for creating a quartic spline
	thk_cp(1,2*k)=2*thk_cp(3,2*k)-thk_cp(5,2*k)
	thk_cp(2,2*k)=2*thk_cp(3,2*k)-thk_cp(4,2*k)
	thk_cp(ncp_thickness+4,2*k)=2*thk_cp(ncp_thickness+2,2*k)-thk_cp(ncp_thickness,2*k)
	thk_cp(ncp_thickness+3,2*k)=2*thk_cp(ncp_thickness+2,2*k)-thk_cp(ncp_thickness+1,2*k)
	
	k=k+1

  end if
end do
endif

if(LE .ne. 0) then
!-----------------------------------------------------------------------------
!Defining LE ontrol points
if (allocated(sting_l_all)) deallocate(sting_l_all)
Allocate(sting_l_all(na))
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
    Allocate(lethk_all(na))
    Allocate(tethk_all(na))
    Allocate(s_all(na))
    Allocate(ee_all(na))
	Allocate(C_le_x_top_all(na))
	Allocate(C_le_x_bot_all(na))
    Allocate(C_le_y_top_all(na))
	Allocate(C_le_y_bot_all(na))
 	Allocate(LE_vertex_ang_all(na))
    Allocate(LE_vertex_dis_all(na))
	!Allocate(sting_l_all(na))
	Allocate(sting_h_all(na,2))
	
	k=1
	do i=1,na
		if (k<=na)then
			lethk_all(k)=bspline_LE(i,2)
			tethk_all(k)=bspline_LE(i,3)
			s_all(k)=bspline_LE(i,4)
			ee_all(k)=bspline_LE(i,5)
			C_le_x_top_all(k)=bspline_LE(i,6)
			C_le_x_bot_all(k)=bspline_LE(i,7)
			C_le_y_top_all(k)=bspline_LE(i,8)
			C_le_y_bot_all(k)=bspline_LE(i,9)
			LE_vertex_ang_all(k)=bspline_LE(i,10)
			LE_vertex_dis_all(k)=bspline_LE(i,11)
			sting_l_all(k)=bspline_LE(i,12)
			sting_h_all(k,1)=bspline_LE(i,13)
			sting_h_all(k,2)=bspline_LE(i,14)
		
			k=k+1
		end if
	end do
end if
endif
!############################################
!This part of the program writes down the data points for the Bspline created using
!the control points
!open(150,file='curv_cp.dat')
!	do i=1,20
!		write(150,200)curv_cp(i,1:na*2)
!	end do
!	
!open(160,file='thk_cp.dat')
!	do i=1,20
!		write(160,200)thk_cp(i,1:na*2)
!	end do	
!200 format(22f15.2)

end subroutine span_output
