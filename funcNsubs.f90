!List of functions and subroutines used in 3DBGB code.
!--------------------------Kiran Siddappaji

!---------------------------------------------------------
! Subroutine to display the welcome message 
!---------------------------------------------------------
subroutine displayMessage
    use file_operations


character(len = :), allocatable :: log_file
integer                         :: nopen
logical                         :: file_open, initial

write(*,*)
write(*,*)'************************************************************'
write(*,*)'************************************************************'
write(*,*)'****  T-BLADE3:Turbomachinery BLADE 3D Geometry Builder ****'
write(*,*)'****                                                    ****' 
write(*,*)'****  Master Version 1.12	                           ****' 
write(*,*)'****                                                    ****'
write(*,*)'****  ...was also called as below till Aug 2016...      ****' 
write(*,*)'****  3DBGB: 3 Dimensional Blade Geometry Builder       ****'
write(*,*)'****                                                    ****'  
write(*,*)'****  Develop Version 1.12.9                            ****' 
write(*,*)'****                                                    ****'
write(*,*)'****  This software comes with ABSOLUTELY NO WARRANTY   ****'
write(*,*)'****                                                    ****'
write(*,*)'****  This is a program which generates a 3D blade...   ****' 
write(*,*)'****  ...shape and outputs 3D blade section files.      ****'
write(*,*)'****                                                    ****'
write(*,*)'****  Inputs: LE and TE curve(x,r), inlet angle,        ****' 
write(*,*)'****          exit angle, chord, tm/c, incidence,       ****'
write(*,*)'****          deviation, secondary flow angles,         ****'
write(*,*)'****          streamline coordinates:(x,r)              ****'   
write(*,*)'****          control points for sweep, lean,           ****'
write(*,*)'****          blade scaling factor.                     ****'
write(*,*)'****                                                    ****'
write(*,*)'****  Outputs: 3D blade sections (x,y,z),               ****'
write(*,*)'****           2D airfoils (mprime,theta).              ****'
write(*,*)'****                                                    ****'
write(*,*)'****  ---------------by Kiran Siddappaji         ----   ****'
write(*,*)'****  ---------------by Mark G. Turner           ----   ****'
write(*,*)'****  ------------------- turnermr@ucmail.uc.edu ----   ****'
write(*,*)'****  ---------------by Karthik Balasubramanian  ----   ****'
write(*,*)'****  ---------------by Syed Moez Hussain Mahmood----   ****'
write(*,*)'****  ---------------by Ahmed Nemnem             ----   ****'
write(*,*)'****  ---------------by Marshall C. Galbraith    ----   ****'
write(*,*)'************************************************************'
write(*,*)'************************************************************'
write(*,*)
write(*,*) 'T-Blade3 Copyright (C) 2017 University of Cincinnati, developed by Kiran Siddappaji,' 
write(*,*) 'Dr. Mark G. Turner, Karthik  Balasubramanian, Syed Moez Hussain, Ahmed Farid Nemnem '
write(*,*) ' and Marshall C. Galbraith.'
write(*,*)
write(*,*) 'This program is free software; you can redistribute it and/or modify it under the '
write(*,*) 'terms of the GNU General Public License as published by the Free Software Foundation; '
write(*,*) 'either version 2 of the License, or (at your option) any later version.'
write(*,*)
write(*,*) 'This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;'
write(*,*) 'without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. '
write(*,*) 'See the GNU General Public License for more details.  For the complete terms of the '
write(*,*) 'GNU General Public License, please see this URL: http://www.gnu.org/licenses/gpl-2.0.html'
write(*,*)
write(*,*)'************************************************************'
write(*,*)

! Check if log file exists or not
! Write screen output to the log file
initial = .true.
call log_file_exists(log_file, nopen, file_open, initial)

write(nopen,*)
write(nopen,*)'************************************************************'
write(nopen,*)'************************************************************'
write(nopen,*)'****  T-BLADE3:Turbomachinery BLADE 3D Geometry Builder ****'
write(nopen,*)'****                                                    ****' 
write(nopen,*)'****  Master Version 1.12                               ****' 
write(nopen,*)'****                                                    ****'
write(nopen,*)'****  ...was also called as below till Aug 2016...      ****' 
write(nopen,*)'****  3DBGB: 3 Dimensional Blade Geometry Builder       ****'
write(nopen,*)'****                                                    ****'  
write(nopen,*)'****  Develop Version 1.12.9                            ****' 
write(nopen,*)'****                                                    ****'
write(nopen,*)'****  This software comes with ABSOLUTELY NO WARRANTY   ****'
write(nopen,*)'****                                                    ****'
write(nopen,*)'****  This is a program which generates a 3D blade...   ****' 
write(nopen,*)'****  ...shape and outputs 3D blade section files.      ****'
write(nopen,*)'****                                                    ****'
write(nopen,*)'****  Inputs: LE and TE curve(x,r), inlet angle,        ****' 
write(nopen,*)'****          exit angle, chord, tm/c, incidence,       ****'
write(nopen,*)'****          deviation, secondary flow angles,         ****'
write(nopen,*)'****          streamline coordinates:(x,r)              ****'   
write(nopen,*)'****          control points for sweep, lean,           ****'
write(nopen,*)'****          blade scaling factor.                     ****'
write(nopen,*)'****                                                    ****'
write(nopen,*)'****  Outputs: 3D blade sections (x,y,z),               ****'
write(nopen,*)'****           2D airfoils (mprime,theta).              ****'
write(nopen,*)'****                                                    ****'
write(nopen,*)'****  ---------------by Kiran Siddappaji         ----   ****'
write(nopen,*)'****  ---------------by Mark G. Turner           ----   ****'
write(nopen,*)'****  ------------------- turnermr@ucmail.uc.edu ----   ****'
write(nopen,*)'****  ---------------by Karthik Balasubramanian  ----   ****'
write(nopen,*)'****  ---------------by Syed Moez Hussain Mahmood----   ****'
write(nopen,*)'****  ---------------by Ahmed Nemnem             ----   ****'
write(nopen,*)'****  ---------------by Marshall C. Galbraith    ----   ****'
write(nopen,*)'************************************************************'
write(nopen,*)'************************************************************'
write(nopen,*)
write(nopen,*) 'T-Blade3 Copyright (C) 2017 University of Cincinnati, developed by Kiran Siddappaji,' 
write(nopen,*) 'Dr. Mark G. Turner, Karthik  Balasubramanian, Syed Moez Hussain, Ahmed Farid Nemnem '
write(nopen,*) ' and Marshall C. Galbraith.'
write(nopen,*)
write(nopen,*) 'This program is free software; you can redistribute it and/or modify it under the '
write(nopen,*) 'terms of the GNU General Public License as published by the Free Software Foundation; '
write(nopen,*) 'either version 2 of the License, or (at your option) any later version.'
write(nopen,*)
write(nopen,*) 'This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;'
write(nopen,*) 'without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. '
write(nopen,*) 'See the GNU General Public License for more details.  For the complete terms of the '
write(nopen,*) 'GNU General Public License, please see this URL: http://www.gnu.org/licenses/gpl-2.0.html'
write(nopen,*)
write(nopen,*)'************************************************************'
write(nopen,*)

! Close the log file if it is open
call close_log_file(nopen, file_open)

return
end subroutine displayMessage
!---------------------------------------------------------

!---------------------------------------------------------
!function to calculate the inlet angle including incidence
!----------------------------------------------------------
real*8 function inBetaInci(inbeta,inci)
implicit none
real*8 inbeta(1),inci(1)

!Positive inlet angle
if(inbeta(1).ge.0.)then
  inBetaInci = inbeta(1) - inci(1)
else
  !Negative inlet angle
  inBetaInci = inbeta(1) + inci(1) 
endif
!print*,inci(1),inBetaInci(1)

end function 
!----------------------------------------------------------

!----------------------------------------------------------
!function to calculate the exit angle including deviation
!----------------------------------------------------------
real*8 function outBetaDevn(inbeta,outbeta,devn)
implicit none
real*8 inbeta(1),outbeta(1),camber(1), devn(1)

!calculating camber 
camber(1) = outbeta(1) - inbeta(1)

!negative camber
if(camber(1).le.0.)then
  outBetaDevn = outbeta(1) - devn(1)
else
  !positive camber
  outBetaDevn = outbeta(1) + devn(1)
endif
!print*,devn(1),outbeta(1)
end function
!----------------------------------------------------------

!--------------------------------------------------------------
!subroutine to write the dimensional hub and casing streamlines
!--------------------------------------------------------------
subroutine hubTipStreamline(xhub,rhub,nphub,xtip,rtip,nptip,nsl,scf,casename)
use file_operations
implicit none

integer i,nphub,nptip,nsl,nopen
real*8 xhub(nphub,1),rhub(nphub,1),xtip(nptip,1),rtip(nptip,1)
real*8 scf
character*80 fname1,fname2
character*32 casename
character(len = :), allocatable :: log_file
logical                         :: file_open


call log_file_Exists(log_file, nopen, file_open)
print*,' Writing dimensional hub and casing streamlines...'
write(nopen,*) 'Writing dimensional hub and casing streamlines...'
call close_log_file(nopen, file_open)
fname1 = 'hub.'//trim(casename)//'.sldcrv'
open(1,file = fname1,status ='unknown')
do i = 1,nphub
   write(1,*)scf*xhub(i,1),0.0,scf*rhub(i,1)
enddo
close(1)

! Casing streamline
fname2 = 'casing.'//trim(casename)//'.sldcrv'
open(2,file = fname2,status ='unknown')
do i = 1,nptip
   write(2,*)scf*xtip(i,1),0.0,scf*rtip(i,1)
enddo
close(2)

return 
end subroutine
!--------------------------------------------------------------

!--------------------------------------------------------------
!subroutine to write the dimensional streamlines (without hub or casing streamlines)nemnem
!--------------------------------------------------------------
subroutine streamlines(xml,rml,np,scf,casename,ia)
use file_operations
implicit none

integer i,ia,np, nopen
real*8 xml(np),rml(np)
real*8 scf
character*80 fname
character*32 casename,temp
character(len = :), allocatable :: log_file
logical                         :: file_open

write(temp,*)ia
call log_file_exists(log_file, nopen, file_open)
print*,' Writing dimensional streamline'//trim(adjustl(temp))//'...'
write(nopen,*) 'Writing dimensional streamline'//trim(adjustl(temp))//'...'
call close_log_file(nopen, file_open)
fname = 'streamlines'//trim(adjustl(temp))//'.'//trim(casename)//'.sldcrv'
open(1,file = fname,status ='unknown')
	do i = 1,np
	   write(1,*)scf*xml(i),0.0,scf*rml(i)
	enddo
close(1)

return 
end subroutine
!--------------------------------------------------------------

!--------------------------------------------------------------
! subroutine calculating hub offset
!--------------------------------------------------------------
subroutine huboffset(mphub,x,r,dxds,drds,hub,nphub,scf,casename)
use file_operations
implicit none

integer i,nphub, nopen

real*8, intent(out):: mphub(nphub,1)
real*8 xhub(nphub,1),rhub(nphub,1),xms_hub(nphub,1),rms_hub(nphub,1)
real*8 x(nphub,1),r(nphub,1),dxds(nphub,1),drds(nphub,1),hub,scf,deltan
real*8 b,xnorm(nphub,1),rnorm(nphub,1),dxn(nphub,1),drn(nphub,1)

character*80 fname1, fname2
character*32 casename

character(len = :), allocatable :: log_file
logical                         :: file_open

!Calcultating the normal and offset coordinates
do i = 1, nphub   
   !print*,dxds(i,1)
   xnorm(i,1) = drds(i,1)
   rnorm(i,1) = -dxds(i,1)
   ! xnorm(1,1) = -drds(i,1)
   ! rnorm(1,1) = dxds(i,1)
   !print*,'xnorm(i,1) rnorm(i,1)', xnorm(i,1), rnorm(i,1)
   deltan = hub*1.
   b = deltan/((xnorm(i,1)**2 + rnorm(i,1)**2)**0.5)
   dxn(i,1) = b*(xnorm(i,1))
   drn(i,1) = b*(rnorm(i,1))
   !offset hub coordinates
   xhub(i,1) = x(i,1) + dxn(i,1)
   rhub(i,1) = r(i,1) + drn(i,1)
enddo  
write(*,*)

!writing the offset coordinates to a file
fname1 = 'hub-offset.'//trim(casename)//'.dat'
open(1,file = fname1, status = 'unknown')
call log_file_exists(log_file, nopen, file_open)
write(*,*)'Calculating hub-offset streamline coordinates...'
write(*,*)'Writing the coordinates to hub-offset.dat file'

write(nopen,*) 'Calculating hub-offset streamline coordinates...'
write(nopen,*) 'Writing the coordinates to hub-offset.dat file'
call close_log_file(nopen, file_open)

!writing the dimensionless offset coordinates to a file
fname1 = 'hub-offset-dimlss.'//trim(casename)//'.dat'
open(2,file = fname1, status = 'unknown')

!Calculating the mprime coordinates of the offset streamline/construction line.
mphub(1,1)= 0.
write(2,*)xhub(1,1),rhub(1,1)
!mp(1,1) = mphub(1,1) ! replacing the old streamline m' to the offset ones
do i = 2, nphub
	!print*,'abs(rhub(i,1)+rhub(i-1,1))',abs(rhub(i,1)+rhub(i-1,1))
	! bug: when radius of streamline is ~= 0 ... Nemnem 6/3/2014
   ! if(rhub(i,1) < radius_tolerance.and.(xhub(i,1)-xhub(i-1,1)) /= 0.0) then
	  ! print*,'Small streamline radius exception used ... (huboffset subroutine)'
      ! mphub(i,1) = mphub(i-1,1) + 2.*(xhub(i,1) - xhub(i-1,1))/(rhub(i,1)+rhub(i-1,1))
   ! else if ((xhub(i,1)-xhub(i-1,1)) == 0.0) then
	  ! mphub(i,1) = mphub(i-1,1)
   ! else
      ! mphub(i,1) = mphub(i-1,1) + 2.*sqrt((rhub(i,1)-rhub(i-1,1))**2+(xhub(i,1)-xhub(i-1,1))**2)/(rhub(i,1)+rhub(i-1,1)) 
   ! endif
   mphub(i,1) = mphub(i-1,1) + 2.*sqrt((rhub(i,1)-rhub(i-1,1))**2+(xhub(i,1)-xhub(i-1,1))**2)/(rhub(i,1)+rhub(i-1,1)) 
   write(1,*)scf*xhub(i,1),0.0,scf*rhub(i,1)
   write(2,*)xhub(i,1),rhub(i,1)
   !mp(i,1) = mphub(i,1)	  ! replacing the old streamline m' to the offset ones
enddo

! Splining the offset xhub, rhub:
call spline(xhub(1,1),xms_hub(1,1),mphub(1,1),nphub, 999.0, -999.0)
call spline(rhub(1,1),rms_hub(1,1),mphub(1,1),nphub, 999.0, -999.0)
! over writing the xm, rm values with hub spline coefficients:
x = xhub
r = rhub
dxds = xms_hub
drds = rms_hub
! print*,'xms_hub',x
! print*,'rms_hub',r

close(2)
close(1)

return
end subroutine
!--------------------------------------------------------------

!--------------------------------------------------------------
! subroutine calculating tip offset
!--------------------------------------------------------------
subroutine tipoffset(mptip,x,r,dxds,drds,tip,nptip,scf,nsl,casename)
use file_operations
implicit none

integer i,nptip,nsl
integer :: nopen
real*8,intent(out) :: mptip(nptip,1)
real*8 xtip(nptip,1),rtip(nptip,1),xms_tip(nptip,1),rms_tip(nptip,1)
real*8 x(nptip,1),r(nptip,1),dxds(nptip,1),drds(nptip,1),tip,scf
real*8 b,xnorm(nptip,1),rnorm(nptip,1),dxn(nptip,1),drn(nptip,1),deltan

character*80 fname1, fname2
character*32 casename
character(len = :), allocatable :: log_file
logical                         :: file_open

!Calcultating the normal and offset coordinates

do i = 1, nptip   
   !print*,dxds(i,1)
   xnorm(i,1) = drds(i,1)
   rnorm(i,1) = -dxds(i,1)
   !print*, xnorm(i,1),rnorm(i,1)
   deltan = tip*1.
   b = deltan/((xnorm(i,1)**2 + rnorm(i,1)**2)**0.5)
   dxn(i,1) = b*(xnorm(i,1))
   drn(i,1) = b*(rnorm(i,1))
   !offset tip coordinates
   xtip(i,1) = x(i,1) - dxn(i,1)
   rtip(i,1) = r(i,1) - drn(i,1)
enddo  
write(*,*)

!writing the offset coordinates to a file
fname1 = 'tip-offset.'//trim(casename)//'.dat'
open(1,file = fname1, status = 'unknown')
call log_file_exists(log_file, nopen, file_open)
write(*,*)'Calculating tip-offset streamline coordinates...'
write(*,*)'Writing the coordinates to tip-offset.dat file'

write(nopen,*) 'Calculating tip-offset streamline coordinates...'
write(nopen,*) 'Writing the coordinates to tip-offset.dat file'
call close_log_file(nopen, file_open)

!writing the dimenstionless offset coordinates to a file
fname1 = 'tip-offset-dimlss.'//trim(casename)//'.dat'
open(2,file = fname1, status = 'unknown')
write(2,*)xtip(1,1),rtip(1,1)

!Calculating the mprime coordinates of the offset streamline/construction line.
mptip(1,1)= 0.
!mp(1,nsl) = mptip(1,nsl) ! replacing the old streamline m' to the offset ones
do i = 2, nptip
   ! if(rtip(i,1) < radius_tolerance.and.((xtip(i,1)-xtip(i-1,1)) /= 0)) then
      ! mptip(i,1) = mptip(i-1,1) + 2.*(xtip(i,1) - xtip(i-1,1))/(rtip(i,1)+rtip(i-1,1))
   ! else if ((xtip(i,1)-xtip(i-1,1)) == 0.0) then
	  ! mptip(i,1) = mptip(i-1,1)
   ! else
      ! mptip(i,1) = mptip(i-1,1) + 2.*sqrt((rtip(i,1)-rtip(i-1,1))**2+(xtip(i,1)-xtip(i-1,1))**2)/(rtip(i,1)+rtip(i-1,1)) 
   ! endif
   mptip(i,1) = mptip(i-1,1) + 2.*sqrt((rtip(i,1)-rtip(i-1,1))**2+(xtip(i,1)-xtip(i-1,1))**2)/(rtip(i,1)+rtip(i-1,1)) 
   write(1,*)scf*xtip(i,1),0.0,scf*rtip(i,1)
   write(2,*)xtip(i,1),rtip(i,1)
   !mp(i,nsl) = mptip(i,nsl)	 ! replacing the old streamline m' to the offset ones 
   !print*,mptip(i,1)   
enddo

! Splining the offset xtip, rtip:
call spline(xtip(1,1),xms_tip(1,1),mptip(1,1),nptip, 999.0, -999.0)
call spline(rtip(1,1),rms_tip(1,1),mptip(1,1),nptip, 999.0, -999.0)
! overwriting xm, rm with new tip spline coefficients:
x = xtip
r = rtip
dxds = xms_tip
drds = rms_tip
! print*,'xms_tip',x
! print*,'rms_tip',r
   
close(2)
close(1)

return
end subroutine
!--------------------------------------------------------------

!--------------------------------------------------------------
!-----INTERSECTION_POINTS--------------------------------------
!--------------------------------------------------------------
SUBROUTINE INTERP(XA,YA,XB,YB,XC,YC,XD,YD,XINT,YINT)
	  
real*8 XA,YA,XB,YB,XC,YC,XD,YD,XINT,YINT
	  
XINT=(XA*(YB-YA)+YC*(XB-XA)-((YD-YC)/(XD-XC))*XC*(XB-XA)-YA*(XB-XA))/((YB-YA)-(((YD-YC)*(XB-XA))/(XD-XC)))
YINT = YC+((XINT-XC)/(XD-XC))*(YD-YC)
	  
RETURN
END subroutine

!*******************************************************

!*******************************************************
!*******************************************************
subroutine outputfiledata(bladedata,nsl,amount_data,throat_pos,casename,units)
integer nsl,amount_data,js
real*8 bladedata(amount_data,nsl)
real*8 in_beta(nsl),out_beta(nsl)
character*20 throat_pos(nsl)
character*32 casename
character*80 file1
character(len=2) :: units

file1 = 'blade_section_data.'//trim(casename)//'.dat'
open(unit= 100,file=file1, form="formatted")
write(100,*)trim(casename)
write(100,*)'Blade sections Data:'
write(100,*)'---------------------'
write(100,*)  
do js = 1,nsl
   if (js == 1) then
	if (units == 'mm') then
	  if (throat_pos(nsl) == 'le') then
	    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                      'chord [mm]','sweep','lean','area[mm^2]','lethk [mm]','tethk [mm]','throat [mm]', &
                      '(r*delta_theta)LE','Geom Zweifel','le/te/btween/none'
	  elseif (throat_pos(nsl) == 'te') then
	    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                      'chord [mm]','sweep','lean','area[mm^2]','lethk [mm]','tethk [mm]','throat [mm]', &
                      '(r*delta_theta)TE','Geom Zweifel','le/te/btween/none'
	  else
	    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                      'chord [mm]','sweep','lean','area[mm^2]','lethk [mm]','tethk [mm]','throat [mm]', &
                      '(r*delta_theta)WT','Geom Zweifel','le/te/btween/none'
      endif
	elseif (units == 'cm') then
	  if (throat_pos(nsl) == 'le') then
	    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                      'chord [cm]','sweep','lean','area[cm^2]','lethk [cm]','tethk [cm]','throat [cm]', &
                      '(r*delta_theta)LE','Geom Zweifel','le/te/btween/none'
	  elseif (throat_pos(nsl) == 'te') then
	    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                      'chord [cm]','sweep','lean','area[cm^2]','lethk [cm]','tethk [cm]','throat [cm]', &
                      '(r*delta_theta)TE','Geom Zweifel','le/te/btween/none'
	  else
	    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                      'chord [cm]','sweep','lean','area[cm^2]','lethk [cm]','tethk [cm]','throat [cm]', &
                      '(r*delta_theta)WT','Geom Zweifel','le/te/btween/none'
      endif
    elseif ((units == 'm ').or.(units == 'm)')) then
	  if (throat_pos(nsl) == 'le') then
	    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                      'chord [ m]','sweep','lean','area[ m^2]','lethk [ m]','tethk [ m]','throat [ m]', &
                      '(r*delta_theta)LE','Geom Zweifel','le/te/btween/none'
	  elseif (throat_pos(nsl) == 'te') then
	    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                      'chord [ m]','sweep','lean','area[ m^2]','lethk [ m]','tethk [ m]','throat [ m]', &
                      '(r*delta_theta)TE','Geom Zweifel','le/te/btween/none'
	  else
	    write(100,202)'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)', &
                      'chord [ m]','sweep','lean','area[ m^2]','lethk [ m]','tethk [ m]','throat [ m]', &
                      '(r*delta_theta)WT','Geom Zweifel','le/te/btween/none'
      endif
	else
		print*,'Error in Blade scaling factor ...'
		print*,'Check the Scaling factor in the input file ...'
		stop
	endif	
   endif
   write(100,201) js,bladedata(1:(amount_data-1),js),throat_pos(js)		
enddo
write(100,*) 'Total Volume of the blade =',bladedata(amount_data,nsl),'[m^3]' 
close(100)

202 format((A7,2x),(A4,11x),4(A14,3x),(A13,1x),2(A6,5x),(A13,2x),(A10,2x),(A10,1x),(A11),2x,A17,2x,A12,2x,A17)
201 format(1x,I3,4x,(f11.8,4x),4(sf14.8,3x),3x,8(f11.8,1x),7x,f11.8,4x,A5)

return
end subroutine
!***************************************************************

!*******************************************************
!***************************************************************
subroutine throatindex(throat_pos,throat_index,n_normal_distance,js,nsl,thick_distr)
use file_operations
implicit none
integer, intent(inout):: js
integer, intent(in):: nsl,throat_index(nsl),n_normal_distance
character*20, intent(out):: throat_pos(nsl)
integer, intent(in)     :: thick_distr
integer                             :: nopen
character(:),   allocatable         :: log_file
logical                             :: file_open

call log_file_exists(log_file, nopen, file_open)
if (thick_distr .ne. 5) then
    if(throat_index(js) == 0) then
      throat_pos(js) = 'none'
      print*,'WARNING: No Throat found'
      write(nopen,*) 'WARNING: No Throat Found'
      !exit
    elseif(throat_index(js) < 0.25*n_normal_distance) then
      throat_pos(js) = 'le'
      print*,'throat_index',js,throat_index(js)
      write(nopen,*) 'throat_index', js, throat_index(js)
    elseif(throat_index(js) > 0.75*n_normal_distance) then
      throat_pos(js) = 'te' 
      print*,'throat_index',js,throat_index(js)
      write(nopen,*) 'throat_index', js, throat_index(js)
    else
      throat_pos(js) = 'btween'
      print*,'throat_index',js,throat_index(js)
      write(nopen,*) 'throat_index', js, throat_index(js)
    endif! end if for throat options
end if
call close_log_file(nopen, file_open)
return
end subroutine

!***************************************************************

!*******************************************************
!***************************************************************
subroutine cascade_nondim_file(msle,mste,mprime_ble,mprime_bte,chordm,pitch,nsl,ibrow,casename)
implicit none

integer, intent(in) :: nsl,ibrow
integer ia

character*80 file1
character*32 casename
real*8, intent(in) :: msle(nsl),mste(nsl),pitch
real*8, intent(in) :: mprime_ble(nsl),mprime_bte(nsl),chordm(nsl)


file1 = 'cascade_nondim.'//trim(casename)//'.dat'
open(13,file = file1,status ='unknown')
write(13,*)trim(casename)
write(13,*)'Blade row:',ibrow
write(13,*)"Non-dimensional quantities:"
write(13,*)"section    m'sLE             m'sTE              m'blade_LE          m'blade_TE&
                       chord             pitch          "

do ia = 1, nsl
   write(13,101)ia,msle(ia),mste(ia),mprime_ble(ia),mprime_bte(ia),chordm(ia),pitch
enddo 

close(13)

101  format(I2,2X,6(f19.16,1x))

return
end subroutine
!***************************************************************

!**********************************************************************************************
! BLADEGEN SUBROUTINES ------------
!**********************************************************************************************
!
      subroutine aecalc(n,x,y,t, itype, area,xcen,ycen,ei11,ei22,apx1,apx2)
      dimension x(n),y(n),t(n)
!
!---------------------------------------------------------------
!     calculates geometric properties of shape x,y
!
!     input:
!       n      number of points
!       x(.)   shape coordinate point arrays
!       y(.)
!       t(.)   skin-thickness array, used only if itype = 2
!       itype  = 1 ...   integration is over whole area  dx dy
!              = 2 ...   integration is over skin  area   t ds
!
!     output:
!       xcen,ycen  centroid location
!       ei11,ei22  principal moments of inertia
!       apx1,apx2  principal-axis angles
!---------------------------------------------------------------
!
      sint  = 0.0
      aint  = 0.0
      xint  = 0.0
      yint  = 0.0
      xxint = 0.0
      xyint = 0.0
      yyint = 0.0
!
      do 10 io = 1, n
        if(io.eq.n) then
          ip = 1
        else
          ip = io + 1
        endif
!
        dx =  x(io) - x(ip)
        dy =  y(io) - y(ip)
        xa = (x(io) + x(ip))*0.50
        ya = (y(io) + y(ip))*0.50
        ta = (t(io) + t(ip))*0.50
!
        ds = sqrt(dx*dx + dy*dy)
        sint = sint + ds

        if(itype.eq.1) then
!-------- integrate over airfoil cross-section
          da = ya*dx
          aint  = aint  +       da
          xint  = xint  + xa   *da
          yint  = yint  + ya   *da/2.0
          xxint = xxint + xa*xa*da
          xyint = xyint + xa*ya*da/2.0
          yyint = yyint + ya*ya*da/3.0
        else
!-------- integrate over skin thickness
          da = ta*ds
          aint  = aint  +       da
          xint  = xint  + xa   *da
          yint  = yint  + ya   *da
          xxint = xxint + xa*xa*da
          xyint = xyint + xa*ya*da
          yyint = yyint + ya*ya*da
        endif
!
 10   continue
!
      area = aint
!
      if(aint .eq. 0.0) then
        xcen  = 0.0
        ycen  = 0.0
        ei11  = 0.0
        ei22  = 0.0
        apx1 = 0.0
        apx2 = atan2(1.0,0.0)
        return
      endif
!
!
!---- calculate centroid location
      xcen = xint/aint
      ycen = yint/aint
!
!---- calculate inertias
      eixx = yyint - ycen*ycen*aint
      eixy = xyint - xcen*ycen*aint
      eiyy = xxint - xcen*xcen*aint
!
!---- set principal-axis inertias, ei11 is closest to "up-down" bending inertia
      eisq  = 0.25*(eixx  - eiyy )**2  + eixy**2
      sgn = sign( 1.0 , eiyy-eixx )
      ei11 = 0.5*(eixx + eiyy) - sgn*sqrt(eisq)
      ei22 = 0.5*(eixx + eiyy) + sgn*sqrt(eisq)
!
      if(eisq/(ei11*ei22) .lt. (0.001*sint)**4) then
!----- rotationally-invariant section (circle, square, etc.)
       apx1 = 0.0
       apx2 = atan2(1.0,0.0)
!
      else
       c1 = eixy
       s1 = eixx - ei11
       c2 = eixy
       s2 = eixx - ei22
!
       sgn1 = sign( 1.0 , c1 )
       sgn2 = sign( 1.0 , c2 )
!
       apx1 = atan2(sgn1*s1,sgn1*c1)
       apx2 = atan2(sgn2*s2,sgn2*c2)
!
      endif
	  
      return
      end subroutine
!*************************************************************

!*******************************************************************
subroutine stacking(xb,yb,xbot,ybot,xtop,ytop,js,np,stack_switch,stack,stk_u,stk_v,area,LE)
use file_operations
!---------------------------------------------------------------------------------------------------
! Airfoil Stacking options-----------------------Nov 2012
! Sample Input value : -053075 means stacked at 53% chord and 75% below meanline.
! Sample Input value : +100035 means stacked at 100% chord (TE) and 35% above meanline.
! Sample Input value : +200100 means stacked at area centroid and 100% above meanline.
! stack = above stacking number
! stack_switch = switch to activate variable radial stacking.
! xb = u, yb = v
! xb = xb - u_stack
! yb = yb - v_stack
! u_stack is the offset along the chord.
! v_stack is the offset above or below meanline.
! Stacking on meanline: v_stack = 0
! stku = stack position as %age of the chord, Range: [0,100].
! stkv = stack position from the meanline (stk_v=0). +ve is above the meanline and -ve is belove the meanline. Range:[-100,100].
! Stacking at the area centroid: value is 200.
!      print*,'stack:',stack
implicit none

integer i,j,k,nx,np_side,np,nphalf,ny,LE
integer, intent(in)::js,stack,stack_switch
parameter (nx = 1000,ny=300)

!real, intent (inout):: xb(nx),yb(nx),xbot(ny),ybot(ny),xtop(ny),ytop(ny)
real, intent (inout):: xb(np),yb(np),xbot((np+1)/2),ybot((np+1)/2),xtop((np+1)/2),ytop((np+1)/2)
real, intent(in):: stk_u(1), stk_v(1)
real umin,umax,sb(nx)
real u_stack,v_stack,vtop_stack,vbot_stack,v_zero_stack
real const_stk_u,const_stk_v,stku,stkv 
real area,ucen,vcen,ei11,ei22,apx1,apx2
integer                             :: nopen
character(len  = :),    allocatable :: log_file
logical                             :: file_open

! Calculating the area centroid cordinates of the airfoil ucen,vcen.
call log_file_exists(log_file, nopen, file_open)
print*,np
write(nopen,*) np
call aecalc(np,xb,yb,sb,1,area,ucen,vcen,ei11,ei22,apx1,apx2)
!
if(mod(np,2).eq.0)then
  np_side = np/2 ! For even no. of points LE = np/2. So, the current formula (np+1)/2 ==> np/2 with this change.
else
  ! points on bottom and top curve
  np_side = (np+1)*0.5
endif    
if(stack_switch.eq.0)then !constant radial stacking.
  const_stk_u = floor(abs(real(stack))/1000)
  stku = const_stk_u
  if(stack.ge.0)then
    const_stk_v = real(stack)-(const_stk_u*1000)
    stkv = const_stk_v
  else
    const_stk_v = real(stack)+(const_stk_u*1000)
    stkv = const_stk_v
  endif
elseif(stack_switch.eq.1)then ! variable radial stacking.Kiran 7/52/13
  stku = stk_u(js)
  stkv = stk_v(js) 
endif
write(*,*)
print*,'Stacking position on the chord as % of chord:',stku
print*,'Stacking position in % above(+ve) or below(-ve) meanline :',stkv
write(nopen,*) ''
write(nopen,*) 'Stacking position on the chord as % of chord:', stku
write(nopen,*) 'Stacking position in % above(+ve) or below(-ve) meanline :', stkv
! stacking on the chord...........
if(stku.eq.200.)then ! 200 is for stacking at the area centroid.
  u_stack = ucen
  v_stack = vcen
  print*,'Stacking at the area centroid of the airfoil...'
  write(nopen,*) 'Stacking at the area centroid of the airfoil...'
  goto 16
else
  ! Calculating u_stack using the % chord stacking information
  !nphalf = (np+1)/2
  if(LE.eq.2)then ! sting LE option
    print*,'LE with sting.'
    write(nopen,*) 'LE with sting.'
    !  nphalf = nphalf+2
    !xb(nphalf) = 0.5*(xb(nphalf-2) + xb(nphalf+2))
    !yb(nphalf) = 0.5*(yb(nphalf-2) + yb(nphalf+2))
  endif
  ! u_stack = xb(nphalf) + (real(stku)/100)*abs(sqrt((xb(nphalf) - xb(np))**2 + (yb(nphalf) - yb(np))**2))
  ! u_stack = xb(nphalf) + (real(stku)/100)*abs(xb(nphalf) - xb(np))! + (yb(nphalf) - yb(np))**2))
  u_stack = xb(1)*(real(stku)/100)
  ! write(*,*)
  ! print*,'ustack',u_stack
endif
!write(*,*)
!
! stacking on/above/below meanline.......
! Calculating vtop_stack and vbot_stack for v_stack.
j = 1
do i = 1, np_side-1 ! from 1 to 99 (top curve) for 199 points [moving from right to left]
   umin = xbot(i)
   umax = xbot(i+1)
   if(u_stack.ge.umin.and.u_stack.le.umax) then
     j = i
     exit
   endif
enddo
print*,'top curve points index range for % chord stacking:',j,j+1
write(nopen,*) 'top curve points index range for % chord stacking:',j,j+1
vbot_stack =  ybot(j) + ((u_stack - xbot(j))*(ybot(j+1) - ybot(j))/(xbot(j+1) - xbot(j))) !*sqrt((xbot(j+1) - xbot(j))**2 + (ybot(j+1) - ybot(j))**2) 
k = 1
do i = 1, np_side-1 ! from 101 to 198 for 199 points (bottom curve)
   umin = xtop(i)
   umax = xtop(i+1)
   if(u_stack.ge.umin.and.u_stack.le.umax) then
     k = i
     exit
   endif
enddo
print*,'bottom curve points index range for % chord stacking:',k,k+1
write(nopen,*) 'bottom curve points index range for % chord stacking:',k,k+1
vtop_stack =  ytop(k) + ((u_stack - xtop(k))*(ytop(k+1) - ytop(k))/(xtop(k+1) - xtop(k))) !*sqrt((xtop(k+1) - xtop(k))**2 + (ytop(k+1) - ytop(k))**2) 
! Stacking on meanline: v_stack = 0
! v_zero_stack = average of yb(istack) and yb(1+np - istack)  
! Above meanline stack: vstack= v_zero_stack - %age(stack)*(distance between meanline and 100% ABOVE meanline @ istack)
! Below meanline stack: vstack= v_zero_stack - %age(stack)*(distance between meanline and 100% BELOW meanline @ istack)
v_zero_stack = (vtop_stack + vbot_stack)/2
print*, "vtop_stack  vbot_stack", vtop_stack, vbot_stack
write(nopen,*) 'vtop_stack vbot_stack', vtop_stack, vbot_stack
if(stku.ne.200.and.stkv.gt.0)then! above meanline stacking
  v_stack = v_zero_stack + (real(stkv)/100)*(abs(vtop_stack - v_zero_stack))
  print*,'+ve stack v_stack',v_stack,(real(stkv)/100)
  write(nopen,*) '+ve stack v_stack', v_stack, (real(stkv)/100)
elseif(stku.ne.200.and.stkv.eq.0)then
  v_stack = 0.!v_zero_stack
elseif(stku.ne.200.and.stkv.lt.0)then !below meanline stacking
  v_stack = v_zero_stack + (real(stkv)/100)*(abs(vbot_stack - v_zero_stack))
  print*,'v_stack',v_stack,(real(stkv)/100)
  write(nopen,*) 'v_stack', v_stack, (real(stkv)/100)
endif
write(*,*)
print*,'u_stack  v_stack', u_stack, v_stack
write(*,*)
write(nopen,*) ''
write(nopen,*) 'u_stack  v_stack', u_stack, v_stack
write(nopen,*) ''
call close_log_file(nopen, file_open)
!-----stacked coordinates
16 do i = 1,np
      xb(i) = xb(i) - u_stack
      yb(i) = yb(i) - v_stack
      !print*,xb(i),yb(i)
   enddo

return
end subroutine
!***************************************************

!*******************************************************
!*******************************************************
subroutine rotate(xb,yb,x,y,angle)
implicit none
real x,y
real, intent(out) :: xb(1),yb(1)
real angle

xb(1) = x*cos(-angle) + y*sin(-angle)
yb(1) = y*cos(-angle) - x*sin(-angle)
!print*,xb(1),yb(1) 

return
end 
!***************************************************

!*******************************************************
!*******************************************************
subroutine rotate2(xb,yb,x,y,angle)
implicit none
real x,y
real, intent(out) :: xb,yb
real angle

xb = x*cos(-angle) + y*sin(-angle)
yb = y*cos(-angle) - x*sin(-angle)
!print*,xb(1),yb(1) 

return
end 
!
!**************************************************

!*******************************************************
!*******************************************************
real function scaled(x,scalefactor)
implicit none
real, intent(in):: x,scalefactor

scaled = x*scalefactor

return
end
!
!*******************************************************

!*******************************************************
!*******************************************************
subroutine bladesection(xb,yb,np,nbls,TE_del,sinls,sexts,chrdd,fext,js,pitch,mble,mbte,airfoil)
implicit none
integer TE_del,nx,nbls,ii,i,js,np,np_side
parameter(nx = 1000)
character(*)    :: fext
character(*)    :: airfoil
character*32 fname,blext(100)
!character*20 airfoil
real*8 sexts,sinls,chrdd,pitch,pi,x1,y1,x2,y2
real*8, intent(inout) :: xb(nx),yb(nx)
real*8 mble,mbte

if(mod(np,2).eq.0)then
  np_side = np/2 ! For even no. of points LE = np/2. So, the current formula (np+1)/2 ==> np/2 with this change.
else
  ! points on bottom and top curve
  np_side = (np+1)*0.5
endif

!If the TE points do not coincide.
x1 = xb(1)
y1 = yb(1)
x2 = xb(np)
y2 = yb(np)
if(len_trim(airfoil).eq.4)then ! for NACA cases to have a closed straight TE.
  xb(1) = 0.5*(x1 + x2)
  yb(1) = 0.5*(y1 + y2)
  xb(np) = xb(1)
  yb(np) = yb(1)
endif
if((x1.ne.x2).and.(y1.ne.y2))then 
  xb(1)  = 0.5*(xb(np) + xb(1))
  yb(1)  = 0.5*(yb(np) + yb(1))
  xb(np) = xb(1)
  yb(np) = yb(1)
endif 
pi = 4.*atan(1.0)
! Calculation of the pitch between adjacent blades:
pitch = 2*pi/nbls 
! Switch for deleting the trailing edge coordinates for each blade:
!TE_del = 1 --- I moved it in the parameters nemnem
if(TE_del == 1) then
  ii = 18   ! ii is the number of points deleted from the Trailing edge.
  !print*,'The switch to delete Trailing edge from blade files is ON'
elseif (TE_del == 0) then
  ii = 0
  !print*,'The switch to delete Trailing edge from blade files is OFF'
endif
!Assiging the LE and TE to variables
mble = xb(np_side)
mbte = xb(np)
!---- output
fname = 'blade.'//trim(fext)
open(2,file=fname,status='unknown')
write(2,100) trim(fext)
write(2,101) sinls,sexts,0.5*chrdd,0.5*chrdd,pitch
! print*,'np before blade file',np
do i = ii+1,np-ii
   write(2,*) xb(i),yb(i)
enddo
close(2)
 
100  format(a)
101  format(5(f19.16,1x))
102  format(2(f35.16,1x))
! 105  format(' Blade Area               : ',f8.5)
!106  format(' Blade Centroid           : ',f8.5,1x,f8.5)

return 
end subroutine
!*****************************************************

!*****************************************************
!*****************************************************
SUBROUTINE st_line_intersection(XA,YA,XB,YB,XC,YC,XD,YD,XINT,YINT)

integer i,j	
real*8 XA,YA,XB,YB,XC,YC,XD,YD,xmin,ymin,xmax,ymax
real*8 , intent(out):: XINT,YINT

! xmin = min(XA,XB,XC,XD)	
! ymin = min(YA,YB,YC,YD)
! xmax = max(XA,XB,XC,XD)	
! ymax = max(YA,YB,YC,YD) 
!
xint =(XA*(YB-YA)+YC*(XB-XA)-((YD-YC)/(XD-XC))*XC*(XB-XA)-YA*(XB-XA))/((YB-YA)-(((YD-YC)*(XB-XA))/(XD-XC)))
yint = YC+((XINT-XC)/(XD-XC))*(YD-YC)
RETURN
END subroutine
!*******************************************************

!********************************************************
!********************************************************
! This subroutine is to calculate the 2D throat between nondimensional blade sections.
! The approach here is to find the perpendicular distance on the pitch line(camber at half pitch)
! 	between 2 adj blades.
! calculating the throat from bottom point on the blade ---(for +ve ainl and -ve aext)---
subroutine throat_calc_pitch_line(xb,yb,np,camber,angle,sang,u,pi,pitch,throat_coord, mouth_coord,exit_coord, &
                                  min_throat_2D,throat_index,n_normal_distance,casename,js,nsl,develop,isdev)
use file_operations
! np = 2*np_sidee-1
! All data used is nondimensional

implicit none
integer i,j,k,np,np_sidee,throat_index,n_normal_distance,js,nsl
integer i_interup1,i_interup2,i_interdwn1,i_interdwn2
real v1_top((np+1)/2),u1_top((np+1)/2),v2_bot((np+1)/2),u2_bot((np+1)/2)
real xb(np),yb(np),yb_upper(np)
real xint_up,yint_up,xint_dwn,yint_dwn
real pitch, throat_coord(4),mouth_coord(4),exit_coord(4)
real inter_coord(4,((np+1)/2))
real pitch_line((np+1)/2), camber((np+1)/2),angle((np+1)/2),u((np+1)/2)
real camber_upper((np+1)/2)
real min_throat_2D,pi,sang,angle_up
real x_interup,y_interup,x_interdwn,y_interdwn
real, allocatable, dimension(:) :: throat
character*80 file4
character*32 casename,develop

logical isdev

integer                                         :: nopen
character(len = :), allocatable                 :: log_file
logical                                         :: file_open

if(mod(np,2).eq.0)then
  np_sidee = np/2 ! For even no. of points LE = np/2. So, the current formula (np+1)/2 ==> np/2 with this change.
else
  ! points on bottom and top curve
  np_sidee = (np+1)/2
endif
!np_sidee = (np+1)/2
call log_file_exists(log_file, nopen, file_open)
print*,'np_sidee',np_sidee
write(nopen,*) 'np_sidee', np_sidee
! intializing the values to 0...
n_normal_distance = 0
throat_index = 0
inter_coord = 0.
throat_coord = 0.; mouth_coord = 0. ;exit_coord = 0.
! Calculating the pitch line in mid way between 2 blades:
pitch_line = camber + (0.5*pitch)
! Create the upper blade:
yb_upper = yb + pitch
camber_upper = camber + pitch
! Finding the upper point using the point and angle:
! after rotation by stagger, 'sang' should be taking into consideration,
! write(85,*),'u1_top v1_top	u2_bot v2_bot'

do i = 1, np_sidee
   v1_top(i) = pitch_line(i)+ 0.5*pitch
   u1_top(i) = 1/tan(angle(i)+sang+pi/2.)*(v1_top(i)-pitch_line(i))+u(i)
   v2_bot(i) = pitch_line(i)- 0.5*pitch
   u2_bot(i) = 1/tan(angle(i)+sang-pi/2.)*(v2_bot(i)-pitch_line(i))+u(i)
   !write(85,*),u1_top(i),v1_top(i)
   !write(85,*),u2_bot(i),v2_bot(i)
enddo

! Identify the throat by intersection point with upper surface of the blade:
! intials:
k = 1
!write(85,*)'intersection points'
do j = 1, np_sidee
   i_interup1 = 0
   i_interdwn1 = 0
  do i = np_sidee,np-1  ! intersection with upper airfoil
     call st_line_intersection(u(j),pitch_line(j),u1_top(j),v1_top(j),xb(i),yb_upper(i),xb(i+1),yb_upper(i+1),xint_up,yint_up)
     !print*,'xint_up,yint_up',xint_up,yint_up
     if((xint_up >= xb(i) ).and.(xint_up <= xb(i+1)))then
       !print*,"correct point of intersection ..."
       !print*,'xint_up,yint_up ----',xint_up,yint_up
       x_interup = xint_up ;y_interup = yint_up
       !write(85,*),'xint_up,yint_up',xint_up,yint_up
       i_interup1 = i ;i_interup2 = i+1
       !print*,'i_interup',i_interup1,i_interup2			
     endif
  enddo       
  do i = 1,np_sidee-1		! intersection with lower airfoil
     call st_line_intersection(u(j),pitch_line(j),u2_bot(j),v2_bot(j),xb(i),yb(i),xb(i+1),yb(i+1),xint_dwn,yint_dwn)
     !print*,'xint_dwn,yint_dwn',xint_dwn,yint_dwn
     if((xint_dwn <= xb(i) ).and.(xint_dwn >= xb(i+1)))then
       !print*,"correct point of intersection ..."
       !print*,'xint_dwn,yint_dwn ----',xint_dwn,yint_dwn
       x_interdwn = xint_dwn ;y_interdwn = yint_dwn
       !write(85,*),'xint_dwn,yint_dwn',xint_dwn,yint_dwn
       i_interdwn1 = i ;i_interdwn2 = i+1
       !print*,'i_interdwn',i_interdwn1,i_interdwn2			
     endif
  enddo
  if((i_interup1.ne.0).and.(i_interdwn1.ne.0)) then
    inter_coord(1,k) = x_interup;inter_coord(2,k) = y_interup  ! first intersection point
    inter_coord(3,k) = x_interdwn ;inter_coord(4,k) = y_interdwn ! second intersection point
    !print*,'inter_coord(:,k)',inter_coord(:,k),k
    ! write(85,*),inter_coord(1:2,k)
    ! write(85,*),inter_coord(3:4,k),k
    n_normal_distance = k  ! number of throats for each section
    k = k+1
  endif
enddo

print*, 'n_normal_distance =',n_normal_distance
write(nopen,*) 'n_normal_distance = ', n_normal_distance
if(n_normal_distance == 0) then
  print*, 'WARNING: No throats found because of low number of blades'
  write(nopen,*) 'WARNING: No throats found because of low number of blades'
  return
endif

! Writing the Mouth and Exit areas:
mouth_coord = inter_coord(:,1)
exit_coord = inter_coord(:,n_normal_distance)

print*,'number of intersection points (k) =',k-1,'from np_side of',np_sidee
write(nopen,*) 'Number of intersection points (k) = ',k-1,'from np_side of',np_sidee

if (allocated(throat)) deallocate(throat)
Allocate (throat(n_normal_distance))
! Calculation of the throat:
throat(1) = sqrt((inter_coord(1,1)-inter_coord(3,1))**2+(inter_coord(2,1)-inter_coord(4,1))**2) ! Nondimensional
min_throat_2D = throat(1)
throat_index = 1
do k = 2,n_normal_distance
   throat(k) = sqrt((inter_coord(1,k)-inter_coord(3,k))**2+(inter_coord(2,k)-inter_coord(4,k))**2) ! Nondimensional
   if(throat(k) < min_throat_2D) then
      min_throat_2D = throat(k)
      throat_index = k
      throat_coord = inter_coord(:,k)
   endif
enddo
! Writing to a file for debugging
!-----------------------------------------------
if(isdev)then     
  print*,""
  print*,'Writing non-dimensional throat points to a file for debugging...'
  print*,""
  write(nopen,*) ''
  write(nopen,*) 'Writing non-dimensional throat points to a file for debugging...'
  write(nopen,*) ''
  if(js==1) then 
    file4 = 'throat_points.'//trim(casename)//'.txt'
    open(unit= 85,file=file4, form="formatted")
    print*,file4
    write(nopen,*) file4
    write(85,*)'pitch',pitch
  endif
  write(85,*)'section',js
  write(85,*)'u	camber	upper_camber'
  do i = 1, np_sidee
     write(85,*) u(i),camber(i),camber_upper(i)
  enddo
  write(85,*)'u	pitch_line'
  do i = 1, np_sidee
     write(85,*)u(i), pitch_line(i)
  enddo
  write(85,*)'u1_top v1_top	u2_bot v2_bot'
  do i = 1, np_sidee
     write(85,*)u1_top(i),v1_top(i)
     write(85,*)u2_bot(i),v2_bot(i)
  enddo
  write(85,*)' intersection points'
  write(85,*) '  u                   v                    counter'   
  do k = 1, n_normal_distance
     write(85,*) inter_coord(1:2,k)
     write(85,*) inter_coord(3:4,k),k
  enddo
  write(85,*)'min_throat_2D',min_throat_2D
  write(85,*)'throat_index',throat_index
  if(js==nsl) then
    close(85)
  endif
endif   

call close_log_file(nopen, file_open)

return
END subroutine throat_calc_pitch_line
!**********************************************************

!*******************************************************
!*******************************************************
subroutine averaged_camber(xb,yb,np,u,camber,angle,sinl)

implicit none
integer np,i
real xb(np),yb(np),u((np+1)/2),camber((np+1)/2),angle((np+1)/2)
real u_i((np+1)/2),camber_i((np+1)/2)
real sinl

! initializing to 0
camber_i 	= 0
u_i 	= 0
camber 	= 0
angle 	= 0
print*, 'np',np
! calculating the average:
do i = 1,(np+1)/2
   u_i(i)      = (xb(i) + xb(np+1-i)) /2
   camber_i(i) = (yb(i) + yb(np+1-i)) /2
enddo

! arrange the u and camber ascendingly 
do i = 1, (np+1)/2
   u(i)      = u_i(((np+1)/2)-i+1)
   camber(i) = camber_i(((np+1)/2)-i+1)
enddo
! open(unit= 50,file= 'NACA_camber.txt', form="formatted")
! write(50,*) 'NACA camber coordinates' 
! do i = 1, (np+1)/2
     ! write(50,*) u(i),camber(i)
! enddo
! close(50)
	
! calculate the camber line angles (slope):
angle(1) 		= atan(sinl)
do i = 1, ((np+1)/2)-1
   !print*,'angle(i)',angle(i)	
   angle(i+1) = atan((camber(i+1)-camber(i))/(u(i+1)-u(i)))
enddo

return
endsubroutine averaged_camber
!**************************************************

!*******************************************************
!**************************************************
subroutine askr(prompt,rinput)
!---- real input
character*(*) prompt
real rinput

np = index(prompt,'^') - 1
if(np.eq.0) np = len(prompt)
10 write(*,1000) prompt(1:np)
read (*,*,err=10) rinput
return
!
1000 format(/a,'   r>  ',$)
end ! askr

!-------------------------------------------------------------------------------

!*******************************************************************************
!*******************************************************************************
subroutine meanline3DNperiodicwall(xb,yb,zb,xposlean,yposlean,zposlean, &
                                   xneglean,yneglean,zneglean,iap,nsec, &
                                   uplmt,scf,casename)
! creates meanline coordinates and periodic walls both top and bottom.

implicit none

integer i,ia, nx, nax
integer, intent(in):: iap,nsec,uplmt

parameter (nx = 1000, nax = 1000)

real*8, intent(in):: xb(iap,nsec),yb(iap,nsec),zb(iap,nsec),scf
real*8, intent(in):: xposlean(iap,nsec),yposlean(iap,nsec),zposlean(iap,nsec)
real*8, intent(in):: xneglean(iap,nsec),yneglean(iap,nsec),zneglean(iap,nsec)
real*8 xprd_top(nx,nax),yprd_top(nx,nax),zprd_top(nx,nax)
real*8 xprd_bot(nx,nax),yprd_bot(nx,nax),zprd_bot(nx,nax)
real*8 xmeanline(nx,nax),ymeanline(nx,nax),zmeanline(nx,nax)

character*32 casename,temp
character*80 fname2,fname4,fname6,fname7

!-------------------------------------------------------------------------------
! Adding the upstream and downstream curves of constant theta. 6 points on both sides.
!-------------------------------------------------------------------------------
! do ia = 1, nsec
   ! do i = 1, iap
      ! print*,xposlean(i,ia),yposlean(i,ia),zposlean(i,ia)
   ! enddo
   ! write(*,*)
! enddo
do ia = 1, nsec
   !! upstream coordinates
   xmeanline(1,ia) = -scf*0.20 + scf*xb(uplmt+1,ia)
   ymeanline(1,ia) = scf*yb(uplmt+1,ia)
   zmeanline(1,ia) = scf*zb(uplmt+1,ia)
   !top periodic wall coordinates
   xprd_top(1,ia) = -scf*0.20 + scf*xposlean(uplmt+1,ia)
   yprd_top(1,ia) = scf*yposlean(uplmt+1,ia)
   zprd_top(1,ia) = scf*zposlean(uplmt+1,ia)
   !bot periodic wall coordinates
   xprd_bot(1,ia) = -scf*0.20 + scf*xneglean(uplmt+1,ia)
   yprd_bot(1,ia) = scf*yneglean(uplmt+1,ia)
   zprd_bot(1,ia) = scf*zneglean(uplmt+1,ia)
   !!
   xmeanline(2,ia) = -scf*0.18 + scf*xb(uplmt+1,ia)
   ymeanline(2,ia) = scf*yb(uplmt+1,ia)
   zmeanline(2,ia) = scf*zb(uplmt+1,ia) 
   !top periodic wall coordinates
   xprd_top(2,ia) = -scf*0.18 + scf*xposlean(uplmt+1,ia)
   yprd_top(2,ia) = scf*yposlean(uplmt+1,ia)
   zprd_top(2,ia) = scf*zposlean(uplmt+1,ia)
   !bot periodic wall coordinates
   xprd_bot(2,ia) = -scf*0.18 + scf*xneglean(uplmt+1,ia)
   yprd_bot(2,ia) = scf*yneglean(uplmt+1,ia)
   zprd_bot(2,ia) = scf*zneglean(uplmt+1,ia)
   !!
   xmeanline(3,ia) = -scf*0.16 + scf*xb(uplmt+1,ia)
   ymeanline(3,ia) = scf*yb(uplmt+1,ia)
   zmeanline(3,ia) = scf*zb(uplmt+1,ia) 
   !top periodic wall coordinates
   xprd_top(3,ia) = -scf*0.16 + scf*xposlean(uplmt+1,ia)
   yprd_top(3,ia) = scf*yposlean(uplmt+1,ia)
   zprd_top(3,ia) = scf*zposlean(uplmt+1,ia)
   !bot periodic wall coordinates
   xprd_bot(3,ia) = -scf*0.16 + scf*xneglean(uplmt+1,ia)
   yprd_bot(3,ia) = scf*yneglean(uplmt+1,ia)
   zprd_bot(3,ia) = scf*zneglean(uplmt+1,ia)
   !!   
   xmeanline(4,ia) = -scf*0.12 + scf*xb(uplmt+1,ia)
   ymeanline(4,ia) = scf*yb(uplmt+1,ia)
   zmeanline(4,ia) = scf*zb(uplmt+1,ia)
   !top periodic wall coordinates
   xprd_top(4,ia) = -scf*0.12 + scf*xposlean(uplmt+1,ia)
   yprd_top(4,ia) = scf*yposlean(uplmt+1,ia)
   zprd_top(4,ia) = scf*zposlean(uplmt+1,ia)
   !bot periodic wall coordinates
   xprd_bot(4,ia) = -scf*0.12 + scf*xneglean(uplmt+1,ia)
   yprd_bot(4,ia) = scf*yneglean(uplmt+1,ia)
   zprd_bot(4,ia) = scf*zneglean(uplmt+1,ia)
   !!
   xmeanline(5,ia) = -scf*0.10 + scf*xb(uplmt+1,ia)
   ymeanline(5,ia) = scf*yb(uplmt+1,ia)
   zmeanline(5,ia) = scf*zb(uplmt+1,ia)
   !top periodic wall coordinates
   xprd_top(5,ia) = -scf*0.10 + scf*xposlean(uplmt+1,ia)
   yprd_top(5,ia) = scf*yposlean(uplmt+1,ia)
   zprd_top(5,ia) = scf*zposlean(uplmt+1,ia)
   !bot periodic wall coordinates
   xprd_bot(5,ia) = -scf*0.10 + scf*xneglean(uplmt+1,ia)
   yprd_bot(5,ia) = scf*yneglean(uplmt+1,ia)
   zprd_bot(5,ia) = scf*zneglean(uplmt+1,ia)
   !!
   xmeanline(6,ia) = -scf*0.06 + scf*xb(uplmt+1,ia)
   ymeanline(6,ia) = scf*yb(uplmt+1,ia)
   zmeanline(6,ia) = scf*zb(uplmt+1,ia)
   !top periodic wall coordinates
   xprd_top(6,ia) = -scf*0.06 + scf*xposlean(uplmt+1,ia)
   yprd_top(6,ia) = scf*yposlean(uplmt+1,ia)
   zprd_top(6,ia) = scf*zposlean(uplmt+1,ia)
   !bot periodic wall coordinates
   xprd_bot(6,ia) = -scf*0.06 + scf*xneglean(uplmt+1,ia)
   yprd_bot(6,ia) = scf*yneglean(uplmt+1,ia)
   zprd_bot(6,ia) = scf*zneglean(uplmt+1,ia)
 
   !------------------------------------------------------------------------------- 
   !!average of the top and bottom curves
   !-------------------------------------------------------------------------------
   do i = 7,uplmt
      xmeanline(i,ia) = scf*(xb((uplmt+1) - (i-1),ia) + xb((uplmt+1) + (i-1),ia))*0.5
      ymeanline(i,ia) = scf*(yb((uplmt+1) - (i-1),ia) + yb((uplmt+1) + (i-1),ia))*0.5
      zmeanline(i,ia) = scf*(zb((uplmt+1) - (i-1),ia) + zb((uplmt+1) + (i-1),ia))*0.5 
      
      !!top periodic wall coordinates
      xprd_top(i,ia) = scf*(xposlean((uplmt+1) - (i-1),ia) + xposlean((uplmt+1) + (i-1),ia))*0.5
      yprd_top(i,ia) = scf*(yposlean((uplmt+1) - (i-1),ia) + yposlean((uplmt+1) + (i-1),ia))*0.5
      zprd_top(i,ia) = scf*(zposlean((uplmt+1) - (i-1),ia) + zposlean((uplmt+1) + (i-1),ia))*0.5
      
      !!bot periodic wall coordinates
      xprd_bot(i,ia) = scf*(xneglean((uplmt+1) - (i-1),ia) + xneglean((uplmt+1) + (i-1),ia))*0.5
      yprd_bot(i,ia) = scf*(yneglean((uplmt+1) - (i-1),ia) + yneglean((uplmt+1) + (i-1),ia))*0.5
      zprd_bot(i,ia) = scf*(zneglean((uplmt+1) - (i-1),ia) + zneglean((uplmt+1) + (i-1),ia))*0.5 
   enddo
   
   !-------------------------------------------------------------------------------
   !!downstream coordinates
   !-------------------------------------------------------------------------------
   xmeanline(uplmt+1,ia) = scf*0.06 + scf*xb(iap,ia)
   ymeanline(uplmt+1,ia) = scf*yb(iap,ia)
   zmeanline(uplmt+1,ia) = scf*zb(iap,ia)
   !top periodic wall coordinates
   xprd_top(uplmt+1,ia) = scf*0.06 + scf*xposlean(iap,ia)
   yprd_top(uplmt+1,ia) = scf*yposlean(iap,ia)
   zprd_top(uplmt+1,ia) = scf*zposlean(iap,ia)
   !bot periodic wall coordinates
   xprd_bot(uplmt+1,ia) = scf*0.06 + scf*xneglean(iap,ia)
   yprd_bot(uplmt+1,ia) = scf*yneglean(iap,ia)
   zprd_bot(uplmt+1,ia) = scf*zneglean(iap,ia)
   !!
   xmeanline(uplmt+2,ia) = scf*0.10  + scf*xb(iap,ia)
   ymeanline(uplmt+2,ia) = scf*yb(iap,ia)
   zmeanline(uplmt+2,ia) = scf*zb(iap,ia) 
   !top periodic wall coordinates
   xprd_top(uplmt+2,ia) = scf*0.10 + scf*xposlean(iap,ia)
   yprd_top(uplmt+2,ia) = scf*yposlean(iap,ia)
   zprd_top(uplmt+2,ia) = scf*zposlean(iap,ia)
   !bot periodic wall coordinates
   xprd_bot(uplmt+2,ia) = scf*0.10 + scf*xneglean(iap,ia)
   yprd_bot(uplmt+2,ia) = scf*yneglean(iap,ia)
   zprd_bot(uplmt+2,ia) = scf*zneglean(iap,ia)
   !!
   xmeanline(uplmt+3,ia) = scf*0.12 + scf*xb(iap,ia)
   ymeanline(uplmt+3,ia) = scf*yb(iap,ia)
   zmeanline(uplmt+3,ia) = scf*zb(iap,ia) 
   !top periodic wall coordinates
   xprd_top(uplmt+3,ia) = scf*0.12 + scf*xposlean(iap,ia)
   yprd_top(uplmt+3,ia) = scf*yposlean(iap,ia)
   zprd_top(uplmt+3,ia) = scf*zposlean(iap,ia)
   !bot periodic wall coordinates
   xprd_bot(uplmt+3,ia) = scf*0.12 + scf*xneglean(iap,ia)
   yprd_bot(uplmt+3,ia) = scf*yneglean(iap,ia)
   zprd_bot(uplmt+3,ia) = scf*zneglean(iap,ia)
   !!   
   xmeanline(uplmt+4,ia) = scf*0.16 + scf*xb(iap,ia)
   ymeanline(uplmt+4,ia) = scf*yb(iap,ia)
   zmeanline(uplmt+4,ia) = scf*zb(iap,ia)
   !top periodic wall coordinates
   xprd_top(uplmt+4,ia) = scf*0.16 + scf*xposlean(iap,ia)
   yprd_top(uplmt+4,ia) = scf*yposlean(iap,ia)
   zprd_top(uplmt+4,ia) = scf*zposlean(iap,ia)
   !bot periodic wall coordinates
   xprd_bot(uplmt+4,ia) = scf*0.16 + scf*xneglean(iap,ia)
   yprd_bot(uplmt+4,ia) = scf*yneglean(iap,ia)
   zprd_bot(uplmt+4,ia) = scf*zneglean(iap,ia)
   !!
   xmeanline(uplmt+5,ia) = scf*0.18+ scf*xb(iap,ia)
   ymeanline(uplmt+5,ia) = scf*yb(iap,ia)
   zmeanline(uplmt+5,ia) = scf*zb(iap,ia)
   !top periodic wall coordinates
   xprd_top(uplmt+5,ia) = scf*0.18 + scf*xposlean(iap,ia)
   yprd_top(uplmt+5,ia) = scf*yposlean(iap,ia)
   zprd_top(uplmt+5,ia) = scf*zposlean(iap,ia)
   !bot periodic wall coordinates
   xprd_bot(uplmt+5,ia) = scf*0.18 + scf*xneglean(iap,ia)
   yprd_bot(uplmt+5,ia) = scf*yneglean(iap,ia)
   zprd_bot(uplmt+5,ia) = scf*zneglean(iap,ia)
   !!
   xmeanline(uplmt+6,ia) = scf*0.20 + scf*xb(iap,ia)
   ymeanline(uplmt+6,ia) = scf*yb(iap,ia)
   zmeanline(uplmt+6,ia) = scf*zb(iap,ia)
   !top periodic wall coordinates
   xprd_top(uplmt+6,ia) = scf*0.20 + scf*xposlean(iap,ia)
   yprd_top(uplmt+6,ia) = scf*yposlean(iap,ia)
   zprd_top(uplmt+6,ia) = scf*zposlean(iap,ia)
   !bot periodic wall coordinates
   xprd_bot(uplmt+6,ia) = scf*0.20 + scf*xneglean(iap,ia)
   yprd_bot(uplmt+6,ia) = scf*yneglean(iap,ia)
   zprd_bot(uplmt+6,ia) = scf*zneglean(iap,ia)
   !!
   
   !-------------------------------------------------------------------------------
   ! Writing to external files.
   !-------------------------------------------------------------------------------
   ! Writing meanline coordinates.
   write(temp,*)ia
   fname2 = 'meanline.sec'//trim(adjustl(temp))//'.'//trim(casename)//'.dat'
   ! fname4 = 'meanline.sec'//trim(adjustl(temp))//'.'//trim(casename)//'.csv'
   !
   open(4,file = fname2,form = 'formatted')
   ! open(5,file = fname4,form = 'formatted')
   !
   do i = 1,uplmt+6
      write(4,*)xmeanline(i,ia),ymeanline(i,ia),zmeanline(i,ia)
      ! write(5,11)xmeanline(i,ia),',',ymeanline(i,ia),',',zmeanline(i,ia)
   enddo  
   close(4)
   ! close(5)
   
   !-------------------------------------------------------------------------------
   ! Writing Top and Bottom periodic wall coordinates.
   ! fname6 = 'top_periodic.sec'//trim(adjustl(temp))//'.'//trim(casename)//'.csv'
   ! fname7 = 'bot_periodic.sec'//trim(adjustl(temp))//'.'//trim(casename)//'.csv'
   ! open(7,file = fname6,form = 'formatted')
   ! open(8,file = fname7,form = 'formatted')
   ! do i = 1, uplmt+6
      ! write(7,11)xprd_top(i,ia),',',yprd_top(i,ia),',',zprd_top(i,ia)
      ! write(8,11)xprd_bot(i,ia),',',yprd_bot(i,ia),',',zprd_bot(i,ia)  
   ! enddo 
   ! close(7)
   ! close(8)
enddo   
write(*,*)

!-------------------------------------------------------------------------------
11 format((f20.16),A,(f20.16),A,(f20.16))
!-------------------------------------------------------------------------------

return
end subroutine meanline3DNperiodicwall

!*******************************************************************************************
!*******************************************************************************
!*******************************************************************************
real*8 function numoffset(num,delta)
implicit none
real*8 delta,num(1)

numoffset = num(1) + delta

end function

!*******************************************************************************
!*******************************************************************************
!*******************************************************************************

subroutine constantslopemeanline3D(xb,yb,zb,xposlean,yposlean,zposlean, &
                                   xneglean,yneglean,zneglean,iap,nsec, &
                                   uplmt,scf,casename)
! creates meanline coordinates and periodic walls both top and bottom.

implicit none

integer i,ia,j, nx, nax, npts, npts_slope, npts_axial
integer, intent(in):: iap,nsec,uplmt

parameter (nx = 1000, nax = 1000)

real*8, intent(in):: xb(iap,nsec),yb(iap,nsec),zb(iap,nsec),scf
real*8, intent(in):: xposlean(iap,nsec),yposlean(iap,nsec),zposlean(iap,nsec)
real*8, intent(in):: xneglean(iap,nsec),yneglean(iap,nsec),zneglean(iap,nsec)
real*8 xprd_top(nx,nax),yprd_top(nx,nax),zprd_top(nx,nax)
real*8 xprd_bot(nx,nax),yprd_bot(nx,nax),zprd_bot(nx,nax)
real*8 xmeanline(nx,nax),ymeanline(nx,nax),zmeanline(nx,nax)
real*8 phi_xz_up,phi_xz_dwn,phi_xy_up,phi_xy_dwn,coefficient(6),mag_coff

character*32 casename,temp
character*80 fname2,fname4,fname6,fname7

!-------------------------------------------------------------------------------
! Adding the upstream and downstream curves of constant theta. 6 points on both sides.

!-------------------------------------------------------------------------------
	npts = 4  ! number of points added
	! npts_slope = 4 ! Number of points maintaing the meanline slope
	! npts_axial = npts - npts_slope ! Number of points in horizontal x dir.
	mag_coff = 1 ! magnification factor for the peiodic boundaries (1 for no magnification)
do ia = 1, nsec
   !------------------------------------------------------------------------------- 
   !!average of the top and bottom curves (meanline calculation)
   !-------------------------------------------------------------------------------
   do i = npts+1,uplmt
      xmeanline(i,ia) = scf*(xb((uplmt+1) - (i-1),ia) + xb((uplmt+1) + (i-1),ia))*0.5
      ymeanline(i,ia) = scf*(yb((uplmt+1) - (i-1),ia) + yb((uplmt+1) + (i-1),ia))*0.5
      zmeanline(i,ia) = scf*(zb((uplmt+1) - (i-1),ia) + zb((uplmt+1) + (i-1),ia))*0.5 
      
      !!top periodic wall coordinates
      xprd_top(i,ia) = scf*(xposlean((uplmt+1) - (i-1),ia) + xposlean((uplmt+1) + (i-1),ia))*0.5
      yprd_top(i,ia) = scf*(yposlean((uplmt+1) - (i-1),ia) + yposlean((uplmt+1) + (i-1),ia))*0.5
      zprd_top(i,ia) = scf*(zposlean((uplmt+1) - (i-1),ia) + zposlean((uplmt+1) + (i-1),ia))*0.5
      
      !!bot periodic wall coordinates
      xprd_bot(i,ia) = scf*(xneglean((uplmt+1) - (i-1),ia) + xneglean((uplmt+1) + (i-1),ia))*0.5
      yprd_bot(i,ia) = scf*(yneglean((uplmt+1) - (i-1),ia) + yneglean((uplmt+1) + (i-1),ia))*0.5
      zprd_bot(i,ia) = scf*(zneglean((uplmt+1) - (i-1),ia) + zneglean((uplmt+1) + (i-1),ia))*0.5 
   enddo 
   
  !-------------------------------------------------------------------------------
  ! Calculating the Slope angles in xz and xy planes from meanline coordinates:
   phi_xz_up = atan(real(zmeanline(npts+2,ia)-zmeanline(npts+1,ia))/(xmeanline(npts+2,ia)-xmeanline(npts+1,ia)))
   phi_xy_up = atan(real(ymeanline(npts+2,ia)-ymeanline(npts+1,ia))/(xmeanline(npts+2,ia)-xmeanline(npts+1,ia)))
   phi_xz_dwn = atan(real(zmeanline(uplmt,ia)-zmeanline(uplmt-1,ia))/(xmeanline(uplmt,ia)-xmeanline(uplmt-1,ia)))
   phi_xy_dwn = atan(real(ymeanline(uplmt,ia)-ymeanline(uplmt-1,ia))/(xmeanline(uplmt,ia)-xmeanline(uplmt-1,ia)))  
  !-------------------------------------------------------------------------------
   !! upstream coordinates
   	!coefficient = [ 0.06, 0.10, 0.12, 0.16, 0.18, 0.2 ]* mag_coff
   	coefficient = [ 0.04, 0.05, 0.06, 0.07, 0.08, 0.1 ]* mag_coff	
	! Constant slope:
    do j = npts, 1, -1
	   xmeanline(j,ia) = -scf*coefficient(npts+1-j) + scf*xb(uplmt+1,ia)
	   ymeanline(j,ia) = (xmeanline(j,ia)-xmeanline(j+1,ia))*tan(phi_xy_up)+ymeanline(j+1,ia)
	   zmeanline(j,ia) = (xmeanline(j,ia)-xmeanline(j+1,ia))*tan(phi_xz_up)+zmeanline(j+1,ia)
	   !top periodic wall coordinates
	   xprd_top(j,ia) = -scf*coefficient(npts+1-j) + scf*xposlean(uplmt+1,ia)
	   yprd_top(j,ia) = (xprd_top(j,ia)-xprd_top(j+1,ia))*tan(phi_xy_up)+scf*yprd_top(j+1,ia)
	   zprd_top(j,ia) = (xprd_top(j,ia)-xprd_top(j+1,ia))*tan(phi_xz_up)+scf*zprd_top(j+1,ia)
	   !bot periodic wall coordinates
	   xprd_bot(j,ia) = -scf*coefficient(npts+1-j) + scf*xneglean(uplmt+1,ia)
	   yprd_bot(j,ia) = (xprd_bot(j,ia)-xprd_bot(j+1,ia))*tan(phi_xy_up)+scf*yprd_bot(j+1,ia)
	   zprd_bot(j,ia) = (xprd_bot(j,ia)-xprd_bot(j+1,ia))*tan(phi_xz_up)+scf*zprd_bot(j+1,ia)
	   ! print*,xmeanline(j,ia),ymeanline(j,ia),zmeanline(j,ia)
	   ! print*,xprd_top(j,ia),yprd_top(j,ia),zprd_top(j,ia)
	   ! print*,xprd_bot(j,ia),yprd_bot(j,ia),zprd_bot(j,ia)
	   ! stop
	enddo
	! Axial points:
    ! do j = npts_slope-1, 1, -1
	   ! xmeanline(j,ia) = -scf*coefficient(npts+1-j) + xmeanline(npts_slope,ia)
	   ! ymeanline(j,ia) = scf*coefficient(npts_slope-1) + ymeanline(npts_slope,ia)! add factor (scf*coefficient(npts_slope-1)) to make transition smooth
	   ! zmeanline(j,ia) = zmeanline(npts_slope,ia)
	   !! top periodic wall coordinates
	   ! xprd_top(j,ia) = -scf*coefficient(npts+1-j) + xprd_top(npts_slope,ia)
	   ! yprd_top(j,ia) = yprd_top(npts_slope,ia)
	   ! zprd_top(j,ia) = zprd_top(npts_slope,ia)
	   !! bot periodic wall coordinates
	   ! xprd_bot(j,ia) = -scf*coefficient(npts+1-j) + xprd_bot(npts_slope,ia)
	   ! yprd_bot(j,ia) = yprd_bot(npts_slope,ia)
	   ! zprd_bot(j,ia) = zprd_bot(npts_slope,ia)
	 ! enddo
	!!
   !------------------------------------------------------------------------------- 
   !!downstream coordinates
   !-------------------------------------------------------------------------------
	! Constant slope
    do j = 1 , npts
	   xmeanline(uplmt+j,ia) = scf*coefficient(j) + scf*xb(iap,ia)
	   ymeanline(uplmt+j,ia) = (xmeanline(uplmt+j,ia)-xmeanline(uplmt+j-1,ia))*tan(phi_xy_dwn)+ymeanline(uplmt+j-1,ia)
	   zmeanline(uplmt+j,ia) = (xmeanline(uplmt+j,ia)-xmeanline(uplmt+j-1,ia))*tan(phi_xz_dwn)+zmeanline(uplmt+j-1,ia)
	   !top periodic wall coordinates
	   xprd_top(uplmt+j,ia) = scf*coefficient(j) + scf*xposlean(iap,ia)
	   yprd_top(uplmt+j,ia) = (xprd_top(uplmt+j,ia)-xprd_top(uplmt+j-1,ia))*tan(phi_xy_dwn)+yprd_top(uplmt+j-1,ia)
	   zprd_top(uplmt+j,ia) = (xprd_top(uplmt+j,ia)-xprd_top(uplmt+j-1,ia))*tan(phi_xz_dwn)+zprd_top(uplmt+j-1,ia)
	   !bot periodic wall coordinates
	   xprd_bot(uplmt+j,ia) = scf*coefficient(j) + scf*xneglean(iap,ia)
	   yprd_bot(uplmt+j,ia) = (xprd_bot(uplmt+j,ia)-xprd_bot(uplmt+j-1,ia))*tan(phi_xy_dwn)+yprd_bot(uplmt+j-1,ia)
	   zprd_bot(uplmt+j,ia) = (xprd_bot(uplmt+j,ia)-xprd_bot(uplmt+j-1,ia))*tan(phi_xz_dwn)+zprd_bot(uplmt+j-1,ia)
	enddo
	! Axial points:
    ! do j = npts_slope+1, npts	
	   ! xmeanline(uplmt+j,ia) = scf*coefficient(j) + xmeanline(uplmt+npts_slope,ia)
	   ! ymeanline(uplmt+j,ia) = ymeanline(uplmt+npts_slope,ia)
	   ! zmeanline(uplmt+j,ia) = zmeanline(uplmt+npts_slope,ia)
	   ! !top periodic wall coordinates
	   ! xprd_top(uplmt+j,ia) = scf*coefficient(j) + xprd_top(uplmt+npts_slope,ia)
	   ! yprd_top(uplmt+j,ia) = yprd_top(uplmt+npts_slope,ia)
	   ! zprd_top(uplmt+j,ia) = zprd_top(uplmt+npts_slope,ia)
	   ! !bot periodic wall coordinates
	   ! xprd_bot(uplmt+j,ia) = scf*coefficient(j) + xprd_bot(uplmt+npts_slope,ia)
	   ! yprd_bot(uplmt+j,ia) = yprd_bot(uplmt+npts_slope,ia)
	   ! zprd_bot(uplmt+j,ia) = zprd_bot(uplmt+npts_slope,ia)
	! enddo
   !-------------------------------------------------------------------------------
   ! Writing to external files.
   !-------------------------------------------------------------------------------
   ! Writing meanline coordinates.
   write(temp,*)ia
   fname2 = 'meanline.sec'//trim(adjustl(temp))//'.'//trim(casename)//'.dat'
   fname4 = 'meanline.sec'//trim(adjustl(temp))//'.'//trim(casename)//'.csv'
   !
   open(4,file = fname2,form = 'formatted')
   !open(5,file = fname4,form = 'formatted')
   !
   do i = 1,uplmt+npts
      write(4,*)xmeanline(i,ia),ymeanline(i,ia),zmeanline(i,ia)
   !   write(5,11)xmeanline(i,ia),',',ymeanline(i,ia),',',zmeanline(i,ia)
   enddo  
   close(4)
   !close(5)
   
   !-------------------------------------------------------------------------------
   ! Writing Top and Bottom periodic wall coordinates.
   ! fname6 = 'top_periodic.sec'//trim(adjustl(temp))//'.'//trim(casename)//'.csv'
   ! fname7 = 'bot_periodic.sec'//trim(adjustl(temp))//'.'//trim(casename)//'.csv'
   ! open(7,file = fname6,form = 'formatted')
   ! open(8,file = fname7,form = 'formatted')
   ! do i = 1, uplmt+npts
      ! write(7,11)xprd_top(i,ia),',',yprd_top(i,ia),',',zprd_top(i,ia)
      ! write(8,11)xprd_bot(i,ia),',',yprd_bot(i,ia),',',zprd_bot(i,ia)  
   ! enddo 
   ! close(7)
   ! close(8)
enddo   
write(*,*)

!*********************************************************************************************
! Mapping of the meanline on the non offsetted hub streamline for solid disc creation purpose:
!*********************************************************************************************


!-------------------------------------------------------------------------------
11 format((f20.16),A,(f20.16),A,(f20.16))
!-------------------------------------------------------------------------------

return
end subroutine constantslopemeanline3D

!*******************************************************************************************



!
! Subroutine for a uniform ckustering of u before starting blade generation
!
!*******************************************************************************************
subroutine uniform_clustering(np,u)
    use file_operations
    implicit none

    integer,                    intent(in)          :: np
    real(kind = 8),             intent(inout)       :: u(*)

    ! Local variables
    integer                                         :: i, nopen
    character(len = :), allocatable                 :: log_file
    logical                                         :: file_open


    ! Print output to screen and write to log file
    call log_file_exists(log_file, nopen, file_open)
    print *, 'Using uniform clustering distribution'
    print *, ''
    write(nopen,*) 'Using uniform clustering distribution'
    write(nopen,*) ''
    call close_log_file(nopen, file_open)


    ! Uniform clustering of u from u = 0.0 to u = 1.0
    do i = 1,np
        u(i)        = real(i - 1,8)/real(np - 1,8)
    end do


end subroutine uniform_clustering
!*******************************************************************************************


!
! Subroutine for a sine function based clustering of u before starting blade generation
!
!*******************************************************************************************
subroutine sine_clustering(np,u,clustering_parameter)
    use file_operations
    implicit none

    integer,                    intent(in)          :: np 
    real(kind = 8),             intent(inout)       :: u(*)
    real(kind = 8),             intent(inout)       :: clustering_parameter

    ! Local variables
    integer                                         :: i, nopen
    real(kind = 8)                                  :: ui, du, pi
    character(len = :),     allocatable             :: log_file
    logical                                         :: file_open


    ! Print output to screen and write to log file
    call log_file_exists(log_file, nopen, file_open)
    print *, 'Using sine function based clustering distribution'
    print *, ''
    write(nopen,*) 'Using sine function based clustering distribution'
    write(nopen,*) ''
    call close_log_file(nopen, file_open)


    ! First element of u is set to zero
    u(1)            = 0.0
    pi              = real(4.0*atan(1.0),8)
    
    ! Cluster u    
    do i = 2,np
        
        ui          = real(i - 1)/real(np)
        du          = (sin(pi*ui))**clustering_parameter
        u(i)        = u(i - 1) + du

    end do


end subroutine sine_clustering
!*******************************************************************************************



!
! Subroutine for exponential clustering of u before starting blade generation 
!
!*******************************************************************************************
subroutine exponential_clustering(np,u,clustering_parameter)
    use file_operations
    implicit none

    integer,                    intent(in)          :: np
    real(kind = 8),             intent(inout)       :: u(*)
    real(kind = 8),             intent(inout)       :: clustering_parameter

    ! Local variables
    integer                                         :: np1, i, j, nopen
    real(kind = 8), allocatable                     :: xi(:), u_temp(:)
    character(len = :), allocatable                 :: log_file
    logical                                         :: file_open


    ! Print output to screen and write to log file
    call log_file_exists(log_file, nopen, file_open)
    print *, 'Using exponential function based clustering distribution'
    print *, ''
    write(nopen,*) 'Using exponential function based clustering distribution'
    write(nopen,*) ''
    call close_log_file(nopen, file_open)


    ! np1 is the size of the stretched arrays from 0.0 to 0.5 and 0.5 to 1.0
    np1                 = (np + 1)/2

    !
    ! Generate stretching coordinate xi with a uniform distribution from
    ! xi = 0.0 to xi = 1.0
    !
    if (allocated(xi)) deallocate(xi)
    allocate(xi(np1))

    do i = 1,np1
        xi(i)           = real(i - 1,8)/real(np1 - 1,8)
    end do

    !
    ! Generate temporary cluster from u = 0.0 to u = 0.5
    ! This cluster will be reversed and used to generate cluster from u = 0.5 to u = 1.0
    !
    if (allocated(u_temp)) deallocate(u_temp)
    allocate(u_temp(np1))

    do i = 1,np1
        u_temp(i)       = (exp(clustering_parameter*xi(i)) - 1.0)/(exp(clustering_parameter) - 1.0)
        u_temp(i)       = 0.5*u_temp(i)
    end do

    !
    ! Populate u from u = 0.0 to u = 1.0
    !
    do i = 1,np1
        u(i)            = u_temp(i)
    end do
    do i = 1,np1 - 1
        j               = np1 + i
        u(j)            = 1.0 - u_temp(np1 - i)
    end do


end subroutine exponential_clustering
!*******************************************************************************************



!
! Subroutine for hyperbolic tangent clustering of u before starting blade generation
!
!*******************************************************************************************
subroutine hyperbolic_tan_clustering(np,u,clustering_parameter)
    use file_operations
    implicit none

    integer,                    intent(in)          :: np
    real(kind = 8),             intent(inout)       :: u(*)
    real(kind = 8),             intent(inout)       :: clustering_parameter

    ! Local variables
    integer                                         :: np1, i, j, nopen
    real(kind = 8), allocatable                     :: xi(:), u_temp(:), temp(:)
    character(len = :), allocatable                 :: log_file
    logical                                         :: file_open


    ! Print output to screen and write to log file
    call log_file_exists(log_file, nopen, file_open)
    print *, 'Using hyperbolic tangent function based clustering'
    print *, ''
    write(nopen,*) 'Using hyperbolic tangent function based clustering'
    write(nopen,*) ''
    call close_log_file(nopen, file_open)


    ! np1 is the size of the stretched arrays from 0.0 to 0.5 and 0.5 to 1.0
    np1                 = (np + 1)/2

    !
    ! Generate stretching coordinate xi with a uniform distribution from
    ! xi = 0.0 to xi = 1.0
    !
    if (allocated(xi)) deallocate(xi)
    allocate(xi(np1))

    do i = 1,np1
        xi(i)           = real(i - 1,8)/real(np1 - 1,8)
    end do

    !
    ! Generate temporary cluster from u = 0.0 to u = 0.5
    ! This cluster will be reversed and used to generate cluster from u = 0.5 to u = 1.0
    !
    if (allocated(u_temp)) deallocate(u_temp)
    allocate(u_temp(np1))
    if (allocated(temp)) deallocate(temp)
    allocate(temp(np1))

    do i = 1,np1

        temp(i)         = 1.0 + (tanh(0.5*clustering_parameter*xi(i))/tanh(0.5*clustering_parameter))
        temp(i)         = 0.5*temp(i)

    end do

    ! Generate cluster from u = 0.0 to u = 0.5
    do i = 1,np1
        u_temp(i)       = 1.0 - temp(np1 + 1 - i)
    end do

    !
    ! Populate u from u = 0.0 to u = 1.0
    !
    do i = 1,np1
        u(i)            = u_temp(i)
    end do
    do i = 1,np1 - 1
        j               = np1 + i
        u(j)            = 1.0 - u_temp(np1 - i)
    end do


end subroutine hyperbolic_tan_clustering
!*******************************************************************************************



!
! Generate LE ellipse
!
!*******************************************************************************************
subroutine LE_ellipse(np,ncp,xcp_thk,ycp_thk,np_cluster,x_ellip_LE,y_ellip_LE)
    implicit none

    integer,                    intent(in)          :: np, ncp
    real,                       intent(in)          :: xcp_thk(ncp), ycp_thk(ncp)  
    integer,                    intent(inout)       :: np_cluster
    real,                       intent(inout)       :: x_ellip_LE(*), y_ellip_LE(*)
    
    ! Local variables
    real                                            :: x_1_LE,y_1_LE,x_2_LE,y_2_LE,x_3_LE,  &
                                                       y_3_LE,x_center_LE,y_center_LE,a_LE, &
                                                       b_LE
    integer                                         :: np_ellip_LE
    real,       allocatable                         :: t_LE(:)
    real,       parameter                           :: pi = 4.0*atan(1.0)
    integer                                         :: i, j, k


    !
    ! Define ellipse end points and center for the LE
    !
    ! 1 - top endpoint of the minor axis of the LE ellipse (y = +b)
    ! 2 - bottom endpoint of the minor axis of the LE ellipse (y = -b)
    ! 3 - left endpoint of the major axis of the LE ellipse (x = -a)
    ! center - center of the LE ellipse
    !
    x_1_LE                      = xcp_thk(1)
    y_1_LE                      = ycp_thk(1)
    x_2_LE                      = xcp_thk(1)
    y_2_LE                      = -ycp_thk(1)
    x_3_LE                      = 0.0
    y_3_LE                      = 0.0
    x_center_LE                 = xcp_thk(1)
    y_center_LE                 = 0.0


    ! Define major and minor axis of the LE ellipse
    a_LE                        = sqrt((x_center_LE - x_3_LE)**2 + (y_center_LE - y_3_LE)**2)
    b_LE                        = sqrt((x_center_LE - x_1_LE)**2 + (y_center_LE - y_1_LE)**2)


    !
    ! Define parameter space for the LE ellipse
    !
    if (allocated(t_LE)) deallocate(t_LE)
    allocate(t_LE(2*np_cluster - 1))
    do i = 1,size(t_LE)

        t_LE(i)                 = (pi*(real(i - 1)/real(size(t_LE) - 1))) + (0.5*pi)

    end do

    ! Generate LE ellipse
    do i = 1,size(t_LE)

        x_ellip_LE(i)           = a_LE*(1.0 + cos(t_LE(i)))
        y_ellip_LE(i)           = b_LE*sin(t_LE(i))

    end do


    !
    ! Ensure matching of ellipse endpoints with generated ellipse
    ! Floating point operations cause slight difference in endpoints
    !
    x_ellip_LE(1)               = x_1_LE;       y_ellip_LE(1)               = y_1_LE
    x_ellip_LE(np_cluster)      = x_3_LE;       y_ellip_LE(np_cluster)      = y_3_LE
    x_ellip_LE(size(t_LE))      = x_2_LE;       y_ellip_LE(size(t_LE))      = y_2_LE


end subroutine LE_ellipse
!*******************************************************************************************



!
! Generate TE ellipse
!
!*******************************************************************************************
subroutine TE_ellipse(np,ncp,xcp_thk,ycp_thk,np_cluster,x_ellip_TE,y_ellip_TE)
    implicit none

    integer,                    intent(in)          :: np, ncp
    real,                       intent(in)          :: xcp_thk(ncp), ycp_thk(ncp)
    integer,                    intent(in)          :: np_cluster
    real,                       intent(inout)       :: x_ellip_TE(*), y_ellip_TE(*)

    ! Local variables
    real                                            :: x_1_TE,y_1_TE,x_2_TE,y_2_TE,x_3_TE,  &
                                                       y_3_TE,x_center_TE,y_center_TE,a_TE, &
                                                       b_TE
    integer                                         :: np_ellip_TE
    real,       allocatable                         :: t_TE(:)
    real,       parameter                           :: pi = 4.0*atan(1.0)
    integer                                         :: i, j, k


    ! 
    ! Define ellipse endpoints and center for the TE ellipse
    !
    ! 1 - top endpoint of the minor axis of the TE ellipse (y = +b)
    ! 2 - bottom endpoint of the minor axis of the TE ellipse (y = -b)
    ! 3 - rght endpoint of the major axis of the TE ellipse (x = +a)
    ! center - center of the TE ellipse
    !
    x_1_TE                      = xcp_thk(ncp)
    y_1_TE                      = ycp_thk(ncp)
    x_2_TE                      = xcp_thk(ncp)
    y_2_TE                      = -ycp_thk(ncp)
    x_3_TE                      = 1.0
    y_3_TE                      = 0.0
    x_center_TE                 = xcp_thk(ncp)
    y_center_TE                 = 0.0


    ! Define major and minor axis of the TE ellipse
    a_TE                        = sqrt((x_center_TE - x_3_TE)**2 + (y_center_TE - y_3_TE)**2)
    b_TE                        = sqrt((x_center_TE - x_1_TE)**2 + (y_center_TE - y_1_TE)**2)


    !
    ! Define parameter space for the TE ellipse
    !
    if (allocated(t_TE)) deallocate(t_TE)
    allocate(t_TE(2*np_cluster - 1))
    do i = 1,size(t_TE)

        t_TE(i)                 = (pi*(real(size(t_TE) - 1 - i + 1)/real(size(t_TE) - 1))) - 0.5*pi

    end do


    ! Generate TE ellipse
    do i = 1,size(t_TE)

        x_ellip_TE(i)           = x_center_TE + (a_TE*cos(t_TE(i)))
        y_ellip_TE(i)           = b_TE*sin(t_TE(i))

    end do


    !
    ! Ensure matching of ellipse endpoints with the generated ellipse
    ! Floating point operations cause slight differences in endpoints
    !
    x_ellip_TE(1)               = x_1_TE;   y_ellip_TE(1)               = y_1_TE
    x_ellip_TE(np_cluster)      = x_3_TE;   y_ellip_TE(np_cluster)      = y_3_TE
    x_ellip_TE(size(t_TE))      = x_2_TE;   y_ellip_TE(size(t_TE))      = y_2_TE


end subroutine TE_ellipse
!*******************************************************************************************



!
! Cluster the part of the blade between the LE and the TE
!
!*******************************************************************************************
subroutine cluster_mid(u_begin,u_end,np_mid,u_mid)
    implicit none

    real,                       intent(in)          :: u_begin, u_end
    integer,                    intent(in)          :: np_mid
    real,                       intent(inout)       :: u_mid(np_mid)

    ! Local variables
    integer                                         :: i,j,k
    real                                            :: delta_u


    delta_u         = abs(u_end - u_begin)

    do i =  1,np_mid

        u_mid(i)    = u_begin + (delta_u*(real(i - 1)/real(np_mid - 1)))

    end do

    u_mid(1)        = u_begin
    u_mid(np_mid)   = u_end


end subroutine cluster_mid
!*******************************************************************************************



!
! Define hyperbolic clustering function for LE side mid clustering
! This function is used in the bisection method when solving for the LE side
! clustering parameter
!
!*******************************************************************************************
real function LE_clustering_parameter_func(K,xi,func_coordinate) result(func)
    implicit none

    real,                       intent(in)          :: K
    real,                       intent(in)          :: xi
    real,                       intent(in)          :: func_coordinate

    
    func            = K + ((tanh(0.5*func_coordinate*(xi - 1.0)))/(tanh(0.5*func_coordinate)))


end function LE_clustering_parameter_func
!*******************************************************************************************



!
! Define hyperbolic clustering function for TE side mid clustering
! This function is used in the bisection method when solving for the TE side
! clustering parameter
!
!*******************************************************************************************
real function TE_clustering_parameter_func(K,xi,func_coordinate) result(func)
    implicit none

    real,                       intent(in)          :: K
    real,                       intent(in)          :: xi
    real,                       intent(in)          :: func_coordinate


    func            = K - ((tanh(0.5*func_coordinate*xi))/(tanh(0.5*func_coordinate)))


end function TE_clustering_parameter_func
!*******************************************************************************************



!
! Bisection solver for the LE side clustering parameter
!
!*******************************************************************************************
subroutine LE_clustering_parameter_solver(xi,K,delta,solver_flag)
    use file_operations
    implicit none

    real,                       intent(in)          :: xi
    real,                       intent(in)          :: K
    real,                       intent(inout)       :: delta
    logical,                    intent(inout)       :: solver_flag

    ! Local variables
    real                                            :: a, b, c, f1, f2, f3, &
                                                       tol = 10E-6
    integer                                         :: i, j, nopen, niter
    character(:),   allocatable                     :: log_file
    logical                                         :: file_open
    interface LE_clustering_parameter_func
        real function LE_clustering_parameter_func(Kf,xif,func_coordinate) 
            real                                    :: Kf
            real                                    :: xif
            real                                    :: func_coordinate
        end function LE_clustering_parameter_func
    end interface


    ! Define initial bisection interval "[a,b]" and interval midpoint "c"
    ! Compute function values at a, b and c
    a               = 0.05
    b               = 10.0
    c               = 0.5*(a + b)
    f1              = LE_clustering_parameter_func(K,xi,a)
    f2              = LE_clustering_parameter_func(K,xi,b)
    f3              = LE_clustering_parameter_func(K,xi,c)
   
    call log_file_exists(log_file, nopen, file_open)

    ! Bisection interval should contain a zero
    if ((f1 < 0 .and. f2 > 0) .or. (f1 > 0 .and. f2 < 0)) then

        ! Iteration counter
        niter       = 0    
        do while (niter .le. 40)
        
            ! Determine whether sign(a) = sign(c) or
            !                   sign(b) = sign(c) and
            ! reinterpret bisection interval
            if ((f1 < 0 .and. f3 < 0) .or. (f1 > 0 .and. f3 > 0)) then
                a   = c
            else if ((f2 < 0 .and. f3 < 0) .or. (f2 > 0 .and. f3 > 0)) then
                b   = c
            end if
      
            ! Compute new midpoint c and new function values 
            c       = 0.5*(a + b)
            f1      = LE_clustering_parameter_func(K,xi,a)
            f2      = LE_clustering_parameter_func(K,xi,b)
            f3      = LE_clustering_parameter_func(K,xi,c)

            ! Update iteration counter
            niter   = niter + 1

            ! Exit condition
            if (abs(f3) < tol) exit
        
        end do

    else
        print *, "WARNING: Could not find initial guesses for the clustering_parameter bisection solver"
        print *, "WARNING: Returning to uniform midchord clustering"
        write(nopen,*) 'WARNING: Could not find initial guesses for the clustering_parameter bisection solver'
        write(nopen,*) 'WARNING: Returning to uniform midchord clustering'
        delta       = 0.0
        solver_flag = .false.
    end if
    call close_log_file(nopen, file_open)

    ! Set clustering_parameter
    delta       = c
    solver_flag = .true.


end subroutine LE_clustering_parameter_solver
!*******************************************************************************************



!
! Bisection solver for TE side clustering parameter
!
!*******************************************************************************************
subroutine TE_clustering_parameter_solver(xi,K,delta,solver_flag)
    use file_operations
    implicit none

    real,                       intent(in)          :: xi
    real,                       intent(in)          :: K
    real,                       intent(inout)       :: delta
    logical,                    intent(inout)       :: solver_flag

    ! Local variables
    real                                            :: a, b, c, f1, f2, f3, &
                                                       tol = 10E-6
    integer                                         :: i, j, nopen, niter
    character(:),   allocatable                     :: log_file
    logical                                         :: file_open
    interface TE_clustering_parameter_func
        real function TE_clustering_parameter_func(Kf,xif,func_coordinate)
            real                                    :: Kf
            real                                    :: xif
            real                                    :: func_coordinate
        end function TE_clustering_parameter_func
    end interface
    
    
    ! Define initial bisection interval "[a,b]" and interval midpoint "c"
    ! Compute function values at a, b and c
    a               = 0.05
    b               = 10.0
    c               = 0.5*(a + b)
    f1              = TE_clustering_parameter_func(K,xi,a)
    f2              = TE_clustering_parameter_func(K,xi,b)
    f3              = TE_clustering_parameter_func(K,xi,c)

    call log_file_exists(log_file, nopen, file_open)
    
    ! Bisection interval should contain a zero    
    if ((f1 < 0. .and. f2 > 0.) .or. (f1 > 0. .and. f2 < 0.)) then

        ! Iteration counter
        niter       = 0
        do while (niter .le. 40) 

            ! Determine whether sign(a) = sign(c) or
            !                   sign(b) = sign(c) and
            ! reinterpret bisection interval
            if ((f1 < 0 .and. f3 < 0) .or. (f1 > 0 .and. f3 > 0)) then
                a   = c
            else if ((f2 < 0 .and. f3 < 0) .or. (f2 > 0 .and. f3 > 0)) then
                b   = c
            end if

            ! Compute new midpoint c and new function values
            c       = 0.5*(a + b)
            f1      = TE_clustering_parameter_func(K,xi,a)
            f2      = TE_clustering_parameter_func(K,xi,b)
            f3      = TE_clustering_parameter_func(K,xi,c)

            ! Update iteration counter
            niter   = niter + 1
            
            ! Exit condition
            if (abs(f3) < tol) exit
                
        end do 

    else
        print *, 'WARNING: Could not find initial guesses for the TE clustering_parameter bisection solver'
        print *, 'WARNING: Returning to uniform midchord clustering'
        write(nopen,*) 'WARNING: Could not find initial guesses for the TE clustering_parameter bisection solver'
        write(nopen,*) 'wARNING: Returning to uniform midchord clustering'
        delta       = 0.0
        solver_flag = .false.
    end if
    call close_log_file(nopen, file_open)

    ! Set clustering parameter
    delta           = c
    solver_flag     = .true.


end subroutine TE_clustering_parameter_solver
!*******************************************************************************************



! Add hyperbolic clustering for LE side middle part
!
!*******************************************************************************************
subroutine mid_hyperbolic_clustering(np_cluster,np_mid,u_LE,u_TE,u_mid)
    use file_operations
    implicit none

    integer,                    intent(in)          :: np_cluster
    integer,                    intent(in)          :: np_mid
    real,                       intent(in)          :: u_LE(np_cluster)
    real,                       intent(in)          :: u_TE(np_cluster)
    real,                       intent(inout)       :: u_mid(np_mid)

    ! Local variables
    integer                                         :: i, j, np_mid_LE, np_mid_TE, nopen
    real,   allocatable                             :: xi(:), u_mid_LE(:), u_mid_TE(:)
    real                                            :: du_LE, u_mid_pt, du_mid_LE, du_mid_TE, K_LE, delta_LE, &
                                                       du_TE, K_TE, delta_TE
    character(:),   allocatable                     :: log_file
    logical                                         :: solver_flag_LE, solver_flag_TE, file_open


    ! Calculate array sizes
    np_mid_LE   = (np_mid + 1)/2
    np_mid_TE   = np_mid_LE


    ! du_LE     - distance between the last two LE points
    ! du_mid_LE - distance between blade mid point and LE
    du_LE       = abs(u_LE(np_cluster) - u_LE(np_cluster - 1))
    du_TE       = abs(u_TE(2) - u_TE(1))
    u_mid_pt    = 0.5*(u_LE(np_cluster) + u_TE(1))
    du_mid_LE   = abs(u_mid_pt - u_LE(np_cluster))
    du_mid_TE   = abs(u_TE(1) - u_mid_pt)


    ! Compute the uniform reference space xi
    ! The second xi value is used for the clustering parameter equation
    ! Equation defined in LE_clustering_parameter_func
    if (allocated(xi)) deallocate(xi)
    allocate(xi(np_mid_LE))
    do i = 1,np_mid_LE
        xi(i)   = real(i - 1,8)/real(np_mid_LE - 1,8)
    end do


    ! Compute the equation constant for the clustering parameter equation
    ! Equation defined in LE_clustering_parameter_func
    K_LE        = 1.0 - (du_LE/du_mid_LE)
    K_TE        = 1.0 - (du_TE/du_mid_TE)


    ! Solve the clustering parameter equation
    ! TODO: Add Newton's solver
    call LE_clustering_parameter_solver(xi(2),K_LE,delta_LE,solver_flag_LE)
    call TE_clustering_parameter_solver(xi(np_mid_LE - 1),K_TE,delta_TE,solver_flag_TE)


    if (solver_flag_LE .and. solver_flag_TE) then

        call log_file_exists(log_file, nopen, file_open)
        print *, 'Hyperbolic midchord clustering with delta_LE = ', delta_LE
        print *, 'Hyperbolic midchord clustering with delta_TE = ', delta_TE
        write(nopen,*) 'Hyperbolic midchord clustering with delta_TE = ', delta_LE 
        write(nopen,*) 'Hyperbolic midchord clustering with delta_TE = ', delta_TE
        call close_log_file(nopen, file_open)
               
        ! Cluster u_LE_mid
        if (allocated(u_mid_LE)) deallocate(u_mid_LE)
        allocate(u_mid_LE(np_mid_LE))
        u_mid_LE(1) = u_LE(np_cluster)
        do i = 2,np_mid_LE

            u_mid_LE(i) = u_mid_LE(1) + (du_mid_LE*(1.0 + ((tanh(0.5*delta_LE*(xi(i) - 1.0)))/(tanh(0.5*delta_LE)))))

        end do
        u_mid_LE(np_mid_LE) = u_mid_pt


        ! Cluster u_TE_mid
        if (allocated(u_mid_TE)) deallocate(u_mid_TE)
        allocate(u_mid_TE(np_mid_TE))
        u_mid_TE(1) = u_mid_pt
        do i = 2,np_mid_TE

            u_mid_TE(i) = u_mid_TE(1) + (du_mid_TE*((tanh(0.5*delta_TE*xi(i)))/(tanh(0.5*delta_TE))))

        end do
        u_mid_TE(np_mid_TE) = u_TE(1)


        ! Generate u_mid
        do i = 1,np_mid_LE

            u_mid(i)    = u_mid_LE(i)

        end do
        do i = 2,np_mid_TE

            u_mid(np_mid_LE + i - 1)    = u_mid_TE(i)

        end do

    else
        
        ! If clustering parameter for midchord hyperbolic clustering is not found
        ! use uniform clustering
        call cluster_mid(u_LE(np_cluster),u_TE(1),np_mid,u_mid)

    end if


end subroutine mid_hyperbolic_clustering
!*******************************************************************************************



!
! Add elliptical clustering for the LE and TE
!
!*******************************************************************************************
subroutine elliptical_clustering(js,np,nsl,ncp,thk_cp,np_cluster,u_new)
    use file_operations
    implicit none

    integer,                    intent(in)          :: js, np, nsl
    integer,                    intent(in)          :: ncp
    real,                       intent(in)          :: thk_cp(20, 2*nsl)
    integer,                    intent(in)          :: np_cluster
    real,                       intent(inout)       :: u_new(np)

    ! Local variables
    integer                                         :: i, j, k, np_mid, nopen
    real,           allocatable                     :: xcp_thk(:), ycp_thk(:)
    real,           allocatable                     :: x_ellip_LE(:), y_ellip_LE(:), &
                                                       x_ellip_TE(:), y_ellip_TE(:), &
                                                       u_LE(:), u_TE(:), u_mid(:)
    character(:),   allocatable                     :: log_file
    logical                                         :: file_open


    ! Print to screen and write to log file
    print *, 'Using ellipse based clustering function'
    print *, ''
    call log_file_exists(log_file, nopen, file_open)
    write(nopen,*) 'Using ellipse based clustering function'
    write(nopen,*) ''
    call close_log_file(nopen, file_open)


    !
    ! Read thickness control points for the js blade section
    !
    if (allocated(xcp_thk) .and. allocated(ycp_thk)) deallocate(xcp_thk,ycp_thk)
    allocate(xcp_thk(ncp), ycp_thk(ncp))
    do i = 1,ncp

        xcp_thk(i)              = thk_cp(i,2*js - 1)
        ycp_thk(i)              = thk_cp(i,2*js)

    end do


    ! Generate LE and TE ellipses
    if (allocated(x_ellip_LE) .and. allocated(y_ellip_LE)) deallocate(x_ellip_LE,y_ellip_LE)
    allocate(x_ellip_LE(2*np_cluster - 1), y_ellip_LE(2*np_cluster - 1))
    call LE_ellipse(np,ncp,xcp_thk,ycp_thk,np_cluster,x_ellip_LE,y_ellip_LE)

    if (allocated(x_ellip_TE) .and. allocated(y_ellip_TE)) deallocate(x_ellip_TE,y_ellip_TE)
    allocate(x_ellip_TE(2*np_cluster - 1), y_ellip_TE(2*np_cluster - 1))
    call TE_ellipse(np,ncp,xcp_thk,ycp_thk,np_cluster,x_ellip_TE,y_ellip_TE)


    ! Store u values for LE and TE
    if (allocated(u_LE) .and. allocated(u_TE)) deallocate(u_LE,u_TE)
    allocate(u_LE(np_cluster), u_TE(np_cluster))
    do i = 1,np_cluster

        u_LE(i)     = x_ellip_LE(np_cluster - i + 1)
        u_TE(i)     = x_ellip_TE(i)

    end do
    

    ! Cluster the middle part of the blade section using
    ! Use hyperbolic clustering
    np_mid  = np - (2*np_cluster) + 2
    if (allocated(u_mid)) deallocate(u_mid)
    allocate(u_mid(np_mid))
    call mid_hyperbolic_clustering(np_cluster,np_mid,u_LE,u_TE,u_mid)


    ! Generate u_new by combining u_LE, u_mid and u_TE
    do i = 1,np_cluster

        u_new(i)                 = u_LE(i)

    end do
    do i = 1,np_mid - 2

        u_new(np_cluster + i)    = u_mid(i + 1)
        j                        = np_cluster + i

    end do
    do i = 1,np_cluster

        u_new(j + i)             = u_TE(i)

    end do


end subroutine elliptical_clustering
!*******************************************************************************************



!
! Interpolate trailing edge angle
!
! Input parameters: u_max = chordwise location of max. thickness for the blade section
!
! Reference: Abbott, I.H., van Doenhoff, A.E., "Families of Wing Sections", Theory of Wing
!            Sections, Dover Publications, New York, 1999, pp. 116-118
!
!*******************************************************************************************
subroutine compute_TE_angle(u_max,trail_angle)
    use file_operations
    implicit none

    real,                       intent(in)      :: u_max
    real,                       intent(inout)   :: trail_angle


    trail_angle = 0.775 + (2.51666667*u_max) + (-13.625*(u_max**2)) + (35.83333333*(u_max**3)) &
                  + (-12.5*(u_max**4))


end subroutine compute_TE_angle
!*******************************************************************************************



!
! Compute coefficients of modified NACA four-digit thickness
!
! For u < u_max: y_t = a_0*sqrt(u) + a_1*u + a_2*(u**2) + a_3*(u**3)
! For u > u_max: y_t = d_0 + d_1*(1 - u) + d_2*((1 - u)**2) + d_3*((1 - u)**3)
!
! Input parameters: t_max    = half max. thickness for the blade section in fraction chord
!                   u_max    = chordwise location of max. thickness for the blade section
!                   dy_dx_TE = trailing edige angle
!                   I        = integer parameter governing roundedness of the leading edge
!                              (default = 6, sharp LE = 0)
!
! Reference: Abbott, I.H, von Doenhoff, A.E., "Families of Wing Sections", Theory of Wing
!            Sections, Dover Publications, New York, 1999, pp. 116-118
!
!*******************************************************************************************
subroutine modified_NACA_four_digit_thickness_coeffs(t_max,u_max,TE_thk,dy_dx_TE,I,a,d)
    use file_operations
    implicit none

    real,                       intent(in)      :: t_max
    real,                       intent(in)      :: u_max
    real,                       intent(in)      :: TE_thk
    real,                       intent(in)      :: dy_dx_TE
    real,                       intent(in)      :: I
    real,                       intent(inout)   :: a(4)
    real,                       intent(inout)   :: d(4)

    ! Local variables
    integer                                     :: j, k, fail_flag, nopen
    real                                        :: temp
    real,           allocatable                 :: aug_matrix(:,:)
    character(:),   allocatable                 :: log_file
    logical                                     :: file_open


    !
    ! Compute d_0 and d_1
    !
    d(1)            = TE_thk!0.02*t_max
    d(2)            = 2.0*dy_dx_TE*t_max
    
    !
    ! Compute d_2 and d_3
    ! Enforce the following conditions: yd_t(u_max)     = t_max
    !                                   yd'_t(u_max)    = 0
    ! Solve the linear system:          A_d_coeffs*d_23 = b_d_coeffs 
    ! with                              d_23            = [d_2, d_3]
    !  
    if (allocated(aug_matrix)) deallocate(aug_matrix)
    allocate(aug_matrix(2,3))

    aug_matrix(1,:) = [(1.0 - u_max)**2 , (1.0 - u_max)**3       , t_max - d(1) - (d(2)*(1.0 - u_max))]
    aug_matrix(2,:) = [2.0*(u_max - 1.0), -3.0*((u_max - 1.0)**2), d(2)                               ]

    call gauss_jordan(2,1,aug_matrix,fail_flag)

    ! Compute d_2 and d_3
    d(3:)           = aug_matrix(:,3)
    

    !
    ! Compute a_0
    !
    a(1)            = sqrt(2.2038)*(2.0*t_max*(real(I,8)/6.0))

    !
    ! Compute a_1, a_2 and a_3
    ! Enforce the following conditions: ya_t(u_max)         = t_max
    !                                   ya'_t(u_max)        = 0
    !                                   ya''_t(u_max)       = yd''_t(u_max)
    ! Solve the linear system:          A_a_coeffs*a_123    = b_a_coeffs
    ! with                              a_123               = [a_1, a_2, a_3]
    !
    if (allocated(aug_matrix)) deallocate(aug_matrix)
    allocate(aug_matrix(3,4))

    temp            = (2.0*d(3)) + (6.0*(1.0 - u_max)*d(4)) + (0.25*a(1)/(u_max*sqrt(u_max)))
    
    aug_matrix(1,:) = [u_max, u_max**2 , u_max**3      , t_max - (a(1)*sqrt(u_max))]
    aug_matrix(2,:) = [1.0  , 2.0*u_max, 3.0*(u_max**2), (-0.5*a(1)/sqrt(u_max))   ]
    aug_matrix(3,:) = [0.0  , 2.0      , 6.0*u_max     , temp                      ]

    call gauss_jordan(3,1,aug_matrix,fail_flag)

    ! Compute a_1, a_2 and a_3
    a(2:)           = aug_matrix(:,4)


end subroutine modified_NACA_four_digit_thickness_coeffs
!*******************************************************************************************



!
! Compute coefficients of modified NACA four-digit thickness
!
! This subroutine solves for the coefficients with an elliptical TE
!
! For u < u_max: y_t = a_0*sqrt(u) + a_1*u + a_2*(u**2) + a_3*(u**3)
! For u > u_max: y_t = d_0 + d_1*(1 - u) + d_2*((1 - u)**2) + d_3*((1 - u)**3)
!
! Input parameters: t_max     = half max. thickness for the blade section 
!                   u_max     = chordwise location of max. thickness for the blade section
!                   TE_thk    = half thickness at the trailing edge
!                   u_TE      = chordwise location of trailing edge
!                   dy_dx_TE  = trailiing edge angle
!                   LE_radius = radius of the leading edge
!
! Reference: Abbott, I.H., von Doenhoff, A.E., "Families of Wing Sections", Theory of Wing
!            Sections, Dover Publications, New York, 1999, pp. 116-118
!
!*******************************************************************************************
subroutine modified_NACA_four_digit_thickness_coeffs_2(t_max,u_max,t_TE,u_TE,dy_dx_TE, &
                                                       LE_radius, a, d)
    use file_operations
    implicit none

    real,                   intent(in)      :: t_max
    real,                   intent(in)      :: u_max
    real,                   intent(in)      :: t_TE
    real,                   intent(in)      :: u_TE
    real,                   intent(in)      :: dy_dx_TE
    real,                   intent(in)      :: LE_radius
    real,                   intent(inout)   :: a(4)
    real,                   intent(inout)   :: d(4)

    ! Local variables
    integer                                 :: i, j, k, nopen, fail_flag
    real                                    :: temp
    real,           allocatable             :: aug_matrix(:,:)
    character(:),   allocatable             :: log_file
    logical                                 :: file_open


    !
    ! Compute d_0, d_1, d_2 and d_3
    ! Enforce the following conditions: yd_t(u_max)     = t_max
    !                                   yd_t(u_TE)      = t_TE
    !                                   yd'_t(u_max)    = 0.0
    !                                   yd'_t(u_TE)     = dy_dx_TE
    !
    ! Gauss Jordan method used to solve the resulting linear system
    !
    if (allocated(aug_matrix)) deallocate(aug_matrix)
    allocate(aug_matrix(4,5))

    aug_matrix(1,:) = [1.0, 1.0 - u_TE , (1.0 - u_TE)**2  , (1.0 - u_TE)**3        , t_TE    ]
    aug_matrix(2,:) = [1.0, 1.0 - u_max, (1.0 - u_max)**2 , (1.0 - u_max)**3       , t_max   ]
    aug_matrix(3,:) = [0.0, -1.0       , 2.0*(u_TE - 1.0) , -3.0*((1.0 - u_TE)**2) , dy_dx_TE]
    aug_matrix(4,:) = [0.0, -1.0       , 2.0*(u_max - 1.0), -3.0*((1.0 - u_max)**2), 0.0     ]

    call gauss_jordan(4,1,aug_matrix,fail_flag)

    d               = aug_matrix(:,5)


    ! Compute a_0
    a(1)            = sqrt(2.2038)*(2.0*t_max*LE_radius/6.0)

    !
    ! Compute a_1, a_2 and a_3
    ! Enforce the following conditions: ya_t(u_max)     = t_max
    !                                   ya'_t(u_max)    = 0.0
    !                                   ya''_t(u_max)   = yd''_t(u_max)
    !
    ! Gauss Jordan method used to solve the resulting linear system
    !
    if (allocated(aug_matrix)) deallocate(aug_matrix)
    allocate(aug_matrix(3,4)) 
    
    temp            = (2.0*d(3)) + (6.0*(1.0 - u_max)*d(4)) + (0.25*a(1)/(u_max*sqrt(u_max)))

    aug_matrix(1,:) = [u_max, u_max**2 , u_max**3      , t_max - (a(1)*sqrt(u_max))]
    aug_matrix(2,:) = [1.0  , 2.0*u_max, 3.0*(u_max**2), (-0.5*a(1)/sqrt(u_max))   ]
    aug_matrix(3,:) = [0.0  , 2.0      , 6.0*u_max     , temp                      ]

    call gauss_jordan(3,1,aug_matrix,fail_flag)

    a(2:)           = aug_matrix(:,4)
    
     
end subroutine modified_NACA_four_digit_thickness_coeffs_2
!*******************************************************************************************



!
! Obtain thickness distribution with the computed coefficients a_i and d_i
!
! Input parameters: np    = number of points along the blade section meanline
!                   u     = points along chord
!                   u_max = chordwise location of max. thickness for the blade section
!                   t_max = half max. thickness for the blade section in fraction chord
!                   a     = thickness coefficients obtained in modified_NACA_four_digit_thickness_coeffs
!                   d     = thickness coefficients obtained in modified_NACA_four_digit_thickness_coeffs
!           
!*******************************************************************************************
subroutine modified_NACA_four_digit_thickness(np,u,u_max,t_max,a,d,thk_data)
    use file_operations
    implicit none

    integer,                intent(in)      :: np
    real,                   intent(in)      :: u(np)
    real,                   intent(in)      :: u_max
    real,                   intent(in)      :: t_max
    real,                   intent(in)      :: a(4)
    real,                   intent(in)      :: d(4)
    real,                   intent(inout)   :: thk_data(np,3)
    !real,                   intent(inout)   :: thk_der(np)

    ! Local variables
    integer                                 :: i, j, k
    real                                    :: tol = 10E-8


    ! Compute thickness distribution
    do i = 1,np

        if (abs(u(i) - u_max) .le. tol) then
            thk_data(i,1)   = t_max
        else if (u(i) .lt. u_max) then
            thk_data(i,1)   = (a(1)*sqrt(u(i))) + (a(2)*u(i)) + (a(3)*(u(i)**2)) + (a(4)*(u(i)**3))
        else if (u(i) .gt. u_max) then
            thk_data(i,1)   = d(1) + (d(2)*(1.0 - u(i))) + (d(3)*((1.0 - u(i))**2)) + (d(4)*((1.0 - u(i))**3))
        end if

    end do


    ! Compute first and second derivatives of thickness distribution
    do i = 1,np

        if (abs(u(i) - u_max) .le. tol) then
            thk_data(i,2)   = 0.0
            thk_data(i,3)   = (2.0*d(3)) + (6.0*d(4)*(1.0 - u_max))
        else if (abs(u(i)) .le. tol) then
            thk_data(i,2)   = (0.5*a(1)/sqrt(tol)) + a(2) + (2.0*a(3)*tol) + (3.0*a(4)*(tol**2))
            thk_data(i,3)   = (-0.25*a(1)/((sqrt(tol))**3)) + (2.0*a(3)) + (6.0*a(4)*tol)
        else if (u(i) .lt. u_max) then
            thk_data(i,2)   = (0.5*a(1)/sqrt(u(i))) + a(2) + (2.0*a(3)*u(i)) + (3.0*a(4)*(u(i)**2))
            thk_data(i,3)   = (-0.25*a(1)/((sqrt(u(i)))**3)) + (2.0*a(3)) + (6.0*a(4)*u(i))
        else if (u(i) .gt. u_max) then
            thk_data(i,2)   = -d(2) + (2.0*(u(i) - 1.0)*d(3)) - (3.0*((u(i) - 1.0)**2)*d(4))
            thk_data(i,3)   = (2.0*d(3)) + (6.0*d(4)*(1.0 - u(i)))
        end if

    end do


end subroutine modified_NACA_four_digit_thickness
!*******************************************************************************************



!
! Add circular TE to a NACA four digit thickness airfoil and rescale 
!
!*******************************************************************************************
subroutine add_circular_TE(np,u,np_circ,utop,vtop,ubot,vbot,ptop,pbot,u_new)
    use file_operations
    implicit none

    integer,                intent(in)      :: np
    real,                   intent(in)      :: u(np)
    integer,                intent(in)      :: np_circ
    real,                   intent(in)      :: utop(np)
    real,                   intent(in)      :: vtop(np)
    real,                   intent(in)      :: ubot(np)
    real,                   intent(in)      :: vbot(np)
    real,                   intent(inout)   :: ptop(2, np + ((np_circ - 1)/2))
    real,                   intent(inout)   :: pbot(2, np + ((np_circ - 1)/2))
    real,                   intent(inout)   :: u_new(np + ((np_circ - 1)/2))

    ! Local variables
    integer                                 :: i, j, k, nopen, ntemp
    real                                    :: uTE(np_circ), vTE(np_circ), m_top, m_top_normal, c_top, &
                                               m_bot, m_bot_normal, c_bot, u_center, v_center, radius, &
                                               t_start, t_end, dt, t_TE(np_circ)
    real,               parameter           :: pi = 4.0*atan(1.0)
    character(len = :), allocatable         :: log_file
    logical                                 :: file_open


    
    
    !
    ! Compute slope of normal and intercept for the top curve
    !
    m_top               = (vtop(np) - vtop(np - 1))/(utop(np) - utop(np - 1))
    m_top_normal        = -1.0/m_top
    c_top               = vtop(np) - (m_top_normal*utop(np))
    
    !
    ! Compute slope of normal and intercept for the bottom curve
    !
    m_bot               = (vbot(np) - vbot(np - 1))/(ubot(np) - ubot(np - 1))
    m_bot_normal        = -1.0/m_bot
    c_bot               = vbot(np) - (m_bot_normal*ubot(np))

    !
    ! Find center and radius of TE circle (intersection of top and bottom normals)
    !
    u_center            = (c_top - c_bot)/(m_bot_normal - m_top_normal)
    v_center            = (m_top_normal*u_center) + c_top
    radius              = sqrt((u_center - utop(np))**2 + (v_center - vtop(np))**2)

    !
    ! Determine angular positions of circular TE start and end
    if (atan(m_top_normal) .lt. 0.0) then
        t_start         = atan(m_top_normal) + pi
    else if (atan(m_top_normal) .gt. 0.0) then
        t_start         = atan(m_top_normal)
    end if 

    if (atan(m_bot_normal) .lt. 0.0) then
        t_end           = atan(m_bot_normal)
    else if (atan(m_bot_normal) .gt. 0.0) then
        t_end           = (-0.5*pi) - atan(m_bot_normal)
    end if

    !
    ! Generate parameter space for the circular TE
    ! 
    dt                  = t_start - t_end
    t_TE(1)             = t_start
    do i = 2,np_circ

        t_TE(i)         = t_start - (dt*(real(i - 1)/real(np_circ - 1)))
        
    end do 
    t_TE(np_circ)       = t_end
  
    !
    ! Compute (u,v) coordinates of circular TE 
    ! Set endpoints of circular TE equal to endpoints of bottom and top curves
    !
    do i = 1,np_circ

        uTE(i)          = u_center + (radius*cos(t_TE(i)))
        vTE(i)          = v_center + (radius*sin(t_TE(i)))

    end do
    uTE(1)              = utop(np)
    vTE(1)              = vtop(np)
    uTE(np_circ)        = ubot(np)
    vTE(np_circ)        = vbot(np)
     
    !
    ! Combine the incoming blade section with the circular TE
    !
    do i = 1,np

        ptop(1,i)       = utop(i)
        ptop(2,i)       = vtop(i)
        pbot(1,i)       = ubot(i)
        pbot(2,i)       = vbot(i)
        u_new(i)        = u(i)

    end do
    ntemp               = (np_circ - 1)/2
    do i = 1,ntemp

        ptop(1,np + i)  = uTE(1 + i)
        ptop(2,np + i)  = vTE(1 + i)
        pbot(1,np + i)  = uTE(np_circ - i)
        pbot(2,np + i)  = vTE(np_circ - i)
        u_new(np + i)   = 0.5*(ptop(1,np + i) + pbot(1,np + i))

    end do

    ptop(1,:)           = ptop(1,:)/maxval(uTE)
    pbot(1,:)           = pbot(1,:)/maxval(uTE)
    u_new               = u_new/u_new(np + ntemp)
    

end subroutine add_circular_TE
!*******************************************************************************************


















