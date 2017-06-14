!List of functions and subroutines used in 3DBGB code.
!--------------------------Kiran Siddappaji

!---------------------------------------------------------
! Subroutine to display the welcome message
!---------------------------------------------------------
subroutine displayMessage

write(*,*)
write(*,*)'************************************************************'
write(*,*)'************************************************************'
write(*,*)'****  T-BLADE3:Turbomachinery BLADE 3D Geometry Builder ****'
write(*,*)'****                                                    ****' 
write(*,*)'****  Version 0.4                                       ****' 
write(*,*)'****                                                    ****'
write(*,*)'****  ...was also called as below till Aug 2016...      ****' 
write(*,*)'****  3DBGB: 3 Dimensional Blade Geometry Builder       ****'
write(*,*)'****                                                    ****'  
write(*,*)'****  Version 1.3                                       ****' 
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
write(*,*) 'T-Blade3 Copyright (C) 2017 University of Cincinnati, developed by Dr. Mark Turner,' 
write(*,*) 'Karthik  Balasubramanian, Syed Moez Hussain, Ahmed Farid Nemnem, '
write(*,*) 'Kiran Siddappaji and Marshall C. Galbraith.'
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
implicit none

integer i,nphub,nptip,nsl
real*8 xhub(nphub,1),rhub(nphub,1),xtip(nptip,1),rtip(nptip,1)
real*8 scf
character*80 fname1,fname2
character*32 casename

print*,' Writing dimensional hub and casing streamlines...'
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
implicit none

integer i,ia,np
real*8 xml(np),rml(np)
real*8 scf
character*80 fname
character*32 casename,temp

write(temp,*)ia
print*,' Writing dimensional streamline'//trim(adjustl(temp))//'...'
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
implicit none

integer i,nphub

real*8, intent(out):: mphub(nphub,1)
real*8 xhub(nphub,1),rhub(nphub,1),xms_hub(nphub,1),rms_hub(nphub,1)
real*8 x(nphub,1),r(nphub,1),dxds(nphub,1),drds(nphub,1),hub,scf,deltan
real*8 b,xnorm(nphub,1),rnorm(nphub,1),dxn(nphub,1),drn(nphub,1)

character*80 fname1, fname2
character*32 casename

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
write(*,*)'Calculating hub-offset streamline coordinates...'
write(*,*)'Writing the coordinates to hub-offset.dat file'

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
implicit none

integer i,nptip,nsl

real*8,intent(out) :: mptip(nptip,1)
real*8 xtip(nptip,1),rtip(nptip,1),xms_tip(nptip,1),rms_tip(nptip,1)
real*8 x(nptip,1),r(nptip,1),dxds(nptip,1),drds(nptip,1),tip,scf
real*8 b,xnorm(nptip,1),rnorm(nptip,1),dxn(nptip,1),drn(nptip,1),deltan

character*80 fname1, fname2
character*32 casename

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
write(*,*)'Calculating tip-offset streamline coordinates...'
write(*,*)'Writing the coordinates to tip-offset.dat file'

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
real*4 bladedata(amount_data,nsl)
real*8 in_beta(nsl),out_beta(nsl)
character*20 throat_pos(nsl)
character*32 casename
character*80 file1
character(len=2) :: units

file1 = 'blade_section_data.'//trim(casename)//'.dat'
open(unit= 100,file=file1, form="formatted")
write(100,*)trim(casename)
write(100,*),'Blade sections Data:'
write(100,*),'---------------------'
write(100,*)  
do js = 1,nsl
   if (js == 1) then
	if (units == 'mm') then
	  if (throat_pos(nsl) == 'le') then
	    write(100,202),'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)','chord [mm]','sweep','lean','area[mm^2]','lethk [mm]','tethk [mm]','throat [mm]','(r*delta_theta)LE','Geom Zweifel','le/te/btween/none'
	  elseif (throat_pos(nsl) == 'te') then
	    write(100,202),'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)','chord [mm]','sweep','lean','area[mm^2]','lethk [mm]','tethk [mm]','throat [mm]','(r*delta_theta)TE','Geom Zweifel','le/te/btween/none'
	  else
	    write(100,202),'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)','chord [mm]','sweep','lean','area[mm^2]','lethk [mm]','tethk [mm]','throat [mm]','(r*delta_theta)WT','Geom Zweifel','le/te/btween/none'
      endif
	elseif (units == 'cm') then
	  if (throat_pos(nsl) == 'le') then
	    write(100,202),'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)','chord [cm]','sweep','lean','area[cm^2]','lethk [cm]','tethk [cm]','throat [cm]','(r*delta_theta)LE','Geom Zweifel','le/te/btween/none'
	  elseif (throat_pos(nsl) == 'te') then
	    write(100,202),'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)','chord [cm]','sweep','lean','area[cm^2]','lethk [cm]','tethk [cm]','throat [cm]','(r*delta_theta)TE','Geom Zweifel','le/te/btween/none'
	  else
	    write(100,202),'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)','chord [cm]','sweep','lean','area[cm^2]','lethk [cm]','tethk [cm]','throat [cm]','(r*delta_theta)WT','Geom Zweifel','le/te/btween/none'
      endif
    elseif ((units == 'm ').or.(units == 'm)')) then
	  if (throat_pos(nsl) == 'le') then
	    write(100,202),'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)','chord [ m]','sweep','lean','area[ m^2]','lethk [ m]','tethk [ m]','throat [ m]','(r*delta_theta)LE','Geom Zweifel','le/te/btween/none'
	  elseif (throat_pos(nsl) == 'te') then
	    write(100,202),'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)','chord [ m]','sweep','lean','area[ m^2]','lethk [ m]','tethk [ m]','throat [ m]','(r*delta_theta)TE','Geom Zweifel','le/te/btween/none'
	  else
	    write(100,202),'section','span','betaZ*le(deg)','betaZ*te(deg)','betaM*le(deg)','betaM*te(deg)','chord [ m]','sweep','lean','area[ m^2]','lethk [ m]','tethk [ m]','throat [ m]','(r*delta_theta)WT','Geom Zweifel','le/te/btween/none'
      endif
	else
		print*,'Error in Blade scaling factor ...'
		print*,'Check the Scaling factor in the input file ...'
		stop
	endif	
   endif
   write(100,201),js,bladedata(1:(amount_data-1),js),throat_pos(js)		
enddo
write(100,*),'Total Volume of the blade =',bladedata(amount_data,nsl),'[m^3]' 
close(100)

202 format((A7,2x),(A4,11x),4(A14,3x),(A13,1x),2(A6,5x),(A13,2x),(A10,2x),(A10,1x),(A11),2x,A17,2x,A12,2x,A17)
201 format(1x,I3,4x,(f11.8,4x),4(sf14.8,3x),3x,8(f11.8,1x),7x,f11.8,4x,A5)

return
end subroutine
!***************************************************************

!*******************************************************
!***************************************************************
subroutine throatindex(throat_pos,throat_index,n_normal_distance,js,nsl)
implicit none
integer, intent(inout):: js
integer, intent(in):: nsl,throat_index(nsl),n_normal_distance
character*20, intent(out):: throat_pos(nsl)

if(throat_index(js) == 0) then
  throat_pos(js) = 'none'
  print*,'No Throat found'
  !exit
elseif(throat_index(js) < 0.25*n_normal_distance) then
  throat_pos(js) = 'le'
  print*,'throat_index',js,throat_index(js)
elseif(throat_index(js) > 0.75*n_normal_distance) then
  throat_pos(js) = 'te' 
  print*,'throat_index',js,throat_index(js)
else
  throat_pos(js) = 'btween'
  print*,'throat_index',js,throat_index(js)
endif! end if for throat options

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
write(13,*)"section    m'sLE             m'sTE              m'blade_LE          m'blade_TE            chord             pitch          "

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
parameter (nx=500,ny=300)

!real, intent (inout):: xb(nx),yb(nx),xbot(ny),ybot(ny),xtop(ny),ytop(ny)
real, intent (inout):: xb(np),yb(np),xbot((np+1)/2),ybot((np+1)/2),xtop((np+1)/2),ytop((np+1)/2)
real, intent(in):: stk_u(1), stk_v(1)
real umin,umax,sb(nx)
real u_stack,v_stack,vtop_stack,vbot_stack,v_zero_stack
real const_stk_u,const_stk_v,stku,stkv 
real area,ucen,vcen,ei11,ei22,apx1,apx2

! Calculating the area centroid cordinates of the airfoil ucen,vcen.
print*,np
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
! stacking on the chord...........
if(stku.eq.200.)then ! 200 is for stacking at the area centroid.
  u_stack = ucen
  v_stack = vcen
  print*,'Stacking at the area centroid of the airfoil...'
  goto 16
else
  ! Calculating u_stack using the % chord stacking information
  !nphalf = (np+1)/2
  if(LE.eq.2)then ! sting LE option
    print*,'LE with sting.'
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
vtop_stack =  ytop(k) + ((u_stack - xtop(k))*(ytop(k+1) - ytop(k))/(xtop(k+1) - xtop(k))) !*sqrt((xtop(k+1) - xtop(k))**2 + (ytop(k+1) - ytop(k))**2) 
! Stacking on meanline: v_stack = 0
! v_zero_stack = average of yb(istack) and yb(1+np - istack)  
! Above meanline stack: vstack= v_zero_stack - %age(stack)*(distance between meanline and 100% ABOVE meanline @ istack)
! Below meanline stack: vstack= v_zero_stack - %age(stack)*(distance between meanline and 100% BELOW meanline @ istack)
v_zero_stack = (vtop_stack + vbot_stack)/2
print*, "vtop_stack  vbot_stack", vtop_stack, vbot_stack
if(stku.ne.200.and.stkv.gt.0)then! above meanline stacking
  v_stack = v_zero_stack + (real(stkv)/100)*(abs(vtop_stack - v_zero_stack))
  print*,'+ve stack v_stack',v_stack,(real(stkv)/100)
elseif(stku.ne.200.and.stkv.eq.0)then
  v_stack = 0.!v_zero_stack
elseif(stku.ne.200.and.stkv.lt.0)then !below meanline stacking
  v_stack = v_zero_stack + (real(stkv)/100)*(abs(vbot_stack - v_zero_stack))
  print*,'v_stack',v_stack,(real(stkv)/100)
endif
write(*,*)
print*,'u_stack  v_stack', u_stack, v_stack
write(*,*)
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
parameter(nx=500)
character*32 fname,fext,blext(100)
character*20 airfoil
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

if(mod(np,2).eq.0)then
  np_sidee = np/2 ! For even no. of points LE = np/2. So, the current formula (np+1)/2 ==> np/2 with this change.
else
  ! points on bottom and top curve
  np_sidee = (np+1)/2
endif
!np_sidee = (np+1)/2
print*,'np_sidee',np_sidee
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
if(n_normal_distance == 0) then
  print*, 'No throats found because of Low number of blades.'
  return
endif

! Writing the Mouth and Exit areas:
mouth_coord = inter_coord(:,1)
exit_coord = inter_coord(:,n_normal_distance)

print*,'number of intersection points (k) =',k-1,'from np_side of',np_sidee

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
  if(js==1) then 
    file4 = 'throat_points.'//trim(casename)//'.txt'
    open(unit= 85,file=file4, form="formatted")
    print*,file4
    write(85,*)'pitch',pitch
  endif
  write(85,*)'section',js
  write(85,*)'u	camber	upper_camber'
  do i = 1, np_sidee
     write(85,*),u(i),camber(i),camber_upper(i)
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
     write(85,*),inter_coord(1:2,k)
     write(85,*),inter_coord(3:4,k),k
  enddo
  write(85,*)'min_throat_2D',min_throat_2D
  write(85,*)'throat_index',throat_index
  if(js==nsl) then
    close(85)
  endif
endif   

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

parameter (nx = 500, nax = 500)

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

parameter (nx = 500, nax = 500)

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











