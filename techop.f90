program TEchop
! This program chops the TE of a 2D airfoil
!input: blade file
!output: blade file rewritten to the same file with chopped TE.

!---Kiran Siddappaji

implicit none

integer i,j,k,points,nx,ny,np

parameter (nx=500,ny=500)
real*8 xb(nx),yb(nx),trarray(2)
real*8 sinl,sext,chrd1,chrd2,pitch
character*32 fname,fext,temp,npoints,fname1

call getarg(1,fname)
!Error message
if((fname.eq.'help').or.(fname.eq.''))then
 write(*,*)
 print*,'******************************************************'
 print*,'*** ERROR: techop command error.                   ***'
 print*,'******************************************************'
 print*,'*** Usage: > techop blade.#.#.casename points      ***'
 print*,'*** Example: techop blade.1.1.case 8               ***'
 print*,'*** This takes off 8 points from PS and SS curves. ***' 
 print*,'******************************************************'
 write(*,*)
 stop
endif
!
call getarg(2,npoints)
read(npoints,*)points
! Reading the blade coordinates
open(1,file=fname,status='unknown')
write(*,*)
write(*,*)" Reading ", trim(adjustl(fname))
write(*,*)
read(1,*)fext
read(1,*)sinl,sext,chrd1,chrd2,pitch
np = 0
do 
 read(1,*,end=10)trarray(1),trarray(2)
 np = np + 1
 xb(np) = trarray(1)
 yb(np) = trarray(2)
enddo 
10 close(1)

!do i = 1, np
! print*,np,xb(np),yb(np)
!enddo
print*,'Original number of coordinates:',np
write(*,*)

write(*,*)' Number of coordinates chopped from the TE.',points
write(*,*)
write(*,*)' Final number of coordinates:',(np - points)
!Overwriting the chopped TE blade coordinates back to the file 
write(*,*)' Overwriting chopped TE blade coordinates to the SAME file'
write(*,*)
open(2,file=fname,status='unknown')
write(2,100)fext
write(2,101)sinl,sext,chrd1,chrd2,pitch
do i = points+1,np-points
 write(2,*)xb(i),yb(i)
enddo
write(2,*)
close(2)
write(*,*) ' ---------END-----------'

100  format(a)
101  format(5(f19.16,1x))
102  format(2(f35.16,1x))

return
end program