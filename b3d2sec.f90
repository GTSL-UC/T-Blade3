subroutine b3d2sec(scf,fext,ibrowc,nbls,casename)

!reads the blade3D file and writes separate files containing..
!.. 3D blade coordinates.

implicit none

real*8 xs,ys,zs,xsav,ysav,zsav
real*8 scf
character*80 fname,fname1,temp,fname2
character*10 ibrowc
character*32 casename,fext
integer j,i,np,ns,nbls

write(*,*)
print*,'casename:',trim(fext)
fname = 'blade3d.'//trim(casename)//'.dat'
write(*,*)
print*,'Fname: ',trim(fname)
write(*,*)
write(*,*)'Creating section data files for CAD system...'

open(2,file=fname,status='unknown')
read(2,*)np,ns
write(*,*)
print*,'# of points:',np
write(*,*)
print*,'# of sections:',ns
write(*,*)
print*,'bsf:',scf
print*,'Number of blades in this row:',nbls
write(*,*)
do j = 1,ns
   write(temp,*)j
   !fname1='sec'//trim(adjustl(temp))//'.'//trim(adjustl(ibrowc))//'.'//trim(fext)//'.dat'
   fname1='sec'//trim(adjustl(temp))//'.'//trim(casename)//'.dat'
   ! fname2='sec'//trim(adjustl(temp))//'.'//trim(casename)//'.csv'
   open(1,file=fname1,form='formatted')
   ! open(3,file=fname2,form='formatted')
   do i = 1,np
      read(2,*)xs,ys,zs
      if(i.eq.1)then
        xsav=xs
        ysav=ys
        zsav=zs
      endif
      write(1,12)xs,ys,zs
	  ! if(i.lt.np)then
	    ! write(3,11)xs,',',ys,',',zs
	  ! endif 
   enddo
   close(1)
   ! close(3)
enddo
close(2)
print*,'--- END ---'
write(*,*)
   
11 format((f25.16),A,(f25.16),A,(f25.16))
12 format((f25.16),1x,(f25.16),1x,(f25.16))
return
end subroutine b3d2sec
 
