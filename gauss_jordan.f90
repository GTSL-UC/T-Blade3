subroutine gauss_jordan (n, nrhs, a, fail_flag)

!!	Added by Karthik Balasubramanian
!		This subroutine implements Gauss-Jordan emilimination to solve a n by n linear 
!		system by converting the coefficient matrix to its reduced row echelon form.
!		Row pivoting is implemented.

implicit none

integer, intent (in) :: n, nrhs
real, intent (inout) :: a(n,n+nrhs)
real, parameter :: eps = 1e-16
real :: pvt, temp(n+nrhs)
integer :: i, j, c, ipvt, fail_flag
c = n+nrhs
fail_flag = 0
do i = 1, n
	! Determine pivot row and coefficient
	ipvt = i
	pvt = a(i,i)
	do j = i+1, n
		if (abs(pvt) .lt. abs(a(j,i))) then
			pvt = a(j,i)
			ipvt = j
		endif
	enddo
	! If all pivot column elements are zero, return fail
	if (abs(pvt) .lt. eps) then
		write (*, *) 'FATAL ERROR: gauss_jordan - Zero pivot term'
		fail_flag = 1
		return
	endif
	! Interchange current row and pivot row
	temp = a(ipvt, :)
	a(ipvt, :) = a(i, :)
	a(i, :) = temp
	! Eliminate coefficients below and above pivot
	! Explicit back substitution not required
	a(i,i) = 1.0
	a(i,i+1:c) = a(i,i+1:c) / pvt
	do j = 1, n
		if (j .ne. i) then
			a(j,i+1:c) = a(j,i+1:c) - a(j, i) * a(i,i+1:c)
			a(j,i) = 0.0
		endif
	enddo
enddo

return

end subroutine gauss_jordan
