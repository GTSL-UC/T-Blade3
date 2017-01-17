real function feval (x, cf, ord)

integer, intent (in) :: ord
real, intent (in) :: cf(ord), x

feval = cf(1)
do i = 2, ord+1
	feval = feval + (cf(i)*(x**(i-1)))
enddo

end function

subroutine poly_solve_bisect (ord, cf, a_bound, b_bound, er, x)

implicit none

integer, intent(in) :: ord
real, intent (in) :: cf(ord), a_bound, b_bound
real, intent (out) :: x(4)
integer, intent (out) :: er
real :: feval, tol = 1e-6, a, b, m, ya, yb, ym, bnd
integer :: i, maxiter = 200

a = a_bound; b = b_bound
ya = feval(a, cf, ord); yb = feval(b, cf, ord)

if (ya*yb .gt. 0.) then
	er = 1
	return
endif

do i = 1, maxiter
	m = (a+b)/2.; ym = feval(m, cf, ord)
	bnd = (b-a)/2.
	if (abs(ym) .lt. tol) exit
	if (ym*ya .lt. 0.) then
		b = m; yb = ym
	else
		a = m; ya = ym
	endif
enddo

x = ym

if (i == maxiter) then
	er = 2
endif

end subroutine poly_solve_bisect