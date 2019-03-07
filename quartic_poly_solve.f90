subroutine quartic_roots (ceff, er, root)

!		a(1) + a(2)*x + ... + a(5)*x**4

implicit none

integer, parameter :: sp = kind(0.0), dp = kind(0.0D0)
real(kind = dp), intent (in) :: ceff(5)
complex(kind = dp), intent (out) :: root(4)
integer, intent (out) :: er
real(kind = dp) :: p, q, r, t, v1, v2, x, y, u, h, v, x1, x2, x3
real(kind = dp) :: b, c, d, e, temp(4)
complex(kind = dp) :: w
integer :: i, j

if (ceff(1).eq.0.0) then
    root(1) = (0.D0,0.D0)
    call cubic_roots(ceff(2:), root(2:))
    return
endif

b = ceff(4)/(4.0D0*ceff(5))
c = ceff(3)/ceff(5)
d = ceff(2)/ceff(5)
e = ceff(1)/ceff(5)

p = 0.5D0*(c - 6.0D0*b*b)
q = d - 2.0D0*b*(c - 4.0D0*b*b)
r = b*b*(c - 3.0D0*b*b) - b*d + e
temp(1) = -q*q/64.0D0
temp(2) = 0.25D0*(p*p - r)
temp(3) =  p
temp(4) = 1.0D0

call cubic_roots(temp, root)
do i = 1, 3
    if (root(i) .ne. root(i)) print*, 'WARNING: cubic_roots subroutine failed:', i , 'th root undefined'
enddo

if (aimag(root(2)) .eq. 0.0D0) then
    x1 = dble(root(1))
    x2 = dble(root(2))
    x3 = dble(root(3))
    if (x1 .gt. x2) then
        t = x1; x1 = x2; x2 = t
    endif
    if (x2 .gt. x3) then
        t = x2; x2 = x3; x3 = t
    endif
    if (x1 .gt. x2) then
        t = x1; x1 = x2; x2 = t
    endif
    u = 0.0D0
    if (x3 .gt. 0.0D0) u = sqrt(x3)
    if (x2 .le. 0.0D0) then
        v1 = sqrt(abs(x1))
        v2 = sqrt(abs(x2))
        if (q .lt. 0.0D0) u = -u
        x = -u - b
        y = v1 - v2
        root(1) = cmplx(x, y, dp)
        root(2) = cmplx(x,-y, dp)
        x =  u - b
        y = v1 + v2
        root(3) = cmplx(x, y, dp)
        root(4) = cmplx(x,-y, dp)
        return
    elseif (x1 .ge. 0.0D0) then
        x1 = sqrt(x1)
        x2 = sqrt(x2)
        if (q .gt. 0.0D0) x1 = -x1
        temp(1) = x1 + x2 + u - b
        temp(2) = -x1 - x2 + u - b
        temp(3) = x1 - x2 - u - b
        temp(4) = -x1 + x2 - u - b
        do i = 1, 3
            do j = i, 4
                if (temp(j) .eq. minval(temp(i:4))) then
                    t = temp(j); temp(j) = temp(i); temp(i) = t
                    exit
                endif
            enddo
        enddo
        if (abs(temp(1)) .lt. 0.1D0*abs(temp(4))) then
            t = temp(2)*temp(3)*temp(4)
            if (t .ne. 0.0D0) temp(1) = e/t
        endif
        root(1) = cmplx(temp(1), 0.0D0, dp)
        root(2) = cmplx(temp(2), 0.0D0, dp)
        root(3) = cmplx(temp(3), 0.0D0, dp)
        root(4) = cmplx(temp(4), 0.0D0, dp)
        return
    elseif (abs(x1) .gt. x2) then
        v1 = sqrt(abs(x1))
        v2 = 0.0D0
        x = -u - b
        y = v1 - v2
        root(1) = cmplx(x, y, dp)
        root(2) = cmplx(x,-y, dp)
        x =  u - b
        y = v1 + v2
        root(3) = cmplx(x, y, dp)
        root(4) = cmplx(x,-y, dp)
        return
    endif
else
    t = dble(root(1))
    x = 0.0D0
    if (t .gt. 0.0D0) then
        x = sqrt(t)
        if (q .gt. 0.0D0) x = -x
        w = sqrt(root(2))
        u = 2.0D0*dble(w)
        v = 2.0D0*abs(aimag(w))
        t =  x - b
        x1 = t + u
        x2 = t - u
        if (abs(x1) .gt. abs(x2)) then
            t = x1; x1 = x2; x2 = t
        endif
        u = -x - b
        h = u*u + v*v
        if (x1*x1 .lt. 0.01D0*min(x2*x2,h)) x1 = e/(x2*h)
        root(1) = cmplx(x1, 0.0D0, dp)
        root(2) = cmplx(x2, 0.0D0, dp)
        root(3) = cmplx(u, v, dp)
        root(4) = cmplx(u,-v, dp)
    elseif (t .lt. 0.0D0) then
        h = abs(dble(root(2))) + abs(aimag(root(2)))
        if (abs(t) .gt. h) then
            w = sqrt(root(2))
            u = 2.0D0*dble(w)
            v = 2.0D0*abs(aimag(w))
            t =  x - b
            x1 = t + u
            x2 = t - u
            if (abs(x1) .gt. abs(x2)) then
                t = x1; x1 = x2; x2 = t
            endif
            u = -x - b
            h = u*u + v*v
            if (x1*x1 .lt. 0.01D0*min(x2*x2,h)) x1 = e/(x2*h)
            root(1) = cmplx(x1, 0.0D0, dp)
            root(2) = cmplx(x2, 0.0D0, dp)
            root(3) = cmplx(u, v, dp)
            root(4) = cmplx(u,-v, dp)
        else
            v = sqrt(abs(t))
            root(1) = cmplx(-b, v, dp)
            root(2) = cmplx(-b,-v, dp)
            root(3) = root(1)
            root(4) = root(2)
        endif
    elseif (t .eq. 0.0D0) then
        w = sqrt(root(2))
        u = 2.0D0*dble(w)
        v = 2.0D0*abs(aimag(w))
        t =  x - b
        x1 = t + u
        x2 = t - u
        if (abs(x1) .gt. abs(x2)) then
            t = x1; x1 = x2; x2 = t
        endif
        u = -x - b
        h = u*u + v*v
        if (x1*x1 .lt. 0.01D0*min(x2*x2,h)) x1 = e/(x2*h)
        root(1) = cmplx(x1, 0.0D0, dp)
        root(2) = cmplx(x2, 0.0D0, dp)
        root(3) = cmplx(u, v, dp)
        root(4) = cmplx(u,-v, dp)
    endif
endif

endsubroutine quartic_roots

subroutine cubic_roots (ceff, root)

implicit none

integer, parameter :: sp = kind(0.0), dp = kind(1.0D0)
real(kind = dp), intent (in), dimension(:) :: ceff(4)
complex(kind = dp), intent (out), dimension(:) :: root(3)

real(kind = dp),parameter:: rt3 = 1.7320508075689D0, eps = epsilon(0.0D0)! (sqrt(3))
real (kind = dp) :: aq(3), arg, c, cf, d, p, p1, q, q1
real(kind = dp):: r, ra, rb, rq, rt
real(kind = dp):: r1, s, sf, sq, sum1, t, tol, t1, w
real(kind = dp):: w1, w2, x, x1, x2, x3, y, y1, y2, y3

if (ceff(1) .eq. 0.0) then
    root(1) = (0.D0,0.D0)
    call quadratic_roots(ceff(2:4), root(2:3))
    return
endif

p = ceff(3)/(3.0D0*ceff(4))
q = ceff(2)/ceff(4)
r = ceff(1)/ceff(4)
tol = 4.0D0*eps

c = 0.0D0
t = ceff(2) - p*ceff(3)
if (abs(t) .gt. tol*abs(ceff(2))) c = t/ceff(4)

t = 2.0D0*p*p - q
if (abs(t) .le. tol*abs(q)) t = 0.0D0
d = r + p*t

if (abs(d) .le. tol*abs(r)) then
    root(1) = cmplx(-p, 0.0D0,dp)
    w = sqrt(abs(c))
    if (c .lt. 0.0D0) then
        if (p .ne. 0.0D0) then
            x = -(p + sign(w,p))
            root(3) = cmplx(x, 0.0D0,dp)
            t = 3.0D0*ceff(1)/(ceff(3)*x)
            if (abs(p) .gt. abs(t)) then
                root(2) = root(1)
                root(1) = cmplx(t, 0.0D0,dp)
            else
                root(2) = cmplx(t, 0.0D0,dp)
            endif
        else
            root(2) = cmplx(w, 0.0D0,dp)
            root(3) = cmplx(-w, 0.0D0,dp)
        endif
    else
        root(2) = cmplx(-p, w,dp)
        root(3) = cmplx(-p,-w,dp)
    endif
    return
endif

s = max(abs(ceff(1)), abs(ceff(2)), abs(ceff(3)))
p1 = ceff(3)/(3.0D0*s)
q1 = ceff(2)/s
r1 = ceff(1)/s

t1 = q - 2.25D0*p*p
if (abs(t1) .le. tol*abs(q)) t1 = 0.0D0
w = 0.25D0*r1*r1
w1 = 0.5D0*p1*r1*t
w2 = q1*q1*t1/27.0D0

if (w1 .ge. 0.0D0) then
    w = w + w1
    sq = w + w2
elseif (w2 .lt. 0.0D0) then
    sq = w + (w1 + w2)
else
    w = w + w2
    sq = w + w1
endif

if (abs(sq) .le. tol*w) sq = 0.0D0
rq = abs(s/ceff(4))*sqrt(abs(sq))
if (sq .ge. 0.0D0) then
    ra = (-0.5D0*d - sign(rq,d))
    if (ra .gt. 0.0D0) then
        ra = ra**(1.0D0/3.0D0)
    elseif (ra .lt. 0.0D0) then
        ra = -1.0D0*(-1.0D0*ra)**(1.0D0/3.0D0)
    else
        ra = 0.0D0
    endif
    rb = -c/(3.0D0*ra)
    t = ra + rb
    w = -p
    x = -p
    if (abs(t) .gt. tol*abs(ra)) then
        w = t - p
        x = -0.5D0*t - p
        if (abs(x) .le. tol*abs(p)) x = 0.0D0
    endif
    t = abs(ra - rb)
    y = 0.5D0*rt3*t
    if (t .le. tol*abs(ra)) then
        if (abs(x) .lt. abs(w)) then
            if (abs(w) .lt. 0.1D0*abs(x)) w = - (r/x)/x
            root(1) = cmplx(w, 0.0D0,dp)
            root(2) = cmplx(x, 0.0D0,dp)
            root(3) = root(2)
            return
        else
            if (abs(x) .lt. 0.1D0*abs(w)) then
            else
                root(1) = cmplx(x, 0.0D0,dp)
                root(2) = root(1)
                root(3) = cmplx(w, 0.0D0,dp)
                return
            endif
        endif
    else
        if (abs(x) .lt. abs(y)) then
            s = abs(y)
            t = x/y
        else
            s = abs(x)
            t = y/x
        endif
        if (s .lt. 0.1D0*abs(w)) then
        else
            w1 = w/s
            sum1 = 1.0D0 + t*t
            if (w1*w1 .lt. 0.01D0*sum1) w = - ((r/sum1)/s)/s
            root(1) = cmplx(w, 0.0D0,dp)
            root(2) = cmplx(x, y,dp)
            root(3) = cmplx(x,-y,dp)
            return
        endif
    endif
else
    arg = atan2(rq, -0.5D0*d)
    cf = cos(arg/3.0D0)
    sf = sin(arg/3.0D0)
    rt = sqrt(-c/3.0D0)
    y1 = 2.0D0*rt*cf
    y2 = -rt*(cf + rt3*sf)
    y3 = -(d/y1)/y2
    x1 = y1 - p
    x2 = y2 - p
    x3 = y3 - p
    if (abs(x1) .gt. abs(x2)) t = x1; x1 = x2; x2 = t
    if (abs(x2) .gt. abs(x3)) t = x2; x2 = x3; x3 = t
    if (abs(x1) .gt. abs(x2)) t = x1; x1 = x2; x2 = t
    w = x3
    if (abs(x2) .lt. 0.1D0*abs(x3)) then
    elseif (abs(x1) .lt. 0.1D0*abs(x2)) then
        x1 = - (r/x3)/x2
        root(1) = cmplx(x1, 0.0D0,dp)
        root(2) = cmplx(x2, 0.0D0,dp)
        root(3) = cmplx(x3, 0.0D0,dp)
        return
    endif
endif

aq(1) = ceff(1)
aq(2) = ceff(2) + ceff(1)/w
aq(3) = -ceff(4)*w
call quadratic_roots(aq, root)
root(3) = cmplx(w, 0.0D0,dp)
if (aimag(root(1)) .eq. 0.0D0) return
root(3) = root(2)
root(2) = root(1)
root(1) = cmplx(w, 0.0D0,dp)
return

endsubroutine cubic_roots

subroutine quadratic_roots (ceff, root)

implicit none

integer, parameter :: sp = kind(0.0), dp = kind(0.0D0)
real(kind = dp),parameter:: eps = epsilon(0.0D0)
real(kind = dp), intent (in), dimension(:) :: ceff(3)
complex(kind = dp), intent (out), dimension(:) :: root(2)
real(dp):: d, r, w, x, y
if(ceff(1).eq.0.0) then
    root(1) = (0.D0,0.D0)
    root(2) = cmplx(-ceff(2)/ceff(3), 0.0D0,dp)
    return
endif

d = ceff(2)*ceff(2) - 4.0D0*ceff(1)*ceff(3)             ! the discriminant
if (abs(d) .le. 2.0D0*eps*ceff(2)*ceff(2)) then
    root(1) = cmplx(-0.5D0*ceff(2)/ceff(3), 0.0D0, dp) ! discriminant is tiny
    root(2) = root(1)
    return
endif

r = sqrt(abs(d))
if (d .lt. 0.0D0) then
    x = -0.5D0*ceff(2)/ceff(3)        ! negative discriminant =.gt. roots are complex   
    y = abs(0.5D0*r/ceff(3))
    root(1) = cmplx(x, y, dp)
    root(2) = cmplx(x,-y, dp)   ! its conjugate
    return
endif

if (ceff(2) /= 0.0D0) then              ! see Numerical Recipes, sec. 5.5
    w = -(ceff(2) + sign(r,ceff(2)))
    root(1) = cmplx(2.0D0*ceff(1)/w,  0.0D0, dp)
    root(2) = cmplx(0.5D0*w/ceff(3), 0.0D0, dp)
    return
endif

x = abs(0.5D0*r/ceff(3))   ! ceff(2)=0 if you get here
root(1) = cmplx( x, 0.0D0, dp)
root(2) = cmplx(-x, 0.0D0, dp)
return

end subroutine quadratic_roots
