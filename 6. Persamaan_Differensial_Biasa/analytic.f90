program analytic_func
    implicit none
    real*8 :: x0,xn
    real*8 :: dx, j, x
    integer :: n, i
    real*8, external :: j0
    
    xn = 50.
    x0 = 0.001
    n = 100
    
    open(unit = 12, file='bessel_analytic.dat',status='replace')
    
    dx = (xn - x0) / real(n)
    do i = 1,n
        x = x0 + (i-1)*dx
        j = j0(x)
        write(*,*) x, j
        write(12,*) x, j
    end do
end program

double precision function j0(x)
    implicit none
    real*8 :: x
    
    j0 = sin(x)/x
    
end function


