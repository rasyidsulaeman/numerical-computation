subroutine trapezoid(b, a, N, intg)
    implicit none
    real*8 :: f
    real*8, intent(in) :: b,a
    integer, intent(in) :: N
    real*8, intent(out) :: intg
    real*8, dimension(0:N) :: w
    real*8 :: h
    integer :: i

    !Quadrature Trapezoid
    h = (b-a)/N
    w(0) = h/2
    do i = 1, N-1
        w(i) = h
    end do
    w(N) = h/2

    intg = 0.d0
    do i = 0, N
        intg = intg + w(i)*f(a+i*h)
    end do

end subroutine trapezoid

subroutine simpson(b, a, N, intg)
    implicit none
    real*8, intent(in) :: b,a
    integer, intent(in) :: N
    real*8, intent(out) :: intg
    real*8, dimension(0:2*N) :: w
    real*8 :: h, f
    integer :: i

    h = (b-a)/(2*N)
    w(0) = h/3
    do i = 1, 2*N-1, 2
        w(i) = 4*h/3
    end do
    do i = 2, 2*N-2, 2
        w(i) = 2*h/3
    end do
    w(2*N) = h/3

    intg = 0.d0
    do i = 0, 2*N
        intg = intg + w(i)*f(a+i*h)
    end do
    
end subroutine simpson

double precision function func(x)
    implicit none
    real*8, intent(in) :: x
    real*8 :: lagrange
    integer, parameter :: Ndata = 16
    real*8 :: xdata(Ndata), fdata(Ndata)

    call inputdata(xdata, fdata)

    func = lagrange(x, xdata, fdata)
    call lagrange_plot(Ndata, xdata, fdata)
    
 end function func