subroutine trapezoid(f, b, a, N, intg)
    implicit none
    real*8, external :: f
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

subroutine simpson(f, b, a, N, intg)
    implicit none
    real*8, intent(in) :: b,a
    integer, intent(in) :: N
    real*8, intent(out) :: intg
    real*8, dimension(0:2*N) :: w
    real*8,external :: f
    real*8 :: h
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


