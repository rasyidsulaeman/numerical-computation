subroutine euler_method(x0,x1,n,y0)
    implicit none
    real*8 :: y0
    real*8 :: x0, x1, h
    integer :: i, n
    real*8 :: func

    h = (x1-x0)/n
    
    open(unit = 11, file = 'euler_method.dat', status = 'replace')
    !Metode Euler
    do i = 1, n
        y0 = y0 +h*func(x0,y0)
        x0 = x0 + h
        write(11,*) x0, y0
    end do

end subroutine euler_method

subroutine euler_modified(x0,x1,n,y0)
    implicit none
    real*8 :: y0
    real*8 :: x0, x1, h, y,x
    integer :: i, n
    real*8 :: func

    h = (x1-x0)/n

    open(unit = 12, file = 'euler_modified.dat', status = 'replace')
    !Euler Method (Modified)
    do i = 1, n
        y = y0 + 0.5*h*func(x0,y0)
        x = x0 + 0.5*h
        y0 = y0 + h*func(x, y)
        x0 = x
        write(12,*) x0,y0
    end do

    write(*,*)

end subroutine euler_modified

subroutine euler_improved(x0,x1,n,y0)
    implicit none
    real*8 :: y0
    real*8 :: x0, x1, h, y, x
    integer :: i, n
    real*8 :: func

    h = (x1-x0)/n

    open(unit = 13, file = 'euler_improved.dat', status = 'replace')
    !Euler Method (Improved)
    do i = 1, n
        y = y0 + h*(func(x0,y0))
        x = x0 + h
        y0 = y0 + 0.5*h*(func(x0,y0) + func(x,y))
        x0 = x
        write(13,*) x0,y0
    end do

    write(*,*)

end subroutine euler_improved

subroutine runge_kutta4th(x0,x1,n,y0)
    implicit none
    real*8 :: y0
    real*8 :: x0,x1
    integer :: n
    real*8 :: h, f0, f1, f2, f3, x, y, y1, x2, y2
    real*8 :: func
    integer :: i

    h = (x1-x0)/n

    open(unit = 14, file = 'runge_kutta4th.dat', status = 'replace')
    !Runge Kutta 4th order
    do i = 1, n
        f0 = func(x0,y0)
        x = x0 + 0.5*h
        y = y0 + 0.5*h*f0
        f1 = func(x,y)
        y1 = y0 + 0.5*h*f1
        f2 = func(x,y1)
        x2 = x0+h
        y2 = y0+h*f2
        f3 = func(x2,y2)
        y0 = y0 + h*(f0+2*f1+2*f2+f3)/6
        x0 = x2
        write(14,*) x0, y0
    end do

    write(*,*)
end subroutine runge_kutta4th

subroutine runge_kutta3rd(x0,x1,n,y0)
    implicit none
    real*8 :: y0
    real*8 :: x0,x1
    integer :: n
    real*8 :: h, f0, f1, f2, x, y, x2, y2
    real*8 :: func
    integer :: i

    h = (x1-x0)/n

    open(unit = 15, file = 'runge_kutta3rd.dat', status = 'replace')
    !Runge Kutta 3rd Order
    y0 = 1
    do i = 1, n
        f0 = func(x0,y0)
        x = x0 + 0.5*h
        y = y0 + 0.5*h*f0
        f1 = func(x,y)
        x2 = x0 + h
        y2 = y0 + h*f1
        f2 = func(x2,y2)
        y0 = y0 + h*(f0+4*f1+f2)/6
        x0 = x2
        write(15,*) x0,y0
    end do

end subroutine runge_kutta3rd

double precision function func(x,y)
    implicit none
    real*8, intent(in) :: x,y

    func = x**2-exp(y)*sin(x)
    
    return
end function func

