subroutine euler_improved(x0,xn,n,y0,u0)
    implicit none
    real*8 :: x0,xn,y0,u0
    integer :: i,n
    real*8 :: h
    real*8 :: f0,x,y,f1,u1
    real*8 :: func

    h = (xn-x0)/n

    open(unit = 11, file = 'eul_imp_orde2.dat', status = 'replace')

    !Euler Method (Improved)
    do i = 1, n
        f0 = func(x0,y0,u0)
        u1 = u0 + h*f0
        x = x0 + h
        y = y0 + h*u0
        f1 = func(x,y,u1)
        y0 = y0 + 0.5*h*(u0+u1)
        u0 = u0 + 0.5*h*(f0+f1)
        x0 = x
        write(11,*) x0, y0
    end do

end subroutine euler_improved

subroutine runge_kutta4th(x0,xn,n,y0,u0)
    implicit none
    real*8 :: x0,xn,y0,u0
    integer :: i,n
    real*8 :: h
    real*8 :: f0,x,y,f1,u1,u2,y1,f2,u3,x1,y2,f3
    real*8 :: func

    h = (xn-x0)/n

    open(unit = 12, file = 'rk4_orde2.dat', status = 'replace')

    !Runge Kutta 4th order
    do i = 1, n
        f0 = func(x0,y0,u0)
        u1 = u0 + 0.5*h*f0
        x = x0 + 0.5*h
        y = y0 + 0.5*h*u0
        f1 = func(x,y,u1)
        u2 = u0 + 0.5*h*f1
        y1 = y0 + 0.5*h*u1
        f2 = func(x,y1,u2)
        u3 = u0 + h*f2
        x1 = x0 + h
        y2 = y0 + h*u2
        f3 = func(x1,y2,u3)
        y0 = y0 + h*(u0+2*u1+2*u2+u3)/6
        u0 = u0 + h*(f0+2*f1+2*f2+f3)/6
        x0 = x1
        write(12,*) x0, y0
    end do

end subroutine runge_kutta4th

double precision function func(x,y,u)
    implicit none
    real*8 :: x, y, u
    
    func = 5*(x-y) - 4*x*u

    return
end function func