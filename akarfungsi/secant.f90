subroutine secant(f, x1,x2, eps, x)
    implicit none

    real:: x1, x2, eps     
    real :: x, f         
    real :: f1, f2, dx, z
    integer :: i
    integer, parameter :: iter = 100

    if (f(x1)*f(x2) > 0.0) then
        write(*,*) 'Tidak ada solusi diantara interval "[x1,x2]"'
        return
    end if

    f1 = f(x1)
    f2 = f(x2)

    do i = 1, iter
        dx = (x2-x1)*f2/(f2-f1)
        x = x2 - dx

        x1 = x2
        f1 = f2
        x2 = x
        f2 = f(x2)

        z = abs((x1-x2)/x1)
        
        write(*,22) i, x, f2, z
        22 format(I7, 5X, F20.17, 2X, F20.17, 2X, F20.17)
        
        if (z < eps) exit

    end do

    return
   
end subroutine secant
