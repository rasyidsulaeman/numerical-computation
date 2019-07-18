subroutine falseposition(f, x1, x2, eps, root)
    implicit none
    real :: x1, x2
    real :: eps
    real :: a, b, f, clama, cbaru, root, fakar, z
    integer :: i
    integer, parameter :: iter = 100

    a = x1
    b = x2

    clama = (a*f(b)-b*f(a))/(f(b)-f(a))
   
    do i = 1, iter
        if (f(a)*f(clama) < 0.0) then
            b = clama
        else 
            a = clama
        end if 

        cbaru = (a*f(b)-b*f(a))/(f(b)-f(a))
        
        z = abs((clama-cbaru)/cbaru)
        if (z < eps) exit

        clama = cbaru

        root = clama
        fakar = f(root)

        write(*,*) i, a, b, root, fakar, z

    end do

    return
end subroutine falseposition

