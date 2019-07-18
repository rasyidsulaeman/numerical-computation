program pdb_orde2
    implicit none
    real*8 :: x0,xn,y0,u0
    integer :: n

    !Input for interval [x0,xn]
    x0 = 0
    xn = 2

    !Input for intital condition
    y0 = 1
    u0 = 0

    n = 100

    !call euler_improved(x0,xn,n,y0,u0)
    call runge_kutta4th(x0,xn,n,y0,u0)

end program pdb_orde2
