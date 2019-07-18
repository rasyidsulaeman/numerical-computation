program akarfungsi
    implicit none

    real :: x1, x2,eps, f, df
    real :: root1, xmax, xmin, fx, x, dx
    integer :: nxpoint, i, j 

    write(*,*) 'input interval x1,x2'
    read (*,*) x1,x2

    eps = 1.0e-6
    xmax = 2
    xmin = -2
    nxpoint = 100

    !Test Grafik
    open(unit = 11, file = 'test_plot.dat', status = 'replace')

    dx = (xmax-xmin)/nxpoint
    do i = 1, nxpoint
        x = xmin + dx*(i-1)
        fx = 0.d0
        do j = 1, nxpoint
            fx = fx + f(x)
        end do
        write(11,*) x, fx
    end do

    call secant(f, x1,x2,eps,root1)

    write(*,*) 'Dapat nilai akar fungsinya'
    write(*,*) root1

end program akarfungsi

function f(t)
    real :: t

    f = 3*t - 2 - 5*exp(-t)

end function

function df(r)
    real :: r

    df = -sin(r) - 1
    
end function

