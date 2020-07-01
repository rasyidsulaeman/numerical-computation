program pdb_orde2
    implicit none
    real*8 :: x0,xn,y0,u0
    integer :: n
    real*8,external :: func

    !Input for interval [x0,xn]
    write(*,*) 'Input interval ODE : [x0, xn]'
    read(*,*) x0, xn
    
    !Input for intital condition
    write(*,*) 'Input initial condition : [y0, u0]'
    read(*,*) y0, u0
    
    !The number of data 
    write(*,*) 'Input the number of data : N'
    read(*,*) n
    
    call runge_kutta4th(func,x0,xn,n,y0,u0)
    
end program pdb_orde2

double precision function func(x,y,u)
    implicit none
    real*8 :: x, y, u
    real*8, parameter :: m = 0.2 !Kg
    real*8, parameter :: k = 32. !N/m
    real*8 :: r                  !Kg/s (Parameter yang diubah)
    real*8 :: alpha, beta        !Rasio overdamped dan underdamped
    
    alpha = 4.0
    beta = 0.25
    
    r = alpha*(4*m*k)**(0.5)
    
    func = (-r/m)*u + (-k/m)*y

end function func

