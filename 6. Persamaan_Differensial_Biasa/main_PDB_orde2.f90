program pdb_orde2
    implicit none
    real*8 :: x0,xn,y0,u0
    integer :: n
    character(len=6) :: method
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
    
    write(*,*) 'Choose the ODE method : string data [euler, rk4]' 
    read(*,*) method
    
    if (method == 'euler') then
        call euler_improved(func,x0,xn,n,y0,u0)
    else if (method == 'rk4') then
        call runge_kutta4th(func,x0,xn,n,y0,u0)
    end if
    
end program pdb_orde2

double precision function func(x,y,u)
    implicit none
    real*8 :: x, y, u
    
    func = -2*u/x - y

end function func

