program akarfungsi
    implicit none

    real :: x1, x2,df
    real, external :: f
    real :: root
    real,parameter :: eps=1e-6

    write(*,*) 'Your function :'
    write(*,*) 'f(x) = cos(x) - x'
    write(*,*) ''
    write(*,*) 'input interval x1,x2'
    read (*,*) x1, x2
    
    call secant(f, x1, x2, eps, root)
    
    write(*,*) 'Root : ', root
    write(*,*) 'Value at root :', f(root)
end program akarfungsi

real function f(t)
    real :: t
    
    f = cos(t) - t
    
end function

function df(t)
    real :: t

    df = -sin(t) - 1
    
end function

