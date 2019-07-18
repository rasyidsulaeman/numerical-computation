program kombinasi
    use akarfungsi
    implicit none

    real :: f, x1, x2, eps, roots

    write(*,*) 'Batas akar x1, x2'
    read(*,*) x1, x2

    eps = 1.0e-6

    
    call falseposition(f,x1,x2,eps,roots)
    
    call secant(f,x1,x2,eps,roots)

    call newton_raphson(f,x1,x2,eps,roots)
    
    call bisection(f,x1,x2,eps,roots)

    write(*,*) roots
    
end program kombinasi

function f(t)
    real :: t

    f = cos(t) -t 
    
end function f

module akarfungsi
    implicit none

    contains

    subroutine falseposition(f, x1, x2, eps, root)
        implicit none
        real :: x1, x2
        real :: eps
        real :: a, b, f, clama, cbaru, root, fakar, z
        integer :: i
        integer, parameter :: iter = 100

        if (f(x1)*f(x2) > 0.0) then
            write(*,*) 'Tidak ada solusi diantara interval "[x1,x2]"'
            return
        end if
    
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
            
            write(*,*) i, x, f2, z
            
            if (z < eps) exit
    
        end do
    
        return
       
    end subroutine secant
    
    subroutine newton_raphson(f, df, xlama,eps,root)
        implicit none
    
        real :: xbaru, xlama, f, df, z, root, fakar, dfakar
        integer :: i
        integer, parameter :: iter = 100 
        real :: eps
    
        do i = 1, iter
            xbaru = xlama - f(xlama)/df(xlama)
            
            z = abs((xlama - xbaru)/xbaru)
            if (z < eps) then
                exit 
            end if 
    
            xlama = xbaru
            root = xlama
            fakar = f(root)
            dfakar = df(root)

            write(*,*) i, root, fakar, dfakar, z
        end do
    
        return
    end subroutine newton_raphson

    subroutine bisection(f, x1,x2,eps,root)
  
        implicit none
        REAL :: f, x1, x2, eps, root
        REAL :: a, b, c, z
        INTEGER :: i
        INTEGER, PARAMETER :: iter = 200
      
        if (f(x1)*f(x2) > 0.0) then
          write(*,*) 'Tidak punya solusi akar2 pada interval [x1,x2]'
          return
        end if
      
        a = x1
        b = x2
      
        DO i = 1, iter
          c = (a+b)/2 
          if (f(a)*f(c) < 0.0) then
            b = c
          else 
            a = c
          end if 
    
          z = abs(b-a)
          if (z < eps) exit 

          root = c
          write(*,*) i, a, b, root, z
        end DO
      
        root = c
      
        return
      end subroutine bisection      
    
      
end module akarfungsi