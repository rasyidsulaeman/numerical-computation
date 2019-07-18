subroutine bisection(f, x1,x2,eps,root)
  
  implicit none
  REAL :: f, x1, x2, eps, root
  REAL :: a, b, c
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
  
    if (abs(b-a) < eps) exit 
  
    write(*,*) a, b, c
  end DO

  root = (a+b)/2

  return
end subroutine bisection
