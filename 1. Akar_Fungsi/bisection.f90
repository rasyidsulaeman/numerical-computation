subroutine bisection(f,x1,x2,eps,root)
  implicit none
  REAL,EXTERNAL :: f
  REAL :: x1, x2, eps, root
  REAL :: a, b, c
  INTEGER :: i, step
  INTEGER, PARAMETER :: iter = 200
  
  if (f(x1)*f(x2) > 0.0) then
    write(*,*) 'Tidak punya solusi akar2 pada interval [x1,x2]'
    return
  end if
  
  a = x1
  b = x2
  
  step = 0
  DO i = 1, iter
    step = step + 1
    c = (a+b)/2 
    if (f(a)*f(c) < 0.0) then
      b = c
    else 
      a = c
    end if 
  
    if (abs(b-a) < eps) exit 
  
  end DO
  
  write(*,*) 'Total step : ', step

  root = (a+b)/2
  return
end subroutine bisection

