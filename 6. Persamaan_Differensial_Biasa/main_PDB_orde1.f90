program calc_PDB
    implicit none
    real*8 :: x0,x1
    integer :: n
    real*8 :: y0

    !Input batas atas dan bawah
    x0 = 0
    x1 = 1

    !Input kondisi batas awal
    y0 = 1

    !Input jumlah bagian yang mau dibagi
    n = 100
    
    !Solve it by the following method
    call runge_kutta4th(x0,x1,n,y0)

end program calc_PDB

