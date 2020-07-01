program integrasi
    implicit none
    real*8 :: a, b, hasil_simp, hasil_trapz
    integer :: N
    real*8, external :: func, fn
    real*8:: va, delta_trapz, delta_simp

    write(*,*) 'Masukan input batas bawah x1'
    read(*,*) a
    write(*,*) 'Masukan input batas atas x2'
    read(*,*) b
    write(*,*) 'Masukan jumlah potongan yang mau dibuat'
    read(*,*) N
    write(*,*) 

    call trapezoid(fn, b, a, N, hasil_trapz)
    call simpson(fn, b, a, N, hasil_simp)

    write(*,*) 'Hasil trapezoid'
    write(*,*) hasil_trapz
    write(*,*)
    write(*,*) 'Hasil simpson'
    write(*,*) hasil_simp
        
end program integrasi

double precision function func(x)
    implicit none
    real*8, intent(in) :: x
    real*8, parameter :: n = 2. !mol 
    real*8, parameter :: T = 303. !Kelvin
    real*8, parameter :: R = 8.3145 !J.mol^-1.K^-1
    
    func = n*R*T*(1.0/x)
    
end function

double precision function fn(x)
    implicit none
    real*8, intent(in) :: x
    real*8, external :: lagrange
    integer, parameter :: Ndata = 10
    real*8,dimension(Ndata) :: xdata, fdata

    call inputdata(xdata, fdata)

    fn = lagrange(x, xdata, fdata)
    
    call lagrange_plot(Ndata, xdata, fdata)
    
 end function 

