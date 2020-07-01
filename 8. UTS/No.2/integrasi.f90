program integrasi
    implicit none
    real*8 :: a, b, hasil_simp, hasil_trapz
    integer :: N
    real*8, external :: func, fn
    character(len=20) :: method, filename
    
    write(*,*) 'Masukan input batas bawah x1'
    read(*,*) a
    write(*,*) 'Masukan input batas atas x2'
    read(*,*) b
    write(*,*) 'Masukan jumlah potongan yang mau dibuat'
    read(*,*) N
    write(*,*) 
    
    write(*,*) 'Choose the method : [fitting,lagrange]'
    read(*,*) method
    
    filename = trim('result_') // trim(method) // trim('.dat')
    
    open(unit=12,file=filename,status='replace,action='write')
    if (method == 'fitting') then
        call trapezoid(func, b, a, N, hasil_trapz)
        call simpson(func, b, a, N, hasil_simp)
    else if (method == 'lagrange') then
        call trapezoid(fn, b, a, N, hasil_trapz)
        call simpson(fn, b, a, N, hasil_simp)
    end if
    
    write(12,*) 'Method that you are used : ', method
    write(12,*)
    write(12,*) 'Hasil trapezoid'
    write(12,*) hasil_trapz /50.
    write(12,*)
    write(12,*) 'Hasil simpson'
    write(12,*) hasil_simp /50.
    
end program integrasi

double precision function func(x)
    implicit none
    real*8, intent(in) :: x
    real*8 :: f
    
    func = 47.200498667572901 + 0.79793716760550115 * x + -0.29335946771921850 * (x**2) &
    + 6.0818072298816417E-002 *(x**3) + -7.8688235935708527E-003*(x**4) + 5.6798048843128613E-004 * (x**5) &
    + -2.2940329923505434E-005 * (x**6) + 5.1698142214747504E-007*(x**7) + -6.0789596921273753E-009 * (x**8) &
    + 2.9073085223293729E-011 * (x**9)
    
end function

double precision function fn(x)
    implicit none
    real*8, intent(in) :: x
    real*8, external :: lagrange
    integer, parameter :: Ndata = 11
    real*8,dimension(Ndata) :: xdata, fdata

    call inputdata(xdata, fdata)

    fn = lagrange(x, xdata, fdata)
    
    call lagrange_plot(Ndata, xdata, fdata)
    
 end function 

