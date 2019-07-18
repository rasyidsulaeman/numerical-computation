program integrasi
    implicit none
    real*8 :: a, b, hasil_simp, hasil_trapz
    integer :: N

    write(*,*) 
    write(*,*) 'Masukan input batas bawah a'
    read(*,*) a
    write(*,*) 'Masukan input batas atas b'
    read(*,*) b
    write(*,*) 'Masukan jumlah potongan yang mau dibuat'
    read(*,*) N

    call trapezoid(b, a, N, hasil_trapz)
    call simpson(b, a, N, hasil_simp)

    write(*,*) 'Hasil trapezoid'
    write(*,*) hasil_trapz
    write(*,*)
    write(*,*) 'Hasil simpson'
    write(*,*) hasil_simp

end program integrasi

! double precision function func(x)
!     implicit none
!     real*8, intent(in) :: x
!     real*8 :: lagrange
!     integer, parameter :: Ndata = 16
!     real*8 :: xdata(Ndata), fdata(Ndata)

!     call inputdata(xdata, fdata)

!     func = lagrange(x, xdata, fdata)
!     call lagrange_plot(Ndata, xdata, fdata)
    
! end function func


