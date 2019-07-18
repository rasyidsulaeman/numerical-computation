program hiperbolik
    implicit none

    real*8, parameter :: pi = 2.d0*asin(1.d0)
    real*8, external :: rho
    real*8, allocatable :: x(:), b(:), t(:), psi(:,:)
    real*8 :: hx, ht, koef_ddt, c, stable, alpha, beta, gamma, xN, x0, tN, t0, thres
    integer :: i,j,Nx,Nt

    koef_ddt = 123
    c = sqrt(1/koef_ddt)

    !Nilai batas
    x0 = 0
    xN = 1.d0
    t0 = 0
    tN = 5.d0

    !Jumlah titik
123 write(*,*) "Masukan nilai Nx : "
    read(*,*) Nx
    hx = (xN - x0)/Nx

    thres = (tN-t0)*c/hx

    write(*,*) "Untuk Nx =", Nx," ambil Nt >=", thres
    write(*,*) "Masukan nilai Nt : "
    read(*,*) Nt

    ht = (tN - t0)/Nt

    stable = c*(ht/hx)
    write(*,*) "Stabitizer =", stable

    if (stable<=1.d0) then
        write(*,*) "Sistem stabil. Proses perhitungan sedang berlangsung......."
    else
        write(*,*) "Sistem belum stabil! Silakan sesuaikan Nx dan Nt kembali~"
        write(*,*)
        goto 123
    end if
    
    !Alokasi memori
    allocate(x(0:Nx))
    allocate(b(0:Nx))
    allocate(t(0:Nt))
    allocate(psi(0:Nx,0:Nt))

    x(0) = x0
    x(Nx) = xN
    t(0) = t0
    t(Nt) = tN

    !Mengisi x dan t
    do i = 1, Nx-1
        x(i) = x(i-1) + hx
    end do
    do i = 1, Nt-1
        t(i) = t(i-1) +ht
    end do

    psi(:,:) = 0.d0
    !Syarat batas
    psi(0,:) = 0.d0
    psi(Nx,:) = 0.d0
    do i=1, Nx-1
        psi(i,0) = exp(-100.d0*(x(i) - 0.5d0)**2)
    end do
    b(:) = 0.d0

    !Perhitungan 
    do j = 0, Nt-1
        do i = 1, Nx-1
            if (j==0) then
                alpha = 2*(c**2)*(ht**2)*pi*rho(x(i),t(0))
                beta = b(i)*ht
                gamma = (c**2)*(ht**2)/((hx**2)*2)
                psi(i,1) = alpha + beta + psi(i,0) + gamma*(psi(i+1,0) - 2*psi(i,0) + psi(i-1,0))
            else
                alpha    = 4*(c**2)*(ht**2)*pi*rho(x(i), t(j))
                beta     = 2*psi(i,j) - psi(i,j-1)
                gamma    = ((c**2)*(ht**2))/(hx**2)
                psi(i,j+1) = alpha + beta + gamma*(psi(i+1,j)-2*psi(i,j)+psi(i-1,j))
            end if 
        end do
    end do

    !Menulis output ke file 
    open(unit = 11, file ='PDP_Hiperbolic.dat', status = 'replace')
    do i = 0, Nx
        do j = 0, Nt
            write(11,*) x(i),"			",t(j),"			",psi(i,j)
        end do
        write(11,*)
    end do
    close(11)

    
	deallocate(x)
	deallocate(b)
	deallocate(t)
    deallocate(psi)
    
end program hiperbolik

double precision function rho(x,y)
    implicit none
    real*8 :: x,y

    rho = 0
end function rho