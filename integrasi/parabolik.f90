program parabolik
    implicit none

    real*8, parameter :: pi = 2.d0*asin(1.d0), epsilon = 1.d-5
    real*8, external :: rho
    real*8, allocatable :: x(:), b(:), t(:), psi(:,:), margin(:)
    real*8 :: hx, ht, koef_ddt, c, stable, alpha, beta, gamma, tFinal, marginmax, thres, x0, xN, t0
    integer :: i,j,Nx,Nt,Nt_write

    koef_ddt = 123

    gamma = 1/koef_ddt

    !Nilai batas
    x0 = 0
    xN = 1.d0
    t0 = 0

    write(*,*) 'Masukan nilai t maksimal'
    read(*,*) tFinal

    !Jumlah titik

123 write(*,*) "Masukan nilai Nx:"
    read(*,*) Nx
    hx = (xN-x0)/Nx

    thres = 2*(tFinal-t0)*gamma/hx/hx

    write(*,*) "Untuk Nx=", Nx, "Ambil Nt >=", thres
    write(*,*) "Masukan nilai Nt :"
    read(*,*) Nt

    ht =(tFinal - t0)/Nt

    stable = gamma*ht/hx/hx
    write(*,*) "Stabilizer =", stable

    if (stable<=0.5d0) then
        write(*,*) "Sistem stabil. Proses perhitungan sedang berlangsung......."
    else
        write(*,*) "Sistem belum stabil! Silakan sesuaikan Nx dan Nt kembali~"
        write(*,*)
        goto 123
    end if

    !Alokasi Memori
    allocate(x(0:Nx))
    allocate(t(0:Nt))
    allocate(psi(0:Nx,0:Nt))
    allocate(margin(0:Nx))

    x(0) = x0
    x(Nx) = xN
    t(0) = t0
    t(Nt) = tFinal

    Nt_write = Nt

    do i = 1, Nx-1
        x(i) = x(i-1) + hx
    end do
    do i = 1, Nt-1
        t(i) = t(i-1) + ht
    end do

    psi(:,:) = 0.d0
    !Syarat batas
    psi(0,:) = 0
    psi(Nx,:) = 110
    do i=1, Nx-1
        psi(i,0) = 27
    end do

    !Perhitungan
    do j=0, Nt-1
        do i=1, Nx-1
            alpha = 4*gamma*ht*pi*rho(x(i), t(j))
            beta = gamma*ht/hx/hx
            psi(i,j+1) = alpha + psi(i,j) + beta*(psi(i+1,j) - 2*psi(i,j) + psi(i-1,j))
        end do

        margin(:) = abs(psi(:,j+1) - psi(:,j))
        marginmax = maxval(margin)

        if (marginmax < epsilon) then
            write(*,*) "Telah tercapai keadaan stasioner pada t =", t(j+1)
            Nt_write = j
            exit
        end if 
    end do

    if (Nt_write == j) then
        write(*,*) 
    else 
        write(*,*) "Perhitungannya belum stasioner"
    end if 

    open(unit =11, file = 'PDP_Parabolik.dat', status ='replace')
    do i = 0, Nx
        do j = 0, Nt_write
            write(11,*) x(i),"			",t(j),"			",psi(i,j)
        end do
        write(11,*)
    end do


    deallocate(x)
    deallocate(t)
    deallocate(psi)
    deallocate(margin)

end program parabolik

double precision function rho(x,y)
    implicit none
    real*8 :: x, y
    rho = 0
end function rho