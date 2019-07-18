program name
    implicit none
    real*8, parameter :: eps = 1.d-5, pi = 2.d0*asin(1.d0)
    real*8, external :: rho
    real*8, allocatable :: x(:), y(:), psi(:,:,:), error(:,:)
    real*8 :: h, C, errormax
    integer :: i,j,k,Nx,Ny,itermax,no_output
    no_output = 0

    !Jumlah titik
    Nx = 10
    Ny = 10
    itermax = 500

    !Alokasi memori
    allocate(x(0:Nx))
    allocate(y(0:Ny))
    allocate(psi(0:Nx,0:Ny,2))
    allocate(error(0:Nx,0:Ny))

    !Nilai batas
    x(0) = 0
    x(Nx) = 1
    y(0) = 0
    y(Ny) = 1.d0

    h =(x(Nx)-x(0))/Nx

    do i = 1, Nx-1
        x(i) = x(i-1)+h
    end do
    do i = 1, Ny-1
        y(i) = y(i-1)+h
    end do

    !Syarat batas 
    psi(:,:,:) = 0.d0
    do i = 0,Nx
        psi(i,0,:) = 1.d0
        psi(i,Ny,:) = 1.d0
    end do
    do i = 0, Ny
        psi(0,i,:) = -1
        psi(Nx,i,:) = -1
    end do

    !Perhitungan
    do k = 1, itermax
        do i = 1, Nx-1
            do j = 1, Ny-1
                C = (h**2)*pi*rho(x(i),y(j))
                psi(i,j,2) = C + 0.25d0*(psi(i+1,j,1) + psi(i-1,j,1) + psi(i,j-1,1) + psi(i,j+1,1))
            end do
        end do

        error(:,:) = abs(psi(:,:,1)-psi(:,:,2))
        errormax = maxval(error)

        if (errormax < eps) then
            write(*,*) 'Konvergensi tercapai pada iterasi:', k
            exit 
        end if 

        psi(:,:,1) = psi(:,:,2)

        if (k == itermax) then
            write(*,*) 'Konvergensi tidak tercapai'
            no_output = 1
        end if 
    end do

    if (no_output ==1) goto 123

    open(unit = 10, file = 'PDP_Eliptik.dat', status = 'unknown')
    do i = 0, Nx
        do j = 0, Ny
            write(10,*) x(i),"			",y(j),"			",psi(i,j,2)
        end do
        write(10,*)
    end do
    close(10)

123 deallocate(x)
    deallocate(y)
    deallocate(psi)
    deallocate(error)

end program name

double precision function rho(x,y)
    implicit none
    real*8 :: x, y
    real*8 :: func
    real*8, parameter :: pi = 2.d0*asin(1.d0)
    
    func = 4.0/pi

    rho = -func/(4.d0*pi)

end function rho