program eigen
    implicit none
    real*8, allocatable :: A(:,:), x(:),x0(:), y(:), u(:), lambda(:)
    real*8 :: sum, z, eps, up, down, temp(2), y0(2)
    real*8 :: temp1(2), temp2(2)
    integer :: i,j,k,N,h, Nk, is

    !Input
    N = 2
    Nk = 3
    eps = 1d-3

    !Alokasi memori
    allocate(A(N,N))
    allocate(x(N))
    allocate(x0(N))
    allocate(y(N))
    allocate(u(N))
    allocate(lambda(N))

    !Matriks A dari persamaan
    ! A(0,0) = f(1)/(h**2)
    ! do i = 1, N-1
    !     A(i,i) = -2*f(i)/(h**2)
    ! end do
    ! A(N,N) = f(N)/(h**2)

    !Matriks A dari data
    do i = 1, N
        A(1,1) = 0
        A(1,2) = 1
        A(2,1) = 2
        A(2,2) = 0
    end do

    !Cek matriks A
    do i = 1, N
        write(*,*) (A(i,j), j = 1, N)
    end do

    !Matriks X
    do i= 1,N
        x(1) = 1
        x(2) = 2
        write(*,*) x(i)
    end do

    do k = 1, Nk !Jumlah iterasi

        x0(:) = x(:)

        y = 0.d0
        do i = 1, N
            do j = 1, N
                y = y + matmul(A,x)
            end do
        end do

        do i = 1, N
            temp1 = temp1 + x0*y
        end do

            lambda = temp1(/temp2
            write(*,*) lambda
        end do

            ! sum = 0.d0
            ! do is = 1, N
            !     sum = sum + (y(is)**2)
            ! end do

            ! !Eigen function
            ! u(i) = y(i)/sum

            ! !Uji Konvergensi
            ! z = abs(1 - lambda(i)/lambda(i+1))

            ! if (z < eps) then
            !     write(*,*) 'Konvergen'
            !     exit
            ! end if

            ! x(i) = y(i)
        

    end do

end program eigen

!Fungsi persamaan diferensial
double precision function f(x)
    implicit none
    real*8 :: x

    f = x**2
    return
end function f