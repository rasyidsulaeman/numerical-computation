program gausselim
    implicit none
    integer, parameter :: n = 4
    real, dimension(n,n) :: a, a0
    real, dimension(n) :: b, b0, x, temp
    real :: sum, temp1
    integer :: i,j,k

    open(unit = 11, file = 'dataA.dat', status = 'old', action = 'read')

    open(unit = 12, file = 'dataB.dat', status = 'old', action = 'read')
    
    !Membuat matriks segitiga 
    !Matriks awal

    do i = 1, n
        read(11,*) (a0(i,j), j = 1, n)
        write(*,*) (a0(i,j), j = 1, n)
    end do

    write(*,*) 
    
    read(12,*) b0
    do i = 1, n
        write(*,*) b0(i)
    end do

    write(*,*) 
    a = a0
    b = b0
    x = 0.0
    temp = 0.0
    !Kalkulasi
    do k = 1, n-1

        !Pivoting
        if (a0(k,k) == 0.0) then
            temp = a0(k,:)
            a0(k,:) = a0(k+1,:)
            a0(k+1,:) = temp

            temp1 = b0(k)
            b0(k) = b0(k+1)
            b0(k+1) = temp1

            a(k,:) = a0(k,:)
            b(k) = b0(k)
        end if
        
        do i = k+1, n
            do j = k, n
                a(i,j) = a0(i,j) - (a0(i,k)*a0(k,j))/a0(k,k)
            end do 
            b(i) = b0(i) - (a0(i,k)*b0(k))/(a0(k,k))
        end do
        a0 = a
        b0 = b
    end do

    !Matriks segitiga
    do i = 1, n
        write(*,*) (a(i,j), j = 1, n)
    end do

    write(*,*)
    do i = 1, n
        write(*,*) b(i)
    end do

    write(*,*) 

    x(n) = b(n)/a(n,n)
    do j = 1, n-1
        sum = 0.0
        do k = n-j+1, n 
            sum = sum + a(n-j, k)*x(k)
        end do
        x(n-j) = (b(n-j) - sum)/(a(n-j,n-j))
    end do

    write(*,*)
    do i = 1, n
        write(*,*) x(i)
    end do

end program gausselim
