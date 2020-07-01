program fitting
    implicit none
    integer:: j, k, i,npoint
    integer, parameter ::orde = 8, N = 11
    double precision, dimension(0:orde,0:orde) ::c
    double precision, dimension(0:orde) :: b, hasil
    double precision, allocatable :: x(:), px(:)
    double precision, dimension(N):: xdata, fdata
    double precision :: sum, sum1, dx
    
    write(*,*)
    write(*,*) '====================================='
    write(*,*) 'This code is written by Rasyid Sulaeman'
    write(*,*) 'Last updated in 02 April 2019'
    write(*,*) '====================================='
    write(*,*)

    open(unit = 11, file = 'data.dat', status = 'old', action = 'read')
    
    write(*,*) 'data x and f(x)'
    do i = 1, N
        read(11,*) xdata(i), fdata(i)
        write(*,*) xdata(i), fdata(i)
    end do

    write(*,*) 
    
    c(:,:) = 0.0
    b(:) = 0.0

    !Calculate matriks C and B
    do k = 0, orde
        do j = 0, orde
           sum = 0.0
            do i = 1, N
                sum = sum + xdata(i)**(j+k)
            end do
            c(k,j) = sum
        end do

            sum1 = 0.0
            do i = 1, N
                sum1 = sum1 + fdata(i)*(xdata(i)**(k))
            end do
            b(k) = sum1
    end do
    
    write(*,*) 'Matriks c'
    do k = 0, orde
        write(*,*) (c(k,j), j =0, orde)
    end do

    write(*,*) 'Matriks b'
    do k = 0, orde
        write(*,*) b(k)
    end do

    write(*,*) '========================================================================'
    
    write(*,*) 'Solving..'
    write(*,*) 'Out : koefisien persamaan ai'
    
    !call gausselim(orde+1, c, b, hasil)
    call LUdcp(orde+1,c,b,hasil)
    
    open(unit = 10, file='koefisien_orde8.dat', action='write')
    write(*,*) 
    write(*,*) 'Koefisien polinomial'
    write(*,*) hasil
    write(10,*) hasil
    
    npoint = 1000

    allocate(x(npoint)) 
    allocate(px(npoint))
    
    dx = (xdata(N) - xdata(1))/real(npoint-1)
 
    open(unit= 13, file = 'dataorde8.dat', action='write')
    
    !Calculate polynomial orde m
    do i = 0, npoint
        x(i) = xdata(1) + real(i)*dx
        sum = 0.0
        do j= 0, orde
            sum = sum + (hasil(j))*(x(i)**(j))
        end do
        px(i) = sum
        write(13,*) x(i), px(i)
    end do

end program fitting
