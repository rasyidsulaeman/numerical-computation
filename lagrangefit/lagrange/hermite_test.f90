program interpolasi_hermite
    implicit none
    real*8 :: dx, H_1, H_2, poly_x, L_derivative, L_koef, x
    integer :: i, j, k, npoint
    integer, parameter :: N = 10
    real*8, dimension(N) :: xdata, fdata, de_fdata

    npoint = 100

    open(unit= 11, file = 'hermite_data.dat', status = 'old', action = 'read')

    do i = 1, N
        read(11,*) xdata(i), fdata(i), de_fdata(i)
        write(*,*) xdata(i), fdata(i), de_fdata(i)
    end do

    write(*,*)
    
    open(unit= 12, file = 'datadata.dat', status = 'replace')

    !Hitung polynomial Hermite
    dx = (xdata(N) - xdata(1))/npoint

    do i = 1, npoint
        x = xdata(1) + dx*(i-1)
        H_1 = 0.d0
        H_2 = 0.d0
        poly_x = 0.d0

        do j = 1, N
            L_koef = 1.d0
            L_derivative = 0.d0
            do k =1, N
                if (j /= k) then
                    L_koef = L_koef*(x - xdata(k))/(xdata(j) - xdata(k))
                    L_derivative = L_derivative + 1/(xdata(j) - xdata(k))
                end if
            end do

            H_2 = (x - xdata(j))*(L_koef**2)
            H_1 = (1 - 2*(x - xdata(j))*L_derivative)*(L_koef**2)
            
            poly_x = poly_x + (H_1*fdata(j) + H_2*de_fdata(j))

        end do

        write(*,*) H_1, H_2
        write(12,*) x, poly_x
    end do




end program interpolasi_hermite