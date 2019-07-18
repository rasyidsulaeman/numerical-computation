subroutine lagrange(N,xdata,fdata)
    implicit none

    real*8, dimension(N), intent(in) :: xdata, fdata
    real*8 :: dx, koef
    real*8 :: x, px
    integer :: i, j,k, N, npoint
    real*8 :: hasil, eps
    real*8 :: x1, x2

    x1 = -0.75
    x2 = -0.65
    
    npoint = 1000

    eps = 1e-5

    dx = (xdata(N) - xdata(1))/real(npoint-1)


    open(unit = 12, file = 'hasildata_uts.dat', status ='replace')


    do k =1, npoint
        x = xdata(1) + dx*(k-1)
        px = 0.d0

        do i = 1, N
            koef = 1.d0
            do j = 1, N
                if (i==j) then
                    koef = koef*1.0
                else
                    koef = koef*(x - xdata(j))/(xdata(i)-xdata(j))
                end if 
            end do

            px = px + koef*fdata(i)
            
        end do

        write(12,*) x, px
    end do


    call secant(px,x1,x2,eps,hasil)

    write(*,*) hasil
    
end subroutine lagrange