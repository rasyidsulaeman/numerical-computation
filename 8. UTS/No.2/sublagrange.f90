subroutine lagrange_plot(N,xdata,fdata)
    implicit none
    real*8, dimension(N), intent(in) :: xdata, fdata
    real*8 :: dx, koef,eps
    real*8 :: x, px
    integer :: i, j,k, N, npoint
    real*8,external :: lagrange

    npoint = 100
    eps = 1e-5

    dx = (xdata(N) - xdata(1))/real(npoint-1)
    
    open(unit = 12, file = 'hasildata.dat', status ='replace')

    do k =1, npoint
        x = xdata(1) + dx*(k-1)
        px = lagrange(x,xdata,fdata)
        write(12,*) x, px
    end do
    close(12)
    
end subroutine lagrange_plot

double precision function lagrange(x, xdata, fdata)
    implicit none
    integer, parameter :: Ndata = 11
    real*8, dimension(Ndata), intent(in) :: xdata, fdata
    integer :: i, j
    real*8 :: koef, x, px
    
    px = 0.d0
    do i = 1, Ndata
        koef = 1.d0
        do j = 1, Ndata
            if (i==j) then
                koef = koef*1.0
            else
                koef = koef*(x - xdata(j))/(xdata(i)-xdata(j))
            end if 
        end do

        px = px + koef*fdata(i)
    end do
    
end function 
