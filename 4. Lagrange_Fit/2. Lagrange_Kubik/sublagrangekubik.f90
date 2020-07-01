subroutine lagrange_kubik(N, xdata, fdata)
    implicit none

    integer, intent(in) :: N
    real*8, intent(in) :: xdata(N), fdata(N)
    real*8 :: dx, x, poly_x, koef
    integer :: i, npoint, j, k, l, c
    
    npoint = 500
    
    open(unit = 12, file = '../plots/lagrange_kubik_output.dat', status = 'replace')
    
    dx = (xdata(N) - xdata(1))/npoint

    do k = 1, npoint

        x = xdata(1) + dx*(k-1)
        poly_x = 0.d0

        do l = 1, N
            if ((x .GE. (xdata(l))) .AND. (x .LE. (xdata(l+1)))) then
    
                if ((x .GE. xdata(1)) .AND. (x .LE. xdata(2))) then
                    c = l+1
                else if ((x .GE. xdata(N-1)) .AND. (x .LE. xdata(N))) then
                    c = l-1
                else 
                    c = l
                end if 

                    do i = c-1,c+2 
                        koef = 1.d0

                        do j = c-1, c+2
                            if (i == j) then
                                koef = koef*1.d0
                            else
                                koef = koef*(x - xdata(j))/(xdata(i)-xdata(j))
                            end if 
                        end do

                        poly_x = poly_x + koef*fdata(i)
                    end do
            end if 
        end do

        write(12,*) x, poly_x
        
    end do
end subroutine lagrange_kubik
