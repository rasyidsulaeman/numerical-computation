subroutine hermite_kubik(N,xdata,fdata)
    implicit none
 
    integer, intent(in) :: N
    real*8, intent(in):: xdata(N), fdata(N)
    integer :: i, npoint, p, j
    real*8 :: x, dx, dum11, dum12, sum, dum21, dum22
    real*8 :: h11, h12, h21,h22, a1,a2, b1, b2
    real*8, dimension(0:3) :: xdummy, fdummy, H

    npoint = 200

    open(unit = 12, file = 'hermite_kubik.dat', status = 'replace')

    dx = (xdata(N)-xdata(1))/npoint

    do i = 1, npoint
        x = xdata(1) + dx*(i-1)

        xdummy(:) = 0.d0
        fdummy(:) = 0.d0

        do p = 1, N
            if ((x .LE. xdata(p+1)) .AND. (x .GE. xdata(p))) then
                if ((x .GE. xdata(1)) .AND. (x .LE. xdata(2))) then
                    xdummy(0) = 0.d0
                    xdummy(1) = xdata(p)
                    xdummy(2) = xdata(p+1)
                    xdummy(3) = xdata(p+2)
                    
                    fdummy(0) = 0.d0
                    fdummy(1) = fdata(p)
                    fdummy(2) = fdata(p+1)
                    fdummy(3) = fdata(p+2)

                else if ((x .GE. xdata(N-1)) .AND. (x .LE. xdata(N))) then
                    xdummy(0) = xdata(N-2)
                    xdummy(1) = xdata(N-1)
                    xdummy(2) = xdata(N)
                    xdummy(3) = 0.d0

                    fdummy(0) = fdata(N-2)
                    fdummy(1) = fdata(N-1)
                    fdummy(2) = fdata(N)
                    fdummy(3) = 0.d0
                else
                    xdummy(0) = xdata(p-1)
                    xdummy(1) = xdata(p)
                    xdummy(2) = xdata(p+1)
                    xdummy(3) = xdata(p+2)

                    fdummy(0) = fdata(p-1)
                    fdummy(1) = fdata(p)
                    fdummy(2) = fdata(p+1)
                    fdummy(3) = fdata(p+2)
           
                end if 

                !Kalkulasi Fungsi Interpolasi
                dum11 = (x-xdummy(2))/(xdummy(1)-xdummy(2))
                dum12 = (x-xdummy(1))/(xdummy(2)-xdummy(1))

                h11 = (1 - 2*(x-xdummy(1))/(xdummy(1)-xdummy(2)))*(dum11**2)
                h12 = (1 - 2*(x-xdummy(2))/(xdummy(2)-xdummy(1)))*(dum12**2)

                dum21 = (x-xdummy(2))/(xdummy(1)-xdummy(2))
                dum22 = (x-xdummy(1))/(xdummy(2)-xdummy(1))

                h21 = (x-xdummy(1))*(dum21**2)
                h22 = (x-xdummy(2))*(dum22**2)

                a1 = h11 + h21*(1/(xdummy(1)-xdummy(2)) + 1/(xdummy(1)-xdummy(0)))
                a2 = h22*(xdummy(2)-xdummy(3))/((xdummy(1)-xdummy(3))*(xdummy(1)-xdummy(2)))

                b1 = h12 + h22*(1/(xdummy(2)-xdummy(1)) + 1/(xdummy(2)-xdummy(3)))
                b2 = h21*(xdummy(1)-xdummy(0))/((xdummy(2)-xdummy(0))*(xdummy(2)-xdummy(1)))

                H(0) = h21*(xdummy(1)-xdummy(2))/((xdummy(0)-xdummy(2))*(xdummy(0)-xdummy(1)))
                H(1) = a1 + a2
                H(2) = b1 + b2
                H(3) = h22*(xdummy(2)-xdummy(1))/((xdummy(3)-xdummy(1))*(xdummy(3)-xdummy(2)))
                
                sum = 0.d0
                do j = 0, 3
                    sum = sum + H(j)*fdummy(j)
                end do

            end if 
            
        end do
  
        write(12,*) x, sum
    end do

end subroutine hermite_kubik

