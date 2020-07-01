subroutine inputdata(xdata,fdata)
    implicit none
    integer, parameter :: Ndata = 10
    real*8, dimension(Ndata):: xdata, fdata
    integer :: i

    open(unit = 11, file = 'xdata.dat', status = 'old', action = 'read')
    do i = 1, Ndata
        read(11,*) xdata(i), fdata(i)
    end do
    close(11)

end subroutine
