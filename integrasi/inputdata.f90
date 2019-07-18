subroutine inputdata(xdata, fdata)
    implicit none
    integer, parameter :: Ndata = 16
    real*8, dimension(Ndata) :: xdata, fdata
    integer :: i

    open(unit = 11, file = 'datainput.dat', status = 'unknown', action = 'read')
    do i = 1, Ndata
        read(11,*) xdata(i), fdata(i)
    end do

end subroutine inputdata