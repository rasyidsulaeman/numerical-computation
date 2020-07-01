program llag_kubik
    implicit none

    integer :: i
    integer, parameter :: Ndata = 10
    real*8, dimension(Ndata) :: xdat, ydat

    write(*,*)
    write(*,*) '====================================='
    write(*,*) 'This code is written by Rasyid Sulaeman'
    write(*,*) 'Last updated in 16 April 2019'
    write(*,*) '====================================='
    write(*,*)

    open(unit = 11, file = 'xdata.dat', status = 'old', action = 'read')
    
    do i = 1, Ndata
        read(11,*) xdat(i), ydat(i)
    end do

    call lagrange_kubik(Ndata, xdat, ydat)

end program llag_kubik
