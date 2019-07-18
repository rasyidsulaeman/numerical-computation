program hermite
    implicit none

    integer :: i
    integer, parameter :: Ndata = 16
    real*8, dimension(Ndata) :: xdat, ydat

    write(*,*)
    write(*,*) '====================================='
    write(*,*) 'This code is written by Rasyid Sulaeman'
    write(*,*) 'Last updated in 16 April 2019'
    write(*,*) '====================================='
    write(*,*)

    open(unit = 11, file = 'datauts.dat', status = 'old', action = 'read')
    
    do i = 1, Ndata
        read(11,*) xdat(i), ydat(i)
    end do

    call hermite_kubik(Ndata, xdat, ydat)

end program hermite
