program llagrange
    implicit none

    real*8, allocatable :: xdata(:), fdata(:)
    integer :: Ndata, i
   

    write(*,*)
    write(*,*) '====================================='
    write(*,*) 'This code is written by Rasyid Sulaeman'
    write(*,*) 'Last updated in 02 April 2019'
    write(*,*) '====================================='
    write(*,*)

    Ndata = 10

    allocate(xdata(Ndata))
    allocate(fdata(Ndata))

    xdata(:) = 0.d0
    fdata(:) = 0.d0

    open(unit = 11, file = 'xdata.dat', status = 'old', action = 'read')
    
    do i = 1, Ndata
        read(11,*) xdata(i), fdata(i)
        write(*,*) xdata(i), fdata(i)
    end do

    call lagrange(Ndata, xdata, fdata)

    
end program llagrange

