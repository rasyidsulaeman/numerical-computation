subroutine newton_raphson(f, df, xlama,eps,root)
    implicit none

    real :: xbaru, xlama, f, df, z, root
    integer :: i
    integer, parameter :: iter = 100 
    real :: eps

    do i = 1, iter
        xbaru = xlama - f(xlama)/df(xlama)
        
        z = abs((xlama - xbaru)/xbaru)
        if (z < eps) then
            exit 
        end if 

        xlama = xbaru
        root = xlama
        write(*,*) root
    end do

    return
end subroutine newton_raphson


