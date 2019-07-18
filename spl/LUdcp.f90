program LUdcp
    implicit none
    integer :: i,j,k, p
    integer, parameter :: ndims = 5
    double precision,dimension(ndims,ndims) :: a, L, U
    double precision,dimension(ndims) :: Y, X, tempL, tempA
    real*8, dimension(5,2) :: b
    double precision :: sum, tempB, det

    !Deklarasi matriksnya
    open(unit = 11, file = 'dataA.dat', status = 'old', action = 'read')
    read(11,*) a

    open(unit = 12, file = 'dataB.dat', status = 'old', action = 'read')
    read(12,*) b
    !Print matriks awal
    do i = 1, ndims
        write(*,*) (a(i,j), j = 1, ndims)
    end do

    write(*,*) 

    do i = 1, ndims
        write(*,*) (b(i,j), j = 1, 2)
    end do

    write(*,*) 

    U(:,:) = 0.d0
    L(:,:) = 0.d0

    !Membuat elemen matriks L dan U
    do i = 1 , ndims
        U(i,i) = 1.0
    end do 
    
    do p = 1, ndims
        
        !Matriks L
        j = p
        do i = j, ndims
            if (j==1) then
                L(i,1) = a(i,1)
            else 
                sum = 0.0
                do k = 1, j-1
                    sum = sum + L(i,k)*U(k,j)
                end do
                L(i,j) = a(i,j) - sum   
            end if 
        end do

        !Exchanging/pivoting
        if (L(p,p) == 0.0) then
            tempL = L(p,:)
            L(p,:) = L(p+1,:)
            L(p+1,:) = tempL

            tempA = a(p,:)
            a(p,:) = a(p+1,:)
            a(p+1,:) = tempA

            !tempB = b(p,:)
            !b(p) = b(p+1)
            !b(p+1) = tempB
        end if
        
        !Matriks U
        i = p
        do j = i, ndims
            if (i==1) then
                U(1,j) = a(1,j)/L(1,1)
            else
                sum = 0.0
                do k = 1, i-1
                    sum = sum + L(i,k)*U(k,j)
                end do
                U(i,j) = (a(i,j) - sum)/L(i,i)
            end if
        end do

    end do

    do i =1, ndims
        write(*,*) (L(i,j), j = 1, ndims)
    end do
    write(*,*) 

    do i =1 , ndims
        write(*,*) (U(i,j), j = 1, ndims)
    end do
    write(*,*) 

    !Menghitung matriks Y dengan substitusi maju
    Y(1) = b(1,1)/L(1,1)
    do i = 2, ndims
        sum = 0.0
        do j = 1, i-1
            sum = sum + L(i,j)*Y(j)          
        end do
        Y(i) = (b(i,i) - sum)/L(i,i)
    end do

    do i = 1, ndims
        write(*,*) Y(i)
    end do
    write(*,*) 

    !Menghitung matriks X dengan substitusi mundur
    X(ndims) = Y(ndims)
    do i = 1, ndims-1
        sum = 0.0
        do j = ndims-i+1, ndims
            sum = sum + U(ndims-1,j)*X(j)
        end do
        X(ndims-i) = Y(ndims-i) - sum
    end do

    do i = 1, ndims
        write(*,*) X(i)
    end do

    det = 1.d0
    do i =1, ndims
        det = det*L(i,i)
    end do

    write(*,*) 
    write(*,*) det
    
end program LUdcp