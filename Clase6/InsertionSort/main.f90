program main
    implicit none 
    integer :: a(0:9)

    a = [2,1,4,6,10,8,9,7,7,10] 
    call insertionSort(a)
    print *, a

contains
    subroutine insertionSort(array)
        integer, intent(inout) :: array(:)
        integer :: n,i,j,key

        n = size(array)
        do i = 2, n
            key = array(i)
            j = i - 1

            do while ((j>=1) .and. (array(j) > key))
                array(j+1) = array(j)
                j = j - 1               
            end do
            array(j+1) = key
        end do
    end subroutine insertionSort
end program main