module hash_module
    implicit none
    private
    public:: insert, print

    integer, parameter :: SIZE = 12
    integer :: table(SIZE)
contains
    subroutine insert(key)
        integer, intent(in) :: key
        integer :: index, hash

        ! Calcular el índice usando la función de hash
        hash = modulo(key, SIZE)

        ! Reorganizar elementos si hay colisión
        if (table(hash) == 0) then
            table(hash) = key
        else
            index = hash
            do while (table(index) /= 0)
                index = modulo(index + 1, SIZE)
            end do
            table(index) = key
        end if
    end subroutine insert

    subroutine print
        integer :: i

        print * , 'Tabla hash:'
        do i = 1, SIZE
            print *, 'Posición ', i, ': ', table(i)
        end do
    end subroutine print

end module hash_module

program main
    use hash_module
    implicit none
    
    call insert(1)
    call insert(1)
    call insert(23)
    call insert(15)
    call insert(5)

    call print()
end program main