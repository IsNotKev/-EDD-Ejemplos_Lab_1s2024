module cola_module
    implicit none
    private

    type, public :: node
        private
        integer :: value
        type(node), pointer :: next     
    end type node

    type, public :: cola
        private
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: bubble_sort
    end type cola

contains

    subroutine append(this, value)
        class(cola), intent(inout) :: this
        integer, intent(in) :: value

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%tail => temp
        else
            this%tail%next => temp
            this%tail => temp
        end if

        print *, 'Append ', value
    end subroutine append

    subroutine print(this)
        class(cola), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        print *, '//-----------------//'
        print *, 'La cola es:'
        print *, '//-----------------//'

        do while (associated(current))
            print *, current%value
            current => current%next
        end do 
    end subroutine print

    subroutine bubble_sort(this)
        class(cola), intent(inout) :: this
        type(node), pointer :: current, next_node
        integer :: temp

        if (.not. associated(this%head)) then
            print *, 'La cola esta vacia'
            return
        end if

        current => this%head
        do while(associated(current))
            next_node => current%next
            do while(associated(next_node))
                if (current%value < next_node%value) then
                    temp = current%value
                    current%value = next_node%value
                    next_node%value = temp
                end if
                next_node => next_node%next
            end do
            current => current%next
        end do
    end subroutine bubble_sort
end module cola_module

program main
    use cola_module
    implicit none

    type(cola) :: my_cola

    call my_cola%append(5)
    call my_cola%append(3)
    call my_cola%append(7)
    call my_cola%append(1)
    call my_cola%append(9)

    call my_cola%print()

    call my_cola%bubble_sort()

    call my_cola%print()
end program main