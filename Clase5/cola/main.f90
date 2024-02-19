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
        procedure :: delete
        procedure :: print
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

    subroutine delete(this)
        class(cola), intent(inout) :: this
        type(node), pointer :: temp

        if (.not. associated(this%head)) then
            print *, 'Cola esta vacia'
            return
        end if

        print *, 'Delete ', this%head%value
        temp => this%head
        this%head => this%head%next
        deallocate(temp)
    end subroutine delete

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
end module cola_module

program main
    use cola_module
    implicit none

    type(cola) :: my_cola

    call my_cola%append(1)
    call my_cola%append(2)
    call my_cola%append(3)
    call my_cola%append(4)
    call my_cola%append(5)

    call my_cola%print()

    call my_cola%delete()
    call my_cola%delete()
    call my_cola%delete()
    call my_cola%delete()
    call my_cola%delete()
    call my_cola%delete()

    call my_cola%print()

end program main