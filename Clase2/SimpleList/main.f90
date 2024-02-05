module linked_list_m
    implicit none
    private

    type, public ::node
        private
        integer :: value
        type(node), pointer :: next     
    end type node

    type, public :: linked_list
        private
        type(node), pointer :: head => null()
    contains
        procedure :: push
        procedure :: append
        procedure :: print
        procedure :: delete
        procedure :: search
    end type linked_list

contains

    subroutine push(this, value)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: value

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            temp%next => this%head
            this%head => temp
        end if

        print *, 'pushed ', value
    end subroutine push

    subroutine append(this, value)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: value

        type(node), pointer :: temp
        type(node), pointer :: current

        allocate(temp)
        temp%value = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            current => this%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => temp
        end if

        print *, 'appended ', value
    end subroutine append

    subroutine delete(this, value)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: value
        type(node), pointer :: current, previous

        current => this%head
        previous => null()

        ! Buscar el nodo a eliminar
        do while (associated(current) .and. current%value /= value)
            previous => current
            current => current%next
        end do

        ! Si se encontró el nodo
        if(associated(current) .and. current%value == value) then
            if(associated(previous)) then
                previous%next => current%next
            else
                this%head => current%next
            end if

            deallocate(current)
            print *, 'Se eliminó el valor ', value
        else
            print *, 'No se encontró el valor ', value
        end if

    end subroutine delete

    function search(this, value) result(retval)
        class(linked_list), intent(in) :: this
        integer, intent(in) :: value

        type(node), pointer :: current

        logical :: retval

        current => this%head
        retval = .false.

        do while(associated(current))
            if(current%value == value) then
                retval = .true.
                exit
            end if
            current => current%next
        end do

    end function search

    subroutine print(this)
        class(linked_list), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        do while (associated(current))
            print *, current%value
            current => current%next
        end do 
    end subroutine print
    
end module linked_list_m

program main
    use linked_list_m
    implicit none

    type(linked_list) :: list

    call list%append(1)
    call list%append(2)
    call list%append(3)
    call list%append(4)
    call list%append(5)

    print *, '//-----------------//'
    print *, 'La lista es:'
    print *, '//-----------------//'
    call list%print()

    print *, '//-----------------//'
    if(list%search(1)) then
        print *, 'El valor 1 esta en la lista'
    else
        print *, 'El valor 1 no esta en la lista'
    end if
    call list%delete(1)
    if(list%search(1)) then
        print *, 'El valor 1 esta en la lista'
    else
        print *, 'El valor 1 no esta en la lista'
    end if

    print *, '//-----------------//'
    print *, 'La lista es:'
    print *, '//-----------------//'
    call list%print()

end program main