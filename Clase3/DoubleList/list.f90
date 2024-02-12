module linked_list_module
    implicit none
    private

    !NODO
    type, public :: node
        private
        character(:), allocatable :: value
        type(node), pointer :: next
        type(node), pointer :: prev
    end type node

    !LISTA
    type, public :: linked_list
        private
        type(node), pointer :: head => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: delete
    end type linked_list

contains

subroutine append(self, value)
    class(linked_list), intent(inout) :: self
    character(len=*), intent(in) :: value
    type(node), pointer :: current       
    type(node), pointer :: temp

    allocate(temp)
    temp%value = value
    temp%next => null()
    temp%prev => self%head

    if (.not. associated(self%head)) then
        self%head => temp
    else         
        current => self%head
        do while (associated(current%next))
            current => current%next
        end do
        current%next => temp
        temp%prev => current
    end if

    print *, "Se ha insertado correctamente el valor: ", value
end subroutine append

    subroutine print(self)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current

        if (.not. associated(self%head)) then
            print *, "La lista está vacía."
            return
        end if

        current => self%head

        do while (associated(current))
            print *, current%value
            current => current%next
        end do
    end subroutine

    subroutine delete(self, value)
        class(linked_list), intent(inout) :: self
        character(len=*), intent(in) :: value
        type(node), pointer :: current

        if(.not. associated(self%head)) then
            print *, "La lista está vacía."
            return
        end if

        current => self%head

        do while (associated(current) .and. current%value /= value)
            current => current%next
        end do

        if (associated(current) .and. current%value == value) then
            if (associated(current%prev)) then
                ! El nodo a eliminar no es el primero
                current%prev%next => current%next
            else
                ! El nodo a eliminar es el primero
                self%head => current%next
            end if
            
            ! Si el nodo a eliminar no es el último
            if (associated(current%next)) then
                current%next%prev => current%prev
            end if

            deallocate(current)
            print *, "Se ha eliminado correctamente el valor: ", value
        else
            print *, "El valor no se encuentra en la lista."
        end if

    end subroutine delete
end module linked_list_module