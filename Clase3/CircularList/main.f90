module linked_list_module
    implicit none
    private

    !NODO
    type, public :: node
        private
        integer :: value
        type(node), pointer :: next => null()
    end type node

    !LISTA CIRCULAR
    type, public :: linked_list
        private
        type(node), pointer :: head => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: delete
    end type linked_list

contains

    !APPEND
    subroutine append(self, value)
        class(linked_list), intent(inout) :: self
        integer, intent(in) ::  value
        
        type(node), pointer :: new
        type(node), pointer :: aux
        allocate(new)

        new = node(value=value, next=null())

        if(associated(self%head)) then
            aux => self%head
            do while(.not. associated(aux%next, self%head))
                aux => aux%next
            end do
            new%next => self%head
            aux%next => new
        else    
            new%next => new
            self%head => new
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
            print *, current%value, ","

            if (associated(current%next, self%head)) exit  ! Salir al completar un ciclo (lista circular)
            
            current => current%next       
        end do
    end subroutine

    subroutine delete(self, value)
        class(linked_list), intent(inout) :: self
        integer, intent(in) :: value
        type(node), pointer :: current, previous

        if (.not. associated(self%head)) then
            print *, "La lista está vacía. No se puede eliminar el valor: ", value
            return
        end if

        current => self%head
        previous => null()

        ! Buscar el nodo con el valor dado
        do while (associated(current) .and. current%value /= value)
            previous => current
            current => current%next
        end do

        ! Si se encuentra el nodo, eliminarlo
        if (associated(current) .and. current%value == value) then
            if (associated(previous)) then
                ! El nodo a eliminar no es el primero
                previous%next => current%next
            else
                ! El nodo a eliminar es el primero
                self%head => current%next
            end if
            
            deallocate(current)
            print *, "Se ha eliminado correctamente el valor: ", value
        else
            print *, "No se ha encontrado el valor: ", value
        end if
    end subroutine delete
end module linked_list_module

program main
    use linked_list_module
    implicit none
    

    type(linked_list) :: list

    call list%append(1)
    call list%append(2)
    call list%append(3)
    call list%append(4)

    call list%print()

    call list%delete(2)

    call list%print()
end program main