module List_of_list_m
    implicit none
    private

    type :: sub_node
        character(:), allocatable :: value
        type(sub_node), pointer :: next => null()
    end type sub_node

    type :: node
        integer :: index
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
        type(sub_node), pointer :: list => null()
    contains
        procedure :: append
        procedure :: print
    end type node

    type, public :: List_of_list
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        ! procedure :: append
        procedure :: insert
        procedure :: printList
    end type List_of_list

contains

    subroutine insert(self, index, value)
        class(List_of_list), intent(inout) :: self
        character(len=*), intent(in) :: value
        integer, intent(in) :: index

        type(node), pointer :: aux
        type(node), pointer :: new
        allocate(new)

        if(.not. associated(self%head)) then
            allocate(aux)
            aux%index = index
            self%head => aux
            self%tail => aux
            call aux%append(value)
        else
            if(index < self%head%index) then
                self%head%prev => new
                new%next => self%head
                self%head => new

                new%index = index
                call new%append(value)
            else
                aux => self%head
                do while (associated(aux%next))
                    if(index < aux%next%index) then
                        if(index == aux%index) then
                            call aux%append(value)
                        else
                            new%next => aux%next
                            new%prev => aux
                            aux%next%prev => new
                            aux%next => new

                            new%index = index
                            call new%append(value)
                        end if
                        return
                    end if
                    aux => aux%next
                end do

                if(index == aux%index) then
                    call aux%append(value)
                else
                    self%tail%next => new
                    new%prev => self%tail
                    self%tail => new

                    new%index = index
                    call new%append(value)
                end if
            end if
        end if
    end subroutine insert

    subroutine printList(self)
        class(List_of_list) :: self
        type(Node), pointer :: aux

        aux => self%head

        do while(associated(aux))
            print *, 'Indice: ', aux%index
            call aux%print()
            print *, ""
            aux => aux%next
        end do
    end subroutine printList

    !Subrutina y funciones de sub nodo
    subroutine append(self, string)
        class(node), intent(inout) :: self
        character(len=*), intent(in) :: string

        type(sub_node), pointer :: aux
        type(sub_node), pointer :: new

        allocate(new)
        new%value = string

        if(.not. associated(self%list)) then
            self%list => new
        else
            aux => self%list
            do while(associated(aux%next))
                aux => aux%next
            end do
            aux%next => new
        end if
    end subroutine append

    subroutine print(self)
        class(node), intent(inout) :: self

        type(sub_node), pointer :: aux
        aux => self%list

        do while(associated(aux))
            print *, aux%value
            aux => aux%next
        end do
    end subroutine print
end module List_of_list_m

program main
    use List_of_list_m
    implicit none

    type(List_of_list) :: list
    call list%insert(1, 'hola')
    call list%insert(1, 'adios')
    call list%insert(2, 'hola2')
    call list%insert(2, 'adios2')
    call list%insert(4, 'hola4')
    call list%insert(4, 'adios4')
    call list%insert(3, 'hola3')

    call list%insert(0,'hola0')
    call list%insert(1,'bye')

    call list%printList()
end program main