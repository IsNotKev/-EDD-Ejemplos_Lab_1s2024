module List_of_list_m
    implicit none
    private

    type :: sub_node
        integer :: value
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
        procedure :: graph
    end type List_of_list
    contains
    subroutine insert(self, index, value)
        class(List_of_list), intent(inout) :: self
        integer, intent(in) :: value
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
        integer, intent(in) :: string

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
    end subroutine 
    
    subroutine graph(self)
        class(List_of_list), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: command
        type(node), pointer :: current_node
        type(sub_node), pointer :: current_subnode
    
        command = "dot -Tpng ./graph.dot -o ./graph.png"
        io = 1
    
        ! Abrir archivo DOT
        open(newunit=io, file="./graph.dot")
        write(io, *) "digraph G {"
    
        ! Recorrer la lista principal
        current_node => self%head
        do while(associated(current_node))
            ! Escribir el nodo principal
            write(io, *) current_node%index, "[label = """, current_node%index, """]"
            
            ! Recorrer la sublista
            current_subnode => current_node%list
            do while(associated(current_subnode))
                ! Escribir el subnodo y la conexión bidireccional con el nodo principal
                write(io, *) current_node%index, " -> ", current_subnode%value, " [dir = normal];"
                current_subnode => current_subnode%next
            end do
            current_node => current_node%next
        end do
    
        write(io, *) "}"
        close(io)
    
        ! Ejecutar Graphviz para generar la imagen
        call execute_command_line(command, exitstat=i)
    
        if (i /= 0) then
            print *, "Error al crear la imagen."
        else
            print *, "Imagen creada con éxito."
        end if
    end subroutine graph
end module List_of_list_m

program main
    use List_of_list_m

    type(List_of_list) :: adjacency_list

    ! Insertar algunos datos de ejemplo
    call adjacency_list%insert(1, 2)
    call adjacency_list%insert(2, 3)
    call adjacency_list%insert(3, 1)
    call adjacency_list%insert(4, 1)
    call adjacency_list%insert(4, 2)
    call adjacency_list%insert(1, 3)

    ! Imprimir la lista de adyacencia
    print *, "Lista de adyacencia:"
    call adjacency_list%printList()

    ! Generar el archivo DOT y la imagen del grafo
    call adjacency_list%graph()

    print *, "Se ha generado la imagen del grafo."
end program main
