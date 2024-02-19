module pila_module
    implicit none
    private

    type, public :: node
        private
        integer :: value
        type(node), pointer :: next     
    end type node

    type, public :: pila
        private
        type(node), pointer :: top => null()
    contains
        procedure :: push
        procedure :: pop
        procedure :: print
        procedure :: graficar
    end type pila

contains

    subroutine push(this, value)
        class(pila), intent(inout) :: this
        integer, intent(in) :: value

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%next => this%top
        this%top => temp

        print *, 'Pushed ', value
    end subroutine push

    subroutine pop(this)
        class(pila), intent(inout) :: this
        type(node), pointer :: temp

        if (.not. associated(this%top)) then
            print *, 'Pila esta vacia.'
            return
        end if
        temp => this%top

        print *, 'Popped', temp%value

        this%top => this%top%next
        deallocate(temp)
    end subroutine pop

    subroutine print(this)
        class(pila), intent(in) :: this
        type(node), pointer :: current

        current => this%top

        print *, '//-----------------//'
        print *, 'La pila es:'
        print *, '//-----------------//'

        do while (associated(current))
            print *, current%value
            current => current%next
        end do 
    end subroutine print

    subroutine graficar(this, filename)
        class(pila), intent(in) :: this
        character(len=*), intent(in) :: filename
    
        integer :: unit
        type(node), pointer :: current
        integer :: count
    
        ! Abrir el archivo DOT
        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph Pila {'
        write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=pink];' ! Aplicar atributos a todos los nodos
        ! Escribir nodos y conexiones
        current => this%top
        count = 0
        do while (associated(current))
            count = count + 1
            write(unit, *) '    "Node', count, '" [label="', current%value, '"];'
            if (associated(current%next)) then
                write(unit, *) '    "Node', count, '" -> "Node', count+1, '";'
            end if
            current => current%next
        end do 
    
        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
    
        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
    
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
    end subroutine graficar
    
end module pila_module

program main
    use pila_module
    implicit none

    type(pila) :: my_pila

    call my_pila%push(1)
    call my_pila%push(2)
    call my_pila%push(3)
    call my_pila%push(4)
    call my_pila%push(5)

    call my_pila%print()

    call my_pila%pop()
    call my_pila%pop()

    call my_pila%print()

    call my_pila%graficar('pila.dot')

end program main