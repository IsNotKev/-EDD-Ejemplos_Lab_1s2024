module abb_m
    use uuid_module
    implicit none
    private

    type :: node
        integer :: value
        integer :: height = 1
        type(node), pointer :: right => null()
        type(node), pointer :: left => null()
    end type node

    type, public :: abb
        type(node), pointer :: root => null()
    contains
        procedure :: insert
        procedure :: delete
        procedure :: preorder
        procedure :: inorder
        procedure :: posorder
        procedure :: graph
    end type abb
contains
    !Subrutinas del tipo abb
    subroutine insert(self, val)
        class(abb), intent(inout) :: self
        integer, intent(in) :: val

        if (.not. associated(self%root)) then
            allocate(self%root)
            self%root%value = val
        else
            call insertRec(self%root, val)
        end if
    end subroutine insert

    !Subrutinas de apoyo
    recursive subroutine insertRec(root, val)
        type(node), pointer, intent(inout) :: root
        integer, intent(in) :: val

        if (val < root%value) then
            if (.not. associated(root%left)) then
                allocate(root%left)
                root%left%value = val
            else
                call insertRec(root%left, val)
            end if
        else if (val > root%value) then
            if (.not. associated(root%right)) then
                allocate(root%right)
                root%right%value = val
            else
                call insertRec(root%right, val)
            end if
        end if
    end subroutine insertRec

    subroutine delete(self, val)
        class(abb), intent(inout) :: self
        integer, intent(inout) :: val
    
        self%root => deleteRec(self%root, val)
    end subroutine delete

    recursive function deleteRec(root, key) result(res)
        type(node), pointer :: root
        integer, intent(in) :: key
        type(node), pointer :: res
        type(node), pointer :: temp

        if (.not. associated(root)) then
            res => root
            return
        end if

        if (key < root%value) then
            root%left => deleteRec(root%left, key)
        else if (key > root%value) then
            root%right => deleteRec(root%right, key)
        else
            if (.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp
                return
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
                return
            else
                call getMajorOfMinors(root%left, temp)
                root%value = temp%value
                root%left => deleteRec(root%left, temp%value)
            end if
        end if

        res => root
    end function deleteRec

    recursive subroutine getMajorOfMinors(root, major)
        type(node), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinors(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinors

    subroutine preorder(self)
        class(abb), intent(in) :: self
        
        call preorderRec(self%root)
    end subroutine preorder

    subroutine inorder(self)
        class(abb), intent(in) :: self
        
        call inordenRec(self%root)
    end subroutine inorder

    subroutine posorder(self)
        class(abb), intent(in) :: self
        
        call posordenRec(self%root)
    end subroutine posorder

    recursive subroutine preorderRec(root)
        type(node), pointer, intent(in) :: root

        if(associated(root)) then
            print *, root%value
            call preorderRec(root%left)
            call preorderRec(root%right)
        end if
    end subroutine preorderRec

    recursive subroutine inordenRec(root)
    type(node), pointer, intent(in) :: root

    if(associated(root)) then
        call inordenRec(root%left)
        print *, root%value
        call inordenRec(root%right)
    end if
end subroutine inordenRec

    recursive subroutine posordenRec(root)
    type(node), pointer, intent(in) :: root

        if(associated(root)) then
            call posordenRec(root%left)
            call posordenRec(root%right)
            print *, root%value
        end if
    end subroutine posordenRec

    subroutine graph(self, filename)
        class(abb), intent(in) :: self
        character(len=*), intent(in) :: filename
        integer :: io
        integer :: i
        character(len=100) :: dot_filename, dot_command, png_filename
    
        ! Agregar extensiones
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"
    
        ! Construir comandos para generar la imagen
        dot_command = "dot -Tpng " // trim(dot_filename) // " -o " // trim(png_filename)
    
        io = 1
        open(newunit=io, file=trim(dot_filename))
        write(io, *) "digraph G {"
        if(associated(self%root)) then
            call printRec(self%root, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)
        
        ! Ejecutar el comando para generar la imagen
        call execute_command_line(trim(dot_command), exitstat=i)
    
        ! Comprobar si hubo algún error al generar la imagen
        if (i /= 0) then
            print *, "Ocurrió un error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine graph

    recursive subroutine printRec(root, name, io)
        type(node), pointer :: root
        character(len=36) :: name
        integer :: io

        character(len=36) :: right
        character(len=36) :: left

        right = generate_uuid()
        left = generate_uuid()

        if(associated(root)) then
            write(io, *)'"Nodo'//name//'"[label = "', root%value, '"]'
            if(associated(root%left)) then
                write(io, *)'"Nodo'//name//'"->"Nodo'//left//'"'
            end if
            if(associated(root%right)) then
                write(io, *)'"Nodo'//name//'"->"Nodo'//right//'"'
            end if
            call printRec(root%left, left, io)
            call printRec(root%right, right, io)
        end if
    end subroutine printRec
end module abb_m