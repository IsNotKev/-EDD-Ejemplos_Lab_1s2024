module avl_m
    use uuid_module
    implicit none
    private

    type :: node
        integer :: value
        integer :: height = 1
        type(node), pointer :: right => null()
        type(node), pointer :: left => null()
    end type node

    type, public :: avl
        type(node), pointer :: root => null()
    contains
        procedure :: insert
        procedure :: delete
        procedure :: preorder
        procedure :: graph
    end type avl

contains    
    !Subrutinas del tipo avl
    subroutine insert(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        call insertRec(self%root, val)
    end subroutine insert

    subroutine delete(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        self%root => deleteRec(self%root, val)
    end subroutine

    subroutine preorder(self)
        class(avl), intent(in) :: self
        
        call preorderRec(self%root)
    end subroutine preorder

    subroutine graph(self)
        class(avl), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: command

        io = 1
        open(newunit=io, file="./avl_tree.dot")
        command = "dot -Tpng ./avl_tree.dot -o ./avl_tree.png"


        write(io, *) "digraph G {"
        if(associated(self%root)) then
            call printRec(self%root, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)
        
        call execute_command_line(command, exitstat=i)

        if ( i == 1 ) then
            print *, "Ocurrió un error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if

    end subroutine graph

    !Subrutinas de apoyo
    recursive subroutine insertRec(root, val)
        type(node), pointer, intent(inout) :: root
        integer, intent(in) :: val

        if(.not. associated(root)) then
            allocate(root)
            root = node(value=val)

        else if(val < root%value) then
            call insertRec(root%left, val)

        else if(val > root%value) then
            call insertRec(root%right, val)
        end if

        root%height = maxHeight(getHeight(root%left), getHeight(root%right)) + 1

        if(getBalance(root) > 1) then
            if(getBalance(root%right) < 0) then
                root%right => rightRotation(root%right)
                root => leftRotation(root)

            else
                root => leftRotation(root)
            end if
        end if

        if(getBalance(root) < -1) then
            if(getBalance(root%left) > 0) then
                root%left => leftRotation(root%left)
                root => rightRotation(root)
            
            else
                root => rightRotation(root)
            end if
        end if
    end subroutine insertRec

    recursive function deleteRec(root, val) result(res)
        type(node), pointer :: root
        integer, intent(in) :: val
        type(node), pointer :: res
        type(node), pointer :: temp

        if(.not. associated(root)) then
            res => root
            return
        end if 

        if(val < root%value) then
            root%left => deleteRec(root%left, val)

        else if(val > root%value) then
            root%right => deleteRec(root%right, val)
        
        else
            if (.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
            else
                call getMajorOfMinors(root%left, temp)
                root%value = temp%value
                root%left => deleteRec(root%left, temp%value)
            end if
        end if

        res => root
        if(.not. associated(root)) return

        root%height = maxHeight(getHeight(root%left), getHeight(root%right)) + 1

        if(getBalance(root) > 1) then
            if(getBalance(root%right) < 0) then
                root%right => rightRotation(root%right)
                root => leftRotation(root)

            else
                root => leftRotation(root)
            end if
        end if

        if(getBalance(root) < -1) then
            if(getBalance(root%left) > 0) then
                root%left => leftRotation(root%left)
                root => rightRotation(root)

            else
                root => rightRotation(root)
            end if
        end if

        res => root
        
    end function deleteRec

    function leftRotation(root) result(rootRight)
        type(node), pointer, intent(in) :: root
        type(node), pointer :: rootRight
        type(node), pointer :: temp  
        
        rootRight => root%right
        temp => root%right%left

        rootRight%left => root
        root%right => temp

        root%height = maxHeight(getHeight(root%left), getHeight(root%right)) + 1
        rootRight%height = maxHeight(getHeight(rootRight%left), getHeight(rootRight%right)) + 1
    end function leftRotation

    function rightRotation(root) result(rootLeft)
        type(node), pointer, intent(in) :: root
        type(node), pointer :: rootLeft
        type(node), pointer :: temp

        rootLeft => root%left
        temp => rootLeft%right

        rootLeft%right => root
        root%left => temp

        root%height = maxHeight(getHeight(root%left), getHeight(root%right) + 1)
        rootLeft%height = maxHeight(getHeight(rootLeft%left), getHeight(rootLeft%right) + 1)
    end function rightRotation

    function maxHeight(left, right) result(res)
        integer, intent(in) :: left
        integer, intent(in) :: right

        integer :: res
        res = right

        if(left >= right) then
            res = left
            return
        end if
    end function maxHeight

    function getHeight(n) result(res)
        type(node), pointer, intent(in) :: n
        integer :: res
        res = 0

        if(.not. associated(n)) return
        res = n%height
    end function getHeight

    function getBalance(root) result(res)
        type(node), intent(in) :: root

        integer :: res
        res = getHeight(root%right) - getHeight(root%left)
    end function getBalance

    recursive subroutine getMajorOfMinors(root, major)
        type(node), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinors(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinors

    recursive subroutine preorderRec(root)
        type(node), pointer, intent(in) :: root

        if(associated(root)) then
            print *, root%value
            call preorderRec(root%left)
            call preorderRec(root%right)
        end if
    end subroutine preorderRec

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

end module avl_m