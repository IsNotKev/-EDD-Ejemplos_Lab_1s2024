module matrix_m
    implicit none
    private

    type :: node_val
        private
        logical :: exists = .false.
        logical :: value
    end type node_val

    type :: node
        private
        integer :: i, j
        logical :: value = .false.
        type(node), pointer :: up => null()
        type(node), pointer :: down => null()
        type(node), pointer :: right => null()
        type(node), pointer :: left => null()
    end type node

    type, public :: matrix
        private
        type(node), pointer :: root
        integer :: width = 0
        integer :: height = 0
    contains
        procedure :: insert
        procedure :: insertRowHeader
        procedure :: insertColumnHeader
        procedure :: insertInRow
        procedure :: insertInColumn
        procedure :: searchRow
        procedure :: searchColumn
        procedure :: nodeExists
        procedure :: print
        procedure :: printColumnHeaders
        procedure :: getValue
        ! procedure :: printRowHeaders
    end type

contains
    subroutine insert(self, i, j, value) 
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: i
        integer, intent(in) :: j
        logical, intent(in) :: value

        type(node), pointer :: new
        type(node), pointer :: row
        type(node), pointer :: column

        allocate(new)
        new = node(i=i, j=j, value=value)

        if(.not. associated(self%root)) then
            allocate(self%root)
            self%root = node(i=-1, j=-1)
        end if

        row => self%searchRow(i)
        column => self%searchColumn(j)

        if(j > self%width) self%width = j
        if(i > self%height) self%height = i

        if(.not. self%nodeExists(new)) then
            if(.not. associated(column)) then
                column => self%insertColumnHeader(j)
            end if

            if(.not. associated(row)) then
                row => self%insertRowHeader(i)
            end if
            call self%insertInColumn(new, row)
            call self%insertInRow(new, column)
        end if
    end subroutine insert

    function searchColumn(self, j) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: j

        type(node), pointer :: actual
        actual => self%root

        do while(associated(actual))
            if(actual%j == j) return
            actual => actual%right
        end do
    end function searchColumn

    function searchRow(self, i) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i

        type(node), pointer :: actual
        actual => self%root

        do while(associated(actual))
            if(actual%i == i) return
            actual => actual%down
        end do
    end function searchRow

    function nodeExists(self, new) result(exists)
        class(matrix), intent(inout) :: self  
        type(node), pointer :: new
        
        logical :: exists
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        rowHeader => self%root
        exists = .false.

        do while(associated(rowHeader))
            if(rowHeader%i == new%i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == new%j) then
                        column%value = new%value
                        exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
        return
    end function nodeExists

    function insertRowHeader(self, i) result(newRowHeader)
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: i

        type(node), pointer :: newRowHeader
        allocate(newRowHeader)

        newRowHeader = node(i=i, j=-1)
        call self%insertInRow(newRowHeader, self%root)
    end function insertRowHeader

    subroutine insertInRow(self, new, rowHeader)
        class(matrix), intent(inout) :: self
        type(node), pointer :: new
        type(node), pointer :: rowHeader

        type(node), pointer :: actual
        actual => rowHeader

        do while(associated(actual%down))
            if(new%i < actual%down%i .and. new%i > actual%i) then
                new%down => actual%down
                new%up => actual
                actual%down%up => new
                actual%down => new
                exit
            end if
            actual => actual%down
        end do

        if(.not. associated(actual%down)) then
            actual%down => new
            new%up => actual
        end if
    end subroutine insertInRow

    function insertColumnHeader(self, j) result(newColumnHeader)
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: j

        type(node), pointer :: newColumnHeader
        allocate(newColumnHeader)

        newColumnHeader = node(i=-1, j=j)
        call self%insertInColumn(newColumnHeader, self%root)
    end function insertColumnHeader

    subroutine insertInColumn(self, new, columnHeader)
        class(matrix), intent(inout) :: self
        type(node), pointer :: new
        type(node), pointer :: columnHeader
        
        type(node), pointer :: actual
        actual => columnHeader
        do while(associated(actual%right))
            if(new%j < actual%right%j .and. new%j > actual%j) then
                new%right => actual%right
                new%left => actual
                actual%right%left => new
                actual%right => new
                exit
            end if
            actual => actual%right
        end do
        
        if(.not. associated(actual%right)) then
            actual%right => new
            new%left => actual
        end if
    end subroutine insertInColumn

    subroutine print(self)
        class(matrix), intent(inout) :: self  
        integer :: i
        integer :: j
        type(node), pointer :: aux
        type(node_val) :: val
        aux => self%root%down

        call self%printColumnHeaders()

        do i = 0, self%height
            print *, ""
            write(*, fmt='(I3)', advance='no') i
            do j = 0, self%width
                val = self%getValue(i,j)
                if(.not. val%exists) then
                    write(*, fmt='(I3)', advance='no') 0
                else
                    write(*, fmt='(L3)', advance='no') val%value
                end if
            end do
        end do
    end subroutine print

    subroutine printColumnHeaders(self)
        class(matrix), intent(in) :: self
        integer :: j

        do j=-1, self%width
            write(*, fmt='(I3)', advance='no') j
        end do
    end subroutine printColumnHeaders

    function getValue(self, i, j) result(val)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        type(node_val) :: val
        rowHeader => self%root

        do while(associated(rowHeader))
            if(rowHeader%i == i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == j) then
                        val%value = column%value
                        val%exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
    end function getValue
end module matrix_m

program main
    use matrix_m
    implicit none
    
    type(matrix) :: m
    call m%insert(1, 5, .true.)
    call m%insert(0, 3, .false.)  
    call m%insert(1, 1, .false.)
    call m%insert(4, 1, .true.)
    call m%insert(5, 3, .false.)
    call m%insert(5, 10, .true.)
    call m%insert(1, 1, .true.)

    call m%print()
end program main