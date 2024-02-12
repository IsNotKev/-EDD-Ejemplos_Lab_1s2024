program main
    use linked_list_module
    implicit none

    type(linked_list) :: list

    call list%append("Hola")
    call list%append("Mundo")
    call list%append("1")
    call list%append("FIN")

    call list%print()

    call list%delete("Mundo")
    call list%print()

end program main