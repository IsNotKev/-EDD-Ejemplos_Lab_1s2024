program main
    use avl_m
    implicit none
    
    type(avl) :: tree
    call tree%insert(20)
    call tree%insert(8)
    call tree%insert(3)
    call tree%insert(1)
    call tree%insert(0)
    call tree%insert(15)
    call tree%insert(30)
    call tree%insert(48)
    call tree%insert(26)
    call tree%insert(10)
    call tree%insert(7)
    call tree%insert(5)
    call tree%insert(60)
    call tree%insert(19)
    call tree%insert(11)
    call tree%insert(21)
    call tree%insert(3)

    call tree%delete(11)
    call tree%delete(10)
    call tree%delete(19)
    
    print *, "Escribiendo en preorden: "
    call tree%preorder()
    call tree%graph()
end program main