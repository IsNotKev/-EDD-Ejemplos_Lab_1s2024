module MisFunciones
    implicit none
    
contains
    function cuadrado(x)
        real, intent(in) :: x
        real :: cuadrado
        cuadrado = x * x
    end function cuadrado

    subroutine imprimir()
        print *, "Hola mundo"
    end subroutine imprimir
    
end module MisFunciones


program hola_mundo
    use MisFunciones
    implicit none

    integer :: i
    real, dimension(5) :: mi_arreglo
    mi_arreglo = [1,2,3,4,5]

    do i = 1, 5
        print *, 'Elemento', i, ':', cuadrado(mi_arreglo(i))
    end do
  
end program hola_mundo