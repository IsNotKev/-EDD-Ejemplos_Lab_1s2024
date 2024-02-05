! Mi primer programa en Fortran

module MisFunciones
    implicit none

    contains

    function cuadrado(x)
        real, intent(in) :: x
        real :: cuadrado
        cuadrado = x * x
    end function cuadrado

end module MisFunciones

program hola_mundo
    use MisFunciones
    implicit none

    integer :: edad, i, io_status
    real :: resultado
    integer, dimension(5) :: mi_arreglo
    character(20) :: cadena1, cadena2
    
    do i = 1, 10, 2
        write(*, '(A, I0)', advance='no') 'Iteracion:', i
    end do
    write(*, '(A)') 'Fin!!!!'
    
    ! Asignar valores a las cadenas
    cadena1 = 'Hola, '
    cadena2 = 'mundo!'
    
    ! Concatenar cadenas
    print *, cadena1 // cadena2
    
    ! También puedes utilizar funciones específicas para manipular cadenas
    print *, 'Longitud de cadena1:', len_trim(cadena1)

    write(*, *) 'Escribe tu edad:'
    read(*, *, iostat=io_status) edad

    if (io_status /= 0) then
        ! Si io_status no es cero, la lectura fue incorrecta
        write(*,*) 'Error: Entrada no válida. Ingresar un número entero.'
        ! Puedes decidir aquí qué hacer, como asignar un valor predeterminado a 'edad' o salir del programa.
    else
        if (edad < 18) then
            print *, 'Eres menor de edad.'
        else
            print *, 'Eres mayor de edad.'
        end if
    end if

    do i = 1, 5
        print *, 'Iteracion:', i
    end do

    ! Uso de la función
    resultado = cuadrado(4.0)
    print *, 'El cuadrado de 4.0 es:', resultado

    ! Asignar valores al arreglo
    mi_arreglo = [1, 2, 3, 4, 5]

    do i = 1, 5
        print *, 'Elemento', i, ':', mi_arreglo(i)
    end do

end program hola_mundo