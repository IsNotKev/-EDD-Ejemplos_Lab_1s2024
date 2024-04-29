program Codificacion
    implicit none

    character(len=20) :: contrasena, contrasena_codificada, contrasena_decodificada
    character(len=1) :: char
    integer :: i, longitud
    real :: semilla
    integer :: codigo, codigo_decodificado

    ! Introducir la contraseña
    print*, "Introduce la contraseña:"
    read(*, '(A20)') contrasena

    semilla = 2

    ! Codificar la contraseña
    longitud = len_trim(contrasena)
    contrasena_codificada = ''
    do i = 1, longitud
        char = contrasena(i:i) ! Obtener el carácter en la posición i
        codigo = ichar(char) + nint(semilla)
        contrasena_codificada = trim(contrasena_codificada) // achar(mod(codigo, 127))
    end do

    ! Mostrar la contraseña codificada
    print*, "Contraseña encriptada:", trim(contrasena_codificada)

    ! Decodificar la contraseña
    contrasena_decodificada = ''
    do i = 1, longitud
        codigo_decodificado = mod(ichar(contrasena_codificada(i:i)) - nint(semilla), 127)
        contrasena_decodificada = trim(contrasena_decodificada) // achar(codigo_decodificado)
    end do

    ! Mostrar la contraseña decodificada
    print*, "Contraseña decodificada:", trim(contrasena_decodificada)

end program Codificacion