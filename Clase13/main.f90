program main
    use json_module
    implicit none

    type(json_core) :: json
    type(json_value), pointer :: p, data, path

    call json%initialize()

    call json%create_object(p,'')

    CALL json%create_array(data,'DATA')

    call json%create_object(path,'')
    call json%add(path,'sucursal_0','s1')
    call json%add(path,'direccion_0','5ta ave Guatemala')
    call json%add(path,'sucursal_1','s2')
    call json%add(path,'direccion_1','4ta ave Guatemala')
    call json%add(path,'total',200)

    call json%add(data,path)

    call json%create_object(path,'')
    call json%add(path,'sucursal_1','s2')
    call json%add(path,'direccion_1','5ta ave Guatemala')
    call json%add(path,'sucursal_2','s3')
    call json%add(path,'direccion_2','4ta ave Guatemala')
    call json%add(path,'total',200)

    call json%add(data,path)

    call json%add(p,'INDEX', 0)
    call json%add(p,'NONCE', 4560)
    call json%add(p, data)
    call json%add(p,'TIMESTAMP', '2021-06-01 12:00:00')

    nullify(data)
    nullify(path)

    call json%print(p,'C:\Users\kevin\Escritorio\Tareas\Practicas\-EDD-Ejemplos_Lab_1s2024\Clase13\example.json')

    print *, 'JSON file created'

    call json%destroy(p)
end program main