module inventario_mod
    implicit none
    integer, parameter :: max_inventario = 1000
    integer :: num_inventarios = 0
    type :: inventario_t
        character(len=1024) :: nombre
        integer :: cantidad
        real :: precioUnitario
        character(len=256) :: ubicacion
    end type inventario_t
    type(inventario_t), allocatable :: inventarios(:)
contains



subroutine generar_informe_inventario(inventarios, num_equipos)
    implicit none
    type(inventario_t), intent(in) :: inventarios(:)
    integer, intent(in) :: num_equipos
    integer :: ios, i
    character(len=256) :: filename
    real :: valor_total

    filename = 'informe_inventario.txt'

    open(30, file=trim(filename), status='replace', action='write', iostat=ios)
    if (ios /= 0) then
        print *, 'Error al abrir el archivo de informe, codigo de error:', ios
        return
    end if

    write(30, '(A)') '==============================='
    write(30, '(A)') '      Informe de Inventario      '
    write(30, '(A)') '==============================='
    write(30, '(A)') ''
    write(30, '(A)') 'Equipo           Cantidad   Precio Unitario   Valor Total   Ubicacion'
    write(30, '(A)') '---------------------------------------------------------------------'

  
    do i = 1, num_equipos
        valor_total = inventarios(i)%cantidad * inventarios(i)%precioUnitario
        write(30, '(A20, I10, 1X, A, F10.2, 1X, A, F10.2, A20)') trim(inventarios(i)%nombre), inventarios(i)%cantidad, 'Q', inventarios(i)%precioUnitario, 'Q', valor_total, trim(inventarios(i)%ubicacion)
    end do

    close(30)
end subroutine generar_informe_inventario

subroutine procesar_eliminar_equipo(linea)
    character(len=*), intent(in) :: linea
    character(len=1024) :: nombreEquipo, cantidadStr, ubicacion
    integer :: cantidad, pos1, pos2, i
    logical :: encontrado

    pos1 = index(linea, ';')
    if (pos1 > 0) then
        nombreEquipo = trim(linea(1:pos1-1))
        pos2 = index(linea(pos1+1:), ';') + pos1
        if (pos2 > pos1) then
            cantidadStr = trim(linea(pos1+1:pos2-1))
            ubicacion = trim(linea(pos2+1:))
            read(cantidadStr, *) cantidad
            encontrado = .false.
            do i = 1, num_inventarios
                if (trim(inventarios(i)%nombre) == trim(nombreEquipo) .and. trim(inventarios(i)%ubicacion) == trim(ubicacion)) then
                    encontrado = .true.
                    if (inventarios(i)%cantidad >= cantidad) then
                        inventarios(i)%cantidad = inventarios(i)%cantidad - cantidad
                        print *, 'Stock eliminado - Nombre: ' // trim(nombreEquipo) // ', Cantidad: ', cantidad, ', Ubicacion: ' // trim(ubicacion)
                        print *, 'Inventario actual de ' // trim(nombreEquipo) // ': ', inventarios(i)%cantidad
                    else
                        print *, '===================================================='
                        print *, 'Error: Cantidad insuficiente en la ubicación especificada, tiene que ser una cantidad menor del inventario.'
                    end if
                    exit
                end if
            end do
            if (.not. encontrado) then
               
                print *, 'Error: El equipo no existe en la ubicacion especificada.'
            end if
        else
            print *, 'No se encontró el segundo delimitador ";" en la línea'
        end if
    else
        print *, 'No se encontró el delimitador ";" en la línea'
    end if
end subroutine procesar_eliminar_equipo

    subroutine redimensionar_inventarios(n)
        integer, intent(in) :: n
        type(inventario_t), allocatable :: temp(:)
        if (allocated(inventarios)) then
            allocate(temp(size(inventarios)))
            temp = inventarios
            deallocate(inventarios)
            allocate(inventarios(n))
            inventarios(1:size(temp)) = temp
        else
            allocate(inventarios(n))
        end if
    end subroutine redimensionar_inventarios

    subroutine procesar_agregar_stock(linea)
        character(len=*), intent(in) :: linea
        character(len=1024) :: nombreEquipo, cantidadStr, ubicacion
        integer :: cantidad, pos1, pos2, i
        logical :: encontrado

        pos1 = index(linea, ';')
        if (pos1 > 0) then
            nombreEquipo = trim(linea(1:pos1-1))
            pos2 = index(linea(pos1+1:), ';') + pos1
            if (pos2 > pos1) then
                cantidadStr = trim(linea(pos1+1:pos2-1))
                ubicacion = trim(linea(pos2+1:))
                read(cantidadStr, *) cantidad
                encontrado = .false.
                do i = 1, num_inventarios
                    if (trim(inventarios(i)%nombre) == trim(nombreEquipo)) then
                        if (inventarios(i)%cantidad + cantidad <= max_inventario) then
                            inventarios(i)%cantidad = inventarios(i)%cantidad + cantidad
                            encontrado = .true.
                            print *, 'Stock agregado - Nombre: ' // trim(nombreEquipo) // ', Cantidad: ', cantidad, ', Ubicacion: ' // trim(ubicacion)
                            print *, 'Inventario actual de ' // trim(nombreEquipo) // ': ', inventarios(i)%cantidad
                        else
                            print *, 'Error: No se puede agregar al inventario. Excede el máximo permitido.'
                        end if
                        exit
                    end if
                end do
                if (.not. encontrado) then
                    print *, 'Error: El equipo no existe en el inventario.'
                end if
            else
                print *, 'No se encontró el segundo delimitador ";" en la línea'
            end if
        else
            print *, 'No se encontró el delimitador ";" en la línea'
        end if
    end subroutine procesar_agregar_stock

    subroutine procesar_linea(linea)
        character(len=*), intent(in) :: linea
        character(len=1024) :: comando
        integer :: pos1

        pos1 = index(linea, ' ')
        if (pos1 > 0) then
            comando = trim(linea(1:pos1-1))
            select case (comando)
                case ('crear_equipo')
                    call procesar_crear_equipo(linea(pos1+1:), comando)
                case ('agregar_stock')
                    call procesar_agregar_stock(linea(pos1+1:))
                case default
                    print *, 'Comando no reconocido:', trim(comando)
            end select
        else
            print *, 'Formato de linea no valido:', trim(linea)
        end if
    end subroutine procesar_linea

 subroutine procesar_crear_equipo(linea, comando)
        character(len=*), intent(in) :: linea
        character(len=*), intent(in) :: comando
        character(len=1024) :: nombreEquipo, cantidadStr, precioUnitarioStr, ubicacion
        integer :: cantidad, pos1, pos2, pos3
        real :: precioUnitario

        ! Procesar la línea para extraer los valores
        pos1 = index(linea, ';')
        if (pos1 > 0) then
            nombreEquipo = trim(linea(1:pos1-1))
            pos2 = index(linea(pos1+1:), ';') + pos1
            if (pos2 > pos1) then
                cantidadStr = trim(linea(pos1+1:pos2-1))
                pos3 = index(linea(pos2+1:), ';') + pos2
                if (pos3 > pos2) then
                    precioUnitarioStr = trim(linea(pos2+1:pos3-1))
                    ubicacion = trim(linea(pos3+1:))
                    read(cantidadStr, *) cantidad
                    read(precioUnitarioStr, *) precioUnitario

                    num_inventarios = num_inventarios + 1
                    if (.not. allocated(inventarios)) then
                        allocate(inventarios(1))
                    else
                        call redimensionar_inventarios(num_inventarios)
                    end if
                    inventarios(num_inventarios)%nombre = trim(nombreEquipo)
                    inventarios(num_inventarios)%cantidad = cantidad
                    inventarios(num_inventarios)%precioUnitario = precioUnitario
                    inventarios(num_inventarios)%ubicacion = trim(ubicacion)

                    print *, 'Equipo creado: ' // trim(nombreEquipo) // ', Cantidad: ' // trim(adjustl(cantidadStr)) // ', Precio Unitario: ' // trim(adjustl(trim(precioUnitarioStr))) // ', Ubicacion: ' // trim(ubicacion)
                else
                    print *, 'No se encontro el tercer delimitador ";" en la línea'
                end if
            else
                print *, 'No se encontró el segundo delimitador ";" en la línea'
            end if
        else
            print *, 'No se encontró el delimitador ";" en la línea'
        end if
    end subroutine procesar_crear_equipo

    subroutine opcion1()
        character(len=1024) :: linea
        integer :: ios
        character(len=256) :: filename
        print *, '===================================================='
        print *, 'Has seleccionado la Opcion 1'
    
        do
            print *, '===================================================='
            print *, 'Por favor, ingrese la ruta del archivo de inventario:'
            read *, filename
            print *, '===================================================='
            print *, 'Intentando abrir el archivo:', trim(filename)
            print *, '===================================================='
            open(unit = 20, file = filename, status = 'old', action = 'read', iostat = ios)
            if (ios /= 0) then
                if (ios == 2) then
                    print *, 'El archivo no existe. Intente de nuevo.'
                else
                    print *, 'Error al abrir el archivo, código de error:', ios
                end if
            else
                print *, 'Archivo abierto exitosamente'
                print *, '----------------------------------------------------'
                exit
            end if
        end do
    
        do
            read(20, '(A)', iostat = ios) linea
            if (ios /= 0) then
                if (ios == -1) then
                    print *, '----------------------------------------------------'
                    print *, 'Fin del archivo alcanzado'
                    exit
                else
                    print *, 'Error al leer la línea, código de error:', ios
                    exit
                end if
            else
                call procesar_linea(trim(linea))
            end if
        end do
    
        close(20)
    end subroutine opcion1
    
    subroutine opcion2()
        implicit none
        integer :: ios
        character(len=256) :: filename
        character(len=1024) :: linea, comando
    
        print *, '===================================================='
        print *, 'Ingrese la ruta del archivo .mov:'
        read(*, '(A)') filename
        print *, '===================================================='
    
        ! Abrir el archivo especificado por el usuario
        open(20, file=trim(filename), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error al abrir el archivo, codigo de error:', ios
            return
        end if
        do
            read(20, '(A)', iostat = ios) linea
            if (ios /= 0) then
                if (ios == -1) then
                    print *, '----------------------------------------------------'
                    print *, 'Fin del archivo alcanzado'
                    exit
                else
                    print *, 'Error al leer la linea, código de error:', ios
                    exit
                end if
            else
                comando = trim(linea(1:index(linea, ' ') - 1))
                select case (comando)
                    case ('agregar_stock')
                        call procesar_agregar_stock(trim(linea(index(linea, ' ') + 1:)))
                    case ('eliminar_equipo')
                        call procesar_eliminar_equipo(trim(linea(index(linea, ' ') + 1:)))
                    case default
                        print *, 'Comando no reconocido: ', comando
                end select
            end if
        end do
    
        close(20)
    end subroutine opcion2

    subroutine opcion3(inventarios, num_equipos)
        implicit none
        type(inventario_t), intent(inout) :: inventarios(:)
        integer, intent(in) :: num_equipos
    
        ! Llamar a la subrutina para generar el informe del inventario
        call generar_informe_inventario(inventarios, num_equipos)
    
        print *, '===================================================='
        print *, 'Informe del inventario generado exitosamente.'
        print *, '===================================================='
    end subroutine opcion3

end module inventario_mod

program main
    use inventario_mod
    implicit none
    integer :: opcion

    do
        print *, '=====================PRACTICA 1====================='
        print *, '                 Sistema de inventario'
        print *, '===================================================='
        print *, '1. Cargar inventario incial'
        print *, '2. Cargar instrucciones de movimientos'
        print *, '3. Crear informe de inventario'
        print *, '4. Salir'
        print *, 'Selecciona una opcion: '
        read *, opcion

        select case (opcion)
            case (1)
               call opcion1()
            case (2)
                call opcion2()
            case (3)
                call opcion3(inventarios, num_inventarios)
            case (4)
                exit
            case default
                print *, 'Opcion no valida. Inténtalo de nuevo.'
        end select
    end do
end program main