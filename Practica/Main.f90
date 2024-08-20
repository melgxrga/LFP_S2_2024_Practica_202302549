module inventario_mod
    implicit none
    integer, parameter :: max_inventarios = 100
    integer, parameter :: max_movimientos = 100
    integer, parameter :: max_inventario = 1000
    integer :: num_inventarios = 0
    integer :: num_movimientos = 0

    type :: inventario_t
        character(len=1024) :: nombre
        integer :: cantidad
        real :: precioUnitario
        character(len=1024) :: ubicacion
    end type inventario_t

    type :: movimiento_t
        character(len=1024) :: nombre
        integer :: cantidad
        character(len=1024) :: ubicacion
        character(len=1024) :: tipo
    end type movimiento_t

    type(inventario_t), allocatable :: inventarios(:)
    type(movimiento_t), allocatable :: movimientos(:)


contains

subroutine generar_informe(inventarios, num_equipos, movimientos, num_movimientos)
    implicit none
    type(inventario_t), intent(inout) :: inventarios(:)
    type(movimiento_t), intent(in) :: movimientos(:)
    integer, intent(in) :: num_equipos, num_movimientos
    integer :: ios, i, j
    character(len=256) :: filename_inventario, filename_movimientos
    real :: valor_total


    filename_movimientos = 'informe_movimientos.txt'

    ! Generar informe de inventario inicial
    open(30, file=trim(filename_movimientos), status='replace', action='write', iostat=ios)
    if (ios /= 0) then
        print *, 'Error al abrir el archivo de informe de inventario, codigo de error:', ios
        return
    end if

    write(30, '(A)') '==============================='
    write(30, '(A)') '      Informe de Movimientos      '
    write(30, '(A)') '==============================='
    write(30, '(A)') ''
    write(30, '(A)') 'Equipo           Cantidad   Precio Unitario   Valor Total   Ubicacion'
    write(30, '(A)') '---------------------------------------------------------------------'

    do i = 1, num_equipos
        valor_total = inventarios(i)%cantidad * inventarios(i)%precioUnitario
        write(30, '(A20, I10, 1X, A, F10.2, 1X, A, F10.2, A20)') trim(inventarios(i)%nombre), inventarios(i)%cantidad, 'Q', inventarios(i)%precioUnitario, 'Q', valor_total, trim(inventarios(i)%ubicacion)
    end do

    close(30)

    ! Aplicar movimientos al inventario
    do i = 1, num_movimientos
        do j = 1, num_equipos
            if (trim(movimientos(i)%nombre) == trim(inventarios(j)%nombre) .and. trim(movimientos(i)%ubicacion) == trim(inventarios(j)%ubicacion)) then
                if (trim(movimientos(i)%tipo) == 'Agregar') then
                    inventarios(j)%cantidad = inventarios(j)%cantidad + movimientos(i)%cantidad
                else if (trim(movimientos(i)%tipo) == 'Eliminar') then
                    inventarios(j)%cantidad = inventarios(j)%cantidad - movimientos(i)%cantidad
                end if
                exit
            end if
        end do
    end do
end subroutine generar_informe

subroutine procesar_eliminar_equipo(linea)
    implicit none
    character(len=*), intent(in) :: linea
    character(len=1024) :: nombreEquipo, cantidadStr, ubicacion
    integer :: cantidad, pos1, pos2, i
    logical :: encontrado, ubicacion_existe
    type(movimiento_t), allocatable :: temp_movimientos(:)

    pos1 = index(linea, ';')
    if (pos1 > 0) then
        nombreEquipo = trim(linea(1:pos1-1))
        pos2 = index(linea(pos1+1:), ';') + pos1
        if (pos2 > pos1) then
            cantidadStr = trim(linea(pos1+1:pos2-1))
            ubicacion = trim(linea(pos2+1:))
            read(cantidadStr, *) cantidad

            ubicacion_existe = .false.
            do i = 1, num_inventarios
                if (trim(inventarios(i)%ubicacion) == ubicacion) then
                    ubicacion_existe = .true.
                    exit
                end if
            end do

            if (.not. ubicacion_existe) then
                print *, '===================================================='
                print *, 'Error: La ubicacion especificada no existe en el inventario.'
                print *, 'Ubicación: ', trim(ubicacion)
                print *, '===================================================='
                return
            end if

            encontrado = .false.
            do i = 1, num_inventarios
                if (trim(inventarios(i)%nombre) == trim(nombreEquipo) .and. trim(inventarios(i)%ubicacion) == trim(ubicacion)) then
                    encontrado = .true.
                    if (inventarios(i)%cantidad >= cantidad) then
                        inventarios(i)%cantidad = inventarios(i)%cantidad - cantidad
                        print *, 'Stock eliminado - Nombre: ', trim(nombreEquipo), ', Cantidad: ', cantidad, ', Ubicacion: ', trim(ubicacion)
                        print *, 'Inventario actual de ', trim(nombreEquipo), ': ', inventarios(i)%cantidad
                    else
                        print *, '===================================================='
                        print *, 'Error: Cantidad insuficiente en la ubicacion especificada, tiene que ser una cantidad menor del inventario.'
                        print *, 'Equipo: ', trim(nombreEquipo), ', Ubicacion: ', trim(ubicacion)
                        print *, '===================================================='
                    end if
                    exit
                end if
            end do
            if (.not. encontrado) then
                print *, '===================================================='
                print *, 'Error: El equipo no existe en la ubicacion especificada.'
                print *, 'Equipo: ', trim(nombreEquipo), ', Ubicacion: ', trim(ubicacion)
                print *, '===================================================='
            end if

            ! Registrar el movimiento
            num_movimientos = num_movimientos + 1
            if (.not. allocated(movimientos)) then
                allocate(movimientos(num_movimientos))
            else
                allocate(temp_movimientos(num_movimientos))
                temp_movimientos(1:num_movimientos-1) = movimientos
                deallocate(movimientos)
                allocate(movimientos(num_movimientos))
                movimientos = temp_movimientos
                deallocate(temp_movimientos)
            end if
            movimientos(num_movimientos)%nombre = nombreEquipo
            movimientos(num_movimientos)%cantidad = cantidad
            movimientos(num_movimientos)%ubicacion = ubicacion
            movimientos(num_movimientos)%tipo = 'Eliminar'
        else
            print *, '===================================================='
            print *, 'Error: No se encontro el segundo delimitador ";" en la línea.'
            print *, 'Línea: ', trim(linea)
            print *, '===================================================='
        end if
    else
        print *, '===================================================='
        print *, 'Error: No se encontro el delimitador ";" en la línea.'
        print *, 'Línea: ', trim(linea)
        print *, '===================================================='
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
        implicit none
        character(len=*), intent(in) :: linea
        character(len=1024) :: nombreEquipo, cantidadStr, ubicacion
        integer :: cantidad, pos1, pos2, i
        logical :: encontrado, ubicacion_existe
        type(movimiento_t), allocatable :: temp_movimientos(:)
    
        pos1 = index(linea, ';')
        if (pos1 > 0) then
            nombreEquipo = trim(linea(1:pos1-1))
            pos2 = index(linea(pos1+1:), ';') + pos1
            if (pos2 > pos1) then
                cantidadStr = trim(linea(pos1+1:pos2-1))
                ubicacion = trim(linea(pos2+1:))
                read(cantidadStr, *) cantidad
    
                ubicacion_existe = .false.
                do i = 1, num_inventarios
                    if (trim(inventarios(i)%ubicacion) == ubicacion) then
                        ubicacion_existe = .true.
                        exit
                    end if
                end do
    
                if (.not. ubicacion_existe) then
                    print *, '===================================================='
                    print *, 'Error: La ubicacion especificada no existe en el inventario.'
                    print *, 'Ubicación: ' // trim(ubicacion)
                    print *, '===================================================='
                    return
                end if
    
                encontrado = .false.
                do i = 1, num_inventarios
                    if (trim(inventarios(i)%nombre) == trim(nombreEquipo) .and. trim(inventarios(i)%ubicacion) == trim(ubicacion)) then
                        if (inventarios(i)%cantidad + cantidad <= max_inventario) then
                            inventarios(i)%cantidad = inventarios(i)%cantidad + cantidad
                            encontrado = .true.
                            print *, 'Stock agregado - Nombre: ' // trim(nombreEquipo) // ', Cantidad: ', cantidad, ', Ubicacion: ' // trim(ubicacion)
                            print *, 'Inventario actual de ' // trim(nombreEquipo) // ': ', inventarios(i)%cantidad
                        else
                            print *, '===================================================='
                            print *, 'Error: No se puede agregar al inventario. Excede el maximo permitido.'
                            print *, '===================================================='
                        end if
                        exit
                    end if
                end do
                if (.not. encontrado) then
                    print *, '===================================================='
                    print *, 'Error: El equipo no existe en el inventario.'
                    print *, '===================================================='
                end if
    
                ! Registrar el movimiento
                num_movimientos = num_movimientos + 1
                if (.not. allocated(movimientos)) then
                    allocate(movimientos(num_movimientos))
                else
                    allocate(temp_movimientos(num_movimientos))
                    temp_movimientos(1:num_movimientos-1) = movimientos
                    deallocate(movimientos)
                    allocate(movimientos(num_movimientos))
                    movimientos = temp_movimientos
                    deallocate(temp_movimientos)
                end if
                movimientos(num_movimientos)%nombre = nombreEquipo
                movimientos(num_movimientos)%cantidad = cantidad
                movimientos(num_movimientos)%ubicacion = ubicacion
                movimientos(num_movimientos)%tipo = 'Agregar'
            else
                print *, '===================================================='
                print *, 'Error: No se encontro el segundo delimitador ";" en la línea.'
                print *, 'Línea: ' // trim(linea)
                print *, '===================================================='
            end if
        else
            print *, '===================================================='
            print *, 'Error: No se encontro el delimitador ";" en la línea.'
            print *, 'Línea: ' // trim(linea)
            print *, '===================================================='
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

    subroutine opcion3(inventarios, num_equipos, movimientos, num_movimientos)
        implicit none
        type(inventario_t), intent(inout) :: inventarios(:)
        type(movimiento_t), intent(inout) :: movimientos(:)
        integer, intent(in) :: num_equipos, num_movimientos

        ! Llamar a la subrutina para generar el informe del inventario y movimientos
        call generar_informe(inventarios, num_equipos, movimientos, num_movimientos)

        print *, '===================================================='
        print *, 'Informe del inventario y movimientos generado exitosamente.'
        print *, '===================================================='
    end subroutine opcion3

end module inventario_mod
subroutine procesar_crear_equipo(linea, comando)
    use inventario_mod
    implicit none
    character(len=*), intent(in) :: linea
    character(len=*), intent(in) :: comando
    character(len=1024) :: nombreEquipo, cantidadStr, precioUnitarioStr, ubicacion
    integer :: cantidad, pos1, pos2, pos3
    real :: precioUnitario
    type(inventario_t), allocatable :: temp_inventarios(:)
    integer :: ios, i
    character(len=256) :: filename_inventario
    real :: valor_total

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

                if (.not. allocated(inventarios)) then
                    allocate(inventarios(1))
                    num_inventarios = 1
                else
                    ! Redimensionar el arreglo para agregar un nuevo inventario
                    num_inventarios = num_inventarios + 1
                    allocate(temp_inventarios(num_inventarios))
                    temp_inventarios(1:num_inventarios-1) = inventarios
                    deallocate(inventarios)
                    allocate(inventarios(num_inventarios))
                    inventarios = temp_inventarios
                    deallocate(temp_inventarios)
                end if

                inventarios(num_inventarios)%nombre = nombreEquipo
                inventarios(num_inventarios)%cantidad = cantidad
                inventarios(num_inventarios)%precioUnitario = precioUnitario
                inventarios(num_inventarios)%ubicacion = ubicacion

                print *, 'Equipo creado - Nombre: ' // trim(nombreEquipo) // ', Cantidad: ', cantidad, ', Precio Unitario: ', precioUnitario, ', Ubicacion: ' // trim(ubicacion)

                ! Generar informe de inventario inicial
                filename_inventario = 'informe_inventario.txt'
                open(30, file=trim(filename_inventario), status='replace', action='write', iostat=ios)
                if (ios /= 0) then
                    print *, 'Error al abrir el archivo de informe de inventario, codigo de error:', ios
                    return
                end if

                write(30, '(A)') '==============================='
                write(30, '(A)') '      Informe de Inventario      '
                write(30, '(A)') '==============================='
                write(30, '(A)') ''
                write(30, '(A)') 'Equipo           Cantidad   Precio Unitario   Valor Total   Ubicacion'
                write(30, '(A)') '---------------------------------------------------------------------'

                do i = 1, num_inventarios
                    valor_total = inventarios(i)%cantidad * inventarios(i)%precioUnitario
                    write(30, '(A20, I10, 1X, A, F10.2, 1X, A, F10.2, A20)') trim(inventarios(i)%nombre), inventarios(i)%cantidad, 'Q', inventarios(i)%precioUnitario, 'Q', valor_total, trim(inventarios(i)%ubicacion)
                end do

                close(30)
            else
                print *, 'No se encontró el tercer delimitador ";" en la línea'
            end if
        else
            print *, 'No se encontró el segundo delimitador ";" en la línea'
        end if
    else
        print *, 'No se encontró el delimitador ";" en la línea'
    end if
end subroutine procesar_crear_equipo
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
                call opcion3(inventarios, num_inventarios, movimientos, num_movimientos)
                exit
            case default
                print *, 'Opcion no valida. Intentalo de nuevo.'
        end select
    end do
end program main