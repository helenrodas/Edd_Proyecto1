program startProgram
    implicit none
    

    logical :: exit_program

    exit_program = .false.
    do while (.not. exit_program)
        call initial_menu(exit_program)
    end do

end program startProgram

subroutine initial_menu(exit_program)
    implicit none
    logical, intent(out) :: exit_program
    integer :: option

    do
        call print_menu()
        read(*, *) option

        select case(option)
        case(1)
            call parametros_iniciales()
        ! case(2)
        !     call carga_masiva_clientes()
        ! case(3)
        !     call cantidad_ventanillas()
        ! case(4)
        !     call ejecutar_paso()
        ! case(5)
        !     call estado_memoria_estructuras()
        case(6)
            exit_program = .true.
            exit
        case default
            print *, "Error!. Por favor seleccione una opcion valida."
        end select
    end do

end subroutine initial_menu

subroutine print_menu()
    print *, "...................................."
    print *, "         Menu Principal             "
    print *, "...................................."
    print *, "1. Parametros iniciales"
    print *, "2. Ejecutar paso"
    print *, "3. Estado en memoria de las estructuras"
    print *, "4. Reportes"
    print *, "5. Acerca de "
    print *, "6. Salir"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
end subroutine print_menu

subroutine parametros_iniciales()
    integer :: option
    do
        call parameters_menu()
        read(*, *) option

        select case(option)
        case(1)
            call readFile()
        case(2)
            call windowNumber()
        case(3)
            exit
        case default
            print *, "Error!. Por favor seleccione una opcion valida."
        end select
    end do
end subroutine parametros_iniciales


subroutine parameters_menu()
    print *, "...................................."
    print *, "         Seleccione una opcion            "
    print *, "...................................."
    print *, "1. Carga masiva de clientes"
    print *, "2. Cantidad de ventanillas"
    print *, "3. Regresar a menu principal"
end subroutine parameters_menu



subroutine readFile()
    use cola_module
    use linkedList
    implicit none
    
    type(cola) :: mi_cola
    !type (linked_list) :: lista_gente
    integer :: id_asInt, img_p_asInt, img_g_asInt
    character(len=50) :: name_asString
    character(len=256) :: filename
    integer :: file_unit, status
    logical :: in_array, in_object
    character(len=50) :: id, nombre, img_p
    character(len=1000) :: line, field
    integer :: comma_position, colon_position

    print *, ">> Ingrese el nombre del archivo JSON:"
    read(*, '(A)') filename

    open(unit = file_unit, file = trim(filename) // '.json', action='READ')
    
    in_array = .false.
    in_object = .false.

    do
        read(file_unit, '(A)', iostat=status) line
        if (status /= 0) exit

        ! Analizar la línea para extraer los datos
        if (index(line, '[') > 0) then
            in_array = .true.
        elseif (index(line, ']') > 0) then
            in_array = .false.
        elseif (index(line, '{') > 0) then
            in_object = .true.
        elseif (index(line, '}') > 0) then
            in_object = .false.
            ! Agregar los datos del objeto a la cola
        
        elseif (in_array .and. in_object) then
            ! Encuentra la posición de la coma
            comma_position = index(line, ',')
            ! Encuentra la posición de los dos puntos (:) para obtener el valor después de él
            colon_position = index(line, ':')
            
            ! Si hay una coma al final de la línea
            if (comma_position > 0) then
                ! Lee el campo hasta la coma
                field = line(colon_position+1:comma_position-1)
            else
                ! Si no hay coma al final de la línea, lee el campo hasta el final de la línea
                field = line(colon_position+1:)
            endif
            
            ! Procesa el campo eliminando las comillas dobles
            ! field = mi_cola%removeQuotes(field)
            
            ! Asigna el valor del campo a la variable correspondiente
            if (index(line, '"id"') > 0) then
                read(field, *) id_asInt
            elseif (index(line, '"nombre"') > 0) then
                !read(field, *) name_asString
                nombre = field
                !print *, nombre
            elseif (index(line, '"img_g"') > 0) then
                read(field, *) img_g_asInt
            elseif (index(line, '"img_p"') > 0) then
                read(field, *) img_p_asInt
                ! Agregar los valores a la cola
                call mi_cola%push(id_asInt, trim(nombre), img_g_asInt, img_p_asInt)
                
            endif
        endif
    end do
    call mi_cola%print
    close(file_unit)
end subroutine readFile 



subroutine windowNumber()
    use linkedlist
        implicit none
        
        type(linked_list) :: windowsList
        integer :: windowsAmount, i
        character(len=1) :: dummy_char

        print *, ">> Ingrese el numero de ventanillas disponibles:"
        read(*, *) windowsAmount

        ! Inicializar la lista de ventanillas
        call init_linked_list(windowsList)

        ! Agregar nodos a la lista para representar las ventanillas
        do i = 1, windowsAmount
            call push(windowsList, i)
        end do

        ! Mostrar las ventanillas creadas
        print *, "Ventanillas creadas exitosamente:"
        call print(windowsList)

        ! Esperar a que el usuario presione cualquier tecla antes de volver al menú
        print *, "Presione cualquier tecla para volver al menu..."
        read(*,*) dummy_char
end subroutine windowNumber
