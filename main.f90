program readFile
    implicit none
    character(len=256) :: filename
    integer :: file_unit, status
    logical :: in_array, in_object
    character(len=50) :: id, nombre, img_g, img_p
    character(len=1000) :: line

    print *, ">> Ingrese el nombre del archivo JSON:"
    read(*, '(A)') filename

    open(unit = file_unit, file = trim(filename) // '.json', action='READ')
    
    ! Variables para el análisis del JSON
    in_array = .false.
    in_object = .false.

    ! Leer el contenido línea por línea
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
            ! Mostrar los datos del objeto
            print *, "ID:", trim(id)
            print *, "Nombre:", trim(nombre)
            print *, "Imagen grande:", trim(img_g)
            print *, "Imagen pequeña:", trim(img_p)
        elseif (in_array .and. in_object) then
            ! Buscar y extraer los campos
            if (index(line, '"id"') > 0) then
                id = line(index(line, ':')+1:)
            elseif (index(line, '"nombre"') > 0) then
                nombre = line(index(line, ':')+1:)
            elseif (index(line, '"img_g"') > 0) then
                img_g = line(index(line, ':')+1:)
            elseif (index(line, '"img_p"') > 0) then
                img_p = line(index(line, ':')+1:)
            endif
        endif
    end do
    close(file_unit)
end program readFile