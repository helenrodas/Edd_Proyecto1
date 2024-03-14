module cola_module
    implicit none
    
    type :: cola
    type(node), pointer :: head => null() ! head of the list

    contains
        procedure :: push
        procedure :: print
        ! Agregamos los procedimientos restantes de la cola
    end type cola

    type :: node
        integer :: id
        character(len=:), allocatable :: nombre
        integer :: img_g
        integer :: img_p
        type(node), pointer :: next
    end type node

    contains

    subroutine push(self, id,nombre,img_g,img_p)
        class(cola), intent(inout) :: self
        integer, intent(in) :: id,img_g,img_p
        character(len=*), intent(in) :: nombre
        
    
        type(node), pointer :: current, newNode
    
        ! Crear un nuevo nodo
        allocate(newNode)
        newNode%id = id
        newNode%nombre = nombre
        newNode%img_g = img_g
        newNode%img_p = img_p
        newNode%next => null()
    
        ! Si la lista está vacía, el nuevo nodo se convierte en la cabeza de la lista
        if (.not. associated(self%head)) then
            self%head => newNode
        else
            ! Encontrar el último nodo de la lista
            current => self%head
            do while (associated(current%next))
                current => current%next
            end do
    
            ! Insertar el nuevo nodo al final de la lista
            current%next => newNode
        end if
    
        !print *, 'pushed:: ', id,nombre,img_g,img_p
    end subroutine push


    subroutine print(self)
        class(cola), intent(in) :: self
    
        type(node), pointer :: current
    
        current => self%head
    
        ! Recorre la lista y imprime los valores
        do while (associated(current))
            print *, "id: ", current%id
            print *, "nombre: ",current%nombre
            print *, "imagenes_grandes: ",current%img_g
            print *, "imagenes pequenas: ",current%img_p
            current => current%next
        end do
    end subroutine print
end module cola_module
