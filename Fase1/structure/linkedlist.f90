module linkedList
    implicit none

    type :: linked_list
    type(node), pointer :: head => null() ! head of the list

    contains
        procedure :: push
        procedure :: print
        procedure :: delete_by_position
    end type linked_list

    type :: node
    integer :: value
    type(node), pointer :: next
    end type node

    contains

    subroutine push(self, value)
        class(linked_list), intent(inout) :: self
        integer, intent(in) :: value
    
        type(node), pointer :: current, newNode
    
        ! Crear un nuevo nodo
        allocate(newNode)
        newNode%value = value
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
    
        print *, 'pushed:: ', value
    end subroutine push
    

    subroutine delete_by_position(self, position)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: position
    type(node), pointer :: current, previous
    integer :: counter

    current => self%head
    previous => null()

    if(position == 1) then
        self%head => current%next
        deallocate(current)
        return
    end if

    counter = 1
    do while (associated(current) .and. counter < position)
        previous => current
        current => current%next
        counter = counter + 1
    end do

    if (.not. associated(current)) then
        print *, 'No se encontro la posicion'
        return
    end if

    previous%next => current%next
    deallocate(current)
    end subroutine delete_by_position

    subroutine print(self)
        class(linked_list), intent(in) :: self
    
        type(node), pointer :: current
    
        current => self%head
    
        ! Recorre la lista y imprime los valores
        do while (associated(current))
            print *, current%value
            current => current%next
        end do
    end subroutine print

    subroutine init_linked_list(self)
        class(linked_list), intent(inout) :: self
        
        ! No es necesario inicializar la lista enlazada si ya está inicializada.
        if (associated(self%head)) then
            print *, "La lista ya está inicializada."
            return
        end if
        
        self%head => null()
    end subroutine init_linked_list

    

end module linkedList