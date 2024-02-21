module Fase_1

  implicit none
    ! contadores globales asignados aqui 
    integer :: global_img_id = 0 ! cada id_imagen sera unica 
    
    !integer, save :: estado_paso = 0
    !integer, save :: cliente_actual_id = 0
    !integer, save :: img_actual_tipo = 0



  !-------------  Cliente 
  type :: Client
      integer :: id
      character(len=50) :: name
      integer :: img_p
      integer :: img_g
  end type 

  type :: NodoClient
      type(Client) :: data
      type(NodoClient), pointer :: next => null()
  end type 

  !----- Cola Repecion
  type :: Tail 
      type(NodoClient), pointer :: head => null()
      type(NodoClient), pointer :: tail => null()
      contains
      procedure :: encolar
      procedure :: show
      procedure :: desencolar
  end type
  !------------- end Cliente

!------ Pila de imagenes
    ! Objeto imagen
    type :: Imagen
    integer :: id_img
    character(len = 1) :: type_img ! tipo imagen (P (pequeña) & G (Grande))
    integer :: id_client  ! id_client id que pertenece esa imagen
    end type 

    type :: NodoImagen
    type(Imagen) :: data_img
    type(NodoImagen), pointer :: next => null()
    end type

    type :: PilaImagenes
    type(NodoImagen), pointer :: top => null()
    integer :: contador = 0
    contains
    procedure :: push_img
    procedure :: show_img
    procedure :: clear_stack
    end type
!------ end Pila de imagenes

  !-------------- Ventanilla
  type :: Ventanilla
      integer :: id
      integer :: id_cliente_asociado = 0    
      type(Client), pointer :: assing_client => null()
      ! bandera de momento
      
      type(PilaImagenes) :: pila_imgs
  end type 

  type :: NodoVentanilla
      type(Ventanilla) :: dataV
      type(NodoVentanilla), pointer :: next => null()
  end type

  type :: VentanillaList
      type(NodoVentanilla), pointer :: head => null()
      contains 
      procedure :: add_ventanilla
      procedure :: show_ventanilla
      !Asignar un cliente a Ventanilla
      !procedure :: assing_client
  end type

  !------------end Ventanilla

    contains   
    !agregar
    subroutine encolar(this, cliente)
        class(Tail), intent(inout) :: this
        type(Client), intent(in) :: cliente
        type(NodoClient), pointer :: tempClient
        allocate(tempClient)
        tempClient%data = cliente
        tempClient%next => null()

        if(associated(this%head)) then
            this%tail%next => tempClient
            this%tail => tempClient
        else
            this%head => tempClient
            this%tail => tempClient
        end if
    end subroutine encolar
    ! eliminar o sacar        
    subroutine desencolar(this, clienteDesencolado)
        class(Tail), intent(inout) :: this
        type(Client), pointer, intent(out) :: clienteDesencolado !--- Agurdamos el cliente Desencolado
        type(NodoClient), pointer :: tempClient
        
        clienteDesencolado => null()
        
        if(associated(this%head)) then
            allocate(clienteDesencolado)
            tempClient => this%head
            
            clienteDesencolado%id = tempClient%data%id
            clienteDesencolado%name = tempClient%data%name
            clienteDesencolado%img_g = tempClient%data%img_g
            clienteDesencolado%img_p = tempClient%data%img_p


            this%head => this%head%next
            if(.not. associated(this%head))then
            this%tail => null()
            end if
            deallocate(tempClient)

        else
            print *, "La cola esta vacia"
        end if
    end subroutine desencolar

    subroutine show(this)
        class(Tail), intent(in) :: this
        type(NodoClient), pointer :: nodo_actual
        nodo_actual => this%head
        do
            if (.not. associated(nodo_actual)) then
                print *, "La cola esta vacia"
                exit
            end if 
            print *, nodo_actual%data%id
            print *, "  ", nodo_actual%data%name
            print *, nodo_actual%data%img_g
            print *, nodo_actual%data%img_p
            
            nodo_actual => nodo_actual%next
        end do
    end subroutine show  

    !-- Subrutinas Ventanillas (add, assing, show)
    subroutine add_ventanilla(this, id)
        class(VentanillaList), intent(inout) :: this
        integer, intent(in) :: id
        type(NodoVentanilla), pointer :: tempV => null()

        allocate(tempV)
        tempV%dataV%id = id
        tempV%dataV%assing_client => null()
        tempV%next => this%head
        this%head => tempV
    end subroutine add_ventanilla

    !-- agregar una imagen a la pila 
    subroutine push_img(this, image)
        class(PilaImagenes), intent(inout) :: this
        type(Imagen), intent(in) :: image
        type(NodoImagen), pointer :: newImg 

        allocate(newImg)
        newImg%data_img = image
        newImg%next => this%top
        this%top => newImg
        this%contador = this%contador + 1
    end subroutine push_img

    ! Limpiamos nuestra pila hasta que quede vacia
    subroutine clear_stack(this)
        class(PilaImagenes), intent(inout) :: this
        type(NodoImagen), pointer :: tempNodo

        do while(associated(this%top))
            tempNodo => this%top
            this%top => this%top%next
            deallocate(tempNodo)
        end do 
   
    end subroutine clear_stack

    subroutine asingar_cliente(this, colaClientes)

        type(VentanillaList), intent(inout) :: this
        type(NodoVentanilla), pointer :: tempVentanilla
        type(Tail), intent(inout) :: colaClientes
        type(Client), pointer :: cliente_to_ventanilla

        tempVentanilla => this%head
        do while(associated(tempVentanilla))
            if(tempVentanilla%dataV%id_cliente_asociado == 0) then
                call colaClientes%desencolar(cliente_to_ventanilla)
                if(associated(cliente_to_ventanilla)) then
                    tempVentanilla%dataV%assing_client => cliente_to_ventanilla
                    tempVentanilla%dataV%id_cliente_asociado = cliente_to_ventanilla%id
                    exit    
                else
                    print *, '---- No hay mas clientes en la cola ---'
                    exit 
                end if
            end if
            tempVentanilla => tempVentanilla%next !valla avanzando
        end do
    end subroutine asingar_cliente

    subroutine asignar_imagen(this, cliente)
        type(VentanillaList), intent(inout) :: this
        type(NodoVentanilla), pointer :: tempVentanilla
        type(Client), intent(inout) :: cliente
        type(Imagen) :: nueva_imagen
        logical :: imagen_asignada

        tempVentanilla => this%head
        do while(associated(tempVentanilla))
            if(tempVentanilla%dataV%id_cliente_asociado == cliente%id)then
                if(cliente%img_p > 0)then
                    cliente%img_p = cliente%img_p - 1
                    nueva_imagen%type_img = 'P'
                else if(cliente%img_g > 0)then
                    cliente%img_g = cliente%img_g - 1
                    nueva_imagen%type_img = 'G'
                else
                    imagen_asignada = .false.
                    return
                end if
                global_img_id = global_img_id + 1
                nueva_imagen%id_img = global_img_id
                nueva_imagen%id_client = cliente%id
                call tempVentanilla%dataV%pila_imgs%push_img(nueva_imagen)
                imagen_asignada = .true.
                return
            end if
            tempVentanilla => tempVentanilla%next
        end do
    end subroutine asignar_imagen   

    subroutine ejecutar(this, colaClientes)
        class(VentanillaList), intent(inout) :: this
        type(Tail), intent(inout) :: colaClientes
        type(NodoVentanilla), pointer :: tempVentanilla
        type(Client), pointer ::  cliente_to_ventanilla
        
        tempVentanilla => this%head
        do while(associated(tempVentanilla))
            if (associated(tempVentanilla%dataV%assing_client)) then
                cliente_to_ventanilla => tempVentanilla%dataV%assing_client
                call asignar_imagen(this, cliente_to_ventanilla)
            tempVentanilla => tempVentanilla%next
            else 
                exit 
            end if
        end do
        
        call asingar_cliente(this, colaClientes)

            
    end subroutine ejecutar
    
    !Montrar Ventanilla 
    subroutine show_ventanilla(this)
        class(VentanillaList), intent(in) :: this
        type(NodoVentanilla), pointer :: tempVent
        
        tempVent => this%head
        
        do while(associated(tempVent))
            print *, "Ventanilla ID:", tempVent%dataV%id
            print *, "----------------------------------"
            if (associated(tempVent%dataV%assing_client)) then
                print *, "ID_CLIENTE", tempVent%dataV%assing_client%id
                print *, "Nombre", "   ", tempVent%dataV%assing_client%name
                print *, "Img-p", tempVent%dataV%assing_client%img_p
                print *, "Img-g", tempVent%dataV%assing_client%img_g
                
                call show_img(tempVent%dataV%pila_imgs)

            else
                print *, "No esta asociado a una ventanilla"
            end if
            

            tempVent => tempVent%next
        end do
    end subroutine show_ventanilla

    subroutine show_img(this)
        class(PilaImagenes), intent(in) :: this
        type(NodoImagen), pointer :: currentNode
    
        currentNode => this%top  ! Comienza en el nodo superior
        print *, "Mostrando pila de imagenes:"
    
        ! Recorre la pila hasta que el puntero al siguiente nodo sea null
        do while(associated(currentNode))
            ! Imprime los detalles de la imagen en el nodo actual
            print *, "ID Imagen: ", currentNode%data_img%id_img
            print *, "Tipo Imagen: ", currentNode%data_img%type_img
            print *, "ID Cliente: ", currentNode%data_img%id_client
            print *, "---------------------------------"
            ! Mueve al siguiente nodo
            currentNode => currentNode%next
        end do
    
        if (.not. associated(this%top)) then
            print *, "La pila esta vacia."
        end if
    end subroutine show_img


    subroutine process_and_clear_ventanillas(this)
        class(VentanillaList), intent(inout) :: this
        type(NodoVentanilla), pointer :: currentVent
    
        currentVent => this%head
        do while(associated(currentVent))
            if(associated(currentVent%dataV%assing_client)) then
                ! Aquí puedes realizar cualquier procesamiento necesario con las imágenes
                ! Por simplicidad, este ejemplo solo limpia la pila y resetea el cliente
                call currentVent%dataV%pila_imgs%clear_stack()
                nullify(currentVent%dataV%assing_client)
            end if
            currentVent => currentVent%next
        end do
    end subroutine process_and_clear_ventanillas

    

end module Fase_1
