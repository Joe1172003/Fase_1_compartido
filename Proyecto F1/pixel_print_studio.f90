module Fase_1

    implicit none
      ! contadores globales asignados aqui 
      integer :: global_img_id = 0 ! cada id_imagen sera unica 
      integer :: contador_ejecutar = 0 ! control por cada llamada de mi funcion ejecutar
  
    !-------------  Cliente 
    type :: Client
        integer :: id
        character(len=50) :: name
        integer :: img_p
        integer :: img_g
        integer :: copy_img_p , copy_img_g !copia de imagenes pequeñas y grandes
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
  
  
    !--- Cola de Impresion Pequeña
    type :: ImpresoraP
      integer :: id_cliente
      character(len = 1) :: tipo_imagen
    end type
  
    type :: NodoImpresionP
      type(ImpresoraP) :: impresoraP_data
      type(NodoImpresionP), pointer :: next => null()
    end type 
  
    type :: Cola_Impresion_P
      type(NodoImpresionP), pointer :: head => null()
      type(NodoImpresionP), pointer :: tail => null()
      contains 
      procedure :: encolar_imp_p
      procedure :: show_imp_p
   end type 
  
  
   !--- Cola de Impresion Pequeña
  type :: ImpresoraG
      integer :: id_cliente
      character(len = 1) :: tipo_imagen
  end type
  
  type :: NodoImpresionG
      type(ImpresoraG) :: impresoraG_data
      type(NodoImpresionG), pointer :: next => null()
  end type 
  
  type :: Cola_Impresion_G
      type(NodoImpresionG), pointer :: head => null()
      type(NodoImpresionG), pointer :: tail => null()
      contains 
      procedure :: encolar_imp_g
      procedure :: show_imp_g
  end type 
  
  
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
  
    ! ---------- Lista de Listas  Clientes Espera AQUI COMIENZA ESTRUCTURA
      
      !----- Objeto sublista Imagen asociada al Cliente Espera
      type ::  img_asociada
          integer :: id_cliente_a
          character (len = 1) :: tipo_img_a
      end type
  
      ! sub Nodo para img_asociada
      type :: sub_nodo 
          type(img_asociada) :: img_data_a ! contiene los datos de el objeto img_asociada
          type(sub_nodo), pointer ::  next => null()
      end type
  
      type :: cliente_espera
          integer :: id_cliente_e
          integer :: img_g_e
          integer :: img_p_e
      end type
  
      ! Nodo cliente en espera
      type :: nodo_cliente_e
          type(cliente_espera) :: cliente_data_e
          type(nodo_cliente_e), pointer :: next => null() 
          type(nodo_cliente_e), pointer :: prev => null()
          type(sub_nodo), pointer :: list_img_a => null()
      end type
  
      ! Lista de Lista cliente espera
      type :: lista_lista_cliente_espera
          type(nodo_cliente_e), pointer :: head => null()
          type(nodo_cliente_e), pointer :: tail => null()
      contains 
          procedure :: add_cliente_espera
          procedure :: show_cliente_espera
          procedure :: add_img_a_cliente
      end type
      
  
    ! ---------- end Lista de Listas Clientes de espera AQUI FINALIZA ESTRUCTURA
  
      contains   
      !agregar a cola reception
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
  
      ! eliminar o sacar cola reception        
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
              clienteDesencolado%copy_img_g = tempClient%data%copy_img_g
              clienteDesencolado%copy_img_p = tempClient%data%copy_img_p
  
              this%head => this%head%next
              if(.not. associated(this%head))then
              this%tail => null()
              end if
              deallocate(tempClient)
  
          else
              print *, "La cola esta vacia"
          end if
      end subroutine desencolar
  
      ! Mostrar Clientes en cola recepcion
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
      
        !Grafica cola recepcion
        subroutine grafica_cola_recepcion(this, filename)
            type(Tail), intent(inout) :: this
            character(len = *), intent(in) :: filename
            
            integer :: unit, count
            type(NodoClient), pointer :: actual
            character(len =200) :: nodeLabel
            character(len=32) :: idStr, imgPStr, imgGStr, tmpStr   ! Asumiendo que los números caben en 32 caracteres

            count = 0
        
            open(unit, file=filename, status= 'replace')
            write(unit, *) 'digraph Cola {'
            write(unit, *) 'rankdir=LR;' !Ordenamos los nodose de izquierda a derecha
            write(unit, *) 'node [shape=box, style=filled, color=blue, fillcolor=pink]'
            ! Escribir nodos y conexiones
            actual => this%head
            do while(associated(actual))
                ! Convertir el entero 'count' y otros enteros a cadena
                ! Convertir cada entero a cadena
                write(imgPStr, '(i0)') actual%data%img_p
                write(imgGStr, '(i0)') actual%data%img_g
                write(tmpStr, '(i0)') count
                ! Formar la etiqueta del nodo
                nodeLabel = '"node' // trim(tmpStr) //'"'// ' [label="Name: ' // trim(actual%data%name) // &
                            '\n '//'img_p: ' // trim(imgPStr) // &
                            '\n'//'img_g: ' // trim(imgGStr) // '"]'

                write(unit, *) nodeLabel
                
                ! conexion al siguiente nodo
                if(associated(actual%next)) then
                    write(unit, "('node',i0,' -> node',i0)") count, count+1
                end if
                actual => actual%next
                count = count + 1
            end do
            write(unit, *) '}'
            close(unit)

            ! Generar el archivo PNG utilizando Graphviz
            call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
            print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'

        end subroutine grafica_cola_recepcion

      ! agregar a cola impresion grande
      subroutine encolar_imp_g(this, data_impresora_g)
          class(Cola_Impresion_G), intent(inout) :: this
          type(ImpresoraG), intent(in) :: data_impresora_g
          type(NodoImpresionG), pointer :: temp_print_g
  
          allocate(temp_print_g)
          temp_print_g%impresoraG_data = data_impresora_g
          temp_print_g%next => null()
  
          if(associated(this%head))then
              this%tail%next => temp_print_g
              this%tail => temp_print_g
          else 
              this%head => temp_print_g
              this%tail => temp_print_g
          end if
      end subroutine encolar_imp_g
  
      ! agregar a cola impresion pequeña
      subroutine encolar_imp_p(this, data_impresora_p)
          class(Cola_Impresion_P), intent(inout) :: this
          type(ImpresoraP), intent(in) :: data_impresora_p
          type(NodoImpresionP), pointer :: temp_print_p
  
          allocate(temp_print_p)
          temp_print_p%impresoraP_data = data_impresora_p
          temp_print_p%next => null()
  
          if(associated(this%head))then
              this%tail%next => temp_print_p
              this%tail => temp_print_p
          else 
              this%head => temp_print_p
              this%tail => temp_print_p
          end if
      end subroutine encolar_imp_p
  
      ! mostrar cola de impresion pequeña 
      subroutine show_imp_p(this)
          class(Cola_Impresion_P), intent(inout) :: this
          type(NodoImpresionP), pointer :: temp_imp_p
          temp_imp_p => this%head
          do 
              if (.not. associated(temp_imp_p))then
                  print *, "La cola de impresora pequena esta vacia"
                  exit
              end if
              print *, "------------- COLA DE IMPRESION P ----------"
              print *, "id_cliente ",temp_imp_p%impresoraP_data%id_cliente
              print *, "tipo_imagen           " ,temp_imp_p%impresoraP_data%tipo_imagen
              print *, "--------------------------------------------"
  
              temp_imp_p => temp_imp_p%next
          end do
      end subroutine show_imp_p
  
       ! mostrar cola de impresion grande
      subroutine show_imp_g(this)
          class(Cola_Impresion_G), intent(inout) :: this
          type(NodoImpresionG), pointer :: temp_imp_g
          temp_imp_g => this%head
          do 
              if (.not. associated(temp_imp_g))then
                  print *, "La cola de impresora pequena esta vacia"
                  exit
              end if
              print *, "------------- COLA DE IMPRESION G ----------"
              print *, "id_cliente ", temp_imp_g%impresoraG_data%id_cliente
              print *, "tipo_imagen           ", temp_imp_g%impresoraG_data%tipo_imagen
              print *, "--------------------------------------------"
  
              temp_imp_g => temp_imp_g%next
          end do
      end subroutine show_imp_g
      
      
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
  
      ! -- Limpiamos nuestra pila hasta que quede vacia
      subroutine clear_stack(this)
          class(PilaImagenes), intent(inout) :: this
          type(NodoImagen), pointer :: tempNodo
  
          do while(associated(this%top))
              tempNodo => this%top
              this%top => this%top%next
              deallocate(tempNodo)
          end do 
     
      end subroutine clear_stack
  
      ! Asingar Cliente a ventanilla
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
  
      !graficar lista ventanilla y pila de clientes
      subroutine graficar_ventanillas(this, filename)
        type(VentanillaList), intent(in) :: this
        type(NodoVentanilla), pointer :: tempV
        type(NodoImagen), pointer :: tempI
        
        integer :: unit
        character(len=200) :: idStr, clientIdStr, imgIdStr, nodeLabel, edgeLabel
        character(len=*), intent(in) :: filename
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, *) 'digraph VentanillaList {'
        write(unit, *) 'rankdir=TB;'
        write(unit, *) 'node [shape=record, style=filled, fillcolor=lightblue]'
        
        tempV => this%head
    
        ! Recorrer los nodos de la lista de ventanillas
        do while(associated(tempV)) 
            ! Crear un nodo para cada ventanilla
            write(idStr, '(i0)') tempV%dataV%id
            nodeLabel = 'Ventanilla' // trim(idStr) // ' [label="Ventanilla ID: ' // trim(idStr) // '"]'
            write(unit, *) nodeLabel

            tempI => tempV%dataV%pila_imgs%top
            if(associated(tempI)) then
                write(unit, *) 'subgraph cluster', trim(idStr),' {'
                write(unit, *) 'rankdir=TB;'
                write(unit, *) 'label= "Ventanilla ' // trim(idStr) //'"'
            end if

            do while(associated(tempI))
                ! Convertir enteros a cadena img_id, cliente_id
                write(imgIdStr, '(i0)') tempI%data_img%id_img
                write(clientIdStr, '(i0)') tempI%data_img%id_client
                    
                ! Formar la etiqueta del nodo imagen
                nodeLabel = 'node' // trim(imgIdStr) // ' [label="Img : ' // &
                            trim(imgIdStr) // '\n id cliente: ' // trim(clientIdStr) // &
                            '\nTipo: ' // trim(tempI%data_img%type_img) // '"]'
                write(unit, *) nodeLabel
                
                ! Conectar las imágenes en la pila si hay más de una
                if(associated(tempI%next)) then
                    write(edgeLabel, '(a, i0, a, i0)') 'node', tempI%data_img%id_img, ' -> node', tempI%next%data_img%id_img
                    write(unit, *) edgeLabel
                end if

                tempI => tempI%next
            end do
            
            if(associated(tempV%dataV%pila_imgs%top)) then
                write(unit, *) '}'
            end if    
                    
            ! Conectar la ventanilla actual con la siguiente, si existe
            if(associated(tempV%next)) then
                write(edgeLabel, '(a, i0, a, i0)') 'Ventanilla', tempV%dataV%id, ' -> Ventanilla', tempV%next%dataV%id
                write(unit, *) edgeLabel
            end if
    
            tempV => tempV%next
        end do
    
        write(unit, *) '}'
        close(unit)

         ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
        
    end subroutine graficar_ventanillas
    
      ! Asignar Imagen a la pila 
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
                      print *, "ESTAS AQUI :l"
                      exit  ! A mira ya no tenemos mas imagenes para asiganar asi que salite  !AQUI ESTABA COMO return
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
  
  
      !Añadir cliente es lista doblemente circula
      subroutine add_cliente_espera(this, cliente_e)
          class(lista_lista_cliente_espera), intent(inout) :: this
          type(cliente_espera), intent(in) :: cliente_e
          type(nodo_cliente_e), pointer :: nuevo
  
          allocate(nuevo)
          nuevo%cliente_data_e = cliente_e
  
          nuevo%next => null()
          nuevo%prev => null()
          
          if(.not. associated(this%head)) then
              this%head => nuevo
              this%tail => nuevo
              nuevo%next => nuevo
              nuevo%prev => nuevo
          else
              nuevo%next => this%head
              nuevo%prev => this%tail ! nuevo nodo apunte al anterior de la lista
              this%head%prev => nuevo  ! el primer nodo de la lista apunta hacia atras
              this%tail%next => nuevo ! aqui el nodo que se inserto al princio apunte al nuevo nodo que se ingreso
              this%tail => nuevo
  
          end if
      end subroutine add_cliente_espera
      
      !Asociar una imagen a un cliente
      subroutine add_img_a_cliente(this, id_cliente_asociado)
          class(lista_lista_cliente_espera), intent(inout) :: this
          integer, intent(in) :: id_cliente_asociado
  
          type(nodo_cliente_e), pointer :: aux
          type(sub_nodo), pointer :: nuevo, last_img
  
          aux => this%head
          ! Buscamos el cliente con el id_cliente_e  que sea igual al id cliente imagen
          do while(associated(aux))
              if(aux%cliente_data_e%id_cliente_e == id_cliente_asociado)then
                  allocate(nuevo)
                  nuevo%img_data_a%id_cliente_a = id_cliente_asociado
                  nuevo%next  => null()
  
                  !Anadimos la imagen a la lista de imagenes del cliente asociado por el id
                  if(.not. associated(aux%list_img_a))then
                      aux%list_img_a => nuevo
                  else
                      last_img => aux%list_img_a
                      do while(associated(last_img%next))
                          last_img => last_img%next
                      end do
                      last_img%next => nuevo
                  end if
                  return 
              end if
              aux => aux%next
          end do
      end subroutine add_img_a_cliente
  
      !Monstrar imagen de cliente en espera 
      subroutine show_cliente_espera(self)
          class(lista_lista_cliente_espera), intent(in) :: self
          type(nodo_cliente_e), pointer :: nodo_cliente_actual
          type(sub_nodo), pointer :: nodo_img_actual
  
          if (.not. associated(self%head))then
              print *, "-------------------"
              print *, "La lista esta vacia."
              print *, "-------------------"
  
              return
          end if
          nodo_cliente_actual => self%head
  
          do while(associated(nodo_cliente_actual))
              ! Mostrar información del cliente
              print *, "-----------------------------------"
              print *, "id cliente:", nodo_cliente_actual%cliente_data_e%id_cliente_e
              print *, "Imagenes grandes:", nodo_cliente_actual%cliente_data_e%img_g_e
              print *, "Imagenes pequenas:", nodo_cliente_actual%cliente_data_e%img_p_e
              ! Mostrar imagenes asociadas al cliente
              if (associated(nodo_cliente_actual%list_img_a)) then
                  print *, "Imagenes asociadas:"
                  nodo_img_actual => nodo_cliente_actual%list_img_a
                  do while(associated(nodo_img_actual))
                      print *, " - ID Imagen:", nodo_img_actual%img_data_a%id_cliente_a
                      print *,  "Tipo:", nodo_img_actual%img_data_a%tipo_img_a 
                      nodo_img_actual => nodo_img_actual%next
                  end do
              else
                  print *, "No hay imagenes asociadas."
  
              end if
              print *, "-----------------------------------"
  
              if(associated(nodo_cliente_actual%next, self%head)) exit ! salir al completar un ciclo (lista circular)
              ! Mover al siguiente cliente en la lista
              nodo_cliente_actual => nodo_cliente_actual%next
          end do
      end subroutine
  
      !Analizar ventanilla
      subroutine analizar_ventanilla(this, listaClientesEspera, colaImpresionP, colaImpresionG)
          type(VentanillaList), intent(inout) :: this
          type(NodoVentanilla), pointer :: tempVentanilla
  
          type(lista_lista_cliente_espera), intent(inout) :: listaClientesEspera
          type(cliente_espera) :: clienteTemp
  
          type(Cola_Impresion_P), intent(inout) :: colaImpresionP
          type(Cola_Impresion_G), intent(inout) :: colaImpresionG
  
          type(ImpresoraP) :: data_impresora_p
          type(ImpresoraG) :: data_impresora_g
  
          type(NodoImagen), pointer :: tempNodoImagen ! temporal nodo imagen
  
          tempVentanilla => this%head
          do while(associated(tempVentanilla))
              if(associated(tempVentanilla%dataV%assing_client))then
                  if((tempVentanilla%dataV%assing_client%img_p == 0 ) .and. (tempVentanilla%dataV%assing_client%img_g == 0)) then
                      
                      tempNodoImagen => tempVentanilla%dataV%pila_imgs%top ! revisar si hay imagenes paara procesar
                      do while(associated(tempNodoImagen)) 
                          select case (tempNodoImagen%data_img%type_img) 
                          Case ('P')
                              data_impresora_p%id_cliente = tempNodoImagen%data_img%id_client
                              data_impresora_p%tipo_imagen = tempNodoImagen%data_img%type_img
                              call colaImpresionP%encolar_imp_p(data_impresora_p)
                          Case ('G')
                              data_impresora_g%id_cliente = tempNodoImagen%data_img%id_client
                              data_impresora_g%tipo_imagen = tempNodoImagen%data_img%type_img
                              call colaImpresionG%encolar_imp_g(data_impresora_g)
                          end select
                          tempNodoImagen => tempNodoImagen%next
                      end do
  
                      ! creamos nuestra lista de clientes de espeta 
                      clienteTemp%id_cliente_e = tempVentanilla%dataV%assing_client%id
                      clienteTemp%img_g_e = tempVentanilla%dataV%assing_client%copy_img_g
                      clienteTemp%img_p_e = tempVentanilla%dataV%assing_client%copy_img_p
  
                      call listaClientesEspera%add_cliente_espera(clienteTemp)
                      
                      !Limpiar la pila de imágenes de la ventanilla actual 
                      call tempVentanilla%dataV%pila_imgs%clear_stack()
  
                      ! Limpiar mi pila de imagenes AQUI
                      tempVentanilla%dataV%id_cliente_asociado = 0
                      nullify(tempVentanilla%dataV%assing_client)
                      
                  end if
              end if
              tempVentanilla => tempVentanilla%next
          end do  
      end subroutine analizar_ventanilla
  
      ! Desencolamos uma imagen de nuestra cola de impresion P
      subroutine desencolar_imp_p(this, data_impresora_p)
          type(Cola_Impresion_P), intent(inout) :: this
          type(ImpresoraP), intent(out) :: data_impresora_p
          type(NodoImpresionP), pointer :: nodo_desencoladoP
  
          !Verificamos si nuestra cola Impresion P  esta vacia
          if(.not. associated(this%head)) then
              data_impresora_p%id_cliente = -1
              data_impresora_p%tipo_imagen = " "
              return
          end if
          
          nodo_desencoladoP => this%head
          data_impresora_p%id_cliente = nodo_desencoladoP%impresoraP_data%id_cliente
          data_impresora_p%tipo_imagen = nodo_desencoladoP%impresoraP_data%tipo_imagen
  
          this%head => this%head%next
          if (.not. associated(this%head)) then
              this%tail => null()
          end if
  
          !liberar el nodo desencolado
          deallocate(nodo_desencoladoP)
      end subroutine desencolar_imp_p
  
      ! Desencolamos una imagen de nuesta cola de impresion G
      subroutine desencolar_imp_g(this, data_impresora_g)
          type(Cola_Impresion_G), intent(inout) :: this
          type(ImpresoraG), intent(out) :: data_impresora_g
          type(NodoImpresionG), pointer :: nodo_desencoladoG
  
          !Verficamos si nuestra cola Impresion G esta vacia
          if (.not. associated(this%head))then
              data_impresora_g%id_cliente = -1
              data_impresora_g%tipo_imagen = " "
              return
          end if 
          nodo_desencoladoG => this%head
          data_impresora_g%id_cliente = nodo_desencoladoG%impresoraG_data%id_cliente
          data_impresora_g%tipo_imagen = nodo_desencoladoG%impresoraG_data%tipo_imagen
  
          this%head => this%head%next
          if(.not. associated(this%head)) then
              this%tail => null()
          end if
          !Liberar el nodo desencolado
          deallocate(nodo_desencoladoG)
      end subroutine desencolar_imp_g
  
      !Asignar imagen a cliente espera
      subroutine asignar_imagen_cliente(this, id_cliente_asociado, tipo_imagen)
          class(lista_lista_cliente_espera), intent(inout) :: this
          type(nodo_cliente_e), pointer :: aux ! Primer Nodo
          type(sub_nodo), pointer :: nuevo ! Segundo Nodo
          type(sub_nodo), pointer :: last ! buscar el ultimo nodo para insertar la nueva imagen
  
          integer, intent(in) :: id_cliente_asociado
          character (len = 1) , intent(in):: tipo_imagen
  
          aux => this%head
          do while(associated(aux))
              ! si Lista esperta id_cliente == id_cliente asociado => encontramos el cliente
              if(aux%cliente_data_e%id_cliente_e == id_cliente_asociado)then
                  print *, "esta entrando aqui *"
                  ! ahora asignemos memoria
                  allocate(nuevo)
                  nuevo%img_data_a%id_cliente_a = id_cliente_asociado
                  nuevo%img_data_a%tipo_img_a = tipo_imagen
  
                  nuevo%next => null() !este nodo es, por ahora, el último nodo de la lista
  
                  if(.not. associated(aux%list_img_a)) then
                      ! si eres el primero es decir no hay imagenes anteriormente incertadas a mi sub nodo
                      aux%list_img_a => nuevo
                  else
                      ! si ya hay imagenes incertadas a mi sub nodo
                      last => aux%list_img_a
                      do while(associated(last%next))
                          last => last%next
                      end do
                      last%next => nuevo
                  end if
                  ! Aqui al momento de que me asignes una imagen 
                  return 
              end if
              aux => aux%next
          end do
      end subroutine asignar_imagen_cliente
  
      !Añadir imagen asociada cliente espera (desencolar - asingar imagen a cliente espera)
      subroutine add_img_a_cliente_espera(this, print_tail_p, print_tail_g)
          class(lista_lista_cliente_espera), intent(inout) :: this
  
          class(Cola_Impresion_P), intent(inout) :: print_tail_p
          type(ImpresoraP) :: data_impresora_P
  
          class(Cola_Impresion_G), intent(inout) :: print_tail_g
          type(ImpresoraG) :: data_impresora_G
  
          contador_ejecutar = contador_ejecutar + 1
          print *, 'CONTADOR' , contador_ejecutar
  
          ! Desenclar imagen de impresora pequeña
          call desencolar_imp_p(print_tail_p, data_impresora_P)
          if(data_impresora_P%id_cliente /= -1)then
              call asignar_imagen_cliente(this, data_impresora_P%id_cliente, 'P')
          end if
  
          ! Desenclar imagen de impresora grande
          if (contador_ejecutar >= 2)then
              call desencolar_imp_g(print_tail_g, data_impresora_G)
              if(data_impresora_G%id_cliente /= -1)then
                  call asignar_imagen_cliente(this, data_impresora_G%id_cliente, 'G')
                  contador_ejecutar = 0
              end if  
          end if
      end subroutine add_img_a_cliente_espera
  
      ! Ejecutar aplicacion
      subroutine ejecutar(this, colaClientes, listaClientesEspera, colaImpresionP, colaImpresionG)
          class(VentanillaList), intent(inout) :: this
          type(Tail), intent(inout) :: colaClientes
          type(NodoVentanilla), pointer :: tempVentanilla
          type(Client), pointer ::  cliente_to_ventanilla
          
          !lista de clientes en espera
          type(lista_lista_cliente_espera), intent(inout) :: listaClientesEspera
  
          !cola de impresion imagenes grandes
          type(Cola_Impresion_P), intent(inout) :: colaImpresionP
  
          !cola de impresion imagenes grandes
          type(Cola_Impresion_G), intent(inout) :: colaImpresionG
  
          ! Agregar imagenes asociadas a clientes en espera
          if(associated(colaImpresionP%head) .or. associated(colaImpresionG%head))then
              call add_img_a_cliente_espera(listaClientesEspera, colaImpresionP, colaImpresionG)  
          else 
              !No hay nodos en colas de impresion pequeña
              print *, "colas de impresion estan vacias."
          end if
  
          ! Añadir Cliente de esperta y imagenes a las colas y limpiar ventanilla
          call analizar_ventanilla(this, listaClientesEspera, colaImpresionP, colaImpresionG)
          
          !Agregar imagenes a la pila de cada ventanilla
          tempVentanilla => this%head
          do while(associated(tempVentanilla))
              if (associated(tempVentanilla%dataV%assing_client)) then  ! Si esta asociado un cliente
                  cliente_to_ventanilla => tempVentanilla%dataV%assing_client
                  call asignar_imagen(this, cliente_to_ventanilla)
              end if
              tempVentanilla => tempVentanilla%next
          end do
          
          !Asingar clientes en ventanillas Disponibles
          call asingar_cliente(this, colaClientes)
  
      end subroutine ejecutar
      

      !Montrar Ventanilla 
      subroutine show_ventanilla(this)
          class(VentanillaList), intent(in) :: this
          type(NodoVentanilla), pointer :: tempVent
          
          tempVent => this%head
          
          do while(associated(tempVent))
              print *, ""
              print *, "----------------------------------"
              print *, "Ventanilla :", tempVent%dataV%id
              print *, "----------------------------------"
              if (associated(tempVent%dataV%assing_client)) then
                  print *, "Id_cliente", tempVent%dataV%assing_client%id
                  print *, "Nombre", "    ", tempVent%dataV%assing_client%name
                  print *, "Img-p", tempVent%dataV%assing_client%img_p
                  print *, "Img-g", tempVent%dataV%assing_client%img_g
                  print *, "copia imagen P", tempVent%dataV%assing_client%copy_img_p
                  print *, "copia imagen G", tempVent%dataV%assing_client%copy_img_g
  
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
      
  
  end module Fase_1
  


program pixel_print_studio
    use json_module
    use Fase_1
    implicit none
    
    !Json
    type(json_file) :: json   ! Se declara una variable del tipo json_file
    type(json_value), pointer :: p_list, p_cliente, p_atributos  ! Se declaran punteros a variables del tipo json_value
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    
    !Cliente
    type(Tail) :: reception_tail
    type(Client) :: cliente
    
    !Ventanilla
    type(VentanillaList) :: listVentanillas
    type(PilaImagenes) :: stack_img
    integer :: opcion, numVentanillas, i

    !Asignar cliente espera
    type(lista_lista_cliente_espera) :: listaDeListas

    !Mostrar cola de impresiones 
    type(Cola_Impresion_P) :: colaImpresion_P
    type(Cola_Impresion_G) :: colaImpresion_G
    
    
    character(:), allocatable :: nombre  ! Se declara una cadena de caracteres que se asignará dinámicamente
    integer :: id_cliente, img_g, img_p

    integer :: j, size        ! Se declaran variables enteras
    logical :: found


    opcion = 0
    do while (opcion /= -1)
      print *, "--------------"
      print *, "Menu principal"
      print *, "--------------"
  
      print *, "1. cargar Clientes"
      print *, "2. Estabeces Ventanillas"
      print *, "3. Procesar asignaciones y mostrar ventanillas"
      print *, "4. Graficar prueba"
  
      print *, "-1 salir"
      
      read *, opcion !leer la opcion
  
      select case (opcion)
      case (1)
        call json%initialize()    ! Se inicializa el módulo JSON
        call json%load(filename='data.json')  ! Se carga el archivo JSON llamado 'data.json'
        
        ! Se obtiene el tamaño del arreglo [] json
        call json%info('',n_children=size)
    
        call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call json%get('', p_list, found)
    
        do j = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            call jsonc%get_child(p_list, j, p_cliente, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
            
            nombre = ''
            id_cliente = 0
            img_g = 0
            img_p = 0

            ! Nombre
            call jsonc%get_child(p_cliente, 'nombre', p_atributos, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
            if (found) then                      
                call jsonc%get(p_atributos, nombre)
            end if
    
            ! Para 'id'
            call jsonc%get_child(p_cliente, 'id', p_atributos, found = found)
            if (found) then
                call jsonc%get(p_atributos, id_cliente)
            end if
    
            ! Para 'img_g'
            call jsonc%get_child(p_cliente, 'img_g', p_atributos, found = found)
            if (found) then
                call jsonc%get(p_atributos, img_g)       
            end if    
    
            ! Para 'img_p'
            call jsonc%get_child(p_cliente, 'img_p', p_atributos, found = found)
            if (found) then
                call jsonc%get(p_atributos, img_p)
            end if    
    
            call encolar(reception_tail, Client(id_cliente, trim(nombre), img_p, img_g, img_p, img_g))
        end do
    
        call json%destroy()  ! Se finaliza el módulo JSON
        
        print *, "Clientes cargados exitosamente"
        call show(reception_tail)

      case (2)
        print *, "Ingrese la cantida de ventanillas: "
        read *, numVentanillas
        do i = 1 , numVentanillas
          call add_ventanilla(listVentanillas, i)
        end do
        print *, "Ventanillas establecidas"
      case (3)
        ! Procesar asignaciones y mostrar ventanillas
        ! Asegurarse de que hay ventanillas y clientes para procesar
        call ejecutar(listVentanillas, reception_tail, listaDeListas, colaImpresion_P, colaImpresion_G)
        call show_ventanilla(listVentanillas)
        call show_cliente_espera(listaDeListas)
        call show_imp_g(colaImpresion_G)
        call show_imp_p(colaImpresion_P)
      case (4)
        call grafica_cola_recepcion(reception_tail, "cola.dot")
        call graficar_ventanillas(listVentanillas, "ventanilla.dot")
      end select
    end do  

end program pixel_print_studio