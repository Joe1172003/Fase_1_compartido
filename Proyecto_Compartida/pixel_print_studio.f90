program pixel_print_studio
  use Fase_1
  implicit none 
  
  !Cliente
  type(Tail) :: reception_tail
  type(Client) :: cliente

  !Ventanilla
  type(VentanillaList) :: listVentanillas
  type(PilaImagenes) :: stack_img
  integer :: opcion, numVentanillas, i
  
  opcion = 0
  do while (opcion /= -1)
    print *, "--------------"
    print *, "Menu principal"
    print *, "--------------"

    print *, "1. cargar Clientes"
    print *, "2. Estabeces Ventanillas"
    print *, "3. Procesar asignaciones y mostrar ventanillas"
    print *, "-1 salir"
    
    read *, opcion !leer la opcion

    select case (opcion)
    case (1)
      call encolar(reception_tail, Client(1, "Sergio", 2, 2))
      call encolar(reception_tail, Client(2, "Joel", 3, 1))
      call encolar(reception_tail, Client(3, "Marck", 1, 3))
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
      call ejecutar(listVentanillas, reception_tail)
      call show_ventanilla(listVentanillas)
    end select
  end do  
  
end program pixel_print_studio








