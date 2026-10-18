!===============================================================
! Arquivo : 17_eventos.f90
! Tema    : Tratamento de eventos
! Observação: Fortran não tem um modelo de eventos de interface
!   gráfica embutido. Os mecanismos disponíveis são:
!   (1) padrão da linguagem: event_type (Fortran 2018) para
!       sincronização entre imagens (event post / event wait);
!   (2) flags de exceção IEEE (ver 16_excecoes.f90);
!   (3) sinais do sistema (extensão do GFortran: call signal);
!   (4) padrão de projeto "callback": um despachador de eventos
!       com tabela de manipuladores (procedure pointers),
!       implementado abaixo.
! Exemplo : laço de eventos de uma interface simulada
!===============================================================
module eventos_mod
  implicit none

  integer, parameter :: EV_CLIQUE = 1, EV_TECLA = 2, EV_TIMER = 3, &
                        EV_SAIR = 4, EV_REDIMENSIONAR = 5

  type :: evento_t
    integer :: tipo
    character(len=20) :: dado = ""
  end type evento_t

  ! Assinatura que todo manipulador de evento deve ter
  abstract interface
    subroutine manipulador(ev)
      import :: evento_t
      type(evento_t), intent(in) :: ev
    end subroutine manipulador
  end interface

  ! Ponteiro para procedimento guardado em um componente
  type :: registro_t
    procedure(manipulador), pointer, nopass :: tratar => null()
  end type registro_t

  type(registro_t) :: tabela(5)

contains

  ! Associa um manipulador a um tipo de evento
  subroutine registra(tipo, h)
    integer, intent(in) :: tipo
    procedure(manipulador) :: h
    tabela(tipo)%tratar => h
  end subroutine registra

  ! Despacha o evento para o manipulador registrado (se houver)
  subroutine despacha(ev)
    type(evento_t), intent(in) :: ev
    if (associated(tabela(ev%tipo)%tratar)) then
      call tabela(ev%tipo)%tratar(ev)
    else
      print '(A,I0,A)', "[sem tratador] evento tipo ", ev%tipo, " ignorado"
    end if
  end subroutine despacha

end module eventos_mod

module tratadores_mod
  use eventos_mod
  implicit none
  integer :: cliques = 0
contains

  subroutine ao_clicar(ev)
    type(evento_t), intent(in) :: ev
    cliques = cliques + 1
    print '(A,A,A,I0)', "[CLIQUE] em '", trim(ev%dado), "' | total de cliques: ", cliques
  end subroutine ao_clicar

  subroutine ao_teclar(ev)
    type(evento_t), intent(in) :: ev
    print '(A,A,A)', "[TECLA] tecla '", trim(ev%dado), "' pressionada"
  end subroutine ao_teclar

  subroutine ao_tempo(ev)
    type(evento_t), intent(in) :: ev
    print '(A,A)', "[TIMER] ", trim(ev%dado)
  end subroutine ao_tempo

  subroutine ao_sair(ev)
    type(evento_t), intent(in) :: ev
    print '(A)', "[SAIR] encerrando o laço de eventos"
  end subroutine ao_sair

end module tratadores_mod

program tratamento_eventos
  use eventos_mod
  use tratadores_mod
  implicit none

  type(evento_t) :: fila(7)
  integer :: i

  ! Registro dos manipuladores (EV_REDIMENSIONAR fica sem tratador)
  call registra(EV_CLIQUE, ao_clicar)
  call registra(EV_TECLA, ao_teclar)
  call registra(EV_TIMER, ao_tempo)
  call registra(EV_SAIR, ao_sair)

  ! Fila de eventos simulada
  fila = [evento_t(EV_CLIQUE, "botao OK"), &
          evento_t(EV_TECLA, "a"), &
          evento_t(EV_TIMER, "tick 1"), &
          evento_t(EV_REDIMENSIONAR, "800x600"), &
          evento_t(EV_CLIQUE, "menu Arquivo"), &
          evento_t(EV_SAIR, ""), &
          evento_t(EV_TECLA, "b")]

  ! Laço de eventos
  print *, "=== Laço de eventos ==="
  do i = 1, size(fila)
    call despacha(fila(i))
    if (fila(i)%tipo == EV_SAIR) exit
  end do
  print *, "Eventos não processados após SAIR:", size(fila) - i

  ! ---- Referência (comentado: depende de suporte do compilador) ----
  ! (a) Eventos entre imagens, Fortran 2018:
  !     use, intrinsic :: iso_fortran_env, only: event_type
  !     type(event_type) :: pronto[*]
  !     if (this_image() == 2) event post (pronto[1])
  !     if (this_image() == 1) event wait (pronto)
  ! (b) Sinais do sistema (extensão GNU), ex.: tratar Ctrl+C:
  !     call signal(2, trata_sigint)   ! 2 = SIGINT
end program tratamento_eventos
