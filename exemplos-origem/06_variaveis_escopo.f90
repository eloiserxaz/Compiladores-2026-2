!===============================================================
! Arquivo : 06_variaveis_escopo.f90
! Tema    : Variáveis: nomes, vinculação, tempo de vida e escopo
! Mostra  :
!  - nomes: letras, dígitos e _, começando por letra, até 63
!    caracteres, SEM distinção entre maiúsculas e minúsculas;
!  - vinculação: estática (save/módulo), automática (pilha),
!    dinâmica (allocatable) e tipagem implícita (legado);
!  - escopo: programa, módulo, bloco (block), associação por
!    hospedeiro (host association).
!===============================================================
module config_mod
  implicit none
  integer :: contador_global = 0      ! escopo de módulo, tempo de vida do programa
contains
  subroutine incrementa()
    contador_global = contador_global + 1
  end subroutine incrementa
end module config_mod

program variaveis_escopo
  use config_mod
  implicit none

  integer :: x = 1                    ! variável do programa principal (estática)
  real    :: Valor                    ! maiúsculas/minúsculas não importam
  integer :: media_final_2026 = 8     ! nome válido: letras, dígitos e _
  integer :: i

  print *, "=== Nomes ==="
  Valor = 3.5
  print *, "VALOR =", VALOR, " | valor =", valor, " | media =", media_final_2026

  print *, "=== Tempo de vida: save ==="
  do i = 1, 3
    call conta_chamadas()
  end do

  print *, "=== Escopo: o bloco sombreia o x externo ==="
  print *, "x externo antes :", x
  block
    integer :: x                     ! outra variável, só existe no bloco
    x = 99
    print *, "x dentro do block:", x
  end block
  print *, "x externo depois :", x

  print *, "=== Escopo de módulo ==="
  call incrementa()
  call incrementa()
  print *, "contador_global =", contador_global

  print *, "=== Associação por hospedeiro ==="
  call usa_variavel_do_host()
  print *, "x após a chamada:", x

  print *, "=== Vinculação automática (tamanho conhecido na chamada) ==="
  call soma_automatica(5)

  print *, "=== Vinculação dinâmica (allocate/deallocate) ==="
  call aloca_dinamica(4)

  print *, "=== Tipagem implícita (legado, sem implicit none) ==="
  call exemplo_implicito()

contains

  ! 'chamadas' é estática: mantém o valor entre chamadas (save)
  subroutine conta_chamadas()
    integer, save :: chamadas = 0
    chamadas = chamadas + 1
    print *, "conta_chamadas foi chamada", chamadas, "vez(es)"
  end subroutine conta_chamadas

  ! Enxerga e altera 'x' do programa principal
  subroutine usa_variavel_do_host()
    x = x + 10
    print *, "dentro da sub-rotina, x =", x
  end subroutine usa_variavel_do_host

  ! 'tmp' é alocado na pilha ao entrar e destruído ao sair
  subroutine soma_automatica(n)
    integer, intent(in) :: n
    real :: tmp(n)
    integer :: k
    tmp = [(real(k), k = 1, n)]
    print *, "soma de 1..n =", sum(tmp)
  end subroutine soma_automatica

  subroutine aloca_dinamica(n)
    integer, intent(in) :: n
    integer, allocatable :: v(:)
    allocate (v(n))
    v = n
    print *, "v =", v
    deallocate (v)
    print *, "v ainda alocado? ", allocated(v)
  end subroutine aloca_dinamica

end program variaveis_escopo

! Sub-rotina EXTERNA, fora do program: não herda 'implicit none'.
! Regra legada: nomes iniciados por I..N são integer; demais, real.
subroutine exemplo_implicito()
  i = 7              ! integer (começa com I)
  x = i / 2          ! divisão inteira 7/2 = 3, depois convertida para real
  print *, "i =", i, " x =", x
end subroutine exemplo_implicito
