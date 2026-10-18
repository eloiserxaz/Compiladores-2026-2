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

! Módulo com uma variável "global" compartilhada entre quem o usar
module config_mod
  implicit none
  ! Variável de módulo: existe durante toda a execução do programa
  integer :: contador_global = 0
contains
  ! Sub-rotina que soma 1 ao contador do módulo
  subroutine incrementa()
    contador_global = contador_global + 1
  end subroutine incrementa
end module config_mod

program variaveis_escopo
  ! Torna visíveis as variáveis e sub-rotinas do módulo config_mod
  use config_mod
  implicit none

  ! Variável do programa principal: vinculação estática, vive até o fim
  integer :: x = 1
  ! Nome com maiúscula: "Valor", "VALOR" e "valor" são a mesma variável
  real    :: Valor
  ! Nome válido: começa com letra e usa letras, dígitos e "_"
  integer :: media_final_2026 = 8
  integer :: i

  print *, "=== Nomes ==="
  Valor = 3.5
  ! Escrever VALOR e valor mostra que a caixa não importa
  print *, "VALOR =", VALOR, " | valor =", valor, " | media =", media_final_2026

  print *, "=== Tempo de vida: save ==="
  ! Chama 3 vezes; a variável "chamadas" lembra o valor entre as chamadas
  do i = 1, 3
    call conta_chamadas()
  end do

  print *, "=== Escopo: o bloco sombreia o x externo ==="
  print *, "x externo antes :", x
  ! "block" cria um escopo local: o que é declarado aqui some no "end block"
  block
    ! Este x é OUTRA variável e "esconde" o x de fora (sombreamento)
    integer :: x
    x = 99
    print *, "x dentro do block:", x
  end block
  ! Fora do bloco volta a valer o x original (continua 1)
  print *, "x externo depois :", x

  print *, "=== Escopo de módulo ==="
  ! Cada chamada soma 1 em contador_global (variável do módulo)
  call incrementa()
  call incrementa()
  print *, "contador_global =", contador_global

  print *, "=== Associação por hospedeiro ==="
  ! A sub-rotina interna enxerga e altera o x do programa
  call usa_variavel_do_host()
  print *, "x após a chamada:", x

  print *, "=== Vinculação automática (tamanho conhecido na chamada) ==="
  call soma_automatica(5)

  print *, "=== Vinculação dinâmica (allocate/deallocate) ==="
  call aloca_dinamica(4)

  print *, "=== Tipagem implícita (legado, sem implicit none) ==="
  call exemplo_implicito()

! Sub-rotinas internas: enxergam as variáveis do programa (host)
contains

  ! 'chamadas' é estática: mantém o valor entre chamadas (save)
  subroutine conta_chamadas()
    ! "save" faz a variável guardar o valor entre uma chamada e outra.
    ! A inicialização "= 0" acontece só uma vez, no início do programa.
    integer, save :: chamadas = 0
    chamadas = chamadas + 1
    print *, "conta_chamadas foi chamada", chamadas, "vez(es)"
  end subroutine conta_chamadas

  ! Enxerga e altera 'x' do programa principal
  subroutine usa_variavel_do_host()
    ! Não há declaração de x aqui: usa o x do programa (hospedeiro)
    x = x + 10
    print *, "dentro da sub-rotina, x =", x
  end subroutine usa_variavel_do_host

  ! 'tmp' é alocado na pilha ao entrar e destruído ao sair
  subroutine soma_automatica(n)
    integer, intent(in) :: n
    ! Array "automático": o tamanho vem do argumento n
    real :: tmp(n)
    integer :: k
    ! Preenche tmp com 1.0, 2.0, ..., n
    tmp = [(real(k), k = 1, n)]
    ! sum() soma os elementos (1+2+3+4+5 = 15)
    print *, "soma de 1..n =", sum(tmp)
  end subroutine soma_automatica

  ! Array dinâmico local à sub-rotina
  subroutine aloca_dinamica(n)
    integer, intent(in) :: n
    integer, allocatable :: v(:)
    ! reserva memória para n inteiros
    allocate (v(n))
    ! atribui n a todos os elementos
    v = n
    print *, "v =", v
    ! libera a memória
    deallocate (v)
    ! allocated() informa se o array ainda está alocado (agora, não)
    print *, "v ainda alocado? ", allocated(v)
  end subroutine aloca_dinamica

end program variaveis_escopo

! Sub-rotina EXTERNA, fora do program: não herda 'implicit none'.
! Regra legada: nomes iniciados por I..N são integer; demais, real.
subroutine exemplo_implicito()
  ! "i" começa com I: virou integer sem ser declarada
  i = 7
  ! "x" começa com X: virou real sem ser declarada.
  ! i / 2 é divisão inteira (7/2 = 3), depois convertida para real (3.0)
  x = i / 2
  print *, "i =", i, " x =", x
end subroutine exemplo_implicito
