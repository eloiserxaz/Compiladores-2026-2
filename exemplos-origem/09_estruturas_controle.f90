!===============================================================
! Arquivo : 09_estruturas_controle.f90
! Tema    : Estruturas de controle e desvios de fluxo
! Mostra  : exit/cycle com nomes de laço, exit de block, goto com
!           rótulo, return antecipado e stop.
!===============================================================
program estruturas_controle
  implicit none

  ! m = matriz 3x3; i, j = índices; alvo = valor procurado
  ! total = soma acumulada; tentativas = contador do goto
  integer :: m(3, 3), i, j, alvo, total, tentativas
  ! indica se o valor foi encontrado
  logical :: achou

  ! Preenche a matriz por colunas com 9 valores
  m = reshape([4, 8, 15, 16, 23, 42, 7, 9, 11], [3, 3])
  alvo = 23
  achou = .false.

  print *, "=== exit com nome: sai do laço EXTERNO ==="
  ! "busca:" é o nome do laço externo (um rótulo para exit/cycle)
  busca: do i = 1, 3
    ! laço interno percorre as colunas
    do j = 1, 3
      ! compara o elemento (i, j) com o valor procurado
      if (m(i, j) == alvo) then
        achou = .true.
        ! "exit busca" interrompe o laço NOMEADO (os dois laços de uma vez)
        exit busca
      end if
    end do
  end do busca
  ! i e j guardam a posição onde o laço foi interrompido
  if (achou) print '(A,I0,A,I0,A,I0,A)', "Valor ", alvo, " em (", i, ",", j, ")"

  print *, "=== cycle com nome: pula para a próxima linha ==="
  total = 0
  linhas: do i = 1, 3
    do j = 1, 3
      ! "cycle linhas" abandona a linha atual e vai para a próxima
      ! iteração do laço EXTERNO (descarta a linha inteira)
      if (m(i, j) == 42) cycle linhas
    end do
    ! só chega aqui se a linha NÃO tinha o 42: soma a linha inteira
    total = total + sum(m(i, :))
  end do linhas
  print *, "Soma das linhas sem o valor 42:", total

  print *, "=== exit de um block ==="
  ! "block" também pode ser nomeado e interrompido com exit
  validacao: block
    ! se alvo fosse negativo, sairia do bloco aqui
    if (alvo < 0) exit validacao
    print *, "alvo é não negativo, validação concluída"
  end block validacao

  print *, "=== goto com rótulo (uso raro, mantido por legado) ==="
  tentativas = 0
  ! "10" é um rótulo numérico; "continue" não faz nada, só marca o ponto
10 continue
  tentativas = tentativas + 1
  print *, "tentativa", tentativas
  ! "goto 10" salta de volta ao rótulo 10 (cria um laço manual)
  if (tentativas < 3) goto 10

  print *, "=== return antecipado ==="
  call verifica_positivo(5)
  call verifica_positivo(-5)

  ! Também existem: select case (arquivo 10), do while (arquivo 11),
  ! error stop (arquivo 16) e o goto computado/IF aritmético, que
  ! são obsolescentes e não devem ser usados em código novo.

  ! "stop" encerra o programa imediatamente e pode mostrar uma mensagem
  stop "Fim normal do programa"

contains

  ! Sub-rotina que sai mais cedo quando o número não é positivo
  subroutine verifica_positivo(n)
    integer, intent(in) :: n
    if (n <= 0) then
      print *, n, "não é positivo: saindo da sub-rotina"
      ! "return" volta imediatamente para quem chamou a sub-rotina
      return
    end if
    ! só executa se o return acima não foi usado
    print *, n, "é positivo"
  end subroutine verifica_positivo

end program estruturas_controle
