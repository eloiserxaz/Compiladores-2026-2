!===============================================================
! Arquivo : 09_estruturas_controle.f90
! Tema    : Estruturas de controle e desvios de fluxo
! Mostra  : exit/cycle com nomes de laço, exit de block, goto com
!           rótulo, return antecipado e stop.
!===============================================================
program estruturas_controle
  implicit none

  integer :: m(3, 3), i, j, alvo, total, tentativas
  logical :: achou

  m = reshape([4, 8, 15, 16, 23, 42, 7, 9, 11], [3, 3])
  alvo = 23
  achou = .false.

  print *, "=== exit com nome: sai do laço EXTERNO ==="
  busca: do i = 1, 3
    do j = 1, 3
      if (m(i, j) == alvo) then
        achou = .true.
        exit busca                 ! interrompe os dois laços
      end if
    end do
  end do busca
  if (achou) print '(A,I0,A,I0,A,I0,A)', "Valor ", alvo, " em (", i, ",", j, ")"

  print *, "=== cycle com nome: pula para a próxima linha ==="
  total = 0
  linhas: do i = 1, 3
    do j = 1, 3
      if (m(i, j) == 42) cycle linhas   ! descarta a linha inteira
    end do
    total = total + sum(m(i, :))
  end do linhas
  print *, "Soma das linhas sem o valor 42:", total

  print *, "=== exit de um block ==="
  validacao: block
    if (alvo < 0) exit validacao
    print *, "alvo é não negativo, validação concluída"
  end block validacao

  print *, "=== goto com rótulo (uso raro, mantido por legado) ==="
  tentativas = 0
10 continue
  tentativas = tentativas + 1
  print *, "tentativa", tentativas
  if (tentativas < 3) goto 10

  print *, "=== return antecipado ==="
  call verifica_positivo(5)
  call verifica_positivo(-5)

  ! Também existem: select case (arquivo 10), do while (arquivo 11),
  ! error stop (arquivo 16) e o goto computado/IF aritmético, que
  ! são obsolescentes e não devem ser usados em código novo.

  stop "Fim normal do programa"     ! encerra a execução

contains

  subroutine verifica_positivo(n)
    integer, intent(in) :: n
    if (n <= 0) then
      print *, n, "não é positivo: saindo da sub-rotina"
      return                         ! retorna ao chamador
    end if
    print *, n, "é positivo"
  end subroutine verifica_positivo

end program estruturas_controle
