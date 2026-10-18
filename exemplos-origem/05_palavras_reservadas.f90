!===============================================================
! Arquivo : 05_palavras_reservadas.f90
! Tema    : Palavras-chave e palavras reservadas
! Ponto central: Fortran NÃO possui palavras reservadas. As
!   palavras-chave (program, if, do, real...) só têm significado
!   especial dentro do contexto de um comando. Por isso até nomes
!   de funções intrínsecas podem ser usados como identificadores.
! Mostra  : insensibilidade a maiúsculas/minúsculas, uso de nomes
!           de intrínsecas como variáveis e uma lista de
!           palavras-chave comuns da linguagem.
!===============================================================

! Repare: PROGRAM, IMPLICIT NONE, INTEGER em maiúsculas funcionam
! igual às minúsculas, pois Fortran não diferencia caixa.
PROGRAM Palavras_Reservadas
  IMPLICIT NONE

  ! Os nomes abaixo são intrínsecas (sum, count, max, len, size),
  ! mas aqui viram variáveis comuns: isso é legal em Fortran.
  ! Dentro deste programa, "sum" passa a ser um inteiro com valor 10
  ! (e não mais a função que soma elementos de um array).
  integer :: sum = 10, count = 3, max = 99, len = 5, size = 8

  ! Vetor constante (parameter) com 37 palavras-chave de Fortran.
  ! "(*)" = o compilador conta quantos elementos há.
  ! "[character(len=12) :: ...]" declara que todos os textos têm 12
  ! posições (necessário porque as palavras têm tamanhos diferentes).
  ! O "&" no fim da linha continua o comando na linha seguinte.
  character(len=12), parameter :: palavras(*) = [character(len=12) :: &
    "program", "end", "module", "contains", "use", "implicit", "none", &
    "integer", "real", "complex", "logical", "character", "type", &
    "if", "then", "else", "select", "case", "do", "while", "exit", &
    "cycle", "goto", "stop", "return", "call", "subroutine", &
    "function", "intent", "allocatable", "pointer", "target", &
    "parameter", "interface", "recursive", "pure", "elemental"]

  ! contador do laço de impressão
  integer :: i

  print *, "=== Não diferencia maiúsculas de minúsculas ==="
  ! "Print" com P maiúsculo funciona como "print"
  Print *, "PRINT, print e Print são o mesmo comando"

  print *, "=== Nomes de intrínsecas usados como variáveis ==="
  ! Imprime as variáveis que "escondem" as funções intrínsecas
  print *, "sum =", sum, " count =", count, " max =", max
  print *, "len =", len, " size =", size

  ! Em Fortran até seria possível chamar uma variável de 'if' ou
  ! 'program' (o compilador decide pelo contexto), mas isso é
  ! fortemente DESACONSELHADO por prejudicar a legibilidade.

  ! size_lista() é uma função interna (definida mais abaixo)
  print *, "=== Palavras-chave comuns (", size_lista(), " listadas) ==="
  ! Vai de 4 em 4 (passo 4) para imprimir 4 palavras por linha
  do i = 1, size_lista(), 4
    ! palavras(i:min(i+3, n)) = fatia de até 4 elementos; min() evita
    ! passar do fim do vetor. '(4A14)' = 4 textos de largura 14.
    write (*, '(4A14)') palavras(i:min(i + 3, size_lista()))
  end do

! Funções internas do programa ficam depois do "contains"
contains

  ! Função que devolve a quantidade de palavras do vetor
  integer function size_lista()
    ! ubound(vetor, 1) = maior índice da dimensão 1 (aqui, 37)
    size_lista = ubound(palavras, 1)
  end function size_lista

end program Palavras_Reservadas
