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
PROGRAM Palavras_Reservadas
  IMPLICIT NONE

  ! Os nomes abaixo são intrínsecas (sum, count, max, len, size),
  ! mas aqui viram variáveis comuns: isso é legal em Fortran.
  integer :: sum = 10, count = 3, max = 99, len = 5, size = 8

  ! Lista (não exaustiva) de palavras-chave do Fortran moderno
  character(len=12), parameter :: palavras(*) = [character(len=12) :: &
    "program", "end", "module", "contains", "use", "implicit", "none", &
    "integer", "real", "complex", "logical", "character", "type", &
    "if", "then", "else", "select", "case", "do", "while", "exit", &
    "cycle", "goto", "stop", "return", "call", "subroutine", &
    "function", "intent", "allocatable", "pointer", "target", &
    "parameter", "interface", "recursive", "pure", "elemental"]

  integer :: i

  print *, "=== Não diferencia maiúsculas de minúsculas ==="
  Print *, "PRINT, print e Print são o mesmo comando"

  print *, "=== Nomes de intrínsecas usados como variáveis ==="
  print *, "sum =", sum, " count =", count, " max =", max
  print *, "len =", len, " size =", size

  ! Em Fortran até seria possível chamar uma variável de 'if' ou
  ! 'program' (o compilador decide pelo contexto), mas isso é
  ! fortemente DESACONSELHADO por prejudicar a legibilidade.

  print *, "=== Palavras-chave comuns (", size_lista(), " listadas) ==="
  do i = 1, size_lista(), 4
    write (*, '(4A14)') palavras(i:min(i + 3, size_lista()))
  end do

contains

  integer function size_lista()
    size_lista = ubound(palavras, 1)
  end function size_lista

end program Palavras_Reservadas
