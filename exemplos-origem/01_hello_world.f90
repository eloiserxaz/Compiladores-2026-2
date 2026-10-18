!===============================================================
! Arquivo : 01_hello_world.f90
! Tema    : Hello World
! Mostra  : estrutura mínima de um programa Fortran (program/end
!           program), saída com print * e write com formato.
!===============================================================

! "program" inicia o programa principal e dá um nome a ele.
! Todo programa Fortran executável tem exatamente um program.
program hello_world

  ! "implicit none" DESLIGA a tipagem implícita antiga do Fortran
  ! (em que a primeira letra do nome decidia o tipo da variável).
  ! Com ele, toda variável precisa ser declarada: evita erros de digitação.
  implicit none

  ! "print *" escreve no terminal. O "*" significa formato livre:
  ! o compilador decide como mostrar os valores.
  ! O texto entre aspas é uma constante do tipo character.
  print *, "Olá, mundo! Hello, World!"

  ! "write (unidade, formato)" é a forma geral de escrever.
  ! Unidade "*" = saída padrão (terminal).
  ! Formato '(A)' = escreva um texto (A de "alphanumeric") como ele é.
  write (*, '(A)') "Hello, World! (com formato explícito)"

! "end program" encerra o programa; o nome é opcional mas ajuda a ler.
end program hello_world
