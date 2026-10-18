!===============================================================
! Arquivo : 01_hello_world.f90
! Tema    : Hello World
! Mostra  : estrutura mínima de um programa Fortran (program/end
!           program), saída com print * e write com formato.
!===============================================================
program hello_world
  implicit none

  ! print * usa formatação "livre" (list-directed)
  print *, "Olá, mundo! Hello, World!"

  ! write(unidade, formato): unidade * = saída padrão (terminal)
  write (*, '(A)') "Hello, World! (com formato explícito)"
end program hello_world
