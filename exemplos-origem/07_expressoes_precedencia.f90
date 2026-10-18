!===============================================================
! Arquivo : 07_expressoes_precedencia.f90
! Tema    : Expressões: precedência e associatividade
! Ordem de precedência (da MAIOR para a MENOR):
!   1. **                       (potência; associa à DIREITA)
!   2. *  /                     (esquerda para direita)
!   3. + - unários e binários   (esquerda para direita)
!   4. //                       (concatenação de strings)
!   5. == /= < <= > >=          (relacionais)
!   6. .not.
!   7. .and.
!   8. .or.
!   9. .eqv.  .neqv.
!  (operadores definidos pelo usuário: ver arquivo 08)
!===============================================================
program expressoes_precedencia
  implicit none

  integer :: a = 2, b = 3, c = 2
  real    :: x = 7.0
  logical :: p = .true., q = .false., s = .true.
  character(len=10) :: nome = "ab"

  print *, "=== Precedência aritmética ==="
  print *, "a + b * c   =", a + b * c, "   (multiplicação primeiro)"
  print *, "(a + b) * c =", (a + b) * c

  print *, "=== Associatividade ==="
  print *, "a ** b ** c   =", a ** b ** c, "  (potência: direita => 2**(3**2))"
  print *, "(a ** b) ** c =", (a ** b) ** c
  print *, "20 - b - c    =", 20 - b - c, "  (subtração: esquerda => (20-3)-2)"
  print *, "20 - (b - c)  =", 20 - (b - c)
  print *, "100 / 10 / 5  =", 100 / 10 / 5

  print *, "=== Menos unário vs potência ==="
  print *, "-a ** 2 =", -a ** 2, "  (é -(a**2), não (-a)**2)"
  print *, "(-a) ** 2 =", (-a) ** 2

  print *, "=== Divisão inteira vs real ==="
  print *, "7 / 2 =", 7 / 2, "   7.0 / 2 =", x / 2, "   mod(7,2) =", mod(7, 2)
  print *, "1 / 2 * 2.0 =", 1 / 2 * 2.0, "  (1/2 inteiro vale 0)"

  print *, "=== Relacionais têm menor precedência que aritméticos ==="
  print *, "a + 1 > b  ->", a + 1 > b

  print *, "=== Lógicos: .not. > .and. > .or. ==="
  print *, "p .or. q .and. s     =", p .or. q .and. s, "  => p .or. (q .and. s)"
  print *, "(p .or. q) .and. s   =", (p .or. q) .and. s
  print *, ".not. p .and. q      =", .not. p .and. q, "  => (.not. p) .and. q"
  print *, "p .eqv. s            =", p .eqv. s

  print *, "=== Concatenação tem precedência sobre relacionais ==="
  print *, 'nome // "c" == "abc" ->', trim(nome) // "c" == "abc"
end program expressoes_precedencia
