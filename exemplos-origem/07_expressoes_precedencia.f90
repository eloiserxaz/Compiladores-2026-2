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

  ! três inteiros para os testes aritméticos
  integer :: a = 2, b = 3, c = 2
  ! um real para mostrar divisão real
  real    :: x = 7.0
  ! três lógicos para os testes com .and./.or.
  logical :: p = .true., q = .false., s = .true.
  ! texto para o teste de concatenação
  character(len=10) :: nome = "ab"

  print *, "=== Precedência aritmética ==="
  ! b * c = 6 é feito antes da soma: 2 + 6 = 8
  print *, "a + b * c   =", a + b * c, "   (multiplicação primeiro)"
  ! parênteses forçam a soma primeiro: (2+3) * 2 = 10
  print *, "(a + b) * c =", (a + b) * c

  print *, "=== Associatividade ==="
  ! ** associa à direita: 2 ** (3 ** 2) = 2 ** 9 = 512
  print *, "a ** b ** c   =", a ** b ** c, "  (potência: direita => 2**(3**2))"
  ! com parênteses: (2 ** 3) ** 2 = 8 ** 2 = 64
  print *, "(a ** b) ** c =", (a ** b) ** c
  ! - associa à esquerda: (20 - 3) - 2 = 15
  print *, "20 - b - c    =", 20 - b - c, "  (subtração: esquerda => (20-3)-2)"
  ! com parênteses: 20 - (3 - 2) = 19
  print *, "20 - (b - c)  =", 20 - (b - c)
  ! / associa à esquerda: (100/10)/5 = 2
  print *, "100 / 10 / 5  =", 100 / 10 / 5

  print *, "=== Menos unário vs potência ==="
  ! ** tem precedência sobre o menos unário: -(2 ** 2) = -4
  print *, "-a ** 2 =", -a ** 2, "  (é -(a**2), não (-a)**2)"
  ! com parênteses o menos entra na potência: (-2) ** 2 = 4
  print *, "(-a) ** 2 =", (-a) ** 2

  print *, "=== Divisão inteira vs real ==="
  ! 7/2 entre inteiros = 3; x/2 com x real = 3.5; mod(7,2) = resto = 1
  print *, "7 / 2 =", 7 / 2, "   7.0 / 2 =", x / 2, "   mod(7,2) =", mod(7, 2)
  ! 1/2 (inteiros) = 0, depois 0 * 2.0 = 0.0
  print *, "1 / 2 * 2.0 =", 1 / 2 * 2.0, "  (1/2 inteiro vale 0)"

  print *, "=== Relacionais têm menor precedência que aritméticos ==="
  ! primeiro calcula a + 1 = 3; depois compara 3 > 3 -> falso
  print *, "a + 1 > b  ->", a + 1 > b

  print *, "=== Lógicos: .not. > .and. > .or. ==="
  ! .and. vem antes de .or.: p .or. (q .and. s) = T .or. F = T
  print *, "p .or. q .and. s     =", p .or. q .and. s, "  => p .or. (q .and. s)"
  ! com parênteses: (T .or. F) .and. T = T
  print *, "(p .or. q) .and. s   =", (p .or. q) .and. s
  ! .not. vem primeiro: (.not. T) .and. F = F .and. F = F
  print *, ".not. p .and. q      =", .not. p .and. q, "  => (.not. p) .and. q"
  ! .eqv. = "equivalente": verdadeiro quando os dois têm o mesmo valor
  print *, "p .eqv. s            =", p .eqv. s

  print *, "=== Concatenação tem precedência sobre relacionais ==="
  ! primeiro concatena "ab" // "c" = "abc"; depois compara com "abc"
  print *, 'nome // "c" == "abc" ->', trim(nome) // "c" == "abc"
end program expressoes_precedencia
