!===============================================================
! Arquivo : 11_repeticao.f90
! Tema    : Comandos de repetição: tipos e implementação
! Exemplo : tabuada, números primos, sequência de Collatz
! Mostra  : do com passo, do while, do infinito com exit, cycle,
!           laços nomeados, do implícito, do concurrent.
! Obs.    : a variável de controle de um do deve ser integer;
!           laços com variável real foram removidos do padrão.
!===============================================================
program repeticao
  implicit none

  integer :: i, j, n, d, passos, k
  logical :: eh_primo
  integer :: quadrados(5)
  real :: a(10)

  print *, "=== do com limites e passo ==="
  do i = 1, 5
    write (*, '(A,I0,A,I0)') "5 x ", i, " = ", 5 * i
  end do
  do i = 10, 1, -3                         ! passo negativo
    write (*, '(I0,1X)', advance='no') i
  end do
  print *

  print *, "=== do while: sequência de Collatz a partir de 27 ==="
  n = 27
  passos = 0
  do while (n /= 1)
    if (mod(n, 2) == 0) then
      n = n / 2
    else
      n = 3 * n + 1
    end if
    passos = passos + 1
  end do
  print *, "Passos até chegar em 1:", passos

  print *, "=== do com cycle e exit: primos até 30 ==="
  do n = 2, 30
    eh_primo = .true.
    do d = 2, int(sqrt(real(n)))
      if (mod(n, d) == 0) then
        eh_primo = .false.
        exit                               ! sai do laço interno
      end if
    end do
    if (.not. eh_primo) cycle              ! pula para o próximo n
    write (*, '(I0,1X)', advance='no') n
  end do
  print *

  print *, "=== do infinito (sem controle) + exit ==="
  k = 0
  do
    k = k + 1
    if (k * k > 50) exit
  end do
  print *, "Menor k com k*k > 50:", k

  print *, "=== laços nomeados ==="
  externo: do i = 1, 3
    interno: do j = 1, 3
      if (j > i) cycle externo
      write (*, '(A,I0,A,I0,A)', advance='no') "(", i, ",", j, ") "
    end do interno
  end do externo
  print *

  print *, "=== do implícito (em listas e construtores de array) ==="
  quadrados = [(i * i, i = 1, 5)]
  print *, "quadrados:", quadrados
  print *, "tabuada do 3:", (3 * i, i = 1, 10)

  print *, "=== do concurrent: iterações independentes ==="
  do concurrent (i = 1:10)
    a(i) = real(i) ** 2
  end do
  print *, "a =", a
end program repeticao
