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

  ! i, j, k = contadores; n = número em teste; d = divisor
  ! passos = contador da sequência de Collatz
  integer :: i, j, n, d, passos, k
  ! indica se n é primo
  logical :: eh_primo
  ! vetor de 5 quadrados e vetor de 10 reais
  integer :: quadrados(5)
  real :: a(10)

  print *, "=== do com limites e passo ==="
  ! do i = início, fim: repete com i = 1, 2, 3, 4, 5
  do i = 1, 5
    ! I0 = inteiro sem espaços extras; escreve "5 x i = resultado"
    write (*, '(A,I0,A,I0)') "5 x ", i, " = ", 5 * i
  end do
  ! terceiro valor = passo; aqui conta de 10 até 1 de 3 em 3: 10, 7, 4, 1
  do i = 10, 1, -3
    ! advance='no' mantém tudo na mesma linha; 1X = um espaço
    write (*, '(I0,1X)', advance='no') i
  end do
  ! print * sem itens termina a linha
  print *

  print *, "=== do while: sequência de Collatz a partir de 27 ==="
  n = 27
  passos = 0
  ! repete ENQUANTO a condição for verdadeira (n diferente de 1)
  do while (n /= 1)
    ! se n é par, divide por 2; senão, faz 3n + 1
    if (mod(n, 2) == 0) then
      n = n / 2
    else
      n = 3 * n + 1
    end if
    ! conta quantos passos foram dados
    passos = passos + 1
  end do
  print *, "Passos até chegar em 1:", passos

  print *, "=== do com cycle e exit: primos até 30 ==="
  do n = 2, 30
    ! começa supondo que n é primo
    eh_primo = .true.
    ! testa divisores de 2 até a raiz quadrada de n
    do d = 2, int(sqrt(real(n)))
      ! se n é divisível por d, não é primo
      if (mod(n, d) == 0) then
        eh_primo = .false.
        ! "exit" sai do laço mais interno (o dos divisores)
        exit
      end if
    end do
    ! "cycle" pula o resto do corpo e vai para o próximo n
    if (.not. eh_primo) cycle
    ! só chega aqui se n é primo: imprime sem pular linha
    write (*, '(I0,1X)', advance='no') n
  end do
  print *

  print *, "=== do infinito (sem controle) + exit ==="
  k = 0
  ! "do" sem condição repete para sempre, até um "exit"
  do
    k = k + 1
    ! quando k*k passar de 50, sai do laço
    if (k * k > 50) exit
  end do
  print *, "Menor k com k*k > 50:", k

  print *, "=== laços nomeados ==="
  ! nomes "externo" e "interno" permitem controlar qual laço sofre cycle/exit
  externo: do i = 1, 3
    interno: do j = 1, 3
      ! quando j passa de i, pula para o próximo i do laço EXTERNO
      if (j > i) cycle externo
      ! imprime o par (i,j) na mesma linha
      write (*, '(A,I0,A,I0,A)', advance='no') "(", i, ",", j, ") "
    end do interno
  end do externo
  print *

  print *, "=== do implícito (em listas e construtores de array) ==="
  ! (expressão, i = 1, 5) gera 1, 4, 9, 16, 25 e preenche o vetor
  quadrados = [(i * i, i = 1, 5)]
  print *, "quadrados:", quadrados
  ! também funciona direto dentro do print
  print *, "tabuada do 3:", (3 * i, i = 1, 10)

  print *, "=== do concurrent: iterações independentes ==="
  ! do concurrent avisa ao compilador que as iterações não dependem
  ! umas das outras, então podem rodar em paralelo
  do concurrent (i = 1:10)
    a(i) = real(i) ** 2
  end do
  print *, "a =", a
end program repeticao
