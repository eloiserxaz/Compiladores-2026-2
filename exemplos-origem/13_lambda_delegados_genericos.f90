!===============================================================
! Arquivo : 13_lambda_delegados_genericos.f90
! Tema    : Subprogramas delegados, "lambdas", co-rotinas e
!           programação genérica
! Observações sobre a linguagem:
!  - Fortran NÃO tem funções lambda anônimas nativas. O mais
!    próximo é passar uma função (módulo ou interna) como
!    argumento; funções internas "capturam" variáveis do
!    programa hospedeiro (host association).
!  - Fortran NÃO tem co-rotinas nativas. Simula-se um gerador com
!    uma função que guarda estado (save).
!  - Genericidade: interfaces genéricas (sobrecarga) e
!    polimorfismo ilimitado class(*) com select type.
! Exemplo : integração numérica (regra do trapézio)
!===============================================================
module numerico_mod
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none

  ! Interface abstrata = descreve a "forma" de uma função (recebe um
  ! real de 64 bits e devolve um real de 64 bits), sem implementá-la.
  ! Serve como "tipo" de função que pode ser passada como argumento.
  abstract interface
    function funcao_real(x) result(y)
      real(8), intent(in) :: x
      real(8) :: y
    end function funcao_real
  end interface

contains

  ! Subprograma que recebe outro subprograma (delegado/callback).
  ! Calcula a área sob f entre a e b usando n trapézios.
  function integra(f, a, b, n) result(area)
    ! "f" é uma função qualquer com a forma de funcao_real
    procedure(funcao_real) :: f
    real(8), intent(in) :: a, b
    integer, intent(in) :: n
    real(8) :: area, h
    integer :: i
    ! h = largura de cada trapézio
    h = (b - a) / n
    ! extremos entram com peso 1/2
    area = 0.5d0 * (f(a) + f(b))
    ! pontos intermediários entram com peso 1
    do i = 1, n - 1
      area = area + f(a + i * h)
    end do
    ! multiplica pela largura
    area = area * h
  end function integra

  ! f(x) = x²
  function quadrado(x) result(y)
    real(8), intent(in) :: x
    real(8) :: y
    y = x * x
  end function quadrado

  ! f(x) = sen(x)
  function seno(x) result(y)
    real(8), intent(in) :: x
    real(8) :: y
    y = sin(x)
  end function seno

  ! "Co-rotina" simulada: cada chamada retoma de onde parou
  function proximo_fib() result(v)
    integer(int64) :: v
    ! "save" faz a e b guardarem os valores entre as chamadas
    integer(int64), save :: a = 0_int64, b = 1_int64
    integer(int64) :: t
    ! devolve o termo atual
    v = a
    ! calcula o próximo termo e avança o estado
    t = a + b
    a = b
    b = t
  end function proximo_fib

end module numerico_mod

module genericos_mod
  implicit none

  ! Um nome genérico, várias implementações (resolvido em compilação):
  ! "troca" chama troca_int, troca_real ou troca_char conforme o tipo
  interface troca
    module procedure troca_int, troca_real, troca_char
  end interface troca

  ! "maior" escolhe entre as versões para inteiro e para real
  interface maior
    module procedure maior_int, maior_real
  end interface maior

contains

  ! Troca o conteúdo de dois inteiros (usa variável temporária t)
  subroutine troca_int(a, b)
    integer, intent(inout) :: a, b
    integer :: t
    ! o ";" permite vários comandos na mesma linha
    t = a; a = b; b = t
  end subroutine troca_int

  subroutine troca_real(a, b)
    real, intent(inout) :: a, b
    real :: t
    t = a; a = b; b = t
  end subroutine troca_real

  subroutine troca_char(a, b)
    ! len=* = aceita texto de qualquer tamanho (a e b com o mesmo tamanho)
    character(len=*), intent(inout) :: a, b
    ! temporário com o mesmo tamanho de a
    character(len=len(a)) :: t
    t = a; a = b; b = t
  end subroutine troca_char

  function maior_int(a, b) result(m)
    integer, intent(in) :: a, b
    integer :: m
    m = max(a, b)
  end function maior_int

  function maior_real(a, b) result(m)
    real, intent(in) :: a, b
    real :: m
    m = max(a, b)
  end function maior_real

  ! Polimorfismo ilimitado: aceita qualquer tipo
  subroutine descreve(x)
    ! class(*) = o argumento pode ser de QUALQUER tipo
    class(*), intent(in) :: x
    ! select type descobre o tipo real do argumento em execução
    select type (x)
    type is (integer)
      print *, "inteiro  :", x
    type is (real)
      print *, "real     :", x
    type is (logical)
      print *, "lógico   :", x
    type is (character(len=*))
      print *, "texto    : ", x
    ! class default = qualquer outro tipo
    class default
      print *, "tipo não tratado"
    end select
  end subroutine descreve

end module genericos_mod

program lambda_delegados_genericos
  use numerico_mod
  use genericos_mod
  implicit none

  ! Ponteiro para procedimento: pode apontar para qualquer função
  ! com a forma de funcao_real; começa sem apontar para nada
  procedure(funcao_real), pointer :: fp => null()
  real(8), parameter :: pi = 3.14159265358979d0
  ! k é "capturado" pela função interna "escalada" (mais abaixo)
  real(8) :: k = 3.0d0
  integer :: na, nb
  real :: ra, rb
  character(len=4) :: ca, cb
  integer :: n

  print *, "=== Delegado: função passada como argumento ==="
  ! passa "quadrado" para integra: resultado ≈ 9 (integral de x² de 0 a 3)
  print *, "Integral de x^2 em [0,3] =", integra(quadrado, 0.0d0, 3.0d0, 1000)

  print *, "=== Ponteiro para procedimento (escolha em execução) ==="
  ! "=>" faz fp apontar para a função seno
  fp => seno
  ! chamar fp(...) executa a função apontada
  print *, "fp(pi/2) =", fp(pi / 2.0d0)
  ! o ponteiro também pode ser passado como argumento: resultado ≈ 2
  print *, "Integral de sen(x) em [0,pi] =", integra(fp, 0.0d0, pi, 1000)

  print *, "=== 'Lambda': função interna que captura k ==="
  ! integral de 3x de 0 a 1 = 1.5
  print *, "Integral de k*x em [0,1] (k=3) =", integra(escalada, 0.0d0, 1.0d0, 1000)

  print *, "=== 'Co-rotina': gerador de Fibonacci com estado ==="
  ! cada chamada de proximo_fib devolve o termo seguinte
  do n = 1, 10
    write (*, '(I0,1X)', advance='no') proximo_fib()
  end do
  print *

  print *, "=== Programação genérica: interface 'troca' ==="
  na = 1; nb = 2
  ra = 1.5; rb = 2.5
  ca = "abcd"; cb = "wxyz"
  ! o mesmo nome "troca" funciona para inteiro, real e texto
  call troca(na, nb)
  call troca(ra, rb)
  call troca(ca, cb)
  print *, na, nb, ra, rb, ca, " ", cb
  print *, "maior(3,8) =", maior(3, 8), "  maior(2.5,1.5) =", maior(2.5, 1.5)

  print *, "=== class(*) + select type ==="
  ! a mesma sub-rotina trata quatro tipos diferentes
  call descreve(42)
  call descreve(3.14)
  call descreve(.true.)
  call descreve("Fortran")

contains

  ! Esta função usa 'k' do programa principal (fecho léxico).
  ! Obs.: passar procedimento interno como argumento pode gerar
  ! aviso sobre "pilha executável" em alguns sistemas; é inofensivo.
  function escalada(x) result(y)
    real(8), intent(in) :: x
    real(8) :: y
    ! k não é declarado aqui: vem do programa principal
    y = k * x
  end function escalada

end program lambda_delegados_genericos
