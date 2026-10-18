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

  ! Interface abstrata = "tipo" de função que pode ser delegada
  abstract interface
    function funcao_real(x) result(y)
      real(8), intent(in) :: x
      real(8) :: y
    end function funcao_real
  end interface

contains

  ! Subprograma que recebe outro subprograma (delegado/callback)
  function integra(f, a, b, n) result(area)
    procedure(funcao_real) :: f
    real(8), intent(in) :: a, b
    integer, intent(in) :: n
    real(8) :: area, h
    integer :: i
    h = (b - a) / n
    area = 0.5d0 * (f(a) + f(b))
    do i = 1, n - 1
      area = area + f(a + i * h)
    end do
    area = area * h
  end function integra

  function quadrado(x) result(y)
    real(8), intent(in) :: x
    real(8) :: y
    y = x * x
  end function quadrado

  function seno(x) result(y)
    real(8), intent(in) :: x
    real(8) :: y
    y = sin(x)
  end function seno

  ! "Co-rotina" simulada: cada chamada retoma de onde parou
  function proximo_fib() result(v)
    integer(int64) :: v
    integer(int64), save :: a = 0_int64, b = 1_int64
    integer(int64) :: t
    v = a
    t = a + b
    a = b
    b = t
  end function proximo_fib

end module numerico_mod

module genericos_mod
  implicit none

  ! Um nome genérico, várias implementações (resolvido em compilação)
  interface troca
    module procedure troca_int, troca_real, troca_char
  end interface troca

  interface maior
    module procedure maior_int, maior_real
  end interface maior

contains

  subroutine troca_int(a, b)
    integer, intent(inout) :: a, b
    integer :: t
    t = a; a = b; b = t
  end subroutine troca_int

  subroutine troca_real(a, b)
    real, intent(inout) :: a, b
    real :: t
    t = a; a = b; b = t
  end subroutine troca_real

  subroutine troca_char(a, b)
    character(len=*), intent(inout) :: a, b   ! mesmo comprimento
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
    class(*), intent(in) :: x
    select type (x)
    type is (integer)
      print *, "inteiro  :", x
    type is (real)
      print *, "real     :", x
    type is (logical)
      print *, "lógico   :", x
    type is (character(len=*))
      print *, "texto    : ", x
    class default
      print *, "tipo não tratado"
    end select
  end subroutine descreve

end module genericos_mod

program lambda_delegados_genericos
  use numerico_mod
  use genericos_mod
  implicit none

  procedure(funcao_real), pointer :: fp => null()   ! ponteiro para procedimento
  real(8), parameter :: pi = 3.14159265358979d0
  real(8) :: k = 3.0d0
  integer :: na, nb
  real :: ra, rb
  character(len=4) :: ca, cb
  integer :: n

  print *, "=== Delegado: função passada como argumento ==="
  print *, "Integral de x^2 em [0,3] =", integra(quadrado, 0.0d0, 3.0d0, 1000)

  print *, "=== Ponteiro para procedimento (escolha em execução) ==="
  fp => seno
  print *, "fp(pi/2) =", fp(pi / 2.0d0)
  print *, "Integral de sen(x) em [0,pi] =", integra(fp, 0.0d0, pi, 1000)

  print *, "=== 'Lambda': função interna que captura k ==="
  print *, "Integral de k*x em [0,1] (k=3) =", integra(escalada, 0.0d0, 1.0d0, 1000)

  print *, "=== 'Co-rotina': gerador de Fibonacci com estado ==="
  do n = 1, 10
    write (*, '(I0,1X)', advance='no') proximo_fib()
  end do
  print *

  print *, "=== Programação genérica: interface 'troca' ==="
  na = 1; nb = 2
  ra = 1.5; rb = 2.5
  ca = "abcd"; cb = "wxyz"
  call troca(na, nb)
  call troca(ra, rb)
  call troca(ca, cb)
  print *, na, nb, ra, rb, ca, " ", cb
  print *, "maior(3,8) =", maior(3, 8), "  maior(2.5,1.5) =", maior(2.5, 1.5)

  print *, "=== class(*) + select type ==="
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
    y = k * x
  end function escalada

end program lambda_delegados_genericos
