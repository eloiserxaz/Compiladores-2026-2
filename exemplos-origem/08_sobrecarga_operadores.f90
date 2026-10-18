!===============================================================
! Arquivo : 08_sobrecarga_operadores.f90
! Tema    : Sobrecarga de operadores
! Exemplo : vetores 2D na Física (posição = v0*t + a*t²/2)
! Mostra  : interface operator(+), (-), (*), (==), operador novo
!           definido pelo usuário (.prod.) e assignment(=).
!===============================================================
module vetor_mod
  implicit none
  private
  public :: vetor2d, norma
  public :: operator(+), operator(-), operator(*), operator(==)
  public :: operator(.prod.), assignment(=)

  type :: vetor2d
    real :: x = 0.0
    real :: y = 0.0
  end type vetor2d

  interface operator(+)
    module procedure soma
  end interface

  interface operator(-)
    module procedure subtrai
  end interface

  interface operator(*)
    module procedure vetor_vezes_escalar, escalar_vezes_vetor
  end interface

  interface operator(==)
    module procedure iguais
  end interface

  interface operator(.prod.)          ! operador NOVO: produto escalar
    module procedure produto_escalar
  end interface

  interface assignment(=)             ! vetor = [x, y]
    module procedure atribui_de_array
  end interface

contains

  pure function soma(a, b) result(r)
    type(vetor2d), intent(in) :: a, b
    type(vetor2d) :: r
    r%x = a%x + b%x
    r%y = a%y + b%y
  end function soma

  pure function subtrai(a, b) result(r)
    type(vetor2d), intent(in) :: a, b
    type(vetor2d) :: r
    r%x = a%x - b%x
    r%y = a%y - b%y
  end function subtrai

  pure function vetor_vezes_escalar(a, k) result(r)
    type(vetor2d), intent(in) :: a
    real, intent(in) :: k
    type(vetor2d) :: r
    r%x = a%x * k
    r%y = a%y * k
  end function vetor_vezes_escalar

  pure function escalar_vezes_vetor(k, a) result(r)
    real, intent(in) :: k
    type(vetor2d), intent(in) :: a
    type(vetor2d) :: r
    r = vetor_vezes_escalar(a, k)
  end function escalar_vezes_vetor

  pure function iguais(a, b) result(r)
    type(vetor2d), intent(in) :: a, b
    logical :: r
    r = (a%x == b%x) .and. (a%y == b%y)
  end function iguais

  pure function produto_escalar(a, b) result(r)
    type(vetor2d), intent(in) :: a, b
    real :: r
    r = a%x * b%x + a%y * b%y
  end function produto_escalar

  pure function norma(a) result(r)
    type(vetor2d), intent(in) :: a
    real :: r
    r = sqrt(a%x ** 2 + a%y ** 2)
  end function norma

  subroutine atribui_de_array(v, arr)
    type(vetor2d), intent(out) :: v
    real, intent(in) :: arr(2)
    v%x = arr(1)
    v%y = arr(2)
  end subroutine atribui_de_array

end module vetor_mod

program sobrecarga_operadores
  use vetor_mod
  implicit none

  type(vetor2d) :: v0, acel, pos, f1, f2, resultante
  real :: t

  ! assignment(=) sobrecarregado
  v0 = [3.0, 4.0]          ! velocidade inicial (m/s)
  acel = [0.0, -9.8]       ! aceleração da gravidade (m/s²)
  t = 2.0

  ! Operadores + e * funcionam sobre o novo tipo
  pos = v0 * t + acel * (0.5 * t * t)
  print *, "Posição após", t, "s: (", pos%x, ",", pos%y, ")"

  ! Força resultante: soma de duas forças
  f1 = [10.0, 0.0]
  f2 = [0.0, 5.0]
  resultante = f1 + f2
  print *, "Força resultante: (", resultante%x, ",", resultante%y, ")"
  print *, "Módulo da resultante:", norma(resultante)

  ! Operador novo .prod.
  print *, "f1 .prod. f2 =", f1 .prod. f2, "  (vetores perpendiculares => 0)"

  ! Operador == sobrecarregado
  print *, "f1 == f2 ?", f1 == f2
  print *, "f1 == f1 ?", f1 == f1
  resultante = f1 - f1
  print *, "f1 - f1  = (", resultante%x, ",", resultante%y, ")"
end program sobrecarga_operadores
