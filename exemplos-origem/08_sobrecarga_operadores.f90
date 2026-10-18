!===============================================================
! Arquivo : 08_sobrecarga_operadores.f90
! Tema    : Sobrecarga de operadores
! Exemplo : vetores 2D na Física (posição = v0*t + a*t²/2)
! Mostra  : interface operator(+), (-), (*), (==), operador novo
!           definido pelo usuário (.prod.) e assignment(=).
!===============================================================
module vetor_mod
  implicit none
  ! Por padrão tudo no módulo fica escondido (private)...
  private
  ! ...e só o que está listado em "public" pode ser usado de fora
  public :: vetor2d, norma
  public :: operator(+), operator(-), operator(*), operator(==)
  public :: operator(.prod.), assignment(=)

  ! Tipo novo: vetor com duas componentes, ambas começam em 0.0
  type :: vetor2d
    real :: x = 0.0
    real :: y = 0.0
  end type vetor2d

  ! "interface operator(+)" liga o símbolo + à função "soma"
  ! quando os operandos forem do tipo vetor2d
  interface operator(+)
    module procedure soma
  end interface

  interface operator(-)
    module procedure subtrai
  end interface

  ! O operador * tem duas versões: vetor*número e número*vetor
  interface operator(*)
    module procedure vetor_vezes_escalar, escalar_vezes_vetor
  end interface

  interface operator(==)
    module procedure iguais
  end interface

  ! Operador NOVO, criado pelo programador: nome entre pontos
  interface operator(.prod.)          ! produto escalar
    module procedure produto_escalar
  end interface

  ! Sobrecarga da atribuição: permite escrever vetor = [x, y]
  interface assignment(=)
    module procedure atribui_de_array
  end interface

contains

  ! "pure" = função sem efeitos colaterais (só calcula e devolve)
  ! Soma componente a componente: (a.x+b.x, a.y+b.y)
  pure function soma(a, b) result(r)
    type(vetor2d), intent(in) :: a, b
    type(vetor2d) :: r
    r%x = a%x + b%x
    r%y = a%y + b%y
  end function soma

  ! Subtração componente a componente
  pure function subtrai(a, b) result(r)
    type(vetor2d), intent(in) :: a, b
    type(vetor2d) :: r
    r%x = a%x - b%x
    r%y = a%y - b%y
  end function subtrai

  ! vetor * número: multiplica cada componente pelo número k
  pure function vetor_vezes_escalar(a, k) result(r)
    type(vetor2d), intent(in) :: a
    real, intent(in) :: k
    type(vetor2d) :: r
    r%x = a%x * k
    r%y = a%y * k
  end function vetor_vezes_escalar

  ! número * vetor: reaproveita a função acima (a ordem não importa)
  pure function escalar_vezes_vetor(k, a) result(r)
    real, intent(in) :: k
    type(vetor2d), intent(in) :: a
    type(vetor2d) :: r
    r = vetor_vezes_escalar(a, k)
  end function escalar_vezes_vetor

  ! Compara dois vetores: iguais se x e y forem iguais
  pure function iguais(a, b) result(r)
    type(vetor2d), intent(in) :: a, b
    logical :: r
    r = (a%x == b%x) .and. (a%y == b%y)
  end function iguais

  ! Produto escalar: x1*x2 + y1*y2 (devolve um número real)
  pure function produto_escalar(a, b) result(r)
    type(vetor2d), intent(in) :: a, b
    real :: r
    r = a%x * b%x + a%y * b%y
  end function produto_escalar

  ! Comprimento (módulo) do vetor: raiz de x² + y²
  pure function norma(a) result(r)
    type(vetor2d), intent(in) :: a
    real :: r
    r = sqrt(a%x ** 2 + a%y ** 2)
  end function norma

  ! Atribuição de um array de 2 reais a um vetor2d.
  ! Em atribuição sobrecarregada: 1º argumento é o lado esquerdo
  ! (intent out) e o 2º é o lado direito (intent in).
  subroutine atribui_de_array(v, arr)
    type(vetor2d), intent(out) :: v
    real, intent(in) :: arr(2)
    v%x = arr(1)
    v%y = arr(2)
  end subroutine atribui_de_array

end module vetor_mod

program sobrecarga_operadores
  ! Importa o que o módulo deixou público
  use vetor_mod
  implicit none

  ! v0 = velocidade inicial, acel = aceleração, pos = posição
  ! f1, f2 = forças, resultante = soma das forças
  type(vetor2d) :: v0, acel, pos, f1, f2, resultante
  ! tempo em segundos
  real :: t

  ! assignment(=) sobrecarregado: usa a sub-rotina atribui_de_array
  v0 = [3.0, 4.0]          ! velocidade inicial (m/s)
  acel = [0.0, -9.8]       ! aceleração da gravidade (m/s²)
  t = 2.0

  ! Operadores + e * funcionam sobre o novo tipo.
  ! Fórmula da física: posição = v0*t + a*t²/2
  pos = v0 * t + acel * (0.5 * t * t)
  print *, "Posição após", t, "s: (", pos%x, ",", pos%y, ")"

  ! Força resultante: soma de duas forças (usa o + sobrecarregado)
  f1 = [10.0, 0.0]
  f2 = [0.0, 5.0]
  resultante = f1 + f2
  print *, "Força resultante: (", resultante%x, ",", resultante%y, ")"
  ! norma() dá o comprimento do vetor: raiz de 10² + 5² ≈ 11.18
  print *, "Módulo da resultante:", norma(resultante)

  ! Operador novo .prod.: produto escalar (perpendiculares dão 0)
  print *, "f1 .prod. f2 =", f1 .prod. f2, "  (vetores perpendiculares => 0)"

  ! Operador == sobrecarregado
  print *, "f1 == f2 ?", f1 == f2
  print *, "f1 == f1 ?", f1 == f1
  ! Subtração usa o operador - sobrecarregado
  resultante = f1 - f1
  print *, "f1 - f1  = (", resultante%x, ",", resultante%y, ")"
end program sobrecarga_operadores
