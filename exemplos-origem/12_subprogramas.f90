!===============================================================
! Arquivo : 12_subprogramas.f90
! Tema    : Subprogramas: declaração, chamada e passagem de
!           parâmetros
! Mostra  : function/subroutine, intent(in/out/inout), passagem
!           por referência (padrão) e por valor (value),
!           argumentos opcionais e nomeados, recursão, funções
!           elementais e arrays de forma assumida.
!===============================================================
module subprogramas_mod
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none
  real, parameter :: pi = 3.14159265358979
contains

  ! Função pura: sem efeitos colaterais
  pure function area_circulo(r) result(a)
    real, intent(in) :: r
    real :: a
    a = pi * r ** 2
  end function area_circulo

  ! Função recursiva
  recursive function fatorial(n) result(f)
    integer, intent(in) :: n
    integer(int64) :: f
    if (n <= 1) then
      f = 1_int64
    else
      f = n * fatorial(n - 1)
    end if
  end function fatorial

  ! Função elemental: escrita para escalar, aplicável a arrays
  elemental function celsius_para_f(c) result(f)
    real, intent(in) :: c
    real :: f
    f = c * 9.0 / 5.0 + 32.0
  end function celsius_para_f

  ! Vários resultados via intent(out); array de forma assumida (:)
  ! e argumento opcional (desvio)
  subroutine estatisticas(v, media, desvio)
    real, intent(in) :: v(:)
    real, intent(out) :: media
    real, intent(out), optional :: desvio
    media = sum(v) / size(v)
    if (present(desvio)) desvio = sqrt(sum((v - media) ** 2) / size(v))
  end subroutine estatisticas

  ! Quociente e resto: dois resultados
  subroutine divmod(a, b, q, r)
    integer, intent(in) :: a, b
    integer, intent(out) :: q, r
    q = a / b
    r = mod(a, b)
  end subroutine divmod

  ! Passagem por REFERÊNCIA (padrão): altera a variável do chamador
  subroutine incrementa_ref(x)
    integer, intent(inout) :: x
    x = x + 1
  end subroutine incrementa_ref

  ! Passagem por VALOR: o chamador não é afetado
  subroutine incrementa_valor(x)
    integer, value :: x
    x = x + 100
    print *, "  (dentro) cópia local x =", x
  end subroutine incrementa_valor

  ! Formata um real; 'casas' é opcional (padrão: 2)
  function formata(valor, casas) result(txt)
    real, intent(in) :: valor
    integer, intent(in), optional :: casas
    character(len=20) :: txt
    character(len=12) :: fmt
    integer :: c
    c = 2
    if (present(casas)) c = casas
    write (fmt, '(A,I0,A)') '(F12.', c, ')'
    write (txt, fmt) valor
    txt = adjustl(txt)
  end function formata

end module subprogramas_mod

program subprogramas
  use subprogramas_mod
  implicit none

  real :: dados(5) = [8.0, 9.5, 7.0, 6.5, 10.0]
  real :: m, dp
  integer :: n, q, r

  print *, "Área do círculo r=2:", area_circulo(2.0)
  print *, "20! =", fatorial(20)

  print *, "Celsius -> Fahrenheit (elemental, vetor):", &
           celsius_para_f([0.0, 25.0, 100.0])

  call estatisticas(dados, m)                    ! sem o argumento opcional
  print *, "Média:", m
  call estatisticas(v=dados, media=m, desvio=dp) ! argumentos NOMEADOS
  print *, "Média:", m, " Desvio padrão:", dp

  call divmod(17, 5, q, r)
  print *, "17 / 5 -> quociente", q, "resto", r

  n = 1
  call incrementa_ref(n)
  print *, "Após incrementa_ref  : n =", n
  call incrementa_valor(n)
  print *, "Após incrementa_valor: n =", n, " (inalterado)"

  print *, "formata(3.14159)    = [", trim(formata(3.14159)), "]"
  print *, "formata(3.14159, 4) = [", trim(formata(3.14159, 4)), "]"
  print *, "Chamada com subseção: média de dados(2:4) ="
  call estatisticas(dados(2:4), m)
  print *, m
end program subprogramas
