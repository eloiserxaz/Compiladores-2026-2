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
  ! Importa o tipo inteiro de 64 bits (usado para o fatorial)
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none
  ! constante pi compartilhada por todo o módulo
  real, parameter :: pi = 3.14159265358979
contains

  ! Função pura: sem efeitos colaterais.
  ! "result(a)" dá nome à variável que carrega o valor devolvido.
  pure function area_circulo(r) result(a)
    real, intent(in) :: r
    real :: a
    a = pi * r ** 2
  end function area_circulo

  ! Função recursiva: chama a si mesma (precisa da palavra "recursive")
  recursive function fatorial(n) result(f)
    integer, intent(in) :: n
    integer(int64) :: f
    ! caso base: 0! = 1! = 1 (encerra a recursão)
    if (n <= 1) then
      f = 1_int64
    else
      ! caso recursivo: n! = n * (n-1)!
      f = n * fatorial(n - 1)
    end if
  end function fatorial

  ! Função elemental: escrita para escalar, aplicável a arrays
  ! (aplica a conta a cada elemento automaticamente)
  elemental function celsius_para_f(c) result(f)
    real, intent(in) :: c
    real :: f
    f = c * 9.0 / 5.0 + 32.0
  end function celsius_para_f

  ! Vários resultados via intent(out); array de forma assumida (:)
  ! e argumento opcional (desvio)
  subroutine estatisticas(v, media, desvio)
    ! "v(:)" aceita vetor de qualquer tamanho; o tamanho vem junto
    real, intent(in) :: v(:)
    ! intent(out) = valor devolvido ao chamador
    real, intent(out) :: media
    ! "optional" = o chamador pode omitir este argumento
    real, intent(out), optional :: desvio
    media = sum(v) / size(v)
    ! present() testa se o argumento opcional foi passado
    if (present(desvio)) desvio = sqrt(sum((v - media) ** 2) / size(v))
  end subroutine estatisticas

  ! Quociente e resto: dois resultados
  subroutine divmod(a, b, q, r)
    integer, intent(in) :: a, b
    integer, intent(out) :: q, r
    ! divisão inteira = quociente
    q = a / b
    ! mod() = resto da divisão
    r = mod(a, b)
  end subroutine divmod

  ! Passagem por REFERÊNCIA (padrão): altera a variável do chamador
  subroutine incrementa_ref(x)
    integer, intent(inout) :: x
    x = x + 1
  end subroutine incrementa_ref

  ! Passagem por VALOR: o chamador não é afetado
  subroutine incrementa_valor(x)
    ! "value" = recebe uma CÓPIA do valor
    integer, value :: x
    x = x + 100
    print *, "  (dentro) cópia local x =", x
  end subroutine incrementa_valor

  ! Formata um real; 'casas' é opcional (padrão: 2)
  function formata(valor, casas) result(txt)
    real, intent(in) :: valor
    integer, intent(in), optional :: casas
    character(len=20) :: txt
    ! fmt guarda o formato montado em tempo de execução, ex.: "(F12.4)"
    character(len=12) :: fmt
    integer :: c
    ! valor padrão: 2 casas decimais
    c = 2
    ! se o argumento foi passado, usa o valor dele
    if (present(casas)) c = casas
    ! escreve DENTRO de uma variável de texto (escrita interna)
    write (fmt, '(A,I0,A)') '(F12.', c, ')'
    ! usa o formato recém-montado para converter o número em texto
    write (txt, fmt) valor
    ! adjustl() empurra o texto para a esquerda (remove espaços iniciais)
    txt = adjustl(txt)
  end function formata

end module subprogramas_mod

program subprogramas
  ! Traz tudo que está no módulo
  use subprogramas_mod
  implicit none

  real :: dados(5) = [8.0, 9.5, 7.0, 6.5, 10.0]
  real :: m, dp
  integer :: n, q, r

  print *, "Área do círculo r=2:", area_circulo(2.0)
  print *, "20! =", fatorial(20)

  ! Passa um vetor para a função elemental: ela é aplicada a cada elemento
  print *, "Celsius -> Fahrenheit (elemental, vetor):", &
           celsius_para_f([0.0, 25.0, 100.0])

  ! Chamada sem o argumento opcional "desvio"
  call estatisticas(dados, m)
  print *, "Média:", m
  ! Argumentos NOMEADOS: nome = valor (a ordem deixa de importar)
  call estatisticas(v=dados, media=m, desvio=dp)
  print *, "Média:", m, " Desvio padrão:", dp

  call divmod(17, 5, q, r)
  print *, "17 / 5 -> quociente", q, "resto", r

  n = 1
  ! por referência: n vira 2
  call incrementa_ref(n)
  print *, "Após incrementa_ref  : n =", n
  ! por valor: a sub-rotina altera só a cópia; n continua 2
  call incrementa_valor(n)
  print *, "Após incrementa_valor: n =", n, " (inalterado)"

  ! sem o 2º argumento usa 2 casas; com 4 usa 4 casas
  print *, "formata(3.14159)    = [", trim(formata(3.14159)), "]"
  print *, "formata(3.14159, 4) = [", trim(formata(3.14159, 4)), "]"
  ! dados(2:4) = fatia com 3 elementos (9.5, 7.0, 6.5)
  print *, "Chamada com subseção: média de dados(2:4) ="
  call estatisticas(dados(2:4), m)
  print *, m
end program subprogramas
