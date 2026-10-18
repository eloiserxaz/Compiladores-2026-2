!===============================================================
! Arquivo : 04_ponteiros.f90
! Tema    : Ponteiros e referências
! Exemplo : lista encadeada de leituras de temperatura
! Mostra  : pointer/target, "=>", associated, nullify, allocate,
!           ponteiro para seção de vetor, remapeamento de posto
!           e passagem de argumentos por referência.
!===============================================================
module lista_mod
  implicit none

  type :: no
    integer :: valor
    type(no), pointer :: proximo => null()
  end type no

contains

  ! Insere um novo nó no início da lista
  subroutine insere(cabeca, valor)
    type(no), pointer :: cabeca
    integer, intent(in) :: valor
    type(no), pointer :: novo
    allocate (novo)
    novo%valor = valor
    novo%proximo => cabeca
    cabeca => novo
  end subroutine insere

  ! Percorre e imprime a lista
  subroutine imprime(cabeca)
    type(no), pointer :: cabeca
    type(no), pointer :: atual
    atual => cabeca
    do while (associated(atual))
      write (*, '(I0,A)', advance='no') atual%valor, " -> "
      atual => atual%proximo
    end do
    print *, "fim"
  end subroutine imprime

  ! Libera toda a memória da lista
  subroutine libera(cabeca)
    type(no), pointer :: cabeca
    type(no), pointer :: prox
    do while (associated(cabeca))
      prox => cabeca%proximo
      deallocate (cabeca)
      cabeca => prox
    end do
  end subroutine libera

end module lista_mod

program ponteiros
  use lista_mod
  implicit none

  integer, target :: alvo = 10
  integer, pointer :: p => null()
  integer, pointer :: q => null()
  real, target :: vetor(6) = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
  real, pointer :: metade(:) => null()
  real, pointer :: mat(:, :) => null()
  type(no), pointer :: cabeca => null()
  integer :: i, numero

  print *, "=== Ponteiro para variável existente ==="
  p => alvo                      ! "=>" associa (aponta); "=" copia valor
  p = p + 5                      ! altera 'alvo' através do ponteiro
  print *, "alvo =", alvo, " associado?", associated(p, alvo)

  print *, "=== Ponteiro com memória alocada dinamicamente ==="
  allocate (q)
  q = 42
  print *, "q aponta para", q
  deallocate (q)
  nullify (p)
  print *, "p ainda associado?", associated(p)

  print *, "=== Ponteiro para seção de vetor ==="
  metade => vetor(1:3)
  metade = 0.0                   ! zera os 3 primeiros elementos de 'vetor'
  print *, "vetor =", vetor

  print *, "=== Remapeamento: vetor 1D visto como matriz 2x3 ==="
  vetor = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
  mat(1:2, 1:3) => vetor
  print *, "mat(2,3) =", mat(2, 3)

  print *, "=== Lista encadeada ==="
  do i = 1, 5
    call insere(cabeca, i * i)
  end do
  call imprime(cabeca)
  call libera(cabeca)
  print *, "Lista liberada? ", .not. associated(cabeca)

  print *, "=== Passagem por referência (padrão em Fortran) ==="
  numero = 7
  call dobra(numero)
  print *, "numero depois de dobra:", numero

contains

  ! Argumentos são passados por referência: alterações aparecem no chamador
  subroutine dobra(x)
    integer, intent(inout) :: x
    x = 2 * x
  end subroutine dobra

end program ponteiros
