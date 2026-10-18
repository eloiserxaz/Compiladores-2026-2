!===============================================================
! Arquivo : 04_ponteiros.f90
! Tema    : Ponteiros e referências
! Exemplo : lista encadeada de leituras de temperatura
! Mostra  : pointer/target, "=>", associated, nullify, allocate,
!           ponteiro para seção de vetor, remapeamento de posto
!           e passagem de argumentos por referência.
!===============================================================

! Módulo = unidade que agrupa tipos e sub-rotinas reutilizáveis
module lista_mod
  implicit none

  ! Um "nó" da lista: guarda um valor e aponta para o próximo nó
  type :: no
    ! dado armazenado no nó
    integer :: valor
    ! ponteiro para outro nó; "=> null()" começa apontando para nada
    type(no), pointer :: proximo => null()
  end type no

! A partir daqui ficam as sub-rotinas do módulo
contains

  ! Insere um novo nó no início da lista
  subroutine insere(cabeca, valor)
    ! "cabeca" é um ponteiro para o primeiro nó da lista
    type(no), pointer :: cabeca
    ! valor a inserir; intent(in) = só é lido, não pode ser alterado
    integer, intent(in) :: valor
    ! ponteiro auxiliar para o nó novo
    type(no), pointer :: novo
    ! cria (aloca) um nó novo na memória
    allocate (novo)
    ! guarda o valor no nó novo
    novo%valor = valor
    ! o próximo do nó novo passa a ser a antiga cabeça da lista
    novo%proximo => cabeca
    ! a cabeça da lista agora é o nó novo
    cabeca => novo
  end subroutine insere

  ! Percorre e imprime a lista
  subroutine imprime(cabeca)
    type(no), pointer :: cabeca
    ! ponteiro que anda pela lista
    type(no), pointer :: atual
    ! começa no primeiro nó
    atual => cabeca
    ! "associated" é verdadeiro enquanto o ponteiro aponta para algo
    do while (associated(atual))
      ! advance='no' = não pula de linha ao escrever
      write (*, '(I0,A)', advance='no') atual%valor, " -> "
      ! anda para o próximo nó
      atual => atual%proximo
    end do
    ! pula a linha e marca o fim
    print *, "fim"
  end subroutine imprime

  ! Libera toda a memória da lista
  subroutine libera(cabeca)
    type(no), pointer :: cabeca
    ! guarda o próximo nó antes de apagar o atual
    type(no), pointer :: prox
    do while (associated(cabeca))
      ! lembra quem é o próximo
      prox => cabeca%proximo
      ! libera o nó atual
      deallocate (cabeca)
      ! avança a cabeça para o próximo
      cabeca => prox
    end do
  end subroutine libera

end module lista_mod

program ponteiros
  ! Traz tudo que o módulo oferece (tipo no, insere, imprime, libera)
  use lista_mod
  implicit none

  ! "target" = variável que PODE ser apontada por um ponteiro
  integer, target :: alvo = 10
  ! ponteiro para inteiro, começa sem apontar para nada
  integer, pointer :: p => null()
  integer, pointer :: q => null()
  ! vetor que pode ser apontado
  real, target :: vetor(6) = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
  ! ponteiro para vetor de tamanho indefinido (1 dimensão)
  real, pointer :: metade(:) => null()
  ! ponteiro para matriz (2 dimensões)
  real, pointer :: mat(:, :) => null()
  ! cabeça da lista encadeada (começa vazia)
  type(no), pointer :: cabeca => null()
  integer :: i, numero

  print *, "=== Ponteiro para variável existente ==="
  ! "=>" faz o ponteiro APONTAR para 'alvo' (não copia valor)
  p => alvo
  ! "p = ..." altera o conteúdo apontado, ou seja, o próprio 'alvo'
  p = p + 5
  ! associated(p, alvo) testa se p aponta exatamente para alvo
  print *, "alvo =", alvo, " associado?", associated(p, alvo)

  print *, "=== Ponteiro com memória alocada dinamicamente ==="
  ! allocate cria uma variável nova na memória e q aponta para ela
  allocate (q)
  q = 42
  print *, "q aponta para", q
  ! libera a memória apontada por q
  deallocate (q)
  ! nullify faz p deixar de apontar para qualquer coisa
  nullify (p)
  print *, "p ainda associado?", associated(p)

  print *, "=== Ponteiro para seção de vetor ==="
  ! 'metade' passa a ser uma "janela" para os elementos 1 a 3 de 'vetor'
  metade => vetor(1:3)
  ! escrever na janela altera o vetor original
  metade = 0.0
  print *, "vetor =", vetor

  print *, "=== Remapeamento: vetor 1D visto como matriz 2x3 ==="
  ! restaura os valores originais
  vetor = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
  ! "mat" enxerga os mesmos 6 números como matriz 2x3 (sem copiar)
  mat(1:2, 1:3) => vetor
  print *, "mat(2,3) =", mat(2, 3)

  print *, "=== Lista encadeada ==="
  ! insere 1, 4, 9, 16 e 25 (sempre no início, por isso saem invertidos)
  do i = 1, 5
    call insere(cabeca, i * i)
  end do
  call imprime(cabeca)
  call libera(cabeca)
  ! após liberar, a cabeça não aponta mais para nada
  print *, "Lista liberada? ", .not. associated(cabeca)

  print *, "=== Passagem por referência (padrão em Fortran) ==="
  numero = 7
  call dobra(numero)
  print *, "numero depois de dobra:", numero

! Sub-rotina interna do programa
contains

  ! Argumentos são passados por referência: alterações aparecem no chamador
  subroutine dobra(x)
    ! intent(inout) = é lido E alterado
    integer, intent(inout) :: x
    x = 2 * x
  end subroutine dobra

end program ponteiros
