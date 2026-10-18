!===============================================================
! Arquivo : 16_excecoes.f90
! Tema    : Tratamento de exceções e erros
! Observação: Fortran NÃO possui try/catch nem exceções como
!   objetos. O tratamento é feito por:
!   (1) códigos de status em comandos (iostat/iomsg, stat/errmsg);
!   (2) flags de exceção de ponto flutuante (módulos IEEE);
!   (3) códigos de erro próprios, propagados pelos argumentos
!       (equivalente a "lançar" e "delegar" o erro);
!   (4) erros fatais/intratáveis: error stop, stop e falhas de
!       execução (ex.: limites de array com -fcheck=bounds).
! Compile SEM otimização (-O0) para ver as flags IEEE.
!===============================================================
module erros_mod
  use, intrinsic :: iso_fortran_env, only: error_unit, int64
  implicit none

  ! "Novos tipos de exceção": códigos de erro definidos pelo usuário
  integer, parameter :: ERR_OK = 0, ERR_ARQUIVO = 10, ERR_CONVERSAO = 20, &
                        ERR_MEMORIA = 30, ERR_NEGATIVO = 40

  type :: erro_t
    integer :: codigo = ERR_OK
    character(len=120) :: mensagem = ""
  end type erro_t

contains

  subroutine abre_arquivo(nome, unidade, e)
    character(len=*), intent(in) :: nome
    integer, intent(out) :: unidade
    type(erro_t), intent(out) :: e
    integer :: ios
    character(len=120) :: msg
    open (newunit=unidade, file=nome, status="old", action="read", &
          iostat=ios, iomsg=msg)
    if (ios /= 0) then                       ! erro TRATÁVEL: não aborta
      e%codigo = ERR_ARQUIVO
      e%mensagem = "arquivo: " // trim(msg)
    end if
  end subroutine abre_arquivo

  subroutine converte_inteiro(txt, valor, e)
    character(len=*), intent(in) :: txt
    integer, intent(out) :: valor
    type(erro_t), intent(out) :: e
    integer :: ios
    character(len=120) :: msg
    valor = 0
    read (txt, *, iostat=ios, iomsg=msg) valor
    if (ios /= 0) then
      e%codigo = ERR_CONVERSAO
      e%mensagem = "conversão de '" // trim(txt) // "' falhou: " // trim(msg)
    end if
  end subroutine converte_inteiro

  subroutine raiz_segura(x, r, e)
    real, intent(in) :: x
    real, intent(out) :: r
    type(erro_t), intent(out) :: e
    r = 0.0
    if (x < 0.0) then
      e%codigo = ERR_NEGATIVO
      e%mensagem = "raiz quadrada de número negativo"
      return
    end if
    r = sqrt(x)
  end subroutine raiz_segura

  ! Nível intermediário: não trata, DELEGA o erro a quem chamou
  subroutine processa_texto(txt, resultado, e)
    character(len=*), intent(in) :: txt
    real, intent(out) :: resultado
    type(erro_t), intent(out) :: e
    integer :: n
    resultado = 0.0
    call converte_inteiro(txt, n, e)
    if (e%codigo /= ERR_OK) return           ! propaga o erro
    call raiz_segura(real(n), resultado, e)
  end subroutine processa_texto

  ! Exceções de ponto flutuante: divisão por zero e operação inválida
  subroutine demo_ieee()
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: ieee_exceptions
    real, volatile :: zero, neg
    real :: x, y
    logical :: flag
    zero = 0.0
    neg = -1.0
    ! Por padrão a execução continua e apenas marca flags
    call ieee_set_halting_mode(ieee_divide_by_zero, .false.)
    call ieee_set_halting_mode(ieee_invalid, .false.)
    call ieee_set_flag(ieee_divide_by_zero, .false.)
    call ieee_set_flag(ieee_invalid, .false.)

    x = 1.0 / zero
    call ieee_get_flag(ieee_divide_by_zero, flag)
    print *, "1/0 = ", x, " | flag divisão por zero:", flag, &
             " | finito?", ieee_is_finite(x)

    y = sqrt(neg)
    call ieee_get_flag(ieee_invalid, flag)
    print *, "sqrt(-1) =", y, " | flag operação inválida:", flag, &
             " | NaN?", ieee_is_nan(y)
  end subroutine demo_ieee

end module erros_mod

program excecoes
  use erros_mod
  implicit none

  logical, parameter :: demonstrar_erro_fatal = .false.
  type(erro_t) :: e
  integer :: u, ios
  real :: r
  integer(int64) :: n_enorme
  real(8), allocatable :: v(:)
  character(len=120) :: msg
  character(len=8) :: entrada = "10"
  character(len=12) :: textos(3) = [character(len=12) :: "16", "abc", "-9"]
  integer :: i, a, b

  print *, "=== 1. Arquivo inexistente (iostat/iomsg) ==="
  call abre_arquivo("arquivo_que_nao_existe.txt", u, e)
  if (e%codigo /= ERR_OK) print *, "Erro", e%codigo, ":", trim(e%mensagem)

  print *, "=== 2. Erros delegados por vários níveis ==="
  do i = 1, 3
    call processa_texto(trim(textos(i)), r, e)
    select case (e%codigo)
    case (ERR_OK)
      print *, "'", trim(textos(i)), "' -> raiz =", r
    case (ERR_CONVERSAO, ERR_NEGATIVO)
      print *, "Tratado em main (código", e%codigo, "): ", trim(e%mensagem)
    case default
      print *, "Erro inesperado"
    end select
  end do

  print *, "=== 3. Fim de arquivo/dados (iostat_end) ==="
  read (entrada, *, iostat=ios) a, b      ! só há um valor: falta o segundo
  if (is_iostat_end(ios)) print *, "Fim dos dados antes do esperado (ios =", ios, ")"

  print *, "=== 4. Falha de alocação (stat/errmsg) ==="
  n_enorme = 10_int64 ** 15
  allocate (v(n_enorme), stat=ios, errmsg=msg)
  if (ios /= 0) then
    print *, "Não foi possível alocar: ", trim(msg)
  else
    deallocate (v)
  end if

  print *, "=== 5. Exceções IEEE de ponto flutuante ==="
  call demo_ieee()

  print *, "=== 6. Erro intratável: error stop ==="
  ! Erros como acesso fora dos limites (-fcheck=bounds), divisão
  ! inteira por zero ou error stop encerram o programa: não há
  ! como "capturar" em Fortran.
  if (demonstrar_erro_fatal) then
    write (error_unit, '(A)') "Erro fatal simulado"
    error stop 2                          ! termina com código de saída 2
  end if
  print *, "(altere demonstrar_erro_fatal para .true. para ver o error stop)"
end program excecoes
