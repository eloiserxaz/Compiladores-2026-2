!===============================================================
! Arquivo : 03_tipos_basicos_padrao.f90
! Tema    : Tipos básicos/padrão e seu formato de implementação
! Mostra  : parâmetro kind, tamanho e faixa de cada tipo,
!           precisão de ponto flutuante, strings, conversões.
! Obs.    : índices de vetores só podem ser integer (nunca real).
!===============================================================
program tipos_basicos_padrao
  ! Importa nomes de "kinds" (tamanhos) do módulo intrínseco da
  ! linguagem: int8 = inteiro de 8 bits, real64 = real de 64 bits, etc.
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, &
                                           real32, real64, real128
  implicit none

  ! "parameter" = constante. selected_real_kind(15, 307) pede um tipo
  ! real com pelo menos 15 dígitos decimais e expoente até 10^307.
  integer, parameter :: dp = selected_real_kind(15, 307)

  ! Inteiro de tamanho padrão (normalmente 32 bits)
  integer          :: padrao = 0
  ! Inteiros de 8, 16 e 64 bits ("_int8" no literal informa o kind)
  integer(int8)    :: i8  = 0_int8
  integer(int16)   :: i16 = 0_int16
  integer(int64)   :: i64 = 0_int64
  ! real padrão: 32 bits (IEEE 754)
  real             :: r   = 0.0
  ! real de 64 bits e de 128 bits
  real(real64)     :: d   = 0.0_real64
  real(real128)    :: q   = 0.0_real128
  ! texto de 10 posições, preenchido com "Fortran" + espaços
  character(len=10) :: s  = "Fortran"
  ! três reais de dupla precisão para mostrar erro de arredondamento
  real(dp)         :: a = 0.1_dp, b = 0.2_dp, c = 0.3_dp

  print *, "=== Inteiros (complemento de dois, com sinal) ==="
  ! kind() informa o número do kind; huge() o maior valor representável
  print *, "kind padrão =", kind(padrao), " maior valor:", huge(padrao)
  ! storage_size() devolve bits; dividido por 8 dá bytes
  print *, "int8  : bytes =", storage_size(i8) / 8, " maior:", huge(i8)
  print *, "int16 : bytes =", storage_size(i16) / 8, " maior:", huge(i16)
  print *, "int64 : bytes =", storage_size(i64) / 8, " maior:", huge(i64)

  print *, "=== Reais (ponto flutuante IEEE 754) ==="
  ! precision() = dígitos decimais confiáveis; epsilon() = menor diferença
  ! perceptível ao somar 1.0 (mede a precisão do tipo)
  print *, "real padrão  : bytes =", storage_size(r) / 8, &
           " dígitos:", precision(r), " epsilon:", epsilon(r)
  print *, "real64       : bytes =", storage_size(d) / 8, &
           " dígitos:", precision(d), " epsilon:", epsilon(d)
  print *, "real128      : bytes =", storage_size(q) / 8, &
           " dígitos:", precision(q)
  ! tiny() = menor real positivo normalizado
  print *, "Maior real padrão:", huge(r), " menor positivo:", tiny(r)

  print *, "=== Erro de arredondamento ==="
  ! 0.1 e 0.2 não têm representação exata em binário: sobra um resíduo
  print *, "0.1 + 0.2 - 0.3 (double) =", a + b - c
  ! Por isso se compara reais com tolerância, nunca com "=="
  print *, "Comparar reais: use tolerância ->", abs(a + b - c) < epsilon(a)

  print *, "=== Character ==="
  ! len() = tamanho declarado; len_trim() = tamanho sem espaços finais
  print *, "len =", len(s), " len_trim =", len_trim(s)
  ! "//" concatena textos
  print *, "Concatenação: [", trim(s) // " 2018", "]"
  ! iachar() = código ASCII do caractere; achar() = caractere do código
  print *, "Código ASCII de 'A':", iachar("A"), " achar(66) =", achar(66)

  print *, "=== Conversões entre tipos ==="
  ! int() trunca (3.9 -> 3); nint() arredonda para o mais próximo (3.9 -> 4)
  print *, "int(3.9) =", int(3.9), " nint(3.9) =", nint(3.9)
  ! 7/2 entre inteiros é divisão inteira (3); real(7)/2 é divisão real (3.5)
  print *, "7/2 =", 7 / 2, "  real(7)/2 =", real(7) / 2
  ! dble() converte para dupla precisão antes de dividir
  print *, "dble(1)/3 =", dble(1) / 3
  ! cmplx(a, b) monta um número complexo a + bi
  print *, "Lógico:", .true., .false., " Complexo:", cmplx(1.0, -2.0)
end program tipos_basicos_padrao
