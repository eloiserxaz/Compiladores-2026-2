!===============================================================
! Arquivo : 03_tipos_basicos_padrao.f90
! Tema    : Tipos básicos/padrão e seu formato de implementação
! Mostra  : parâmetro kind, tamanho e faixa de cada tipo,
!           precisão de ponto flutuante, strings, conversões.
! Obs.    : índices de vetores só podem ser integer (nunca real).
!===============================================================
program tipos_basicos_padrao
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, &
                                           real32, real64, real128
  implicit none

  integer, parameter :: dp = selected_real_kind(15, 307)  ! >= 15 dígitos

  integer          :: padrao = 0          ! kind padrão (normalmente 32 bits)
  integer(int8)    :: i8  = 0_int8
  integer(int16)   :: i16 = 0_int16
  integer(int64)   :: i64 = 0_int64
  real             :: r   = 0.0           ! padrão: 32 bits (IEEE)
  real(real64)     :: d   = 0.0_real64
  real(real128)    :: q   = 0.0_real128
  character(len=10) :: s  = "Fortran"
  real(dp)         :: a = 0.1_dp, b = 0.2_dp, c = 0.3_dp

  print *, "=== Inteiros (complemento de dois, com sinal) ==="
  print *, "kind padrão =", kind(padrao), " maior valor:", huge(padrao)
  print *, "int8  : bytes =", storage_size(i8) / 8, " maior:", huge(i8)
  print *, "int16 : bytes =", storage_size(i16) / 8, " maior:", huge(i16)
  print *, "int64 : bytes =", storage_size(i64) / 8, " maior:", huge(i64)

  print *, "=== Reais (ponto flutuante IEEE 754) ==="
  print *, "real padrão  : bytes =", storage_size(r) / 8, &
           " dígitos:", precision(r), " epsilon:", epsilon(r)
  print *, "real64       : bytes =", storage_size(d) / 8, &
           " dígitos:", precision(d), " epsilon:", epsilon(d)
  print *, "real128      : bytes =", storage_size(q) / 8, &
           " dígitos:", precision(q)
  print *, "Maior real padrão:", huge(r), " menor positivo:", tiny(r)

  print *, "=== Erro de arredondamento ==="
  print *, "0.1 + 0.2 - 0.3 (double) =", a + b - c
  print *, "Comparar reais: use tolerância ->", abs(a + b - c) < epsilon(a)

  print *, "=== Character ==="
  print *, "len =", len(s), " len_trim =", len_trim(s)
  print *, "Concatenação: [", trim(s) // " 2018", "]"
  print *, "Código ASCII de 'A':", iachar("A"), " achar(66) =", achar(66)

  print *, "=== Conversões entre tipos ==="
  print *, "int(3.9) =", int(3.9), " nint(3.9) =", nint(3.9)
  print *, "7/2 =", 7 / 2, "  real(7)/2 =", real(7) / 2
  print *, "dble(1)/3 =", dble(1) / 3
  print *, "Lógico:", .true., .false., " Complexo:", cmplx(1.0, -2.0)
end program tipos_basicos_padrao
