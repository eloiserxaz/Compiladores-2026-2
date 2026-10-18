!===============================================================
! Arquivo : 02_tipos_primitivos_compostos.f90
! Tema    : Tipos de dados primitivos e compostos
! Exemplo : leituras de uma estação meteorológica
! Mostra  : integer, real, double precision, complex, logical,
!           character; vetores, matrizes, índices customizados,
!           arrays dinâmicos (allocatable) e tipos derivados.
!===============================================================
program tipos_primitivos_compostos
  implicit none

  ! ---------- Tipo composto: tipo derivado (registro) ----------
  type :: leitura
    character(len=20) :: sensor      = "desconhecido"
    real              :: temperatura = 0.0
    logical           :: valida      = .false.
  end type leitura

  ! ---------- Tipos primitivos ----------
  integer           :: id_estacao = 101
  real              :: umidade    = 63.5
  double precision  :: latitude   = -5.1875d0
  complex           :: z          = (3.0, 4.0)
  logical           :: ativa      = .true.
  character(len=12) :: cidade     = "Natal"

  ! ---------- Tipos compostos: arrays ----------
  real :: temperaturas(7) = [29.1, 30.4, 31.0, 28.7, 27.9, 30.2, 29.5]
  real :: matriz(2, 3)
  integer :: contagem(0:4) = 0          ! índices de 0 a 4 (limites livres)
  real, allocatable :: historico(:)     ! tamanho definido em execução

  type(leitura) :: l1
  type(leitura) :: lote(3)
  integer :: i, n

  print *, "=== Primitivos ==="
  print '(A,I0)', "Id da estação (integer): ", id_estacao
  print *, "Umidade (real):", umidade
  print *, "Latitude (double precision):", latitude
  print *, "Complexo z =", z, " |z| =", abs(z)
  print *, "Ativa? (logical):", ativa
  print *, "Cidade (character):", trim(cidade)

  print *, "=== Vetor estático ==="
  print *, "Temperaturas:", temperaturas
  print *, "Média:", sum(temperaturas) / size(temperaturas)
  print *, "Máxima:", maxval(temperaturas), " no dia", maxloc(temperaturas)
  print *, "Dias 2 a 4 (seção):", temperaturas(2:4)

  print *, "=== Matriz (armazenada por colunas) ==="
  matriz = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2, 3])
  do i = 1, 2
    print '(3F6.1)', matriz(i, :)
  end do

  print *, "=== Índices customizados 0:4 ==="
  do i = 0, 4
    contagem(i) = i * i
  end do
  print *, "contagem(0:4) =", contagem

  print *, "=== Array dinâmico ==="
  n = 5
  allocate (historico(n))
  historico = [(real(i) * 1.5, i = 1, n)]
  print *, "historico =", historico
  deallocate (historico)

  print *, "=== Tipo derivado ==="
  l1 = leitura("termometro-1", 29.8, .true.)
  print *, "Sensor:", trim(l1%sensor), " Temp:", l1%temperatura, " Válida:", l1%valida

  lote(2)%sensor = "pluviometro"
  lote(2)%valida = .true.
  print *, "Sensores do lote:", (trim(lote(i)%sensor) // ";", i = 1, 3)
end program tipos_primitivos_compostos
