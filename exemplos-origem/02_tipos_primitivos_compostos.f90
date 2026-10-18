!===============================================================
! Arquivo : 02_tipos_primitivos_compostos.f90
! Tema    : Tipos de dados primitivos e compostos
! Exemplo : leituras de uma estação meteorológica
! Mostra  : integer, real, double precision, complex, logical,
!           character; vetores, matrizes, índices customizados,
!           arrays dinâmicos (allocatable) e tipos derivados.
!===============================================================
program tipos_primitivos_compostos
  ! Exige que toda variável seja declarada (sem tipagem implícita)
  implicit none

  ! ---------- Tipo composto: tipo derivado (registro) ----------
  ! "type :: leitura" cria um NOVO tipo, parecido com struct em C.
  ! Cada campo (componente) pode ter um valor padrão após o "=".
  type :: leitura
    ! texto de até 20 caracteres, começa como "desconhecido"
    character(len=20) :: sensor      = "desconhecido"
    ! número real (ponto flutuante), começa em 0.0
    real              :: temperatura = 0.0
    ! valor lógico (verdadeiro/falso), começa como falso
    logical           :: valida      = .false.
  end type leitura

  ! ---------- Tipos primitivos ----------
  ! integer: número inteiro; "= 101" já inicializa a variável
  integer           :: id_estacao = 101
  ! real: ponto flutuante de precisão simples (32 bits)
  real              :: umidade    = 63.5
  ! double precision: ponto flutuante de precisão dupla (64 bits).
  ! O "d0" no final do literal indica que o valor é double.
  double precision  :: latitude   = -5.1875d0
  ! complex: par (parte real, parte imaginária)
  complex           :: z          = (3.0, 4.0)
  ! logical: só aceita .true. ou .false. (com pontos ao redor)
  logical           :: ativa      = .true.
  ! character(len=12): texto de tamanho fixo (12); sobra vira espaço
  character(len=12) :: cidade     = "Natal"

  ! ---------- Tipos compostos: arrays ----------
  ! vetor estático de 7 reais; "[...]" é um construtor de array
  real :: temperaturas(7) = [29.1, 30.4, 31.0, 28.7, 27.9, 30.2, 29.5]
  ! matriz 2 linhas x 3 colunas (ainda sem valores)
  real :: matriz(2, 3)
  ! vetor com índices de 0 a 4 (em Fortran o limite inferior é livre);
  ! "= 0" inicializa todos os elementos com zero
  integer :: contagem(0:4) = 0
  ! array dinâmico: "allocatable" = o tamanho só é definido depois,
  ! com allocate. "(:)" indica "uma dimensão, tamanho indefinido".
  real, allocatable :: historico(:)

  ! variável do tipo derivado criado acima
  type(leitura) :: l1
  ! vetor com 3 registros do tipo leitura
  type(leitura) :: lote(3)
  ! contadores de laço e tamanho
  integer :: i, n

  ! Imprime um título
  print *, "=== Primitivos ==="
  ! '(A,I0)': A = texto, I0 = inteiro com o menor número de dígitos
  print '(A,I0)', "Id da estação (integer): ", id_estacao
  ! Imprime o real da umidade
  print *, "Umidade (real):", umidade
  ! Imprime o double precision (mostra mais casas decimais)
  print *, "Latitude (double precision):", latitude
  ! abs(z) calcula o módulo do número complexo (raiz de 3²+4² = 5)
  print *, "Complexo z =", z, " |z| =", abs(z)
  ! Imprime o valor lógico (T ou F)
  print *, "Ativa? (logical):", ativa
  ! trim() remove os espaços que sobraram no fim do texto
  print *, "Cidade (character):", trim(cidade)

  print *, "=== Vetor estático ==="
  ! Imprime o vetor inteiro de uma vez
  print *, "Temperaturas:", temperaturas
  ! sum() soma todos os elementos; size() dá a quantidade de elementos
  print *, "Média:", sum(temperaturas) / size(temperaturas)
  ! maxval() = maior valor; maxloc() = posição (índice) do maior valor
  print *, "Máxima:", maxval(temperaturas), " no dia", maxloc(temperaturas)
  ! "2:4" é uma seção (fatia) do vetor: elementos 2, 3 e 4
  print *, "Dias 2 a 4 (seção):", temperaturas(2:4)

  print *, "=== Matriz (armazenada por colunas) ==="
  ! reshape() transforma o vetor de 6 valores em matriz 2x3.
  ! Fortran preenche coluna por coluna (column-major).
  matriz = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2, 3])
  ! Percorre as 2 linhas
  do i = 1, 2
    ! matriz(i, :) = linha i inteira; '(3F6.1)' = 3 reais, largura 6, 1 casa
    print '(3F6.1)', matriz(i, :)
  end do

  print *, "=== Índices customizados 0:4 ==="
  ! Laço de 0 a 4 (os índices válidos de "contagem")
  do i = 0, 4
    ! guarda o quadrado de i na posição i
    contagem(i) = i * i
  end do
  print *, "contagem(0:4) =", contagem

  print *, "=== Array dinâmico ==="
  ! define o tamanho desejado
  n = 5
  ! allocate reserva memória: agora "historico" tem 5 elementos
  allocate (historico(n))
  ! "[(expr, i = 1, n)]" é um do implícito: gera 1.5, 3.0, 4.5, ...
  historico = [(real(i) * 1.5, i = 1, n)]
  print *, "historico =", historico
  ! deallocate libera a memória reservada
  deallocate (historico)

  print *, "=== Tipo derivado ==="
  ! Construtor de tipo: preenche os 3 campos na ordem em que foram declarados
  l1 = leitura("termometro-1", 29.8, .true.)
  ! "%" acessa um campo (componente) do registro
  print *, "Sensor:", trim(l1%sensor), " Temp:", l1%temperatura, " Válida:", l1%valida

  ! Altera campos do 2º elemento do vetor de registros
  lote(2)%sensor = "pluviometro"
  lote(2)%valida = .true.
  ! "//" concatena textos; o do implícito percorre os 3 elementos do lote.
  ! Os outros dois mantêm o valor padrão "desconhecido".
  print *, "Sensores do lote:", (trim(lote(i)%sensor) // ";", i = 1, 3)
end program tipos_primitivos_compostos
