!===============================================================
! Arquivo : 14_concorrencia_openmp.f90
! Tema    : Concorrência, paralelismo e E/S assíncrona
! Mostra  :
!  - do concurrent (paralelismo previsto no padrão da linguagem);
!  - OpenMP via diretivas (!$omp): redução, região paralela,
!    seção crítica e operação atômica;
!  - E/S assíncrona (asynchronous / wait).
! Compilação: gfortran -fopenmp 14_concorrencia_openmp.f90
!   Sem -fopenmp as linhas "!$" são comentários e o programa
!   roda de forma sequencial, com o mesmo resultado.
!===============================================================
program concorrencia_openmp
! A linha abaixo só é compilada com -fopenmp (prefixo "!$" especial):
! ela importa as funções da biblioteca OpenMP, como omp_get_thread_num.
!$ use omp_lib
  implicit none

  ! dp = kind de dupla precisão
  integer, parameter :: dp = kind(1.0d0)
  ! n = tamanho do vetor (2 milhões de elementos)
  integer, parameter :: n = 2000000
  ! vetor dinâmico e variável da soma
  real(dp), allocatable :: a(:)
  real(dp) :: soma
  ! contador, unidade de arquivo e código de erro
  integer :: i, contador, u, ios
  ! "asynchronous" = este vetor pode ser usado por E/S assíncrona
  real, asynchronous :: buffer(1000)
  ! texto de mensagem de erro
  character(len=100) :: msg

  ! reserva memória para 2 milhões de reais
  allocate (a(n))

  print *, "=== do concurrent: iterações independentes ==="
  ! Cada iteração é independente: o compilador pode paralelizar
  do concurrent (i = 1:n)
    a(i) = sin(real(i, dp)) ** 2
  end do
  print *, "a(1) =", a(1)

  print *, "=== OpenMP: redução (soma paralela) ==="
  soma = 0.0_dp
  ! "parallel do" divide as iterações entre as threads.
  ! reduction(+:soma): cada thread soma o seu pedaço e, no fim, os
  ! resultados parciais são somados (evita conflito na variável soma).
  !$omp parallel do reduction(+:soma)
  do i = 1, n
    soma = soma + a(i)
  end do
  !$omp end parallel do
  ! F0.3 = real com 3 casas decimais e sem espaços sobrando
  print '(A,F0.3)', "Soma dos senos ao quadrado: ", soma

  print *, "=== OpenMP: região paralela + seção crítica ==="
  ! "parallel" cria várias threads que executam o mesmo bloco.
  ! "critical" faz uma thread por vez entrar (para o print não misturar).
  ! A linha com "!$ print" só existe quando compilado com -fopenmp.
  !$omp parallel
  !$omp critical
  !$ print '(A,I0,A,I0)', "  thread ", omp_get_thread_num(), " de ", omp_get_num_threads()
  !$omp end critical
  !$omp end parallel
  !$ print *, "(compilado com OpenMP)"

  print *, "=== OpenMP: operação atômica ==="
  contador = 0
  ! Várias threads somam 1 ao mesmo contador. "atomic" garante que cada
  ! soma aconteça por completo, sem uma thread atrapalhar a outra.
  !$omp parallel do
  do i = 1, 100000
    !$omp atomic
    contador = contador + 1
  end do
  !$omp end parallel do
  print *, "contador =", contador, " (esperado: 100000)"

  print *, "=== E/S assíncrona ==="
  ! preenche o buffer com 1.0, 2.0, ..., 1000.0
  buffer = [(real(i), i = 1, 1000)]
  ! Abre um arquivo binário para escrita assíncrona.
  ! newunit=u: o sistema escolhe o número da unidade de arquivo.
  ! iostat guarda o código de erro (0 = sucesso); iomsg guarda o texto.
  open (newunit=u, file="saida_async.tmp", form="unformatted", &
        access="stream", status="replace", asynchronous="yes", &
        iostat=ios, iomsg=msg)
  ! se não conseguiu abrir, mostra o erro e encerra com código 1
  if (ios /= 0) then
    print *, "Falha ao abrir arquivo: ", trim(msg)
    stop 1
  end if
  ! começa a gravar; o programa pode continuar sem esperar terminar
  write (u, asynchronous="yes") buffer
  print *, "calculando enquanto o arquivo é gravado..."
  ! "wait" bloqueia até a gravação terminar (sincronização)
  wait (u)
  ! fecha e apaga o arquivo temporário
  close (u, status="delete")
  print *, "E/S assíncrona concluída"
end program concorrencia_openmp
