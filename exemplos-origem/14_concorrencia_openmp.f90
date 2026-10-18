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
!$ use omp_lib
  implicit none

  integer, parameter :: dp = kind(1.0d0)
  integer, parameter :: n = 2000000
  real(dp), allocatable :: a(:)
  real(dp) :: soma
  integer :: i, contador, u, ios
  real, asynchronous :: buffer(1000)
  character(len=100) :: msg

  allocate (a(n))

  print *, "=== do concurrent: iterações independentes ==="
  do concurrent (i = 1:n)
    a(i) = sin(real(i, dp)) ** 2
  end do
  print *, "a(1) =", a(1)

  print *, "=== OpenMP: redução (soma paralela) ==="
  soma = 0.0_dp
  !$omp parallel do reduction(+:soma)
  do i = 1, n
    soma = soma + a(i)
  end do
  !$omp end parallel do
  print '(A,F0.3)', "Soma dos senos ao quadrado: ", soma

  print *, "=== OpenMP: região paralela + seção crítica ==="
  !$omp parallel
  !$omp critical
  !$ print '(A,I0,A,I0)', "  thread ", omp_get_thread_num(), " de ", omp_get_num_threads()
  !$omp end critical
  !$omp end parallel
  !$ print *, "(compilado com OpenMP)"

  print *, "=== OpenMP: operação atômica ==="
  contador = 0
  !$omp parallel do
  do i = 1, 100000
    !$omp atomic
    contador = contador + 1
  end do
  !$omp end parallel do
  print *, "contador =", contador, " (esperado: 100000)"

  print *, "=== E/S assíncrona ==="
  buffer = [(real(i), i = 1, 1000)]
  open (newunit=u, file="saida_async.tmp", form="unformatted", &
        access="stream", status="replace", asynchronous="yes", &
        iostat=ios, iomsg=msg)
  if (ios /= 0) then
    print *, "Falha ao abrir arquivo: ", trim(msg)
    stop 1
  end if
  write (u, asynchronous="yes") buffer      ! pode retornar antes de terminar
  print *, "calculando enquanto o arquivo é gravado..."
  wait (u)                                  ! sincroniza: espera a E/S terminar
  close (u, status="delete")
  print *, "E/S assíncrona concluída"
end program concorrencia_openmp
