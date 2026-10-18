!===============================================================
! Arquivo : 15_coarrays.f90
! Tema    : Programação paralela e distribuída: coarrays
! Modelo  : SPMD. Várias "imagens" executam o mesmo programa;
!           cada uma tem sua cópia dos coarrays e acessa a das
!           outras com a sintaxe variavel[imagem].
! Mostra  : this_image, num_images, sync all, critical,
!           coarray escalar, co_sum e co_broadcast.
! Compilação (uma imagem):  gfortran -fcoarray=single 15_coarrays.f90
! Várias imagens: OpenCoarrays -> caf 15_coarrays.f90 -o c ; cafrun -n 4 ./c
!===============================================================
program coarrays_exemplo
  implicit none

  ! meu_id = número desta imagem; n_imgs = total de imagens
  integer :: meu_id, n_imgs, i
  ! "[*]" torna a variável um COARRAY: cada imagem tem a sua cópia,
  ! e as outras imagens podem ler/escrever nela com parcial[numero]
  real    :: parcial[*]
  integer :: valor[*]
  ! variáveis comuns (locais de cada imagem)
  real    :: total
  integer :: dado

  ! this_image() = quem sou eu (1, 2, 3...); num_images() = quantas existem
  meu_id = this_image()
  n_imgs = num_images()
  print '(A,I0,A,I0)', "Olá da imagem ", meu_id, " de ", n_imgs

  ! Cada imagem soma uma fatia dos números 1..1000 (partição cíclica):
  ! a imagem 1 pega 1, 1+n, 1+2n...; a imagem 2 pega 2, 2+n...
  parcial = 0.0
  do i = meu_id, 1000, n_imgs
    parcial = parcial + real(i)
  end do

  ! "sync all" = barreira: todas as imagens esperam umas pelas outras
  sync all

  ! Só a imagem 1 executa este bloco
  if (meu_id == 1) then
    total = 0.0
    do i = 1, n_imgs
      ! parcial[i] lê o valor guardado na imagem i (leitura remota)
      total = total + parcial[i]
    end do
    print '(A,F0.1)', "Soma 1..1000 (leitura remota) = ", total
  end if

  sync all

  ! Redução coletiva: todas as imagens recebem a soma
  total = parcial
  ! co_sum soma o valor de "total" de todas as imagens e devolve o
  ! resultado em "total" para cada uma
  call co_sum(total)
  print '(A,I0,A,F0.1)', "Imagem ", meu_id, ": co_sum = ", total

  ! Difusão: a imagem 1 envia um valor para todas
  dado = 0
  if (meu_id == 1) dado = 42
  ! co_broadcast copia "dado" da imagem 1 para todas as outras
  call co_broadcast(dado, source_image=1)
  print '(A,I0,A,I0)', "Imagem ", meu_id, " recebeu ", dado

  ! Seção crítica: apenas uma imagem por vez altera valor[1]
  valor = 0
  sync all
  ! "critical" = só uma imagem por vez executa o bloco
  critical
    ! lê e atualiza o valor da imagem 1 sem conflito
    valor[1] = valor[1] + 1
  end critical
  sync all
  if (meu_id == 1) print *, "Contador após seção crítica =", valor

  ! Outros mecanismos do padrão (não usados aqui):
  !   lock/unlock (lock_type), sync images, atomic_define/atomic_ref,
  !   event post/wait (Fortran 2018) e teams (form team/change team).
end program coarrays_exemplo
