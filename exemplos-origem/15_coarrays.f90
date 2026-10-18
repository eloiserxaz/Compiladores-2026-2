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

  integer :: meu_id, n_imgs, i
  real    :: parcial[*]       ! um valor por imagem, acessível por todas
  integer :: valor[*]
  real    :: total
  integer :: dado

  meu_id = this_image()
  n_imgs = num_images()
  print '(A,I0,A,I0)', "Olá da imagem ", meu_id, " de ", n_imgs

  ! Cada imagem soma uma fatia dos números 1..1000 (partição cíclica)
  parcial = 0.0
  do i = meu_id, 1000, n_imgs
    parcial = parcial + real(i)
  end do

  sync all                    ! barreira: todas terminam antes de continuar

  if (meu_id == 1) then       ! a imagem 1 lê os valores das outras
    total = 0.0
    do i = 1, n_imgs
      total = total + parcial[i]
    end do
    print '(A,F0.1)', "Soma 1..1000 (leitura remota) = ", total
  end if

  sync all

  ! Redução coletiva: todas as imagens recebem a soma
  total = parcial
  call co_sum(total)
  print '(A,I0,A,F0.1)', "Imagem ", meu_id, ": co_sum = ", total

  ! Difusão: a imagem 1 envia um valor para todas
  dado = 0
  if (meu_id == 1) dado = 42
  call co_broadcast(dado, source_image=1)
  print '(A,I0,A,I0)', "Imagem ", meu_id, " recebeu ", dado

  ! Seção crítica: apenas uma imagem por vez altera valor[1]
  valor = 0
  sync all
  critical
    valor[1] = valor[1] + 1
  end critical
  sync all
  if (meu_id == 1) print *, "Contador após seção crítica =", valor

  ! Outros mecanismos do padrão (não usados aqui):
  !   lock/unlock (lock_type), sync images, atomic_define/atomic_ref,
  !   event post/wait (Fortran 2018) e teams (form team/change team).
end program coarrays_exemplo
