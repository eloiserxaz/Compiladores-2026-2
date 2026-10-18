!===============================================================
! Arquivo : 10_condicionais.f90
! Tema    : Comandos condicionais: tipos e implementação
! Exemplo : classificação de IMC, faixa etária, alertas de
!           temperatura
! Mostra  : if lógico, if/else if/else, if nomeado, select case
!           (listas e intervalos), where (condicional em arrays)
!           e merge (escolha "ternária").
!===============================================================
program condicionais
  implicit none

  real    :: peso = 72.0, altura = 1.75, imc
  integer :: idade = 34, dia = 6, i
  character(len=3) :: sigla = "sab"
  real    :: temp(5) = [18.5, 29.0, 33.4, 36.1, 24.0]
  logical :: alerta(5)
  character(len=3) :: resposta

  imc = peso / altura ** 2

  print *, "=== if lógico (uma linha) ==="
  if (imc > 0.0) print *, "IMC calculado:", imc

  print *, "=== if / else if / else ==="
  if (imc < 18.5) then
    print *, "Abaixo do peso"
  else if (imc < 25.0) then
    print *, "Peso normal"
  else if (imc < 30.0) then
    print *, "Sobrepeso"
  else
    print *, "Obesidade"
  end if

  print *, "=== if nomeado ==="
  verifica: if (idade >= 18) then
    print *, "Maior de idade"
  else verifica
    print *, "Menor de idade"
  end if verifica

  print *, "=== select case com intervalos ==="
  select case (idade)
  case (:17)
    print *, "Faixa: menor"
  case (18:64)
    print *, "Faixa: adulto"
  case (65:)
    print *, "Faixa: idoso"
  end select

  print *, "=== select case com lista de valores ==="
  select case (dia)
  case (1)
    print *, "Domingo"
  case (2:6)
    print *, "Dia útil"
  case (7)
    print *, "Sábado"
  case default
    print *, "Dia inválido"
  end select

  print *, "=== select case com character ==="
  select case (sigla)
  case ("sab", "dom")
    print *, "Fim de semana"
  case default
    print *, "Dia de semana"
  end select

  print *, "=== where: condicional sobre arrays inteiros ==="
  where (temp > 30.0)
    alerta = .true.
  elsewhere
    alerta = .false.
  end where
  do i = 1, 5
    print '(F6.1,A,L2)', temp(i), " graus -> alerta:", alerta(i)
  end do

  print *, "=== merge: escolha entre dois valores ==="
  resposta = merge("SIM", "NAO", imc < 25.0)
  print *, "IMC abaixo de 25? ", resposta
end program condicionais
