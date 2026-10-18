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

  ! dados de uma pessoa: peso (kg), altura (m) e o IMC calculado
  real    :: peso = 72.0, altura = 1.75, imc
  ! idade, dia da semana (1 = domingo) e contador
  integer :: idade = 34, dia = 6, i
  ! sigla de dia da semana com 3 letras
  character(len=3) :: sigla = "sab"
  ! temperaturas de 5 medições e vetor de alertas correspondente
  real    :: temp(5) = [18.5, 29.0, 33.4, 36.1, 24.0]
  logical :: alerta(5)
  ! resposta "SIM"/"NAO"
  character(len=3) :: resposta

  ! IMC = peso / altura ao quadrado
  imc = peso / altura ** 2

  print *, "=== if lógico (uma linha) ==="
  ! forma curta: sem "then" nem "end if", executa um só comando
  if (imc > 0.0) print *, "IMC calculado:", imc

  print *, "=== if / else if / else ==="
  ! Testa as condições em ordem e executa só o primeiro bloco verdadeiro
  if (imc < 18.5) then
    print *, "Abaixo do peso"
  else if (imc < 25.0) then
    print *, "Peso normal"
  else if (imc < 30.0) then
    print *, "Sobrepeso"
  else
    ! executa se nenhuma condição acima foi verdadeira
    print *, "Obesidade"
  end if

  print *, "=== if nomeado ==="
  ! O nome "verifica" ajuda a ler blocos longos e repete no else/end if
  verifica: if (idade >= 18) then
    print *, "Maior de idade"
  else verifica
    print *, "Menor de idade"
  end if verifica

  print *, "=== select case com intervalos ==="
  ! select case compara um valor com várias opções (case)
  select case (idade)
  ! "(:17)" = qualquer valor até 17
  case (:17)
    print *, "Faixa: menor"
  ! "(18:64)" = de 18 a 64
  case (18:64)
    print *, "Faixa: adulto"
  ! "(65:)" = 65 ou mais
  case (65:)
    print *, "Faixa: idoso"
  end select

  print *, "=== select case com lista de valores ==="
  select case (dia)
  case (1)
    print *, "Domingo"
  ! intervalo de 2 a 6
  case (2:6)
    print *, "Dia útil"
  case (7)
    print *, "Sábado"
  ! "case default" cobre qualquer valor não listado
  case default
    print *, "Dia inválido"
  end select

  print *, "=== select case com character ==="
  select case (sigla)
  ! lista de valores separados por vírgula
  case ("sab", "dom")
    print *, "Fim de semana"
  case default
    print *, "Dia de semana"
  end select

  print *, "=== where: condicional sobre arrays inteiros ==="
  ! "where" aplica a condição a cada elemento do vetor de uma vez.
  ! Onde temp > 30, alerta recebe .true.; nos demais, .false.
  where (temp > 30.0)
    alerta = .true.
  elsewhere
    alerta = .false.
  end where
  do i = 1, 5
    ! '(F6.1,A,L2)' = real (largura 6, 1 casa), texto, lógico (largura 2)
    print '(F6.1,A,L2)', temp(i), " graus -> alerta:", alerta(i)
  end do

  print *, "=== merge: escolha entre dois valores ==="
  ! merge(se_verdadeiro, se_falso, condição): equivale ao ?: de C.
  ! Os dois textos precisam ter o mesmo tamanho (3 letras).
  resposta = merge("SIM", "NAO", imc < 25.0)
  print *, "IMC abaixo de 25? ", resposta
end program condicionais
