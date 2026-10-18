#!/usr/bin/env bash
# ^ "shebang": diz ao sistema para executar este arquivo com o bash.

# Compila todos os exemplos em ./build usando o GFortran.
# Uso: ./compilar_todos.sh        (depois: ./build/01_hello_world)

# "set -u" faz o script avisar e parar se alguma variável
# for usada sem ter sido definida (ajuda a achar erros de digitação).
set -u

# FLAGS = opções do compilador, usadas em todos os arquivos:
#   -std=f2018 : exige que o código siga o padrão Fortran 2018
#   -Wall      : mostra os avisos mais comuns
#   -Wextra    : mostra avisos adicionais
FLAGS="-std=f2018 -Wall -Wextra"

# Cria a pasta "build" (onde ficam os executáveis).
# "-p" evita erro se a pasta já existir.
mkdir -p build

# Laço: repete o bloco abaixo para cada arquivo que termina em .f90
# na pasta atual. A cada volta, a variável "f" guarda o nome do arquivo.
for f in *.f90; do
  # Tira a extensão ".f90" do nome. Ex.: "01_hello_world.f90"
  # vira "01_hello_world" (será o nome do executável).
  nome="${f%.f90}"

  # "extra" guarda flags específicas de cada arquivo.
  # Começa vazia e só é preenchida nos casos abaixo.
  extra=""

  # "case" escolhe uma ação conforme o nome do arquivo:
  case "$f" in
    # Arquivos que começam com 14_ usam OpenMP (paralelismo com threads)
    14_*) extra="-fopenmp" ;;            # OpenMP
    # Arquivos que começam com 15_ usam coarrays; "single" = 1 imagem
    15_*) extra="-fcoarray=single" ;;    # coarrays (1 imagem)
  esac

  # Mostra na tela qual arquivo está sendo compilado
  echo ">> Compilando $f"

  # Roda o gfortran: junta as flags comuns, a flag extra (se houver),
  # o arquivo de entrada ("$f") e define o executável de saída
  # ("-o build/$nome"). O "if" testa se a compilação deu certo.
  if gfortran $FLAGS $extra "$f" -o "build/$nome"; then
    # Compilou sem erros
    echo "   OK -> build/$nome"
  else
    # Houve erro de compilação: avisa e continua para o próximo arquivo
    echo "   FALHOU: $f"
  fi
done
