# Exemplos de código — Linguagem de origem: Fortran

Esta pasta contém 17 programas de exemplo em **Fortran** (padrão Fortran
2008/2018, formato livre `.f90`), um para cada assunto tratado nos
seminários da disciplina.

## Compilador utilizado

**GFortran** (parte do GCC), versão 9 ou superior recomendada.

### Instalação

| Sistema | Comando |
|---|---|
| Ubuntu / Debian / WSL | `sudo apt update && sudo apt install gfortran` |

Confirme com: `gfortran --version`

## Como compilar e executar

Um arquivo:

```bash
gfortran -std=f2018 -Wall -Wextra 01_hello_world.f90 -o hello
./hello
```

Todos de uma vez (gera os executáveis em `build/`):

```bash
chmod +x compilar_todos.sh
./compilar_todos.sh
./build/02_tipos_primitivos_compostos
```

## Arquivos

| Arquivo | Tema |
|---|---|
| `01_hello_world.f90` | Hello World |
| `02_tipos_primitivos_compostos.f90` | Tipos primitivos e compostos (arrays, tipos derivados) |
| `03_tipos_basicos_padrao.f90` | Tipos básicos/padrão, `kind`, tamanho e precisão |
| `04_ponteiros.f90` | Ponteiros e referências (lista encadeada) |
| `05_palavras_reservadas.f90` | Palavras-chave e (ausência de) palavras reservadas |
| `06_variaveis_escopo.f90` | Nomes, vinculação, tempo de vida e escopo |
| `07_expressoes_precedencia.f90` | Precedência e associatividade |
| `08_sobrecarga_operadores.f90` | Sobrecarga de operadores e operador novo |
| `09_estruturas_controle.f90` | Desvios de fluxo (`exit`, `cycle`, `goto`, `return`, `stop`) |
| `10_condicionais.f90` | `if`, `select case`, `where`, `merge` |
| `11_repeticao.f90` | `do`, `do while`, `do concurrent`, do implícito |
| `12_subprogramas.f90` | Funções, sub-rotinas e passagem de parâmetros |
| `13_lambda_delegados_genericos.f90` | Delegados, "lambdas", co-rotinas simuladas, genéricos |
| `14_concorrencia_openmp.f90` | `do concurrent`, OpenMP, E/S assíncrona |
| `15_coarrays.f90` | Programação paralela/distribuída com coarrays |
| `16_excecoes.f90` | Tratamento de erros e exceções |
| `17_eventos.f90` | Tratamento de eventos (despachador com callbacks) |
