# Compiladores — Semestre 2026.2

Repositório da disciplina de **COMPILADORES E PARADIGMAS DE PROGRAMAÇÃO**.

## Projeto

Projeto de um **transpilador** cuja **linguagem de origem é Fortran** e cuja
linguagem de destino é **C**.

**Equipe:** Alice Mariana, Anna Leticia, Gustavo Morais, Jaryson Simão e Vitória Eloise.

## Conteúdo das pastas

- `exemplos-origem/`: contém 17 códigos de exemplo em Fortran (um por assunto
  dos seminários), com README explicando como instalar o compilador (GFortran),
  compilar e executar.


## Uso de IA generativa

Foi utilizada a ferramenta Claude (Anthropic), pela interface web/app (claude.ai), com configurações padrão, sem personalização. Os códigos foram gerados a partir do prompt abaixo, depois compilados e testados pelo grupo com o GFortran 15 no Ubuntu (WSL).

- `Prompt`: “me dê exemplos de códigos em fortran para os seguintes tópicos: Hello World; tipos de dados (primitivos e compostos); tipos de dados (básicos/padrão da linguagem); ponteiros e/ou referências; palavras-chave e reservadas; variáveis (nomes, vinculação, tempo de vida e escopo); expressões (associatividade, precedência e sobrecarga de operadores); estruturas de controle e desvios de fluxo; comandos condicionais; comandos de repetição; subprogramas (passagem de parâmetros, co-rotinas, delegados, lambda, genéricos); concorrência; tratamento de exceções; tratamento de eventos.”
