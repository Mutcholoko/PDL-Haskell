# Verificador de Frames PDL em Haskell

Este é o repositório para o nosso trabalho de Linguagens de Programação, que consiste na implementação de um verificador de Frames PDL, utilizando a linguagem de programação Haskell.

O trabalho foi feito pelos alunos:
- Gabriel Gavazzi Felix
- Lucas Souza de Oliveira

## Descrição do Projeto

O objetivo deste projeto é desenvolver um verificador de frames PDL. O verificador será capaz de analisar se um grafo F = (W, Rα) corresponde a um frame válido para um dado programa π, e caso contrário, irá retornar o estado do programa em que quebra a compatibilidade.

O projeto está sendo desenvolvido em Haskell, uma linguagem funcional que nos serviu como introdução a este paradigma de programação.

## Funcionamento

O funcionamento do nosso verificador se dá da seguinte forma:

1. O usuário insere a fórmula do programa na variável `postfixExpression`, em notação pós fixada.
2. O usuário insere a String do estado inicial do grafo, no último `print`, da seguinte forma: `print (avaliaExpressao frame raiz ESTADO_INICIAL)`
3. O usuário compila o programa e o executa. (instruções para compilação e execução na Seção **Instruções de Compilação/Execução**)
4. O usuário digita os estados de entrada no seguinte formato: `estado_origem estado_destino programa` (sendo os dois primeiros Strings e o último um Char).
5. O usuário continua a entrada, até que ele para, digitando `pronto`.
6. O programa executa a verificação e, caso seja o grafo F seja válido, retorna True; caso contrário, retorna False e o estado do programa em que houve a quebra da compatibilidade.

## Conteúdo do Repositório

O repositório possui os seguintes arquivos:

- `main`: Contém o arquivo binário executável do programa
- `main.hs`: O código fonte do programa.
- `main-old.hs`: A versão antiga do programa, que foi abandonada
- `README.md`: Este arquivo, que fornece informações sobre o projeto e sobre o funcionamento do programa.

## Instruções de Compilação/Execução

Para compilar e executar o programa, siga as etapas abaixo:

1. Certifique-se de ter o GHC (Glasgow Haskell Compiler) instalado em sua máquina.
2. Clone este repositório em seu ambiente local.
3. Acesse o diretório raíz do repositório por meio do terminal.
4. Compile o código Haskell executando o seguinte comando: `ghc main.hs`.
5. Após a compilação bem-sucedida, execute o programa com o comando: `./main`.
6. Siga as instruções para entrada na Seção **Funcionamento**.

## Licença

Este projeto está licenciado sob a GNU General Public License v3.0.

##
