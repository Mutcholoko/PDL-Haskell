# Verificador de Frames PDL em Haskell

Este é o repositório para o nosso trabalho de Linguagens de Programação, que consiste na implementação de um verificador de Frames PDL, utilizando a linguagem de programação Haskell.

O trabalho foi feito pelos alunos:
- Gabriel Gavazzi Felix
- Lucas Souza de Oliveira

## Descrição do Projeto

O objetivo deste projeto é desenvolver um verificador de frames PDL. O verificador será capaz de analisar se um grafo F = (W, Rα) corresponde a um frame válido para um dado programa π, e caso contrário, irá retornar o estado do programa que quebra a compatibilidade.

O projeto está sendo desenvolvido em Haskell, uma linguagem funcional que nos serviu como introdução a este paradigma de programação.

## Conteúdo do Repositório

O repositório possui os seguintes arquivos:

- `main`: Contém o arquivo binário executável do programa
- `main.hs`: O código fonte do programa.
- `main-old.hs`: A versão antiga do programa, que foi abandonada
- `README.md`: Este arquivo, que fornece informações sobre o projeto e sobre o funcionamento do programa.
- `exemplos.txt`: Arquivo de texto com exemplos prontos para testar a execução do programa mais facilmente.

## Funcionamento

O funcionamento do nosso verificador se dá da seguinte forma:

1. O usuário insere a fórmula do programa na variável `postfixExpression`, em notação pós fixada. (Checar a Seção **Exemplos** para mais informações)
2. O usuário insere a String do estado inicial do grafo, no último `print`, da seguinte forma: `print (avaliaExpressao frame raiz ESTADO_INICIAL)`
3. O usuário compila o programa e o executa. (instruções para compilação e execução na Seção **Instruções de Compilação/Execução**)
4. O usuário digita os estados de entrada no seguinte formato: `estado_origem estado_destino programa` (sendo os dois primeiros Strings e o último um Char).
5. O usuário continua a entrada, até que ele para, digitando `pronto`.
6. O programa executa a verificação e, caso seja o grafo F seja válido, retorna True; caso contrário, retorna False e o estado do programa em que houve a quebra da compatibilidade.

## Instruções de Compilação/Execução

Para compilar e executar o programa, siga as etapas abaixo:

1. Certifique-se de ter o GHC (Glasgow Haskell Compiler) e o make instalados em sua máquina.
2. Clone este repositório em seu ambiente local.
3. Acesse o diretório raíz do repositório por meio do terminal.
4. Compile o código Haskell executando o seguinte comando: `make`.
5. Após a compilação bem-sucedida, execute o programa com o comando: `./main`.
6. Siga as instruções para entrada na Seção **Funcionamento**.

## Exemplos

Em todos os exemplos abaixo, o estado inicial é w1. Além do mais, na fórmula, o item da esquerda é a fórmula em PDL, e na direita (depois do '//') é a notação pós-fixada. Você deve digitar a notação pós-fixada na varíavel `postfixExpression`.

Caso o usuário se interesse, também pode checar o arquivo `exemplos.txt`, que contém toda a entrada necessária para a execução.

Fórmula: `(a;b) // ab;` ; Retorno: `False`

![image](https://github.com/Mutcholoko/PDL-Haskell/assets/46263572/a82abbb5-b6ab-48cc-983d-5e991737d1be)

Fórmula: `(a*;b) // a*b;` ; Retorno: `True`

![image](https://github.com/Mutcholoko/PDL-Haskell/assets/46263572/f19f293d-a4bc-499b-aea7-dfbb9036f351)

Fórmula: `(a*;b)Ua // a*b;aU` ; Retorno: `True`

![image](https://github.com/Mutcholoko/PDL-Haskell/assets/46263572/347aa7df-a77f-45bd-96c5-abf81df9aa41)

Fórmula: `(a;b;b?;a) // ab;b?;a;` ; Retorno: `True`

![image](https://github.com/Mutcholoko/PDL-Haskell/assets/46263572/95028e28-cf10-4e1e-bccd-8f66d86d35a2)

Fórmula: `(a*;b) // a*b;` ; Retorno: `False`

![image](https://github.com/Mutcholoko/PDL-Haskell/assets/46263572/aebd5f3b-b094-4fe1-a776-57281d6fb711)


## Licença

Este projeto está licenciado sob a GNU General Public License v3.0.

##
