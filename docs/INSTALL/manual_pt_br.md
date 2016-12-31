/*   
  Free-mtrix - Free cultural selection and social behavior experiments.   
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.   

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License   
  along with this program. If not, see <http://www.gnu.org/licenses/>.   
*/

# Manual

## Sumário.

  1 - Introdução.

  2 - Designer.

    2.1 - Instalação.
    2.2 - Compilação.
    2.3 - Execução.
      2.3.1 - Windows.
      2.3.2 - Sistemas Operacionais Linux.
    2.4 - Utilização.

  3 - Runner.

    3.1 - Instalação.
      3.1.1 - libzmq no Windows.
      3.1.2 - libzmq em Sistemas Operacionais Linux.
    3.2 - Compilação.
      3.1.1 - Windows.
      3.1.2 - Sistemas Operacionais Linux.
    3.3 - Execução.
      3.3.1 - Windows.
      3.3.2 - Sistemas Operacionais Linux.
      3.3.3 - Observações para todos os sistemas.
    3.4 - Utilização.
      3.4.1 - Em um único computador.
      3.4.2 - Em uma rede local.
  4 - Problemas conhecidos e soluções.
    4.1 - Uma referência foi retornada do servidor.

  5 - Referências.

## 1. Introdução

  Free-mtrix (v1.0.0) é um sistema que permite o planejamento e a apresentação de experimentos sociais baseados em tarefas similares àquelas apresentadas por Vich, Andery e Glenn (2009). Ele é composto por dois programas de computador. O programa `Designer` é usado para o planejamento e o programa `Runner` para a apresentação de experimentos.  

## 2. Designer

### 2.1. Instalação

  O programa `Designer` é uma aplicação portável (portable application) e não necessita de instalação.

### 2.2. Compilação

  Para compilar o programa `Designer` siga os passos a seguir:

  1. Obtenha uma cópia do código fonte: https://github.com/lacs-ufpa/free-mtrix.git. 
  2. Instale o Lazarus (Interface de Desenvolvimento Rápido de Aplicações) versão 1.6.2 ou superior.
  3. Execute o Lazarus.
  4. Abra o arquivo `experiment_designer.lpi` localizado na pasta `experiment_designer` por meio da interface gráfica do Lazarus.
  5. Execute o comando `compilar` do Lazarus (por exemplo, apertando F9).

### 2.3. Execução

#### 2.3.1. Windows

  Nos sistemas operacionais Windows, o programa `Designer` é executado por meio do arquivo `experiment_designer.exe`, localizado na pasta `experiment_designer`. Recomenda-se a execução no Windows 10, 32 bits.

#### 2.3.2. Sistemas Operacionais Linux

  Nos sistemas operacionais Linux, o programa `Designer` é executado por meio do arquivo `experiment_designer`, localizado na pasta `experiment_designer`. Recomenda-se a execução no Debian 8, 32 ou 64 bits.

### 2.4. Utilização

  O programa `Designer` é usado para o planejamento de experimentos sociais por meio do preenchimento de um formulário pelo pesquisador. O formulário é curto (~10 min) e utiliza a terminologia do campo de estudos chamado "Análise do Comportamento", tornando-o intuitivo aos pesquisadores da área. Todo o processo de preenchimento é salvo automaticamente. Ao final, um arquivo de configuração do experimento é disponibilizado para uso, reuso, arquivamento ou edição.

## 3. Runner

### 3.1 Instalação

  O programa `Runner` é uma aplicação auto-executável e não necessita de instalação. Entretanto, o programa `Runner` depende da instalação da biblioteca `libzmq` (http://zeromq.org/). Uma cópia da biblioteca libzmq é distribuida com o Free-mtrix. Confira a seguir como instalar a biblioteca no seu sistema.

#### 3.1.1. libzmq no Windows

Siga os passos a seguir para instalar a biblioteca libzmq no Windows 10.

  1. Baixe e instale o `Visual C++ Redistributable for Visual Studio 2015`. Você pode encontrá-lo aqui: https://www.microsoft.com/en-us/download/details.aspx?id=48145
  2. Copie o arquivo `libzmq.dll` distribuido com este (v3.2.5, 32bits, dinâmica, compilada com o VS 2015).
    - Alternativamente, você mesmo pode construir a biblioteca seguindo as instruções aqui: http://zeromq.org/intro:get-the-software#toc8.
  3. Cole o arquivo `libzmq.dll` dentro da pasta `experiment_runner`. A pasta deve conter o arquivo executável `experiment_runner.exe`. 
  4. Pronto!

#### 3.1.2. libzmq em Sistemas Operacionais Linux

Siga os passos a seguir para instalar a biblioteca libzmq em sistemas Linux.

  1. Obtenha uma cópia do código fonte da biblioteca "libzmq", v3.2.5, aqui: http://zeromq.org/intro:get-the-software. 
  2. Construa e instale a biblioteca "libzmq.so" seguindo as instruções aqui: http://zeromq.org/intro:get-the-software#toc7.
  3. Pronto!

### 3.2. Compilação

  Para compilar o programa "Runner" siga os passos a seguir:

  1. Obtenha uma cópia do código fonte: https://github.com/lacs-ufpa/free-mtrix.git.
  2. Instale a biblioteca libzmq.
  3. Instale o Lazarus (Interface de Desenvolvimento Rápido de Aplicações) versão 1.6.2 ou superior.
    - No Windows instale necessariamente a versão 32 bits do Lazarus ou, alternativamente, instale a versão 64 bits e implemente o suporte para a plataforma.
    - Em sistemas Linux, recomenda-se a versão 64 bits do Lazarus.
  4. Execute o Lazarus.
  5. Abra o arquivo "experiment_runner.lpi" localizado na pasta "experiment_runner" por meio da interface gráfica do Lazarus.
  6. Execute o comando "compilar" do Lazarus (por exemplo, apertando F9).

### 3.3. Execução

#### 3.3.1. Windows

  Nos sistemas operacionais Windows, `Runner` é executado por meio do arquivo `experiment_runner.exe`, localizado na pasta `experiment_runner\`. Recomenda-se a execução no Windows 10, 32 bits.

#### 3.3.2. Sistemas Operacionais Linux

  Nos sistemas operacionais Linux, `Runner` é executado por meio do arquivo `experiment_runner`, localizado na pasta `experiment_runner/`. Recomenda-se a execução no Debian 8, 64 bits.

#### 3.3.3. Observações para todos os sistemas

  - Se você possui um firewall ligado, you necessitará criar uma regra adicionando o programa como uma exceção.

  - Você necessitará de uma instância do programa sendo executada como Servidor (Pesquisador) antes de executar instâncias como Clientes (Participantes). O programa funciona com no mínimo 2 clientes.

### 4. Utilização

  Por meio da leitura de arquivos de configuração válidos (gerados pelo programa "Designer" ou não), o programa `Runner` é usado para a apresentação de experimentos aos participantes de uma pesquisa. Um chat integrado ao programa, se habilitado no arquivo de configuração, permite a interação entre os participantes por meio de texto. Os participantes devem ser instruidos a realizar uma tarefa de escolha apresentada pelo programa. O registro das escolhas e da interação é automático e em tempo real. O programa pode ser executado em um único computador, para testes e debug por exemplo, ou pode ser facilmente configurado em uma rede local com múltiplos computadores.

  O programa `Runner` possui seu próprio servidor e cliente embarcados. Veja seguir como executar diferentes instâncias do programa como servidor e cliente.

#### 3.4.1. - Em um único computador

  O exemplo a seguir assume que uma instância do programa será executada como um servidor, e três instâncias como participantes em uma mesma máquina. Também se assume a existência de um arquivo de configuração chamado `experimento_x.ini`, um experimento de nome `Experimento_X` configurado para 3 participantes e feito por um pesquisador de nome `Pesquisador_X`.

  1. Instale a biblioteca libzmq na máquina alvo.
  2. Copie o executável `experiment_runner` para diferentes pastas (se Windows, também copie a biblioteca `libzmq`). Por exemplo, no Linux a estrutura de arquivos ficaria assim:

```
  ~/Servidor/experiment_runner
  ~/Participante1/experiment_runner
  ~/Participante2/experiment_runner
  ~/Participante3/experiment_runner
```
  OBS.: Para facilitar a comunicação, os diferentes executáveis de cada diretório serão chamados pelo nome do próprio diretório que os contém como Servidor, Participante1, Participante2 e Participante3.

  3. Execute o Servidor e pressione o botão `Pesquisador`.
  4. Pressione o botão `Carregar` na caixa `Pesquisador` à direita e carregue o arquivo de configuração `experimento_x.ini`.
  5. Execute o Participante1 e pressione o botão `Participante`.
    - repita a operação para o Participante2 e o Participante3.
  6. Pronto! O experimento está sendo executado, os três participantes podem executar a tarefa e o Servidor pode monitorá-los!
  7. Ao final do experimento os arquivos de dados (`000.data` e `000.chat`) podem ser conferidos no diretório:
    - ~/Servidor/Pesquisadores/Pesquisador_X/Experimento_X/

#### 3.4.2. - Em uma rede local

  O exemplo a seguir assume que diferentes instâncias do programa, sendo um servidor e três clientes, serão executadas em 4 diferentes máquinas de uma rede local. Também se assume que a máquina executando o servidor possui o ip `192.168.1.100`, se assume um arquivo de configuração chamado `experimento_x.ini`, um experimento de nome `Experimento_X` configurado para três participantes e feito por um pesquisador de nome `Pesquisador_X`.

  1. Copie o executável `experiment_runner` para um diretório e instale a biblioteca libzmq em cada máquina, por exemplo:

```
  terminal@Máquina1$~/Servidor/experiment_runner
  terminal@Máquina2$~/Participante1/experiment_runner
  terminal@Máquina3$~/Participante2/experiment_runner
  terminal@Máquina4$~/Participante3/experiment_runner
```

  OBS.: Para facilitar a comunicação, os diferentes executáveis de cada diretório serão chamados pelo nome do próprio diretório que os contém como Servidor, Participante1, Participante2 e Participante3.

  2. Os participantes devem saber aonde o servidor está. Para isso, crie um arquivo de texto chamado `IP` no diretório de cada participante. Na primeira linha do arquivo `IP` escreva o endereço ip do servidor, neste caso `192.168.1.100`.
  3. Execute o Servidor e pressione o botão `Pesquisador`.
  4. Pressione o botão `Carregar` na caixa `Pesquisador` à direita e carregue o arquivo de configuração `experimento_x.ini`.
  5. Execute o Participante1 e pressione o botão `Participante`.
    - repita a operação para o Participante2 e o Participante3.
  6. Pronto! O experimento está sendo executado, os três participantes podem executar a tarefa e o Servidor pode monitorá-los!
  7. Ao final do experimento os arquivos de dados (`000.data` e `000.chat`) podem ser conferidos no diretório:
    - terminal@Máquina1$~/Servidor/Pesquisadores/Pesquisador_X/Experimento_X/

## 4. Problemas conhecidos e soluções.

### 4.1. Uma referência foi retornada do servidor.

   Esse erro pode ocorrer apenas no Windows.

   - Solução 1: Clique com o botão direito sobre o executável e selecione propriedades. Na aba "compatibilidade", marque a caixa "Executar como administrador".

## 5. Referências

Vichi, C., Andery, M. A. P. A., & Glenn, S. S. (2009). A metacontingency experiment: the effects of contingent consequences on patterns of interlocking contingencies reinforcement. Behavioral and Social Issues, 18, 41-57. doi: 10.5210/bsi.v18i1.2292