{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_resources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

function GenResourceName(i : integer) : string;

resourcestring
  KV_SEP = '=';
  VV_SEP = ',';
  SEC_EXPERIMENT = 'Experimento';
  KEY_NAME = 'Nome';
  KEY_AIM = 'Objetivo';
  KEY_RESEARCHER = 'Pesquisador.Nome';
  KEY_RESEARCHER_CANCHAT = 'Pesquisador.BatePapo';
  KEY_RESEARCHER_CANPLAY = 'Pesquisador.PodeJogar';
  KEY_GEN_PLAYER_AS_NEEDED = 'Jogadores.GerarAutomaticamente';
  KEY_CHAT_HISTORY_FOR_NEW_PLAYERS = 'Jogadores.Novos.Enviar_Histórico_do_BatePapo';
  KEY_CURRENT_CONDITION = 'ComeçarNaCondição';
  KEY_MATRIX_TYPE= 'TipoDaMatrix';
  KEY_POINTS_TYPE= 'TipoDePontuação';

  SEC_PLAYER = 'Jogador.';
  KEY_PLAYER_TEMP = 'Data.X';
  KEY_PLAYER_TURN = 'Jogada';
  KEY_PLAYER_CHOICE_LAST = 'Escolha';
  KEY_PLAYER_ID = 'ID';
  KEY_PLAYER_NICNAME = 'Apelido';
  KEY_PLAYER_LOGIN = 'Usuário';
  KEY_PLAYER_PASSWORD = 'Senha';
  KEY_PLAYER_POINTS = 'Pontos';
  KEY_PLAYER_STATUS = 'Status';

  SEC_CONDITION = 'Condição.';
  KEY_COND_NAME = 'Nome';

  KEY_TURN_VALUE = 'Rodada.NúmeroDeJogadores';  // 3
  KEY_TURN_COUNT = 'Rodada.IniciarNaJogada';    // 1
  KEY_TURN_RANDOM = 'Rodada.OrdemDeJogadaAleatória'; // Sim

  KEY_POINTS_COUNT = 'Pontos.AoRecomeço';
  KEY_POINTS_ONSTART = 'Pontos.SomarAoComeço';

  KEY_CYCLES_VALUE = 'Ciclos.MudançaDeGeração';
  KEY_CYCLES_COUNT = 'Ciclos.AoRecomeço';
  KEY_CYCLES_GEN = 'Ciclos.IniciarNaGeração';

  //KEY_PROMPT_VALUE = 'Questão.Apresentar'; // BOOL,CSQPROMPTCODE
  KEY_PROMPT_STYLE = 'Questão.Estilo'; // string
  KEY_PROMPT_MESSAGE = 'Questão.Mensagem'; // string
  KEY_ENDCRITERIA = 'Critério_de_Finalização.Estilo'; //2,50,10,30,
  KEY_ENDCRITERIA_CYCLES = 'Critério_de_Finalização.Ciclos'; // 20
  KEY_ENDCRITERIA_PORCENTAGE = 'Critério_de_Finalização.Porcentagem'; // 80|10


  KEY_CONTINGENCY = 'Contingência.';
  KEY_METACONTINGENCY = 'Metacontingência.';

  // ROW,COLOR,OPCODE
  KEY_CONT_NAME = '.Nome';
  KEY_CRITERIA = '.Resposta';
  KEY_CONSEQUE = '.Consequência'; // A,B,G,CSQCODE
  KEY_CONSEQUE_MESSAGE_PREPEND = '.Consequência.Mensagem.Prefixo';
  KEY_CONSEQUE_MESSAGE_APPENDS = '.Consequência.Mensagem.Sufixo.Singular';
  KEY_CONSEQUE_MESSAGE_APPENDP = '.Consequência.Mensagem.Sufixo.Plural';

  VAL_CONSEQUENCE = 'Cosequência';
  VAL_RESEARCHER = 'Pesquisador';
  VAL_EXPERIMENT = 'Experimento';
  VAL_INTERLOCKING = 'Entrelaçamento';

  VAL_BEGIN_TIME = 'Começo';

  DEF_END = '2,20,10,10,';  // which come first, 20 cycles | 10% entrelaçamentos in the last 10 cycles
  DEF_POINTS = '0,0,0,';
  DEF_CONSEQUENCE = '1,0|M,C,P,A,|$NICNAME|ponto|pontos|';
  DEF_METARESPONSE = 'IMPAR,E,DIFERENTES,';
  DEF_CRITERIA = 'PAR,E,INDIFERENTE,';
  DEF_PROMPTMESSAGE = 'Vocês perderam <$G> item escolar. Desejam recuperá-lo gastando pontos do Tipo A?';

const
  // grid colors
  ccYellow = $00FFFF;
  ccRed = $FF0018;
  ccGreen = $006400;
  ccBlue = $0000FF;
  ccMagenta = $8B008B;

const

  CPlayerNamesMale : array [0..49] of string =
     ('João','Rodrigo','Francisco','Martim','Santiago',
     'Tomás','Afonso','Duarte','Miguel','Guilherme','Tiago',
     'Gonçalo','Diogo','Gabriel','Pedro','Rafael','Salvador',
     'Dinis','Lucas','Simael','Gustavo','David',
     'José','Vicente','Lourenço','Diego','Daniel',
     'António','André','Vasco','Manuel','Henrique',
     'Leonardo','Bernardo','Mateus','Luís','Eduardo',
     'Alexandre','Leandro','Filipe','Enzo','Ricardo',
     'Matias','Rúben','Samuel','Bruno','Isaac','Xavier','Nuno','Carlos');

  CPlayerNamesFemale : array [0..49] of string =
    ('Maria','Matilde','Thais','Beatriz','Mariana',
    'Carolina','Ana','Inês','Sofia','Margarida',
    'Lara','Joana','Laura','Francisca','Diana',
    'Mafalda','Madalena','Clara','Luana','Sara',
    'Bianca','Alice','Rita','Íris','Constança',
    'Letícia','Eva','Gabriela','Camila','Yara',
    'Benedita','Mara','Catarina','Ariana','Ema',
    'Vitória','Marta','Carlota','Iara','Yasmin',
    'Nicole','Luísa','Daniela','Núria','Bruna',
    'Victória','Alícia','Rafaela','Helena','Miriam');


implementation

//uses zhelpers;

function GenResourceName(i: integer): string;
var r :integer;
begin
  if  (i >= 0) and (i <= 49) then
    r := i
  else r := Random(50);

  if Random > 0.5 then
    Result := CPlayerNamesMale[r]
  else
    Result := CPlayerNamesFemale[r];

end;


initialization

  Randomize;

end.

