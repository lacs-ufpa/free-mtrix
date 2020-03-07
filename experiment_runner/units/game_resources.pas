{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

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
  SEC_EXPERIMENT = 'Experiment';
  KEY_NAME = 'Name';
  KEY_AIM = 'Aim';
  KEY_RESEARCHER = 'Researcher.Name';
  KEY_RESEARCHER_CANCHAT = 'Researcher.Chat';
  KEY_RESEARCHER_CANPLAY = 'Researcher.CanPlay';
  KEY_GEN_PLAYER_AS_NEEDED = 'Players.AutoLogIn';
  KEY_CHAT_FOR_PLAYERS = 'Players.Chat';
  KEY_CHAT_HISTORY_FOR_NEW_PLAYERS = 'Players.OnLogIn.SendChatHistory';
  KEY_CURRENT_CONDITION = 'StartAtCondition';
  KEY_MATRIX_TYPE= 'MatrizElements';
  KEY_POINTS_TYPE= 'PointsType';

  SEC_PLAYER = 'Player.';
  KEY_PLAYER_TEMP = 'Data.X';
  KEY_PLAYER_TURN = 'Cycle';
  KEY_PLAYER_CHOICE_LAST = 'Choice';
  KEY_PLAYER_ID = 'ID';
  KEY_PLAYER_NICNAME = 'Nickname';
  KEY_PLAYER_LOGIN = 'User';
  KEY_PLAYER_PASSWORD = 'Password';
  KEY_PLAYER_POINTS = 'Points';
  KEY_PLAYER_STATUS = 'Status';

  SEC_CONDITION = 'Condition.';
  KEY_COND_NAME = 'Name';
  KEY_CULTURANT1_CAPTION = 'Culturant1.Counter.Label';
  KEY_CULTURANT1_PICTURE = 'Culturant1.Counter.Picture';
  KEY_CULTURANT2_CAPTION = 'Culturant2.Counter.Label';
  KEY_CULTURANT2_PICTURE = 'Culturant2.Counter.Picture';

  KEY_TURN_VALUE = 'Turn.NumberOfPlayers';  // 3
  KEY_TURN_COUNT = 'Turn.StartAt';    // 1
  KEY_TURN_RANDOM = 'Turn.RandomizeOrder'; // Sim

  KEY_POINTS_COUNT = 'Points.OnStart';
  KEY_POINTS_ONSTART_A = 'Points.OnConditionStartSum.A';
  KEY_POINTS_ONSTART_B = 'Points.OnConditionStartSum.B';
  KEY_POINTS_ONSTART_I = 'Points.OnConditionStartSum.I';
  KEY_POINTS_ONSTART_G1 = 'Points.OnConditionStartSum.C1';
  KEY_POINTS_ONSTART_G2 = 'Points.OnConditionStartSum.C2';

  KEY_CYCLES_VALUE = 'Cycles.ForGeneration';
  KEY_CYCLES_COUNT = 'Cycles.OnRestart';
  KEY_CYCLES_GEN = 'Cycles.StartAtGeneration';

  //KEY_PROMPT_VALUE = 'Questão.Apresentar'; // BOOL,CSQPROMPTCODE
  KEY_PROMPT_STYLE = 'Question.Style'; // string
  KEY_PROMPT_MESSAGE = 'Question.Message'; // string
  KEY_PROMPT_TARGET = 'Question.TargetMetacontingency';
  KEY_ENDCRITERIA = 'EndCriterium.Style'; //2,50,10,30,
  KEY_ENDCRITERIA_CYCLES = 'EndCriterium.Cycles'; // 20
  KEY_ENDCRITERIA_PORCENTAGE = 'EndCriterium.Porcentage'; // 80|10
  KEY_ENDCRITERIA_REACH_ZERO = 'EndCriterium.OnZeroReached';

  KEY_CONTINGENCY = 'Contingency.';
  KEY_METACONTINGENCY = 'Metacontingency.';

  // ROW,COLOR,OPCODE
  KEY_CONT_NAME = '.Name';
  KEY_CRITERIA = '.Response';
  KEY_CONSEQUE = '.Consequence'; // A,B,G,CSQCODE
  KEY_STYLE = '.Style';
  KEY_CONSEQUE_MESSAGE_PREPEND = '.Consequence.Message.Prefix';
  KEY_CONSEQUE_MESSAGE_PREPEND_LOSS = '.Consequence.Message.PrefixOnLoss';
  KEY_CONSEQUE_MESSAGE_APPEND_LOSS_S = '.Consequence.Message.SufixOnLoss.Singular';
  KEY_CONSEQUE_MESSAGE_APPEND_LOSS_P = '.Consequence.Message.SufixOnLoss.Plural';
  KEY_CONSEQUE_MESSAGE_PREPEND_EARN = '.Consequence.Message.PrefixOnEarn';
  KEY_CONSEQUE_MESSAGE_APPEND_EARN_S = '.Consequence.Message.SufixOnEarn.Singular';
  KEY_CONSEQUE_MESSAGE_APPEND_EARN_P = '.Consequence.Message.SufixOnEarn.Plural';
  KEY_CONSEQUE_MESSAGE_APPEND_ZERO = '.Consequence.Message.SufixOnZero';

  VAL_CONSEQUENCE = 'Consequence';
  VAL_RESEARCHER = 'Researcher';
  VAL_EXPERIMENT = 'Experiment';
  VAL_INTERLOCKING = 'Interlocking';
  VAL_BEGIN_TIME = 'Begin';
  VAL_END_TIME = 'End';
  VAL_RESEARCHERS = 'Researchers';

  DEF_END_CRITERIA_STYLE = 'CYCLES';
  //DEF_END_CRITERIA_CYCLES = '20';
  DEF_END_CRITERIA_PORCENTAGE = '80,10';

  DEF_POINTS = '0,0,0,';
  DEF_CONSEQUENCE = '1,0|M,C,P,A,|$NICNAME won|point.|points.|';
  DEF_METARESPONSE = 'ODD,AND,DIFFERENT,';
  DEF_CRITERIA = 'EVEN,AND,NOTHINGELSE,';
  //DEF_PROMPTMESSAGE = 'Vocês perderam 1 item escolar. Desejam recuperá-lo gastando pontos do Tipo A?';
  DEF_MATRIX_TYPE = 'COLORS,ROWS,';
const
  // grid colors
  ccYellow = $00FFFF;
  ccRed = $0000FF;
  ccGreen = $006400;
  ccBlue = $FF0018;
  ccMagenta = $8B008B;

var
  GLOBAL_MESSAGE_INTERVAL : Cardinal = 3000;
  GLOBAL_SYSTEM_MESSAGE_INTERVAL : Cardinal = 5000;
  GLOBAL_MESSAGES_INTERVAL : Cardinal = 2200;

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

end.

