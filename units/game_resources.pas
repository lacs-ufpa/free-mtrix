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
  Classes, SysUtils, Graphics
  , game_actors
  ;

function GenResourceName(i : integer) : string;
function GetColorFromCode(ACode : TGameColor) : TColor;

resourcestring
  KV_SEP = '=';
  VV_SEP = ',';
  SEC_EXPERIMENT = 'Experimento';
  KEY_NAME = 'Nome';
  KEY_AIM = 'Objetivo';
  KEY_RESEARCHER = 'Pesquisador.Responsável';
  KEY_GEN_PLAYER_AS_NEEDED = 'GerarJogadoresAutomaticamente';
  KEY_CURRENT_CONDITION = 'ComeçarNaCondição';
  KEY_MATRIX_TYPE= 'TipoDaMatrix';

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
  KEY_ENDCRITERIA = 'Critério.DeFinalizaçãoDaCondição'; //2,50,10,30,

  KEY_CONTINGENCY = 'Contingência.';
  KEY_METACONTINGENCY = 'Metacontingência.';

  // ROW,COLOR,OPCODE
  KEY_CRITERIA = '.EsquemaDeReforço';
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

  C_PLAYER_TEMPLATE : TPlayer = (
    ID : '';
    Nicname : '';
    Login :'';
    Password : '';
    Status : gpsWaiting;
    Data : nil;
    Choice : (Row:grNone; Color:gcNone;);
    Points : (A:0; B:0);
    Turn : -1;
  );

  //C_OPERANT_1 : TContingency =
  //  (
  //    Consequence : (
  //      Style : [gscShowMessage,gscPoints, gscB];
  //      Message : '<$JOGADOR> produziu 1 ponto do tipo B.';
  //      Value: 1;
  //      Variation:1;
  //
  //    Criteria : (
  //      Style : goNONE;
  //      Rows : [grEven];
  //      Colors : [gcNone];
  //    );
  //
  //    Meta : False;
  //  );

  //C_OPERANT_2 : TContingency =
  //  (
  //    Consequence : (
  //      Style : [gscShowMessage,gscPoints, gscA];
  //      Message : '<$JOGADOR> produziu 3 pontos do tipo A.';
  //
  //    Criteria : (
  //      Operator_ : goNONE;
  //      Rows : [grEven];
  //      Colors : [gcNone];
  //    );
  //
  //    Meta : False;
  //  );

  //C_METACONTINGENCY_A1 : TContingency =
  //  (
  //    Consequence : (
  //      Style : [gscShowMessage,gscPoints,gscBroadcastMessage];
  //      Points :( A : 0;    B : 0;    G : 1;);
  //      Message : 'Vocês produziram 1 item escolar.';    // show first in case of last participant
  //      Cycles : 0;                 // absolute,
  //      VariationMin: 0;            // porcentage,
  //      VariationMax : 0;     // porcentage
  //      Prompt : (
  //        Message : '';
  //        Style : [];
  //      );
  //    );
  //
  //    Response : (
  //      Operator_ : goAND;
  //      Rows : [grEven];
  //      Colors : [gcDiff];
  //    );
  //
  //    Meta : True;
  //  );


    //C_METACONTINGENCY_B2: TContingency =
    //  (
    //    Consequence : (
    //      Style : [gscShowMessage,gscPoints,gscBroadcastMessage,gscPromptQuestion];
    //      Points :(A :0;    B : 0;    G : -1;);
    //      Message : 'Vocês produziram a perda de 1 item escolar.';
    //      Cycles : 0;                 // absolute,
    //      VariationMin: 0;            // porcentage,
    //      VariationMax : 0;     // porcentage
    //      Prompt : (
    //        Message : 'Um item escolar foi perdido, desejam recuperá-lo gastando pontos do Tipo A?';
    //        Style : [gsAll,gsYes,gsMetacontingency,gsRecoverLostPoints, gsContingency, gsBasA];
    //      );
    //    );
    //
    //    Response : (
    //      Operator_ : goNONE;
    //      Rows : [grOdd, grSome];
    //      Colors : [gcNone];
    //    );
    //
    //    Meta : True;
    //  );

  C_CONDITION_TEMPLATE : TCondition =
    (
      ConditionName : '';
      Contingencies : nil;
      Interlocks : (
        Count : 0;
        History : nil;
      );

      Points : (
        Count : ( A:0; B:0; G:0; );
        OnStart : ( A:0; B:0; G:0; );
      );

      Turn : (
        Count: 0;
        Value : 0;
        Random: False;
      );

      Cycles : (
        Count : 0;
        Value : 0;
        Generation : 0;
      );

      Prompt : nil;
      EndCriterium : (
        Value : gecWhichComeFirst;
        InterlockingPorcentage : 50;
        LastCycles : 4;
        AbsoluteCycles: 6;
      );
    );

implementation

uses zhelpers;

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

function GetColorFromCode(ACode: TGameColor): TColor;
begin
  case ACode of
    gcNone :Result  :=  clInactiveCaption;
    gcYellow :Result  :=  ccYellow;
    gcRed :Result  :=  ccRed;
    gcMagenta :Result  :=  ccMagenta;
    gcBlue :Result  :=  ccBlue;
    gcGreen :Result  :=  ccGreen;
  end;
end;

initialization

  Randomize;

end.
