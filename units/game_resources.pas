unit game_resources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , game_actors
  ;

function GenResourceName(i : integer) : UTF8string;

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
  KEY_PLAYER_CHOICE_CURRENT = 'Escolha.Atual';
  KEY_PLAYER_CHOICE_LAST = 'Escolha.Passada';
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

  KEY_RESPONSE = '.RespostaEsperada'; // ROW,COLOR,OPCODE
  KEY_CONSEQUE = '.Consequência'; // A,B,G,CSQCODE
  KEY_CONSEQUE_MESSAGE = '.Consequência.Mensagem';

  VAL_RESEARCHER = 'Pesquisador';

  DEF_END = '2,20,10,10,';  // which come first, 20 cycles | 10% entrelaçamentos in the last 10 cycles
  DEF_POINTS = '0,0,0,';
  DEF_CONSEQUENCE = '0,0,0,NON,50,50,';
  DEF_METACONSEQUENCE_MESSAGE = 'Vocês produziram <$G> <$SG>.';
  DEF_CONSEQUENCE_MESSAGE = '<$JOGADOR> ganhou <$A> <$SA> e <$B> <$SB>.';
  DEF_METARESPONSE = 'IMPAR,E,DIFERENTES,';
  DEF_RESPONSE = 'PAR,E,INDIFERENTE,';
  DEF_PROMPTMESSAGE = 'Vocês perderam <$G> item escolar. Desejam recuperá-lo gastando pontos do Tipo A?';
const

  CPlayerNamesMale : array [0..49] of UTF8String =
     ('João','Rodrigo','Francisco','Martim','Santiago',
     'Tomás','Afonso','Duarte','Miguel','Guilherme','Tiago',
     'Gonçalo','Diogo','Gabriel','Pedro','Rafael','Salvador',
     'Dinis','Lucas','Simão','Gustavo','David',
     'José','Vicente','Lourenço','Diego','Daniel',
     'António','André','Vasco','Manuel','Henrique',
     'Leonardo','Bernardo','Mateus','Luís','Eduardo',
     'Alexandre','Leandro','Filipe','Enzo','Ricardo',
     'Matias','Rúben','Samuel','Bruno','Isaac','Xavier','Nuno','Carlos');

  CPlayerNamesFemale : array [0..49] of UTF8String =
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
    Choice : (
      Current : (Row:grNone; Color:gcNone;);
      Last : (Row:grNone; Color:gcNone;);
    );
    Points : (A:0; B:0);
    Turn : -1;
  );

  //C_OPERANT_1 : TContingency =
  //  (
  //    Consequence : (
  //      Style : [gscShowMessage,gscPoints];
  //      Points :(A : 0;    B : 1;    G : 0;);
  //      Message : '<$JOGADOR> produziu 1 ponto do tipo B.';
  //      Cycles : 0;                 // absolute,
  //      VariationMin: 0;            // porcentage,
  //      VariationMax : 0;     // porcentage
  //      Prompt : (
  //        Message : '';
  //        Style : [];
  //      );
  //    );                  // prompt needs its own class
  //
  //    Response : (
  //      Operator_ : goNONE;
  //      Rows : [grEven];
  //      Colors : [gcNone];
  //    );
  //
  //    Meta : False;
  //  );

  //C_OPERANT_2 : TContingency =
  //  (
  //    Consequence : (
  //      Style : [gscShowMessage,gscPoints];
  //      Points :(A : 3;    B : 0;    G : 0;);
  //      Message : '<$JOGADOR> produziu 3 pontos do tipo A.';
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
  //      Operator_ : goNONE;
  //      Rows : [grOdd];
  //      Colors : [gcNone];
  //    );
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

  //C_METACONTINGENCY_A2 : TContingency =
  //  (
  //    Consequence : (
  //      Style : [gscShowMessage,gscPoints,gscBroadcastMessage];
  //      Points :( A : 0;    B : 0;    G : -1;);
  //      Message : 'Vocês perderam 1 item escolar.';    // show first in case of last participant
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
  //      Operator_ : goNONE;
  //      Rows : [grOdd,grSome];
  //      Colors : [gcNone];
  //    );
  //
  //    Meta : True;
  //  );

    //C_METACONTINGENCY_B1: TContingency =
    //  (
    //    Consequence : (
    //      Style : [gscShowMessage,gscPoints,gscBroadcastMessage];
    //      Points :(A :-1;    B : 0;    G : -1;);
    //      Message : 'Vocês perderam 1 item escolar e uma quantidade de pontos do Tipo A.';
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
    //      Operator_ : goNONE;
    //      Rows : [grOdd, grSome];
    //      Colors : [gcNone];
    //    );
    //
    //    Meta : True;
    //  );

    //C_METACONTINGENCY_B2: TContingency =
    //  (
    //    Consequence : (
    //      Style : [gscShowMessage,gscPoints,gscBroadcastMessage,gscPromptQuestion];
    //      Points :(A :-3;    B : 0;    G : -1;);
    //      Message : 'Vocês perderam 1 item escolar.';
    //      Cycles : 0;                 // absolute,
    //      VariationMin: 0;            // porcentage,
    //      VariationMax : 0;     // porcentage
    //      Prompt : (
    //        Message : 'Vocês perderam 1 item escolar, desejam recuperá-lo gastando pontos do Tipo A?';
    //        Style : [gsAll,gsYes,gsMetacontingency,gsRecoverLostPoints];
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

      Points : (
        Count : ( A:1; B:2; G:3; );
        OnStart : ( A:3; B:1; G:0; );
      );

      Turn : (
        Count: 0;
        Value : 3;
        Random: False;
      );

      Cycles : (
        Count : 0;
        Value : 3;
        Generation : 0;
      );

      Prompt : nil;
      EndCriterium : (
        Value : gecWhichComeFirst;
        InterlockingPorcentage : 10;
        LastCycles : 6;
        AbsoluteCyles: 8;
      );
    );

implementation

uses zhelpers;

function GenResourceName(i: integer): UTF8string;
begin
  Randomize;
  if (i <= 49) and (i>=0) then
    begin
      if Random>0.5 then
        Result := CPlayerNamesMale[i]
      else
        Result := CPlayerNamesFemale[i];
    end
  else s_random(10);
end;

end.

