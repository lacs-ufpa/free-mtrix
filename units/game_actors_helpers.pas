{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_actors_helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, game_actors;

function GetColorFromCode(ACode : TGameColor) : TColor;

const

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
      //Interlocks : (
      //  Count : 0;
      //  History : nil;
      //);

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
        Style : gecWhichComeFirst;
        InterlockingPorcentage : 50;
        LastCycles : 4;
        AbsoluteCycles: 6;
      );
    );

implementation

uses game_resources;

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

end.

