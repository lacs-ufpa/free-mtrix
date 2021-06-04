{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_actors_helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, game_actors, game_zmq_actors;

function Sanitize(S : string):string;
function DeduceNicname(S : string; P : TPlayer) : string;
function GetColorFromCode(ACode : TGameColor) : TColor;
function GetMessagesFromPromptStyle(APromptStyle : TPromptStyle;
  AContingencies : TContingencies) : TStringList;
function FirstDelimitedString(S : string):string;
procedure IncLabel(ALabel : TLabel; AValue:integer=0);
function GameActor(AZMQActor : TZMQActor) : TGameActor;
function PointsToMoney(APoints : integer; AFactor :integer) : string;

var
  FStart : cardinal;

const

  C_PLAYER_TEMPLATE : TPlayer = (
    ID : '';
    Nicname : '';
    Login :'';
    Password : '';
    Status : gpsWaiting;
    Data : nil;
    Choice : (Row:grNone; Color:gcNone;);
    Points : (A:0; B:0; G1:0; G2:0);
    Index : -1;
    Turn : -1;
  );

  //C_OPERANT_1 : TContingency =
  //  (
  //    Consequence : (
  //      Style : [gscShowMessage,gscGlobalPoints, gscB];
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
  //      Style : [gscShowMessage,gscGlobalPoints, gscA];
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
  //      Style : [gscShowMessage,gscGlobalPoints,gscBroadcastMessage];
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
    //      Style : [gscShowMessage,gscGlobalPoints,gscBroadcastMessage,gscPromptQuestion];
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
      Index : -1;
      ConditionName : '';
      Label1:'';
      Label2:'';
      TargetMetacontingency:'';
      Slides : nil;
      GenerationSlidesLogIn : nil;
      GenerationSlidesLogOut : nil;
      Contingencies : nil;
      //Interlocks : (
      //  Count : 0;
      //  History : nil;
      //);

      Points : (
        Count : ( A:0; B:0; G1:0; G2:0; );
        OnStart : ( A:0; B:0; G1:0; G2:0; );
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
        ReachZero : False;
        Style : gecWhichComesFirst;
        UpperInterlockingPorcentage : 80;
        LowerInterlockingPorcentage : 20;
        LastCycles : 20;
        AbsoluteCyclesMin: 50;
        AbsoluteCyclesMax: 100;
      );
    );

implementation

uses strutils, game_resources, string_methods;

function Sanitize(S: string): string;
const
  LineFeed = '\n';
begin
  Result := S;
  if (Pos(LineFeed, S) > 0) then
     Result := ReplaceStr(S, LineFeed, LineEnding);
end;

function DeduceNicname(S: string; P: TPlayer): string;
begin
  Result := S;
  if (Pos('$NICNAME',S) > 0) and (P.Nicname <> '') then
     Result := ReplaceStr(S,'$NICNAME', P.Nicname);
end;

function PointsToMoney(APoints : integer; AFactor :integer) : string;

  function FormatMoney : string;
  begin
    Result := FormatCurr('$#0.00', (APoints*AFactor)/100);
  end;

begin
  case APoints of
    0 : Result := '$0.00';
    1..9: Result := FormatMoney + ' cents';
    10 : Result := FormatMoney + ' dollar';
    else Result := FormatMoney + ' dollars';
  end;
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
    else { do nothing };
  end;
end;


procedure ApplyPromptStylePointConditionTo(var AConsequenceString: string;
  AID: string; APromptStyle: TPromptStyle; ATargetContingencies: TContingencies);
var
  i : integer;
  //LI,LS,LP,
  LPoints : integer;
  LCsqStyle : TConsequenceStyle;
  LPrepend,
  LPrependLoss,LPrependEarn,
  LAppendiceLossSingular,LAppendiceLossPlural,
  LAppendiceEarnSingular,LAppendiceEarnPlural,
  LAppendiceZero : string;
begin
  LPoints := StrToInt(ExtractDelimited(1,AConsequenceString, ['|']));
  LCsqStyle := GetConsequenceStyleFromString(ExtractDelimited(2,AConsequenceString, ['|']));
  LPrepend := ExtractDelimited(3,AConsequenceString, ['|']);
  LPrependLoss := ExtractDelimited(4,AConsequenceString, ['|']);
  LAppendiceLossSingular := ExtractDelimited(5,AConsequenceString, ['|']);
  LAppendiceLossPlural := ExtractDelimited(6,AConsequenceString, ['|']);
  LPrependEarn := ExtractDelimited(7,AConsequenceString, ['|']);
  LAppendiceEarnSingular := ExtractDelimited(8,AConsequenceString, ['|']);
  LAppendiceEarnPlural := ExtractDelimited(9,AConsequenceString, ['|']);
  LAppendiceZero := ExtractDelimited(10,AConsequenceString, ['|']);

  if (gsRevertPoints in APromptStyle) or
     ((AID = 'M') and (gsRevertMetaPoints in APromptStyle)) or
     ((AID <> 'M') and (gsRevertIndiPoints in APromptStyle)) then
    LPoints *= -1;

  // BasA must revert message variables
  if (gscB in LCsqStyle) and (gsBasA in APromptStyle) then
    begin
      LCsqStyle += [gscA];
      LCsqStyle -= [gscB];

      for i := 0 to Length(ATargetContingencies) -1 do
        if not ATargetContingencies[i].Meta then
          if gscA in ATargetContingencies[i].Consequence.Style then
            begin
              LPrependLoss := ATargetContingencies[i].Consequence.PrependLoss;
              LAppendiceLossSingular := ATargetContingencies[i].Consequence.AppendiceLossSingular;
              LAppendiceLossPlural := ATargetContingencies[i].Consequence.AppendiceLossPlural;
              LPrependEarn := ATargetContingencies[i].Consequence.PrependEarn;
              LAppendiceEarnSingular := ATargetContingencies[i].Consequence.AppendiceEarnSingular;
              LAppendiceEarnPlural := ATargetContingencies[i].Consequence.AppendiceEarnPlural;
              LAppendiceZero := ATargetContingencies[i].Consequence.AppendiceZero;
              Break;
            end;
    end;

  AConsequenceString := AID + '#' +
    IntToStr(LPoints) +'|'+
    GetConsequenceStyleString(LCsqStyle) +'|'+
    LPrepend +'|'+
    LPrependLoss +'|'+
    LAppendiceLossSingular +'|'+
    LAppendiceLossPlural +'|'+
    LPrependEarn  +'|'+
    LAppendiceEarnSingular +'|'+
    LAppendiceEarnPlural +'|'+
    LAppendiceZero;
  // WriteLn(AConsequenceString);
end;

function GetMessagesFromPromptStyle(APromptStyle: TPromptStyle;
  AContingencies: TContingencies): TStringList;
var
  i, j: integer;
  LID,
  LConsequence : string;
begin
  Result := TStringList.Create;

  // use downto to load metacontingency messages first
  for i := Length(AContingencies)-1 downto 0 do
    for j := 0 to AContingencies[i].Consequence.ByPlayerID.Count-1do
      begin
        LID := AContingencies[i].Consequence.ByPlayerID.Names[j];
        LConsequence := AContingencies[i].Consequence.ByPlayerID.Values[LID];

        if gsMetacontingency in APromptStyle then
          if (AContingencies[i].Fired) and AContingencies[i].Meta then
            ApplyPromptStylePointConditionTo(LConsequence, 'M',APromptStyle,AContingencies);

        if gsContingency in APromptStyle then
          if (AContingencies[i].Fired) and (not AContingencies[i].Meta) then
            ApplyPromptStylePointConditionTo(LConsequence, LID,APromptStyle,AContingencies);

        Result.Append(LConsequence);
      end;
end;

function FirstDelimitedString(S: string): string;
begin
  Result := ExtractDelimited(1,S,['#'])
end;

procedure IncLabel(ALabel: TLabel; AValue:integer);
var
  C : integer;
begin
  C := StrToInt(ALabel.Caption);
  C += AValue;
  if C > 0 then
    ALabel.Caption := IntToStr(C)
  else
    ALabel.Caption := '0';
end;

function GameActor(AZMQActor : TZMQActor) : TGameActor;
begin
  if AZMQActor.ClassType = TZMQAdmin then
    Result := gaAdmin;
  if AZMQActor.ClassType = TZMQPlayer then
    Result := gaPlayer;
  if AZMQActor.ClassType = TZMQWatcher then
    Result := gaWatcher;
end;

end.

