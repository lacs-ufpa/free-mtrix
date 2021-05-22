{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit string_methods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LazFileUtils
  , game_actors
  , game_resources
  ;

function Delimited(i:integer;S:string):string;
function GetAndDelFirstValue(var S: string;Sep:Char=','): string;
function GetChoiceFromString(S:string) : TPlayerChoice;
function GetChoiceString(AChoice : TPlayerChoice) : string;
function GetColorFromString(S : string): TColor;
function GetColorStringFromGameColor(AColor : TGameColor) : string;
function GetColorString(C : TColor):string;
function GetConsequenceStyleFromString(S : string):TConsequenceStyle;
function GetConsequenceStyleString(CS : TConsequenceStyle): string;
function GetCriteriaFromString(S : string) : TCriteria;
function GetCriteriaString(ACriteria : TCriteria) : string;
function GetCriteriaStyleString(AStyle: TGameStyle) : string;
function GetEndCriteriaFromString(S:string) : TEndConditionCriterium;
function GetEndCriteriaLastCyclesFromString(S:string):integer;
function GetEndCriteriaLowerPorcentageFromString(S:string):integer;
function GetEndCriteriaString(AEndCriterium:TEndConditionCriterium) : string;
function GetEndCriteriaStyleFromString(S:string):TGameEndCondition;
function GetEndCriteriaStyleString(AEndCriteriaStyle : TGameEndCondition):string;
function GetEndCriteriaUpperPorcentageFromString(S:string):integer;
function GetGameColorFromString(S : string) : TGameColor;
function GetGamePromptStyleFromString(S : string) : TGamePromptStyle;
function GetMatrixTypeFromString(S:string) : TGameMatrixType;
function GetMatrixTypeString(AMatrixType: TGameMatrixType): string;
function GetPlayerAsString(P: TPlayer): string;
function GetPlayerFromString(s: string): TPlayer;
function GetPointsFromString(S: string) : TPoints;
function GetPointsString(APoints : TPoints) : string;
function GetPPointsFromString(S:string) : TPlayerPoints;
function GetPPointsString(APPoints : TPlayerPoints) : string;
function GetPromptStyleFromString(S : string) : TPromptStyle;
function GetPromptStyleString(AStyle : TPromptStyle) : string;
function GetRowAsString(ARow : TGameRow) : string;
function GetRowFromString(S : string):TGameRow;
function GetStatusFromString(S : string): TGamePlayerStatus;
function GetStatusString(AStatus : TGamePlayerStatus): string;
function MessageHas(const A_CONST: string; AMessage: TStringList; I: ShortInt=0): Boolean;

// gui helpers

implementation

uses strutils, game_actors_helpers;

function Delimited(i: integer; S: string): string;
begin
  Result := ExtractDelimited(i, S,['|']);
end;

function GetAndDelFirstValue(var S: string;Sep:Char=','): string;
begin
  Result := Copy(S, 0, pos(Sep, S)-1);
  Delete(S, 1, pos(Sep, S));
  if Length(S) > 0 then while S[1] = Sep do Delete(S, 1, 1);
end;

function GetChoiceFromString(S:string) : TPlayerChoice;
begin
  Result.Row := GetRowFromString(ExtractDelimited(1,S,[',']));
  Result.Color := GetGameColorFromString(ExtractDelimited(2,S,[',']));
end;

function GetChoiceString(AChoice: TPlayerChoice): string;
begin
  Result := GetRowAsString(AChoice.Row) + VV_SEP;
  Result := Result+ GetColorStringFromGameColor(AChoice.Color) + VV_SEP;
end;

function GetColorFromString(S: string): TColor;
begin
  case S of
    'Y' : Result := ccYellow;
    'B' : Result := ccBlue;
    'G' : Result := ccGreen;
    'R' : Result := ccRed;
    'M' : Result := ccMagenta;
    else { do nothing };
  end;
end;

function GetColorString(C: TColor): string;
begin
  case C of
    ccYellow: Result := 'Y';
    ccBlue : Result := 'B';
    ccGreen: Result := 'G';
    ccRed: Result := 'R';
    ccMagenta: Result := 'M';
    else { do nothing };
  end;
end;

function GetColorStringFromGameColor(AColor: TGameColor): string;
begin
  Result := '';
  case AColor of
    gcNone :Result  :=  'NOTHINGELSE';
    gcYellow :Result  :=  'YELLOW';
    gcRed :Result  :=  'RED';
    gcMagenta :Result  :=  'PURPLE';
    gcBlue :Result  :=  'BLUE';
    gcGreen :Result  :=  'GREEN';
    gcDiff :Result  :=  'DIFFERENT';
    gcEqual :Result  :=  'EQUAL';
    gcThis  :Result  := 'THIS';
    gcNot : Result := 'EXCEPT';
  end;
end;


function GetConsequenceStyleFromString(S : string):TConsequenceStyle;
var
  LCount,
  i : integer;
begin
  Result := [];
  LCount := WordCount(S,[#0,',']);
  for i:= 1 to LCount do
    case ExtractDelimited(i,S,[',']) of
      '0':Result+=[gscNone];
      'M':Result+=[gscMessage];
      'C':Result+=[gscBroadcastMessage];
      'P':Result+=[gscGlobalPoints];
      'V':Result+=[gscVariablePoints];
      'A':Result+=[gscA];
      'B':Result+=[gscB];
      '1':Result+=[gscG1];
      '2':Result+=[gscG2];
      'I':Result+=[gscI];
      else { do nothing };
    end;
end;

function GetConsequenceStyleString(CS : TConsequenceStyle): string;
var ConsequenceStyle : TGameConsequenceStyle;
begin
  Result := '';
  for ConsequenceStyle in CS do
    begin
      case ConsequenceStyle of
        gscNone: Result += '0';
        gscMessage:Result += 'M';
        gscBroadcastMessage:Result += 'C';
        gscGlobalPoints:Result += 'P';
        gscVariablePoints:Result += 'V';
        gscA:Result += 'A';
        gscB:Result += 'B';
        gscG1:Result += '1';
        gscG2:Result += '2';
        gscI:Result += 'I';
        else { do nothing };
      end;
      Result += ',';
    end;
end;

function GetCriteriaFromString(S : string):TCriteria;
var
  LS : string;
  i,
  LCount: integer;
begin
  LS := ExtractDelimited(1,S,['|']);
  LCount := WordCount(LS,[#0,',']);
  Result.Rows := [];
  for i := 1 to LCount do
    Result.Rows += [GetRowFromString(ExtractDelimited(i,LS,[',']))];
   case ExtractDelimited(2,S,['|'])of
    'NONE':Result.Style:=gtNone;
    'AND':Result.Style:=gtRowsAndColors;
    'OR':Result.Style:=gtRowsOrColors;
    'ROWS':Result.Style:=gtRowsOnly;
    'COLORS':Result.Style:=gtColorsOnly;
  end;
  LS := ExtractDelimited(3,S,['|']);
  LCount := WordCount(LS,[#0,',']);
  SetLength(Result.Colors, LCount);
  for i := 1 to LCount do
    Result.Colors[i-1] := GetGameColorFromString(ExtractDelimited(i,LS,[',']));
end;

function GetCriteriaString(ACriteria: TCriteria): string;
var R : TGameRow;
    C : TGameColor;
begin
  Result := '';
  for R in ACriteria.Rows do
    Result += GetRowAsString(R) + VV_SEP;
  Result += '|';
  Result += GetCriteriaStyleString(ACriteria.Style)+'|';
  for C in ACriteria.Colors do
    Result += GetColorStringFromGameColor(C) + VV_SEP;
  Result += '|';
end;

function GetCriteriaStyleString(AStyle : TGameStyle) : string;
begin
  case AStyle of
    gtNone : Result := 'NONE';
    gtRowsAndColors : Result  := 'AND';
    gtRowsOrColors : Result  := 'OR';
    gtRowsOnly: Result := 'ROWS';
    gtColorsOnly:Result := 'COLORS';
    else { do nothing };
  end;
end;

function GetEndCriteriaFromString(S : string) : TEndConditionCriterium;
begin
  case StrToIntDef(ExtractDelimited(1,S,[',']),2) of
    0: Result.Style := gecAbsoluteCycles;
    1: Result.Style := gecInterlockingPorcentage;
    2: Result.Style := gecWhichComesFirst;
  end;
  Result.AbsoluteCyclesMax := StrToIntDef(ExtractDelimited(2,S,[',']), 100);
  Result.AbsoluteCyclesMin := StrToIntDef(ExtractDelimited(3,S,[',']), 50);
  Result.UpperInterlockingPorcentage := StrToIntDef(ExtractDelimited(4,S,[',']),80);
  Result.LowerInterlockingPorcentage := StrToIntDef(ExtractDelimited(5,S,[',']),20);
  Result.LastCycles := StrToIntDef(ExtractDelimited(6,S,[',']), 10);
end;

function GetEndCriteriaLastCyclesFromString(S: string): integer;
begin
  try
    Result := StrToInt(ExtractDelimited(3,S,[',']));
  except
    on E : Exception do
      Result := 0;
  end;
end;

function GetEndCriteriaLowerPorcentageFromString(S: string): integer;
begin
  try
    Result := StrToInt(ExtractDelimited(2,S,[',']));
  except
    on E : Exception do
      Result := 0;
  end;
end;

function GetEndCriteriaString(AEndCriterium: TEndConditionCriterium): string;
begin
  // 100,0,80,20,
  Result := GetEndCriteriaStyleString(AEndCriterium.Style);
  Result := Result + VV_SEP;
  Result := Result + IntToStr(AEndCriterium.AbsoluteCyclesMax) + VV_SEP;
  Result := Result + IntToStr(AEndCriterium.AbsoluteCyclesMin) + VV_SEP;
  Result := Result + IntToStr(AEndCriterium.UpperInterlockingPorcentage) + VV_SEP;
  Result := Result + IntToStr(AEndCriterium.LowerInterlockingPorcentage) + VV_SEP;
  Result := Result + IntToStr(AEndCriterium.LastCycles) + VV_SEP;
end;



function GetEndCriteriaStyleFromString(S: string): TGameEndCondition;
begin
  case S of
   'CYCLES', 'CICLOS': Result := gecAbsoluteCycles;
   '%': Result := gecInterlockingPorcentage;
   'WHICHEVER OCCURS FIRST', 'O QUE OCORRER PRIMEIRO': Result := gecWhichComesFirst;
   else { do nothing };
  end;
end;

function GetEndCriteriaStyleString(AEndCriteriaStyle: TGameEndCondition): string;
begin
  case AEndCriteriaStyle of
   gecAbsoluteCycles: Result := 'CYCLES';
   gecInterlockingPorcentage: Result := '%';
   gecWhichComesFirst: Result := 'WHICHEVER OCCURS FIRST';
   else { do nothing };
  end;
end;

function GetEndCriteriaUpperPorcentageFromString(S: string): integer;
begin
  try
    Result := StrToInt(ExtractDelimited(1,S,[',']));
  except
    on E : Exception do
      Result := 0;
  end;
end;

function GetGameColorFromString(S : string): TGameColor;
begin
  case UpperCase(S) of
    '.', 'INDIFERENTE', 'NONE' : Result := gcNone;
    'Y', 'AMARELO', 'YELLOW' : Result := gcYellow;
    'B', 'AZUL', 'BLUE' : Result := gcBlue;
    'G', 'VERDE', 'GREEN' : Result := gcGreen;
    'R', 'VERMELHO', 'RED'   : Result := gcRed;
    'M', 'ROXO','MAGENTA', 'VIOLETA', 'PURPLE' : Result := gcMagenta;
    '!','<>','DIFERENTES', 'DIFFERENT' : Result := gcDiff;
    '=','IGUAIS', 'EQUAL' : Result := gcEqual;
    'ESPECIFICAMENTE', 'THIS' : Result := gcThis;
    'EXCETO', 'EXCEPT' : Result := gcNot;
    else { do nothing };
  end;
end;

function GetGamePromptStyleFromString(S : string): TGamePromptStyle;
begin
  case UpperCase(S) of
    'TODOS', 'ALL' : Result := gsAll;
    'SIM', 'YES','S','Y': Result := gsYes;
    'NÃO','NAO','N', 'NO' : Result := gsNo;
    'CONTINGÊNCIA','CONTINGENCIA','CONTINGENCY','OPERANTE', 'OPERANT': Result := gsContingency;
    'METACONTINGÊNCIA','METACONTINGENCIA','METACONTINGENCY','META': Result := gsMetacontingency;
    'REVERTER', 'EXCHANGE': Result := gsRevertPoints;
    'TIRAR DE A AO INVES DE B','TIRAR DE A AO INVÉS DE B', 'B as A','B->A' : Result := gsBasA;
    'REVERTER META', 'EXCHANGE META' : Result := gsRevertMetaPoints;
    'REVERTER INDI', 'EXCHANGE INDI' : Result := gsRevertIndiPoints;
  end;
end;

function GetMatrixTypeFromString(S: string): TGameMatrixType;
var
  LCount, i: Integer;
begin
  Result := [];
  LCount := WordCount(S,[#0,',']);
  for i:= 1 to LCount do
    case ExtractDelimited(i,S,[',']) of
      'COLORS' : Result +=[gmColors];
      'ROWS':Result+=[gmRows];
      'COLUMNS':Result+=[gmColumns];
      'CIRCLES FILLED':Result+=[gmDots];
      'CIRCLES EMPTY':Result+=[gmClearDots];
      'CIRCLES BOTH':Result+=[gmDotsClearDots];
    end;
end;

function GetMatrixTypeString(AMatrixType: TGameMatrixType): string;
var
  // LCount: Integer;
  LType : TGameMatrix;
begin
  Result := '';
  for LType in AMatrixType do
    case LType of
      gmColors : Result += 'COLORS,';
      gmRows : Result += 'ROWS,';
      gmColumns : Result += 'COLUMNS,';
      gmDots : Result += 'CIRCLES FILLED,';
      gmClearDots : Result += 'CIRCLES EMPTY,';
      gmDotsClearDots : Result += 'CIRCLES BOTH,';
      else { do nothing };
    end;
end;

function GetPlayerAsString(P: TPlayer): string;
var
  i : integer;
  M : array of string;

  procedure SetM(A : array of string);
  var i : integer;
  begin
    SetLength(M,Length(A));
    for i := 0 to Length(A) -1 do
      M[i] := A[i];
  end;

  function PointsString(APPoints : TPlayerPoints) : string;
  begin
    Result := IntToStr(APPoints.A)+VV_SEP+IntToStr(APPoints.B);
  end;

  function StatusString(AStatus : TGamePlayerStatus): string;
  begin
    case AStatus of
      gpsWaiting: Result := '0';
      gpsPlaying: Result := '1';
      gpsPlayed: Result := '2';
      else { do nothing };
    end;
  end;

  function RowString(ARow: TGameRow): string;
  begin
    case ARow of
      grNone : Result := '.';
      grOne : Result := '1';
      grTwo : Result := '2';
      grThree : Result :='3';
      grFour : Result := '4';
      grFive : Result := '5';
      grSix : Result := '6';
      grSeven : Result := '7';
      grEight : Result := '8';
      grNine : Result := '9';
      grTen : Result := '0';
      grEqual : Result := 'E';
      grDiff : Result := 'D';
      else { do nothing };
    end;
  end;

  function ColorString(AColor: TGameColor): string;
  begin
    case AColor of
      gcNone :Result  :=  '0';
      gcYellow :Result  :=  '1';
      gcRed :Result  :=  '2';
      gcMagenta :Result  :=  '3';
      gcBlue :Result  :=  '4';
      gcGreen :Result  :=  '5';
      else { do nothing };
    end;
  end;

  function ChoiceString(AChoice : TPlayerChoice) : string;
  begin
    Result := RowString(AChoice.Row) + VV_SEP;
    Result := Result+ ColorString(AChoice.Color);
  end;

begin
  Result := '';
  SetM([P.ID
    , P.Nicname
    , PointsString(P.Points)
    , StatusString(P.Status)
    , ChoiceString(P.Choice)
    , IntToStr(P.Turn)
  ]);
  for i := 0 to Length(M)-1 do
    Result += M[i] + '|';
end;

function GetPlayerFromString(s: string): TPlayer;

  function RowFromString(S: string): TGameRow;
  begin
    case S of
      '.'  : Result := grNone;
      '1' : Result := grOne;
      '2' : Result := grTwo;
      '3' : Result := grThree;
      '4' : Result := grFour;
      '5' : Result := grFive;
      '6' : Result := grSix;
      '7' : Result := grSeven;
      '8' : Result := grEight;
      '9' : Result := grNine;
      '0' : Result := grTen;
      'E' : Result := grEqual;
      'D' : Result := grDiff;
    end;
  end;

  function ColorFromString(S: string): TGameColor;
  begin
    case S of
      '0'  : Result := gcNone;
      '1' : Result := gcYellow;
      '2' : Result := gcRed;
      '3' : Result := gcMagenta;
      '4' : Result := gcBlue;
      '5' : Result := gcGreen;
    end;
  end;

  function ChoiceFromString(S:string) : TPlayerChoice;
  begin
    Result.Row := RowFromString(ExtractDelimited(1,S,[',']));
    Result.Color := ColorFromString(ExtractDelimited(2,S,[',']));
  end;

  function PointsFromString(S:string) : TPlayerPoints;
  begin
    Result.A := StrToIntDef(ExtractDelimited(1,S,[',']),0);
    Result.B := StrToIntDef(ExtractDelimited(2,S,[',']),0);
  end;

  function StatusFromString(S : string): TGamePlayerStatus;
  begin
    case S of
      '0': Result := gpsWaiting;
      '1': Result := gpsPlaying;
      '2': Result := gpsPlayed;
    end;
  end;
begin
  Result := C_PLAYER_TEMPLATE;
  {$IFDEF DEBUG}
    WriteLn(ExtractDelimited(1,s,['|']));
    WriteLn(ExtractDelimited(2,s,['|']));
    WriteLn(ExtractDelimited(3,s,['|']));
    WriteLn(ExtractDelimited(4,s,['|']));
    WriteLn(ExtractDelimited(5,s,['|']));
    WriteLn(ExtractDelimited(6,s,['|']));
  {$ENDIF}
  if WordCount(S,['|']) < 6 then Exit;
  Result.ID := ExtractDelimited(1,s,['|']);
  Result.Nicname := ExtractDelimited(2,s,['|']);
  Result.Points := PointsFromString(ExtractDelimited(3,s,['|']));
  Result.Status := StatusFromString(ExtractDelimited(4,s,['|']));
  Result.Choice := ChoiceFromString(ExtractDelimited(5,s,['|']));
  Result.Turn:=StrToInt(ExtractDelimited(6,s,['|']));
end;

function GetPointsFromString(S: string) : TPoints;
begin
  Result.A := StrToIntDef(ExtractDelimited(1,S,[',']),0);
  Result.B := StrToIntDef(ExtractDelimited(2,S,[',']),0);
  Result.G1 := StrToIntDef(ExtractDelimited(3,S,[',']),0);
  Result.G2 := StrToIntDef(ExtractDelimited(4,S,[',']),0);
end;

function GetPointsString(APoints : TPoints) : string;
begin
  Result := IntToStr(APoints.A) + VV_SEP;
  Result := Result + IntToStr(APoints.B) + VV_SEP;
  Result := Result + IntToStr(APoints.G1) + VV_SEP;
  Result := Result + IntToStr(APoints.G2) + VV_SEP;
end;

function GetPPointsFromString(S:string) : TPlayerPoints;
begin
  Result.A := StrToIntDef(ExtractDelimited(1,S,[',']),0);
  Result.B := StrToIntDef(ExtractDelimited(2,S,[',']),0);
end;

function GetPPointsString(APPoints: TPlayerPoints): string;
begin
  Result := IntToStr(APPoints.A)+VV_SEP+IntToStr(APPoints.B);
end;

function GetPromptStyleFromString(S:string):TPromptStyle;
var
  i : integer;
  LCount : integer;
begin
  Result := [];
  LCount := WordCount(S,[',']);
  for i := 1 to LCount do
      Result += [GetGamePromptStyleFromString(ExtractDelimited(i,S,[',']))];
end;

function GetPromptStyleString(AStyle : TPromptStyle): string;
var Style : TGamePromptStyle;
begin
  Result:='';
  for Style in AStyle do
    case Style of
      gsAll: Result += 'ALL'+VV_SEP;
      gsYes: Result += 'YES'+VV_SEP;
      gsNo: Result += 'NO'+VV_SEP;
      gsContingency: Result += 'OPERANT'+VV_SEP;
      gsMetacontingency: Result += 'META'+VV_SEP;
      gsRevertPoints: Result += 'EXCHANGE'+VV_SEP;
      gsBasA: Result += 'B->A'+VV_SEP;
      gsRevertMetaPoints : Result += 'EXCHANGE META'+VV_SEP;
      gsRevertIndiPoints : Result += 'EXCHANGE INDI'+VV_SEP;
      else { do nothing };
    end;
end;

function GetRowAsString(ARow: TGameRow): string;
begin
  Result := '';
  case ARow of
    grNone : Result := '0';
    grOne : Result := '1';
    grTwo : Result := '2';
    grThree : Result :='3';
    grFour : Result := '4';
    grFive : Result := '5';
    grSix : Result := '6';
    grSeven : Result := '7';
    grEight : Result := '8';
    grNine : Result := '9';
    grTen : Result := '10';
    grNot : Result := 'EXCEPT';
    grEven : Result := 'EVEN';
    grOdd : Result := 'ODD';
    grNot_EVEN_ODD_ONLY : Result := 'EXCEPT_EVEN_IMP';
    grEqual : Result := 'EQUAL';
    grDiff : Result := 'DIFFERENT';
    grNot_DIFF_EQUAL_ONLY : Result := 'EXCEPT_EQUAL_DIF';
    else { do nothing };
  end;
end;

function GetRowFromString(S: string): TGameRow;
begin
  case UpperCase(S) of
    'NA', '.' , '0', 'NONE'     : Result := grNone;
    '01', '1', 'UM', 'ONE'      : Result := grOne;
    '02', '2', 'DOIS', 'TWO'    : Result := grTwo;
    '03', '3', 'TRÊS', 'THREE'  : Result := grThree;
    '04', '4', 'QUATRO', 'FOUR' : Result := grFour;
    '05', '5', 'CINCO', 'FIVE'  : Result := grFive;
    '06', '6', 'SEIS', 'SIX'    : Result := grSix;
    '07', '7', 'SETE', 'SEVEN'  : Result := grSeven;
    '08', '8', 'OITO', 'EIGHT'  : Result := grEight;
    '09', '9', 'NOVE', 'NINE'   : Result := grNine;
    '10', 'DEZ', 'TEN'    : Result := grTen;
    'EXCETO','EXCEPT'     : Result := grNot;
    'PAR', 'EVEN'         : Result := grEven;
    'IMPAR', 'ODD'        : Result := grOdd;
    'EXCETO_PAR_IMP','EXCEPT_EVEN_IMP' : Result := grNot_EVEN_ODD_ONLY;
    'IGUAIS', 'EQUAL'     : Result := grEqual;
    'DIFERENTES','DIFFERENT' : Result := grDiff;
    'EXCETO_IGL_DIF', 'EXCEPT_EQUAL_DIF'      : Result := grNot_DIFF_EQUAL_ONLY;
    else { do nothing };
  end;
end;

function GetStatusFromString(S : string): TGamePlayerStatus;
begin
  case ExtractDelimited(1,S,[',']) of
    'esperando': Result := gpsWaiting;
    'jogou': Result := gpsPlayed;
    'jogando': Result := gpsPlaying;
    else { do nothing };
  end;
end;

function GetStatusString(AStatus: TGamePlayerStatus): string;
begin
  case AStatus of
    gpsWaiting: Result := 'esperando';
    gpsPlayed: Result := 'jogou';
    gpsPlaying: Result := 'jogando';
    else { do nothing };
  end;
end;

function MessageHas(const A_CONST : string; AMessage : TStringList;
  I : ShortInt) : Boolean;
begin
  Result:= False;
  if not Assigned(AMessage) then begin
    Exit;
  end;
  Result := Pos(A_CONST,AMessage[I])>0;
end;

end.

