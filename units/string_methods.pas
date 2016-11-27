unit string_methods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LazFileUtils
  , game_actors
  , game_resources
  ;

function GetAndDelFirstValue(var S: string;Sep:Char=','):string; deprecated 'Use ExtracteDelimited from strutils instead';

function GetRowString(ARow : TGameRow) : string;
function GetRowFromString(S : string):TGameRow;

function GetColorString(C : TColor):string; overload;
function GetColorFromString(S : string): TColor;
function GetColorString(AColor : TGameColor) : string; overload;
function GetGameColorFromString(S : string) : TGameColor;

function GetPromptStyleFromString(S : string) : TPromptStyle;
function GetPromptStyleString(AStyle : TPromptStyle) : string;

function GetConsequenceStyleFromString(s : string):TGameConsequenceStyle;
function GetConsequenceStyleString(AStyle : TGameConsequenceStyle): string;
function GetConsequenceStylesFromString(S : string):TConsequenceStyle;
function GetConsequenceStylesString(CS : TConsequenceStyle): string;

function GetCriteriaString(ACriteria : TCriteria) : string;
function GetCriteriaFromString(S : string) : TCriteria;
function GetCriteriaStyleString(AStyle: TGameStyle) : string;

function GetStatusString(AStatus : TGamePlayerStatus): string;
function GetPPointsString(APPoints : TPlayerPoints) : string;
function GetChoiceString(AChoice : TPlayerChoice) : string;
function GetPointsString(APoints : TPoints) : string;
function GetEndCriteriaString(AEndCriterium:TEndConditionCriterium) : string;

function GetPlayerFromString(s: string): TPlayer;
function GetPlayerAsString(P: TPlayer): string;

implementation

uses strutils;

function GetAndDelFirstValue(var S: string;Sep:Char=','): string;
begin
  Result := Copy(S, 0, pos(Sep, S)-1);
  Delete(S, 1, pos(Sep, S));
  if Length(S) > 0 then while S[1] = Sep do Delete(S, 1, 1);
end;

function GetRowString(ARow: TGameRow): string;
begin
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
    grEven : Result := 'PAR';
    grOdd : Result := 'IMPAR';
  end;
end;

function GetRowFromString(S: string): TGameRow;
begin
  case UpperCase(S) of
    'NA', '.' , '0', 'NONE'           : Result := grNone;
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
    'PAR', 'EVEN'         : Result := grEven;
    'IMPAR', 'ODD'        : Result := grOdd;
  end;
end;

function GetColorString(AColor: TGameColor): string;
begin
  case AColor of
    gcNone :Result  :=  'INDIFERENTE';
    gcYellow :Result  :=  'AMARELO';
    gcRed :Result  :=  'VERMELHO';
    gcMagenta :Result  :=  'ROXO';
    gcBlue :Result  :=  'AZUL';
    gcGreen :Result  :=  'VERDE';
    gcDiff :Result  :=  'DIFERENTES';
    gcEqual :Result  :=  'IGUAIS';
  end;
end;

function GetGameColorFromString(S: string): TGameColor;
begin
  case UpperCase(S) of
    '.', 'INDIFERENTE', 'NONE' : Result := gcNone;
    'Y', 'AMARELO', 'YELLOW' : Result := gcYellow;
    'B', 'AZUL', 'BLUE' : Result := gcBlue;
    'G', 'VERDE', 'GREEN' : Result := gcGreen;
    'R', 'VERMELHO', 'RED'   : Result := gcRed;
    'M', 'ROXO','MAGENTA', 'VIOLETA' : Result := gcMagenta;
    '!','<>','DIFERENTES', 'DIFFERENT' : Result := gcDiff;
    '=','IGUAIS', 'EQUAL' : Result := gcEqual;
  end;
end;


function GetPromptStyleFromString(S: string): TPromptStyle;
begin
  // todos,sim,metacontingência,recuperar pontos,
  case UpperCase(S) of
    //'NENHUM','NONE': Result:=[gsNone];
    'TODOS', 'ALL' : Result:=[gsAll];
    'SIM', 'YES','S','Y': Result:=[gsYes];
    'NÃO','NAO','N' : Result:=[gsNo];
    'CONTINGÊNCIA','CONTINGENCIA','CONTINGENCY','OPERANTE', 'OPERANT': Result:=[gsContingency];
    'METACONTINGÊNCIA','METACONTINGENCIA','METACONTINGENCY','META': Result:=[gsMetacontingency];
    'RECUPERA','RECUPERAR','RECUPERAR PONTOS','RECOVER','RESETAR', 'RESET': Result:=[gsRevertPoints];
    'TIRAR DE A AO INVES DE B','TIRAR DE A AO INVÉS DE B', 'B as A' : Result:=[gsBasA];
  end;
end;

function GetPromptStyleString(AStyle: TPromptStyle): string;
var Style : TGamePromptStyle;
begin
  Result:='';
  for Style in AStyle do
    case Style of
      //gsNone: Result:= Result+'nenhum'+VV_SEP;
      gsAll: Result:= Result+'todos'+VV_SEP;
      gsYes: Result:= Result+'s'+VV_SEP;
      gsNo: Result:= Result+'n'+VV_SEP;
      gsContingency: Result:= Result+'operante'+VV_SEP;
      gsMetacontingency: Result:= Result+'meta'+VV_SEP;
      gsRevertPoints: Result:= Result+'recupera'+VV_SEP;
      gsBasA: Result:= Result+'b as a'+VV_SEP;
    end;
end;

function GetConsequenceStyleFromString(s: string): TGameConsequenceStyle;
begin
  case UpperCase(S) of
    'NADA': Result:= gscNone;
    'MENSAGEM' : Result:= gscMessage;
    'MENSAGEM A TODOS' : Result:= gscBroadcastMessage;
    'PONTOS' : Result:= gscPoints;
    'PONTOS COM VARIAÇÃO' : Result:= gscVariablePoints;
  end;
end;

function GetConsequenceStyleString(AStyle: TGameConsequenceStyle): string;
begin
  case AStyle of
    gscNone : Result:= 'NADA';
    gscMessage : Result:= 'MENSAGEM' ;
    gscBroadcastMessage : Result:= 'MENSAGEM A TODOS';
    gscPoints : Result:= 'PONTOS' ;
    gscVariablePoints : Result:= 'PONTOS COM VARIAÇÃO';
  end;
end;

function GetCriteriaString(ACriteria: TCriteria): string;
var R : TGameRow;
    C : TGameColor;
begin
  for R in ACriteria.Rows do
    Result += GetRowString(R) + VV_SEP;
  Result += '|';

  Result += GetCriteriaStyleString(ACriteria.Style)+'|';

  for C in ACriteria.Colors do
    Result += GetColorString(C) + VV_SEP;
  Result += '|';
end;

function GetCriteriaFromString(S: string): TCriteria;
var
  s1 : string;
  i : integer;
begin
  s1 := ExtractDelimited(1,S,['|']);
  Result.Rows := [];

  for i  := 1 to WordCount(s1,[#0,',']) do
    if ExtractDelimited(i,s1,[',']) <> '' then
      Result.Rows += [GetRowFromString(ExtractDelimited(i,s1,[',']))]
    else Break;

  s1 := ExtractDelimited(2,S,['|']);
  case UpperCase(s1) of
    '','INDIFERENTE', 'NONE' : Result.Style := gtNone;
    'E', 'AND' : Result.Style := gtRowsAndColors;
    'OU', 'OR' : Result.Style := gtRowsOrColors;

  end;

  s1 := ExtractDelimited(3,S,['|']);
  Result.Colors := [];
  for i  := 1 to WordCount(s1,[#0,',']) do
    if ExtractDelimited(i,s1,[',']) <> '' then
      Result.Colors += [GetGameColorFromString(ExtractDelimited(i,s1,[',']))]
    else Break;
end;

function GetCriteriaStyleString(AStyle: TGameStyle): string;
begin
  case AStyle of
    gtNone : Result := 'INDIFERENTE';
    gtRowsAndColors : Result  := 'E';
    gtRowsOrColors : Result  := 'OU';
    gtRowsOnly: Result := 'LINHAS';
    gtColorsOnly:Result := 'CORES';
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
  end;
end;

function GetColorFromString(S: string): TColor;
begin
  case S of
    'Y' : Result := ccYellow;
    'B' : Result := ccBlue;
    'G' : Result := ccGreen;
    'R' : Result := ccRed;
    'M' : Result := ccMagenta;
  end;
end;

function GetConsequenceStylesFromString(S:string):TConsequenceStyle;
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
      'P':Result+=[gscPoints];
      'V':Result+=[gscVariablePoints];
      'A':Result+=[gscA];
      'B':Result+=[gscB];
      'G':Result+=[gscG]
    end;
end;

function GetConsequenceStylesString(CS: TConsequenceStyle): string;
var ConsequenceStyle : TGameConsequenceStyle;
begin
  Result := '';
  for ConsequenceStyle in CS do
    begin
      case ConsequenceStyle of
        gscNone: Result += '0';
        gscMessage:Result += 'M';
        gscBroadcastMessage:Result += 'C';
        gscPoints:Result += 'P';
        gscVariablePoints:Result += 'V';
        gscA:Result += 'A';
        gscB:Result += 'B';
        gscG:Result += 'G';
      end;
      Result += ',';
    end;
end;

function GetEndCriteriaString(AEndCriterium: TEndConditionCriterium
  ): string;
begin
  // 2,20,10,10,
  case AEndCriterium.Value of
   gecAbsoluteCycles: Result := '0';
   gecInterlockingPorcentage: Result := '1';
   gecWhichComeFirst: Result := '2';
  end;
  Result := Result + VV_SEP;
  Result := Result + IntToStr(AEndCriterium.AbsoluteCycles) + VV_SEP;
  Result := Result + IntToStr(AEndCriterium.InterlockingPorcentage) + VV_SEP;
  Result := Result + IntToStr(AEndCriterium.LastCycles) + VV_SEP;
end;

function GetPointsString(APoints: TPoints): string;
begin
  Result := IntToStr(APoints.A) + VV_SEP;
  Result := Result + IntToStr(APoints.B) + VV_SEP;
  Result := Result + IntToStr(APoints.G) + VV_SEP;
end;

function GetChoiceString(AChoice: TPlayerChoice): string;
begin
  Result := GetRowString(AChoice.Row) + VV_SEP;
  Result := Result+ GetColorString(AChoice.Color) + VV_SEP;
end;

function GetPPointsString(APPoints: TPlayerPoints): string;
begin
  Result := IntToStr(APPoints.A)+VV_SEP+IntToStr(APPoints.B);
end;

function GetStatusString(AStatus: TGamePlayerStatus): string;
begin
  case AStatus of
   gpsWaiting: Result := 'esperando';
   gpsPlayed: Result := 'jogou';
   gpsPlaying: Result := 'jogando';
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
    Result.A := StrToInt(ExtractDelimited(1,S,[',']));
    Result.B := StrToInt(ExtractDelimited(2,S,[',']));
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
  {$IFDEF DEBUG}
    WriteLn(ExtractDelimited(1,s,['|']));
    WriteLn(ExtractDelimited(2,s,['|']));
    WriteLn(ExtractDelimited(3,s,['|']));
    WriteLn(ExtractDelimited(4,s,['|']));
    WriteLn(ExtractDelimited(5,s,['|']));
    WriteLn(ExtractDelimited(6,s,['|']));
  {$ENDIF}
  Result.ID := ExtractDelimited(1,s,['|']);
  Result.Nicname := ExtractDelimited(2,s,['|']);
  Result.Points := PointsFromString(ExtractDelimited(3,s,['|']));
  Result.Status := StatusFromString(ExtractDelimited(4,s,['|']));
  Result.Choice := ChoiceFromString(ExtractDelimited(5,s,['|']));
  Result.Turn:=StrToInt(ExtractDelimited(6,s,['|']));
end;

end.

