unit string_methods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LazFileUtils
  , game_actors
  , game_resources
  ;

function GetAndDelFirstValue(var S: string;Sep:Char=','):string;
function GetRowString(ARow : TGameRow) : string;
function GetRowFromString(S : string):TGameRow;
function GetColorString(AColor : TGameColor) : string;
function GetColorFromString(S : string) : TGameColor;
function GetPromptStyleFromString(S : string) : TPromptStyle;
function GetPromptStyleString(AStyle : TPromptStyle) : string;
function GetConsequenceStyleFromString(s:string):TGameConsequenceStyle;
function GetConsequenceStyleString(AStyle: TGameConsequenceStyle): string;
function GetResponseString(ACriteria : TCriteria) : string;
function GetResponseFromString(S: string) : TCriteria;
function GetRowColorString(C: TColor):string;

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
    '0', 'NONE'           : Result := grNone;
    '1', 'UM', 'ONE'      : Result := grOne;
    '2', 'DOIS', 'TWO'    : Result := grTwo;
    '3', 'TRÊS', 'THREE'  : Result := grThree;
    '4', 'QUATRO', 'FOUR' : Result := grFour;
    '5', 'CINCO', 'FIVE'  : Result := grFive;
    '6', 'SEIS', 'SIX'    : Result := grSix;
    '7', 'SETE', 'SEVEN'  : Result := grSeven;
    '8', 'OITO', 'EIGHT'  : Result := grEight;
    '9', 'NOVE', 'NINE'   : Result := grNine;
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

function GetColorFromString(S: string): TGameColor;
begin
  case UpperCase(S) of
    'INDIFERENTE', 'NONE' : Result := gcNone;
    'AMARELO', 'YELLOW' : Result := gcYellow;
    'VERMELHO', 'RED'   : Result := gcRed;
    'ROXO','MAGENTA', 'VIOLETA' : Result := gcMagenta;
    'AZUL', 'BLUE' : Result := gcBlue;
    'VERDE', 'GREEN' : Result := gcGreen;
    '!=','<>','DIFERENTES', 'DIFFERENT' : Result := gcDiff;
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

function GetResponseString(ACriteria : TCriteria) : string;
var R : TGameRow;
    C : TGameColor;
begin
  Result := '[';
  for R in ACriteria.Rows do
    Result += GetRowString(R) + VV_SEP;
  Result += ']';

  Result += '[';
  case ACriteria.Style of
    gtNone : Result += 'INDIFERENTE'+ VV_SEP;
    gtRowsAndColors : Result  += 'E'+ VV_SEP;
    gtRowsOrColors : Result  += 'OU'+ VV_SEP;
  end;
  Result += ']';

  Result += '[';
  for C in ACriteria.Colors do
    Result += GetColorString(C) + VV_SEP;
  Result += ']';
end;

function GetResponseFromString(S: string) : TCriteria;
var
  R : TGameRow;
  C : TGameColor;
  LS : string;
  s1 : string;
  i : integer;
begin
  LS := S + VV_SEP;
  s1 := ExtractDelimited(2,LS,['[',']']);
  Result.Rows := [];
  for i  := 0 to 10 do
    if s1 <> '' then
      Result.Rows += [GetRowFromString(UpperCase(GetAndDelFirstValue(s1)))]
    else Break;

  s1 := ExtractDelimited(4,LS,['[',']']);
  case UpperCase(GetAndDelFirstValue(s1)) of
    '','INDIFERENTE', 'NONE' : Result.Style := gtNone;
    'E', 'AND' : Result.Style := gtRowsAndColors;
    'OU', 'OR' : Result.Style := gtRowsOrColors;
  end;

  s1 := ExtractDelimited(6,LS,['[',']']);
  Result.Colors := [];
  for i  := 0 to 10 do
    if s1 <> '' then
      Result.Colors += [GetColorFromString(UpperCase(GetAndDelFirstValue(s1)))]
    else Break;
end;

function GetRowColorString(C: TColor): string;
begin
  case C of
    ccYellow: Result := 'Y';
    ccBlue : Result := 'B';
    ccGreen: Result := 'G';
    ccRed: Result := 'R';
    ccMagenta: Result := 'M';
  end;
end;

//function ValidateString(S: String): string;
////var
////  i:integer;
//begin
//  //for i:= Low(S) to High(S) do
//  //  case S[i] of
//  //    #32 : S[i] := #
//  //    #128 : S[i] := #128;
//  //
//  //  end;
//  //Result := AnsiToUtf8(S);
//end;

end.

