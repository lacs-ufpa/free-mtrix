{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_actors_point;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TGamePoint }

  TGamePoint = class(TComponent)
  private
    FResultCalculated : Boolean;
    FResult: integer;
    FValue,
    FVariation : integer;
    function GetCalculatedResultAsInteger: integer;
    function GetCalculatedResultAsString: string;
    function GetResult: integer;
  public
    constructor Create(AOwner:TComponent;AValue : integer);overload;
    constructor Create(AOwner:TComponent;AValue : array of integer); overload;
    constructor Create(AOwner:TComponent;AResult : string); overload;
    function PointMessage(APrepend, APrependLoss, AAppendiceLossSingular,AAppendiceLossPlural,
      APrependEarn,AAppendiceEarnSingular,AAppendiceEarnPlural,AAppendiceZero: string; IsGroupPoint: Boolean) : string;
    property CalculateResult : integer read GetResult;
    property Value : integer read FValue;
    property Variation : integer read FVariation;
    property AsString : string read GetCalculatedResultAsString;
    property AsInteger : integer read GetCalculatedResultAsInteger;
  end;

//operator :=(I :integer) : TGamePoint;
//operator :=(A : array of integer):TGamePoint;

implementation

uses strutils;

//operator:=(I: integer):TGamePoint;
//begin
//  Result := ;
//  Result.Value := I;
//end;
//
//operator:=(A: array of integer): TGamePoint;
//begin
//  Result := TGamePoint.Create(A);
//end;

{ TCsqPoint }

function TGamePoint.GetResult: integer;
begin
  Result := FValue - FVariation + Random((2 * FVariation) + 1);
  FResult := Result;
  FResultCalculated := True;
end;

function TGamePoint.GetCalculatedResultAsInteger: integer;
begin
  if not FResultCalculated then
    FResult := GetResult;
  Result := FResult;
end;

function TGamePoint.GetCalculatedResultAsString: string;
begin
  if not FResultCalculated then
    Exception.Create('TGamePoint.GetCalculatedResultAsString Exception');
  Result := IntToStr(abs(FResult));
end;

constructor TGamePoint.Create(AOwner: TComponent; AValue: integer);
begin
  inherited Create(AOwner);
  FResultCalculated := False;
  FValue := AValue;
  FVariation:=0;
end;

constructor TGamePoint.Create(AOwner: TComponent; AValue: array of integer);
begin
  inherited Create(AOwner);
  FResultCalculated := False;
  FValue := AValue[0];
  FVariation := AValue[1];
end;

constructor TGamePoint.Create(AOwner: TComponent; AResult: string);
begin
  inherited Create(AOwner);
  FValue := 0;//does not matter here, this creation method is called by a player, result is sent by the admin
  FVariation := 0;
  FResult := StrToInt(AResult);
  FResultCalculated := True;
end;

function TGamePoint.PointMessage(APrepend,
  APrependLoss, AAppendiceLossSingular, AAppendiceLossPlural,
  APrependEarn, AAppendiceEarnSingular, AAppendiceEarnPlural,
  AAppendiceZero: string; IsGroupPoint: Boolean): string;

  procedure ReadCustomMessage;
  begin
    Result := APrepend;
    case FResult of
      -MaxInt..-2: Result += #32+APrependLoss+#32+Self.AsString+#32+AAppendiceLossPlural;
     -1 : Result += #32+APrependLoss+#32+Self.AsString+#32+AAppendiceLossSingular;
      0 : Result += #32+AAppendiceZero;
      1 : Result += #32+APrependEarn+#32+Self.AsString+#32+AAppendiceEarnSingular;
      2..MaxInt: Result += #32+APrependEarn+#32+Self.AsString+#32+AAppendiceEarnPlural;
    end;
  end;

  procedure ReadBuiltInGroupMessage;
  begin
    Result := 'You';
    case FResult of
      -MaxInt..-2: Result += #32+'removed'+#32+Self.AsString+#32+'toys for donation to children at cancer hospitals';
     -1 : Result += #32+'removed'+#32+Self.AsString+#32+'toy for donation to children at cancer hospitals';
      0 : Result += #32+'do not added nor removed any toy for donation to children at cancer hospitals';
      1 : Result += #32+'added'+#32+Self.AsString+#32+'toy for donation to children at cancer hospitals';
      2..MaxInt: Result += #32+'added'+#32+Self.AsString+#32+'toys for donation to children at cancer hospitals';
    end;
    Result += '.';
  end;

  procedure ReadBuiltInIndividualMessage;
  begin
    Result := '$NICNAME';
    case FResult of
      -MaxInt..-2: Result += #32+'lost'+#32+Self.AsString+#32+'tokens';
     -1 : Result += #32+'lost'+#32+Self.AsString+#32+'token';
      0 : Result += #32+'do not lost nor earned any token';
      1 : Result += #32+'earned'+#32+Self.AsString+#32+'token';
      2..MaxInt: Result += #32+'earned'+#32+Self.AsString+#32+'tokens';
    end;
    Result += '.';
  end;

begin
  if (APrepend <> '') then
    begin
      if (APrependLoss = '') and (AAppendiceLossSingular = '') and (AAppendiceLossPlural = '') and
         (APrependEarn = '') and (AAppendiceEarnSingular = '') and (AAppendiceEarnPlural = '') and
         (AAppendiceZero = '') then
        begin
          Result := APrepend;
          Exit;
        end;
      ReadCustomMessage;
      Exit;
    end;

  if (APrependLoss = '') and (AAppendiceLossSingular = '') and (AAppendiceLossPlural = '') and
     (APrependEarn = '') and (AAppendiceEarnSingular = '') and (AAppendiceEarnPlural = '') and
     (AAppendiceZero = '') and (APrepend = '') then
    begin
      if IsGroupPoint then
        ReadBuiltInGroupMessage
      else
        ReadBuiltInIndividualMessage;
    end;
end;


end.

