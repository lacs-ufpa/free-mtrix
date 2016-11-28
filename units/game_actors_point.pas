{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

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
    FResult: integer;
    FValue,
    FVariation : integer;
    function GetResult: integer;
    function GetResultAsString: string;
    function GetValue: integer;
  public
    //Cycles : integer; // specify when present points regarding condition cycles
    constructor Create(AOwner:TComponent;AValue : integer);overload;
    constructor Create(AOwner:TComponent;AValue : array of integer); overload;
    constructor Create(AOwner:TComponent;AResult : string); overload;
    function PointMessage(APrepend, AAppendicePlural, AAppendiceSingular: string; IsGroupPoint: Boolean) : string;
    property ValueWithVariation : integer read GetValue write FValue;
    property Variation : integer read FVariation write FVariation;

    property AsString : string read GetResultAsString;
    property ResultAsInteger : integer read GetResult;
  end;

//operator :=(I :integer) : TGamePoint;
//operator :=(A : array of integer):TGamePoint;
//
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

function TGamePoint.GetValue: integer;
begin
  Result := FValue - FVariation + Random((2 * FVariation) + 1);
  FResult := Result;
end;

function TGamePoint.GetResult: integer;
begin
  Result := FResult;
end;

function TGamePoint.GetResultAsString: string;
begin
  Result := IntToStr(FResult);
end;

constructor TGamePoint.Create(AOwner: TComponent; AValue: integer);
begin
  inherited Create(AOwner);
  FValue := AValue;
  FVariation:=0;
end;

constructor TGamePoint.Create(AOwner: TComponent; AValue: array of integer);
begin
  inherited Create(AOwner);
  FValue := AValue[0];
  FVariation := AValue[1];
end;

constructor TGamePoint.Create(AOwner: TComponent; AResult: string);
begin
  FValue := 0;//does not matter here, this creation method is called by a player, admin sent a result
  FVariation := 0;
  FResult := StrToInt(AResult);
end;

function TGamePoint.PointMessage(APrepend, AAppendicePlural, AAppendiceSingular: string; IsGroupPoint: Boolean): string;
begin
  if IsGroupPoint then
    begin
      if APrepend = '' then
        Result := 'Vocês'
      else
        Result := APrepend;

      if (AAppendiceSingular = '') or (AAppendicePlural = '') then
        begin
          case FResult of
            -MaxInt..-2: Result += ' produziram a perda de '+Self.AsString+ ' pontos para o grupo';
           -1 : Result += ' produziram a perda de  1 ponto para o grupo';
            0 : Result += ' pontos do grupo não foram produzidos nem perdidos';
            1 : Result += ' produziram 1 ponto para o grupo';
            2..MaxInt: Result += ' produziram '+Self.AsString+' pontos para o grupo'
          end;
        end
      else
        begin
          case FResult of
            -MaxInt..-2: Result += ' produziram a perda de '+Self.AsString+ ' ' + AAppendicePlural;
           -1 : Result += ' produziram a perda de  1'+ ' ' + AAppendiceSingular;
            0 : Result += ' não produziram ' + AAppendicePlural;
            1 : Result += ' produziram 1 ' + AAppendiceSingular;
            2..MaxInt: Result += ' produziram '+Self.AsString+ ' ' + AAppendicePlural;
          end;
        end;
    end
  else
    begin
      if APrepend = '' then
        Result := 'Alguém'
      else
        Result := APrepend;

      if (AAppendiceSingular = '') or (AAppendicePlural = '') then
        begin
          case FResult of
            -MaxInt..-2: Result += ' perdeu '+Self.AsString+ ' pontos';
           -1 : Result += ' perdeu 1 ponto';
            0 : Result += ' não perdeu nem ganhou pontos';
            1 : Result += ' ganhou 1 ponto';
            2..MaxInt: Result += ' ganhou '+Self.AsString+' pontos'
          end;
        end
      else
        begin
          case FResult of
            -MaxInt..-2: Result += ' perdeu '+Self.AsString+ ' ' + AAppendicePlural;
           -1 : Result += ' ponto  1 ' + AAppendiceSingular;
            0 : Result += ' não perdeu nem ganhou ' + AAppendicePlural;
            1 : Result += ' ganhou 1 ' + AAppendiceSingular;
            2..MaxInt: Result += ' ganhou '+Self.AsString+ ' ' + AAppendicePlural;
          end;
        end;
    end;
  Result += '.';
end;


end.

