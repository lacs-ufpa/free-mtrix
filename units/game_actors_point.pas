unit game_actors_point;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TGamePoint }

  TGamePoint = class(TComponent)
  private
    FValue,
    FVariation : integer;
    function GetValue: integer;
    procedure SetValue(AValue: integer);
  public
    //Cycles : integer;
    constructor Create(AValue : integer);overload;
    constructor Create(AValue : array of integer); overload;
    property Value : integer read GetValue write SetValue;
    property Variation : integer read FVariation write FVariation;
  end;

//operator :=(I :integer) : TGamePoint;
//operator :=(A : array of integer):TGamePoint;
//
implementation

//operator:=(I: integer):TGamePoint;
//begin
//  Result := TGamePoint.Create(I);
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
end;

procedure TGamePoint.SetValue(AValue: integer);
begin
  FValue := AValue;
end;

constructor TGamePoint.Create(AValue: integer);
begin
  FValue := AValue;
end;

constructor TGamePoint.Create(AValue : array of integer);
begin
  FValue := AValue[0];
  FVariation := AValue[1];
  //Cycles := AValue[2];
end;

end.

