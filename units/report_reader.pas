unit report_reader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TRowRange = record
    Low,
    High : integer;
  end;

  { TReportReader }

  TReportReader = class
  private
    FLastRowsX : integer;
    FRows : TStringList;
    FCols : TStringList;
    FRowRange: TRowRange;
    FUseRange: Boolean;
    function GetColumnOf(AName: string): TStringList;
    procedure RangeAsLastXRows;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(ARow : string);
    procedure Clean;
    procedure SetXLastRows(X:integer);
    property Range : TRowRange read FRowRange;
    property UseRange : Boolean read FUseRange write FUseRange;
    property ColumnOf[AName:string]:TStringList read GetColumnOf;
  end;

implementation

uses strutils;

{ TReportReader }

function TReportReader.GetColumnOf(AName: string): TStringList;
var
  c,
  i : integer;
  Row : string;
begin
  Result := TStringList.Create;
  c := FCols.IndexOf(AName);
  if c > -1 then
    if FUseRange and (FRowRange.Low <= FRowRange.High) and (FRowRange.Low > 0) then
      for i := FRowRange.Low to FRowRange.High do
        Result.Append(ExtractDelimited(c+1, FRows[i],[#9,#10]))
    else
      for Row in FRows do
        Result.Append(ExtractDelimited(c+1, Row,[#9,#10]));
end;

constructor TReportReader.Create;
begin
  inherited Create;
  FUseRange := False;
  FRows := TStringList.Create;
  FCols := TStringList.Create;
  FCols.Delimiter := #9;
  FCols.StrictDelimiter := True;
end;

destructor TReportReader.Destroy;
begin
  FRows.Free;
  FCols.Free;
  inherited Destroy;
end;

procedure TReportReader.Append(ARow: string);
begin
  if FCols.Count = 0 then
    FCols.DelimitedText := ARow
  else
    begin
      FRows.Append(ARow);
      RangeAsLastXRows;
    end;
end;

procedure TReportReader.Clean;
begin
  FCols.Clear;
  FRows.Clear;
end;

procedure TReportReader.SetXLastRows(X: integer);
begin
  FLastRowsX:=X;
  RangeAsLastXRows;
end;

procedure TReportReader.RangeAsLastXRows;
begin
  FRowRange.High := FRows.Count-1;
  FRowRange.Low := FRows.Count-FLastRowsX;
  {$IFDEF DEBUG}
  if FRowRange.Low > FRowRange.High then
    WriteLn('Warning: FRowRange.Low > FRowRange.High, range will not be used');

  if FRowRange.Low < 0 then
    WriteLn('Warning: FRowRange.Low < 0, range will not be used');
  {$ENDIF}
end;


end.

