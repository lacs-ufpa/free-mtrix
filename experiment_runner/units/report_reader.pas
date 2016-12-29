{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
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
    VRow : string; //helper
    constructor Create;
    destructor Destroy; override;
    function Dump : string;
    procedure Append(ARow : string);
    procedure Extend(ARowExtention : string);
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
        Result.Append(ExtractDelimited(c+2, FRows[i],[#9,#10]))
    else
      for Row in FRows do
        Result.Append(ExtractDelimited(c+2, Row,[#9,#10]));
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

function TReportReader.Dump: string;
begin
  Result := FCols.Text+LineEnding+FRows.Text;
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

procedure TReportReader.Extend(ARowExtention: string);
begin
  FRows[FRows.Count-1] := FRows[FRows.Count-1] + ARowExtention;
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

