unit csv_writer;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, LazFileUtils;

type

  { TCSVWriter }

  TCSVWriter = class(TComponent)
  private
    FFileName: string;
    FFile: TextFile;
    FSessionNumber: integer;
    procedure Close;
    procedure UpdateFileName(ANewFileName : string);
    function OpenNoOverride(AFilename : string):string;
  public
    constructor Create(AOwner: TComponent; AFileName: String); reintroduce;
    destructor Destroy; override;
    procedure Write(AData: array of const);
  end;




implementation

{ TCSVWriter }

procedure TCSVWriter.Close;
begin
  if FFilename <> '' then
    if TextRec(FFile).Mode = 55218 then // file is opened read/write
      begin
        CloseFile(FFile);
      end
end;

procedure TCSVWriter.UpdateFileName(ANewFileName: string);
begin
  if (ANewFileName = '') or (ANewFileName = FFilename) then Exit;
  Close;
  FFileName := OpenNoOverride(ANewFileName);
end;

function TCSVWriter.OpenNoOverride(AFilename: string): string;
var
  i : Integer;
  FilePath, LExtension: string;
begin
  if AFileName <> '' then
      begin
        ForceDirectoriesUTF8(ExtractFilePath(AFilename));
        FilePath := ExtractFilePath(AFilename);
        LExtension := ExtractFileExt(AFilename);
        i := 0;

        // ensure to never override an existing file
        while FileExistsUTF8(AFilename) do begin
          Inc(i);
          AFilename := FilePath + StringOfChar(#48, 3 - Length(IntToStr(i))) + IntToStr(i) + LExtension;
        end;

        FSessionNumber := i;

        // as override is impossible, don't mind about an Assign/Rewrite conditional
        AssignFile(FFile, AFilename);
        Rewrite(FFile);
        {$ifdef DEBUG}
          WriteLn(FFile, mt_Debug + 'Saving data to:' + AFilename );
        {$endif}
        Result := AFilename;
     end;
end;

constructor TCSVWriter.Create(AOwner: TComponent; AFileName: String);
begin
  inherited Create(AOwner);
  FFilename := OpenNoOverride(AFilename);
end;

destructor TCSVWriter.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TCSVWriter.Write(AData: array of const);
begin

end;

end.

