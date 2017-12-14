unit DataProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TDataProcessor }

  TDataProcessor = class
  private
    FData : TStringList;
    procedure DoProcess;
  protected
    procedure ProcessData; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue;
    property Data : TStringList read FData;
  end;


implementation


{ TDataProcessor }

procedure TDataProcessor.DoProcess;
begin
  try
    ProcessData;
  finally
    Free;
  end;
end;

constructor TDataProcessor.Create;
begin
  FData := TStringList.Create;
end;

destructor TDataProcessor.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TDataProcessor.Queue;
begin
  TThread.Queue(nil, @DoProcess);
end;


end.

