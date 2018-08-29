unit form_regressivecounter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormRegressiveCounter }

  TFormRegressiveCounter = class(TForm)
    GBScholarItems: TGroupBox;
    GBTokens: TGroupBox;
    ImageGroup1: TImage;
    ImageInd: TImage;
    LabelMessage: TLabel;
    LabelGroup1Count: TLabel;
    LabelTimer: TLabel;
    LabelTokens: TLabel;
    LabelTokens1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer: TTimer;
    TimerMessage: TTimer;
    procedure TimerMessageTimer(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FCounting : integer;
    FOnZeroReachedOrange: TNotifyEvent;
    FOnZeroReached : TNotifyEvent;
    FUpperTime: integer;
    procedure ZeroReachedBlack(Sender : TObject);
    procedure SetOnZeroReached(AValue: TNotifyEvent);
    procedure SetUpperTime(AValue : integer);
    procedure UpdateLabel;
  public
    procedure ShowSystemMessage(AMessage : string);
    procedure Reset;
    procedure ChangeToBlack;
    procedure Stop;
    procedure Start;
    procedure SetupConditionA;
    procedure SetupConditionB;
    property UpperTime : integer read FUpperTime write SetUpperTime;
    property OnZeroReached : TNotifyEvent read FOnZeroReached write SetOnZeroReached;
  end;

var
  FormRegressiveCounter: TFormRegressiveCounter;

implementation

{$R *.lfm}

{ TFormRegressiveCounter }

procedure TFormRegressiveCounter.TimerTimer(Sender: TObject);
begin
  Dec(FCounting);
  UpdateLabel;
  if FCounting = 0 then
  begin
    Reset;
    if Assigned(OnZeroReached) then OnZeroReached(Sender);
  end;
end;

procedure TFormRegressiveCounter.ZeroReachedBlack(Sender: TObject);
begin
  FOnZeroReached := FOnZeroReachedOrange;
  LabelGroup1Count.Font.Color := $0000A5FF;
  LabelTimer.Hide;
end;

procedure TFormRegressiveCounter.TimerMessageTimer(Sender: TObject);
begin
  TimerMessage.Enabled := False;
  Panel1.Show;
  Panel2.Show;
  LabelMessage.Hide;
  Start;
end;

procedure TFormRegressiveCounter.SetOnZeroReached(AValue: TNotifyEvent);
begin
  if FOnZeroReached=AValue then Exit;
  FOnZeroReached:=AValue;
end;

procedure TFormRegressiveCounter.SetUpperTime(AValue: integer);
begin
  if FUpperTime=AValue then Exit;
  FUpperTime:=AValue;
end;

procedure TFormRegressiveCounter.UpdateLabel;
begin
  LabelTimer.Caption := 'Tempo'+LineEnding+IntToStr(FCounting)+#32+'s';;
end;

procedure TFormRegressiveCounter.ShowSystemMessage(AMessage: string);
begin
  Panel1.Hide;
  Panel2.Hide;
  LabelMessage.Caption := AMessage;
  LabelMessage.Show;
  TimerMessage.Enabled := True;
end;

procedure TFormRegressiveCounter.Reset;
begin
  Timer.Enabled := False;
  FCounting := FUpperTime;
  UpdateLabel;
  Timer.Enabled := True;
end;

procedure TFormRegressiveCounter.ChangeToBlack;
begin
  Reset;
  FOnZeroReachedOrange := FOnZeroReached;
  FOnZeroReached := @ZeroReachedBlack;
  LabelGroup1Count.Font.Color := clBlack;
  LabelTimer.Show;
end;

procedure TFormRegressiveCounter.Stop;
begin
  Timer.Enabled := False;
  LabelTimer.Hide;
end;

procedure TFormRegressiveCounter.Start;
begin
  //LabelTimer.Show;
  Reset;
end;

procedure TFormRegressiveCounter.SetupConditionA;
begin
  Stop;
  LabelGroup1Count.Font.Color := clBlack;
end;

procedure TFormRegressiveCounter.SetupConditionB;
begin
  LabelGroup1Count.Font.Color := $0000A5FF;
  LabelTimer.Hide;
  Start;
end;

end.

