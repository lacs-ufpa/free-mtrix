unit form_points;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  TExperimentType = (Experiment1, Experiment2);

  { TFormPoints }

  TFormPoints = class(TForm)
    GBScholarItemsReserve: TGroupBox;
    GBTokensReserve: TGroupBox;
    GBTokens: TGroupBox;
    GBCanTalk: TGroupBox;
    ImageInd: TImage;
    ImageItemsReserve: TImage;
    LabelCanTalkText: TLabel;
    LabelReadjustItems: TLabel;
    LabelReserveItems: TLabel;
    LabelReserveItemsCount: TLabel;
    LabelMessage: TLabel;
    LabelCanTalkCount: TLabel;
    LabelTokensIndividual: TLabel;
    MemoInstructions: TMemo;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    TimerCanTalkLabel: TTimer;
    TimerMessage: TTimer;
    TimerCanTalk: TTimer;
    TimerReadjustMessage: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerCanTalkTimer(Sender: TObject);
    procedure TimerCanTalkLabelTimer(Sender: TObject);
    procedure TimerReadjustMessageTimer(Sender: TObject);
    procedure TimerMessageTimer(Sender: TObject);
  private
    FCanTalk : Boolean;
    FCanTalkDuration : TTime;
    FCanTalkStartTime : TTime;
    FCanTalkCount : integer;
    function GetReserveItems: integer;
    function Group1Items : integer;
    procedure SetupConditionA;
    procedure SetupConditionB;
    procedure SetupConditionC;
  public
    procedure UpdateCummulativeEffect(AValue : integer);
    procedure UpdateCanTalkCount;
    //procedure IncrementReserveItems(AValue : integer);
    procedure ShowSystemMessage(AMessage : string);
    procedure SetupCondition;
    //procedure UpdateItems;
    procedure EndExperiment;
    procedure UpdateCaptions;
    property ReserveItems : integer read GetReserveItems;
  end;

resourcestring
  RSCanNotTalk = 'Agora vocês' + LineEnding + 'NÃO' + LineEnding +  'podem conversar';
  RSCanTalk = 'Agora vocês' + LineEnding +  'podem conversar';

var
  FormPoints: TFormPoints;
  ExperimentType : TExperimentType = Experiment1;

implementation

{$R *.lfm}

uses game_experiment, form_matrixgame, PlaySound;

{ TFormPoints }

procedure TFormPoints.TimerReadjustMessageTimer(Sender: TObject);
begin
  FormMatrixGame.SendHideSystemMessage;
  TimerReadjustMessage.Enabled := False;
  LabelReadjustItems.Hide;
  UpdateCaptions;
end;

procedure TFormPoints.FormCreate(Sender: TObject);
begin
  FCanTalk := False;
  DefaultFormatSettings.LongTimeFormat:='mm:ss';
  FCanTalkDuration := EncodeTime(0,2,0,0);
end;

procedure TFormPoints.TimerCanTalkTimer(Sender: TObject);
begin
  Play(CanNotTalkBuffer);
  TimerCanTalk.Enabled := False;
  TimerCanTalkLabel.Enabled:=False;
  LabelCanTalkText.Caption := RSCanNotTalk;
  LabelCanTalkCount.Caption := '';
end;

procedure TFormPoints.TimerCanTalkLabelTimer(Sender: TObject);
var
  LTime : TTime;
begin
  LTime := FCanTalkDuration-(Now - FCanTalkStartTime);
  LabelCanTalkCount.Caption := TimeToStr(LTime);
end;


procedure TFormPoints.TimerMessageTimer(Sender: TObject);
begin
  TimerMessage.Enabled := False;
  PanelRight.Show;
  PanelLeft.Show;
  LabelMessage.Hide;
end;

function TFormPoints.Group1Items: integer;
begin
  Result := Experiment.AllItems;
end;

function TFormPoints.GetReserveItems: integer;
begin
  Result := Experiment.CurrentCondition.Points.Count.G1;
end;

procedure TFormPoints.UpdateCaptions;
begin
  if not TimerReadjustMessage.Enabled then
  if ReserveItems < 0 then
    LabelReserveItemsCount.Caption := '0'
  else
    LabelReserveItemsCount.Caption := ReserveItems.ToString;
end;

procedure TFormPoints.UpdateCummulativeEffect(AValue : integer);
begin
  FormMatrixGame.SendSystemMessage('REAJUSTE');
  LabelReserveItemsCount.Caption:=AValue.ToString;
  LabelReadjustItems.Show;
  TimerReadjustMessage.Enabled:=True;
end;

procedure TFormPoints.UpdateCanTalkCount;
begin
  if Experiment.CurrentCondition.CyclesToTalk > 0 then
  begin
    if not FCanTalk then
    begin
      Inc(FCanTalkCount);
      if FCanTalkCount = Experiment.CurrentCondition.CyclesToTalk then
      begin
        Play(CanTalkBuffer);
        FCanTalk := True;
        FCanTalkCount := 0;
        LabelCanTalkText.Caption := RSCanTalk;
        TimerCanTalk.Enabled := True;
        TimerCanTalkLabel.Enabled:=True;
        FCanTalkStartTime := Now;
      end;
    end;
  end;
end;

procedure TFormPoints.ShowSystemMessage(AMessage: string);
begin
  PanelRight.Hide;
  PanelLeft.Hide;
  LabelMessage.Caption := AMessage;
  LabelMessage.Show;
  TimerMessage.Enabled := True;
end;

procedure TFormPoints.SetupCondition;
begin
  if Experiment.CurrentConditionI <> 0 then
    Play(NewConditionBuffer);

  FCanTalkCount := 0;
  TimerCanTalk.Enabled := False;
  TimerCanTalkLabel.Enabled:=False;
  LabelCanTalkText.Caption := RSCanNotTalk;
  LabelCanTalkCount.Caption := '';

  MemoInstructions.Lines.Clear;
  MemoInstructions.Lines.Text := Experiment.CurrentCondition.Instruction;
  FCanTalk := False;
  if Experiment.CurrentCondition.CyclesToTalk > 0 then
    begin
      LabelCanTalkText.Show;
      LabelCanTalkCount.Show;
      LabelCanTalkText.Caption := RSCanNotTalk;
      LabelCanTalkCount.Caption := '';
    end
  else
    begin
      LabelCanTalkText.Hide;
      LabelCanTalkCount.Hide;
    end;
  case Experiment.CurrentCondition.ConditionName of
    'A1', 'A2', 'A3' :
      FormPoints.SetupConditionA;
    'B1', 'B2', 'B3' :
      FormPoints.SetupConditionB;
    'C1', 'C2', 'C3' :
      FormPoints.SetupConditionC;
  end;
  UpdateCaptions;
end;

procedure TFormPoints.EndExperiment;
begin
  //UpdateItems;
  TimerCanTalk.Enabled := False;
  TimerCanTalkLabel.Enabled:=False;
  LabelCanTalkText.Caption := RSCanNotTalk;
  LabelCanTalkCount.Caption := '';
end;

procedure TFormPoints.SetupConditionA;
begin
  case ExperimentType of
    Experiment1 :
      begin
        LabelReserveItems.Hide;
        GBScholarItemsReserve.Hide;
      end;
    Experiment2 :
      begin
        LabelReserveItems.Show;
        GBScholarItemsReserve.Show;
      end;
  end;
end;

procedure TFormPoints.SetupConditionB;
begin
  LabelReserveItems.Show;
  GBScholarItemsReserve.Show;
end;

procedure TFormPoints.SetupConditionC;
begin
  LabelReserveItems.Show;
  GBScholarItemsReserve.Show;
end;



end.

