unit form_points;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormPoints }

  TFormPoints = class(TForm)
    GBTotalAccumulated: TGroupBox;
    GBScholarItemsReserve: TGroupBox;
    GBTokensReserve: TGroupBox;
    GBTokens: TGroupBox;
    ImageItemsReserve: TImage;
    ImageInd: TImage;
    LabelItems: TLabel;
    LabelTokens: TLabel;
    LabelTokensCount: TLabel;
    LabelReserveTokensCount: TLabel;
    LabelReajustItems: TLabel;
    LabelReserveItemsCount: TLabel;
    LabelMessage: TLabel;
    LabelItemsCount: TLabel;
    LabelReajustTokens: TLabel;
    LabelTokensIndividual: TLabel;
    LabelReserveItems: TLabel;
    LabelReserveTokens: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    TimerMessage: TTimer;
    TimerReadjustMessage: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerReadjustMessageTimer(Sender: TObject);
    procedure TimerMessageTimer(Sender: TObject);
  private
    FItems : integer;
    FTokens: integer;
    FReserveItems : integer;
    FReserveTokens: integer;
    procedure UpdateCaptions;
    procedure DecrementItems(AValue : integer);
    procedure DecrementTokens(AValue : integer);
    procedure SetupConditionA;
    procedure SetupConditionB;
    procedure SetupConditionC;
    procedure CummulativeEffect(APorcentage : integer);
  public
    procedure UpdateCummulativeEffect;
    procedure Decrement(AValue : integer);
    procedure ShowSystemMessage(AMessage : string);
    procedure SetupCondition;
    procedure UpdateItems;
    procedure EndExperiment;
  end;

var
  FormPoints: TFormPoints;

implementation

{$R *.lfm}

uses game_experiment, form_matrixgame;
{ TFormPoints }

procedure TFormPoints.TimerReadjustMessageTimer(Sender: TObject);
begin
  FormMatrixGame.SendHideSystemMessage;
  TimerReadjustMessage.Enabled := False;
  CummulativeEffect(3);
  UpdateCaptions;
  LabelReajustItems.Hide;
  LabelReajustTokens.Hide;
  BringToFront;
  Show;
  case Experiment.CurrentCondition.ConditionName of
    'Condição A1', 'Condição A2', 'Condição A3' :
      if (FReserveTokens >= 306) or
         (FReserveTokens <= 0) then
         Experiment.ForceEndCondition;

    'Condição B1', 'Condição B2', 'Condição B3' :
      if (FReserveItems >= 306) or
         (FReserveItems <= 0) then
         Experiment.ForceEndCondition;

    'Condição C1', 'Condição C2', 'Condição C3' :
      if (FReserveTokens >= 306) or
         (FReserveTokens <= 0) or
         (FReserveItems >= 306) or
         (FReserveItems <= 0) then
         Experiment.ForceEndCondition;
  end;
end;

procedure TFormPoints.FormCreate(Sender: TObject);
begin
  FItems:=0;
  FTokens:=0;
  FReserveItems:=0;
  FReserveTokens:=0;
end;

procedure TFormPoints.TimerMessageTimer(Sender: TObject);
begin
  TimerMessage.Enabled := False;
  Panel1.Show;
  Panel2.Show;
  LabelMessage.Hide;
end;

procedure TFormPoints.UpdateCaptions;
begin
  LabelReserveTokensCount.Caption := FReserveTokens.ToString;
  LabelReserveItemsCount.Caption := FReserveItems.ToString;
  LabelItemsCount.Caption := FItems.ToString;
  LabelTokensCount.Caption := FTokens.ToString;
end;

procedure TFormPoints.CummulativeEffect(APorcentage: integer);
var
  LItemsToIncrement : integer;
  LTokensToIncrement : integer;
begin
  case Experiment.CurrentCondition.ConditionName of
    'Condição A1', 'Condição A2', 'Condição A3' :
      begin
        LTokensToIncrement := (APorcentage*FReserveTokens)div 100;
        Inc(FReserveTokens, LTokensToIncrement);
      end;
    'Condição B1', 'Condição B2', 'Condição B3' :
      begin
        LItemsToIncrement := (APorcentage*FReserveItems)div 100;
        Inc(FReserveItems, LItemsToIncrement);
      end;
    'Condição C1', 'Condição C2', 'Condição C3' :
      begin
        LTokensToIncrement := (APorcentage*FReserveTokens)div 100;
        Inc(FReserveTokens, LTokensToIncrement);
        LItemsToIncrement := (APorcentage*FReserveItems)div 100;
        Inc(FReserveItems, LItemsToIncrement);
      end;
  end;
end;

procedure TFormPoints.UpdateCummulativeEffect;
begin
  FormMatrixGame.SendSystemMessage('REAJUSTE');
  case Experiment.CurrentCondition.ConditionName of
    'Condição A1', 'Condição A2', 'Condição A3' :
      LabelReajustTokens.Show;

    'Condição B1', 'Condição B2', 'Condição B3' :
      LabelReajustItems.Show;

    'Condição C1', 'Condição C2', 'Condição C3' :
      begin
        LabelReajustTokens.Show;
        LabelReajustItems.Show;
      end;
  end;
  TimerReadjustMessage.Enabled:=True;
end;

procedure TFormPoints.Decrement(AValue: integer);
begin
  case Experiment.CurrentCondition.ConditionName of
    'Condição A1', 'Condição A2', 'Condição A3' :
      FormPoints.DecrementTokens(AValue);
    'Condição B1', 'Condição B2', 'Condição B3' :
      FormPoints.DecrementItems(AValue);
    'Condição C1', 'Condição C2', 'Condição C3' :
      begin
        FormPoints.DecrementItems(AValue);
        FormPoints.DecrementTokens(AValue);
      end;
  end;
end;

procedure TFormPoints.DecrementItems(AValue: integer);
begin
  Dec(FReserveItems, AValue);
  if FReserveItems < 0 then FReserveItems := 0;
  UpdateCaptions;
end;

procedure TFormPoints.DecrementTokens(AValue: integer);
begin
  Dec(FReserveTokens, AValue);
  if FReserveTokens < 0 then FReserveTokens := 0;
  UpdateCaptions;
end;

procedure TFormPoints.ShowSystemMessage(AMessage: string);
begin
  Panel1.Hide;
  Panel2.Hide;
  LabelMessage.Caption := AMessage;
  LabelMessage.Show;
  TimerMessage.Enabled := True;
end;

procedure TFormPoints.SetupCondition;
begin
  case Experiment.CurrentCondition.ConditionName of
    'Condição A1', 'Condição A2', 'Condição A3' :
      FormPoints.SetupConditionA;
    'Condição B1', 'Condição B2', 'Condição B3' :
      FormPoints.SetupConditionB;
    'Condição C1', 'Condição C2', 'Condição C3' :
      FormPoints.SetupConditionC;
  end;
end;

procedure TFormPoints.UpdateItems;
var
  LItems : string;
  LTokens: string;
begin
  if FReserveItems > 0 then
    Inc(FItems, FReserveItems);
  LItems := FItems.ToString;

  if FReserveTokens > 0 then
    Inc(FTokens, FReserveTokens);
  LTokens := FTokens.ToString;

  if Experiment.CurrentConditionI > 0 then
  begin
    Experiment.WriteChatLn('Items acumulados: '+ LItems + LineEnding);
    Experiment.WriteChatLn('Fichas acumuladas:'+ LTokens + LineEnding);
  end;
  LabelItemsCount.Caption := LItems;
  FormMatrixGame.UpdateNetwork('items', LItems);
end;

procedure TFormPoints.EndExperiment;
begin
  UpdateItems;
  FReserveItems:=0;
  FReserveTokens:=0;
  LabelReserveItems.Hide;
  GBScholarItemsReserve.Hide;
  LabelReserveTokens.Hide;
  GBTokensReserve.Hide;
end;

procedure TFormPoints.SetupConditionA;
begin
  UpdateItems;
  FReserveItems := 0;
  FReserveTokens:= 200;
  LabelReserveTokens.Show;
  LabelReserveTokensCount.Show;
  LabelReserveItems.Hide;
  LabelReserveItemsCount.Hide;
  GBScholarItemsReserve.Hide;
  UpdateCaptions;
end;

procedure TFormPoints.SetupConditionB;
begin
  UpdateItems;
  FReserveItems := 200;
  FReserveTokens:= 0;
  LabelReserveTokens.Hide;
  LabelReserveTokensCount.Hide;
  LabelReserveItems.Show;
  LabelReserveItemsCount.Show;
  GBScholarItemsReserve.Show;
  UpdateCaptions;
end;

procedure TFormPoints.SetupConditionC;
begin
  UpdateItems;
  FReserveItems := 200;
  FReserveTokens:= 200;
  LabelReserveTokens.Show;
  LabelReserveTokensCount.Show;
  LabelReserveItems.Show;
  LabelReserveItemsCount.Show;
  GBScholarItemsReserve.Show;
  UpdateCaptions;
end;



end.

