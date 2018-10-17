unit form_points;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormPoints }

  TFormPoints = class(TForm)
    GBScholarItems: TGroupBox;
    GBScholarItems1: TGroupBox;
    GBScholarItems2: TGroupBox;
    GBTokens: TGroupBox;
    ImageGroup1: TImage;
    ImageGroup2: TImage;
    ImageInd: TImage;
    LabelReserveItemsCount: TLabel;
    LabelMessage: TLabel;
    LabelItemsCount: TLabel;
    LabelReserveTokensCount: TLabel;
    LabelTokens: TLabel;
    LabelItems: TLabel;
    LabelReserveItems: TLabel;
    LabelReserveTokens: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    TimerMessage: TTimer;
    procedure TimerMessageTimer(Sender: TObject);
  private
    FItems : integer;
    FTokens: integer;
    procedure UpdateCaptions;
    procedure DecrementItems(AValue : integer);
    procedure DecrementTokens(AValue : integer);
    procedure SetupConditionA;
    procedure SetupConditionB;
    procedure SetupConditionC;
  public
    procedure CummulativeEffect(APorcentage : integer);
    procedure Decrement(AValue : integer);
    procedure ShowSystemMessage(AMessage : string);
    procedure SetupCondition;
    procedure UpdateItems;
  end;

var
  FormPoints: TFormPoints;

implementation

{$R *.lfm}

uses game_experiment, form_matrixgame;
{ TFormPoints }

procedure TFormPoints.TimerMessageTimer(Sender: TObject);
begin
  TimerMessage.Enabled := False;
  Panel1.Show;
  Panel2.Show;
  LabelMessage.Hide;
end;

procedure TFormPoints.UpdateCaptions;
begin
  LabelReserveTokensCount.Caption := FTokens.ToString;
  LabelReserveItemsCount.Caption := FItems.ToString;
end;

procedure TFormPoints.CummulativeEffect(APorcentage: integer);
var
  LItemsToIncrement : integer;
  LTokensToIncrement : integer;
begin
  case Experiment.CurrentCondition.ConditionName of
    'Condição A1', 'Condição A2', 'Condição A3' :
      begin
        LTokensToIncrement := (APorcentage*FTokens)div 100;
        Inc(FTokens, LTokensToIncrement);
      end;
    'Condição B1', 'Condição B2', 'Condição B3' :
      begin
        LItemsToIncrement := (APorcentage*FItems)div 100;
        Inc(FItems, LItemsToIncrement);
      end;
    'Condição C1', 'Condição C2', 'Condição C3' :
      begin
        LTokensToIncrement := (APorcentage*FTokens)div 100;
        Inc(FTokens, LTokensToIncrement);
        LItemsToIncrement := (APorcentage*FItems)div 100;
        Inc(FItems, LItemsToIncrement);
      end;
  end;
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
  Dec(FItems, AValue);
  UpdateCaptions;
  FormMatrixGame.UpdateNetwork('items', FItems.ToString);
end;

procedure TFormPoints.DecrementTokens(AValue: integer);
begin
  Dec(FTokens, AValue);
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
begin
  LabelItemsCount.Caption := FItems.ToString;
end;

procedure TFormPoints.SetupConditionA;
begin
  FItems := 0;
  FTokens:= 200;
  LabelReserveTokens.Show;
  LabelReserveTokensCount.Show;
  LabelReserveItems.Hide;
  LabelReserveItemsCount.Hide;
  UpdateCaptions;
end;

procedure TFormPoints.SetupConditionB;
begin
  FItems := 200;
  FTokens:= 0;
  LabelReserveTokens.Hide;
  LabelReserveTokensCount.Hide;
  LabelReserveItems.Show;
  LabelReserveItemsCount.Show;
  UpdateCaptions;
end;

procedure TFormPoints.SetupConditionC;
begin
  FItems := 200;
  FTokens:= 200;
  LabelReserveTokens.Show;
  LabelReserveTokensCount.Show;
  LabelReserveItems.Show;
  LabelReserveItemsCount.Show;
  UpdateCaptions;
end;



end.

