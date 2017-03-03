unit game_visual_experiment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls
  ;

type

  { TExperimentBox }

  TExperimentBox = class (TGroupBox)
  private
    LabelCondition,
    LabelConditionCount,
    LabelGeneration,
    LabelGenerationCount,
    LabelCycle,
    LabelCycleCount,
    LabelConditionCycle,
    LabelConditionCycleCount,
    LabelTurn,
    LabelTurnCount,
    LabelInterlock,
    LabelInterlockCount,
    LabelTargetInterlock,
    LabelTargetInterlockCount: TLabel;
  private
    function GetInterlockString(Sender: TObject):string;
  published
    //procedure EndCondition(Sender: TObject);
    //procedure EndCycle(Sender: TObject);
    //procedure EndExperiment(Sender: TObject);
    //procedure EndGeneration(Sender: TObject);
    //procedure EndTurn(Sender: TObject);

    procedure Interlocking(Sender: TObject);
    procedure StartCondition(Sender: TObject);
    procedure StartCycle(Sender: TObject);
    procedure StartExperiment(Sender: TObject);
    procedure StartGeneration(Sender: TObject);
    procedure StartTurn(Sender: TObject);
    procedure TargetInterlocking(Sender: TObject);
    procedure EndExperiment(Sender: TObject);
    //procedure WriteReport(S: string);
  public
    constructor Create(AOwner: TComponent); reintroduce;
  end;

resourcestring
  CAP_EXPERIMENT = 'Experimento';
  CAP_INTERLOCK = 'Entrelaçamentos';
  CAP_TARGET_INTERLOCK = 'Entrelaçamentos alvo';
  CAP_CONDITION = 'Condição';
  CAP_TURN = 'Turno';
  CAP_GENERATION = 'Geração';
  CAP_COND_CYCLE = 'Ciclo (Condição)';
  CAP_CYCLES = 'Ciclo';
  HINT_GENERATION_A = 'Em parênteses o número de ciclos a cada mudança de geração';

implementation

uses
  game_experiment;

{ TExperimentBox }

function TExperimentBox.GetInterlockString(Sender: TObject): string;
var
  N: Integer;
begin
  Result := '0 %';
  N := Round(TExperiment(Sender).InterlockingsInLastCycles);
  if N > 0 then
    LabelTargetInterlockCount.Caption := IntToStr(N) + ' %';
end;

procedure TExperimentBox.Interlocking(Sender: TObject);
var
  S : string;
begin
  S := LabelInterlockCount.Caption;
  LabelInterlockCount.Caption := IntToStr((StrToInt(S)+1));

  TExperiment(TComponent(Sender).Owner).InterlockingsInLastCycles;
  LabelTargetInterlockCount.Caption := GetInterlockString(TComponent(Sender).Owner);
end;

procedure TExperimentBox.StartCondition(Sender: TObject);
begin
  LabelConditionCount.Caption := TExperiment(Sender).CurrentCondition.ConditionName;
  LabelGeneration.Caption := CAP_GENERATION + ' ('+IntToStr(TExperiment(Sender).Cycles.GenerationValue)+')';
  LabelTargetInterlockCount.Caption := '0 %';
  LabelInterlockCount.Caption := '0';
end;

procedure TExperimentBox.StartCycle(Sender: TObject);
begin
  LabelCycleCount.Caption := IntToStr(TExperiment(Sender).Cycles.Global+1);
  LabelConditionCycleCount.Caption := IntToStr(TExperiment(Sender).CurrentCondition.Cycles.Count+1);
  LabelTargetInterlockCount.Caption := GetInterlockString(Sender);
end;

procedure TExperimentBox.StartExperiment(Sender: TObject);
begin
  LabelConditionCount.Caption := TExperiment(Sender).CurrentCondition.ConditionName;
  LabelCycleCount.Caption := IntToStr(TExperiment(Sender).Cycles.Global+1);
  LabelGeneration.Caption :=  CAP_GENERATION + ' ('+IntToStr(TExperiment(Sender).Cycles.GenerationValue)+')';
  LabelGenerationCount.Caption := IntToStr(TExperiment(Sender).Cycles.Generations+1);
  LabelTurnCount.Caption := IntToStr(TExperiment(Sender).CurrentCondition.Turn.Count+1);
  LabelInterlockCount.Caption := '0';
  LabelTargetInterlockCount.Caption := '0 %';
end;

procedure TExperimentBox.StartGeneration(Sender: TObject);
begin
  LabelGenerationCount.Caption := IntToStr(TExperiment(Sender).Cycles.Generations+1);
end;

procedure TExperimentBox.StartTurn(Sender: TObject);
begin
  LabelTurnCount.Caption := IntToStr(TExperiment(Sender).CurrentCondition.Turn.Count+1);
end;

procedure TExperimentBox.TargetInterlocking(Sender: TObject);
begin
  LabelTargetInterlockCount.Caption := GetInterlockString(TComponent(Sender).Owner);
end;

procedure TExperimentBox.EndExperiment(Sender: TObject);
begin
  LabelConditionCount.Caption := 'NA';
  LabelGenerationCount.Caption := 'NA';
  LabelCycleCount.Caption := 'NA';
  LabelTurnCount.Caption := 'NA';
  LabelInterlockCount.Caption := 'NA';
  LabelTargetInterlockCount.Caption := 'NA';
  LabelConditionCycle.Caption := 'NA';
  LabelConditionCycleCount.Caption:= 'NA';
end;

constructor TExperimentBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Top := 60;
  Left := 16;
  AutoSize := True;
  Caption := CAP_EXPERIMENT;
  with ChildSizing do
    begin
  	  ControlsPerLine := 2;
  	  EnlargeHorizontal := crsHomogenousChildResize;
  	  HorizontalSpacing := 30;
  	  Layout := cclLeftToRightThenTopToBottom;
  	  LeftRightSpacing := 20;
  	  TopBottomSpacing := 20;
  	  VerticalSpacing := 10;
    end;

  LabelCondition := TLabel.Create(Self);
  LabelCondition.Caption := CAP_CONDITION;
  LabelCondition.Parent := Self;

  LabelConditionCount := TLabel.Create(Self);
  LabelConditionCount.Caption := 'NA';
  LabelConditionCount.Parent := Self;

  LabelGeneration := TLabel.Create(Self);
  LabelGeneration.Caption := CAP_GENERATION;
  LabelGeneration.Hint := HINT_GENERATION_A;
  LabelGeneration.ShowHint := True;
  LabelGeneration.Parent := Self;

  LabelGenerationCount := TLabel.Create(Self);
  LabelGenerationCount.Caption := 'NA';
  LabelGenerationCount.Parent := Self;

  LabelCycle := TLabel.Create(Self);
  LabelCycle.Caption := CAP_CYCLES;
  LabelCycle.Parent := Self;

  LabelCycleCount := TLabel.Create(Self);
  LabelCycleCount.Caption := 'NA';
  LabelCycleCount.Parent := Self;

  LabelConditionCycle:= TLabel.Create(Self);
  LabelConditionCycle.Caption:= CAP_COND_CYCLE;
  LabelConditionCycle.Parent:=Self;

  LabelConditionCycleCount:= TLabel.Create(Self);
  LabelConditionCycleCount.Caption:= 'NA';
  LabelConditionCycleCount.Parent:=Self;

  LabelTurn := TLabel.Create(Self);
  LabelTurn.Caption := CAP_TURN;
  LabelTurn.Parent := Self;

  LabelTurnCount := TLabel.Create(Self);
  LabelTurnCount.Caption := 'NA';
  LabelTurnCount.Parent := Self;

  LabelInterlock := TLabel.Create(Self);
  LabelInterlock.Caption := CAP_INTERLOCK;
  LabelInterlock.Parent := Self;

  LabelInterlockCount := TLabel.Create(Self);
  LabelInterlockCount.Caption := 'NA';
  LabelInterlockCount.Parent := Self;

  LabelTargetInterlock := TLabel.Create(Self);
  LabelTargetInterlock.Caption := CAP_TARGET_INTERLOCK;
  LabelTargetInterlock.Parent := Self;

  LabelTargetInterlockCount := TLabel.Create(Self);
  LabelTargetInterlockCount.Caption := 'NA';
  LabelTargetInterlockCount.Parent := Self;
end;

end.

