{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_visual_experiment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls
  ;

type

  { TGroupBoxExperiment }

  TGroupBoxExperiment = class (TGroupBox)
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
    LabelGroup1,
    LabelGroup1Count,
    LabelGroup1Global,
    LabelGroup1GlobalCount,
    LabelTargetInterlockCount: TLabel;
  private
    function GetInterlockString(Sender: TObject):string;
  published
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
  //CAP_EXPERIMENT = 'Experiment';
  //CAP_INTERLOCK = 'Interlockings';
  //CAP_TARGET_INTERLOCK = 'Target interlock';
  //CAP_CONDITION = 'Condition';
  //CAP_TURN = 'Turn';
  //CAP_GENERATION = 'Generation';
  //CAP_COND_CYCLE = 'Cycle (Condition)';
  //CAP_CYCLES = 'Cycle (Global)';
  //CAP_NA = 'NA';
  //CAP_GROUP_POINTS_GLOBAL = 'Group Points (Global)';
  //CAP_GROUP_POINTS_CONDITION =  'Group Points (Condition)';
  //HINT_GENERATION_A = 'Inside parentheses, it is the number of cycles of the current generation.';
  CAP_EXPERIMENT = 'Experimento';
  CAP_INTERLOCK = 'Entrelaçamentos';
  CAP_TARGET_INTERLOCK = 'Entrelaçamento alvo';
  CAP_CONDITION = 'Condição';
  CAP_TURN = 'Turno';
  CAP_GENERATION = 'Geração';
  CAP_COND_CYCLE = 'Ciclo (Condição)';
  CAP_CYCLES = 'Ciclo (Global)';
  CAP_NA = 'NA';
  CAP_GROUP_POINTS_GLOBAL = 'Pontos do grupo (Global)';
  CAP_GROUP_POINTS_CONDITION =  'Pontos do grupo (Condição)';
  HINT_GENERATION_A = 'Número de ciclos na geração atual entre parênteses.';

implementation

uses
  game_actors
  , game_experiment
  , game_visual_board
  ;

{ TGroupBoxExperiment }

function TGroupBoxExperiment.GetInterlockString(Sender: TObject): string;
var
  N: Integer;
begin
  Result := '0 %';
  N := Abs(Round(TExperiment(Sender).InterlockingsInLastCycles));
  if N > 0 then
    Result := IntToStr(N) + ' %';
end;

procedure TGroupBoxExperiment.Interlocking(Sender: TObject);
var
  S : string;
begin
  S := LabelInterlockCount.Caption;
  LabelInterlockCount.Caption := IntToStr((StrToInt(S)+1));
  LabelTargetInterlockCount.Caption := GetInterlockString(TComponent(Sender).Owner);
end;

procedure TGroupBoxExperiment.StartCondition(Sender: TObject);
begin
  LabelConditionCount.Caption := TExperiment(Sender).CurrentCondition.ConditionName;
  LabelConditionCycleCount.Caption := IntToStr(TExperiment(Sender).CurrentCondition.Cycles.Count+1);
  LabelGeneration.Caption := CAP_GENERATION + ' ('+IntToStr(TExperiment(Sender).Cycles.GenerationValue)+')';
  LabelTargetInterlockCount.Caption := '0 %';
  LabelInterlockCount.Caption := '0';
  //LabelGroup1Count.Caption := TExperiment(Sender).CurrentCondition.Points.Count.G1.ToString;
  //LabelGroup1GlobalCount.Caption := TExperiment(Sender).CurrentCondition.Points.Count.G2.ToString;
end;

procedure TGroupBoxExperiment.StartCycle(Sender: TObject);
begin
  LabelCycleCount.Caption := IntToStr(TExperiment(Sender).Cycles.Global+1);
  LabelConditionCycleCount.Caption := IntToStr(TExperiment(Sender).CurrentCondition.Cycles.Count+1);
  LabelTargetInterlockCount.Caption := GetInterlockString(Sender);
  LabelGroup1Count.Caption := TExperiment(Sender).CurrentCondition.Points.Count.G1.ToString;
  LabelGroup1GlobalCount.Caption := TExperiment(Sender).GlobalPoints(gscG1).ToString;
end;

procedure TGroupBoxExperiment.StartExperiment(Sender: TObject);
begin
  LabelConditionCount.Caption := TExperiment(Sender).CurrentCondition.ConditionName;
  LabelConditionCycleCount.Caption := (TExperiment(Sender).CurrentCondition.Cycles.Count+1).ToString;
  LabelCycleCount.Caption := (TExperiment(Sender).Cycles.Global+1).ToString;
  LabelGeneration.Caption :=  CAP_GENERATION + ' ('+TExperiment(Sender).Cycles.GenerationValue.ToString+')';
  LabelGenerationCount.Caption := (TExperiment(Sender).Cycles.Generations+1).ToString;
  LabelTurnCount.Caption := (TExperiment(Sender).CurrentCondition.Turn.Count+1).ToString;


  LabelGroup1Count.Caption := TExperiment(Sender).CurrentCondition.Points.Count.G1.ToString;
  LabelGroup1GlobalCount.Caption := TExperiment(Sender).GlobalPoints(gscG1).ToString;

  LabelInterlockCount.Caption := '0';
  LabelTargetInterlockCount.Caption := '0 %';
end;

procedure TGroupBoxExperiment.StartGeneration(Sender: TObject);
begin
  LabelGenerationCount.Caption := IntToStr(TExperiment(Sender).Cycles.Generations+1);
end;

procedure TGroupBoxExperiment.StartTurn(Sender: TObject);
begin
  LabelTurnCount.Caption := IntToStr(TExperiment(Sender).CurrentCondition.Turn.Count+1);
end;

procedure TGroupBoxExperiment.TargetInterlocking(Sender: TObject);
begin
  LabelTargetInterlockCount.Caption := GetInterlockString(TComponent(Sender).Owner);
end;

procedure TGroupBoxExperiment.EndExperiment(Sender: TObject);
begin
  LabelConditionCount.Caption := CAP_NA;
  LabelGenerationCount.Caption := CAP_NA;
  LabelCycleCount.Caption := CAP_NA;
  LabelTurnCount.Caption := CAP_NA;
  LabelInterlockCount.Caption := CAP_NA;
  LabelTargetInterlockCount.Caption := CAP_NA;
  LabelConditionCycle.Caption := CAP_NA;
  LabelConditionCycleCount.Caption:= CAP_NA;
  LabelGroup1Count.Caption:=CAP_NA;
  LabelGroup1GlobalCount.Caption:=CAP_NA;
end;

constructor TGroupBoxExperiment.Create(AOwner: TComponent);
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
  LabelConditionCount.Caption := CAP_NA;
  LabelConditionCount.Parent := Self;

  LabelGeneration := TLabel.Create(Self);
  LabelGeneration.Caption := CAP_GENERATION;
  LabelGeneration.Hint := HINT_GENERATION_A;
  LabelGeneration.ShowHint := True;
  LabelGeneration.Parent := Self;

  LabelGenerationCount := TLabel.Create(Self);
  LabelGenerationCount.Caption := CAP_NA;
  LabelGenerationCount.Parent := Self;

  LabelCycle := TLabel.Create(Self);
  LabelCycle.Caption := CAP_CYCLES;
  LabelCycle.Parent := Self;

  LabelCycleCount := TLabel.Create(Self);
  LabelCycleCount.Caption := CAP_NA;
  LabelCycleCount.Parent := Self;

  LabelConditionCycle:= TLabel.Create(Self);
  LabelConditionCycle.Caption:= CAP_COND_CYCLE;
  LabelConditionCycle.Parent:=Self;

  LabelConditionCycleCount:= TLabel.Create(Self);
  LabelConditionCycleCount.Caption:= CAP_NA;
  LabelConditionCycleCount.Parent:=Self;

  LabelTurn := TLabel.Create(Self);
  LabelTurn.Caption := CAP_TURN;
  LabelTurn.Parent := Self;

  LabelTurnCount := TLabel.Create(Self);
  LabelTurnCount.Caption := CAP_NA;
  LabelTurnCount.Parent := Self;

  LabelInterlock := TLabel.Create(Self);
  LabelInterlock.Caption := CAP_INTERLOCK;
  LabelInterlock.Parent := Self;

  LabelInterlockCount := TLabel.Create(Self);
  LabelInterlockCount.Caption := CAP_NA;
  LabelInterlockCount.Parent := Self;

  LabelTargetInterlock := TLabel.Create(Self);
  LabelTargetInterlock.Caption := CAP_TARGET_INTERLOCK;
  LabelTargetInterlock.Parent := Self;

  LabelTargetInterlockCount := TLabel.Create(Self);
  LabelTargetInterlockCount.Caption := CAP_NA;
  LabelTargetInterlockCount.Parent := Self;

  LabelGroup1 := TLabel.Create(Self);
  LabelGroup1.Caption := CAP_GROUP_POINTS_CONDITION;
  LabelGroup1.Parent := Self;

  LabelGroup1Count := TLabel.Create(Self);
  LabelGroup1Count.Caption := CAP_NA;
  LabelGroup1Count.Parent := Self;

  LabelGroup1Global := TLabel.Create(Self);
  LabelGroup1Global.Caption := CAP_GROUP_POINTS_GLOBAL;
  LabelGroup1Global.Parent := Self;

  LabelGroup1GlobalCount := TLabel.Create(Self);
  LabelGroup1GlobalCount.Caption := CAP_NA;
  LabelGroup1GlobalCount.Parent := Self;

  if Owner is TGameBoard then
  begin
    TGameBoard(Owner).OnEndExperiment := @EndExperiment;
    TGameBoard(Owner).OnInterlocking := @Interlocking;
    TGameBoard(Owner).OnStartCondition := @StartCondition;
    TGameBoard(Owner).OnStartCycle := @StartCycle;
    TGameBoard(Owner).OnStartExperiment := @StartExperiment;
    TGameBoard(Owner).OnStartGeneration := @StartGeneration;
    TGameBoard(Owner).OnStartTurn := @StartTurn;
    TGameBoard(Owner).OnTargetInterlocking := @TargetInterlocking;
    //TGameBoard(Owner).OnConsequence := @Consequence;
    //TGameBoard(Owner).OnEndChoice := @EndChoice;
    //TGameBoard(Owner).OnEndCondition := @EndCondition;
    //TGameBoard(Owner).OnEndCycle := @EndCycle;
    //TGameBoard(Owner).OnEndGeneration := @EndGeneration;
    //TGameBoard(Owner).OnEndTurn := @EndTurn;
    //TGameBoard(Owner).OnCleanEvent := @CleanEvent;
    //TGameBoard(Owner).OnPlayerExit := @PlayerExit;
  end;
end;

end.

