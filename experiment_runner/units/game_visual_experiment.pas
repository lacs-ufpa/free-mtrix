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
    LabelGroup1,
    LabelGroup1Count,
    LabelGroup1Global,
    LabelGroup1GlobalCount,
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
  CAP_EXPERIMENT = 'Experiment';
  CAP_INTERLOCK = 'Interlockings';
  CAP_TARGET_INTERLOCK = 'Target interlock';
  CAP_CONDITION = 'Condition';
  CAP_TURN = 'Turn';
  CAP_GENERATION = 'Generation';
  CAP_COND_CYCLE = 'Cycle (Condition)';
  CAP_CYCLES = 'Cycle (Global)';
  HINT_GENERATION_A = 'Inside parentheses, it is the number of cycles of the current generation.';

implementation

uses
  game_actors,
  game_experiment;

{ TExperimentBox }

function TExperimentBox.GetInterlockString(Sender: TObject): string;
var
  N: Integer;
begin
  Result := '0 %';
  N := Abs(Round(TExperiment(Sender).InterlockingsInLastCycles));
  if N > 0 then
    Result := IntToStr(N) + ' %';
end;

procedure TExperimentBox.Interlocking(Sender: TObject);
var
  S : string;
begin
  S := LabelInterlockCount.Caption;
  LabelInterlockCount.Caption := IntToStr((StrToInt(S)+1));
  LabelTargetInterlockCount.Caption := GetInterlockString(TComponent(Sender).Owner);
end;

procedure TExperimentBox.StartCondition(Sender: TObject);
begin
  LabelConditionCount.Caption := TExperiment(Sender).CurrentCondition.ConditionName;
  LabelConditionCycleCount.Caption := IntToStr(TExperiment(Sender).CurrentCondition.Cycles.Count+1);
  LabelGeneration.Caption := CAP_GENERATION + ' ('+IntToStr(TExperiment(Sender).Cycles.GenerationValue)+')';
  LabelTargetInterlockCount.Caption := '0 %';
  LabelInterlockCount.Caption := '0';
  //LabelGroup1Count.Caption := TExperiment(Sender).CurrentCondition.Points.Count.G1.ToString;
  //LabelGroup1GlobalCount.Caption := TExperiment(Sender).CurrentCondition.Points.Count.G2.ToString;
end;

procedure TExperimentBox.StartCycle(Sender: TObject);
begin
  LabelCycleCount.Caption := IntToStr(TExperiment(Sender).Cycles.Global+1);
  LabelConditionCycleCount.Caption := IntToStr(TExperiment(Sender).CurrentCondition.Cycles.Count+1);
  LabelTargetInterlockCount.Caption := GetInterlockString(Sender);
  LabelGroup1Count.Caption := TExperiment(Sender).CurrentCondition.Points.Count.G1.ToString;
  LabelGroup1GlobalCount.Caption := TExperiment(Sender).GlobalPoints(gscG1).ToString;
end;

procedure TExperimentBox.StartExperiment(Sender: TObject);
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
  LabelGroup1Count.Caption:='NA';
  LabelGroup1GlobalCount.Caption:='NA';
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

  LabelGroup1 := TLabel.Create(Self);
  LabelGroup1.Caption := 'Group Points (Condition)';
  LabelGroup1.Parent := Self;

  LabelGroup1Count := TLabel.Create(Self);
  LabelGroup1Count.Caption := 'NA';
  LabelGroup1Count.Parent := Self;

  LabelGroup1Global := TLabel.Create(Self);
  LabelGroup1Global.Caption := 'Group Points (Global)';
  LabelGroup1Global.Parent := Self;

  LabelGroup1GlobalCount := TLabel.Create(Self);
  LabelGroup1GlobalCount.Caption := 'NA';
  LabelGroup1GlobalCount.Parent := Self;


end;

end.

