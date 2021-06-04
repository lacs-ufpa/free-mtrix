{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_experiment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , game_actors
  , game_report
  ;

type

  { TExperiment }

  TNotifyOnWriteReport = procedure (S : string) of object;

  TExperimentState = (xsNone,xsWaiting,xsRunning,xsPaused,xsCanceled);

  TExperiment = class(TComponent)
  private
    FGameReport:TGameReport;
    FGameActor : TGameActor;
    FABPoints: Boolean;
    FExperimentAim  : string;
    FExperimentName : string;
    FFilename       : string;
    FResearcher     : string;
    FGenPlayersAsNeeded : Boolean;
    FResearcherCanChat: Boolean;
    FResearcherCanPlay: Boolean;
    FSendChatHistoryForNewPlayers: Boolean;
    FShowChat: Boolean;
    FMatrixType: TGameMatrixType;
  private
    FPlayers : TPlayers;
    FOldPlayers : TPlayers;
    FCurrentCondition : integer;
    FConditions : TConditions;
    FState: TExperimentState;
    FRandomTurns : TStringList;
    FCycles : TCycles;
    function GetCondition(I : Integer): TCondition;
    function GetConditionsCount: integer;
    function GetContingenciesCount(C: integer): integer;
    function GetContingency(ACondition, I : integer): TContingency;
    function GetNextTurn : string;
    function GetNextTurnPlayerID: string;
    function GetNextCondition:string;
    function AliasPlayerAsString(P: TPlayer): string;
    function AliasPlayerFromString(s : string): TPlayer;
    function GetPlayer(I : integer): TPlayer; overload;
    function GetPlayer(AID : string): TPlayer; overload;
    function GetPlayerToKick: string;
    function GetPlayerIndexFromID(AID : string): integer;
    function GetPlayerIsPlaying(AID : string): Boolean;
    function GetPlayersCount: integer;
    function GetInterlockingPorcentageInLastCycles:real;
    function GetConsequenceStringFromChoice(P:TPlayer): string;
    function GetConsequenceStringFromChoices:string;
    procedure CheckNeedForRandomTurns;
    procedure EndExperiment;
    procedure SetCondition(I : Integer; AValue: TCondition);
    procedure SetContingency(ACondition, I : integer; AValue: TContingency);
    procedure SetMatrixType(AValue: TGameMatrixType);
    procedure SetPlayer(I : integer; AValue: TPlayer); overload;
    procedure SetPlayer(S : string ; AValue: TPlayer); overload;
    procedure SetResearcherCanChat(AValue: Boolean);
    procedure SetResearcherCanPlay(AValue: Boolean);
    procedure SetSendChatHistoryForNewPlayers(AValue: Boolean);
    procedure SetState(AValue: TExperimentState);
    procedure SetTargetInterlockingEvent;
    procedure SetContingenciesEvents;
  private
    FLastGenerationCount : integer;
    FAvoidOverlapingChanges : TAvoidOverlapingChanges;
    FConditionMustBeUpdated: Boolean;
    FConsequenceStringFromChoices: string;
    FEndCycle: Boolean;
    FOnConsequence: TNotifyEvent;
    FOnInterlocking: TNotifyEvent;
    FOnEndTurn: TNotifyEvent;
    FOnEndCondition: TNotifyEvent;
    FOnEndCycle: TNotifyEvent;
    FOnEndExperiment: TNotifyEvent;
    FOnEndGeneration: TNotifyEvent;
    FOnStartCondition: TNotifyEvent;
    FOnStartCycle: TNotifyEvent;
    FOnStartGeneration: TNotifyEvent;
    FOnStartTurn: TNotifyEvent;
    FOnWriteReport: TNotifyOnWriteReport;
    FOnStartExperiment: TNotifyEvent;
    FOnTargetInterlocking: TNotifyEvent;
    function GetTurns : string;
    function GetCurrentCondition: TCondition;
    function GetFirstTurn(AValue:integer): integer;
    function GetMatrixTypeAsString: string;
    procedure Consequence(Sender : TObject);
    procedure Interlocking(Sender : TObject);
    procedure SetFilename(AValue : string);
    procedure SetMatrixTypeFromString(AValue: string);
    procedure SetOnStartCondition(AValue: TNotifyEvent);
    procedure SetOnStartCycle(AValue: TNotifyEvent);
    procedure SetOnStartGeneration(AValue: TNotifyEvent);
    procedure SetOnStartTurn(AValue: TNotifyEvent);
    procedure SetOnWriteReport(AValue: TNotifyOnWriteReport);
    procedure SetOnStartExperiment(AValue: TNotifyEvent);
    procedure SetOnTargetInterlocking(AValue: TNotifyEvent);
    procedure SetTurns(AValue : string);
    procedure TargetInterlocking(Sender : TObject);
    procedure SetOnConsequence(AValue: TNotifyEvent);
    procedure SetOnEndCondition(AValue: TNotifyEvent);
    procedure SetOnEndCycle(AValue: TNotifyEvent);
    procedure SetOnEndExperiment(AValue: TNotifyEvent);
    procedure SetOnEndGeneration(AValue: TNotifyEvent);
    procedure SetOnEndTurn(AValue: TNotifyEvent);
    procedure SetOnInterlocking(AValue: TNotifyEvent);
  public // creation/ destruction
    constructor Create(AOwner:TComponent); override;
    constructor Create(AOwner:TComponent; AActor : TGameActor); reintroduce;
    destructor Destroy; override;
    function LoadFromFile(AFilename: string):Boolean;
    function LoadFromGenerator:Boolean;
    procedure SaveToFile(AFilename: string); overload;
    procedure SaveToFile; overload;
  public // global configuration
    procedure WriteRowPrompt;
    property ExperimentAim : string read FExperimentAim write FExperimentAim;
    property ExperimentName : string read FExperimentName write FExperimentName;
    property ABPoints :  Boolean read FABPoints write FABPoints;
    property GenPlayersAsNeeded : Boolean read FGenPlayersAsNeeded write FGenPlayersAsNeeded;
    property ResearcherCanPlay : Boolean read FResearcherCanPlay write SetResearcherCanPlay;
    property ResearcherCanChat : Boolean read FResearcherCanChat write SetResearcherCanChat;
    property Researcher : string read FResearcher write FResearcher;
    property ShowChat : Boolean read FShowChat write FShowChat;
    property SendChatHistoryForNewPlayers : Boolean read FSendChatHistoryForNewPlayers write SetSendChatHistoryForNewPlayers;
    property MatrixType : TGameMatrixType read FMatrixType write SetMatrixType;
    property MatrixTypeAsString : string read GetMatrixTypeAsString write SetMatrixTypeFromString;
  public // manipulation/ self awareness
    function ConditionSlides : TStringArray;
    function GenerationSlidesLogIn : TStringArray;
    function GenerationSlidesLogOut : TStringArray;
    function HasGenerationSlidesToShow : Boolean;
    function HasSlidesToShow : Boolean;
    function IsStartCondition : Boolean;
    function LastParticipantReadSlides : Boolean;
    function CurrentTurn : integer;
    function LastGenerationCount : integer;
    function PlayerRootFolderFromID(AID : string) : string;
    procedure MovePlayersQueueLeft;
    procedure ArquiveOldPlayer(APlayer : TPlayer);
    function GlobalPoints(AGameConsequencestyle: TGameConsequenceStyle;
      AID : string = '') : integer;
    function AppendCondition : integer; overload;
    function AppendCondition(ACondition : TCondition) : integer;overload;
    function AppendContingency(ACondition : integer) : integer;overload;
    function AppendContingency(ACondition : integer;AContingency : TContingency) : integer;overload;
    function AppendPlayer : integer;overload;
    function AppendPlayer(APlayer : TPlayer) : integer; overload;
    function PlayerPointsSummationFromID(AID : string) : integer;
    function PlayerPointsFromID(AID : string) : TPLayerPoints;
    function IsLastCondition: Boolean;
    function TargetIntelockingFired : Boolean;
    function ShouldAskQuestion : string;
    function ContingencyFired(AContingencyName : string):Boolean;
    function ShouldStartExperiment : Boolean;
    property IsEndCycle : Boolean read FEndCycle;
    property Condition[I : Integer]: TCondition read GetCondition write SetCondition;
    property ConditionsCount : integer read GetConditionsCount;
    property CurrentConditionI : integer read FCurrentCondition write FCurrentCondition;
    property CurrentCondition : TCondition read GetCurrentCondition;
    property Contingency[C, I : integer] : TContingency read GetContingency write SetContingency;
    property ContingenciesCount[C:integer]:integer read GetContingenciesCount;
    property Cycles : TCycles read FCycles;
    property InterlockingsInLastCycles:real read GetInterlockingPorcentageInLastCycles;
    property Player[I : integer] : TPlayer read GetPlayer write SetPlayer;
    property Players : TPlayers read FPlayers;
    property PlayerFromID[S : string ] : TPlayer read GetPlayer write SetPlayer;
    property PlayersCount : integer read GetPlayersCount;
    property PlayerIsPlaying[s : string] : Boolean read GetPlayerIsPlaying;
    property PlayerIndexFromID[s : string]: integer read GetPlayerIndexFromID;
    property PlayerAsString[P:TPlayer]: string read AliasPlayerAsString;
    property PlayerFromString[s : string]: TPlayer read AliasPlayerFromString;
    property FirstTurn[i:integer] : integer read GetFirstTurn;
  public // standard control
    function ShouldEndGeneration:Boolean;
    function ShouldEndCondition:Boolean;
    function CurrentConditionAsString:string;
    function ValidID(AID : string) : Boolean;
    function AsString : string;
    procedure CountOverlapingCycles;
    procedure ForceEndCondition;
    procedure Clean;
    procedure Play;
    procedure UpdatePlayerTurns(ANextTurnString: string);
    procedure IncMetaPoints(AGameConsequenceStyle: TGameConsequenceStyle;
      AValue: integer);
    procedure IncPlayerPoints(AGameConsequenceStyle: TGameConsequenceStyle;
      AValue: integer; AID: string);
    procedure ResetGroupPoints;
    property ConsequenceStringFromChoice[P:TPlayer]:string read GetConsequenceStringFromChoice;
    property ConsequenceStringFromChoices: string read FConsequenceStringFromChoices;
    property NextTurnPlayerID : string read GetNextTurnPlayerID;
    property NextTurn : string read GetNextTurn;
    property NextCondition : string read GetNextCondition;
    property NextGeneration: string read GetPlayerToKick;
    property ConditionMustBeUpdated : Boolean read FConditionMustBeUpdated write FConditionMustBeUpdated;
    property State : TExperimentState read FState write SetState;
    property Turns : string read GetTurns write SetTurns;
  public // events
    property OnConsequence : TNotifyEvent read FOnConsequence write SetOnConsequence;
    property OnEndCondition : TNotifyEvent read FOnEndCondition write SetOnEndCondition;
    property OnEndCycle : TNotifyEvent read FOnEndCycle write SetOnEndCycle;
    property OnEndExperiment : TNotifyEvent read FOnEndExperiment write SetOnEndExperiment;
    property OnEndGeneration : TNotifyEvent read FOnEndGeneration write SetOnEndGeneration;
    property OnEndTurn : TNotifyEvent read FOnEndTurn write SetOnEndTurn;
    property OnInterlocking : TNotifyEvent read FOnInterlocking write SetOnInterlocking;
    property OnStartCondition : TNotifyEvent read FOnStartCondition write SetOnStartCondition;
    property OnStartCycle : TNotifyEvent read FOnStartCycle write SetOnStartCycle;
    property OnStartExperiment : TNotifyEvent read FOnStartExperiment write SetOnStartExperiment;
    property OnStartGeneration : TNotifyEvent read FOnStartGeneration write SetOnStartGeneration;
    property OnStartTurn : TNotifyEvent read FOnStartTurn write SetOnStartTurn;
    property OnTargetInterlocking : TNotifyEvent read FOnTargetInterlocking write SetOnTargetInterlocking;
    property Report : TGameReport read FGameReport;
    property Filename : string read FFilename write SetFilename;
  end;

resourcestring
  WARN_CANNOT_SAVE = 'The experiment could not be saved.';

implementation

uses game_actors_helpers, game_file_methods, game_resources, string_methods, strutils;

{ TExperiment }

function TExperiment.GetCondition(I : Integer): TCondition;
begin
  Result := FConditions[I];
end;

function TExperiment.GetConditionsCount: integer;
begin
  Result := Length(FConditions);
end;

function TExperiment.GetContingenciesCount(C: integer): integer;
begin
  Result := Length(FConditions[C].Contingencies);
end;

function TExperiment.GetContingency(ACondition, I : integer): TContingency;
begin
  Result := FConditions[ACondition].Contingencies[I];
end;

function TExperiment.GetNextTurn: string; // used during player arriving
var
  i : integer;
  function HasOverlappingChanges : Boolean;
  var
    LShouldEndCondition : Boolean;
    LShouldEndGeneration : Boolean;
  begin
    Result := True;
    LShouldEndCondition := ShouldEndCondition;
    LShouldEndGeneration := ShouldEndGeneration;
    if (LShouldEndCondition and LShouldEndGeneration) or
       ((not LShouldEndCondition) and LShouldEndGeneration) then begin
      FAvoidOverlapingChanges.BlockCondition := True;
      FAvoidOverlapingChanges.BlockGeneration := False;
      Exit;
    end;

    if LShouldEndCondition and (not LShouldEndGeneration) then begin
      FAvoidOverlapingChanges.BlockCondition := False;
      FAvoidOverlapingChanges.BlockGeneration := True;
      Exit;
    end;
    Result := False;
  end;

begin
  if Assigned(FOnEndTurn) then FOnEndTurn(Self);
  Result := #32;

  if CurrentCondition.Turn.Count < CurrentCondition.Turn.Value-1 then
    begin
      FEndCycle := False;
      Inc(FConditions[CurrentConditionI].Turn.Count);
    end
  else
    begin
      if Assigned(FOnEndCycle) then FOnEndCycle(Self);
      FEndCycle := True;
      FConsequenceStringFromChoices := GetConsequenceStringFromChoices;
      FGameReport.WriteRow(CurrentCondition,FPlayers, FCycles);
      for i := Low(FConditions[CurrentConditionI].Contingencies) to
               High(FConditions[CurrentConditionI].Contingencies) do
      begin
        FConditions[CurrentConditionI].Contingencies[i].Clean;
      end;

      FConditions[CurrentConditionI].Turn.Count := 0;
      Inc(FCycles.Global);
      Inc(FConditions[CurrentConditionI].Cycles.Count);

      if FAvoidOverlapingChanges.Enabled then begin
        if FAvoidOverlapingChanges.Active then begin
          CountOverlapingCycles;
        end else begin
          FAvoidOverlapingChanges.Active := HasOverlappingChanges;
        end;
      end;

      if CurrentCondition.Turn.Random then
        begin
          CheckNeedForRandomTurns;
          Result := '';
          for i:= 0 to FRandomTurns.Count-1 do
            Result += FRandomTurns[i]+'+';
        end;
      if Assigned(FOnStartCycle) then FOnStartCycle(Self);
    end;

  if Assigned(FOnStartTurn) then FOnStartTurn(Self);
end;

function TExperiment.GetNextTurnPlayerID: string; // used during cycles
begin
  Result := Player[FConditions[CurrentConditionI].Turn.Count].ID;
end;

{
  May return:
    - Fexperiment.CurrentConditionAsString -> next condition parameters
    - #27 -> end FExperiment
    - #32 -> do nothing
}
function TExperiment.GetNextCondition: string;
begin
  Result := #32;                               // 'do nothing' envelop item
  if ShouldEndCondition then
    begin
      if Assigned(FOnEndCondition) then
        FOnEndCondition(Self);

      if IsLastCondition then
        begin
          Result := #27;                      // 'end experiment' envelop item
          EndExperiment;
          Exit;
        end;
      Inc(FCurrentCondition);
      FCycles.GenerationValue := CurrentCondition.Cycles.Value;
      // may the generation count be reseted?
      //FCycles.GenerationCount:=0;
      SetTargetInterlockingEvent;
      SetContingenciesEvents;
      FGameReport.NextCondition(CurrentCondition, FPlayers);
      if Assigned(FOnStartCondition) then FOnStartCondition(Self);
      Result := CurrentConditionAsString;     // 'end condition' envelop item
    end;
end;

function TExperiment.GetPlayer(I : integer): TPlayer;
begin
  Result := FPlayers[i];
end;

function TExperiment.GetPlayer(AID: string): TPlayer;
var
  i : integer;
  P : TPlayer = (
    ID : '';
    Nicname : '';
    Login: '';
    Password: '';
    Status : gpsWaiting;
    Data : nil;
    Choice : (Row:grNone; Color:gcNone);
    Points : (A:0; B:0);
    Index : -1;
    Turn : -1;
  );
begin
  Result := P;
  if PlayersCount > 0 then
    for i:= 0 to PlayersCount -1 do
      if FPlayers[i].ID = AID then
        begin
          Result := FPlayers[i];
          Break;
        end;
   Exception.Create('TExperiment.GetPlayer Exception');
end;

// fewer as possible data
function TExperiment.AliasPlayerAsString(P: TPlayer): string;
begin
  Result:= GetPlayerAsString(P);
end;

function TExperiment.AliasPlayerFromString(s: string): TPlayer;
begin
  Result := GetPlayerFromString(S);
end;

function TExperiment.GetPlayerIndexFromID(AID: string): integer;
var i : integer;
begin
  Result := -1;
  for i:= 0 to PlayersCount -1 do
    if FPlayers[i].ID = AID then
      begin
        Result := i;
        Break;
      end;
end;

function TExperiment.GetPlayerIsPlaying(AID: string): Boolean;
var i : integer;
begin
  Result := PlayersCount > 0;
  if Result then
    for i := 0 to PlayersCount -1 do
      if Player[i].ID = AID then
        Exit;
  Result:= False;
end;

function TExperiment.GetPlayersCount: integer;
begin
  Result := Length(FPlayers);
end;

function TExperiment.GetInterlockingPorcentageInLastCycles: real;
var
  LRow,
  LContingencyName : string;
  i : integer;
  c : integer;
  LContingencyResults : TStringList;
begin
  Result := 0;
  c := CurrentConditionI;

  // for now getting the first metacontingency as target
  for i :=0 to ContingenciesCount[c] -1 do
    if Contingency[c,i].Meta then
      begin
        LContingencyName := Contingency[c,i].ContingencyName;
        Break;
      end;

  LContingencyResults := FGameReport.Reader.ColumnOf[LContingencyName];
  if LContingencyResults.Count > 0 then
    begin
      i := 0;
      for LRow in LContingencyResults do
        if LRow = '1' then Inc(i);
      Result := (i*100)/LContingencyResults.Count;

      if LContingencyResults.Count = Condition[c].EndCriterium.LastCycles then
        Exit;

      if LContingencyResults.Count > Condition[c].EndCriterium.LastCycles then
        Result := 0
      else
        Result *= -1;
    end;
end;

function TExperiment.GetConsequenceStringFromChoice(P: TPlayer): string;
var
  i : integer;
  c : integer;
begin
  c := CurrentConditionI;
  PlayerFromID[P.ID] := P;
  Result:= '';
  for i :=0 to ContingenciesCount[c] -1 do
    if not Contingency[c,i].Meta then
      if Contingency[c,i].ResponseMeetsCriteriaI(P.Choice.Row,P.Choice.Color) then
        Result += DeduceNicname(Contingency[c,i].Consequence.AsString(P.ID), P)+'+';
end;

function TExperiment.GetConsequenceStringFromChoices: string;
var
  i : integer;
  j : integer;
  c : integer;
  LMessages : TStringList;
  P : TPlayer;
begin
  c := CurrentConditionI;
  Result:= '';
  for i :=0 to ContingenciesCount[c] -1 do
    if Contingency[c,i].Meta then
      if Contingency[c,i].ResponseMeetsCriteriaG(FPlayers) then
        begin
          // WriteLn(GetPromptStyleString(Contingency[c,i].Style));
          if Contingency[c,i].Style = [] then
            Result += 'M#'+Contingency[c,i].Consequence.AsString('M')+'+'
          else
            begin
              Contingency[c,i].Consequence.AsString('M');
              LMessages := GetMessagesFromPromptStyle(Contingency[c,i].Style,CurrentCondition.Contingencies);
              for j := 0 to LMessages.Count -1 do
                begin
                  P := PlayerFromID[FirstDelimitedString(LMessages[j])];
                  Result += DeduceNicname(LMessages[j],P)+'+';
                end;
            end;
        end;
end;

procedure TExperiment.CheckNeedForRandomTurns;
var
  LNumberOfPlayers,
  i,
  r : integer;
  LOldRandomOrder , LNewRandomOrder: TStringList;
begin
  if CurrentCondition.Turn.Random then
    begin
      LNumberOfPlayers := CurrentCondition.Turn.Value;
      LNewRandomOrder := TStringList.Create;
      LOldRandomOrder := TStringList.Create;
      try
        for i:= 0 to LNumberOfPlayers-1 do
          begin
            if (Length(FPlayers)   = LNumberOfPlayers) and
               (FRandomTurns.Count = LNumberOfPlayers) then
              LOldRandomOrder.Append(Delimited(2,FRandomTurns[i]))
            else
              LOldRandomOrder.Append(IntToStr(i));
            LNewRandomOrder.Append(IntToStr(i));
          end;
        FRandomTurns.Clear;
        repeat
          for i := 0 to LNewRandomOrder.Count - 1 do
            begin
              r := Random(LNumberOfPlayers);
              while r = i do r := Random(LNumberOfPlayers);
              LNewRandomOrder.Exchange(r,i);
            end;
        until LNewRandomOrder.Text <> LOldRandomOrder.Text;

        for i:= 0 to LNumberOfPlayers-1 do
          if Length(FPlayers) = LNumberOfPlayers then
            FRandomTurns.Append(FPlayers[i].ID+'|'+LNewRandomOrder[i])
          else
            FRandomTurns.Append('first_turn_doesnt_matter'+'|'+LNewRandomOrder[i]);
      finally
        LOldRandomOrder.Free;
        LNewRandomOrder.Free;
      end;
    end;
end;

procedure TExperiment.EndExperiment;
begin
  State:=xsNone;
  FGameReport.WriteFooter;
  Clean;
  if Assigned(FOnEndExperiment) then FOnEndExperiment(Self);
end;

procedure TExperiment.SetCondition(I : Integer; AValue: TCondition);
begin
  FConditions[I] := AValue;
end;

procedure TExperiment.SetContingency(ACondition, I : integer; AValue: TContingency);
begin
  FConditions[ACondition].Contingencies[I] := AValue;
end;

procedure TExperiment.SetMatrixType(AValue: TGameMatrixType);
begin
  if FMatrixType=AValue then Exit;
  FMatrixType:=AValue;
end;

procedure TExperiment.SetOnConsequence(AValue: TNotifyEvent);
begin
  if FOnConsequence=AValue then Exit;
  FOnConsequence:=AValue;
end;

procedure TExperiment.SetOnEndCondition(AValue: TNotifyEvent);
begin
  if FOnEndCondition=AValue then Exit;
  FOnEndCondition:=AValue;
end;

procedure TExperiment.SetOnEndCycle(AValue: TNotifyEvent);
begin
  if FOnEndCycle=AValue then Exit;
  FOnEndCycle:=AValue;
end;

procedure TExperiment.SetOnEndExperiment(AValue: TNotifyEvent);
begin
  if FOnEndExperiment=AValue then Exit;
  FOnEndExperiment:=AValue;
end;

procedure TExperiment.SetOnEndGeneration(AValue: TNotifyEvent);
begin
  if FOnEndGeneration=AValue then Exit;
  FOnEndGeneration:=AValue;
end;

procedure TExperiment.SetOnEndTurn(AValue: TNotifyEvent);
begin
  if FOnEndTurn=AValue then Exit;
  FOnEndTurn:=AValue;
end;

procedure TExperiment.SetOnInterlocking(AValue: TNotifyEvent);
begin
  if FOnInterlocking=AValue then Exit;
  FOnInterlocking:=AValue;
end;

procedure TExperiment.SetPlayer(I : integer; AValue: TPlayer);
begin
  FPlayers[I] := AValue;
end;

procedure TExperiment.SetPlayer(S: string; AValue: TPlayer);
var i : integer;
begin
  if PlayersCount > 0 then
    for i:= 0 to PlayersCount -1 do
      if FPlayers[i].ID = S then
        begin
          FPlayers[i] := AValue;
          Exit;
        end;
  raise Exception.Create('TExperiment.SetPlayer: Could not set player.');
end;

procedure TExperiment.SetResearcherCanChat(AValue: Boolean);
begin
  if FResearcherCanChat=AValue then Exit;
  FResearcherCanChat:=AValue;
end;

procedure TExperiment.SetResearcherCanPlay(AValue: Boolean);
begin
  if FResearcherCanPlay=AValue then Exit;
  FResearcherCanPlay:=AValue;
end;

procedure TExperiment.SetSendChatHistoryForNewPlayers(AValue: Boolean);
begin
  if FSendChatHistoryForNewPlayers=AValue then Exit;
  FSendChatHistoryForNewPlayers:=AValue;
end;

procedure TExperiment.SetState(AValue: TExperimentState);
begin
  if FState=AValue then Exit;
  FState:=AValue;
end;

procedure TExperiment.SetTargetInterlockingEvent;
var i : integer;
begin
  for i:= 0 to ContingenciesCount[CurrentConditionI]-1 do
    if Condition[CurrentConditionI].Contingencies[i].Meta then
      begin
        Condition[CurrentConditionI].Contingencies[i].OnTargetCriteria:=@TargetInterlocking;
        Break;
      end;
end;

procedure TExperiment.SetContingenciesEvents;
var
  i: Integer;
begin
  for i := 0 to ContingenciesCount[CurrentConditionI]-1 do
    if FConditions[CurrentConditionI].Contingencies[I].Meta then
      FConditions[CurrentConditionI].Contingencies[I].OnCriteria:=@Interlocking
    else
      FConditions[CurrentConditionI].Contingencies[I].OnCriteria:=@Consequence;
end;

procedure TExperiment.Consequence(Sender: TObject);
begin
  // Sender = TContingency
  if Assigned(FOnConsequence) then FOnConsequence(Sender);
end;

function TExperiment.GetCurrentCondition: TCondition;
begin
  Result := FConditions[CurrentConditionI];
end;

function TExperiment.GetFirstTurn(AValue: integer): integer;
begin
  if CurrentCondition.Turn.Random then
    Result := StrToInt(Delimited(2,FRandomTurns[AValue]))
  else
    Result := AValue;
end;

function TExperiment.GetMatrixTypeAsString: string;
begin
  Result := GetMatrixTypeString(MatrixType);
end;

procedure TExperiment.TargetInterlocking(Sender: TObject);
begin
  // Sender = TContingency
  if Assigned(FOnTargetInterlocking) then FOnTargetInterlocking(Sender);
end;

procedure TExperiment.MovePlayersQueueLeft;
var
  i : integer;
  P : TPlayer;
begin
  P := FPlayers[0];
  for i := 0 to PlayersCount-2 do
    FPlayers[i] := FPlayers[i+1];
  FPlayers[High(FPlayers)] := P;
end;

function TExperiment.GetPlayerToKick: string;
begin
  Result := #32;
  if ShouldEndGeneration then begin
    if Assigned(FOnEndGeneration) then FOnEndGeneration(Self);

    // remove the older participant
    Result := FPlayers[0].ID;
    FLastGenerationCount := FCycles.GenerationCount;
    FCycles.GenerationCount := 0;
    Inc(FCycles.Generations);
    if Assigned(FOnStartGeneration) then FOnStartGeneration(Self);
  end else begin
    Inc(FCycles.GenerationCount);
  end;
end;

procedure TExperiment.Interlocking(Sender: TObject);
begin
  // Sender = TContingency
  if Assigned(FOnInterlocking) then FOnInterlocking(Sender);
end;

procedure TExperiment.SetFilename(AValue : string);
begin
  if FFilename = AValue then Exit;
  FFilename := AValue;
end;


procedure TExperiment.SetMatrixTypeFromString(AValue: string);
begin
  MatrixType := GetMatrixTypeFromString(AValue);
end;

procedure TExperiment.SetOnStartCondition(AValue: TNotifyEvent);
begin
  if FOnStartCondition=AValue then Exit;
  FOnStartCondition:=AValue;
end;

procedure TExperiment.SetOnStartCycle(AValue: TNotifyEvent);
begin
  if FOnStartCycle=AValue then Exit;
  FOnStartCycle:=AValue;
end;

procedure TExperiment.SetOnStartGeneration(AValue: TNotifyEvent);
begin
  if FOnStartGeneration=AValue then Exit;
  FOnStartGeneration:=AValue;
end;

procedure TExperiment.SetOnStartTurn(AValue: TNotifyEvent);
begin
  if FOnStartTurn=AValue then Exit;
  FOnStartTurn:=AValue;
end;

procedure TExperiment.SetOnWriteReport(AValue: TNotifyOnWriteReport);
begin
  if FOnWriteReport=AValue then Exit;
  FOnWriteReport:=AValue;
end;

procedure TExperiment.SetOnStartExperiment(AValue: TNotifyEvent);
begin
  if FOnStartExperiment=AValue then Exit;
  FOnStartExperiment:=AValue;
end;

procedure TExperiment.SetOnTargetInterlocking(AValue: TNotifyEvent);
begin
  if FOnTargetInterlocking=AValue then Exit;
  FOnTargetInterlocking:=AValue;
end;

procedure TExperiment.SetTurns(AValue : string);
begin
  if FGameActor = gaPlayer then
    FRandomTurns.Text := AValue;
end;

procedure TExperiment.IncMetaPoints(
  AGameConsequenceStyle: TGameConsequenceStyle; AValue: integer);
var
  i : integer;
begin
  case AGameConsequenceStyle of
    gscG1 :
      begin
        Inc(FConditions[CurrentConditionI].Points.Count.G1, AValue);
        for i := Low(FPlayers) to High(FPlayers) do begin
          Inc(FPlayers[i].Points.G1, AValue);
        end;
      end;
    gscG2 :
      begin
        Inc(FConditions[CurrentConditionI].Points.Count.G2, AValue);
        for i := Low(FPlayers) to High(FPlayers) do begin
          Inc(FPlayers[i].Points.G2, AValue);
        end;
      end;
    else { do nothing };
  end;
end;

procedure TExperiment.IncPlayerPoints(
  AGameConsequenceStyle: TGameConsequenceStyle; AValue: integer; AID: string);
var
  P : TPlayer;
begin
  case AGameConsequenceStyle of
    gscA :
      begin
        if AID = '' then Exception.Create('TExperiment.IncPoint Exception');
        P := PlayerFromID[AID];
        Inc(P.Points.A, AValue);
        PlayerFromID[AID] := P;
      end;
    gscB :
      begin
        if AID = '' then Exception.Create('TExperiment.IncPoint Exception');
        P := PlayerFromID[AID];
        Inc(P.Points.B, AValue);
        PlayerFromID[AID] := P;
      end
    else { do nothing };
  end;
end;

// only for players
procedure TExperiment.ResetGroupPoints;
begin
  FConditions[CurrentConditionI].Points.Count.G1 := 0;
  FConditions[CurrentConditionI].Points.Count.G2 := 0;
end;

constructor TExperiment.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Exception.Create('Wrong Create Method');
end;

constructor TExperiment.Create(AOwner:TComponent; AActor : TGameActor);
begin
  inherited Create(AOwner);
  FCurrentCondition := 0;
  FGameActor := AActor;
  FGameReport := TGameReport.Create(FGameActor);
  FRandomTurns := TStringList.Create;
  with FCycles do
    begin
      Global := 0;
      Generations := 0;
      GenerationCount := 0;
      GenerationValue:=0;
    end;
  State := xsNone;
end;

destructor TExperiment.Destroy;
begin
  FGameReport.Free;
  FRandomTurns.Free;
  inherited Destroy;
end;

function TExperiment.LoadFromFile(AFilename: string): Boolean;
begin
  Result := LoadExperimentFromFile(Self, AFilename);
  if Result then
    FFilename := AFilename
  else Exception.Create('TExperiment.LoadFromFile Exception');

  SetTargetInterlockingEvent;
  SetContingenciesEvents;
  CheckNeedForRandomTurns;
  FGameReport.Start(FFileName, FResearcher, FExperimentName,
    CurrentCondition, FCycles, FPlayers);
  State := xsWaiting;
end;

function TExperiment.LoadFromGenerator: Boolean;
begin
  Result := LoadExperimentFromResource(Self);
  if Result then
    FFilename := GetCurrentDir + PathDelim + FResearcher + PathDelim;
  CheckNeedForRandomTurns;
end;

function TExperiment.AppendCondition: integer;
begin
  SetLength(FConditions, Length(FConditions)+1);
  Result := High(FConditions);
end;

function TExperiment.AppendCondition(ACondition: TCondition): integer;
begin
  SetLength(FConditions, Length(FConditions)+1);
  Result := High(FConditions);
  FConditions[Result] := ACondition;
end;

function TExperiment.AppendContingency(ACondition: integer): integer;
begin
  SetLength(FConditions[ACondition].Contingencies, Length(FConditions[ACondition].Contingencies)+1);
  Result := High(FConditions[ACondition].Contingencies);
end;

function TExperiment.AppendContingency(ACondition: integer;
  AContingency: TContingency): integer;
begin
  SetLength(FConditions[ACondition].Contingencies, Length(FConditions[ACondition].Contingencies)+1);
  Result := High(FConditions[ACondition].Contingencies);
  FConditions[ACondition].Contingencies[Result] := AContingency;
end;

function TExperiment.AppendPlayer: integer;
begin
  SetLength(FPlayers, Length(FPlayers)+1);
  Result := High(FPlayers);
end;

function TExperiment.AppendPlayer(APlayer: TPlayer): integer;
begin
  SetLength(FPlayers, Length(FPlayers)+1);
  Result := High(FPlayers);
  FPlayers[Result] := APlayer;
end;

function TExperiment.PlayerPointsSummationFromID(AID: string): integer;
var
  P : TPlayer;
begin
  P := PlayerFromID[AID];
  if ABPoints then
    Result := P.Points.A + P.Points.B
  else
    Result := P.Points.A;
end;

function TExperiment.PlayerPointsFromID(AID: string): TPLayerPoints;
var
  P : TPlayer;
begin
  P := PlayerFromID[AID];
  Result := P.Points;
end;

function TExperiment.IsLastCondition: Boolean;
begin
  Result := CurrentConditionI = ConditionsCount-1;
end;

function TExperiment.TargetIntelockingFired: Boolean;
var i : integer;
begin
  Result := False;
  for i:= 0 to ContingenciesCount[CurrentConditionI]-1 do
    if Condition[CurrentConditionI].Contingencies[i].Meta then
      begin
        Result := Condition[CurrentConditionI].Contingencies[i].Fired;
        Break;
      end;
end;

function TExperiment.ShouldAskQuestion: string;
begin
  if Assigned(CurrentCondition.Prompt) and
    ContingencyFired(CurrentCondition.Prompt.TargetMetacontingencyName) then
    Result := CurrentCondition.Prompt.Question
  else
    begin
      Result := #32;
      if Assigned(CurrentCondition.Prompt) then
        FGameReport.WriteRowPrompt(CurrentCondition, FPlayers);
      Clean;
    end;
end;

function TExperiment.ContingencyFired(AContingencyName: string): Boolean;
var
  i: Integer;
  LContingencyName : string;
begin
  Result := False;
  for i:= 0 to ContingenciesCount[CurrentConditionI]-1 do
    begin
      LContingencyName := Condition[CurrentConditionI].Contingencies[i].ContingencyName;
      if UpperCase(LContingencyName).Contains(UpperCase(AContingencyName)) then
        if Condition[CurrentConditionI].Contingencies[i].Fired then
          begin
            Result := True;
            Break;
          end;
    end;
end;

function TExperiment.ShouldStartExperiment: Boolean;
begin
  Result := PlayersCount = Condition[CurrentConditionI].Turn.Value;
end;

procedure TExperiment.CountOverlapingCycles;
begin
  with FAvoidOverlapingChanges do begin
    if Count >= Value-1 then begin
      Active := False;
      BlockCondition := False;
      BlockGeneration := False;
      Count := 0;
    end else begin
      Inc(Count);
    end;
  end;
end;

function TExperiment.ShouldEndGeneration : Boolean;
  function IsEnd : Boolean;
  begin
    Result := FCycles.GenerationCount >= FCycles.GenerationValue -1;
  end;
begin
  Result := False;
  if FAvoidOverlapingChanges.Enabled then begin
    if FAvoidOverlapingChanges.BlockGeneration then begin
      Exit;
    end;
  end;
  Result := IsEnd;
end;

function TExperiment.ShouldEndCondition: Boolean;
var
  LInterlocks: Real;
  LCyclesInCurrentCondition: Integer;
begin
  Result := False;
  if FAvoidOverlapingChanges.Enabled then begin
    if FAvoidOverlapingChanges.BlockCondition then begin
      Exit;
    end;
  end;

  // interlockings in the last x cycles
  LInterlocks := InterlockingsInLastCycles;

  // absolute cycles count
  LCyclesInCurrentCondition := CurrentCondition.Cycles.Count;
  case FConditions[CurrentConditionI].EndCriterium.Style of
    gecWhichComesFirst:
      begin
        if LCyclesInCurrentCondition < CurrentCondition.EndCriterium.AbsoluteCyclesMin then
          Exit;

        if (LCyclesInCurrentCondition = CurrentCondition.EndCriterium.AbsoluteCyclesMax) or
           (LInterlocks >= CurrentCondition.EndCriterium.UpperInterlockingPorcentage) or
           (LInterlocks <= CurrentCondition.EndCriterium.LowerInterlockingPorcentage) then
          Result := True;

      end;
    gecAbsoluteCycles:
      begin
        if LCyclesInCurrentCondition < CurrentCondition.EndCriterium.AbsoluteCyclesMin then
          Exit;

        if LCyclesInCurrentCondition = CurrentCondition.EndCriterium.AbsoluteCyclesMin then
          Result := True;
      end;

    gecInterlockingPorcentage:
      begin
        if (LInterlocks >= CurrentCondition.EndCriterium.UpperInterlockingPorcentage) or
           (LInterlocks <= CurrentCondition.EndCriterium.LowerInterlockingPorcentage) then
          Result := True;
      end;
  end;
end;

function TExperiment.CurrentConditionAsString: string;
begin
  if ABPoints then
    Result :=
      Condition[CurrentConditionI].Label1+'|'+
      Condition[CurrentConditionI].Label2+'|'+
      Condition[CurrentConditionI].TargetMetacontingency+'|'+
      IntToStr(Condition[CurrentConditionI].Points.OnStart.A)+'|'+
      IntToStr(Condition[CurrentConditionI].Points.OnStart.B)+'|'+
      IntToStr(Condition[CurrentConditionI].Points.OnStart.G1)+'|'+
      IntToStr(Condition[CurrentConditionI].Points.OnStart.G2)
  else
    Result:=
      Condition[CurrentConditionI].Label1+'|'+
      Condition[CurrentConditionI].Label2+'|'+
      Condition[CurrentConditionI].TargetMetacontingency+'|'+
      IntToStr(Condition[CurrentConditionI].Points.OnStart.A)+'|'+
      IntToStr(Condition[CurrentConditionI].Points.OnStart.G1)+'|'+
      IntToStr(Condition[CurrentConditionI].Points.OnStart.G2);
end;

function TExperiment.ValidID(AID: string): Boolean;
var
  P : TPlayer;
begin
  Result := True;
  for P in FPlayers do
    if P.ID = AID then begin
      Result := False;
      Exit;
    end;

  for P in FOldPlayers do
    if P.ID = AID then begin
      Result := False;
      Exit;
    end;
end;

function TExperiment.AsString : string;
var
  LStringList : TStringList;
begin
  LStringList := TStringList.Create;
  try
    LStringList.LoadFromFile(FFilename);
    Result := LStringList.Text;
  finally
    LStringList.Free;
  end;
end;

function TExperiment.GetTurns : string;
begin
  if CurrentCondition.Turn.Random then begin
    Result := FRandomTurns.Text;
  end else begin
    Result := #32;
  end;
end;

procedure TExperiment.ForceEndCondition;
begin
  if CurrentCondition.EndCriterium.ReachZero then
    FConditions[CurrentConditionI].Cycles.Count :=
      CurrentCondition.EndCriterium.AbsoluteCyclesMax-1;
end;

procedure TExperiment.SaveToFile(AFilename: string);
begin
  SaveExperimentToFile(Self,AFilename);
end;

procedure TExperiment.SaveToFile;
begin
  if FFilename <> '' then
    SaveExperimentToFile(Self, FFilename)
  else;
end;

procedure TExperiment.WriteRowPrompt;
begin
  FGameReport.WriteRowPrompt(CurrentCondition, FPlayers);
end;

function TExperiment.ConditionSlides : TStringArray;
begin
  Result := CurrentCondition.Slides;
end;

function TExperiment.GenerationSlidesLogIn : TStringArray;
begin
  Result := CurrentCondition.GenerationSlidesLogIn;
end;

function TExperiment.GenerationSlidesLogOut : TStringArray;
begin
  Result := CurrentCondition.GenerationSlidesLogOut;
end;


function TExperiment.HasGenerationSlidesToShow : Boolean;
begin
  Result :=
    (Length(CurrentCondition.GenerationSlidesLogIn) > 0) and
    (Length(CurrentCondition.GenerationSlidesLogOut) > 0);
end;

function TExperiment.HasSlidesToShow : Boolean;
begin
  Result := Length(CurrentCondition.Slides) > 0;
end;

function TExperiment.IsStartCondition : Boolean;
begin
  Result := CurrentCondition.Cycles.Count = 0;
end;

function TExperiment.LastParticipantReadSlides : Boolean;
var
  P : TPlayer;
  i : integer;
begin
  Result := False;
  for P in FPlayers do begin
    if P.Status <> gpsFinishedReading then begin
      Exit;
    end;
  end;
  for i := Low(FPlayers) to High(FPlayers) do begin
    FPlayers[i].Status := gpsPlaying;
  end;
  Result := True;
end;

function TExperiment.CurrentTurn : integer;
begin
  Result := CurrentCondition.Turn.Count;
end;

function TExperiment.LastGenerationCount : integer;
begin
  Result := FLastGenerationCount;
end;

function TExperiment.PlayerRootFolderFromID(AID : string) : string;
begin
 Result := 'P'+(PlayerFromID[AID].Index+1).ToString + PathDelim
end;

procedure TExperiment.ArquiveOldPlayer(APlayer: TPlayer);
begin
  SetLength(FOldPlayers, Length(FOldPlayers)+1);
  FOldPlayers[High(FOldPlayers)] := APlayer;
end;

function TExperiment.GlobalPoints(AGameConsequencestyle: TGameConsequenceStyle;
  AID: string): integer;
var
  i : integer;
  P : TPlayer;
begin
  Result := 0;

  // grupos 1 e 2 ocupam o mesmo controle visual externo, mas
  // precisam ser contados separadamente dentro do TExperiment

  case AGameConsequenceStyle of
    gscG1 :
      begin
        if AID = '' then begin
          for i := 0 to ConditionsCount-1 do
            Result += FConditions[i].Points.Count.G1;
        end else begin
          P := PlayerFromID[AID];
          Result := P.Points.G1;
        end;
      end;
    gscG2 :
      begin
        if AID = '' then begin
          for i := 0 to ConditionsCount-1 do
            Result += FConditions[i].Points.Count.G2;
        end else begin
          P := PlayerFromID[AID];
          Result := P.Points.G2;
        end;
      end;
    gscA :
      begin
        if AID = '' then begin
          if Length(FOldPlayers) > 0 then
            for i := Low(FOldPlayers) to High(FOldPlayers) do
              Result += FOldPlayers[i].Points.A;

          for i := Low(FPlayers) to High(FPlayers) do
            Result += FPlayers[i].Points.A;

        end else begin
          P := PlayerFromID[AID];
          Result := P.Points.A;
        end;
      end;
    gscB :
      begin
        if AID = '' then begin
          if Length(FOldPlayers) > 0 then
            for i := Low(FOldPlayers) to High(FOldPlayers) do
              Result += FOldPlayers[i].Points.B;

          for i := Low(FPlayers) to High(FPlayers) do
            Result += FPlayers[i].Points.B;

        end else begin
          P := PlayerFromID[AID];
          Result := P.Points.B;
        end;
      end
    else { do nothing };
  end;
end;

procedure TExperiment.Clean;
var c,i : integer;
begin
  for i := 0 to PlayersCount -1 do
    begin
      FPlayers[i].Choice.Row:=grNone;
      FPlayers[i].Choice.Color:=gcNone;
    end;
  c := CurrentConditionI;
  for i := 0 to ContingenciesCount[c]-1 do
    Contingency[c,i].Clean;

  if Assigned(Condition[c].Prompt) then  // TODO: FIND WHY OPTIMIZATION 3 GENERATES BUG HERE
    Condition[c].Prompt.Clean;

  FGameReport.Clean;
end;

procedure TExperiment.Play;
begin
  FAvoidOverlapingChanges.Enabled := True;
  FAvoidOverlapingChanges.Active := False;
  FAvoidOverlapingChanges.Count := 0;
  FAvoidOverlapingChanges.Value := 5;
  FCycles.GenerationValue := CurrentCondition.Cycles.Value;
  FState:=xsRunning;

  // only admin can send the start experiment event
  if FGameActor = gaAdmin then
    if Assigned(FOnStartExperiment) then FOnStartExperiment(Self);

  if Assigned(OnStartCondition) then OnStartCondition(Self);
end;

procedure TExperiment.UpdatePlayerTurns(ANextTurnString: string);
var
  LCount: integer;
  LCode : string;
  LPlayerID : string;
  LTurnID, i: integer;
  P: TPlayer;
begin
  LCount := WordCount(ANextTurnString,['+']);
  if LCount > 0 then
    for i := 1 to LCount do
      begin
        LCode     := ExtractDelimited(i,ANextTurnString,['+']);
        if FGameActor = gaPlayer then begin
          FRandomTurns[i-1] := LCode;
        end;
        LPlayerID := ExtractDelimited(1,LCode,['|']);
        LTurnID   := StrToInt(ExtractDelimited(2,LCode,['|']));
        P := PlayerFromID[LPlayerID];
        P.Turn := LTurnID;
        PlayerFromID[LPlayerID] := P;
      end;
end;


end.

