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
  , regdata
  , report_reader
  ;

type

  { TExperiment }

  TNotifyOnWriteReport = procedure (S : string) of object;

  TExperimentState = (xsNone,xsWaiting,xsRunning,xsPaused,xsCanceled);

  TConditions = array of TCondition;

  TCycles = record
    Global,
    GenerationCount,
    GenerationValue,
    Generations : integer;
  end;

  TExperiment = class(TComponent)
  private
    FABPoints: Boolean;
    //FChangeGeneration: string;
    FExperimentAim,
    FExperimentName,
    FFilename,
    FResearcher : string;
    FGenPlayersAsNeeded : Boolean;
    FResearcherCanChat: Boolean;
    FResearcherCanPlay: Boolean;
    FSendChatHistoryForNewPlayers: Boolean;
    FShowChat: Boolean;
    FMatrixType: TGameMatrixType;
  private
    FExperimentPath,
    FLastReportColNames2,
    FLastReportColNames : string;
    FRegData : TRegData;
    FRegChat : TRegData;
    FReportReader : TReportReader;
    FPlayers : TPlayers;
    FCurrentCondition : integer;
    FConditions : TConditions;
    FState: TExperimentState;
    FRandomTurns : TStringList;
    FCycles : TCycles;
    function GetCondition(I : Integer): TCondition;
    function GetConditionsCount: integer;
    function GetContingenciesCount(C: integer): integer;
    function GetContingency(ACondition, I : integer): TContingency;
    function GetNextTurn : UTF8string;
    function GetNextTurnPlayerID: UTF8string;
    function GetNextCondition:string;
    function AliasPlayerAsString(P: TPlayer): UTF8string;
    function AliasPlayerFromString(s : UTF8string): TPlayer;
    function GetPlayer(I : integer): TPlayer; overload;
    function GetPlayer(AID : UTF8string): TPlayer; overload;
    function GetPlayerToKick: string;
    function GetPlayerIndexFromID(AID : UTF8string): integer;
    function GetPlayerIsPlaying(AID : UTF8string): Boolean;
    function GetPlayersCount: integer;
    function GetInterlockingPorcentageInLastCycles:real;
    function GetConsequenceStringFromChoice(P:TPlayer): Utf8string;
    function GetConsequenceStringFromChoices:UTF8String;
    procedure CheckNeedForRandomTurns;
    procedure EndExperiment;
    procedure WriteReportFooter;
    procedure WriteReportHeader;
    procedure WriteReportRowNames;
    procedure WriteReportRow;
    procedure SetCondition(I : Integer; AValue: TCondition);
    procedure SetContingency(ACondition, I : integer; AValue: TContingency);
    procedure SetMatrixType(AValue: TGameMatrixType);
    procedure SetPlayer(I : integer; AValue: TPlayer); overload;
    procedure SetPlayer(S : UTF8string ; AValue: TPlayer); overload;
    procedure SetResearcherCanChat(AValue: Boolean);
    procedure SetResearcherCanPlay(AValue: Boolean);
    procedure SetSendChatHistoryForNewPlayers(AValue: Boolean);
    procedure SetState(AValue: TExperimentState);
    procedure SetTargetInterlockingEvent;
    procedure SetContingenciesEvents;
    procedure SetPlayersQueue(AValue: string);
  private
    FConditionMustBeUpdated: string;
    FConsequenceStringFromChoices: UTF8String;
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
    procedure Consequence(Sender : TObject);
    function GetCurrentCondition: TCondition;
    function GetFirstTurn(AValue:integer): integer;
    function GetMatrixTypeAsUTF8String: UTF8String;
    procedure Interlocking(Sender : TObject);
    procedure SetMatrixTypeFromUTF8String(AValue: UTF8String);
    procedure SetOnStartCondition(AValue: TNotifyEvent);
    procedure SetOnStartCycle(AValue: TNotifyEvent);
    procedure SetOnStartGeneration(AValue: TNotifyEvent);
    procedure SetOnStartTurn(AValue: TNotifyEvent);
    procedure SetOnWriteReport(AValue: TNotifyOnWriteReport);
    procedure SetOnStartExperiment(AValue: TNotifyEvent);
    procedure SetOnTargetInterlocking(AValue: TNotifyEvent);
    procedure TargetInterlocking(Sender : TObject);
    procedure SetOnConsequence(AValue: TNotifyEvent);
    procedure SetOnEndCondition(AValue: TNotifyEvent);
    procedure SetOnEndCycle(AValue: TNotifyEvent);
    procedure SetOnEndExperiment(AValue: TNotifyEvent);
    procedure SetOnEndGeneration(AValue: TNotifyEvent);
    procedure SetOnEndTurn(AValue: TNotifyEvent);
    procedure SetOnInterlocking(AValue: TNotifyEvent);
  public // creation/ destruction
    constructor Create(AOwner:TComponent);override;
    constructor Create(AOwner:TComponent; AppPath:string);overload;
    constructor Create(AOwner:TComponent; AFilename, AppPath:string); overload;
    destructor Destroy; override;
    function LoadFromFile(AFilename: string):Boolean;
    function LoadFromGenerator:Boolean;
    procedure SaveToFile(AFilename: string); overload;
    procedure SaveToFile; overload;
  public // global configuration
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
    property MatrixTypeAsString : UTF8String read GetMatrixTypeAsUTF8String write SetMatrixTypeFromUTF8String;
  public // manipulation/ self awareness
    PlayerTurn : integer;
    function AppendCondition : integer; overload;
    function AppendCondition(ACondition : TCondition) : integer;overload;
    function AppendContingency(ACondition : integer) : integer;overload;
    function AppendContingency(ACondition : integer;AContingency : TContingency) : integer;overload;
    function AppendPlayer : integer;overload;
    function AppendPlayer(APlayer : TPlayer) : integer; overload;
    function IsLastCondition: Boolean;
    function TargetIntelockingFired : Boolean;
    function ShouldAskQuestion : string;
    function ContingencyFired(AContingencyName : string):Boolean;
    function ShouldStartExperiment : Boolean;
    function DumpReportReader:string;
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
    property PlayerFromID[S : UTF8string ] : TPlayer read GetPlayer write SetPlayer;
    property PlayersCount : integer read GetPlayersCount;
    property PlayerIsPlaying[s : UTF8string] : Boolean read GetPlayerIsPlaying;
    property PlayerIndexFromID[s : UTF8string]: integer read GetPlayerIndexFromID;
    property PlayerAsString[P:TPlayer]: UTF8string read AliasPlayerAsString;
    property PlayerFromString[s : UTF8string]: TPlayer read AliasPlayerFromString;
    property FirstTurn[i:integer] : integer read GetFirstTurn;
  public // standard control
    function ShouldEndCondition:Boolean;
    function CurrentConditionAsString:UTF8String;
    procedure Clean;
    procedure Play;
    procedure WriteReportRowPrompt;
    procedure WriteChatLn(ALn : string);
    property ConsequenceStringFromChoice[P:TPlayer]:UTF8String read GetConsequenceStringFromChoice;
    property ConsequenceStringFromChoices: UTF8String read FConsequenceStringFromChoices;
    property NextTurnPlayerID : UTF8string read GetNextTurnPlayerID;
    property NextTurn : UTF8string read GetNextTurn;
    property NextCondition : string read GetNextCondition;
    property NextGeneration: string read GetPlayerToKick write SetPlayersQueue;
    property ConditionMustBeUpdated : string read FConditionMustBeUpdated write FConditionMustBeUpdated;
    property State : TExperimentState read FState write SetState;
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
    property OnWriteReport : TNotifyOnWriteReport read FOnWriteReport write SetOnWriteReport;
  end;

resourcestring
  WARN_CANNOT_SAVE = 'O experimento não pode ser salvo.';

implementation

uses game_actors_helpers, game_file_methods, game_resources, string_methods;

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

function TExperiment.GetNextTurn: UTF8string; // used during player arriving
var
  i : integer;
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
      WriteReportRow;
      FConditions[CurrentConditionI].Turn.Count := 0;
      Inc(FCycles.Global);
      Inc(FConditions[CurrentConditionI].Cycles.Count);

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

function TExperiment.GetNextTurnPlayerID: UTF8string; // used during cycles
begin
  Result := Player[FConditions[CurrentConditionI].Turn.Count].ID;
end;

{
  May return:
    - Fexperiment.CurrentConditionAsString -> next condition parameters
    - #27 -> end experiment
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
      FReportReader.Clean;
      FReportReader.SetXLastRows(Condition[CurrentConditionI].EndCriterium.LastCycles);
      FRegData.SaveData(LineEnding);
      WriteReportRowNames;
      FReportReader.UpdateCols(FLastReportColNames2);
      if Assigned(FOnStartCondition) then FOnStartCondition(Self);
      Result := CurrentConditionAsString;     // 'end condition' envelop item
    end;
end;

function TExperiment.GetPlayer(I : integer): TPlayer;
begin
  Result := FPlayers[i];
end;

function TExperiment.GetPlayer(AID: UTF8string): TPlayer;
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
end;

// fewer as possible data
function TExperiment.AliasPlayerAsString(P: TPlayer): UTF8string;
begin
  Result:= GetPlayerAsString(P);
end;

function TExperiment.AliasPlayerFromString(s: UTF8string): TPlayer;
begin
  Result := GetPlayerFromString(S);
end;

function TExperiment.GetPlayerIndexFromID(AID: UTF8string): integer;
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

function TExperiment.GetPlayerIsPlaying(AID: UTF8string): Boolean;
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

  LContingencyResults := FReportReader.ColumnOf[LContingencyName];
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

function TExperiment.GetConsequenceStringFromChoice(P: TPlayer): Utf8string;
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

function TExperiment.GetConsequenceStringFromChoices: UTF8String;
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
            if Length(FPlayers) = LNumberOfPlayers then
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
            FRandomTurns.Append('fisrt_turn_doesnt_matter'+'|'+LNewRandomOrder[i]);
      finally
        LOldRandomOrder.Free;
        LNewRandomOrder.Free;
      end;
    end;
end;

procedure TExperiment.EndExperiment;
begin
  State:=xsWaiting;
  WriteReportFooter;
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

procedure TExperiment.SetPlayer(S: UTF8string; AValue: TPlayer);
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

function TExperiment.GetMatrixTypeAsUTF8String: UTF8String;
begin
  Result := GetMatrixTypeString(MatrixType);
end;

procedure TExperiment.TargetInterlocking(Sender: TObject);
begin
  // Sender = TContingency
  if Assigned(FOnTargetInterlocking) then FOnTargetInterlocking(Sender);
end;

procedure TExperiment.SetPlayersQueue(AValue: string);
var
  i : integer;
begin
  for i := 0 to PlayersCount-2 do
    begin
      FPlayers[i] := FPlayers[i+1];
    end;
  FPlayers[High(FPlayers)] := PlayerFromString[AValue];
end;

function TExperiment.GetPlayerToKick: string;
begin
  if FCycles.GenerationCount < FCycles.GenerationValue -1 then
    begin
      Result := #32;
      Inc(FCycles.GenerationCount);
    end
  else
    begin
      if Assigned(FOnEndGeneration) then FOnEndGeneration(Self);
      Result := FPlayers[0].ID;
      FCycles.GenerationCount := 0;
      Inc(FCycles.Generations);
      if Assigned(FOnStartGeneration) then FOnStartGeneration(Self);
    end;
end;

procedure TExperiment.Interlocking(Sender: TObject);
begin
  // Sender = TContingency
  if Assigned(FOnInterlocking) then FOnInterlocking(Sender);
end;


procedure TExperiment.SetMatrixTypeFromUTF8String(AValue: UTF8String);
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

procedure TExperiment.WriteReportFooter;
var
  LFooter : string;
begin
  if Assigned(FRegData) then
    begin
      LFooter := LineEnding +
                 VAL_END_TIME+':' + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time) +#9+ LineEnding;

      FRegData.SaveData(LFooter);
       if Assigned(FOnWriteReport) then FOnWriteReport(LFooter);
    end;
end;

procedure TExperiment.WriteReportHeader;
var
  LHeader : string;
begin
  if Assigned(FRegData) then
    begin
      // header
      LHeader := VAL_RESEARCHER+':' + #9 + FResearcher         + #9 + LineEnding +
                 VAL_EXPERIMENT+':' + #9 + FExperimentName     + #9 + LineEnding +
                 VAL_BEGIN_TIME+':' + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time) +#9+ LineEnding + #9 + LineEnding;

      FRegData.SaveData(LHeader);
      if Assigned(FOnWriteReport) then FOnWriteReport(LHeader);
    end;
end;

procedure TExperiment.WriteReportRowNames;
var
  c,j,i: integer;
  LNames1, LNames2 : string;
begin
  if Assigned(FRegData) then
    begin
      c:= CurrentConditionI;

      // column names, line 1
      LNames1 := 'Experimento'+#9+#9+#9;
      for i:=0 to Condition[c].Turn.Value-1 do // player's response
        begin
          LNames1 += FPlayers[i].Nicname+#9+#9;
          for j:=0 to ContingenciesCount[c]-1 do
            if not Contingency[c,j].Meta then
              LNames1 += #9;
        end;

      LNames1 += VAL_INTERLOCKING+'s';
      for i:=0 to ContingenciesCount[c]-1 do
        if Contingency[c,i].Meta then
         LNames1 += #9;

      if Assigned(Condition[c].Prompt) then
        begin
          LNames1 += 'Respostas à Pergunta';
          for i:=0 to Condition[c].Turn.Value-1 do
            LNames1 += #9;
        end;
      LNames1 += LineEnding;

      // column names, line 2
      LNames2 := 'Condição'+#9+'Geração'+#9+'Ciclos'+#9;
      for i:=0 to Condition[c].Turn.Value-1 do
        begin
          LNames2 += 'Linha'+#9+'Cor'+#9;
          for j:=0 to ContingenciesCount[c]-1 do
            if not Contingency[c,j].Meta then
              LNames2 += Contingency[c,j].ContingencyName+#9;
        end;

      for i:=0 to ContingenciesCount[c]-1 do
        if Contingency[c,i].Meta then
          LNames2 += Contingency[c,i].ContingencyName+#9;

      if Assigned(Condition[c].Prompt) then
        for i:=0 to Condition[c].Turn.Value-1 do
          LNames2 += 'R'+IntToStr(i+1)+#9;
      LNames2 += LineEnding;

      if FLastReportColNames <> LNames1+LNames2 then
        begin
          FLastReportColNames := LNames1+LNames2;
          FLastReportColNames2 := LNames2;
          FRegData.SaveData(FLastReportColNames);
          if FReportReader.Cols = 0 then
            FReportReader.UpdateCols(FLastReportColNames2);
          if Assigned(FOnWriteReport) then FOnWriteReport(FLastReportColNames);
        end;
    end;
end;

procedure TExperiment.WriteReportRow;
var
  c,j,i: integer;
  LRow : string;
begin
  WriteReportRowNames;
  if Assigned(FRegData) then
    begin
      c:= CurrentConditionI;

      LRow := IntToStr(c+1)+#9+
              IntToStr(FCycles.Generations+1)+#9+
              IntToStr(FCycles.Global+1)+#9;

      for i:=0 to Condition[c].Turn.Value-1 do
        begin
        LRow += GetRowString(FPlayers[i].Choice.Row)+#9+GetColorString(FPlayers[i].Choice.Color)+#9;
        for j:=0 to ContingenciesCount[c]-1 do
          if not Contingency[c,j].Meta then
            if Contingency[c,j].ConsequenceFromPlayerID(FPlayers[i].ID) <> '' then
              LRow += '1'+#9
            else
              LRow += '0'+#9;
        end;

      for i:=0 to ContingenciesCount[c]-1 do
        if Contingency[c,i].Meta then
          if Contingency[c,i].Fired then
            LRow += '1'+#9
          else
            LRow += '0'+#9;

      if Assigned(FRegData) and Assigned(CurrentCondition.Prompt) then
        // do nothing
      else
        LRow += LineEnding;

      FRegData.SaveData(LRow);
      FReportReader.Append(LRow);
      if Assigned(FOnWriteReport) then FOnWriteReport(LRow);
    end;
end;

procedure TExperiment.WriteReportRowPrompt;
var
  c,i: integer;
  LRow : string;
begin
  c := CurrentConditionI;
  if Assigned(FRegData) and Assigned(Condition[c].Prompt) then
    begin
      LRow := '';
      if Condition[c].Prompt.ResponsesCount = Condition[c].Turn.Value then
        for i:=0 to Condition[c].Prompt.ResponsesCount-1 do
          LRow += 'P'+IntToStr(PlayerIndexFromID[Delimited(1,Condition[c].Prompt.Response(i))]+1)+
          '|'+
          Delimited(2,Condition[c].Prompt.Response(i))+#9
      else
        for i:=0 to Condition[c].Turn.Value-1 do
          LRow += 'NA'+#9;
      LRow += LineEnding;
      FRegData.SaveData(LRow);
      FReportReader.Extend(LRow);  // Write, i.e, extend last row
      if Assigned(FOnWriteReport) then FOnWriteReport(LRow);
    end;
end;

procedure TExperiment.WriteChatLn(ALn: string);
begin
  if Assigned(FRegChat) then
    begin
      FRegChat.SaveData(ALn);
      FRegChat.CloseAndOpen;
    end;
end;

constructor TExperiment.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRandomTurns := TStringList.Create;
  with FCycles do
    begin
      Global := 0;
      Generations := 0;
      GenerationCount := 0;
      GenerationValue:=0;
    end;
  State := xsNone;
  PlayerTurn := 0;
end;

constructor TExperiment.Create(AOwner: TComponent;AppPath:string);
begin
  inherited Create(AOwner);
  FExperimentPath := AppPath;
  FRandomTurns := TStringList.Create;
  State := xsNone;
  PlayerTurn := 0;
end;

constructor TExperiment.Create(AOwner:TComponent;AFilename,AppPath:string);
begin
  inherited Create(AOwner);
  FRandomTurns := TStringList.Create;
  if LoadExperimentFromFile(Self,AFilename) then
    begin
      FExperimentPath := AppPath;
      CheckNeedForRandomTurns;
      State := xsWaiting;
    end;
  PlayerTurn := 0;
end;

destructor TExperiment.Destroy;
begin
  FReportReader.Free;
  FRandomTurns.Free;
  inherited Destroy;
end;

function TExperiment.LoadFromFile(AFilename: string): Boolean;
var
  LDataPath : string;
begin
  Result := LoadExperimentFromFile(Self, AFilename);
  if Result then
    FFilename := AFilename
  else Exit;
  FExperimentPath := ExtractFilePath(FFilename);
  LDataPath := FExperimentPath+ExperimentName+PathDelim;

  SetTargetInterlockingEvent;
  SetContingenciesEvents;

  CheckNeedForRandomTurns;

  FReportReader := TReportReader.Create;
  FReportReader.UseRange:=True;
  FReportReader.SetXLastRows(Condition[CurrentConditionI].EndCriterium.LastCycles);

  FRegData := TRegData.Create(Self, LDataPath+'000.data');
  FRegChat := TRegData.Create(Self, LDataPath+'000.chat');
  WriteReportHeader;
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
        WriteReportRowPrompt; //TODO: FIND WHY OPTIMIZATION 3 GENERATES BUG HERE
      Clean;
    end;
end;

function TExperiment.ContingencyFired(AContingencyName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i:= 0 to ContingenciesCount[CurrentConditionI]-1 do
    if Condition[CurrentConditionI].Contingencies[i].ContingencyName = AContingencyName then
      begin
        Result := Condition[CurrentConditionI].Contingencies[i].Fired;
        Break;
      end;
end;

function TExperiment.ShouldStartExperiment: Boolean;
begin
  Result := PlayersCount = Condition[CurrentConditionI].Turn.Value;
end;

function TExperiment.DumpReportReader: string;
begin
  Result := FReportReader.Dump;
end;

function TExperiment.ShouldEndCondition: Boolean;
var
  LInterlocks: Real;
  LCyclesInCurrentCondition: Integer;
begin
  Result := False;
  // interlockings in the last x cycles
  LInterlocks := InterlockingsInLastCycles;

  // absolute cycles count
  LCyclesInCurrentCondition := CurrentCondition.Cycles.Count;
  case FConditions[CurrentConditionI].EndCriterium.Style of
    gecWhichComeFirst:
      begin
        if (LCyclesInCurrentCondition = CurrentCondition.EndCriterium.AbsoluteCycles) or
           (LInterlocks >= CurrentCondition.EndCriterium.InterlockingPorcentage) then
          Result := True;

      end;
    gecAbsoluteCycles:
        if LCyclesInCurrentCondition = CurrentCondition.EndCriterium.AbsoluteCycles then
          Result := True;

    gecInterlockingPorcentage:
        if LInterlocks >= CurrentCondition.EndCriterium.InterlockingPorcentage then
          Result := True;
  end;
end;

function TExperiment.CurrentConditionAsString: UTF8String;
begin
  if ABPoints then
    Result :=
      IntToStr(Condition[CurrentConditionI].Points.OnStart.A)+'|'+
      IntToStr(Condition[CurrentConditionI].Points.OnStart.B)+'|'+
      IntToStr(Condition[CurrentConditionI].Points.OnStart.G)
  else
    Result:=
      IntToStr(Condition[CurrentConditionI].Points.OnStart.A)+'|'+
      IntToStr(Condition[CurrentConditionI].Points.OnStart.G);
end;

//procedure TExperiment.TargetInterlocking;
//begin
//  SetTargetInterlocking;
//end;

procedure TExperiment.SaveToFile(AFilename: string);
begin
  SaveExperimentToFile(Self,AFilename);
end;

procedure TExperiment.SaveToFile;
begin
  if FFilename <> '' then
    SaveExperimentToFile(Self,FFilename)
  else;
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

  FRegData.CloseAndOpen;
end;

procedure TExperiment.Play;
begin
  FConditionMustBeUpdated := '';
  FCycles.GenerationValue := CurrentCondition.Cycles.Value;
  FState:=xsRunning;
  if Assigned(FOnStartExperiment) then FOnStartExperiment(Self);
end;


end.

