unit game_experiment;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils
  , game_actors
  , regdata
  ;

type

  { TExperiment }

  TExperimentState = (xsWaiting,xsRunning,xsPaused,xsCanceled);
  TConditions = array of TCondition;

  TExperiment = class(TComponent)
  private
    FExperimentAim,
    FExperimentName,
    FFilename,
    FResearcher : UTF8string;
    FOnConsequence: TNotifyEvent;
    FOnInterlocking: TNotifyEvent;
    FOnEndTurn: TNotifyEvent;
    FOnEndCondition: TNotifyEvent;
    FOnEndCycle: TNotifyEvent;
    FOnEndExperiment: TNotifyEvent;
    FOnEndGeneration: TNotifyEvent;
    FMatrixType: TGameMatrixType;
    FRegData : TRegData;
    FGenPlayersAsNeeded : Boolean;
    FPlayers : TPlayers;
    FCurrentCondition : integer;
    FConditions : TConditions;
    FResearcherCanChat: Boolean;
    FResearcherCanPlay: Boolean;
    FSendChatHistoryForNewPlayers: Boolean;
    FShowChat: Boolean;
    FState: TExperimentState;
    FTurnsRandom : TStringList;
    function GetCondition(I : Integer): TCondition;
    function GetConditionsCount: integer;
    function GetContingenciesCount(C: integer): integer;
    function GetContingency(ACondition, I : integer): TContingency;
    function GetNextTurn: integer;
    function GetNextTurnPlayerID: UTF8string;
    function GetNextCycle:integer;
    function GetNextCondition:integer;
    function GetPlayer(I : integer): TPlayer; overload;
    function GetPlayer(AID : UTF8string): TPlayer; overload;
    function AliasPlayerAsString(P: TPlayer): UTF8string;
    function AliasPlayerFromString(s : UTF8string): TPlayer;
    function GetPlayerIndexFromID(AID : UTF8string): integer;
    function GetPlayerIsPlaying(AID : UTF8string): Boolean;
    function GetPlayersCount: integer;
    function GetInterlockingsIn(ALastCycles : integer):integer;
    function GetConsequenceStringFromChoice(P:TPlayer): Utf8string;
    function GetConsequenceStringFromChoices:UTF8String;
    procedure SetCondition(I : Integer; AValue: TCondition);
    procedure SetContingency(ACondition, I : integer; AValue: TContingency);
    procedure SetMatrixType(AValue: TGameMatrixType);
    procedure SetOnConsequence(AValue: TNotifyEvent);
    procedure SetOnEndCondition(AValue: TNotifyEvent);
    procedure SetOnEndCycle(AValue: TNotifyEvent);
    procedure SetOnEndExperiment(AValue: TNotifyEvent);
    procedure SetOnEndGeneration(AValue: TNotifyEvent);
    procedure SetOnEndTurn(AValue: TNotifyEvent);
    procedure SetOnInterlocking(AValue: TNotifyEvent);
    procedure SetPlayer(I : integer; AValue: TPlayer); overload;
    procedure SetPlayer(S : UTF8string ; AValue: TPlayer); overload;
    procedure SetResearcherCanChat(AValue: Boolean);
    procedure SetResearcherCanPlay(AValue: Boolean);
    procedure SetSendChatHistoryForNewPlayers(AValue: Boolean);
    procedure SetState(AValue: TExperimentState);
  private
    procedure Consequence(Sender : TObject);
    procedure Interlocking(Sender : TObject);
  public
    constructor Create(AOwner:TComponent);override;
    constructor Create(AFilename: string; AOwner:TComponent); overload;
    destructor Destroy; override;
    function LoadFromFile(AFilename: string):Boolean;
    function LoadFromGenerator:Boolean;
    function AppendCondition : integer; overload;
    function AppendCondition(ACondition : TCondition) : integer;overload;
    function AppendContingency(ACondition : integer) : integer;overload;
    function AppendContingency(ACondition : integer;AContingency : TContingency) : integer;overload;
    function AppendPlayer : integer;overload;
    function AppendPlayer(APlayer : TPlayer) : integer; overload;
    procedure SaveToFile(AFilename: string); overload;
    procedure SaveToFile; overload;
    procedure Clean;
    procedure Play;
    property ResearcherCanPlay : Boolean read FResearcherCanPlay write SetResearcherCanPlay;
    property ResearcherCanChat : Boolean read FResearcherCanChat write SetResearcherCanChat;
    property Researcher : UTF8string read FResearcher write FResearcher;
    property Condition[I : Integer]: TCondition read GetCondition write SetCondition;
    property ConditionsCount : integer read GetConditionsCount;
    property CurrentCondition : integer read FCurrentCondition write FCurrentCondition;
    property Contingency[C, I : integer] : TContingency read GetContingency write SetContingency;
    property ContingenciesCount[C:integer]:integer read GetContingenciesCount;
    property ExperimentAim : UTF8string read FExperimentAim write FExperimentAim;
    property ExperimentName : UTF8string read FExperimentName write FExperimentName;
    property GenPlayersAsNeeded : Boolean read FGenPlayersAsNeeded write FGenPlayersAsNeeded;
    property InterlockingsIn[i:integer]:integer read GetInterlockingsIn;
    property Player[I : integer] : TPlayer read GetPlayer write SetPlayer;
    property PlayerFromID[S : UTF8string ] : TPlayer read GetPlayer write SetPlayer;
    property PlayersCount : integer read GetPlayersCount;
    property PlayerIsPlaying[s : UTF8string] : Boolean read GetPlayerIsPlaying;
    property PlayerIndexFromID[s : UTF8string]: integer read GetPlayerIndexFromID;
    property PlayerAsString[P:TPlayer]: UTF8string read AliasPlayerAsString;
    property PlayerFromString[s : UTF8string]: TPlayer read AliasPlayerFromString;
    property ConsequenceStringFromChoice[P:Tplayer]:UTF8String read GetConsequenceStringFromChoice;
    property ConsequenceStringFromChoices: UTF8String read GetConsequenceStringFromChoices;
    property ShowChat : Boolean read FShowChat write FShowChat;
    property SendChatHistoryForNewPlayers : Boolean read FSendChatHistoryForNewPlayers write SetSendChatHistoryForNewPlayers;
    property MatrixType : TGameMatrixType read FMatrixType write SetMatrixType;
    property NextTurnPlayerID : UTF8string read GetNextTurnPlayerID;
    property NextTurn : integer read GetNextTurn;
    property NextCycle : integer read GetNextCycle;
    property NextCondition : integer read GetNextCondition;
    property State : TExperimentState read FState write SetState;
  public
    property OnEndTurn : TNotifyEvent read FOnEndTurn write SetOnEndTurn;
    property OnEndCycle : TNotifyEvent read FOnEndCycle write SetOnEndCycle;
    property OnEndGeneration : TNotifyEvent read FOnEndGeneration write SetOnEndGeneration;
    property OnEndCondition : TNotifyEvent read FOnEndCondition write SetOnEndCondition;
    property OnEndExperiment : TNotifyEvent read FOnEndExperiment write SetOnEndExperiment;
  public
    property OnConsequence : TNotifyEvent read FOnConsequence write SetOnConsequence;
    property OnInterlocking : TNotifyEvent read FOnInterlocking write SetOnInterlocking;
  end;

resourcestring
  WARN_CANNOT_SAVE = 'O experimento não pode ser salvo.';

implementation

uses game_file_methods, game_actors_point, game_resources, string_methods;

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

function TExperiment.GetNextTurn: integer; // used during player arriving
begin
  if FConditions[CurrentCondition].Turn.Random then
    Result := StrToInt(FTurnsRandom.Names[FConditions[CurrentCondition].Turn.Count])
  else
    Result := FConditions[CurrentCondition].Turn.Count;
  if Assigned(FOnEndTurn) then FOnEndTurn(Self);
  if FConditions[CurrentCondition].Turn.Count < FConditions[CurrentCondition].Turn.Value then
    Inc(FConditions[CurrentCondition].Turn.Count)
  else
    begin
      FConditions[CurrentCondition].Turn.Count := 0;
      NextCycle;
    end;
{$IFDEF DEBUG}
  WriteLn('TExperiment.GetNextTurn:',Result);
{$ENDIF}
end;

function TExperiment.GetNextTurnPlayerID: UTF8string; // used during cycles
begin
  Result := Player[FConditions[CurrentCondition].Turn.Count].ID;
  GetNextTurn;
end;

function TExperiment.GetNextCycle: integer;
begin
  Result := FConditions[CurrentCondition].Cycles.Count;
  if Assigned(FOnEndCycle) then FOnEndCycle(Self);
  if FConditions[CurrentCondition].Cycles.Count < FConditions[CurrentCondition].Cycles.Value then
    Inc(FConditions[CurrentCondition].Cycles.Count)
  else
    begin
      FConditions[CurrentCondition].Cycles.Count := 0;
      Inc(FConditions[CurrentCondition].Cycles.Generation);
      if Assigned(FOnEndGeneration) then FOnEndGeneration(Self);
      NextCondition;
    end;
  {$IFDEF DEBUG}
    WriteLn('TExperiment.GetNextCycle:',Result);
  {$ENDIF}
end;

function TExperiment.GetNextCondition: integer;
var
  LAbsCycles : integer;
  LInterlocks : integer;

  procedure EndCondition;
  begin
    Inc(FCurrentCondition);
    if Assigned(FOnEndCondition) then FOnEndCondition(Self);
  end;

begin
  Result := CurrentCondition;
  LAbsCycles := (FConditions[CurrentCondition].Cycles.Value *
             FConditions[CurrentCondition].Cycles.Generation) + FConditions[CurrentCondition].Cycles.Count;

  // interlockings in last x cycles
  LInterlocks := InterlockingsIn[FConditions[CurrentCondition].EndCriterium.LastCycles];
  case FConditions[CurrentCondition].EndCriterium.Value of
    gecWhichComeFirst:
      begin
        if (LAbsCycles = FConditions[CurrentCondition].EndCriterium.AbsoluteCycles) or
           (LInterlocks = FConditions[CurrentCondition].EndCriterium.InterlockingPorcentage) then
          EndCondition;

      end;
    gecAbsoluteCycles:
        if LAbsCycles = FConditions[CurrentCondition].EndCriterium.AbsoluteCycles then
          EndCondition;

    gecInterlockingPorcentage:
        if LInterlocks = FConditions[CurrentCondition].EndCriterium.InterlockingPorcentage then
          EndCondition

  end;
  {$IFDEF DEBUG}
    WriteLn('TExperiment.GetNextCondition:',Result);
  {$ENDIF}
end;

function TExperiment.GetPlayer(I : integer): TPlayer;
begin
  Result := FPlayers[i];
end;

function TExperiment.GetPlayer(AID: UTF8string): TPlayer;
var
  i : integer;
begin
  //Result.ID := '';
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

function TExperiment.GetInterlockingsIn(ALastCycles: integer): integer;
begin

end;

function TExperiment.GetConsequenceStringFromChoice(P: TPlayer): Utf8string;
var
  i : integer;
  c : integer;
begin
  c := CurrentCondition;
  PlayerFromID[P.ID] := P;
  Result:= '';
  for i :=0 to ContingenciesCount[c] -1 do
    if not Contingency[c,i].Meta then
      if Contingency[c,i].ResponseMeetsCriteriaI(P.Choice.Row,P.Choice.Color) then
        Result += Contingency[c,i].Consequence.AsString(P.ID) + '+';
end;

function TExperiment.GetConsequenceStringFromChoices: UTF8String;
var
  i : integer;
  c : integer;
begin
  c := CurrentCondition;
  Result:= '';
  for i :=0 to ContingenciesCount[c] -1 do
    if Contingency[c,i].Meta then
      if Contingency[c,i].ResponseMeetsCriteriaG(FPlayers) then
        Result += Contingency[c,i].Consequence.AsString(IntToStr(i)) + '+';
end;

procedure TExperiment.SetCondition(I : Integer; AValue: TCondition);
begin
  FConditions[I] := AValue;
end;

procedure TExperiment.SetContingency(ACondition, I : integer; AValue: TContingency);
begin
  FConditions[ACondition].Contingencies[I] := AValue;
  if FConditions[ACondition].Contingencies[I].Meta then
    FConditions[ACondition].Contingencies[I].OnCriteria:=@Interlocking
  else
    FConditions[ACondition].Contingencies[I].OnCriteria:=@Consequence;
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

procedure TExperiment.Consequence(Sender: TObject);
begin
  if Assigned(FOnConsequence) then FOnConsequence(Sender);
end;

procedure TExperiment.Interlocking(Sender: TObject);
begin
  if Assigned(FOnInterlocking) then FOnInterlocking(Sender);
end;

constructor TExperiment.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadExperimentFromResource(Self);
end;

constructor TExperiment.Create(AFilename: string;AOwner:TComponent);
var c ,
    i,
    r : integer;
begin
  inherited Create(AOwner);
  FTurnsRandom := TStringList.Create;
  LoadExperimentFromFile(Self,AFilename);
  if Condition[CurrentCondition].Turn.Random then
    begin
      for i:= 0 to Condition[CurrentCondition].Turn.Value-1 do
        FTurnsRandom.Add(IntToStr(i));

      c := FTurnsRandom.Count - 1;
      for i := 0 to c do
        begin
          r := Random(c);
          while r = i do r := Random(c);
          FTurnsRandom.Exchange(r,i);
        end;
    end;
end;

destructor TExperiment.Destroy;
begin
  FTurnsRandom.Free;
  inherited Destroy;
end;

function TExperiment.LoadFromFile(AFilename: string): Boolean;
begin
  Result := LoadExperimentFromFile(Self, AFilename);
  if Result then
    FFilename := AFilename;
end;

function TExperiment.LoadFromGenerator: Boolean;
begin
  Result := LoadExperimentFromResource(Self);
  if Result then
    FFilename := GetCurrentDir + PathDelim + FResearcher + PathDelim;
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

procedure TExperiment.SaveToFile(AFilename: string);
begin
  SaveExperimentToFile(Self,AFilename);
end;

procedure TExperiment.SaveToFile;
begin
  if FFilename <> '' then
    SaveExperimentToFile(Self,FFilename)
  else
{$IFDEF DEBUG}
  WriteLn(WARN_CANNOT_SAVE)
{$ENDIF};
end;

procedure TExperiment.Clean;
begin

end;

procedure TExperiment.Play;
begin

end;

end.

