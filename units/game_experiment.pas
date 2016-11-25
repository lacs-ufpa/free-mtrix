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
    function GetPlayerAsString(P: TPlayer): UTF8string;
    function GetPlayerFromString(s : UTF8string): TPlayer;
    function GetPlayerIndexFromID(AID : UTF8string): integer;
    function GetPlayerIsPlaying(AID : UTF8string): Boolean;
    function GetPlayersCount: integer;
    function GetInterlockingsIn(ALastCycles : integer):integer;
    function GetConsequenceStringFromChoice(P:TPlayer): Utf8string;
    procedure SetCondition(I : Integer; AValue: TCondition);
    procedure SetContingency(ACondition, I : integer; AValue: TContingency);
    procedure SetMatrixType(AValue: TGameMatrixType);
    procedure SetOnEndCondition(AValue: TNotifyEvent);
    procedure SetOnEndCycle(AValue: TNotifyEvent);
    procedure SetOnEndExperiment(AValue: TNotifyEvent);
    procedure SetOnEndGeneration(AValue: TNotifyEvent);
    procedure SetOnEndTurn(AValue: TNotifyEvent);
    procedure SetPlayer(I : integer; AValue: TPlayer); overload;
    procedure SetPlayer(S : UTF8string ; AValue: TPlayer); overload;
    procedure SetResearcherCanChat(AValue: Boolean);
    procedure SetResearcherCanPlay(AValue: Boolean);
    procedure SetSendChatHistoryForNewPlayers(AValue: Boolean);
    procedure SetState(AValue: TExperimentState);
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
    property PlayerAsString[P:TPlayer]: UTF8string read GetPlayerAsString;
    property PlayerFromString[s : UTF8string]: TPlayer read GetPlayerFromString;
    property ConsequenceStringFromChoice[P:Tplayer]:UTF8String read GetConsequenceStringFromChoice;
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
  end;

resourcestring
  WARN_CANNOT_SAVE = 'O experimento não pode ser salvo.';

implementation

uses game_file_methods, game_actors_point, game_resources, strutils;

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
      if Assigned(FOnEndCycle) then FOnEndCycle(Self);
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
  if FConditions[CurrentCondition].Cycles.Count < FConditions[CurrentCondition].Cycles.Value then
    Inc(FConditions[CurrentCondition].Cycles.Count)
  else
    begin
      FConditions[CurrentCondition].Cycles.Count := 0;
      if State = xsRunning then
        begin
          if Assigned(FOnEndGeneration) then FOnEndGeneration(Self);
          NextCondition;
        end;
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
  Inc(FConditions[CurrentCondition].Cycles.Generation);
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
function TExperiment.GetPlayerAsString(P: TPlayer): UTF8string;
var
  i : integer;
  M : array of UTF8String;

  procedure SetM(A : array of UTF8String);
  var i : integer;
  begin
    SetLength(M,Length(A));
    for i := 0 to Length(A) -1 do
      M[i] := A[i];
  end;

  function GetPPointsString(APPoints : TPlayerPoints) : string;
  begin
    Result := IntToStr(APPoints.A)+VV_SEP+IntToStr(APPoints.B);
  end;

  function GetStatusString(AStatus : TGamePlayerStatus): string;
  begin
    case AStatus of
      gpsWaiting: Result := '0';
      gpsPlaying: Result := '1';
      gpsPlayed: Result := '2';
    end;
  end;

  function GetRowString(ARow: TGameRow): string;
  begin
    case ARow of
      grNone : Result := '.';
      grOne : Result := '1';
      grTwo : Result := '2';
      grThree : Result :='3';
      grFour : Result := '4';
      grFive : Result := '5';
      grSix : Result := '6';
      grSeven : Result := '7';
      grEight : Result := '8';
      grNine : Result := '9';
      grTen : Result := '0';
    end;
  end;

  function GetColorString(AColor: TGameColor): string;
  begin
    case AColor of
      gcNone :Result  :=  '0';
      gcYellow :Result  :=  '1';
      gcRed :Result  :=  '2';
      gcMagenta :Result  :=  '3';
      gcBlue :Result  :=  '4';
      gcGreen :Result  :=  '5';
    end;
  end;

  function GetChoiceString(AChoice : TPlayerChoice) : string;
  begin
    Result := GetRowString(AChoice.Row) + VV_SEP;
    Result := Result+ GetColorString(AChoice.Color);
  end;

begin
  Result := '';
  SetM([P.ID
    , P.Nicname
    , GetPPointsString(P.Points)
    , GetStatusString(P.Status)
    , GetChoiceString(P.Choice)
    , IntToStr(P.Turn)
  ]);
  for i := 0 to Length(M)-1 do
    Result += M[i] + '|';
end;

function TExperiment.GetPlayerFromString(s: UTF8string): TPlayer;

  function GetRowFromString(S: string): TGameRow;
  begin
    case S of
      '.'  : Result := grNone;
      '1' : Result := grOne;
      '2' : Result := grTwo;
      '3' : Result := grThree;
      '4' : Result := grFour;
      '5' : Result := grFive;
      '6' : Result := grSix;
      '7' : Result := grSeven;
      '8' : Result := grEight;
      '9' : Result := grNine;
      '0' : Result := grTen;
    end;
  end;

  function GetColorFromString(S: string): TGameColor;
  begin
    case S of
      '0'  : Result := gcNone;
      '1' : Result := gcYellow;
      '2' : Result := gcRed;
      '3' : Result := gcMagenta;
      '4' : Result := gcBlue;
      '5' : Result := gcGreen;
    end;
  end;

  function GetChoiceFromString(S:string) : TPlayerChoice;
  begin
    Result.Row := GetRowFromString(ExtractDelimited(1,S,[',']));
    Result.Color := GetColorFromString(ExtractDelimited(2,S,[',']));
  end;

  function GetPPointsFromString(S:string) : TPlayerPoints;
  begin
    Result.A := StrToInt(ExtractDelimited(1,S,[',']));
    Result.B := StrToInt(ExtractDelimited(2,S,[',']));
  end;

  function GetStatusFromString(S : string): TGamePlayerStatus;
  begin
    case S of
      '0': Result := gpsWaiting;
      '1': Result := gpsPlaying;
      '2': Result := gpsPlayed;
    end;
  end;
begin
  {$IFDEF DEBUG}
    WriteLn(ExtractDelimited(1,s,['|']));
    WriteLn(ExtractDelimited(2,s,['|']));
    WriteLn(ExtractDelimited(3,s,['|']));
    WriteLn(ExtractDelimited(4,s,['|']));
    WriteLn(ExtractDelimited(5,s,['|']));
    WriteLn(ExtractDelimited(6,s,['|']));
  {$ENDIF}
  Result.ID := ExtractDelimited(1,s,['|']);
  Result.Nicname := ExtractDelimited(2,s,['|']);
  Result.Points := GetPPointsFromString(ExtractDelimited(3,s,['|']));
  Result.Status := GetStatusFromString(ExtractDelimited(4,s,['|']));
  Result.Choice := GetChoiceFromString(ExtractDelimited(5,s,['|']));
  Result.Turn:=StrToInt(ExtractDelimited(6,s,['|']));
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
        Result += Contingency[c,i].Consequence.AsString + '+';
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

