unit game_experiment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , game_actors
  , regdata
  ;

type

  { TExperiment }

  TPlayers = array of TPlayer;
  TConditions = array of TCondition;

  TExperiment = class(TComponent)
  private
    FExperimentAim,
    FExperimentName,
    FFilename,
    FResearcher : string;
    FMatrixType: TGameMatrixType;
    FRegData : TRegData;
    FGenPlayersAsNeeded : Boolean;
    FPlayersPlaying : TList;
    FPlayers : TPlayers;
    FCurrentCondition : integer;
    FConditions : TConditions;
    FResearcherCanChat: Boolean;
    FResearcherCanPlay: Boolean;
    FSendChatHistoryForNewPlayers: Boolean;
    FShowChat: Boolean;
    function GetCondition(I : Integer): TCondition;
    function GetConditionsCount: integer;
    function GetContingency(ACondition, I : integer): TContingency;
    function GetNextTurn: integer;
    function GetNextTurnPlayerID: UTF8string;
    function GetPlayer(I : integer): TPlayer; overload;
    function GetPlayer(AID : string): TPlayer; overload;
    function GetPlayerAsString(P: TPlayer): UTF8string;
    function GetPlayerFromString(s : string): TPlayer;
    function GetPlayerIndexFromID(AID : string): integer;
    function GetPlayerIsPlaying(AID : string): Boolean;
    function GetPlayerPointer(i: integer): PPlayer;
    function GetPlayersCount: integer;
    //function GetPlayersPlaying: TList;
    procedure SetCondition(I : Integer; AValue: TCondition);
    procedure SetContingency(ACondition, I : integer; AValue: TContingency);
    procedure SetMatrixType(AValue: TGameMatrixType);
    procedure SetPlayer(I : integer; AValue: TPlayer); overload;
    procedure SetPlayer(S : string ; AValue: TPlayer); overload;
    procedure SetPlayersPlaying(AValue: TList);
    procedure SetResearcherCanChat(AValue: Boolean);
    procedure SetResearcherCanPlay(AValue: Boolean);
    procedure SetSendChatHistoryForNewPlayers(AValue: Boolean);
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
    property ResearcherCanPlay : Boolean read FResearcherCanPlay write SetResearcherCanPlay;
    property ResearcherCanChat : Boolean read FResearcherCanChat write SetResearcherCanChat;
    property Researcher : string read FResearcher write FResearcher;
    property Condition[I : Integer]: TCondition read GetCondition write SetCondition;
    property ConditionsCount : integer read GetConditionsCount;
    property CurrentCondition : integer read FCurrentCondition write FCurrentCondition;
    property Contingency[C, I : integer] : TContingency read GetContingency write SetContingency;
    property ExperimentAim : string read FExperimentAim write FExperimentAim;
    property ExperimentName : string read FExperimentName write FExperimentName;
    property GenPlayersAsNeeded : Boolean read FGenPlayersAsNeeded write FGenPlayersAsNeeded;
    property Player[I : integer] : TPlayer read GetPlayer write SetPlayer;
    property PlayerFromID[S : string ] : TPlayer read GetPlayer write SetPlayer;
    property PlayersCount : integer read GetPlayersCount; // how many players per turn?
    property PlayersPlaying : TList read FPlayersPlaying write SetPlayersPlaying; // how many players are playing?
    property PlayerIsPlaying[s : string] : Boolean read GetPlayerIsPlaying;
    property PlayerIndexFromID[s : string]: integer read GetPlayerIndexFromID;
    property PlayerAsString[P:TPlayer]: UTF8string read GetPlayerAsString;
    property PlayerFromString[s : string]: TPlayer read GetPlayerFromString;
    property PlayerPointer[i:integer]: PPlayer read GetPlayerPointer;
    property ShowChat : Boolean read FShowChat write FShowChat;
    property SendChatHistoryForNewPlayers : Boolean read FSendChatHistoryForNewPlayers write SetSendChatHistoryForNewPlayers;
    property MatrixType : TGameMatrixType read FMatrixType write SetMatrixType;
    property NextTurnPlayerID : UTF8string read GetNextTurnPlayerID;
    property NextTurn : integer read GetNextTurn;
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
  Result := High(FConditions);
end;

function TExperiment.GetContingency(ACondition, I : integer): TContingency;
begin
  Result := FConditions[ACondition].Contingencies[I];
end;

function TExperiment.GetNextTurn: integer; // used during player arriving
begin
  Result := FConditions[CurrentCondition].Turn.Count;
  if FConditions[CurrentCondition].Turn.Count = FConditions[CurrentCondition].Turn.Value then
    FConditions[CurrentCondition].Turn.Count := 0
  else Inc(FConditions[CurrentCondition].Turn.Count);
end;

function TExperiment.GetNextTurnPlayerID: UTF8string; // used during cycles
var
  P : PPlayer;
begin
  Result := '';
  P := New(PPlayer);
  P := PlayersPlaying[FConditions[CurrentCondition].Turn.Count];
  Result := P^.ID;
  GetNextTurn;
  Dispose(P);
end;

function TExperiment.GetPlayer(I : integer): TPlayer;
begin
  Result := FPlayers[i];
end;

function TExperiment.GetPlayer(AID: string): TPlayer;
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
      gpsPlayed: Result := '1';
      gpsPlaying: Result := '2';
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
    , GetChoiceString(P.Choice.Current)
    , GetChoiceString(P.Choice.Last)
  ]);
  for i := 0 to Length(M)-1 do
    Result += M[i] + '|';
end;

function TExperiment.GetPlayerFromString(s : string): TPlayer;

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
      '1': Result := gpsPlayed;
      '2': Result := gpsPlaying;
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
  Result.Choice.Current := GetChoiceFromString(ExtractDelimited(5,s,['|']));
  Result.Choice.Last := GetChoiceFromString(ExtractDelimited(6,s,['|']));
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
  Result := PlayersPlaying.Count > 0;
  if Result then
    for i := 0 to PlayersPlaying.Count -1 do
      if PPlayer(PlayersPlaying[i])^.ID = AID then
        Exit;
  Result:= False;
end;

function TExperiment.GetPlayerPointer(i: integer): PPlayer;
begin
  Result := @FPlayers[i];
end;

function TExperiment.GetPlayersCount: integer;
begin
  Result := Length(FPlayers);
end;

//function TExperiment.GetPlayersPlaying: TList;
//var
//  //i:integer;
//  //P:PPlayer;
//begin
//  //P := New(PPlayer);
//  //if FPlayersPlaying.Count > 0 then
//  //  FPlayersPlaying.Clear;
//  //
//  //for i := Low(FPlayers) to High(FPlayers) do
//  //  if FPlayers[i].Status = gpsPlaying then
//  //    begin
//  //      P := @FPlayers[i];
//  //      FPlayersPlaying.Add(P);
//  //    end;
//  //Dispose(P);
//  Result := FPlayersPlaying;
//end;

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


procedure TExperiment.SetPlayer(I : integer; AValue: TPlayer);
begin
  FPlayers[I] := AValue;
end;

procedure TExperiment.SetPlayer(S : string ; AValue: TPlayer);
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

procedure TExperiment.SetPlayersPlaying(AValue: TList);
begin
  if FPlayersPlaying = AValue then Exit;
  FPlayersPlaying := AValue;
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

constructor TExperiment.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPlayersPlaying := TList.Create;
  LoadExperimentFromResource(Self);
end;

constructor TExperiment.Create(AFilename: string;AOwner:TComponent);
begin
  inherited Create(AOwner);
  LoadExperimentFromFile(Self,AFilename);
end;

destructor TExperiment.Destroy;
begin
  FPlayersPlaying.Free;
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

end.

