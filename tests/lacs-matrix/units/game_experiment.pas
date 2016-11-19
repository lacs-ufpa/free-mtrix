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
    FShowChat: Boolean;
    function GetCondition(I : Integer): TCondition;
    function GetConditionsCount: integer;
    function GetContingency(ACondition, I : integer): TContingency;
    function GetPlayer(I : integer): TPlayer;
    function GetPlayersCount: integer;
    function GetPlayersPlaying: TList;
    procedure SetCondition(I : Integer; AValue: TCondition);
    procedure SetContingency(ACondition, I : integer; AValue: TContingency);
    procedure SetMatrixType(AValue: TGameMatrixType);
    procedure SetPlayer(I : integer; AValue: TPlayer);
    procedure SetPlayersPlaying(AValue: TList);
    procedure SetResearcherCanChat(AValue: Boolean);
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
    property ResearcherCanChat : Boolean read FResearcherCanChat write SetResearcherCanChat ;
    property Researcher : string read FResearcher write FResearcher;
    property Condition[I : Integer]: TCondition read GetCondition write SetCondition;
    property ConditionsCount : integer read GetConditionsCount;
    property CurrentCondition : integer read FCurrentCondition write FCurrentCondition;
    property Contingency[C, I : integer] : TContingency read GetContingency write SetContingency;
    property ExperimentAim : string read FExperimentAim write FExperimentAim;
    property ExperimentName : string read FExperimentName write FExperimentName;
    property GenPlayersAsNeeded : Boolean read FGenPlayersAsNeeded write FGenPlayersAsNeeded;
    property Player[I : integer] : TPlayer read GetPlayer write SetPlayer;
    property PlayersCount : integer read GetPlayersCount; // how many players per turn?
    property PlayersPlaying : TList read GetPlayersPlaying write SetPlayersPlaying; // how many players are playing?
    property ShowChat : Boolean read FShowChat write FShowChat;
    property MatrixType : TGameMatrixType read FMatrixType write SetMatrixType;
  end;

resourcestring
  WARN_CANNOT_SAVE = 'O experimento não pode ser salvo.';

implementation

uses game_file_methods, game_actors_point,Dialogs;

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

function TExperiment.GetPlayer(I : integer): TPlayer;
begin
  Result := FPlayers[i];
end;

function TExperiment.GetPlayersCount: integer;
begin
  if Length(FPlayers) = 0 then
    Result := High(FPlayers)
  else
    Result := -1;
end;

function TExperiment.GetPlayersPlaying: TList;
var
  i:integer;
  P:PPlayer;
begin
  if FPlayersPlaying.Count > 0 then
    FPlayersPlaying.Clear;

  for i := Low(FPlayers) to High(FPlayers) do
    if Player[i].Status = gpsPlaying then
      begin
        P := nil;
        P^ := Player[i];
        FPlayersPlaying.Add(P);
      end;

  Result := FPlayersPlaying;
end;

procedure TExperiment.SetCondition(I : Integer; AValue: TCondition);
begin
  if (I >= Low(FConditions)) and (I <= High(FConditions)) then
    FConditions[I] := AValue;
end;

procedure TExperiment.SetContingency(ACondition, I : integer; AValue: TContingency);
begin
  if (ACondition >= Low(FConditions)) and (ACondition <= High(FConditions)) then
      if (I >= Low(FConditions[ACondition].Contingencies)) and (I <= High(FConditions[ACondition].Contingencies)) then
        FConditions[ACondition].Contingencies[I] := AValue;
end;

procedure TExperiment.SetMatrixType(AValue: TGameMatrixType);
begin
  if FMatrixType=AValue then Exit;
  FMatrixType:=AValue;
end;


procedure TExperiment.SetPlayer(I : integer; AValue: TPlayer);
begin
  if (I >= Low(FPlayers)) and (I <= High(FPlayers)) then
    FPlayers[I] := AValue;
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
  Result := Length(FPlayers)-1;
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
    WriteLn(WARN_CANNOT_SAVE)
end;

end.

