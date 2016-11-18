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

  TPlayersPlaying = array of integer;

  TExperiment = class(TComponent)
  private
    FExperimentAim,
    FExperimentName,
    FFilename,
    FResearcher : string;
    FRegData : TRegData;
    FGenPlayersAsNeeded : Boolean;
    FPlayersPlaying : TPlayersPlaying;
    FPlayers : array of TPlayer;
    FCurrentCondition : integer;
    FConditions : array of TCondition;
    FShowChat: Boolean;
    function GetCondition(I : Integer): TCondition;
    function GetConditionsCount: integer;
    function GetContingency(ACondition, I : integer): TContingency;
    function GetPlayer(I : integer): TPlayer;
    function GetPlayersCount: integer;
    function GetPlayersPlaying: TPlayersPlaying;
    procedure SetCondition(I : Integer; AValue: TCondition);
    procedure SetContingency(ACondition, I : integer; AValue: TContingency);
    procedure SetPlayer(I : integer; AValue: TPlayer);
    procedure SetPlayersPlaying(AValue: TPlayersPlaying);
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
    function AppendPlayer(APlayer : TPlayer) : integer;overload;
    procedure SaveToFile(AFilename: string); overload;
    procedure SaveToFile; overload;
    property Researcher : string read FResearcher write FResearcher;
    property Condition[I : Integer]: TCondition read GetCondition write SetCondition;
    property ConditionsCount : integer read GetConditionsCount;
    property CurrentCondition : integer read FCurrentCondition write FCurrentCondition;
    property Contingency[C, I : integer] : TContingency read GetContingency write SetContingency;
    property ExperimentAim : string read FExperimentAim write FExperimentAim;
    property ExperimentName : string read FExperimentName write FExperimentName;
    property GenPlayersAsNeeded : Boolean read FGenPlayersAsNeeded write FGenPlayersAsNeeded;
    property Player[I : integer] : TPlayer read GetPlayer write SetPlayer;
    property PlayersCount : integer read GetPlayersCount;
    property PlayersPlaying : TPlayersPlaying read GetPlayersPlaying write SetPlayersPlaying;
    property ShowChat : Boolean read FShowChat write FShowChat;
  end;

resourcestring
  WARN_CANNOT_SAVE = 'O experimento não pode ser salvo.';

implementation

uses game_file_methods, game_actors_point;

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
  Result := High(FPlayers);
end;

function TExperiment.GetPlayersPlaying: TPlayersPlaying;
var i:integer;
begin
  if Length(FPlayersPlaying) = 0 then
    for i := Low(FPlayers) to High(FPlayers) do
      if Player[i].Status = gpsPlaying then
        begin
          SetLength(FPlayersPlaying, Length(FPlayersPlaying)+1);
          FPlayersPlaying[High(FPlayersPlaying)] := i;
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

procedure TExperiment.SetPlayer(I : integer; AValue: TPlayer);
begin
  if (I >= Low(FPlayers)) and (I <= High(FPlayers)) then
    FPlayers[I] := AValue;
end;

procedure TExperiment.SetPlayersPlaying(AValue: TPlayersPlaying);
var i : integer; LAllEqualDontSet : Boolean;
begin
  LAllEqualDontSet := True;
  if Length(FPlayersPlaying) = Length(AValue) then
    for i := Low(AValue) to High(AValue) do
        if not FPlayersPlaying[i] <> AValue[i] then
          begin
            LAllEqualDontSet := False;
            Break;
          end;
  if LAllEqualDontSet then Exit;
  SetLength(FPlayersPlaying,Length(AValue));
  for i := Low(AValue) to High(AValue) do
      FPlayersPlaying[i] := AValue[i];
end;

constructor TExperiment.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadExperimentFromResource(Self);
end;

constructor TExperiment.Create(AFilename: string;AOwner:TComponent);
begin
  inherited Create(AOwner);
  LoadExperimentFromFile(Self,AFilename);
end;

destructor TExperiment.Destroy;
begin
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
  FConditions[AppendCondition] := ACondition;
  Result := High(FConditions);
end;

function TExperiment.AppendContingency(ACondition: integer): integer;
begin
  SetLength(FConditions[ACondition].Contingencies, Length(FConditions[ACondition].Contingencies)+1);
  //FConditions[ACondition].Contingencies[High(FConditions[ACondition].Contingencies)].Consequence := TConsequence.Create(Self);
  //FConditions[ACondition].Contingencies[High(FConditions[ACondition].Contingencies)].Consequence.Points.A := TGamePoint.Create(Self);
  Result := High(FConditions[ACondition].Contingencies);
end;

function TExperiment.AppendContingency(ACondition: integer;
  AContingency: TContingency): integer;
begin
  FConditions[ACondition].Contingencies[AppendContingency(ACondition)] := AContingency;
  Result := High(FConditions[ACondition].Contingencies);
end;

function TExperiment.AppendPlayer: integer;
begin
  SetLength(FPlayers, Length(FPlayers)+1);
  Result := High(FPlayers);
end;

function TExperiment.AppendPlayer(APlayer: TPlayer): integer;
begin
  FPlayers[AppendPlayer] := APlayer;
  Result := High(FPlayers);
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

