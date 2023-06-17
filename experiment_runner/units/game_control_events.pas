unit game_control_events;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils
  , game_actors
  ;

type
  TPlayerEvent = procedure (P : TPlayer; AMessage : string) of object;

  { TGameEvents

    event order convention:
   ->              ->             ->
   game_experiment,  game_control,  game_visual_board
  }

  TGameEvents = class(TComponent)
  protected
    procedure Close(Sender : TObject); virtual;
    procedure Consequence(Sender : TObject); virtual;
    procedure EndChoice(Sender : TObject); virtual;
    procedure EndCondition(Sender : TObject); virtual;
    procedure EndCycle(Sender : TObject); virtual;
    procedure EndExperiment(Sender : TObject);virtual;
    procedure EndGeneration(Sender : TObject); virtual;
    procedure EndTurn(Sender : TObject); virtual;
    procedure Interlocking(Sender : TObject); virtual;
    procedure PlayerExit(P : TPlayer; AMessage : string); virtual;
    procedure StartChoice(Sender : TObject); virtual;
    procedure StartCondition(Sender : TObject); virtual;
    procedure StartCycle(Sender : TObject); virtual;
    procedure StartExperiment(Sender : TObject); virtual;
    procedure StartGeneration(Sender : TObject); virtual;
    procedure StartTurn(Sender : TObject); virtual;
    procedure TargetInterlocking(Sender : TObject); virtual;
    procedure WaitForServer(Sender : TObject); virtual;
  private
    FOnClose : TNotifyEvent;
    FOnConsequence : TNotifyEvent;
    FOnEndChoice : TNotifyEvent;
    FOnEndCondition : TNotifyEvent;
    FOnEndCycle : TNotifyEvent;
    FOnEndExperiment : TNotifyEvent;
    FOnEndGeneration : TNotifyEvent;
    FOnEndTurn : TNotifyEvent;
    FOnInterlocking : TNotifyEvent;
    FOnPlayerExit : TPlayerEvent;
    FOnStartChoice : TNotifyEvent;
    FOnStartCondition : TNotifyEvent;
    FOnStartCycle : TNotifyEvent;
    FOnStartExperiment : TNotifyEvent;
    FOnStartGeneration : TNotifyEvent;
    FOnStartTurn : TNotifyEvent;
    FOnTargetInterlocking : TNotifyEvent;
    FOnWaitForServer : TNotifyEvent;
    procedure SetOnClose(AValue : TNotifyEvent);
    procedure SetOnConsequence(AValue : TNotifyEvent);
    procedure SetOnEndChoice(AValue : TNotifyEvent);
    procedure SetOnEndCondition(AValue : TNotifyEvent);
    procedure SetOnEndCycle(AValue : TNotifyEvent);
    procedure SetOnEndExperiment(AValue : TNotifyEvent);
    procedure SetOnEndGeneration(AValue : TNotifyEvent);
    procedure SetOnEndTurn(AValue : TNotifyEvent);
    procedure SetOnInterlocking(AValue : TNotifyEvent);
    procedure SetOnPlayerExit(AValue : TPlayerEvent);
    procedure SetOnStartChoice(AValue : TNotifyEvent);
    procedure SetOnStartCondition(AValue : TNotifyEvent);
    procedure SetOnStartCycle(AValue : TNotifyEvent);
    procedure SetOnStartExperiment(AValue : TNotifyEvent);
    procedure SetOnStartGeneration(AValue : TNotifyEvent);
    procedure SetOnStartTurn(AValue : TNotifyEvent);
    procedure SetOnTargetInterlocking(AValue : TNotifyEvent);
    procedure SetOnWaitForServer(AValue : TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    property OnConsequence : TNotifyEvent read FOnConsequence write SetOnConsequence;
    property OnEndChoice : TNotifyEvent read FOnEndChoice write SetOnEndChoice;
    property OnEndCondition : TNotifyEvent read FOnEndCondition write SetOnEndCondition;
    property OnEndCycle : TNotifyEvent read FOnEndCycle write SetOnEndCycle;
    property OnEndExperiment : TNotifyEvent read FOnEndExperiment write SetOnEndExperiment;
    property OnEndGeneration : TNotifyEvent read FOnEndGeneration write SetOnEndGeneration;
    property OnEndTurn  : TNotifyEvent read FOnEndTurn write SetOnEndTurn;
    property OnInterlocking : TNotifyEvent read FOnInterlocking write SetOnInterlocking;
    property OnStartChoice : TNotifyEvent read FOnStartChoice write SetOnStartChoice;
    property OnStartCondition : TNotifyEvent read FOnStartCondition write SetOnStartCondition;
    property OnStartCycle : TNotifyEvent read FOnStartCycle write SetOnStartCycle;
    property OnStartExperiment : TNotifyEvent read FOnStartExperiment write SetOnStartExperiment;
    property OnStartGeneration : TNotifyEvent read FOnStartGeneration write SetOnStartGeneration;
    property OnStartTurn : TNotifyEvent read FOnStartTurn write SetOnStartTurn;
    property OnTargetInterlocking : TNotifyEvent read FOnTargetInterlocking write SetOnTargetInterlocking;
    property OnWaitForServer : TNotifyEvent read FOnWaitForServer write SetOnWaitForServer;
  public
    property OnPlayerExit : TPlayerEvent read FOnPlayerExit write SetOnPlayerExit;
    //property OnClose : TNotifyEvent read FOnClose write SetOnClose;
  end;

implementation

{ TGameEvents }

procedure TGameEvents.Close(Sender : TObject);
begin
  { do something }
  //if Assigned(OnClose) then
  //  OnClose(Sender);
end;

procedure TGameEvents.Consequence(Sender : TObject);
begin
  { do something }
  if Assigned(OnConsequence) then
    OnConsequence(Sender);
end;

procedure TGameEvents.EndChoice(Sender : TObject);
begin
  { do something }
  if Assigned(OnEndChoice) then
    OnEndChoice(Sender);
end;

procedure TGameEvents.EndTurn(Sender : TObject);
begin
  { do something }
  if Assigned(OnEndTurn) then
    OnEndTurn(Sender);
end;

procedure TGameEvents.EndCondition(Sender : TObject);
begin
  { do something }
  if Assigned(OnEndCondition) then
    OnEndCondition(Sender);
end;

procedure TGameEvents.EndCycle(Sender : TObject);
begin
  { do something }
  if Assigned(OnEndCycle) then
    OnEndCycle(Sender);
end;

procedure TGameEvents.EndExperiment(Sender : TObject);
begin
  { do something }
  if Assigned(OnEndExperiment) then
    OnEndExperiment(Sender);
end;

procedure TGameEvents.EndGeneration(Sender : TObject);
begin
  { do something }
  if Assigned(OnEndGeneration) then
    OnEndGeneration(Sender);
end;

procedure TGameEvents.Interlocking(Sender : TObject);
begin
  { do something }
  if Assigned(OnInterlocking) then
    OnInterlocking(Sender);
end;

procedure TGameEvents.PlayerExit(P : TPlayer; AMessage : string);
begin
  { do something }
  if Assigned(OnPlayerExit) then
    OnPlayerExit(P, AMessage);
end;

procedure TGameEvents.StartChoice(Sender : TObject);
begin
  { do something }
  if Assigned(OnStartChoice) then
    OnStartChoice(Sender);
end;

procedure TGameEvents.StartCondition(Sender : TObject);
begin
  { do something }
  if Assigned(OnStartCondition) then
    OnStartCondition(Sender);
end;

procedure TGameEvents.StartCycle(Sender : TObject);
begin
  { do something }
  if Assigned(OnStartCycle) then
    OnStartCycle(Sender);
end;

procedure TGameEvents.StartExperiment(Sender : TObject);
begin
  { do something }
  if Assigned(OnStartExperiment) then
    OnStartExperiment(Sender);
end;

procedure TGameEvents.StartGeneration(Sender : TObject);
begin
  { do something }
  if Assigned(OnStartGeneration) then
    OnStartGeneration(Sender);
end;

procedure TGameEvents.StartTurn(Sender : TObject);
begin
  { do something }
  if Assigned(OnStartTurn) then
    OnStartTurn(Sender);
end;

procedure TGameEvents.TargetInterlocking(Sender : TObject);
begin
  { do something }
  if Assigned(OnTargetInterlocking) then
    OnTargetInterlocking(Sender);
end;

procedure TGameEvents.WaitForServer(Sender : TObject);
begin
  { do something }
  if Assigned(OnWaitForServer) then
    OnWaitForServer(Sender);
end;

procedure TGameEvents.SetOnConsequence(AValue : TNotifyEvent);
begin
  if FOnConsequence = AValue then Exit;
  FOnConsequence := AValue;
end;

procedure TGameEvents.SetOnClose(AValue : TNotifyEvent);
begin
  if FOnClose = AValue then Exit;
  FOnClose := AValue;
end;

procedure TGameEvents.SetOnEndChoice(AValue : TNotifyEvent);
begin
  if FOnEndChoice = AValue then Exit;
  FOnEndChoice := AValue;
end;

procedure TGameEvents.SetOnEndCondition(AValue : TNotifyEvent);
begin
  if FOnEndCondition = AValue then Exit;
  FOnEndCondition := AValue;
end;

procedure TGameEvents.SetOnEndCycle(AValue : TNotifyEvent);
begin
  if FOnEndCycle = AValue then Exit;
  FOnEndCycle := AValue;
end;

procedure TGameEvents.SetOnEndExperiment(AValue : TNotifyEvent);
begin
  if FOnEndExperiment = AValue then Exit;
  FOnEndExperiment := AValue;
end;

procedure TGameEvents.SetOnEndGeneration(AValue : TNotifyEvent);
begin
  if FOnEndGeneration = AValue then Exit;
  FOnEndGeneration := AValue;
end;

procedure TGameEvents.SetOnEndTurn(AValue : TNotifyEvent);
begin
  if FOnEndTurn = AValue then Exit;
  FOnEndTurn := AValue;
end;

procedure TGameEvents.SetOnInterlocking(AValue : TNotifyEvent);
begin
  if FOnInterlocking = AValue then Exit;
  FOnInterlocking := AValue;
end;

procedure TGameEvents.SetOnPlayerExit(AValue : TPlayerEvent);
begin
  if FOnPlayerExit = AValue then Exit;
  FOnPlayerExit := AValue;
end;

procedure TGameEvents.SetOnStartChoice(AValue : TNotifyEvent);
begin
  if FOnStartChoice = AValue then Exit;
  FOnStartChoice := AValue;
end;

procedure TGameEvents.SetOnStartCondition(AValue : TNotifyEvent);
begin
  if FOnStartCondition = AValue then Exit;
  FOnStartCondition := AValue;
end;

procedure TGameEvents.SetOnStartCycle(AValue : TNotifyEvent);
begin
  if FOnStartCycle = AValue then Exit;
  FOnStartCycle := AValue;
end;

procedure TGameEvents.SetOnStartExperiment(AValue : TNotifyEvent);
begin
  if FOnStartExperiment = AValue then Exit;
  FOnStartExperiment := AValue;
end;

procedure TGameEvents.SetOnStartGeneration(AValue : TNotifyEvent);
begin
  if FOnStartGeneration = AValue then Exit;
  FOnStartGeneration := AValue;
end;

procedure TGameEvents.SetOnStartTurn(AValue : TNotifyEvent);
begin
  if FOnStartTurn = AValue then Exit;
  FOnStartTurn := AValue;
end;

procedure TGameEvents.SetOnTargetInterlocking(AValue : TNotifyEvent);
begin
  if FOnTargetInterlocking = AValue then Exit;
  FOnTargetInterlocking := AValue;
end;

procedure TGameEvents.SetOnWaitForServer(AValue : TNotifyEvent);
begin
  if FOnWaitForServer = AValue then Exit;
  FOnWaitForServer := AValue;
end;

constructor TGameEvents.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  OnConsequence := nil;
  OnEndChoice := nil;
  OnEndCondition := nil;
  OnEndCycle := nil;
  OnEndExperiment := nil;
  OnEndGeneration := nil;
  OnEndTurn := nil;
  OnInterlocking := nil;
  OnPlayerExit := nil;
  OnStartChoice := nil;
  OnStartCondition := nil;
  OnStartCycle := nil;
  OnStartExperiment := nil;
  OnStartGeneration := nil;
  OnStartTurn := nil;
  OnTargetInterlocking := nil;
  OnWaitForServer := nil;
  //OnClose := nil;
end;

end.

