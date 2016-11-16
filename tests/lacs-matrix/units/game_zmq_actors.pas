unit game_zmq_actors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , zmq_network
  //, zmq_client
  ;

type

  { TZMQActor }

  TZMQActor = class(TComponent)
  private
    FSubscriber: TZMQPollThread;
    FOnMessageReceived : TMessRecvProc;
    function GetActorID: UTF8string; virtual;
  protected
    procedure MessageReceived(AMultipartMessage : TStringList);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Start; virtual;
    property OnMessageReceived : TMessRecvProc read FOnMessageReceived write FOnMessageReceived;
    property ID : UTF8string read GetActorID;
  end;

  { TZMQPlayer }

  TZMQPlayer = class(TZMQActor)
  private
    FPusher : TZMQPusher;
    function GetActorID: UTF8string; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure SendMessage(AMessage : array of UTF8string);
    property ID : UTF8string read GetActorID;
  end;

  { TZMQAdmin }

  TZMQAdmin = class(TZMQPlayer)
  private
    FPublisher : TZMQPubThread;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
  end;

  { TZMQWatcher }

  TZMQWatcher = class(TZMQActor)
  public
    procedure Start; override;
  end;

implementation

{ TZMQWatcher }

procedure TZMQWatcher.Start;
begin
  AbstractError;
  inherited Start;
  WriteLn('TZMQWatcher.Start');
end;

{ TZMQAdmin }

constructor TZMQAdmin.Create(AOwner: TComponent);
begin
  FPublisher := TZMQPubThread.Create;
  inherited Create(AOwner);
end;

destructor TZMQAdmin.Destroy;
begin
  FPublisher.Terminate;
  inherited Destroy;
end;

procedure TZMQAdmin.Start;
begin
  FPublisher.Start;
  inherited Start;
  WriteLn('TZMQAdmin.Start');
end;

{ TZMQPlayer }

procedure TZMQPlayer.SendMessage(AMessage: array of UTF8string);
begin
  FPusher.SendMessage(AMessage);
end;

function TZMQPlayer.GetActorID: UTF8string;
begin
  Result := FPusher.ID;
end;

constructor TZMQPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPusher := TZMQPusher.Create;
end;

destructor TZMQPlayer.Destroy;
begin
  FPusher.Free;
  inherited Destroy;
end;

procedure TZMQPlayer.Start;
begin
  inherited Start;
  WriteLn('TZMQPlayer.Start');
end;

{ TZMQActor }

function TZMQActor.GetActorID: UTF8string;
begin
  AbstractError;
  Result := '';
end;

procedure TZMQActor.MessageReceived(AMultipartMessage: TStringList);
begin
  if Assigned(FOnMessageReceived) then FOnMessageReceived(AMultipartMessage);
end;

constructor TZMQActor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSubscriber := TZMQPollThread.Create;
  FSubscriber.OnMessageReceived:=@MessageReceived;
end;

destructor TZMQActor.Destroy;
begin
  OnMessageReceived := nil;
  FSubscriber.Terminate;
  inherited Destroy;
end;

procedure TZMQActor.Start;
begin
  FSubscriber.Start;
  WriteLn('TZMQActor.Start');
end;

end.

