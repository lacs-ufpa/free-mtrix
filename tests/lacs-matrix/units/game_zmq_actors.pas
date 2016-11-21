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
    FOnMessageReceived : TMessRecvProc;
    FOnReplyReceived: TMessRecvProc;
    FOnRequestReceived: TReqRecvProc;
  protected
    FID: UTF8string;
    procedure MessageReceived(AMultipartMessage : TStringList);
    procedure ReplyReceived(AMultipartMessage : TStringList);
    procedure RequestReceived(var AMultipartMessage : TStringList);
  public
    constructor Create(AOwner : TComponent; AID : UTF8String); virtual; overload;
    procedure Start; virtual;
    procedure SendMessage(AMessage : array of UTF8string);virtual;abstract;
    procedure Request(ARequest : array of UTF8string);virtual;abstract;
    property OnMessageReceived : TMessRecvProc read FOnMessageReceived write FOnMessageReceived;
    property OnRequestReceived : TReqRecvProc read FOnRequestReceived write FOnRequestReceived;
    property OnReplyReceived : TMessRecvProc read FOnReplyReceived write FOnReplyReceived;
    property ID : UTF8string read FID;
  end;

  { TZMQPlayer }

  TZMQPlayer = class(TZMQActor)
  private
    FZMQClient : TZMQClientThread;
  public
    constructor Create(AOwner : TComponent; AID : UTF8String); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure SendMessage(AMessage : array of UTF8string); override;
    procedure Request(ARequest : array of UTF8string);override;
  end;

  { TZMQAdmin }

  TZMQAdmin = class(TZMQActor)
  private
    FZMQServer : TZMQServerThread;
  public
    constructor Create(AOwner : TComponent; AID : UTF8String); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure SendMessage(AMessage: array of UTF8string); override;
    procedure Request(ARequest: array of UTF8string); override;
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
  inherited Start;
  WriteLn('TZMQWatcher.Start');
end;

{ TZMQAdmin }

constructor TZMQAdmin.Create(AOwner: TComponent; AID: UTF8String);
begin
  inherited Create(AOwner);
  FID:=AID;
  FZMQServer := TZMQServerThread.Create(AID);
  FZMQServer.OnMessageReceived:=@MessageReceived;
  FZMQServer.OnRequestReceived:=@RequestReceived;

end;

destructor TZMQAdmin.Destroy;
begin
  FZMQServer.Terminate;
  inherited Destroy;
end;

procedure TZMQAdmin.SendMessage(AMessage: array of UTF8string);
begin
  FZMQServer.Push(AMessage);
end;

procedure TZMQAdmin.Request(ARequest: array of UTF8string);
begin
  // do nothing, you are the server
end;

procedure TZMQAdmin.Start;
begin
  inherited Start;
  FZMQServer.Start;
  WriteLn('TZMQAdmin.Start');
end;

{ TZMQPlayer }

procedure TZMQPlayer.SendMessage(AMessage: array of UTF8string);
begin
  FZMQClient.Push(AMessage);
end;

procedure TZMQPlayer.Request(ARequest: array of UTF8string);
begin
  FZMQClient.Request(ARequest);
end;

constructor TZMQPlayer.Create(AOwner: TComponent; AID: UTF8String);
begin
  inherited Create(AOwner);
  FID:=AID;
  FZMQClient := TZMQClientThread.Create(AID);
  FZMQClient.OnMessageReceived:=@MessageReceived;
  FZMQClient.OnReplyReceived:=@ReplyReceived;
end;

destructor TZMQPlayer.Destroy;
begin
  FZMQClient.Terminate;
  inherited Destroy;
end;

procedure TZMQPlayer.Start;
begin
  inherited Start;
  FZMQClient.Start;
  WriteLn('TZMQPlayer.Start');
end;

{ TZMQActor }

procedure TZMQActor.MessageReceived(AMultipartMessage: TStringList);
begin
  if Assigned(FOnMessageReceived) then FOnMessageReceived(AMultipartMessage);
end;

procedure TZMQActor.ReplyReceived(AMultipartMessage: TStringList);
begin
  if Assigned(FOnReplyReceived) then FOnReplyReceived(AMultipartMessage);
end;

procedure TZMQActor.RequestReceived(var AMultipartMessage: TStringList);
begin
  if Assigned(FOnRequestReceived) then FOnRequestReceived(AMultipartMessage);
end;

constructor TZMQActor.Create(AOwner: TComponent; AID: UTF8String);
begin
  inherited Create(AOwner);
end;

procedure TZMQActor.Start;
begin
  WriteLn('TZMQActor.Start');
end;

end.

