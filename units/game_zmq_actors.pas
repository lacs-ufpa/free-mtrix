{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_zmq_actors;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}

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
    procedure SendMessage(AMessage : array of UTF8string);virtual;
    procedure Request(ARequest : array of UTF8string);virtual;
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
  {$IFDEF DEBUG}
  WriteLn('TZMQWatcher.Start');
  {$ENDIF}

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
  {$IFDEF DEBUG}
  WriteLn('WARNING:'+ClassType.ClassName+':'+'CannotSendRequests:'+ARequest[2]);
  {$ENDIF}
end;

procedure TZMQAdmin.Start;
begin
  inherited Start;
  FZMQServer.Start;
end;

{ TZMQPlayer }

procedure TZMQPlayer.SendMessage(AMessage: array of UTF8string);
begin
  {$IFDEF DEBUG}
  inherited SendMessage(AMessage);
  {$ENDIF}
  FZMQClient.Push(AMessage);
end;

procedure TZMQPlayer.Request(ARequest: array of UTF8string);
begin
  {$IFDEF DEBUG}
  inherited Request(ARequest);
  {$ENDIF}
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
end;

{ TZMQActor }

procedure TZMQActor.MessageReceived(AMultipartMessage: TStringList);
var i : integer;
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'ReceivedAMessage');
  for i:= 0 to AMultipartMessage.Count-1 do
    WriteLn(i,':',AMultipartMessage[i]);
  {$ENDIF}
  if Assigned(FOnMessageReceived) then FOnMessageReceived(AMultipartMessage);
end;

procedure TZMQActor.ReplyReceived(AMultipartMessage: TStringList);
var i : integer;
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'ReceivedAReply');
  for i:= 0 to AMultipartMessage.Count-1 do
    WriteLn(i,':',AMultipartMessage[i]);
  {$ENDIF}
  if Assigned(FOnReplyReceived) then FOnReplyReceived(AMultipartMessage);
end;

procedure TZMQActor.RequestReceived(var AMultipartMessage: TStringList);
var i : integer;
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'ReceivedARequest');
  for i:= 0 to AMultipartMessage.Count-1 do
    WriteLn(i,':',AMultipartMessage[i]);
  {$ENDIF}
  if Assigned(FOnRequestReceived) then FOnRequestReceived(AMultipartMessage);
end;

constructor TZMQActor.Create(AOwner: TComponent; AID: UTF8String);
begin
  inherited Create(AOwner);
end;

procedure TZMQActor.Start;
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'Starting');
  {$ENDIF}
end;

procedure TZMQActor.SendMessage(AMessage: array of UTF8string);
var i : integer;
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'SendingMessage:'+AMessage[1]);
  for i:= 0 to Length(AMessage)-1 do
    WriteLn(i,':',AMessage[i]);
  {$ENDIF}
end;

procedure TZMQActor.Request(ARequest: array of UTF8string);
var i : integer;
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'SendingRequest:'+ARequest[2]);
  for i:= 0 to Length(ARequest)-1 do
    WriteLn(i,':',ARequest[i]);
  {$ENDIF}
end;

end.

