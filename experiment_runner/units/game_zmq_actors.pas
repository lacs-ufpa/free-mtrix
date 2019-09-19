{   
  Free-mtrix - Free cultural selection and social behavior experiments.   
  Copyright (C) 2016-2019 Carlos Rafael Fernandes Picanço.   
  Copyright (C) 2016-2019 Thais Maria Monteiro Guimarães.   
  Copyright (C) 2016-2019 Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License   
  along with this program. If not, see <http://www.gnu.org/licenses/>.   
}
unit game_zmq_actors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , zmq_network
  ;

type

  { TZMQActor }

  TZMQActor = class(TComponent)
  private
    FOnMessageReceived : TMessRecvProc;
    FOnReplyReceived: TMessRecvProc;
    FOnRequestReceived: TReqRecvProc;
  protected
    FID: string;
    procedure MessageReceived(AMultipartMessage : TStringList);
    procedure ReplyReceived(AMultipartMessage : TStringList);
    procedure RequestReceived(var AMultipartMessage : TStringList);
  public
    constructor Create(AOwner : TComponent; AID : string); virtual; overload;
    procedure Start; virtual;
    procedure SendMessage(AMessage : array of string);virtual;
    procedure Request(ARequest : array of string);virtual;
    property OnMessageReceived : TMessRecvProc read FOnMessageReceived write FOnMessageReceived;
    property OnRequestReceived : TReqRecvProc read FOnRequestReceived write FOnRequestReceived;
    property OnReplyReceived : TMessRecvProc read FOnReplyReceived write FOnReplyReceived;
    property ID : string read FID;
  end;

  { TZMQPlayer }

  TZMQPlayer = class(TZMQActor)
  private
    FZMQMessages : TZMQMessagesThread;
    FZMQRequests  : TZMQRequestsThread;
  public
    constructor Create(AOwner : TComponent; AID : string); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure SendMessage(AMessage : array of string); override;
    procedure Request(ARequest : array of string);override;
  end;

  { TZMQAdmin }

  TZMQAdmin = class(TZMQActor)
  private
    FZMQServer : TZMQServerThread;
  public
    constructor Create(AOwner : TComponent; AID : string); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure SendMessage(AMessage: array of string); override;
    procedure Request(ARequest: array of string); override;
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

constructor TZMQAdmin.Create(AOwner: TComponent; AID: string);
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

procedure TZMQAdmin.SendMessage(AMessage: array of string);
begin
  FZMQServer.Push(AMessage);
end;

procedure TZMQAdmin.Request(ARequest: array of string);
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

procedure TZMQPlayer.SendMessage(AMessage: array of string);
begin
  {$IFDEF DEBUG}
  inherited SendMessage(AMessage);
  {$ENDIF}
  FZMQMessages.Push(AMessage);
end;

procedure TZMQPlayer.Request(ARequest: array of string);
begin
  {$IFDEF DEBUG}
  inherited Request(ARequest);
  {$ENDIF}
  FZMQRequests.Request(ARequest);
end;

constructor TZMQPlayer.Create(AOwner: TComponent; AID: string);
begin
  inherited Create(AOwner);
  FID:=AID;
  FZMQMessages := TZMQMessagesThread.Create(AID);
  FZMQMessages.OnMessageReceived:=@MessageReceived;

  FZMQRequests := TZMQRequestsThread.Create;
  FZMQRequests.OnReplyReceived:=@ReplyReceived;
end;

destructor TZMQPlayer.Destroy;
begin
  FZMQMessages.Terminate;
  FZMQRequests.Terminate;
  inherited Destroy;
end;

procedure TZMQPlayer.Start;
begin
  inherited Start;
  FZMQMessages.Start;
  FZMQRequests.Start;
end;

{ TZMQActor }

procedure TZMQActor.MessageReceived(AMultipartMessage: TStringList);
{$IFDEF DEBUG}
var i : integer;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'ReceivedAMessage');
  for i:= 0 to AMultipartMessage.Count-1 do
    WriteLn(i,':',AMultipartMessage[i]);
  {$ENDIF}
  if Assigned(FOnMessageReceived) then FOnMessageReceived(AMultipartMessage);
end;

procedure TZMQActor.ReplyReceived(AMultipartMessage: TStringList);
{$IFDEF DEBUG}
var i : integer;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'ReceivedAReply');
  for i:= 0 to AMultipartMessage.Count-1 do
    WriteLn(i,':',AMultipartMessage[i]);
  {$ENDIF}
  if Assigned(FOnReplyReceived) then FOnReplyReceived(AMultipartMessage);
end;

procedure TZMQActor.RequestReceived(var AMultipartMessage: TStringList);
{$IFDEF DEBUG}
var i : integer;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'ReceivedARequest');
  for i:= 0 to AMultipartMessage.Count-1 do
    WriteLn(i,':',AMultipartMessage[i]);
  {$ENDIF}
  if Assigned(FOnRequestReceived) then FOnRequestReceived(AMultipartMessage);
end;

constructor TZMQActor.Create(AOwner: TComponent; AID: string);
begin
  inherited Create(AOwner);
end;

procedure TZMQActor.Start;
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'Starting');
  {$ENDIF}
end;

procedure TZMQActor.SendMessage(AMessage: array of string);
{$IFDEF DEBUG}
var i : integer;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'SendingMessage:'+AMessage[1]);
  for i:= 0 to Length(AMessage)-1 do
    WriteLn(i,':',AMessage[i]);
  {$ENDIF}
end;

procedure TZMQActor.Request(ARequest: array of string);
{$IFDEF DEBUG}
var i : integer;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'SendingRequest:'+ARequest[2]);
  for i:= 0 to Length(ARequest)-1 do
    WriteLn(i,':',ARequest[i]);
  {$ENDIF}
end;

end.

