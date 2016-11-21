{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit zmq_network;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}

interface

uses Classes, SysUtils, Process
  , zmqapi
  ;

type
  { TMessRecvProc }

  TMessRecvProc = procedure(AResponse: TStringList) of object;

  TReqRecvProc = procedure(var ARequest : TStringList) of object;

  { TZMQClientThread }

  TZMQClientThread = class(TThread)
  private
    FContext : TZMQContext;
    FSubscriber,
    FPusher,
    FRequester : TZMQSocket;
    FPoller : TZMQPoller;
    FMessage : TStringList;
    FOnReplyReceived: TMessRecvProc;
    FOnMessageReceived: TMessRecvProc;
    procedure MessageReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean = True);
    destructor Destroy; override;
    procedure Request(AMultipartMessage : array of UTF8String);
    procedure Push(AMultipartMessage : array of UTF8String);
    property OnMessageReceived : TMessRecvProc read FOnMessageReceived write FOnMessageReceived;
    property OnReplyReceived : TMessRecvProc read FOnReplyReceived write FOnReplyReceived;
  end;

  { TZMQServerThread }

  TZMQServerThread = class(TThread)
  private
    FOnMessageReceived: TMessRecvProc;
    FOnRequestReceived: TReqRecvProc;
    FContext : TZMQContext;
    FPublisher,
    FSubscriber,
    FPuller,
    FPusher,
    FRouter,
    FReplier : TZMQSocket;
    FPoller : TZMQPoller;
    FMessage : TStringList;
    procedure Connect;
    procedure MessageReceived;
    procedure RequestReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean = True);
    destructor Destroy; override;
    procedure Push(AMultipartMessage: array of UTF8string);
    property OnMessageReceived : TMessRecvProc read FOnMessageReceived write FOnMessageReceived;
    property OnRequestReceived : TReqRecvProc read FOnRequestReceived write FOnRequestReceived;
  end;

implementation

const
  CHost = 'tcp://*:';
  CLocalHost = 'tcp://localhost:';
  CPortPublisher = '5056';
  CPortPuller = '5057';
  CPortRouter = '5058';


{ TZMQClientThread }


procedure TZMQClientThread.MessageReceived;
begin
  if Assigned(FOnMessageReceived) then FOnMessageReceived(FMessage);
end;

procedure TZMQClientThread.Execute;
var
  LMultipartMessage : TStringList;
  LPollEvent,
  LMessagesCount : integer;
begin
  LMultipartMessage := TStringList.Create;
  while not Terminated do
    begin
      LMultipartMessage.Clear;
      LPollEvent := FPoller.poll(50000);
      if LPollEvent > 0 then
        begin
          WriteLn('Server4:FPoller:',FPoller.PollNumber);
          LMessagesCount := FSubscriber.recv(LMultipartMessage);
          if LMessagesCount > 0 then
            begin
              FMessage := LMultipartMessage;
              Synchronize(@MessageReceived);
            end;
        end;
    end;
  LMultipartMessage.Free;
end;


constructor TZMQClientThread.Create(CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;
  FContext := TZMQContext.create;

  // client subscribe to server, it receives from itself
  FSubscriber := FContext.Socket( stSub );
  FSubscriber.connect(CLocalHost+CPortPublisher);FSubscriber.Subscribe('');
  // pushes to server
  FPusher := FContext.Socket( stPush );
  FPusher.connect(CLocalHost+CPortPuller);

  // request from server
  FRequester := FContext.Socket( stReq );
  FRequester.connect(CLocalHost+CPortRouter);

  // handle income messages
  FPoller := TZMQPoller.Create(True, FContext);
  FPoller.Register(FSubscriber, [pePollIn], True);

  inherited Create(CreateSuspended);
end;

destructor TZMQClientThread.Destroy;
begin
  FPoller.Terminate;
  FPoller.Free;
  FPusher.Free;
  FSubscriber.Free;
  FContext.Free;
  inherited Destroy;
end;

procedure TZMQClientThread.Request(AMultipartMessage: array of UTF8String);
var AReply : TStringList;
begin
  AReply:=TStringList.Create;
  FRequester.send( AMultipartMessage );
  FRequester.recv( AReply );
  if Assigned(FOnReplyReceived) then FOnReplyReceived(AReply);
  AReply.Free;
end;

procedure TZMQClientThread.Push(AMultipartMessage: array of UTF8String);
begin
  FPusher.send(AMultipartMessage);
end;



{ TZMQServerThread }



procedure TZMQServerThread.Connect;
begin
  {$IFDEF DEBUG}
    WriteLn('TZMQServerThread.Started');
  {$ENDIF}
end;

procedure TZMQServerThread.MessageReceived;
begin
  if Assigned(FOnMessageReceived) then FOnMessageReceived(FMessage);
end;

procedure TZMQServerThread.RequestReceived;
begin
  if Assigned(FOnMessageReceived) then FOnMessageReceived(FMessage);
end;

procedure TZMQServerThread.Execute;
var
  LMultipartMessage : TStringList;
  LPollCount,
  LMessagesCount : integer;
begin
  Synchronize(@Connect);
  LPollCount := 0;
  LMessagesCount := 0;
  LMultipartMessage := TStringList.Create;
  while not Terminated do
    begin
      LMultipartMessage.Clear;
      LPollCount := FPoller.poll(50000);
      if LPollCount > 0 then
        begin
          case FPoller.PollNumber of
            2 : begin// puller
                  {$IFDEF DEBUG}
                    WriteLn('Server2:');
                  {$ENDIF}
      	          LMessagesCount := FPuller.recv(LMultipartMessage);
                  if LMessagesCount > 0 then
                    begin
                      FMessage := LMultipartMessage;
                      Synchronize(@MessageReceived);
                      FPublisher.send(LMultiPartMessage);
                    end;
                end;

            1 : begin//router
                  {$IFDEF DEBUG}
                    WriteLn('Server1:');
                  {$ENDIF}
                  // Exit;
                  if LMessagesCount > 2 then
                    begin
                      FRouter.recv(LMultipartMessage);
                      FMessage := LMultipartMessage;
                      Synchronize(@RequestReceived);
                      LMultipartMessage := FMessage;
                      FRouter.send(LMultipartMessage);
                    end;
                end;
          end;

        end;
    end;
end;

constructor TZMQServerThread.Create(CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;
  FContext := TZMQContext.create;

  // publishes for subscribers, server subscribe to itself
  FPublisher := FContext.Socket( stPub );
  FSubscriber := FContext.Socket( stSub );
  FSubscriber.connect(CLocalHost+CPortPublisher);FSubscriber.Subscribe('');

  // pushes from inside to outside
  FPuller  := FContext.Socket( stPull );
  FPusher := FContext.Socket( stPush );
  FPusher.connect(CLocalHost+CPortPuller);

  // reply requests from outside
  FRouter := FContext.Socket( stRouter );

  // local setup
  FPublisher.bind(CHost+CPortPublisher);
  FPuller.bind(CHost+CPortPuller);
  FRouter.bind(CHost+CPortRouter);

  // handle sockets
  FPoller := TZMQPoller.Create(True, FContext);
  FPoller.Register(FPuller,[pePollIn],True);
  FPoller.Register(FRouter, [pePollIn], True);

  inherited Create(CreateSuspended);
end;

destructor TZMQServerThread.Destroy;
begin
  FPoller.Terminate;
  FPoller.Free;
  FRouter.Free;
  FPusher.Free;
  FPuller.Free;
  FSubscriber.Free;
  FPublisher.Free;
  FContext.Free;
  inherited Destroy;
end;

procedure TZMQServerThread.Push(AMultipartMessage: array of UTF8string);
begin
  FPusher.send(AMultipartMessage);
end;


end.
