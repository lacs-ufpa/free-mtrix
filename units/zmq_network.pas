{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit zmq_network;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Process

  , zmqapi
  //, zmq_client
  ;

type

  { TZMQPusher }

  TZMQPusher = class
  private
    FContext : TZMQContext;
    FID: UTF8string;
    FPusher : TZMQSocket;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendMessage(AMultipartMessage : array of UTF8string);
    property ID : UTF8string read FID;
  end;

  { TZMQPubThread }

  TZMQPubThread = class(TThread)
  private
    FContext : TZMQContext;
    FPublisher : TZMQSocket;
    FPuller : TZMQSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean = True);
    destructor Destroy; override;
  end;

  { TMessRecvProc }

  TMessRecvProc = procedure(AResponse: TStringList) of object;

  { TZMQPollThread }

  TZMQPollThread = class(TThread)
  private
    FMultipartMessage : TStringList;
    FContext : TZMQContext;
    FSubscriber : TZMQSocket;
    FPoller : TZMQPoller;
    FOnMessageReceived: TMessRecvProc;
    procedure MessageReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean = True);
    destructor Destroy; override;
    property OnMessageReceived : TMessRecvProc read FOnMessageReceived write FOnMessageReceived;
  end;


implementation

uses zhelpers;

{ TZMQSubscriber }

procedure TZMQPollThread.MessageReceived;
begin
  if Assigned(OnMessageReceived) then OnMessageReceived(FMultipartMessage);
end;

procedure TZMQPollThread.Execute;
var
  LMultipartMessage : TStringList;
  LPollEvent,
  LMessagesCount : integer;
begin
{$IFDEF DEBUG}
  WriteLn('SubThread.Execute');
{$ENDIF}
  while not Terminated do
    begin
      LPollEvent := FPoller.poll(50000);
      if LPollEvent > 0 then
        begin
        {$IFDEF DEBUG}
          WriteLn('SubThread.Execute.PollMessageReceived');
        {$ENDIF}
          LMultipartMessage := TStringList.Create;
          LMessagesCount := FSubscriber.recv(LMultipartMessage);
          if LMessagesCount > 0 then
            try
              FMultipartMessage := LMultipartMessage;
              Synchronize(@MessageReceived);
            finally
              LMultipartMessage.Free;
            end;
        end;
    end;
end;

constructor TZMQPollThread.Create(CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;
  FContext := TZMQContext.create;
  FSubscriber := FContext.Socket( stSub );
  FSubscriber.connect('tcp://localhost:5056');
  FSubscriber.Subscribe('');

  FPoller := TZMQPoller.Create(True, FContext);
  // FPoller.onEvent := @PollerEvent; // async
  FPoller.Register(FSubscriber, [pePollIn], True);
  inherited Create(CreateSuspended);
end;

destructor TZMQPollThread.Destroy;
begin
  FContext.Free;
  FPoller.Terminate;
  FPoller.Free;
  FSubscriber.Free;
  inherited Destroy;
end;

{ TZmqPusher }

constructor TZMQPusher.Create;
begin
  FID := s_random(10);
  FContext := TZMQContext.create;
  FPusher := FContext.Socket( stPush );
  FPusher.connect('tcp://localhost:5057');
end;

destructor TZMQPusher.Destroy;
begin
  FPusher.Free; // also can be freed by freeing the context
  FContext.Free;
  inherited Destroy;
end;

procedure TZMQPusher.SendMessage(AMultipartMessage: array of UTF8string);
begin
  FPusher.send(AMultipartMessage);
end;



{ TZMQPubThread }

procedure TZMQPubThread.Execute;
var
  LMultipartMessage : TStringList;
  LMessagesCount : integer;
begin
{$IFDEF DEBUG}
  WriteLn('PubThread.Execute');
{$ENDIF}
  while not Terminated do
    begin
      LMultipartMessage := TStringList.Create;
	    LMessagesCount := FPuller.recv(LMultipartMessage);
      if LMessagesCount > 0 then
          begin
          {$IFDEF DEBUG}
            WriteLn('PubThread.Execute.MessageReceived');
          {$ENDIF}
            FPublisher.send(LMultiPartMessage);
          end;
      LMultipartMessage.Free;
    end;
end;

constructor TZMQPubThread.Create(CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;

  FContext := TZMQContext.Create;
  FPublisher := FContext.Socket( stPub );
  FPublisher.bind('tcp://*:5056');

  FPuller  := FContext.Socket( stPull );
  FPuller.bind('tcp://*:5057');
  inherited Create(CreateSuspended);
end;

destructor TZMQPubThread.Destroy;
begin
  FContext.Free;
  FPuller.Free;
  FPublisher.Free;
  inherited Destroy;
end;

end.
