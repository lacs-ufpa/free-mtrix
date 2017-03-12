{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit zmq_network;

{$mode objfpc}{$H+}

// TODO: MsgPack optimization. Compress->send->receive->Decompress.

interface

uses Classes, SysUtils, Process
  , zmqapi
  ;

type

  { TMessRecvProc }

  TMessRecvProc = procedure(AResponse: TStringList) of object;

  { TZMQRequestsThread }

  TZMQRequestsThread = class(TThread)
  private
    FContext : TZMQContext;
    FOnReplyReceived: TMessRecvProc;
    FPusher_REQ,
    FRequester : TZMQSocket;
    FReply : TStringList;
    FRTLEvent: PRTLEvent;
    FMultipartMessage: array of UTF8String;
    {$IFDEF DEBUG}
    procedure RequestDebug;
    {$ENDIF}
    procedure ReplyReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(AZMQContext:TZMQContext; CreateSuspended: Boolean = True); overload;
    destructor Destroy; override;

    // Send a non blocking Request(identity, ' ', s1, .. sn)
    procedure Request(AMultipartMessage : array of UTF8String);
    property OnReplyReceived : TMessRecvProc read FOnReplyReceived write FOnReplyReceived;
  end;

  { TZMQMessagesThread }

  TZMQMessagesThread = class(TThread)
  private
    FContext : TZMQContext;
    FID: shortstring;
    FSubscriber,
    FPusher_PUB : TZMQSocket;
    FPoller : TZMQPoller;
    FMessage : TStringList;
    FOnMessageReceived: TMessRecvProc;
    procedure ThreadStarted;
    procedure MessageReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(AID : UTF8String; CreateSuspended: Boolean = True); overload;
    destructor Destroy; override;
    procedure Push(AMultipartMessage : array of UTF8String);
    property OnMessageReceived : TMessRecvProc read FOnMessageReceived write FOnMessageReceived;
    property ID :shortstring read FID;
    property Context : TZMQContext read FContext;
  end;


  {TReqRecvProc}

  TReqRecvProc = procedure(var ARequest : TStringList) of object;

  { TZMQServerThread }

  TZMQServerThread = class(TThread)
  private
    FID: shortstring;
    FOnMessageReceived: TMessRecvProc;
    FOnRequestReceived: TReqRecvProc;
    FContext : TZMQContext;
    FPublisher,
    FPuller_PUB,
    FPusher_PUB,
    FPuller_REP,
    //FRouter,
    FReplier : TZMQSocket;
    FPoller : TZMQPoller;
    FMessage : TStringList;
    procedure ThreadStart;
    procedure MessageReceived;
    procedure RequestReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(AID : UTF8String; CreateSuspended: Boolean = True); overload;
    destructor Destroy; override;
    procedure Push(AMultipartMessage: array of UTF8string);
    property OnMessageReceived : TMessRecvProc read FOnMessageReceived write FOnMessageReceived;
    property OnRequestReceived : TReqRecvProc read FOnRequestReceived write FOnRequestReceived;
    property ID :shortstring read FID;
  end;

resourcestring
  ERROR_RECV_TIMEOUT = 'O servidor foi solicitado, mas não respondeu. O programa será fechado.';

implementation

uses Forms, Dialogs;

var GClientHost : string;

const
  CHost = 'tcp://*:';
  CLocalHost = 'tcp://localhost:';
  CPortPublisher = '5056';
  CPortPuller_PUB = '5057';
  CPortPuller_REP = '6057';
  //CPortRouter = '5058';
  CPortReplier = '5059';



{ TZMQRequestsThread }


{$IFDEF DEBUG}
procedure TZMQRequestsThread.RequestDebug;
begin
  WriteLn('RTLEvent Ok');
end;
{$ENDIF}

procedure TZMQRequestsThread.ReplyReceived;
begin
  if Assigned(FOnReplyReceived) then
    FOnReplyReceived(FReply);
end;

procedure TZMQRequestsThread.Execute;
var
  LMultipartMessage : array of UTF8string;
  LReply : TStringList;
begin
  while not Terminated do
    begin
      RTLeventWaitFor(FRTLEvent);

      {$IFDEF DEBUG}
        Synchronize(@RequestDebug);
      {$ENDIF}

      LReply := TStringList.Create;
      try
        // send the real message to trigger the polling routine
        LMultipartMessage := FMultipartMessage;
        FPusher_REQ.send( LMultipartMessage );

        // send empty non blocking message
        // this fake message is necessary to avoid infinite loops inside the server pool
        FRequester.send('');

        // block thread until a real message is received
        FRequester.recv( LReply );
        FReply.Text := LReply.Text;

        // inform the main thread
        Synchronize(@ReplyReceived);
      finally
        LReply.Free;
      end;
  end;
end;

constructor TZMQRequestsThread.Create(AZMQContext: TZMQContext;
  CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;
  FRTLEvent := RTLEventCreate;
  FReply := TStringList.Create;
  FContext := AZMQContext;

  // pushes to server
  FPusher_REQ := FContext.Socket( stPush );
  FPusher_REQ.connect(GClientHost+CPortPuller_REP);

  // request from server
  FRequester := FContext.Socket( stReq );
  FRequester.connect(GClientHost+CPortReplier);
  inherited Create(CreateSuspended);
end;

destructor TZMQRequestsThread.Destroy;
begin
  RTLeventdestroy(FRTLEvent);
  FPusher_REQ.Free;
  FReply.Free;
  //FContext.Free;
  inherited Destroy;
end;

procedure TZMQRequestsThread.Request(AMultipartMessage: array of UTF8String);
var
  LLength,
  i : integer;
begin
  LLength := Length(AMultipartMessage);
  if LLength > 0 then
    begin
      SetLength(FMultipartMessage,LLength);
      for i := 0 to LLength-1 do
        FMultipartMessage[i] := AMultipartMessage[i];
    end;
  RTLeventSetEvent(FRTLEvent);
end;



{ TZMQMessagesThread }



procedure TZMQMessagesThread.ThreadStarted;
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'Started');
  {$ENDIF}
end;

procedure TZMQMessagesThread.MessageReceived;
begin
  if Assigned(FOnMessageReceived) then FOnMessageReceived(FMessage);
end;

procedure TZMQMessagesThread.Execute;
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
          LMessagesCount := FSubscriber.recv(LMultipartMessage);
          if LMessagesCount > 0 then
            begin
              FMessage := LMultipartMessage;
              Synchronize(@MessageReceived);
            end;
          {$IFDEF DEBUG}
          WriteLn('Client:Received:',LPollEvent,',',LMessagesCount);
          {$ENDIF}
        end;
    end;
  LMultipartMessage.Free;
end;


constructor TZMQMessagesThread.Create(AID: UTF8String; CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;
  FContext := TZMQContext.create;

  // client subscribe to server, it receives from itself
  FSubscriber := FContext.Socket( stSub );
  FSubscriber.connect(GClientHost+CPortPublisher);
  FSubscriber.Subscribe('');
  // pushes to server
  FPusher_PUB := FContext.Socket( stPush );
  FPusher_PUB.connect(GClientHost+CPortPuller_PUB);

  //FPusher_REQ := FContext.Socket( stPush );
  //FPusher_REQ.connect(GClientHost+CPortPuller_REP);

  // request from server
  //FRequester := FContext.Socket( stReq );
  //FRequester.Identity := AID;
  //FRequester.Linger:=0;
  //FRequester.RcvTimeout:=5000;
  //FRequester.connect(GClientHost+CPortReplier);

  //FRequester.connect(CLocalHost+CPortRouter);

  // handle income messages
  FPoller := TZMQPoller.Create(True, FContext);
  FPoller.Register(FSubscriber, [pePollIn], True);

  inherited Create(CreateSuspended);
end;

destructor TZMQMessagesThread.Destroy;
begin
  FPoller.Terminate;
  FPoller.Free;
  //FPusher_REQ.Free;
  FPusher_PUB.Free;
  FSubscriber.Free;
  FContext.Free;
  inherited Destroy;
end;

procedure TZMQMessagesThread.Push(AMultipartMessage: array of UTF8String);
begin
  FPusher_PUB.send(AMultipartMessage);
end;



{ TZMQServerThread }



procedure TZMQServerThread.ThreadStart;
begin
  {$IFDEF DEBUG}
  WriteLn(ClassType.ClassName+':'+'Started');
  {$ENDIF}
end;

procedure TZMQServerThread.MessageReceived;
begin
  if Assigned(FOnMessageReceived) then FOnMessageReceived(FMessage);
end;

procedure TZMQServerThread.RequestReceived;
begin
  if Assigned(FOnRequestReceived) then FOnRequestReceived(FMessage);
end;

procedure TZMQServerThread.Execute;
var
  LMultipartMessage, S : TStringList;
  LPollCount,
  LMessagesCount : integer;
begin
  Synchronize(@ThreadStart);
  LPollCount := 0;
  LMessagesCount := 0;
  LMultipartMessage := TStringList.Create;
  while not Terminated do
    begin
      LPollCount := FPoller.poll;
      if LPollCount = 0 then Continue;
      if pePollIn in FPoller.PollItem[0].revents then
        begin
          LMultipartMessage.Clear;
      	  LMessagesCount := FPuller_PUB.recv(LMultipartMessage);
          if LMessagesCount > 0 then
            begin
              FMessage := LMultipartMessage;
              Synchronize(@MessageReceived);
              FPublisher.send(LMultiPartMessage);
              {$IFDEF DEBUG}
                WriteLn('Server:Published:',LPollCount,',',LMessagesCount);
              {$ENDIF}
            end;
        end;

      if pePollIn in FPoller.PollItem[1].revents then
        begin
          LMultipartMessage.Clear;
          LMessagesCount := FPuller_REP.recv(LMultipartMessage);
          if LMessagesCount > 2 then
            begin
              FMessage := LMultipartMessage;
              Synchronize(@RequestReceived); LMultipartMessage := FMessage; S := TStringList.Create;
              FReplier.recv(S); S.Free;
              FReplier.send(LMultipartMessage);
              {$IFDEF DEBUG}
              WriteLn('Server:Replied:',LPollCount,',',LMessagesCount);
              {$ENDIF}
            end;
        end;
    end;
end;

constructor TZMQServerThread.Create(AID: UTF8String; CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;
  FContext := TZMQContext.create;

  // publisher for subscribers
  FPublisher := FContext.Socket( stPub ); // server don't need to subscribe to itself
  FPublisher.bind(CHost+CPortPublisher);

  // pull from inside and outside
  FPuller_PUB  := FContext.Socket( stPull );
  FPuller_PUB.bind(CHost+CPortPuller_PUB);

  // pushes from inside to outside
  FPusher_PUB := FContext.Socket( stPush );
  FPusher_PUB.connect(CLocalHost+CPortPuller_PUB);

  // reply requests from outside
  FPuller_REP  := FContext.Socket( stPull );
  FPuller_REP.bind(CHost+CPortPuller_REP);

  // blocking server thread for now
  FReplier := FContext.Socket( stRep );
  FReplier.bind(CHost+CPortReplier);

  // handle sockets
  FPoller := TZMQPoller.Create(True, FContext);
  FPoller.Register(FPuller_PUB,[pePollIn],True);
  FPoller.Register(FPuller_REP,[pePollIn],True);
  //FPoller.Register(FRouter, [pePollIn], True);

  inherited Create(CreateSuspended);
end;

destructor TZMQServerThread.Destroy;
begin
  FPoller.Terminate;
  FPoller.Free;
  //FRouter.Free;
  FPuller_REP.Free;
  FPusher_PUB.Free;
  FPuller_PUB.Free;
  FPublisher.Free;
  FContext.Free;
  inherited Destroy;
end;

procedure TZMQServerThread.Push(AMultipartMessage: array of UTF8string);
begin
  FPusher_PUB.send(AMultipartMessage);
end;

procedure LoadIP; //forward;
var
  S : TStringList;
  IPPath: String;
begin
  IPPath := ExtractFilePath(Application.ExeName)+'IP';
  if FileExists(IPPath) then
    begin
      S := TStringList.Create;
      try
        S.LoadFromFile(IPPath);
        GClientHost := 'tcp://'+S[0]+':';
      finally
        S.Free;
      end;
    end
  else GClientHost := CLocalHost;
end;

initialization
begin
  LoadIP;
end

end.
