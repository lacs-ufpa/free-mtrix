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

uses Classes, SysUtils;

type

  { TMessRecvProc }

  TMessRecvProc = procedure(AResponse: TStringList) of object;

  { TZMQRequestsThread }

  TZMQRequestsThread = class(TThread)
  private
    FContext : Pointer;
    FOnReplyReceived: TMessRecvProc;
    FRequester : Pointer;
    FReply : TStringList;
    FRTLEvent: PRTLEvent;
    FMultipartMessage: array of string;
    procedure ReplyReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean = True); overload;
    destructor Destroy; override;

    // Send a non blocking Request(identity, ' ', s1, .. sn)
    procedure Request(AMultipartMessage : array of string);
    property OnReplyReceived : TMessRecvProc read FOnReplyReceived write FOnReplyReceived;
  end;

  { TZMQMessagesThread }

  TZMQMessagesThread = class(TThread)
  private
    FContext : Pointer;
    FID: shortstring;
    FSubscriber,
    FPusher_PUB : Pointer;
    FMessage : TStringList;
    FOnMessageReceived: TMessRecvProc;
    procedure ThreadStarted;
    procedure MessageReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(AID : string; CreateSuspended: Boolean = True); overload;
    destructor Destroy; override;
    procedure Push(AMultipartMessage : array of string);
    property OnMessageReceived : TMessRecvProc read FOnMessageReceived write FOnMessageReceived;
    property ID :shortstring read FID;
    property Context : Pointer read FContext;
  end;


  {TReqRecvProc}

  TReqRecvProc = procedure(var ARequest : TStringList) of object;

  { TZMQServerThread }

  TZMQServerThread = class(TThread)
  private
    FID: shortstring;
    FOnMessageReceived: TMessRecvProc;
    FOnRequestReceived: TReqRecvProc;
    FContext,
    FPublisher,
    FPuller_PUB,
    FPusher_PUB,
    FReplier : Pointer;
    FMessage : TStringList;
    procedure ThreadStart;
    procedure MessageReceived;
    procedure RequestReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(AID : string; CreateSuspended: Boolean = True); overload;
    destructor Destroy; override;
    procedure Push(AMultipartMessage: array of string);
    property OnMessageReceived : TMessRecvProc read FOnMessageReceived write FOnMessageReceived;
    property OnRequestReceived : TReqRecvProc read FOnRequestReceived write FOnRequestReceived;
    property ID :shortstring read FID;
  end;

resourcestring
  ERROR_RECV_TIMEOUT = 'Server time-out. The program is being terminated.';

implementation

uses Forms, Dialogs, zmq, zmq.helpers;

var GClientHost : string;

const
  CHost = 'tcp://*:';
  CLocalHost = 'tcp://localhost:';
  CPortPublisher = '5056';
  CPortPuller_PUB = '5057';
  //CPortRouter = '5058';
  CPortReplier = '5059';


{ TZMQRequestsThread }

procedure TZMQRequestsThread.ReplyReceived;
begin
  if Assigned(OnReplyReceived) then OnReplyReceived(FReply);
end;

procedure TZMQRequestsThread.Execute;
var
  LMultipartMessage : array of string;
begin
  FReply := TStringList.Create;
  try
    while not Terminated do
    begin
      RTLeventWaitFor(FRTLEvent);
      FReply.Clear;

      // send multipart message
      LMultipartMessage := FMultipartMessage;
      SendMultiPartString(FRequester, LMultipartMessage);

      // receive multipart message
      RecvMultiPartString(FRequester, FReply);
      Synchronize(@ReplyReceived);
    end;
  finally
    FReply.Free;
  end;
end;

constructor TZMQRequestsThread.Create(CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;
  FRTLEvent := RTLEventCreate;
  FContext := zmq_ctx_new;

  // request from server
  FRequester := zmq_socket(FContext, ZMQ_REQ);
  zmq_connect(FRequester, PChar(GClientHost+CPortReplier));
  inherited Create(CreateSuspended);
end;

destructor TZMQRequestsThread.Destroy;
begin
  RTLeventdestroy(FRTLEvent);
  zmq_close(FRequester);
  zmq_ctx_destroy(FContext);
  inherited Destroy;
end;

procedure TZMQRequestsThread.Request(AMultipartMessage: array of string);
var
  LLength,
  i : integer;
begin
  LLength := Length(AMultipartMessage);
  if LLength > 0 then
    begin
      SetLength(FMultipartMessage, LLength);
      for i := 0 to LLength-1 do
        FMultipartMessage[i] := AMultipartMessage[i];
      RTLeventSetEvent(FRTLEvent);
    end;
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
  if Assigned(OnMessageReceived) then OnMessageReceived(FMessage);
end;

procedure TZMQMessagesThread.Execute;
var
  rc: integer;
  LMessage : TStringList;
  item : zmq_pollitem_t;
begin
  with item do
  begin
    socket := FSubscriber;
    fd := 0;
    events := ZMQ_POLLIN;
    revents := 0;
  end;

  LMessage := TStringList.Create;
  try
    while not Terminated do
    begin
      rc := zmq_poll(item, 1, -1);
      if rc = 0 then continue;
      if (item.revents and ZMQ_POLLIN) > 0 then
        begin
          LMessage.Clear;
          RecvMultiPartString(FSubscriber, LMessage);
          FMessage := LMessage;
          Synchronize(@MessageReceived);
        end;
    end;
  finally
    LMessage.Free;
  end;
end;


constructor TZMQMessagesThread.Create(AID: string; CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;
  FContext := zmq_ctx_new;

  // client subscribe to server, it receives from itself
  FSubscriber := zmq_socket(FContext, ZMQ_SUB);
  zmq_connect(FSubscriber, PChar(GClientHost+CPortPublisher));

  // subscribe to all messages
  zmq_setsockopt(FSubscriber, ZMQ_SUBSCRIBE, nil, 0);

  // pushes to server
  FPusher_PUB := zmq_socket(FContext, ZMQ_PUSH);
  zmq_connect(FPusher_PUB, PChar(GClientHost+CPortPuller_PUB));
  inherited Create(CreateSuspended);
end;

destructor TZMQMessagesThread.Destroy;
begin
  zmq_close(FPusher_PUB);
  zmq_close(FSubscriber);
  zmq_ctx_destroy(FContext);
  inherited Destroy;
end;

procedure TZMQMessagesThread.Push(AMultipartMessage: array of string);
begin
  SendMultiPartString(FPusher_PUB, AMultipartMessage);
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
  LMultipartMessage : TStringList;
  rc : integer = 0;

  items : array [0..1] of zmq_pollitem_t;
begin
  with items[0] do
  begin
    socket := FPuller_PUB;
    fd := 0;
    events := ZMQ_POLLIN;
    revents := 0;
  end;
  with items[1] do
  begin
    socket := FReplier;
    fd := 0;
    events := ZMQ_POLLIN;
    revents := 0;
  end;

  LMultipartMessage := TStringList.Create;
  try
    rc := 0;
    while not Terminated do
    begin
      rc := zmq_poll(items[0], 2, -1);
      if rc = 0 then continue;
      if (items[0].revents and ZMQ_POLLIN) > 0 then
      begin
        LMultipartMessage.Clear;
        RecvMultiPartString(FPuller_PUB, LMultipartMessage);
        if LMultipartMessage.Count > 0 then
          begin
            //WriteLn('--------------------- pull -----------------------');
            //WriteLn(LMultipartMessage.Count);
            //WriteLn(LMultipartMessage.Text);
            SendMultiPartString(FPublisher, LMultiPartMessage);
            FMessage := LMultipartMessage;
            Synchronize(@MessageReceived);
          end;
      end;

      if (items[1].revents and ZMQ_POLLIN) > 0 then
      begin
        LMultipartMessage.Clear;
        RecvMultiPartString(FReplier, LMultipartMessage);
        if LMultipartMessage.Count > 2 then
          begin
            FMessage := LMultipartMessage;
            //WriteLn('--------------------- req -----------------------');
            //WriteLn(LMultipartMessage.Count);
            //WriteLn(LMultipartMessage.Text);
            Synchronize(@RequestReceived);
            LMultipartMessage := FMessage;
            SendMultiPartString(FReplier, LMultipartMessage);
          end;
      end;
    end;
  finally
    LMultipartMessage.Free;
  end;
end;

constructor TZMQServerThread.Create(AID: string; CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;
  FContext := zmq_ctx_new;

  // publisher for subscribers
  FPublisher := zmq_socket(FContext, ZMQ_PUB); // server don't need to subscribe to itself
  zmq_bind(FPublisher, PChar(CHost+CPortPublisher));

  // pull from inside and outside
  FPuller_PUB  := zmq_socket(FContext, ZMQ_PULL);
  zmq_bind(FPuller_PUB, PChar(CHost+CPortPuller_PUB));

  // pushes from inside to outside
  FPusher_PUB := zmq_socket(FContext, ZMQ_PUSH);
  zmq_connect(FPusher_PUB, PChar(CLocalHost+CPortPuller_PUB));

  //// reply requests from outside
  //FPuller_REP  := zmq_socket(FContext, ZMQ_PULL);
  //zmq_bind(FPuller_REP, PChar(CHost+CPortPuller_REP));

  // blocking server thread for now
  FReplier := zmq_socket(FContext, ZMQ_REP);
  zmq_bind(FReplier, PChar(CHost+CPortReplier));

  inherited Create(CreateSuspended);
end;

destructor TZMQServerThread.Destroy;
begin
  zmq_close(FPusher_PUB);
  zmq_close(FPuller_PUB);
  zmq_close(FPublisher);
  zmq_ctx_destroy(FContext);
  inherited Destroy;
end;

procedure TZMQServerThread.Push(AMultipartMessage: array of string);
begin
  SendMultiPartString(FPusher_PUB, AMultipartMessage);
end;

procedure LoadIP; //forward;
var
  S : TStringList;
  IPPath: String;
begin
  IPPath := ExtractFilePath(Application.ExeName)+'IP.txt';
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
