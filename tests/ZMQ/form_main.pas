unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls
  , zmq_network;
type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Server : TZMQServerThread;
    Requester : TZMQRequestsThread;
    procedure ReplyReceived(AReply: TStringList);
    procedure RequestReceived(var ARequest : TStringList);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Server := TZMQServerThread.Create('A');
  Server.OnRequestReceived := @RequestReceived;
  Server.Start;

  Requester := TZMQRequestsThread.Create(True);
  Requester.OnReplyReceived:=@ReplyReceived;
  Requester.Start;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Requester.Request(['B', '', 'Test']);
end;

procedure TForm1.ReplyReceived(AReply: TStringList);
begin
  ShowMessage(AReply.Text);
end;

procedure TForm1.RequestReceived(var ARequest: TStringList);
begin
  ShowMessage(ARequest.Text);
end;

end.

