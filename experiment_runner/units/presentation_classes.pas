{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit presentation_classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls;

type

  { TAnnouncerStartEvent }

  TAnnouncerStartEvent = procedure (AMessage : array of string) of object;

  { TAnnoucerMessage }
  TAnnoucerMessage = record
    Message : array of string;
    Interval : integer;
  end;

  { TAnnoucerMessages }

  TAnnoucerMessages = array of TAnnoucerMessage;

  { TIntervalarAnnouncer }

  TIntervalarAnnouncer = class(TComponent)
  private
    FMessages: TAnnoucerMessages;
    FTimer : TTimer;
    FOnStart: TAnnouncerStartEvent;
    function GetEnabled: Boolean;
    procedure NextMessage;
    procedure SetEnabled(AValue: Boolean);
    procedure SelfDestroy(Sender: TObject);
    procedure StartTimer(Sender:TObject);
  public
    constructor Create(AOwner : TComponent); override;
    procedure Append(M : array of string; AInterval: integer);
    procedure Reversed;
    property Messages : TAnnoucerMessages read FMessages write FMessages;
    property OnStart : TAnnouncerStartEvent read FOnStart write FOnStart;
    property Enabled : Boolean read GetEnabled write SetEnabled;
  end;

implementation

{ TIntervalarAnnouncer }

function TIntervalarAnnouncer.GetEnabled: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TIntervalarAnnouncer.NextMessage;
begin
  SetLength(FMessages,Length(FMessages)-1);
  if Length(FMessages) > 0 then
    FTimer.Interval := FMessages[High(FMessages)].Interval;
end;

procedure TIntervalarAnnouncer.SetEnabled(AValue: Boolean);
begin
  if AValue = FTimer.Enabled then Exit;
  FTimer.Enabled:=AValue;

  if AValue and (Length(FMessages) > 0) then
    FTimer.Interval := FMessages[High(FMessages)].Interval;
end;

procedure TIntervalarAnnouncer.SelfDestroy(Sender : TObject);
var LAnnouncer : TIntervalarAnnouncer;
begin
  if Length(FMessages) > 0 then
    begin
      LAnnouncer := TIntervalarAnnouncer.Create(nil);
      LAnnouncer.Messages := FMessages;
      LAnnouncer.OnStart := FOnStart;
      LAnnouncer.Enabled := True;
    end;
  Free;
end;

procedure TIntervalarAnnouncer.StartTimer(Sender: TObject);
var
  M : array of string;
begin
  if Length(FMessages) > 0 then
    begin
      M := FMessages[High(FMessages)].Message;
      NextMessage;
      if Assigned(FOnStart) then FOnStart(M);
    end;
end;

constructor TIntervalarAnnouncer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 5000;
  FTimer.OnTimer:=@SelfDestroy;
  //FTimer.OnStopTimer:=@SelfDestroy;
  FTimer.OnStartTimer:=@StartTimer;
end;

procedure TIntervalarAnnouncer.Append(M: array of string; AInterval: integer);
var
  H : TAnnoucerMessages;
  i: Integer;
begin
  SetLength(H, 1);
  H[0].Interval := AInterval;
  SetLength(H[0].Message, Length(M));
  for i := Low(M) to High(M) do
    H[0].Message[i] := M[i];

  SetLength(FMessages,Length(FMessages)+1);
  FMessages[High(FMessages)] := H[0];
end;

procedure TIntervalarAnnouncer.Reversed;
var
  i : integer;
  M : TAnnoucerMessages;
begin
  SetLength(M, 0);
  for i := High(FMessages) downto Low(FMessages) do
    begin
      SetLength(M,Length(M)+1);
      M[High(M)] := FMessages[i]
    end;
  FMessages := M;
end;

end.

