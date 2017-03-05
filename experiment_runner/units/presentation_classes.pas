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

  TAnnouncerStartEvent = procedure (AMessage : array of UTF8String) of object;

  { TAnnoucerMessages }

  TAnnoucerMessages = array of array of UTF8String;

  { TIntervalarAnnouncer }

  TIntervalarAnnouncer = class(TComponent)
  private
    FMessages: TAnnoucerMessages;
    FTimer : TTimer;
    FOnStart: TAnnouncerStartEvent;
    function GetEnabled: Boolean;
    function GetInterval: LongWord;
    procedure NextMessage;
    procedure SetEnabled(AValue: Boolean);
    procedure SelfDestroy(Sender: TObject);
    procedure SetInterval(AValue: LongWord);
    procedure StartTimer(Sender:TObject);
  public
    constructor Create(AOwner : TComponent); override;
    procedure Append(M : array of UTF8String);
    procedure Reversed;
    property Messages : TAnnoucerMessages read FMessages write FMessages;
    property OnStart : TAnnouncerStartEvent read FOnStart write FOnStart;
    property Interval : LongWord read GetInterval write SetInterval;
    property Enabled : Boolean read GetEnabled write SetEnabled;
  end;

implementation

{ TIntervalarAnnouncer }

procedure TIntervalarAnnouncer.SetEnabled(AValue: Boolean);
begin
  if FTimer.Enabled=AValue then Exit;
  FTimer.Enabled:= AValue;
end;

function TIntervalarAnnouncer.GetEnabled: Boolean;
begin
  Result := FTimer.Enabled;
end;

function TIntervalarAnnouncer.GetInterval: LongWord;
begin
  Result := FTimer.Interval;
end;

procedure TIntervalarAnnouncer.NextMessage;
begin
  SetLength(FMessages,Length(FMessages)-1);
end;

procedure TIntervalarAnnouncer.SelfDestroy(Sender : TObject);
var LAnnouncer : TIntervalarAnnouncer;
begin
  if Length(FMessages) > 0 then
    begin
      LAnnouncer := TIntervalarAnnouncer.Create(nil);
      LAnnouncer.Messages := FMessages;
      LAnnouncer.OnStart:= FOnStart;
      LAnnouncer.Enabled:=True;
    end;
  Free;
end;

procedure TIntervalarAnnouncer.SetInterval(AValue: LongWord);
begin
  if FTimer.Interval=AValue then Exit;
  FTimer.Interval:= AValue;
end;

procedure TIntervalarAnnouncer.StartTimer(Sender: TObject);
var M : array of UTF8String;
begin
  if Length(FMessages) > 0 then
    begin
      M := FMessages[High(FMessages)];
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

procedure TIntervalarAnnouncer.Append(M: array of UTF8String);
var
  H : TAnnoucerMessages;
  i: Integer;
begin
  SetLength(H,1,Length(M));

  for i := Low(M) to High(M) do
    H[0,i] := M[i];

  SetLength(FMessages,Length(FMessages)+1);
  FMessages[High(FMessages)] := H[0];
end;

procedure TIntervalarAnnouncer.Reversed;
var
  i : integer;
  M : TAnnoucerMessages;
begin
  for i := High(FMessages) downto Low(FMessages) do
    begin
      SetLength(M,Length(M)+1);
      M[High(M)] := FMessages[i]
    end;
  FMessages := M;
end;

end.

