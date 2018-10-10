{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit form_chooseactor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls
  , game_actors
  ;

type

  { TFormChooseActor }

  TFormChooseActor = class(TForm)
    btnAdmin: TButton;
    btnPlayer: TButton;
    btnPlayerResume: TButton;
    //btnAdmin: TButton;
    //btnPlayer: TButton;
    //btnPlayerResume: TButton;
    procedure btnAdminClick(Sender: TObject);
    procedure btnPlayerClick(Sender: TObject);
    procedure btnPlayerResumeClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure ExitApplication(Sender: TObject);
    procedure ShowResumeButton(Sender: TObject);
  private
    FGameActor: TGameActor;
    FCanClose : Boolean;
    FStyle: string;
    procedure SetStyle(AValue: string);
    { private declarations }
  public
    procedure ShowPoints(M : string);
    property GameActor : TGameActor read FGameActor;
    property Style : string read FStyle write SetStyle;
  end;

var
  FormChooseActor: TFormChooseActor;

implementation

{$R *.lfm}

{ TFormChooseActor }

procedure TFormChooseActor.btnAdminClick(Sender: TObject);
begin
  FGameActor:=gaAdmin;
  FCanClose := True;
  ModalResult:=1;
end;

procedure TFormChooseActor.btnPlayerClick(Sender: TObject);
begin
  FGameActor:=gaPlayer;
  FCanClose := True;
  ModalResult:=1;
end;

procedure TFormChooseActor.btnPlayerResumeClick(Sender: TObject);
begin
  FCanClose := True;
  ModalResult:=1;
end;

procedure TFormChooseActor.FormCloseQuery(Sender: TObject; var CanClose: boolean  );
begin
  CanClose := FCanClose;
end;

procedure TFormChooseActor.FormCreate(Sender: TObject);
begin
  FCanClose := True;
end;

procedure TFormChooseActor.ExitApplication(Sender: TObject);
begin
  {$IFNDEF TEST_MODE}
  Application.Terminate;
  {$ENDIF}
end;

procedure TFormChooseActor.SetStyle(AValue: string);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  case AValue of
    '.Arrived': btnPlayerResume.Visible:=False;
    '.Left', '.EndX':
      begin
        btnPlayerResume.Visible:=False;
        btnAdmin.Visible:= False;
        btnPlayer.Visible:= False;
        BorderStyle:=bsNone;
        {$IFDEF WINDOWS}
        BoundsRect := Monitor.BoundsRect;
        {$ENDIF}
        Position:=poDesigned;
        FormStyle:=fsNormal;
        WindowState:=wsFullScreen;
      end;
  end;
end;

procedure TFormChooseActor.ShowPoints(M: string);
var L : TLabel;
begin
  L := TLabel.Create(Self);
  with L do
  begin
    Name := 'LabelGoodBye';
    Align:=alClient;
    Caption:= M;
    Alignment := taCenter;
    Anchors := [akLeft,akRight];
    Layout := tlCenter;
    WordWrap := True;
    Parent:=Self;
    Font.Size := 30;
    //OnClick := @ShowResumeButton;
    case FStyle of
      '.Left': btnPlayerResume.Caption := 'Entrar';
      '.EndX':
        begin
          btnPlayerResume.Caption := 'Sair';
          FCanClose := True;
        end;
    end;
  end;
end;

procedure TFormChooseActor.ShowResumeButton(Sender: TObject);
var i : integer;
begin
  for i := 0 to ComponentCount-1 do
    if Components[i].Name = 'LabelGoodBye' then
      begin
       TLabel(Components[i]).Visible:=False;
       Break;
      end;
  btnPlayerResume.Visible:=True;
end;

end.

