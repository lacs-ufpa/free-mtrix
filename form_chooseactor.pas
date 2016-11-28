{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

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
  private
    FGameActor: TGameActor;
    FCanClose : Boolean;
    FStyle: string;
    procedure SetStyle(AValue: string);
    { private declarations }
  public
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

procedure TFormChooseActor.SetStyle(AValue: string);
begin
  if FStyle=AValue then Exit;
  case AValue of
    '.Arrived': btnPlayerResume.Visible:=False;
    '.Left': btnPlayerResume.Visible:=True;
  end;
  btnAdmin.Visible:= not btnPlayerResume.Visible;
  btnPlayer.Visible:= not btnPlayerResume.Visible;
  FStyle:=AValue;
end;

end.

