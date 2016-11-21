unit form_chooseactor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls,ExtCtrls, LCLType
  , game_actors
  ;

type

  { TFormChooseActor }

  TFormChooseActor = class(TForm)
    btnAdmin: TButton;
    btnPlayer: TButton;
    btnPlayerResume: TButton;
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

procedure TFormChooseActor.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
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
  FStyle:=AValue;
end;



end.

