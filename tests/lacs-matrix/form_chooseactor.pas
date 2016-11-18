unit form_chooseactor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  game_actors
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAdmin: TButton;
    btnPlayer: TButton;
    procedure btnAdminClick(Sender: TObject);
    procedure btnPlayerClick(Sender: TObject);
  private
    FGameActor: TGameActor;
    procedure SetGameActor(AValue: TGameActor);
    { private declarations }
  public
    property GameActor : TGameActor read FGameActor write SetGameActor;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnAdminClick(Sender: TObject);
begin
  GameActor:=gaAdmin;
  ModalResult:=1;
end;

procedure TForm1.btnPlayerClick(Sender: TObject);
begin
  GameActor:=gaPlayer;
  ModalResult:=1;
end;

procedure TForm1.SetGameActor(AValue: TGameActor);
begin
  if FGameActor=AValue then Exit;
  FGameActor:=AValue;
end;



end.

