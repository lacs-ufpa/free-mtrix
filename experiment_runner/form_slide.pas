unit form_slide;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls
  , game_slide
  ;

type

  { TFormSlide }

  TFormSlide = class(TForm)
    procedure FormCloseQuery(Sender : TObject; var CanClose : Boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure DoCustomClose(Sender : TObject);
  private
    FCanClose : Boolean;
    FGameSlides : TGameSlideList;
  public
    function ShowModal : integer; override;
    procedure Append(ASlideText : string);
    procedure FullScreen;
  end;

var
  FormSlide : TFormSlide;

implementation

{$R *.lfm}

{ TFormSlide }

procedure TFormSlide.FormCloseQuery(Sender : TObject; var CanClose : Boolean);
begin
  CanClose := FCanClose;
end;

procedure TFormSlide.FormCreate(Sender : TObject);
begin
  FCanClose := False;
  FGameSlides := TGameSlideList.Create;
  FGameSlides.CurrentSlide := TGameSlide.Create(Self);
  FGameSlides.OnEndSlides := @DoCustomClose;
end;


procedure TFormSlide.FormDestroy(Sender : TObject);
begin
  FGameSlides.Free;
end;

procedure TFormSlide.DoCustomClose(Sender : TObject);
begin
  FCanClose := True;
  Close;
  ModalResult:=1;
end;

procedure TFormSlide.Append(ASlideText : string);
begin
  FGameSlides.Append(ASlideText);
end;

procedure TFormSlide.FullScreen;
begin
  with Self do begin
    BorderStyle:=bsNone;
    {$IFDEF WINDOWS}
    BoundsRect := Monitor.BoundsRect;
    {$ENDIF}
    Position:=poDesigned;
    FormStyle:=fsNormal;
    WindowState:=wsFullScreen;
  end;
end;

function TFormSlide.ShowModal : integer;
begin
  FGameSlides.Play;
  Result := inherited ShowModal;
end;


end.

