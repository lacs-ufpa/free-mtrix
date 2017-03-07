unit popup_hack;

{$mode objfpc}{$H+}

interface

uses
  LCLType, LCLIntf, Classes, SysUtils,
  Controls, ExtCtrls, StdCtrls, Graphics
  , PopupNotifier
  ;

type

  { TPopupNotifierHack }

  TPopupNotifierHack = class(TPopupNotifier)
  private
    FTimer : TTimer;
    FLabel : TLabel;
    procedure AutoDestroy(Sender:TObject);
    procedure AutoDestroyIn(AInterval : integer);
  public
    constructor Create(AOwner : TComponent); override;
    //destructor Destroy;override;
    procedure ShowAndAutoDestroy(AText:string; AControl:TControl;AInterval:integer=15000);
  end;

implementation

{ TPopupNotifierHack }

procedure TPopupNotifierHack.AutoDestroy(Sender: TObject);
begin
  if Assigned(Self) then
    Free;
end;

constructor TPopupNotifierHack.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Title:='';
  Text:='';
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer:=@AutoDestroy;
  FLabel := TLabel.Create(vNotifierForm);
  FLabel.Name:='UglyHack';
  FLabel.Align:=alClient;
  FLabel.Anchors := [akLeft,akRight];
  FLabel.Alignment := taCenter;
  FLabel.AutoSize:=True;
  FLabel.Layout := tlCenter;
  FLabel.WordWrap := False;
  FLabel.BorderSpacing.Top:=26;
  FLabel.BorderSpacing.Left:=26;
  FLabel.BorderSpacing.Right:=26;
  FLabel.BorderSpacing.Bottom:=26;
  FLabel.OnClick := vNotifierForm.OnClick;
  FLabel.Parent := vNotifierForm;
  vNotifierForm.AutoSize:=True;
  Color:=clTeal;
end;

//destructor TPopupNotifierHack.Destroy;
//begin
//
//  inherited Destroy;
//end;

procedure TPopupNotifierHack.AutoDestroyIn(AInterval: integer);
begin
  FTimer.Interval:=AInterval;
  FTimer.Enabled:=True;
end;

procedure TPopupNotifierHack.ShowAndAutoDestroy(AText: string;
  AControl: TControl; AInterval: integer);
var
  PopUpPos : TPoint;
  w: Integer;
  r: TRect;
begin
  FLabel.Caption:=AText;
  Show;
  w := FLabel.Width;
  Hide;
  vNotifierForm.AutoSize:=False;
  r:=vNotifierForm.CalcHintRect(w, AText, nil);
  vNotifierForm.HintRect:=r;
  vNotifierForm.Width:=r.Right-r.Left + 52;
  vNotifierForm.Height:=r.Bottom-r.Top + 52;

  PopUpPos.X := (AControl.Width div 2) - (vNotifierForm.Width div 2);
  PopUpPos.Y := (AControl.Height div 2) - (vNotifierForm.Height div 2);
  ShowAtPos(PopUpPos.X, PopUpPos.Y);
  AutoDestroyIn(AInterval);
end;

end.

