{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
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
  FLabel.Caption:='';
  FLabel.Align:=alClient;
  FLabel.Anchors := [akLeft,akRight];
  FLabel.Alignment := taCenter;
  FLabel.AutoSize:=True;
  FLabel.Layout := tlCenter;
  FLabel.WordWrap := False;
  FLabel.Font.Color:=clDefault;
  FLabel.BorderSpacing.Top:=26;
  FLabel.BorderSpacing.Left:=26;
  FLabel.BorderSpacing.Right:=26;
  FLabel.BorderSpacing.Bottom:=26;
  FLabel.OnClick := vNotifierForm.OnClick;
  FLabel.Parent := vNotifierForm;
  vNotifierForm.AutoSize:=True;
  vNotifierForm.Font.Size := 12;
  Color:=clDefault;
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
  if Assigned(AControl) then
    begin
      PopUpPos.X := (AControl.Width div 2) - (vNotifierForm.Width div 2);
      PopUpPos.Y := (AControl.Height div 2) - (vNotifierForm.Height div 2);
      ShowAtPos(PopUpPos.X, PopUpPos.Y);
    end
  else
    ShowAtPos(0, 0);
  AutoDestroyIn(AInterval);
end;

end.

