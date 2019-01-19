{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_visual_elements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls,Graphics;

type

  { TPlayerBox }

  TPlayerBox = class (TGroupBox)
    LabelLastColor : TLabel;
    PanelLastColor : TPanel;
    LabelLastRow : TLabel;
    LabelLastRowCount : TLabel;
    LabelPoints : TLabel;
    LabelPointsCount : TLabel;
  private
    FID: string;
  public
    constructor Create(AOwner: TComponent;AID:string;Admin:Boolean=False); reintroduce;
    procedure InvisibleLineRow;
    property ID : string read FID write FID;
  end;

resourcestring
  CAP_ROW = 'Row:';
  CAP_COLOR = 'Color:';
  CAP_POINTS = 'Tokens:';
  CAP_NA = 'NA';
  CAP_WAINTING_FOR_PLAYER = 'Waiting for player...';

implementation

{ TPlayerBox }

constructor TPlayerBox.Create(AOwner: TComponent; AID: string; Admin: Boolean);
begin
  inherited Create(AOwner);
  FID := AID;
  AutoSize := True;
  Caption := CAP_WAINTING_FOR_PLAYER;
  Font.Size := 12;
  with ChildSizing do
    begin
  	  ControlsPerLine := 2;
  	  EnlargeHorizontal := crsHomogenousChildResize;
  	  HorizontalSpacing := 30;
  	  Layout := cclLeftToRightThenTopToBottom;
  	  LeftRightSpacing := 20;
  	  TopBottomSpacing := 20;
  	  VerticalSpacing := 10;
    end;

  LabelLastColor := TLabel.Create(Self);
  with LabelLastColor do
    begin
      Caption := CAP_COLOR;
      Alignment:=taLeftJustify;
      Layout:=tlCenter;
      Parent := Self;
    end;

  PanelLastColor := TPanel.Create(Self);
  with PanelLastColor do
    begin
      //Caption:=CAP_NA;
      Constraints.MinHeight:=30;
      Constraints.MinWidth:=30;
      //Color:= $0;
      Parent:= Self;
    end;

  LabelLastRow:= TLabel.Create(Self);
  with LabelLastRow do
    begin
      Caption:=CAP_ROW;
      Alignment:=taLeftJustify;
      Layout:=tlCenter;
      Parent := Self;
    end;


  LabelLastRowCount:= TLabel.Create(Self);
  with LabelLastRowCount do
    begin
      Caption:=CAP_NA;
      Alignment:=taCenter;
      Layout:=tlCenter;
      Parent := Self;
    end;
  Enabled:= False;

  LabelPoints:= TLabel.Create(Self);
  with LabelPoints do
    begin
      Caption:=CAP_POINTS;
      Alignment:=taLeftJustify;
      Layout:=tlCenter;
      Parent := Self;
    end;

  LabelPointsCount:= TLabel.Create(Self);
  with LabelPointsCount do
    begin
      Alignment:=taCenter;
      Layout:=tlCenter;
      Caption:='0';
      Parent := Self;
    end;

  if Admin then
    begin
      LabelPoints.Visible := True;
      LabelPointsCount.Visible := True;
    end
  else
    begin
      LabelPoints.Visible := False;
      LabelPointsCount.Visible := False;
    end;
  //LabelLastRow.AutoSize := False;
end;

procedure TPlayerBox.InvisibleLineRow;
begin
  LabelLastColor.Visible:=False;
  PanelLastColor.Visible:=False;
  LabelLastRow.Visible:=False;
  LabelLastRowCount.Visible:=False;
end;

end.

