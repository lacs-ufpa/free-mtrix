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
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Graphics;

type

  { TPlayerBox }

  TPlayerBox = class (TGroupBox)
    LabelLastColor : TLabel;
    PanelLastColor : TPanel;
    LabelLastRow : TLabel;
    LabelLastRowCount : TLabel;
    LabelPointsRed : TLabel;
    LabelPointsRedCount : TLabel;
    LabelPointsBlue : TLabel;
    LabelPointsBlueCount : TLabel;
  private
    FID: string;
  public
    constructor Create(AOwner: TComponent;
      AID:string; Admin:Boolean=False); reintroduce;
    procedure InvisibleLineRow;
    property ID : string read FID write FID;
  end;

resourcestring
  CAP_ROW = 'Row:';
  CAP_COLOR = 'Color:';
  CAP_POINTS_RED  = 'RTokens:';
  CAP_POINTS_BLUE = 'BTokens:';
  CAP_NA = 'NA';
  CAP_WAINTING_FOR_PLAYER = 'Waiting for player...';

implementation

{ TPlayerBox }

constructor TPlayerBox.Create(AOwner: TComponent;
  AID: string; Admin: Boolean);
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

  LabelPointsRed:= TLabel.Create(Self);
  with LabelPointsRed do
    begin
      Caption:=CAP_POINTS_RED;
      Alignment:=taLeftJustify;
      Layout:=tlCenter;
      Parent := Self;
    end;

  LabelPointsRedCount:= TLabel.Create(Self);
  with LabelPointsRedCount do
    begin
      Alignment:=taCenter;
      Layout:=tlCenter;
      Caption:='0';
      Parent := Self;
    end;

  LabelPointsBlue:= TLabel.Create(Self);
  with LabelPointsBlue do
    begin
      Caption:=CAP_POINTS_BLUE;
      Alignment:=taLeftJustify;
      Layout:=tlCenter;
      Parent := Self;
    end;

  LabelPointsBlueCount:= TLabel.Create(Self);
  with LabelPointsBlueCount do
    begin
      Alignment:=taCenter;
      Layout:=tlCenter;
      Caption:='0';
      Parent := Self;
    end;

  if Admin then
    begin
      LabelPointsRed.Visible := True;
      LabelPointsRedCount.Visible := True;
      LabelPointsBlue.Visible := True;
      LabelPointsBlueCount.Visible := True;
    end
  else
    begin
      LabelPointsRed.Visible := False;
      LabelPointsRedCount.Visible := False;
      LabelPointsBlue.Visible := False;
      LabelPointsBlueCount.Visible := False;
    end;
  Enabled:= False;
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

