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
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls;

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
    property ID : string read FID write FID;
  end;

resourcestring
  CAP_ROW = 'Linha:';
  CAP_COLOR = 'Cor:';
  CAP_POINTS = 'Pontos:';
  CAP_NA = 'NA';
  CAP_WAINTING_FOR_PLAYER = 'Esperando Jogador...';

implementation

{ TPlayerBox }

constructor TPlayerBox.Create(AOwner: TComponent; AID: string; Admin: Boolean);
begin
  inherited Create(AOwner);
  FID := AID;
  AutoSize := True;
  Caption := CAP_WAINTING_FOR_PLAYER;
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
  LabelLastColor.Caption := CAP_COLOR;
  LabelLastColor.Parent := Self;

  PanelLastColor := TPanel.Create(Self);
  PanelLastColor.Caption:=CAP_NA;
  //PanelLastColor.Color:= $0;
  PanelLastColor.Parent:= Self;
  LabelLastRow:= TLabel.Create(Self);
  LabelLastRow.Caption:=CAP_ROW;
  LabelLastRow.Parent := Self;

  LabelLastRowCount:= TLabel.Create(Self);
  LabelLastRowCount.Caption:=CAP_NA;
  LabelLastRowCount.Parent := Self;
  Enabled:= False;

  if Admin then
    begin
      LabelPoints:= TLabel.Create(Self);
      LabelPoints.Caption:=CAP_POINTS;
      LabelPoints.Parent := Self;

      LabelPointsCount:= TLabel.Create(Self);
      LabelPointsCount.Caption:='0';
      LabelPointsCount.Parent := Self;
    end;
  //LabelLastRow.AutoSize := False;
end;

end.

