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
  private
    FID: string;
  public
    constructor Create(AOwner: TComponent;AID:string); reintroduce;
    property ID : string read FID write FID;
  end;

resourcestring
  CAP_ROW = 'Linhas:';
  CAP_COLOR = 'Cor:';
  CAP_NA = 'NA';
  CAP_WAINTING_FOR_PLAYER = 'Esperando Jogador...';

implementation

{ TPlayerBox }

constructor TPlayerBox.Create(AOwner: TComponent; AID: string);
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
  PanelLastColor.Caption:='';
  //PanelLastColor.Color:= $0;
  PanelLastColor.Parent:= Self;

  LabelLastRow:= TLabel.Create(Self);
  LabelLastRow.Caption:=CAP_ROW;
  LabelLastRow.Parent := Self;

  LabelLastRow:= TLabel.Create(Self);
  LabelLastRow.Caption:=CAP_NA;
  LabelLastRow.Parent := Self;
  Enabled:= False;
  //LabelLastRow.AutoSize := False;
end;

end.

