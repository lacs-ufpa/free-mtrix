{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit form_matrixgame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, DBGrids, ExtCtrls

  //, zmq_pub_sub
  , game_zmq_actors
  , game_actors
  , game_control
  ;

type

  { TFormMatrixGame }

  TFormMatrixGame = class(TForm)
    btnConfirmRow: TButton;
    GBIndividual: TGroupBox;
    GBLastChoice: TGroupBox;
    GBIndividualAB: TGroupBox;
    GBGrupo: TGroupBox;
    GBAdmin: TGroupBox;
    GBLastChoiceP0: TGroupBox;
    GBLastChoiceP1: TGroupBox;
    GBLastChoiceP2: TGroupBox;
    GBExperiment: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    LabelExpCondCount: TLabel;
    LabelExpGen: TLabel;
    LabelExpGenCount: TLabel;
    LabelExpCycle: TLabel;
    LabelExpCycleCount: TLabel;
    LabelExpNxtPlayer: TLabel;
    LabelExpNxtPlayerCount: TLabel;
    LabelExpInterlocks: TLabel;
    LabelExpInterlocksCount: TLabel;
    LabelIndCount: TLabel;
    LabelIndACount: TLabel;
    LabelIndBCount: TLabel;
    LabelCurrentColor1: TLabel;
    LabelCurrentLine1: TLabel;
    LabelIndA: TLabel;
    LabelGroupCount: TLabel;
    LabelIndB: TLabel;
    LabelCurrentLineNumber1: TLabel;
    LabelYouLastChoiceColor3: TLabel;
    LabelYouLastChoiceColor4: TLabel;
    LabelExpCond: TLabel;
    ChatMemoRecv: TMemo;
    ChatMemoSend: TMemo;
    ChatPanel: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PanelCurrentColor1: TPanel;
    ChatSplitter: TSplitter;
    StringGridMatrix: TStringGrid;
    procedure btnConfirmRowClick(Sender: TObject);
    procedure ChatMemoSendKeyPress(Sender: TObject; var Key: char);
    procedure CheckBoxDrawDotsChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure StringGridMatrixBeforeSelection(Sender: TObject; aCol, aRow: integer);
    procedure StringGridMatrixDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
  private
    FGameControl : TGameControl;
    FID: string;
    FMustDrawDots: Boolean;
    FMustDrawDotsClear: Boolean;
    FRowBase: integer;
  public
    procedure SetID(S : string);
    procedure SetGameActor(AValue: TGameActor);
    property ID : string read FID;
  end;

var
  FormMatrixGame: TFormMatrixGame;

implementation

uses LCLType, game_resources;

// uses datamodule;
var
  MustDrawSelection : Boolean; // work around until a bug fix for ClearSelection is released

{$R *.lfm}

{ TFormMatrixGame }

procedure TFormMatrixGame.StringGridMatrixDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  OldCanvas: TCanvas;
  RowBase : integer;

  procedure SaveOldCanvas;
  begin
    OldCanvas := TCanvas.Create;
    OldCanvas.Brush.Style := TStringGrid(Sender).Canvas.Brush.Style;
    OldCanvas.Brush.Color := TStringGrid(Sender).Canvas.Brush.Color;
    OldCanvas.Pen.Width := TStringGrid(Sender).Canvas.Pen.Width;
    OldCanvas.Pen.Color := TStringGrid(Sender).Canvas.Pen.Color;
    OldCanvas.Pen.Mode := TStringGrid(Sender).Canvas.Pen.Mode;
  end;

  procedure LoadOldCanvas;
  begin
    TStringGrid(Sender).Canvas.Brush.Style := OldCanvas.Brush.Style;
    TStringGrid(Sender).Canvas.Brush.Color := OldCanvas.Brush.Color;
    TStringGrid(Sender).Canvas.Pen.Width := OldCanvas.Pen.Width;
    TStringGrid(Sender).Canvas.Pen.Color := OldCanvas.Pen.Color;
    TStringGrid(Sender).Canvas.Pen.Mode := OldCanvas.Pen.Mode;
  end;

  procedure DrawLines(Color: TColor);
  //function HalfDarker(Color : TColor) : TColor;
  //begin
  //  Result := ((Blue(Color) and $7F) shl 16) or ((Green(Color) and $7F) shl 8 ) or (Red(Color) and $7F)
  //end;

    procedure DrawDots;
    var
      LFix, LLeft, LRight, LHSize, LVSize: longint;
    begin
      LFix := 2;
      LVSize := ((aRect.Bottom - aRect.Top) div 2);
      LHSize := aRect.Left + (aRect.Right - aRect.Left) div 2;
      LLeft := LHSize - LVSize;
      LRight := LHSize + LVSize;
      TStringGrid(Sender).Canvas.Brush.Style := bsClear;
      TStringGrid(Sender).Canvas.Brush.Color := clBlack;
      TStringGrid(Sender).Canvas.Pen.Color := clBlack;
      TStringGrid(Sender).Canvas.Ellipse(LLeft + LFix, aRect.Top + LFix,
        LRight - LFix, aRect.Bottom - LFix);
    end;

  begin
    TStringGrid(Sender).Canvas.Brush.Style := bsSolid;
    TStringGrid(Sender).Canvas.Pen.Width := 1;
    TStringGrid(Sender).Canvas.Brush.Color := Color;
    TStringGrid(Sender).Canvas.Pen.Color := Color;
    TStringGrid(Sender).Canvas.Rectangle(aRect);
    if Assigned(FGameControl) then
      if FGameControl.MustDrawDots then
        if (Odd(aRow + RowBase) and not Odd(aCol)) or
          (not Odd(aRow + RowBase) and Odd(aCol)) then
          DrawDots;
  end;
  //function GetTextX(S : String): Longint;
  //begin
  //  Result := aRect.Left+((aRect.Right-aRect.Left)div 2) - ((Length(S)*7)div 2);
  //end;

begin
  if Assigned(FGameControl) then
    RowBase:=FGameControl.RowBase;
  SaveOldCanvas;
  try
    //if (aRow >= RowBase) and (aCol = 10) then
    //  DrawLines(clWhite);

    if (aCol <> 0) and (aRow > (RowBase-1)) then
      begin
        DrawLines(GetRowColor(aRow,RowBase));

        if (gdSelected in aState) and MustDrawSelection then
          begin
            TStringGrid(Sender).Canvas.Pen.Width := 10;
            TStringGrid(Sender).Canvas.Pen.Color := clWhite;
            if (aRow = TStringGrid(Sender).Selection.Top) and (aCol = TStringGrid(Sender).Selection.Left) then
              begin
                TStringGrid(Sender).Canvas.PenPos := aRect.TopLeft;
                TStringGrid(Sender).Canvas.LineTo(Point(aRect.Left,aRect.Bottom));
              end;
            TStringGrid(Sender).Canvas.PenPos := aRect.BottomRight;
            TStringGrid(Sender).Canvas.LineTo(Point(aRect.Left,aRect.Bottom));
            TStringGrid(Sender).Canvas.PenPos := aRect.TopLeft;
            TStringGrid(Sender).Canvas.LineTo(Point(aRect.Right,aRect.Top));

            if (aRow = TStringGrid(Sender).Selection.Top) and (aCol = TStringGrid(Sender).Selection.Right) then
              begin
                TStringGrid(Sender).Canvas.PenPos := aRect.BottomRight;
                TStringGrid(Sender).Canvas.LineTo(Point(aRect.Right,aRect.Top));
              end;
          end;
      end;

    TStringGrid(Sender).Canvas.Pen.Width := 2;
    TStringGrid(Sender).Canvas.Font.Size := 10;
    TStringGrid(Sender).Canvas.Font.Color := clBlack;
    TStringGrid(Sender).Canvas.Brush.Style := bsClear;

    if (aCol = 10) and (gdSelected in aState) and MustDrawSelection then
      if (aRow = TStringGrid(Sender).Selection.Top) and (aCol = TStringGrid(Sender).Selection.Right) then
        begin
          btnConfirmRow.Top := aRect.Top+5;
          btnConfirmRow.Left := aRect.Right+5;
        end;

  finally
    LoadOldCanvas;
    OldCanvas.Free;
  end;
end;

procedure TFormMatrixGame.SetGameActor(AValue: TGameActor);

  procedure SetZMQAdmin;
  begin
    FGameControl := TGameControl.Create(TZMQAdmin.Create(Self));
    GBAdmin.Visible:= True;
  end;

  procedure SetZMQPlayer;
  begin
    FGameControl := TGameControl.Create(TZMQPlayer.Create(Self));
    btnConfirmRow.Visible := True;
    StringGridMatrix.Enabled := True;

    FGameControl.SendMessage(K_ARRIVED);
  end;

  procedure SetZMQWatcher;
  begin
    FGameControl := TGameControl.Create(TZMQWatcher.Create(Self));
  end;

begin
  case AValue of
    gaAdmin: SetZMQAdmin;
    gaPlayer: SetZMQPlayer;
    gaWatcher: SetZMQWatcher;
  end;
  FGameControl.SetID(FID);
end;

procedure TFormMatrixGame.SetID(S: string);
begin
  FID := S;
end;

procedure TFormMatrixGame.CheckBoxDrawDotsChange(Sender: TObject);
begin
  StringGridMatrix.Invalidate;
end;

procedure TFormMatrixGame.FormActivate(Sender: TObject);
begin
  StringGridMatrix.ClearSelections;
  StringGridMatrix.FocusRectVisible := False;
end;

procedure TFormMatrixGame.StringGridMatrixBeforeSelection(Sender: TObject; aCol, aRow: integer);
begin
  if MustDrawSelection then Exit;
  MustDrawSelection := True;
end;

procedure TFormMatrixGame.ChatMemoSendKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Char(VK_RETURN) then
    begin
      FGameControl.SendMessage(K_CHAT_M);
      with ChatMemoSend do
        begin
          Clear;
          SelStart:=0;
          SelLength:=0;
          SetFocus;
        end;
      Key := Char(VK_UNKNOWN);
    end;
end;

procedure TFormMatrixGame.btnConfirmRowClick(Sender: TObject);
begin
  //StringGridMatrix.ClearSelections;
  //MustDrawSelection := False;
  StringGridMatrix.Enabled:= False;
  btnConfirmRow.Visible:=False;
  FGameControl.SendMessage(K_CHOICE);
end;

end.
