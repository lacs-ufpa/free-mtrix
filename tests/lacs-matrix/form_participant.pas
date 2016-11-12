{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit form_participant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, DBGrids, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBoxDrawDots: TCheckBox;
    GBChoicesCurrentTurn: TGroupBox;
    GBIndividualPoints: TGroupBox;
    GBGrupo: TGroupBox;
    GBChoicesLastTurn: TGroupBox;
    IndLastResponse: TGroupBox;
    IndLastResponse1: TGroupBox;
    IndLastResponse2: TGroupBox;
    IndCurrentResponse1: TGroupBox;
    IndLastResponse4: TGroupBox;
    IndLastResponse5: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    LabelIndACount: TLabel;
    LabelIndBCount: TLabel;
    LabelCurrentColor1: TLabel;
    LabelLastLine: TLabel;
    LabelCurrentLine1: TLabel;
    LabelLastLineNumber: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelIndA: TLabel;
    LabelGroupCount: TLabel;
    LabelIndB: TLabel;
    LabelLastColor: TLabel;
    LabelCurrentLineNumber1: TLabel;
    LabelYouLastChoiceColor1: TLabel;
    LabelYouLastChoiceColor2: TLabel;
    LabelYouLastChoiceColor3: TLabel;
    LabelYouLastChoiceColor4: TLabel;
    Panel4: TPanel;
    Panel5: TPanel;
    PanelLastColor: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelCurrentColor1: TPanel;
    rgMatrixType: TRadioGroup;
    StringGridMatrix: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure CheckBoxDrawDotsChange(Sender: TObject);
    procedure rgMatrixTypeClick(Sender: TObject);
    procedure StringGridMatrixBeforeSelection(Sender: TObject; aCol, aRow: integer);
    procedure StringGridMatrixDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  RowBase: integer = 0;
  MustDrawSelection : Boolean;

const
  ccYellow = $00FFFF;
  ccRed = $FF0018;
  ccGreen = $006400;
  ccBlue = $0000FF;
  ccMagenta = $8B008B;

implementation

// uses datamodule;

{$R *.lfm}

{ TForm1 }

procedure TForm1.StringGridMatrixDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  OldCanvas: TCanvas;

  procedure SaveOldCanvas;
  begin
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
    if CheckBoxDrawDots.Checked then
      if (Odd(aRow + RowBase) and not Odd(aCol)) or
        (not Odd(aRow + RowBase) and Odd(aCol)) then
        DrawDots;
  end;

begin
  OldCanvas := TCanvas.Create;
  SaveOldCanvas;
  try
    if (aCol <> 0) then
    begin
      if (aRow = RowBase) or (aRow = (RowBase + 5)) then DrawLines(ccYellow);
      if (aRow = RowBase + 1) or (aRow = (RowBase + 6)) then DrawLines(ccGreen);
      if (aRow = RowBase + 2) or (aRow = (RowBase + 7)) then DrawLines(ccRed);
      if (aRow = RowBase + 3) or (aRow = (RowBase + 8)) then DrawLines(ccBlue);
      if (aRow = RowBase + 4) or (aRow = (RowBase + 9)) then DrawLines(ccMagenta);
    end;

    if (aCol <> 0) then
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
  finally
    LoadOldCanvas;
    OldCanvas.Free;
  end;
end;


procedure TForm1.CheckBoxDrawDotsChange(Sender: TObject);
begin
  StringGridMatrix.Invalidate;
end;

procedure TForm1.rgMatrixTypeClick(Sender: TObject);

  procedure WriteGridFixedNames(AStringGrid: TStringGrid; WriteCols: boolean);
  var
    i: integer;
  begin
    with AStringGrid do
      for i := 0 to 9 do
      begin
        Cells[0, i + RowBase] := IntToStr(i + 1);
        if WriteCols then
          Cells[i + 1, 0] := chr(65 + i);
      end;
  end;

begin
  case rgMatrixType.ItemIndex of
    // rows only
    0:
    begin
      StringGridMatrix.Clean;
      StringGridMatrix.FixedRows := 0;
      StringGridMatrix.RowCount := 10;
      StringGridMatrix.Options := [goFixedHorzLine, goHorzLine, goDrawFocusSelected, goRowSelect];
      RowBase := 0;
      WriteGridFixedNames(StringGridMatrix, False);
    end;
    // rows and cols
    1:
    begin
      StringGridMatrix.Clean;
      StringGridMatrix.FixedRows := 1;
      StringGridMatrix.RowCount := 11;
      StringGridMatrix.Options := [goFixedHorzLine, goHorzLine, goDrawFocusSelected, goRowSelect, goVertLine];
      RowBase := 1;
      WriteGridFixedNames(StringGridMatrix, True);
    end;
  end;

end;

procedure TForm1.StringGridMatrixBeforeSelection(Sender: TObject; aCol, aRow: integer);
begin
  if MustDrawSelection then Exit;
  MustDrawSelection := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  StringGridMatrix.ClearSelections;
  MustDrawSelection := False;
  StringGridMatrix.Invalidate;
end;

end.
