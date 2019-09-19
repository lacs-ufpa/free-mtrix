{   
  Free-mtrix - Free cultural selection and social behavior experiments.   
  Copyright (C) 2016-2019 Carlos Rafael Fernandes Picanço.   
  Copyright (C) 2016-2019 Thais Maria Monteiro Guimarães.   
  Copyright (C) 2016-2019 Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License   
  along with this program. If not, see <http://www.gnu.org/licenses/>.   
}
unit game_visual_matrix_a;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Grids, PopupNotifier, Graphics;

type

  { TStringGridA }

  TStringGridA = class (TStringGrid)
  private
    FDrawClearDots: Boolean;
    FDrawDots: Boolean;
    FConfirmationButton: TButton;
    FHasCols: Boolean;
    FHasRows: Boolean;
    //FPopUpNotifier: TPopupNotifier;
    FRowBase: integer;
    function GetRowColor(ARow: integer): TColor;
    procedure GridDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure GridClick(Sender: TObject);
    procedure SetConfirmationButton(AValue: TButton);
    procedure SetDrawClearDots(AValue: Boolean);
    procedure SetDrawDots(AValue: Boolean);
    procedure SetHasCols(AValue: Boolean);
    procedure SetHasRowsOnly(AValue: Boolean);
    //procedure SetPopUpNotifier(AValue: TPopupNotifier);
    procedure UpdateHeight;
    procedure UpdateWidth;
    procedure WriteGridFixedNames;
  public
    constructor Create(AOwner:TComponent);override;
    function GetSelectedRow : integer;
    function GetSelectedRowF : string;
    function GetSelectedMatrixColor : TColor;
    function GetSelectedMatrixColorF: string;
    procedure UpdateSizeAndNames;
    property ConfirmationButton : TButton read FConfirmationButton write SetConfirmationButton;
    // property PopUpNotifier : TPopupNotifier read FPopUpNotifier write SetPopUpNotifier;
    property DrawFilledDots : Boolean read FDrawDots write SetDrawDots;
    property DrawClearDots : Boolean read FDrawClearDots write SetDrawClearDots;
    property HasRows : Boolean read FHasRows write SetHasRowsOnly;
    property HasCols : Boolean read FHasCols write SetHasCols;
  end;

  //GetRowColor(AStringGrid.Selection.Top,RowBase)

implementation

uses game_resources, string_methods;

{ TStringGridA }

constructor TStringGridA.Create(AOwner: TComponent);
var
  LTextStyle: TTextStyle;
begin
  inherited Create(AOwner);
  FConfirmationButton := nil;
  //FPopUpNotifier := nil;
  Top := 50;
  Left := 100;
  AutoEdit:=False;
  Enabled := False;
  LTextStyle := DefaultTextStyle;
  LTextStyle.Alignment:=taCenter;
  DefaultTextStyle := LTextStyle;
  Font.Style:=[fsBold];
  Font.Size:=15;
  Font.Name := 'Times New Roman';
  Options:=[];
  ParentFont:=False;
  OnClick:=@GridClick;
  OnDrawCell:=@GridDrawCell;
  FDrawDots := False;
  FDrawClearDots:= False;
  HasRows:=True;
  //HasCols:=True;
  UpdateSizeAndNames;
end;

function TStringGridA.GetSelectedRow: integer;
begin
  if FRowBase = 0 then
    Result := Selection.Top + 1
  else
    Result := Selection.Top;
end;

function TStringGridA.GetSelectedRowF: string;
begin
  Result := Format('%-*.*d', [1,2,GetSelectedRow]);
end;

function TStringGridA.GetSelectedMatrixColor: TColor;
begin
  Result := GetRowColor(Selection.Top);
end;

function TStringGridA.GetSelectedMatrixColorF: string;
begin
  Result := GetColorString(GetSelectedMatrixColor);
end;

procedure TStringGridA.UpdateSizeAndNames;
begin
  UpdateWidth;
  UpdateHeight;
  WriteGridFixedNames;
end;

function TStringGridA.GetRowColor(ARow: integer): TColor;
var LRow : integer;
begin
  if FRowBase = 1 then
    LRow := ARow -1
  else
    LRow := ARow;

  case LRow of
    0,7 :Result := ccYellow;
    1,6 :Result := ccGreen;
    2,5 :Result := ccRed;
    3,8 :Result := ccBlue;
    4,9 :Result := ccMagenta;
    else
      Result := 0;
  end;
end;

procedure TStringGridA.GridDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  OldCanvas: TCanvas;
  LP : TPoint;

  procedure SaveOldCanvas;
  begin
    OldCanvas := TCanvas.Create;
    OldCanvas.Brush.Style := Canvas.Brush.Style;
    OldCanvas.Brush.Color := Canvas.Brush.Color;
    OldCanvas.Pen.Width := Canvas.Pen.Width;
    OldCanvas.Pen.Color := Canvas.Pen.Color;
    OldCanvas.Pen.Mode := Canvas.Pen.Mode;
  end;

  procedure LoadOldCanvas;
  begin
    Canvas.Brush.Style := OldCanvas.Brush.Style;
    Canvas.Brush.Color := OldCanvas.Brush.Color;
    Canvas.Pen.Width := OldCanvas.Pen.Width;
    Canvas.Pen.Color := OldCanvas.Pen.Color;
    Canvas.Pen.Mode := OldCanvas.Pen.Mode;
  end;

  procedure DrawLines(Color: TColor);
  //function HalfDarker(Color : TColor) : TColor;
  //begin
  //  Result := ((Blue(Color) and $7F) shl 16) or ((Green(Color) and $7F) shl 8 ) or (Red(Color) and $7F)
  //end;

    procedure DrawDots(Filled:Boolean);
    var
      LFix, LLeft, LRight, LHSize, LVSize: longint;
    begin
      LFix := 12;
      LVSize := ((aRect.Bottom - aRect.Top) div 2);
      LHSize := aRect.Left + (aRect.Right - aRect.Left) div 2;
      LLeft := LHSize - LVSize;
      LRight := LHSize + LVSize;
      Canvas.Pen.Color := clBlack;
      if Filled then
        begin
          Canvas.Brush.Style := bsClear;
          Canvas.Brush.Color := clBlack;
          Canvas.Ellipse(LLeft + LFix, aRect.Top + LFix,
            LRight - LFix, aRect.Bottom - LFix);
        end
      else
        begin
          LFix := 13;
          Canvas.Pen.Width := 3;
          Canvas.Pen.EndCap:=pecFlat;
          Canvas.Brush.Style := bsClear;
          Canvas.Brush.Color := clBlack;
          Canvas.Pen.Color := clBlack;
          Canvas.Arc(LLeft + LFix, aRect.Top + LFix-3,
            LRight - LFix, aRect.Bottom - LFix-3,0,5760);
          Canvas.Pen.EndCap:=pecRound;
        end;

    end;

  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Width := 1;
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Rectangle(aRect);

    if FDrawDots then
      if (Odd(aRow + FRowBase) and not Odd(aCol)) or
        (not Odd(aRow + FRowBase) and Odd(aCol)) then
        DrawDots(True);

    if FDrawClearDots then
      if (Odd(aRow + FRowBase) and not Odd(aCol+1)) or
        (not Odd(aRow + FRowBase) and Odd(aCol+1)) then
        DrawDots(False);
  end;
  //function GetTextX(S : String): Longint;
  //begin
  //  Result := aRect.Left+((aRect.Right-aRect.Left)div 2) - ((Length(S)*7)div 2);
  //end;

begin
  SaveOldCanvas;
  try
    //if (aRow >= RowBase) and (aCol = 10) then
    //  DrawLines(clWhite);
    if (aCol <> 0) and (aRow > (FRowBase-1)) then
      begin
        DrawLines(GetRowColor(aRow));

        if (gdSelected in aState) and (goRowSelect in Options)then
          begin
            Canvas.Pen.Width := 10;
            Canvas.Pen.Color := clWhite;
            if (aRow = Selection.Top) and (aCol = Selection.Left) then
              begin
                Canvas.PenPos := aRect.TopLeft;
                Canvas.LineTo(Point(aRect.Left,aRect.Bottom));
              end;
            Canvas.PenPos := aRect.BottomRight;
            Canvas.LineTo(Point(aRect.Left,aRect.Bottom));
            Canvas.PenPos := aRect.TopLeft;
            Canvas.LineTo(Point(aRect.Right,aRect.Top));

            if (aRow = Selection.Top) and (aCol = Selection.Right) then
              begin
                Canvas.PenPos := aRect.BottomRight;
                Canvas.LineTo(Point(aRect.Right,aRect.Top));
              end;
          end;
      end;

    Canvas.Pen.Width := 2;
    Canvas.Font.Size := 10;
    Canvas.Font.Color := clBlack;
    Canvas.Brush.Style := bsClear;

    if (aCol = 10) and (gdSelected in aState) and (goRowSelect in Options) then
      if (aRow = Selection.Top) and (aCol = Selection.Right) then
        begin
          if Assigned(FConfirmationButton) then
            begin
              LP := ClientToParent(Point(aRect.Right+8,aRect.Top+4));
              FConfirmationButton.Top := LP.Y;
              //FConfirmationButton.Left := LP.X;
            end;
        end;

  finally
    LoadOldCanvas;
    OldCanvas.Free;
  end;
end;

procedure TStringGridA.GridClick(Sender: TObject);
begin
  if goRowSelect in TStringGrid(Sender).Options then Exit;
  TStringGrid(Sender).Options := TStringGrid(Sender).Options+[goRowSelect];
  FConfirmationButton.Visible := True;
  //FPopupNotifier.Visible:= False;
end;

procedure TStringGridA.SetConfirmationButton(AValue: TButton);
begin
  if FConfirmationButton=AValue then Exit;
  FConfirmationButton:=AValue;
end;

procedure TStringGridA.SetDrawClearDots(AValue: Boolean);
begin
  if FDrawClearDots=AValue then Exit;
  FDrawClearDots:=AValue;
end;

procedure TStringGridA.SetDrawDots(AValue: Boolean);
begin
  if FDrawDots=AValue then Exit;
  FDrawDots:=AValue;
end;

procedure TStringGridA.SetHasCols(AValue: Boolean);
begin
  if FHasCols=AValue then Exit;
  FHasCols:=AValue;
  FRowBase := 1;
  FixedRows := 1;
  RowCount := 11;
  ColCount := 11;
  Options := [goFixedHorzLine, goHorzLine, goVertLine];
end;

procedure TStringGridA.SetHasRowsOnly(AValue: Boolean);
begin
  if FHasRows=AValue then Exit;
  FHasRows:=AValue;
  FRowBase := 0;
  FixedRows := 0;
  ColCount := 11;
  RowCount := 10;
  Options := [goFixedHorzLine, goHorzLine];
end;

//procedure TStringGridA.SetPopUpNotifier(AValue: TPopupNotifier);
//begin
//  if FPopUpNotifier=AValue then Exit;
//  FPopUpNotifier:=AValue;
//end;

procedure TStringGridA.UpdateHeight;
var
  i: Integer;
  LGridHeight: LongInt;
begin
  LGridHeight := 0;
  for i := 0 to RowCount -1 do
    begin
      RowHeights[i] := 45;
      LGridHeight += RowHeights[i];
    end;
  Height := LGridHeight+5;
end;

procedure TStringGridA.UpdateWidth;
var
  LGridWidth: LongInt;
  i: Integer;
begin
  LGridWidth := 0;
  for i := 0 to ColCount -1 do
    begin
      ColWidths[i] := 45;
      LGridWidth += ColWidths[i];
    end;
  Width  := LGridWidth+5;
end;

procedure TStringGridA.WriteGridFixedNames;
var
  i: integer;
begin
  if FHasRows then
    for i := 0 to RowCount-FRowBase-1 do
      Cells[0, i + FRowBase] := IntToStr(i + 1);

  if FHasCols then
    begin
    Cells[0,0] := '';
    for i := 0 to ColCount-2 do
      Cells[i+1, 0] := chr(65 + i);

    end;
end;

end.

