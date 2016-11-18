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
  ;

type

  { TFormMatrixGame }

  TFormMatrixGame = class(TForm)
    btnConfirmRow: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBoxDrawDots: TCheckBox;
    GBLastChoice: TGroupBox;
    GBIndividualPoints: TGroupBox;
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
    Label21: TLabel;
    Label22: TLabel;
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
    rgMatrixType: TRadioGroup;
    ChatSplitter: TSplitter;
    StringGridMatrix: TStringGrid;
    procedure btnConfirmRowClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ChatMemoSendKeyPress(Sender: TObject; var Key: char);
    procedure CheckBoxDrawDotsChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgMatrixTypeClick(Sender: TObject);
    procedure StringGridMatrixBeforeSelection(Sender: TObject; aCol, aRow: integer);
    procedure StringGridMatrixDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
  private
    //FZMQAdmin : TZMQAdmin;
    FZMQActor : TZMQActor;
    FGameActor : TGameActor;
    //FGameActors : TGameActors;
    function GetSelectedColorF(AStringGrid : TStringGrid) : UTF8string;
    function GetSelectedRowF(AStringGrid : TStringGrid) : UTF8string;
    function GetRowColor(ARow : integer) : TColor;
    procedure SetGameActor(AValue: TGameActor);
    procedure MessageReceived(AMessage : TStringList);
  public
    property GameActor : TGameActor read FGameActor write SetGameActor;
  end;

var
  FormMatrixGame: TFormMatrixGame;

implementation

uses LCLType, game_resources;

// uses datamodule;
var
  RowBase: integer = 0;
  MustDrawSelection : Boolean; // work around until a bug fix for ClearSelection is released

{$R *.lfm}

{ TFormMatrixGame }

procedure TFormMatrixGame.StringGridMatrixDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  OldCanvas: TCanvas;

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
    if CheckBoxDrawDots.Checked then
      if (Odd(aRow + RowBase) and not Odd(aCol)) or
        (not Odd(aRow + RowBase) and Odd(aCol)) then
        DrawDots;
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

    if (aCol <> 0) and (aRow > (RowBase-1)) then
      begin
        DrawLines(GetRowColor(aRow));

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

function TFormMatrixGame.GetSelectedColorF(AStringGrid : TStringGrid): UTF8string;
var LColor : TColor;
begin
  LColor := GetRowColor(AStringGrid.Selection.Top);
  case LColor of
    ccYellow: Result := 'Y';
    ccBlue : Result := 'B';
    ccGreen: Result := 'G';
    ccRed: Result := 'R';
    ccMagenta: Result := 'M';
  end;
end;

function TFormMatrixGame.GetSelectedRowF(AStringGrid: TStringGrid): UTF8string;
begin
  Result := IntToStr(AStringGrid.Selection.Top);
end;

function TFormMatrixGame.GetRowColor(ARow: integer): TColor;
var LRow : integer;
begin
  if RowBase = 1 then
    LRow := aRow -1
  else LRow := aRow;

  case LRow of
    0,5 :Result := ccYellow;
    1,6 :Result := ccGreen;
    2,7 :Result := ccRed;
    3,8 :Result := ccBlue;
    4,9 :Result := ccMagenta;
  end;
end;

procedure TFormMatrixGame.SetGameActor(AValue: TGameActor);

  procedure SetZMQAdmin;
  begin
    FZMQActor := TZMQAdmin.Create(Self);
    GBAdmin.Visible:= True;
  end;

  procedure SetZMQPlayer;
  begin
    FZMQActor := TZMQPlayer.Create(Self);
    btnConfirmRow.Visible := True;
    StringGridMatrix.Enabled := True;
  end;

  procedure SetZMQWatcher;
  begin
    FZMQActor := TZMQWatcher.Create(Self);
  end;

begin
  if FGameActor=AValue then Exit;
  FGameActor:=AValue;

  case FGameActor of
    gaAdmin: SetZMQAdmin;
    gaPlayer: SetZMQPlayer;
    gaWatcher: SetZMQWatcher;
  end;

  FZMQActor.OnMessageReceived:=@MessageReceived;
  FZMQActor.Start;
end;

procedure TFormMatrixGame.MessageReceived(AMessage: TStringList);

  procedure SendChat;
  begin
    ChatMemoRecv.Lines.Append(('['+AMessage[1]+']: ')+AMessage[2]);
  end;

  procedure PlayerChoice;
  begin

  end;

  procedure PlayerArrived;
  begin
    WriteLn('arrived');
  end;

  procedure PlayerLogin;
  begin
    WriteLn('login');
  end;

  procedure PlayerLogout;
  begin
    WriteLn('logout');
  end;
begin
  case AMessage[0] of
    'Player.Choice' : PlayerChoice;
    'Player.Arrived' : PlayerArrived;
    'Player.Login' : PlayerLogin;
    'Player.Logout': PlayerLogout;
    'Player.SendChat','Admin.SendChat': SendChat;
  end;
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


procedure TFormMatrixGame.FormCreate(Sender: TObject);
begin
  GameActor := gaNone;
end;

procedure TFormMatrixGame.rgMatrixTypeClick(Sender: TObject);

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
      StringGridMatrix.Height:=305;
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
      StringGridMatrix.Height:=335;
      StringGridMatrix.Options := [goFixedHorzLine, goHorzLine, goDrawFocusSelected, goRowSelect, goVertLine];
      RowBase := 1;
      WriteGridFixedNames(StringGridMatrix, True);
    end;
  end;

end;

procedure TFormMatrixGame.StringGridMatrixBeforeSelection(Sender: TObject; aCol, aRow: integer);
begin
  if MustDrawSelection then Exit;
  MustDrawSelection := True;
end;

procedure TFormMatrixGame.Button1Click(Sender: TObject);
begin

end;

procedure TFormMatrixGame.Button3Click(Sender: TObject);
begin
  //S := TStringList.Create;
  //S.Add('Player.Arrived');
  //S.Add(TZMQAdmin(FZMQActor).ID);
  TZMQAdmin(FZMQActor).SendMessage(['Player.Arrived', TZMQAdmin(FZMQActor).ID]);
  //S.Free;
end;

procedure TFormMatrixGame.ChatMemoSendKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Char(VK_RETURN) then
    begin
      if FZMQActor.ClassType = TZMQAdmin then
        TZMQAdmin(FZMQActor).SendMessage(['Admin.SendChat', CPlayerNamesMale[0], ChatMemoSend.Lines.Text]);

      if FZMQActor.ClassType = TZMQPlayer then
        TZMQPlayer(FZMQActor).SendMessage(['Player.SendChat', CPlayerNamesFemale[0], ChatMemoSend.Lines.Text]);

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
  if FZMQActor.ClassType = TZMQPlayer then
    begin
      //StringGridMatrix.ClearSelections;
      //MustDrawSelection := False;
      StringGridMatrix.Enabled:= False;
      btnConfirmRow.Visible:=False;
      TZMQPlayer(FZMQActor).SendMessage(['Player.Choice',
                                         TZMQPlayer(FZMQActor).ID,
                                         GetSelectedRowF(StringGridMatrix),
                                         GetSelectedColorF(StringGridMatrix)]);
    end;
end;

end.
