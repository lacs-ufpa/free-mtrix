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
  StdCtrls, DBGrids, ExtCtrls, PopupNotifier

  , game_zmq_actors
  , game_actors
  , game_control
  ;

type

  { TFormMatrixGame }

  TFormMatrixGame = class(TForm)
    btnConfirmRow: TButton;
    ButtonExpStart: TButton;
    ButtonExpPause: TButton;
    Button3: TButton;
    ButtonExpCancel: TButton;
    GBIndividual: TGroupBox;
    GBLastChoice: TGroupBox;
    GBIndividualAB: TGroupBox;
    GBGrupo: TGroupBox;
    GBAdmin: TGroupBox;
    GBExperiment: TGroupBox;
    LabelUnseen1: TLabel;
    LabelUnseen2: TLabel;
    LabelUnseen3: TLabel;
    LabelExpCountCondition: TLabel;
    LabelExpGen: TLabel;
    LabelExpCountGeneration: TLabel;
    LabelExpCycle: TLabel;
    LabelExpCountCycle: TLabel;
    LabelExpTurn: TLabel;
    LabelExpCountTurn: TLabel;
    LabelExpInterlocks: TLabel;
    LabelExpCountInterlocks: TLabel;
    LabelIndCount: TLabel;
    LabelIndACount: TLabel;
    LabelIndBCount: TLabel;
    LabelIndA: TLabel;
    LabelGroupCount: TLabel;
    LabelIndB: TLabel;
    LabelExpCond: TLabel;
    ChatMemoRecv: TMemo;
    ChatMemoSend: TMemo;
    ChatPanel: TPanel;
    ChatSplitter: TSplitter;
    OpenDialog: TOpenDialog;
    PopupNotifier: TPopupNotifier;
    StringGridMatrix: TStringGrid;
    Timer: TTimer;
    procedure btnConfirmRowClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ButtonExpCancelClick(Sender: TObject);
    procedure ButtonExpPauseClick(Sender: TObject);
    procedure ButtonExpStartClick(Sender: TObject);
    procedure ChatMemoSendKeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
    procedure PopupNotifierClose(Sender: TObject; var CloseAction: TCloseAction
      );
    procedure StringGridMatrixClick(Sender: TObject);
    procedure StringGridMatrixDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure TimerTimer(Sender: TObject);
  private
    FGameControl : TGameControl;
    FID: string;
  public
    procedure SetID(S : string);
    procedure SetGameActor(AValue: TGameActor);
    property ID : string read FID;
  end;

var
  FormMatrixGame: TFormMatrixGame;
resourcestring
  RS_RESEARCHERS = 'Pesquisadores';

implementation

uses form_chooseactor, game_resources;

// uses datamodule;

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

        if (gdSelected in aState) and (goRowSelect in TStringGrid(Sender).Options)then
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

    if (aCol = 10) and (gdSelected in aState) and (goRowSelect in TStringGrid(Sender).Options) then
      if (aRow = TStringGrid(Sender).Selection.Top) and (aCol = TStringGrid(Sender).Selection.Right) then
        begin
          btnConfirmRow.Top := aRect.Top+4;
          btnConfirmRow.Left := aRect.Right+8;
        end;

  finally
    LoadOldCanvas;
    OldCanvas.Free;
  end;
end;

procedure TFormMatrixGame.TimerTimer(Sender: TObject);
begin
  PopupNotifier.Visible:=False;
  Timer.Enabled := False;
end;

procedure TFormMatrixGame.SetGameActor(AValue: TGameActor);

  procedure SetZMQAdmin;
  begin
    FGameControl := TGameControl.Create(TZMQAdmin.Create(Self,FID));
    GBAdmin.Visible:= True;
  end;

  procedure SetZMQPlayer;
  begin
    FGameControl := TGameControl.Create(TZMQPlayer.Create(Self,FID));
    //StringGridMatrix.Enabled := True;
  end;

  procedure SetZMQWatcher;
  begin
    //FGameControl := TGameControl.Create(TZMQWatcher.Create(Self,FID));
  end;

begin
  case AValue of
    gaAdmin: SetZMQAdmin;
    gaPlayer: SetZMQPlayer;
    gaWatcher: SetZMQWatcher;
  end;
end;

procedure TFormMatrixGame.SetID(S: string);
begin
  FID := S;
end;


procedure TFormMatrixGame.FormActivate(Sender: TObject);
begin
  FormChooseActor := TFormChooseActor.Create(Self);
  FormChooseActor.Style := '.Arrived';
  try
    if FormChooseActor.ShowModal = 1 then
      begin
        case FormChooseActor.GameActor of
          gaAdmin:FormMatrixGame.SetGameActor(gaAdmin);
          gaPlayer: FormMatrixGame.SetGameActor(gaPlayer);
          gaWatcher: FormMatrixGame.SetGameActor(gaWatcher);
        end;
        StringGridMatrix.ClearSelections;
        StringGridMatrix.FocusRectVisible := False;
        FGameControl.SetMatrix;
      end
    else Close;
  finally
    FormChooseActor.Free;
  end;
end;

procedure TFormMatrixGame.PopupNotifierClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  // do nothing for now
end;

procedure TFormMatrixGame.StringGridMatrixClick(Sender: TObject);
begin
  if goRowSelect in StringGridMatrix.Options then Exit;
  StringGridMatrix.Options := StringGridMatrix.Options+[goRowSelect];
  btnConfirmRow.Visible := True;
end;

procedure TFormMatrixGame.ChatMemoSendKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Char(13) then
    begin
      FGameControl.SendMessage(K_CHAT_M);
      with ChatMemoSend do
        begin
          Clear;
          SelStart:=0;
          SelLength:=0;
          SetFocus;
        end;
      Key := Char(0);
    end;
end;

procedure TFormMatrixGame.btnConfirmRowClick(Sender: TObject);
begin
  FGameControl.SendMessage(K_CHOICE);
end;

procedure TFormMatrixGame.Button3Click(Sender: TObject);
begin
  FGameControl.Experiment.SaveToFile(OpenDialog.FileName+'.ini');
end;

procedure TFormMatrixGame.ButtonExpCancelClick(Sender: TObject);
begin
  ButtonExpStart.Enabled := True;
  ButtonExpStart.Caption := 'Começar';
  ButtonExpCancel.Enabled := not ButtonExpStart.Enabled;
  ButtonExpPause.Enabled := not ButtonExpStart.Enabled;
  //FGameControl.Experiment.SaveToFile(SaveDialog.FileName'.canceled');
  //FGameControl.Experiment.Clean;
end;

procedure TFormMatrixGame.ButtonExpPauseClick(Sender: TObject);
begin
  ButtonExpStart.Enabled := True;
  ButtonExpStart.Caption := 'Recomeçar';
  ButtonExpPause.Enabled := not ButtonExpStart.Enabled;
  //FGameControl.Experiment.Pause;
end;

procedure TFormMatrixGame.ButtonExpStartClick(Sender: TObject);
begin
  OpenDialog.InitialDir:=ExtractFilePath(Application.ExeName)+RS_RESEARCHERS;
  if ButtonExpStart.Caption = 'Começar' then
    if OpenDialog.Execute then
      begin
        ButtonExpStart.Enabled := False;
        ButtonExpStart.Caption := 'Rodando';
        ButtonExpCancel.Enabled := not ButtonExpStart.Enabled;
        ButtonExpPause.Enabled := not ButtonExpStart.Enabled;
        //FGameControl.Experiment.LoadFromFile(OpenDialog.FileName);
      end;

  if ButtonExpStart.Caption = 'Recomeçar' then
      begin
        ButtonExpStart.Enabled := False;
        ButtonExpStart.Caption := 'Rodando';
        ButtonExpCancel.Enabled := not ButtonExpStart.Enabled;
        ButtonExpPause.Enabled := not ButtonExpStart.Enabled;
        //FGameControl.Experiment.Resume;
      end;
end;

end.
