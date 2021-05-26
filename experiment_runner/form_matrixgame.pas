{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit form_matrixgame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ExtCtrls, PopupNotifier
  , game_actors
  , game_visual_matrix_a
  , game_visual_board
  , game_control
  ;

type

  { TFormMatrixGame }

  TFormMatrixGame = class(TForm)
    btnConfirmRow: TButton;
    ButtonCreateParticipantsFolder : TButton;
    ButtonExpCancel: TButton;
    ButtonExpPause: TButton;
    ButtonExpStart: TButton;
    GBLastChoice: TGroupBox;
    GBPoints: TGroupBox;
    GBAdmin: TGroupBox;
    ImageGroup2: TImage;
    ImageIndA: TImage;
    ImageInd: TImage;
    ImageIndB: TImage;
    ImageGroup1: TImage;
    LabelGroup2: TLabel;
    LabelGroup1Count: TLabel;
    LabelGroup2Count: TLabel;
    LabelOldPlayers: TLabel;
    LabelGroup1: TLabel;
    LabelInd: TLabel;
    LabelIndCount: TLabel;
    LabelIndACount: TLabel;
    LabelIndBCount: TLabel;
    LabelIndA: TLabel;
    LabelIndB: TLabel;
    ChatMemoRecv: TMemo;
    ChatMemoSend: TMemo;
    ChatPanel: TPanel;
    ChatSplitter: TSplitter;
    ListBoxOldPlayers: TListBox;
    OpenDialog: TOpenDialog;
    PopupNotifier: TPopupNotifier;
    Timer: TTimer;
    procedure btnConfirmRowClick(Sender: TObject);
    procedure ButtonCreateParticipantsFolderClick(Sender : TObject);
    procedure ButtonExpCancelClick(Sender: TObject);
    procedure ButtonExpPauseClick(Sender: TObject);
    procedure ButtonExpStartClick(Sender: TObject);
    procedure ChatMemoSendKeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
    procedure PopupNotifierClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure TimerTimer(Sender: TObject);
  private
    FInitParameter: string;
    FGameControl : TGameControl;
    FGameBoard : TGameBoard;
  public
    procedure LoadFromFile(FFilename : string);
    procedure SetParameters(P : string);
    procedure SetGameActor(AValue: TGameActor);
  end;

var
  FormMatrixGame: TFormMatrixGame;

resourcestring
  CAPTION_RESUME = 'Resume';
  CAPTION_START = 'Start';
  CAPTION_RUNNING = 'Running';

implementation

uses
  form_chooseactor
  , game_resources
  , helpers
  ;

{$R *.lfm}

{ TFormMatrixGame }

procedure TFormMatrixGame.TimerTimer(Sender: TObject);
begin
  Timer.Enabled:=False;
  PopupNotifier.Visible:=False;
end;

procedure TFormMatrixGame.LoadFromFile(FFilename: string);
begin
  if not FGameControl.LoadFromFile(FFilename) then
    Exit;

  ButtonExpStart.Enabled := False;
  ButtonExpStart.Caption := CAPTION_RUNNING;
  ButtonExpCancel.Enabled := not ButtonExpStart.Enabled;
  ButtonExpPause.Enabled := not ButtonExpStart.Enabled;
end;

procedure TFormMatrixGame.SetGameActor(AValue: TGameActor);
begin
  FGameControl := TGameControl.Create(Self, AValue);
  FGameBoard := TGameBoard.Create(
    Self,
    FGameControl,
    AValue,
    Self,
    btnConfirmRow,
    GBLastChoice,
    GBPoints,
    GBAdmin,
    PopupNotifier
  );

  FGameBoard.Chat := ChatMemoRecv.Lines;
  FGameBoard.ChatPanel := ChatPanel;
  FGameBoard.BackgroundForm := Self;
  FGameBoard.Timer := Timer;
  FGameBoard.TimerEvent := @TimerTimer;
  FGameBoard.ImagePointI := ImageInd;
  FGameBoard.LabelPointINAme := LabelInd;
  FGameBoard.LabelPointICount := LabelIndCount;
  FGameBoard.ListBoxOldPlayers := ListBoxOldPlayers;

  FGameBoard.ImagePointA := ImageIndA;
  FGameBoard.LabelPointAName := LabelIndA;
  FGameBoard.LabelPointACount := LabelIndACount;

  FGameBoard.ImagePointB := ImageIndB;
  FGameBoard.LabelPointBName := LabelIndB;
  FGameBoard.LabelPointBCount := LabelIndBCount;

  FGameBoard.ImageGroup1 := ImageGroup1;
  FGameBoard.LabelGroup1Name := LabelGroup1;
  FGameBoard.LabelGroup1Count := LabelGroup1Count;

  FGameBoard.ImageGroup2 := ImageGroup2;
  FGameBoard.LabelGroup2Name := LabelGroup2;
  FGameBoard.LabelGroup2Count := LabelGroup2Count;
  FGameBoard.InitialSetup;

  FGameControl.GameBoard := FGameBoard;
  FGameControl.Login;
end;


procedure TFormMatrixGame.SetParameters(P: string);
begin
  FInitParameter := P
end;


procedure TFormMatrixGame.FormActivate(Sender: TObject);
begin
  case FInitParameter of
    'a': FormMatrixGame.SetGameActor(gaAdmin);
    'p': FormMatrixGame.SetGameActor(gaPlayer);
    'w': FormMatrixGame.SetGameActor(gaWatcher);
    else
      if not Assigned(FGameControl) then begin
        FormChooseActor := TFormChooseActor.Create(nil);
        FormChooseActor.Style := '.Arrived';
        if FormChooseActor.ShowModal = 1 then begin
          case FormChooseActor.GameActor of
            gaAdmin: FormMatrixGame.SetGameActor(gaAdmin);
            gaPlayer: FormMatrixGame.SetGameActor(gaPlayer);
            gaWatcher: FormMatrixGame.SetGameActor(gaWatcher);
            else begin
              FormChooseActor.Free;
              Close;
              Exit;
            end;
          end;
        end else begin
          FormChooseActor.Free;
          Close;
          Exit;
        end;
        FormChooseActor.Free;
      end;
  end;
  FGameBoard.StringGridMatrix.Enabled := False;
  OnActivate:=nil;
end;

procedure TFormMatrixGame.PopupNotifierClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Timer.Enabled := False;
end;

procedure TFormMatrixGame.ChatMemoSendKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Char(13) then
    begin
      FGameControl.SendMessage(K_CHAT_M, [ChatMemoSend.Lines.Text]);
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
var
  S : TStringGridA;
begin
  btnConfirmRow.Enabled := False;
  S := TStringGridA(FGameBoard.StringGridMatrix);
  FGameControl.SendRequest(K_CHOICE, [S.GetSelectedRowF, S.GetSelectedMatrixColorF]);
end;

procedure TFormMatrixGame.ButtonCreateParticipantsFolderClick(Sender : TObject);
begin
  CreateDebugFoldersForPlayers;
end;

procedure TFormMatrixGame.ButtonExpCancelClick(Sender: TObject);
begin
  ButtonExpStart.Enabled := True;
  ButtonExpStart.Caption := CAPTION_START;
  ButtonExpCancel.Enabled := not ButtonExpStart.Enabled;
  ButtonExpPause.Enabled := not ButtonExpStart.Enabled;
  FGameBoard.StringGridMatrix.Clean;
  FGameBoard.StringGridMatrix.Options := [];
  FGameControl.Cancel;
end;

procedure TFormMatrixGame.ButtonExpPauseClick(Sender: TObject);
begin
  ShowMessage('Not implemented yet.');
end;

procedure TFormMatrixGame.ButtonExpStartClick(Sender: TObject);
begin
  OpenDialog.InitialDir:=ExtractFilePath(Application.ExeName)+VAL_RESEARCHERS;
  if ButtonExpStart.Caption = CAPTION_START then
    if OpenDialog.Execute then
      LoadFromFile(OpenDialog.FileName);

  if ButtonExpStart.Caption = CAPTION_RESUME then
    begin
      ButtonExpStart.Enabled := False;
      ButtonExpStart.Caption := CAPTION_RUNNING;
      ButtonExpCancel.Enabled := not ButtonExpStart.Enabled;
      ButtonExpPause.Enabled := not ButtonExpStart.Enabled;
    end;
end;

end.
