{   
  Free-mtrix - Free cultural selection and social behavior experiments.   
  Copyright (C) 2016-2019 Carlos Rafael Fernandes Picanço.   
  Copyright (C) 2016-2019 Thais Maria Monteiro Guimarães.   
  Copyright (C) 2016-2019 Universidade Federal do Pará.

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

  , game_zmq_actors
  , game_actors
  , game_control
  , game_visual_experiment
  ;

type

  { TFormMatrixGame }

  TFormMatrixGame = class(TForm)
    btnConfirmRow: TButton;
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
    procedure ButtonExpCancelClick(Sender: TObject);
    procedure ButtonExpPauseClick(Sender: TObject);
    procedure ButtonExpStartClick(Sender: TObject);
    procedure ChatMemoSendKeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PopupNotifierClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure TimerTimer(Sender: TObject);
  private
    FGameControl : TGameControl;
    FExperimentBox : TExperimentBox;
    FID: string;
    FInitParameter: string;
    procedure DisableConfirmationButton(Sender : TObject);
    procedure CleanMatrix(Sender: TObject; AEnabled : Boolean);
    procedure PlayerExit(P : TPlayer; AMessage : string);

  public
    StringGridMatrix: TStringGrid;
    procedure LoadFromFile(FFilename : string);
    procedure SetID(S, P : string);
    procedure SetGameActor(AValue: TGameActor);
    procedure SetFullscreen;
    property Game : TGameControl read FGameControl;
    property ID : string read FID;
  end;

var
  FormMatrixGame: TFormMatrixGame;

resourcestring
  CAPTION_RESUME = 'Recomeçar';
  CAPTION_START = 'Começar';
  CAPTION_RUNNING = 'Rodando';

implementation

uses form_chooseactor, game_resources, game_visual_matrix_a;

{$R *.lfm}

{ TFormMatrixGame }

procedure TFormMatrixGame.TimerTimer(Sender: TObject);
begin
  Timer.Enabled:=False;
  PopupNotifier.Visible:=False;
end;

procedure TFormMatrixGame.DisableConfirmationButton(Sender: TObject);
begin
  StringGridMatrix.Enabled:= False;
  btnConfirmRow.Enabled:=False;
  btnConfirmRow.Caption:='OK';
end;

procedure TFormMatrixGame.CleanMatrix(Sender: TObject; AEnabled: Boolean);
begin
  StringGridMatrix.Enabled:=AEnabled;
  StringGridMatrix.Options := FormMatrixGame.StringGridMatrix.Options-[goRowSelect];
  btnConfirmRow.Enabled:=True;
  btnConfirmRow.Caption:='Confirmar';
  btnConfirmRow.Visible := False;
end;

procedure TFormMatrixGame.PlayerExit(P: TPlayer; AMessage: string);
begin
  ListBoxOldPlayers.Items.Append(AMessage);
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

  procedure SetZMQAdmin;
  begin
    FGameControl := TGameControl.Create(TZMQAdmin.Create(Self,FID),ExtractFilePath(Application.ExeName));
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
  FGameControl.OnPlayerExit:=@PlayerExit;
  FGameControl.OnEndChoice:=@DisableConfirmationButton;
  FGameControl.OnCleanEvent:=@CleanMatrix;
  FGameControl.SystemPopUp := PopupNotifier;
  FGameControl.LabelGroup1 := LabelGroup1Count;
  FGameControl.LabelGroup2 := LabelGroup2Count;
  FGameControl.LabelPointA := LabelIndACount;
  FGameControl.LabelPointB := LabelIndBCount;
  FGameControl.LabelPointI := LabelIndCount;
  FGameControl.GroupBoxPlayers := GBLastChoice;
  FGameControl.OnInterlocking := @FExperimentBox.Interlocking;
  FGameControl.OnTargetInterlocking:= @FExperimentBox.TargetInterlocking;
  FGameControl.OnStartExperiment := @FExperimentBox.StartExperiment;
  FGameControl.OnStartTurn := @FExperimentBox.StartTurn;
  FGameControl.OnStartCycle := @FExperimentBox.StartCycle;
  FGameControl.OnStartGeneration:= @FExperimentBox.StartGeneration;
  FGameControl.OnStartCondition:= @FExperimentBox.StartCondition;
  FGameControl.OnEndExperiment :=@FExperimentBox.EndExperiment;
end;

procedure TFormMatrixGame.SetFullscreen;
begin
  BorderStyle:=bsNone;
  {$IFDEF WINDOWS}
  BoundsRect := Monitor.BoundsRect;
  {$ENDIF}
  Position:=poDesigned;
  FormStyle:=fsNormal;
  WindowState:=wsFullScreen;
end;

procedure TFormMatrixGame.SetID(S, P: string);
begin
  FID := S;
  FInitParameter := P
end;


procedure TFormMatrixGame.FormActivate(Sender: TObject);
begin
  StringGridMatrix.ClearSelections;
  StringGridMatrix.FocusRectVisible := False;
  btnConfirmRow.Visible := False;
  case FInitParameter of
    'a': FormMatrixGame.SetGameActor(gaAdmin);
    'p': FormMatrixGame.SetGameActor(gaPlayer);
    'w': FormMatrixGame.SetGameActor(gaWatcher);
    else
      if not Assigned(FGameControl) then
        begin
          FormChooseActor := TFormChooseActor.Create(Self);
          FormChooseActor.Style := '.Arrived';
          try
            if FormChooseActor.ShowModal = 1 then
              case FormChooseActor.GameActor of
                gaAdmin: FormMatrixGame.SetGameActor(gaAdmin);
                gaPlayer: FormMatrixGame.SetGameActor(gaPlayer);
                gaWatcher: FormMatrixGame.SetGameActor(gaWatcher);
              end
            else
              Close;
          finally
            FormChooseActor.Free;
          end;
        end
  end;
  OnActivate:=nil;
end;

procedure TFormMatrixGame.FormCreate(Sender: TObject);
var
  L : TLabel;
  LLeft : integer;
begin
  StringGridMatrix := TStringGridA.Create(Self);
  // TStringGridA(StringGridMatrix).PopUpNotifier := PopupNotifier;
  TStringGridA(StringGridMatrix).ConfirmationButton := btnConfirmRow;
  StringGridMatrix.Parent := Self;
  LLeft := StringGridMatrix.Width+btnConfirmRow.Width+GBPoints.Width+10;
  LLeft := LLeft div 2;
  LLeft := (Screen.Width div 2)-LLeft;
  StringGridMatrix.Left:=LLeft;

  btnConfirmRow.AnchorSideLeft.Control := StringGridMatrix;
  btnConfirmRow.AnchorSideLeft.Side := asrRight;
  btnConfirmRow.BorderSpacing.Left := 5;
  btnConfirmRow.Top := StringGridMatrix.Top;
  btnConfirmRow.Left := StringGridMatrix.BoundsRect.Right+5;
  GBPoints.Left := btnConfirmRow.Left+btnConfirmRow.Width+10;
  GBAdmin.Left := GBPoints.Left+GBPoints.Width+5;

  {$IFDEF DEBUG}
  with TPlayerBox.Create(GBLastChoice,'test') do
    Parent := GBLastChoice;
  {$ENDIF}

  FExperimentBox := TExperimentBox.Create(GBAdmin);
  FExperimentBox.Parent := GBAdmin;
  PopupNotifier.Icon.Assign(Application.Icon);
  PopupNotifier.Title:='';
  PopupNotifier.Text:='';
  // PopupNotifier.vNotifierForm.Font.Size:=12;
  L := TLabel.Create(FormMatrixGame.PopupNotifier.vNotifierForm);
  L.Name:='UglyHack';
  L.Align:=alClient;
  L.Anchors := [akLeft,akRight];
  L.Alignment := taCenter;
  L.AutoSize:=True;
  L.Layout := tlCenter;
  L.WordWrap := False;
  L.BorderSpacing.Top:=26;
  L.BorderSpacing.Left:=26;
  L.BorderSpacing.Right:=26;
  L.BorderSpacing.Bottom:=26;
  L.OnClick := PopupNotifier.vNotifierForm.OnClick;
  L.Parent := PopupNotifier.vNotifierForm;
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
  S := TStringGridA(StringGridMatrix);
  FGameControl.SendRequest(K_CHOICE, [S.GetSelectedRowF, S.GetSelectedMatrixColorF]);
end;

procedure TFormMatrixGame.ButtonExpCancelClick(Sender: TObject);
begin
  ButtonExpStart.Enabled := True;
  ButtonExpStart.Caption := CAPTION_START;
  ButtonExpCancel.Enabled := not ButtonExpStart.Enabled;
  ButtonExpPause.Enabled := not ButtonExpStart.Enabled;
  FGameControl.Experiment.SaveToFile(OpenDialog.FileName+'.canceled');
  StringGridMatrix.Clean;
  StringGridMatrix.Options := [];
  FGameControl.Cancel;
end;

procedure TFormMatrixGame.ButtonExpPauseClick(Sender: TObject);
begin
  ShowMessage('Não implementado.');
  //ButtonExpStart.Enabled := True;
  //ButtonExpStart.Caption := CAPTION_RESUME;
  //ButtonExpPause.Enabled := not ButtonExpStart.Enabled;
  //FGameControl.Pause;
end;

procedure TFormMatrixGame.ButtonExpStartClick(Sender: TObject);
begin
  OpenDialog.InitialDir:=ExtractFilePath(Application.ExeName)+VAL_RESEARCHERS;
  if ButtonExpStart.Caption = CAPTION_START then
    if OpenDialog.Execute then
      LoadFromFile(OpenDialog.FileName);
  //FGameControl.ShowSystemPopUp('É sua vez! Clique sobre uma linha da matriz e confirme sua escolha.',5000);
  if ButtonExpStart.Caption = CAPTION_RESUME then
    begin
      ButtonExpStart.Enabled := False;
      ButtonExpStart.Caption := CAPTION_RUNNING;
      ButtonExpCancel.Enabled := not ButtonExpStart.Enabled;
      ButtonExpPause.Enabled := not ButtonExpStart.Enabled;
      FGameControl.Resume;
    end;
end;

end.
