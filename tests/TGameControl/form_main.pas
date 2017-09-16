{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, PopupNotifier
  , game_visual_experiment
  , game_control
  , game_actors
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonQY1: TButton;
    ButtonQY2: TButton;
    ButtonChoice1: TButton;
    ButtonChoice10: TButton;
    ButtonChoice3: TButton;
    ButtonChoice4: TButton;
    ButtonChoice5: TButton;
    ButtonChoice6: TButton;
    ButtonChoice7: TButton;
    ButtonChoice8: TButton;
    ButtonChoice9: TButton;
    ButtonLogin: TButton;
    ButtonChoice2: TButton;
    ButtonQY3: TButton;
    ButtonQN1: TButton;
    ButtonQN2: TButton;
    ButtonQN3: TButton;
    CheckBoxAutoPlay: TCheckBox;
    chkP1: TCheckBox;
    chkP2: TCheckBox;
    chkP3: TCheckBox;
    GBLastChoiceServer: TGroupBox;
    GBLastChoiceP1: TGroupBox;
    GBLastChoiceP2: TGroupBox;
    GBLastChoiceP3: TGroupBox;
    GBPoints: TGroupBox;
    GBPoints1: TGroupBox;
    GBPoints2: TGroupBox;
    GBPoints3: TGroupBox;
    ImageGroup: TImage;
    ImageGroup1: TImage;
    ImageGroup2: TImage;
    ImageGroup3: TImage;
    ImageInd: TImage;
    ImageInd1: TImage;
    ImageInd2: TImage;
    ImageInd3: TImage;
    ImageIndA: TImage;
    ImageIndA1: TImage;
    ImageIndA2: TImage;
    ImageIndA3: TImage;
    ImageIndB: TImage;
    ImageIndB1: TImage;
    ImageIndB2: TImage;
    ImageIndB3: TImage;
    LabelGroup: TLabel;
    LabelGroup1: TLabel;
    LabelGroup2: TLabel;
    LabelGroup3: TLabel;
    LabelGroupCountServer: TLabel;
    LabelGroupCountP1: TLabel;
    LabelGroupCountP2: TLabel;
    LabelGroupCountP3: TLabel;
    LabelInd: TLabel;
    LabelInd1: TLabel;
    LabelInd2: TLabel;
    LabelInd3: TLabel;
    LabelIndA: TLabel;
    LabelIndA1: TLabel;
    LabelIndA2: TLabel;
    LabelIndA3: TLabel;
    LabelIndACountServer: TLabel;
    LabelIndACountP1: TLabel;
    LabelIndACountP2: TLabel;
    LabelIndACountP3: TLabel;
    LabelIndB: TLabel;
    LabelIndB1: TLabel;
    LabelIndB2: TLabel;
    LabelIndB3: TLabel;
    LabelIndBCountServer: TLabel;
    LabelIndBCountP1: TLabel;
    LabelIndBCountP2: TLabel;
    LabelIndBCountP3: TLabel;
    LabelIndCountServer: TLabel;
    LabelIndCountP1: TLabel;
    LabelIndCountP2: TLabel;
    LabelIndCountP3: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Timer1: TTimer;
    procedure ButtonChoice1Click(Sender: TObject);
    procedure ButtonLoginClick(Sender: TObject);
    procedure ButtonQY1Click(Sender: TObject);
    procedure CheckBoxAutoPlayChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FPopupNotifierServer : TPopupNotifier;
    FPopupNotifierP1 : TPopupNotifier;
    FPopupNotifierP2 : TPopupNotifier;
    FPopupNotifierP3 : TPopupNotifier;
    FServer : TGameControl;
    FPlayer1 : TGameControl;
    FPlayer2 : TGameControl;
    FPlayer3 : TGameControl;
    FExperimentBox : TExperimentBox;
    procedure WriteReport(S: string);
    procedure PlayerExit(P : TPlayer; AMessage:string);
    procedure DisableConfirmationButton(Sender : TObject);
    procedure CleanMatrix(Sender : TObject; B : Boolean);
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  GPlayers : integer = 0;

implementation

uses
  game_actors_helpers
  , game_zmq_actors
  , game_resources
  ;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  L : TLabel;
begin
  FPopupNotifierServer := TPopupNotifier.Create(Self);
  L := TLabel.Create(FPopupNotifierServer.vNotifierForm);
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
  L.OnClick := FPopupNotifierServer.vNotifierForm.OnClick;
  L.Parent := FPopupNotifierServer.vNotifierForm;
  FPopupNotifierP1 := TPopupNotifier.Create(Self);
  L := TLabel.Create(FPopupNotifierP1.vNotifierForm);
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
  L.OnClick := FPopupNotifierP1.vNotifierForm.OnClick;
  L.Parent := FPopupNotifierP1.vNotifierForm;
  FPopupNotifierP2 := TPopupNotifier.Create(Self);
  L := TLabel.Create(FPopupNotifierP2.vNotifierForm);
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
  L.OnClick := FPopupNotifierP2.vNotifierForm.OnClick;
  L.Parent := FPopupNotifierP2.vNotifierForm;
  FPopupNotifierP3 := TPopupNotifier.Create(Self);
  L := TLabel.Create(FPopupNotifierP3.vNotifierForm);
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
  L.OnClick := FPopupNotifierP3.vNotifierForm.OnClick;
  L.Parent := FPopupNotifierP3.vNotifierForm;
  FExperimentBox := TExperimentBox.Create(Self);
  FExperimentBox.Left := 926;
  FExperimentBox.Top := 16;
  FExperimentBox.Parent := Self;

  FServer := TGameControl.Create(TZMQAdmin.Create(Self,
    Format('%12X-%12X',[Random($1000000000000), Random($1000000000000)])),
    ExtractFilePath(Application.ExeName));

  FServer.OnInterlocking := @FExperimentBox.Interlocking;
  FServer.OnTargetInterlocking:= @FExperimentBox.TargetInterlocking;
  FServer.OnStartExperiment := @FExperimentBox.StartExperiment;
  FServer.OnStartTurn := @FExperimentBox.StartTurn;
  FServer.OnStartCycle := @FExperimentBox.StartCycle;
  FServer.OnStartGeneration:= @FExperimentBox.StartGeneration;
  FServer.OnStartCondition:= @FExperimentBox.StartCondition;
  FServer.OnEndExperiment :=@FExperimentBox.EndExperiment;
  FServer.Experiment.OnWriteReport:=@WriteReport;
  FServer.LoadFromFile('/home/rafael/git/free-mtrix/experiment_runner/Pesquisadores/teste_2_estudo_2_mc3.ini');
  FServer.LabelGroup := LabelGroupCountServer;
  FServer.LabelPointA := LabelIndACountServer;
  FServer.LabelPointB := LabelIndBCountServer;
  FServer.LabelPointI := LabelIndCountServer;
  FServer.OnPlayerExit:=@PlayerExit;
  FServer.OnEndChoice:=@DisableConfirmationButton;
  FServer.OnCleanEvent:=@CleanMatrix;
  FServer.SystemPopUp := FPopupNotifierServer;
  FServer.GroupBoxPlayers := GBLastChoiceServer;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if chkP1.Checked xor chkP2.Checked xor chkP3.Checked then
    ButtonChoice1Click(Sender)
  else exit;
end;

procedure TForm1.ButtonLoginClick(Sender: TObject);
var
  ID: string;
begin
  ID := Format('%12X-%12X',[Random($1000000000000), Random($1000000000000)]);
  case GPlayers of
    0 :
      begin
        FPlayer1 := TGameControl.Create(TZMQPlayer.Create(Self,ID));
        FPlayer1.GroupBoxPlayers := GBLastChoiceP1;
        FPlayer1.LabelGroup := LabelGroupCountP1;
        FPlayer1.LabelPointA := LabelIndACountP1;
        FPlayer1.LabelPointB := LabelIndBCountP1;
        FPlayer1.LabelPointI := LabelIndCountP1;
        FPlayer1.OnPlayerExit:=@PlayerExit;
        FPlayer1.OnEndChoice:=@DisableConfirmationButton;
        FPlayer1.OnCleanEvent:=@CleanMatrix;
        FPlayer1.SystemPopUp := FPopupNotifierP1;
      end;
    1 :
      begin
        FPlayer2 := TGameControl.Create(TZMQPlayer.Create(Self,ID));
        FPlayer2.GroupBoxPlayers := GBLastChoiceP2;
        FPlayer2.LabelGroup := LabelGroupCountP2;
        FPlayer2.LabelPointA := LabelIndACountP2;
        FPlayer2.LabelPointB := LabelIndBCountP2;
        FPlayer2.LabelPointI := LabelIndCountP2;
        FPlayer2.OnPlayerExit:=@PlayerExit;
        FPlayer2.OnEndChoice:=@DisableConfirmationButton;
        FPlayer2.OnCleanEvent:=@CleanMatrix;
        FPlayer2.SystemPopUp := FPopupNotifierP2;
      end;
    2 :
      begin
        FPlayer3 := TGameControl.Create(TZMQPlayer.Create(Self,ID));
        FPlayer3.GroupBoxPlayers := GBLastChoiceP3;
        FPlayer3.LabelGroup := LabelGroupCountP3;
        FPlayer3.LabelPointA := LabelIndACountP3;
        FPlayer3.LabelPointB := LabelIndBCountP3;
        FPlayer3.LabelPointI := LabelIndCountP3;
        FPlayer3.OnPlayerExit:=@PlayerExit;
        FPlayer3.OnEndChoice:=@DisableConfirmationButton;
        FPlayer3.OnCleanEvent:=@CleanMatrix;
        FPlayer3.SystemPopUp := FPopupNotifierP3;
      end;
    else
      Exit;
  end;
  Inc(GPlayers);
end;

procedure TForm1.ButtonQY1Click(Sender: TObject);
begin
  if Sender = ButtonQY1 then
    begin

    end;

  if Sender = ButtonQY2 then
    begin

    end;

  if Sender = ButtonQY3 then
    begin

    end;

  if Sender = ButtonQN1 then
    begin

    end;

  if Sender = ButtonQN2 then
    begin

    end;

  if Sender = ButtonQN3 then
    begin

    end;

end;

procedure TForm1.CheckBoxAutoPlayChange(Sender: TObject);
begin
  Timer1.Enabled:= not Timer1.Enabled;
  if Timer1.Enabled then
  begin
    GLOBAL_MESSAGE_INTERVAL := 30;
    GLOBAL_SYSTEM_MESSAGE_INTERVAL := 50;
    GLOBAL_MESSAGES_INTERVAL := 22;
  end
  else
  begin
    GLOBAL_MESSAGE_INTERVAL := 3000;
    GLOBAL_SYSTEM_MESSAGE_INTERVAL := 5000;
    GLOBAL_MESSAGES_INTERVAL := 2200;
  end;
end;

type TTestChoice = record
  n : string;
  c : string;
end;

procedure TForm1.ButtonChoice1Click(Sender: TObject);
const
  choices : array [0..9] of
    TTestChoice = (
      (n : '1';  c : 'Y'),
      (n : '2';  c : 'R'),
      (n : '3';  c : 'G'),
      (n : '4';  c : 'B'),
      (n : '5';  c : 'M'),
      (n : '6';  c : 'Y'),
      (n : '7';  c : 'R'),
      (n : '8';  c : 'G'),
      (n : '9';  c : 'B'),
      (n : '10'; c : 'M')
    );

var
  choice : TTestChoice;
begin
  if Sender = ButtonChoice1 then choice := choices[0];
  if Sender = ButtonChoice2 then choice := choices[1];
  if Sender = ButtonChoice3 then choice := choices[2];
  if Sender = ButtonChoice4 then choice := choices[3];
  if Sender = ButtonChoice5 then choice := choices[4];
  if Sender = ButtonChoice6 then choice := choices[5];
  if Sender = ButtonChoice7 then choice := choices[6];
  if Sender = ButtonChoice8 then choice := choices[7];
  if Sender = ButtonChoice9 then choice := choices[8];
  if Sender = ButtonChoice10 then choice := choices[9];
  if Sender = Timer1 then choice := choices[Random(10)];

  if chkP1.Checked then
    FPlayer1.SendRequest(K_CHOICE, [choice.n,choice.c])
  else if chkP2.Checked then
    FPlayer2.SendRequest(K_CHOICE, [choice.n,choice.c])
  else if chkP3.Checked then
    FPlayer3.SendRequest(K_CHOICE, [choice.n,choice.c]);
end;

procedure TForm1.WriteReport(S: string);
begin
  ListBox2.Items.Append(S);
end;

procedure TForm1.PlayerExit(P: TPlayer; AMessage: string);
begin
  ListBox1.Items.Append(AMessage);
end;

procedure TForm1.DisableConfirmationButton(Sender: TObject);
begin
  if Sender = FServer then
    begin
      ListBox1.Items.Append('ServerMatrixB='+BoolToStr(False,True));
    end;

  if Sender = FPlayer1 then
    begin
      ListBox1.Items.Append('P1MatrixB='+BoolToStr(False,True));
      chkP1.Checked:=False;
    end;

  if Sender = FPlayer2 then
    begin
      ListBox1.Items.Append('P2MatrixB='+BoolToStr(False,True));
      chkP2.Checked:=False;
    end;

  if Sender = FPlayer3 then
    begin
      ListBox1.Items.Append('P3MatrixB='+BoolToStr(False,True));
      chkP3.Checked:=False;
    end;
end;

procedure TForm1.CleanMatrix(Sender: TObject; B: Boolean);
begin
  if Sender = FServer then
    begin
      ListBox1.Items.Append('ServerMatrix='+BoolToStr(B,True));
    end;

  if Sender = FPlayer1 then
    begin
      ListBox1.Items.Append('P1Matrix='+BoolToStr(B,True));
      chkP1.Checked:=B;
    end;

  if Sender = FPlayer2 then
    begin
      ListBox1.Items.Append('P2Matrix='+BoolToStr(B,True));
      chkP2.Checked:=B;
    end;

  if Sender = FPlayer3 then
    begin
      ListBox1.Items.Append('P3Matrix='+BoolToStr(B,True));
      chkP3.Checked:=B;
    end;
end;


end.

