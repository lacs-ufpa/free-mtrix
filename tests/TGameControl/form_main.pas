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
    ButtonRandom: TButton;
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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelRandomBias: TLabel;
    LabelExperimentNotFound: TLabel;
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
    LabelRandomBias1: TLabel;
    LabelRandomBias2: TLabel;
    ListBoxExperiment: TListBox;
    ListBoxMessages: TListBox;
    ListBoxMessages1: TListBox;
    ListBoxMessages2: TListBox;
    ListBoxMessages3: TListBox;
    ListBoxReport: TListBox;
    ListBoxOldParticipants: TListBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    Timer1: TTimer;
    TrackBarRandomBias: TTrackBar;
    procedure ButtonChoice1Click(Sender: TObject);
    procedure ButtonLoginClick(Sender: TObject);
    procedure ButtonQY1Click(Sender: TObject);
    procedure CheckBoxAutoPlayChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
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
begin
  FExperimentBox := TExperimentBox.Create(Self);
  FExperimentBox.Left := 926;
  FExperimentBox.Top := 16;
  FExperimentBox.Parent := Self;

  FServer := TGameControl.Create(
    TZMQAdmin.Create(Self, TZMQActor.NewRandomID),
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
  LabelExperimentNotFound.Visible := not FServer.LoadFromFile('Experiment1.ini');
  FServer.LabelGroup1Count := LabelGroupCountServer;
  FServer.LabelPointA := LabelIndACountServer;
  FServer.LabelPointB := LabelIndBCountServer;
  FServer.LabelPointI := LabelIndCountServer;
  FServer.ImageGroup1 := ImageGroup;
  FServer.OnPlayerExit:=@PlayerExit;
  FServer.OnEndChoice:=@DisableConfirmationButton;
  FServer.OnCleanEvent:=@CleanMatrix;
  FServer.GroupBoxPlayers := GBLastChoiceServer;
  FServer.FallbackMessages := ListBoxMessages;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if chkP1.Checked xor chkP2.Checked xor chkP3.Checked then
    ButtonChoice1Click(Sender)
  else exit;
end;

procedure TForm1.ButtonLoginClick(Sender: TObject);
begin
  case GPlayers of
    0 :
      begin
        FPlayer1 := TGameControl.Create(
          TZMQPlayer.Create(Self,TZMQActor.NewRandomID));
        FPlayer1.GroupBoxPlayers := GBLastChoiceP1;
        FPlayer1.LabelGroup1Count := LabelGroupCountP1;
        FPlayer1.LabelPointA := LabelIndACountP1;
        FPlayer1.LabelPointB := LabelIndBCountP1;
        FPlayer1.LabelPointI := LabelIndCountP1;
        FPlayer1.ImageGroup1 := ImageGroup1;
        FPlayer1.OnPlayerExit:=@PlayerExit;
        FPlayer1.OnEndChoice:=@DisableConfirmationButton;
        FPlayer1.OnCleanEvent:=@CleanMatrix;
        FPlayer1.FallbackMessages := ListBoxMessages1;
      end;
    1 :
      begin
        FPlayer2 := TGameControl.Create(
          TZMQPlayer.Create(Self,TZMQActor.NewRandomID));
        FPlayer2.GroupBoxPlayers := GBLastChoiceP2;
        FPlayer2.LabelGroup1Count := LabelGroupCountP2;
        FPlayer2.LabelPointA := LabelIndACountP2;
        FPlayer2.LabelPointB := LabelIndBCountP2;
        FPlayer2.LabelPointI := LabelIndCountP2;
        FPlayer2.ImageGroup1 := ImageGroup2;
        FPlayer2.OnPlayerExit:=@PlayerExit;
        FPlayer2.OnEndChoice:=@DisableConfirmationButton;
        FPlayer2.OnCleanEvent:=@CleanMatrix;
        FPlayer2.FallbackMessages := ListBoxMessages2;
      end;
    2 :
      begin
        FPlayer3 := TGameControl.Create(
          TZMQPlayer.Create(Self,TZMQActor.NewRandomID));
        FPlayer3.GroupBoxPlayers := GBLastChoiceP3;
        FPlayer3.LabelGroup1Count := LabelGroupCountP3;
        FPlayer3.LabelPointA := LabelIndACountP3;
        FPlayer3.LabelPointB := LabelIndBCountP3;
        FPlayer3.LabelPointI := LabelIndCountP3;
        FPlayer3.ImageGroup1 := ImageGroup3;
        FPlayer3.OnPlayerExit:=@PlayerExit;
        FPlayer3.OnEndChoice:=@DisableConfirmationButton;
        FPlayer3.OnCleanEvent:=@CleanMatrix;
        FPlayer3.FallbackMessages := ListBoxMessages3;

        ButtonLogin.Enabled := False;
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
var
  LEnabled : Boolean;
begin
  LEnabled := Timer1.Enabled;
  Timer1.Enabled:= not LEnabled;
  ButtonChoice1.Enabled:=LEnabled;
  ButtonChoice2.Enabled:=LEnabled;
  ButtonChoice3.Enabled:=LEnabled;
  ButtonChoice4.Enabled:=LEnabled;
  ButtonChoice5.Enabled:=LEnabled;
  ButtonChoice6.Enabled:=LEnabled;
  ButtonChoice7.Enabled:=LEnabled;
  ButtonChoice8.Enabled:=LEnabled;
  ButtonChoice9.Enabled:=LEnabled;
  ButtonChoice10.Enabled:=LEnabled;
  ButtonRandom.Enabled:=LEnabled;
  if Timer1.Enabled then
  begin
    GLOBAL_MESSAGES_INTERVAL := 50;
    GLOBAL_MESSAGE_INTERVAL := 50;
    GLOBAL_SYSTEM_MESSAGE_INTERVAL := 50;
  end else begin
    GLOBAL_MESSAGES_INTERVAL := DEFAULT_GLOBAL_MESSAGES_INTERVAL;
    GLOBAL_MESSAGE_INTERVAL := DEFAULT_GLOBAL_MESSAGE_INTERVAL;
    GLOBAL_SYSTEM_MESSAGE_INTERVAL := DEFAULT_GLOBAL_SYSTEM_MESSAGE_INTERVAL;
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
  r : real;
  ri : integer;
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

  if (Sender = Timer1) or (Sender = ButtonRandom) then
  begin
    r := Random;
    if r < (TrackBarRandomBias.Position/100) then
      repeat
        ri := Random(10);
      until not Odd(ri)
    else
      repeat
        ri := Random(10);
      until Odd(ri);
    choice := choices[ri];
  end;

  if chkP1.Checked then
    FPlayer1.SendRequest(K_CHOICE, [choice.n,choice.c])
  else if chkP2.Checked then
    FPlayer2.SendRequest(K_CHOICE, [choice.n,choice.c])
  else if chkP3.Checked then
    FPlayer3.SendRequest(K_CHOICE, [choice.n,choice.c]);
end;

procedure TForm1.WriteReport(S: string);
begin
  ListBoxReport.Items.Append(S);
end;

procedure TForm1.PlayerExit(P: TPlayer; AMessage: string);
begin
  ListBoxOldParticipants.Items.Append(
    'ID:' + AMessage + LineEnding +
    'Name: '+ P.Nicname + LineEnding +
    'Red Tokens:' + P.Points.A.ToString + LineEnding +
    'Blue Tokens:' + P.Points.B.ToString
    );
end;

procedure TForm1.DisableConfirmationButton(Sender: TObject);
begin
  if Sender = FServer then
    begin
      ListBoxExperiment.Items.Append('Server.DisableConfirmationButton='+BoolToStr(False,True));
    end;

  if Sender = FPlayer1 then
    begin
      ListBoxExperiment.Items.Append('P1.DisableConfirmationButton='+BoolToStr(False,True));
      chkP1.Checked:=False;
    end;

  if Sender = FPlayer2 then
    begin
      ListBoxExperiment.Items.Append('P2.DisableConfirmationButton='+BoolToStr(False,True));
      chkP2.Checked:=False;
    end;

  if Sender = FPlayer3 then
    begin
      ListBoxExperiment.Items.Append('P3.DisableConfirmationButton='+BoolToStr(False,True));
      chkP3.Checked:=False;
    end;
end;

procedure TForm1.CleanMatrix(Sender: TObject; B: Boolean);
begin
  if Sender = FServer then
    begin
      ListBoxExperiment.Items.Append('Server.CleanMatrix='+BoolToStr(B,True));
    end;

  if Sender = FPlayer1 then
    begin
      ListBoxExperiment.Items.Append('P1.CleanMatrix='+BoolToStr(B,True));
      chkP1.Checked:=B;
    end;

  if Sender = FPlayer2 then
    begin
      ListBoxExperiment.Items.Append('P2.CleanMatrix='+BoolToStr(B,True));
      chkP2.Checked:=B;
    end;

  if Sender = FPlayer3 then
    begin
      ListBoxExperiment.Items.Append('P3.CleanMatrix='+BoolToStr(B,True));
      chkP3.Checked:=B;
    end;
end;


end.

