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
  ComCtrls, ExtCtrls, IniPropStorage
  , game_control
  , game_visual_board
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
    ImagePlayer1Group2 : TImage;
    ImagePlayer2Group2 : TImage;
    ImagePlayer3Group2 : TImage;
    ImageServerGroup1: TImage;
    ImagePlayer1Group1: TImage;
    ImagePlayer2Group1: TImage;
    ImagePlayer3Group1: TImage;
    ImageServerGroup2 : TImage;
    ImageServerA: TImage;
    ImagePlayer1A: TImage;
    ImagePlayer2A: TImage;
    ImagePlayer3A: TImage;
    ImageServerB: TImage;
    ImagePlayer1B: TImage;
    ImagePlayer2B: TImage;
    ImagePlayer3B: TImage;
    IniPropStorage1 : TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelPlayer1Group2Count : TLabel;
    LabelPlayer1Group2Name : TLabel;
    LabelPlayer2Group2Count : TLabel;
    LabelPlayer2Group2Name : TLabel;
    LabelPlayer3Group2Count : TLabel;
    LabelPlayer3Group2Name : TLabel;
    LabelRandomBias: TLabel;
    LabelExperimentNotFound: TLabel;
    LabelServerGroup2Count : TLabel;
    LabelServerGroup1Name: TLabel;
    LabelPlayer1Group1Name: TLabel;
    LabelPlayer2Group1Name: TLabel;
    LabelPlayer3Group1Name: TLabel;
    LabelServerGroup1Count: TLabel;
    LabelPlayer1Group1Count: TLabel;
    LabelPlayer2Group1Count: TLabel;
    LabelPlayer3Group1Count: TLabel;
    LabelServerGroup2Name : TLabel;
    LabelServerAName: TLabel;
    LabelPlayer1AName: TLabel;
    LabelPlayer2AName: TLabel;
    LabelPlayer3AName: TLabel;
    LabelServerACount: TLabel;
    LabelPlayer1ACount: TLabel;
    LabelPlayer2ACount: TLabel;
    LabelPlayer3ACount: TLabel;
    LabelServerBName: TLabel;
    LabelPlayer1BName: TLabel;
    LabelPlayer2BName: TLabel;
    LabelPlayer3BName: TLabel;
    LabelServerBCount: TLabel;
    LabelPlayer1BCount: TLabel;
    LabelPlayer2BCount: TLabel;
    LabelPlayer3BCount: TLabel;
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
    FServerGameControl : TGameControl;
    FPlayer1GameControl : TGameControl;
    FPlayer2GameControl : TGameControl;
    FPlayer3GameControl : TGameControl;

    FServerGameBoard   : TGameBoard;
    FPlayer1GameBoard : TGameBoard;
    FPlayer2GameBoard : TGameBoard;
    FPlayer3GameBoard : TGameBoard;
    procedure WriteReport(S: string);
    procedure PlayerExit(P : TPlayer; AMessage:string);
    procedure EndChoice(Sender : TObject);
    procedure StartChoice(Sender : TObject);
    procedure WaitForServer(Sender : TObject);
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  GPlayers : integer = 0;

implementation

uses
  game_actors_helpers
  , game_resources
  , game_report
  ;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FStart := GetTickCount64;
  FServerGameControl := TGameControl.Create(Self,gaAdmin);
  FServerGameControl.Experiment.Report.OnWriteReport:=@WriteReport;
  LabelExperimentNotFound.Visible := not FServerGameControl.LoadFromFile('Experiment1.ini');

  FServerGameBoard := TGameBoard.Create(
    Self,FServerGameControl,gaAdmin,nil, nil,GBLastChoiceServer,nil,nil,nil);
  with FServerGameBoard do
    begin
      OnPlayerExit := @Self.PlayerExit;
      OnEndChoice := @Self.EndChoice;
      OnStartChoice := @Self.StartChoice;
      OnWaitForServer := @Self.WaitForServer;

      Chat := nil;
      ChatPanel := nil;
      Timer := nil;
      TimerEvent := nil;
      ImagePointI := nil;
      LabelPointIName := nil;
      LabelPointICount := nil;
      ListBoxOldPlayers := ListBoxOldParticipants;

      ImagePointA := ImageServerA;
      LabelPointAName := LabelServerAName;
      LabelPointACount := LabelServerACount;

      ImagePointB := ImageServerB;
      LabelPointBName := LabelServerBName;
      LabelPointBCount := LabelServerBCount;

      ImageGroup1 := ImageServerGroup1;
      LabelGroup1Name := LabelServerGroup1Name;
      LabelGroup1Count := LabelServerGroup1Count;

      ImageGroup2 := ImageServerGroup2;
      LabelGroup2Name := LabelServerGroup2Name;
      LabelGroup2Count := LabelServerGroup2Count;
      FallbackMessages := ListBoxMessages;
      BeforeStartExperimentSetup;
    end;

  FServerGameBoard.GroupBoxExperiment.Parent := Self;
  FServerGameBoard.GroupBoxExperiment.Left :=
    PageControl1.ClientRect.Right+10;
  FServerGameBoard.GroupBoxExperiment.Top := 16;

  FServerGameControl.GameBoard := FServerGameBoard;
  FServerGameControl.Login;
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
        FPlayer1GameControl := TGameControl.Create(Self,gaPlayer);
        FPlayer1GameBoard := TGameBoard.Create(
          Self,FPlayer1GameControl,gaPlayer,nil, nil,GBLastChoiceP1,nil,nil,nil);
        with FPlayer1GameBoard do
          begin
            OnPlayerExit := @Self.PlayerExit;
            OnEndChoice := @Self.EndChoice;
            OnStartChoice := @Self.StartChoice;
            OnWaitForServer := @Self.WaitForServer;

            Chat := nil;
            ChatPanel := nil;
            Timer := nil;
            TimerEvent := nil;
            ImagePointI := nil;
            LabelPointIName := nil;
            LabelPointICount := nil;
            ListBoxOldPlayers := nil;

            ImagePointA := ImagePlayer1A;
            LabelPointAName := LabelPlayer1AName;
            LabelPointACount := LabelPlayer1ACount;

            ImagePointB := ImagePlayer1B;
            LabelPointBName := LabelPlayer1BName;
            LabelPointBCount := LabelPlayer1BCount;

            ImageGroup1 := ImagePlayer1Group1;
            LabelGroup1Name := LabelPlayer1Group1Name;
            LabelGroup1Count := LabelPlayer1Group1Count;

            ImageGroup2 := ImagePlayer1Group2;
            LabelGroup2Name := LabelPlayer1Group2Name;
            LabelGroup2Count := LabelPlayer1Group2Count;
            FallbackMessages := ListBoxMessages1;
            BeforeStartExperimentSetup;
          end;
        FPlayer1GameControl.GameBoard := FPlayer1GameBoard;
        FPlayer1GameControl.Login;
      end;
    1 :
      begin
        FPlayer2GameControl := TGameControl.Create(Self,gaPlayer);
        FPlayer2GameBoard := TGameBoard.Create(
          Self,FPlayer2GameControl,gaPlayer,nil, nil,GBLastChoiceP2,nil,nil,nil);
        with FPlayer2GameBoard do
          begin
            OnPlayerExit := @Self.PlayerExit;
            OnEndChoice := @Self.EndChoice;
            OnStartChoice := @Self.StartChoice;
            OnWaitForServer := @Self.WaitForServer;

            Chat := nil;
            ChatPanel := nil;
            Timer := nil;
            TimerEvent := nil;
            ImagePointI := nil;
            LabelPointIName := nil;
            LabelPointICount := nil;
            ListBoxOldPlayers := nil;

            ImagePointA := ImagePlayer2A;
            LabelPointAName := LabelPlayer2AName;
            LabelPointACount := LabelPlayer2ACount;

            ImagePointB := ImagePlayer2B;
            LabelPointBName := LabelPlayer2BName;
            LabelPointBCount := LabelPlayer2BCount;

            ImageGroup1 := ImagePlayer2Group1;
            LabelGroup1Name := LabelPlayer2Group1Name;
            LabelGroup1Count := LabelPlayer2Group1Count;

            ImageGroup2 := ImagePlayer2Group2;
            LabelGroup2Name := LabelPlayer2Group2Name;
            LabelGroup2Count := LabelPlayer2Group2Count;
            FallbackMessages := ListBoxMessages2;
            BeforeStartExperimentSetup;
          end;
        FPlayer2GameControl.GameBoard := FPlayer2GameBoard;
        FPlayer2GameControl.Login;
      end;
    2 :
      begin
        FPlayer3GameControl := TGameControl.Create(Self,gaPlayer);
        FPlayer3GameBoard := TGameBoard.Create(
          Self,FPlayer3GameControl,gaPlayer,nil, nil,GBLastChoiceP3,nil,nil,nil);
        with FPlayer3GameBoard do
          begin
            OnPlayerExit := @Self.PlayerExit;
            OnEndChoice := @Self.EndChoice;
            OnStartChoice := @Self.StartChoice;
            OnWaitForServer := @Self.WaitForServer;

            Chat := nil;
            ChatPanel := nil;
            Timer := nil;
            TimerEvent := nil;
            ImagePointI := nil;
            LabelPointIName := nil;
            LabelPointICount := nil;
            ListBoxOldPlayers := nil;

            ImagePointA := ImagePlayer3A;
            LabelPointAName := LabelPlayer3AName;
            LabelPointACount := LabelPlayer3ACount;

            ImagePointB := ImagePlayer3B;
            LabelPointBName := LabelPlayer3BName;
            LabelPointBCount := LabelPlayer3BCount;

            ImageGroup1 := ImagePlayer3Group1;
            LabelGroup1Name := LabelPlayer3Group1Name;
            LabelGroup1Count := LabelPlayer3Group1Count;

            ImageGroup2 := ImagePlayer3Group2;
            LabelGroup2Name := LabelPlayer3Group2Name;
            LabelGroup2Count := LabelPlayer3Group2Count;
            FallbackMessages := ListBoxMessages3;
            BeforeStartExperimentSetup;
          end;
        FPlayer3GameControl.GameBoard := FPlayer3GameBoard;
        FPlayer3GameControl.Login;
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

  if chkP1.Checked then begin
    chkP1.Checked := False;
    FPlayer1GameControl.SendRequest(K_CHOICE, [choice.n,choice.c])
  end else if chkP2.Checked then begin
    chkP2.Checked := False;
    FPlayer2GameControl.SendRequest(K_CHOICE, [choice.n,choice.c])
  end else if chkP3.Checked then begin
    chkP3.Checked := False;
    FPlayer3GameControl.SendRequest(K_CHOICE, [choice.n,choice.c]);
  end;
end;

procedure TForm1.WriteReport(S: string);
begin
  ListBoxReport.Items.Append(S);
end;

procedure TForm1.PlayerExit(P: TPlayer; AMessage: string);
begin

end;

procedure TForm1.EndChoice(Sender: TObject);
begin
  if Sender = FServerGameControl then
    begin
      ListBoxExperiment.Items.Append('Server.EndChoice');
    end;

  if Sender = FPlayer1GameControl then
    begin
      ListBoxExperiment.Items.Append('P1.EndChoice');
      chkP1.Checked:=False;
    end;

  if Sender = FPlayer2GameControl then
    begin
      ListBoxExperiment.Items.Append('P2.EndChoice');
      chkP2.Checked:=False;
    end;

  if Sender = FPlayer3GameControl then
    begin
      ListBoxExperiment.Items.Append('P3.EndChoice');
      chkP3.Checked:=False;
    end;
end;

procedure TForm1.StartChoice(Sender : TObject);
begin
  if Sender = FServerGameControl then
    ListBoxExperiment.Items.Append('Server.StartChoice');

  if Sender = FPlayer1GameControl then begin
    ListBoxExperiment.Items.Append('P1.StartChoice');
    chkP1.Checked:=True;
  end;

  if Sender = FPlayer2GameControl then begin
    ListBoxExperiment.Items.Append('P2.StartChoice');
    chkP2.Checked:=True;
  end;

  if Sender = FPlayer3GameControl then begin
    ListBoxExperiment.Items.Append('P3.StartChoice');
    chkP3.Checked:=True;
  end;
end;

procedure TForm1.WaitForServer(Sender : TObject);
begin
  if Sender = FServerGameControl then
    ListBoxExperiment.Items.Append('Server.WaitForServer');

  if Sender = FPlayer1GameControl then begin
    ListBoxExperiment.Items.Append('P1.WaitForServer');
    chkP1.Checked:=False;
  end;

  if Sender = FPlayer2GameControl then begin
    ListBoxExperiment.Items.Append('P2.WaitForServer');
    chkP2.Checked:=False;
  end;

  if Sender = FPlayer3GameControl then begin
    ListBoxExperiment.Items.Append('P3.WaitForServer');
    chkP3.Checked:=False;
  end;

end;


end.

