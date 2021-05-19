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
  ComCtrls, Spin
  , game_experiment
  , game_visual_experiment
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ButtonRandomChoice: TButton;
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
    Label1: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    PageControl1: TPageControl;
    SpinEdit1: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure ButtonChoice1Click(Sender: TObject);
    procedure ButtonLoginClick(Sender: TObject);
    procedure ButtonQY1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FExperiment : TExperiment;
    FExperimentBox : TExperimentBox;
    procedure WriteReport(S: string);
    procedure EnableButtons;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  game_actors
  , game_resources
  , string_methods
  , strutils
  , game_actors_helpers
  ;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
//var
//  LCS : TCriteria;
//  i, j: Integer;
begin
  FExperimentBox := TExperimentBox.Create(Self);
  FExperimentBox.Left := 926;
  FExperimentBox.Top := 16;
  FExperimentBox.Parent := Self;

  FExperiment := TExperiment.Create(Self);
  FExperiment.OnInterlocking := @FExperimentBox.Interlocking;
  FExperiment.OnTargetInterlocking:= @FExperimentBox.TargetInterlocking;
  FExperiment.OnStartExperiment := @FExperimentBox.StartExperiment;
  FExperiment.OnStartTurn := @FExperimentBox.StartTurn;
  FExperiment.OnStartCycle := @FExperimentBox.StartCycle;
  FExperiment.OnStartGeneration:= @FExperimentBox.StartGeneration;
  FExperiment.OnStartCondition:= @FExperimentBox.StartCondition;
  FExperiment.OnEndExperiment :=@FExperimentBox.EndExperiment;

  FExperiment.OnWriteReport:=@WriteReport;
  //FExperiment.LoadFromFile('C:\Users\User\Documents\GitHub\free-mtrix\experiment_runner\Pesquisadores\Pesquisador_X\Teste 2.Estudo 2.MC3.ini');
  //FExperiment.LoadFromFile('/home/cpicanco/Code/git/free-mtrix/experiment_runner/Participant0/Experiment1.ini');
  //for j := 0 to FExperiment.ConditionsCount-1 do
  //  for i := 0 to Length(FExperiment.Condition[j].Contingencies)-1 do
  //    begin
  //      LCS := FExperiment.Condition[j].Contingencies[i].Criteria;
  //      ListBox1.Items.Append(FExperiment.Condition[j].Contingencies[i].Consequence.AsString('M'));
  //    end;
end;

procedure TForm1.ButtonLoginClick(Sender: TObject);
var
  PID , TS: string;
  P : TPlayer;
  i: LongInt;
begin
  PID := Format('%12X-%12X',[Random($1000000000000), Random($1000000000000)]);
  // validate login request
  if not FExperiment.PlayerIsPlaying[PID] then
    begin
      if FExperiment.PlayersCount < FExperiment.CurrentCondition.Turn.Value then
        begin
          // ok, let player login
          P.ID := PID;

          // check if we already know this player
          i := FExperiment.PlayerIndexFromID[P.ID];
          if i > -1 then
            begin
              // then load p data
              P := FExperiment.Player[i]
            end
          else
            begin
              // if not then generate and save p data
              i := FExperiment.AppendPlayer;
              if FExperiment.GenPlayersAsNeeded then
                P.Nicname := GenResourceName(i)
              else
                P.Nicname := GenResourceName(i);

              P.Points.A:=0;
              P.Points.B:=0;
              P.Status:=gpsPlaying;
              P.Choice.Color:=gcNone;
              P.Choice.Row:=grNone;
              // first turn always by entrance order
              P.Turn := i;
              FExperiment.Player[i] := P;
            end;

          // create/config playerbox
          // CreatePlayerBox(P,False,True);

          // Request is now a reply with the following standard:
          // [Requester.ID 0, ' ' 1, ReplyTag 2, PlayerData 3, PlayersPlaying 4 .. n, ChatData Last]
          //ARequest[2] := GA_ADMIN+ARequest[2]+K_ARRIVED;

          // player
          ListBox1.Items.Append('PlayerAsString:'+FExperiment.PlayerAsString[P]);
          //ARequest.Append(PS);

          // append current players playing
          if FExperiment.PlayersCount > 0 then
            for i:=0 to FExperiment.PlayersCount -1 do
              if FExperiment.Player[i].ID <> P.ID then
                begin
                  TS := FExperiment.PlayerAsString[FEXperiment.Player[i]];
                  ListBox1.Items.Append('CurrentPlayersPlaying:'+TS);
                end;
          // appen matrix type
          ListBox1.Items.Append('MatrixTypeString:'+FExperiment.MatrixTypeAsString);

          // append chat data
          if FExperiment.ShowChat then
            begin
              if FExperiment.SendChatHistoryForNewPlayers then
                ListBox1.Items.Append('[CHAT HISTORY]')
              else
                ListBox1.Items.Append('[CHAT]'); // must append something to keep the message envelop with standard size
            end
          else
            ListBox1.Items.Append('[NOCHAT]');

          // append global configs.
          ListBox1.Items.Append('ABPoints:'+BoolToStr(FExperiment.ABPoints)); // COUNT-2

          // append condition global data
          ListBox1.Items.Append('CurrentConditionString:'+FExperiment.CurrentConditionAsString);

          // inform all players about the new player, including itself

          // start Experiment
          if FExperiment.ShouldStartExperiment then
            begin
              FExperiment.Play;
              EnableButtons;
            end;
          ListBox1.Items.Append('');

        end
      else
        ListBox1.Items.Append('Login Refused Full');
    end
  else
    ListBox1.Items.Append('Login Refused Playing');
end;

procedure TForm1.ButtonQY1Click(Sender: TObject);
var
  R : string;
  P : TPlayer;
  i : integer;
  LPromptConsequences : TStringList;
begin
  if Sender = ButtonQY1 then
    begin
      P := FExperiment.PlayerFromID[FExperiment.Player[0].ID];
      R := 'Y';
    end;

  if Sender = ButtonQY2 then
    begin
      P := FExperiment.PlayerFromID[FExperiment.Player[1].ID];
      R := 'Y';
    end;

  if Sender = ButtonQY3 then
    begin
      P := FExperiment.PlayerFromID[FExperiment.Player[2].ID];
      R := 'Y';
    end;

  if Sender = ButtonQN1 then
    begin
      P := FExperiment.PlayerFromID[FExperiment.Player[0].ID];
      R := 'N';
    end;

  if Sender = ButtonQN2 then
    begin
      P := FExperiment.PlayerFromID[FExperiment.Player[1].ID];
      R := 'N';
    end;

  if Sender = ButtonQN3 then
    begin
      P := FExperiment.PlayerFromID[FExperiment.Player[2].ID];
      R := 'N';
    end;

  // append response of each player
  ListBox1.Items.Append('Question Response:'+P.ID+' '+R);
  FExperiment.CurrentCondition.Prompt.AppendResponse(P.ID,R);

  // return to experiment and present the prompt consequence, if any
  if FExperiment.CurrentCondition.Prompt.ResponsesCount = FExperiment.PlayersCount then
    begin
      // generate messages
      LPromptConsequences := FExperiment.CurrentCondition.Prompt.AsString;

      if LPromptConsequences.Count > 0 then
        begin
          for i := 0 to LPromptConsequences.Count-1 do
            begin
              P := FExperiment.PlayerFromID[ExtractDelimited(1,LPromptConsequences[i],['+'])];
              LPromptConsequences[i] := DeduceNicname(LPromptConsequences[i],P);
            end;
          ListBox1.Items.Append('Question Consequences:');
          ListBox1.Items.Append(LPromptConsequences.Text);
        end
      else
        ListBox1.Items.Append('Question Consequences: empty');;

      FExperiment.WriteReportRowPrompt;
      FExperiment.Clean;
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //FExperiment.Clean;
end;

procedure TForm1.ButtonChoice1Click(Sender: TObject);
var
  P,PtoKick : TPlayer;
  G,
  T, S , LIDTurn: string;
  LCount, i: Integer;
begin

  P := FExperiment.PlayerFromID[FExperiment.Player[FExperiment.CurrentCondition.Turn.Count].ID];
  if Sender = ButtonChoice1 then
    begin
      P.Choice.Row := grOne;
      P.Choice.Color := gcYellow;
    end;

  if Sender = ButtonChoice2 then
    begin
      P.Choice.Row := grTwo;
      P.Choice.Color := gcRed;
    end;

  if Sender = ButtonChoice3 then
    begin
      P.Choice.Row := grThree;
      P.Choice.Color := gcGreen;
    end;

  if Sender = ButtonChoice4 then
    begin
      P.Choice.Row := grFour;
      P.Choice.Color := gcBlue;
    end;
  if Sender = ButtonChoice5 then
    begin
      P.Choice.Row := grFive;
      P.Choice.Color := gcMagenta;
    end;

  if Sender = ButtonChoice6 then
    begin
      P.Choice.Row := grSix;
      P.Choice.Color := gcYellow;
    end;

  if Sender = ButtonChoice7 then
    begin
      P.Choice.Row := grSeven;
      P.Choice.Color := gcRed;
    end;

  if Sender = ButtonChoice8 then
    begin
      P.Choice.Row := grEight;
      P.Choice.Color := gcGreen;
    end;

  if Sender = ButtonChoice9 then
    begin
      P.Choice.Row := grNine;
      P.Choice.Color := gcBlue;
    end;

  if Sender = ButtonChoice10 then
    begin
      P.Choice.Row := grTen;
      P.Choice.Color := gcMagenta;
    end;

  if Sender = ButtonRandomChoice then
    begin
      P.Choice.Row := TGameRow(Random(Succ(Ord(High(TGameRow)))));
      P.Choice.Color := TGameColor(Random(Succ(Ord(High(TGameColor)))));
    end;

  //ShowMessage(TComponent(Sender).Name);
  //Sleep(500);
  ListBox1.Items.Append('Player:'+P.Nicname +' : ' + GetRowString(P.Choice.Row)+' : '+GetColorString(P.Choice.Color));

  // individual consequences from player choice
  S := FExperiment.ConsequenceStringFromChoice[P];

  // update turns
  T := FExperiment.NextTurn;

  // update next turn if necessary
  if T <> #32 then
    begin
      LCount := WordCount(T,['+']);
      if LCount > 0 then
        for i := 1 to LCount do
          begin
            LIDTurn := ExtractDelimited(i,T,['+']);
            ListBox1.Items.Append('UpdatedTurnA:'+ExtractDelimited(1,LIDTurn,['|']));
            ListBox1.Items.Append('UpdatedTurnB:'+ExtractDelimited(2,LIDTurn,['|']));
          end;
    end
  else
    ListBox1.Items.Append('UpdatedTurn:'+T); // 5

  ListBox1.Items.Append('IndividualConsequences:'+S);    // 6
  if FExperiment.IsEndCycle then // >7 = EndCycle
    begin
      // group consequences from choices of all players
      ListBox1.Items.Append('GroupConsequences:'+FExperiment.ConsequenceStringFromChoices); // 7

      // prompt question if an odd row was selected
      ListBox1.Items.Append('ShouldAskQuestion:'+FExperiment.ShouldAskQuestion);            // 8

      // #32 resume else NextGeneration = PlayerToKick AID
      G := FExperiment.NextGeneration;
      ListBox1.Items.Append('Generation:'+G);                                               // 9
      if G <> #32 then
        begin
          PtoKick := FExperiment.PlayerFromID[G];
          PtoKick.Nicname := GenResourceName(-1);
          G := FExperiment.PlayerAsString[PtoKick];
          FExperiment.NextGeneration := G;
        end;

      // Check if we need to end the current condition
      ListBox1.Items.Append(FExperiment.NextCondition);                                     // 10
    end;
  ListBox1.Items.Append('');
  ListBox1.ItemIndex:=ListBox1.Items.Count-1;
  ListBox1.MakeCurrentVisible;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to SpinEdit1.Value-1 do begin
    ButtonRandomChoice.Click;
  end;
end;

procedure TForm1.WriteReport(S: string);
begin
  ListBox2.Items.Append(S);
end;

procedure TForm1.EnableButtons;
begin
  ButtonChoice1.Enabled:=True;
  ButtonChoice2.Enabled:=True;
  ButtonChoice3.Enabled:=True;
  ButtonChoice4.Enabled:=True;
  ButtonChoice5.Enabled:=True;
  ButtonChoice6.Enabled:=True;
  ButtonChoice7.Enabled:=True;
  ButtonChoice8.Enabled:=True;
  ButtonChoice9.Enabled:=True;
  ButtonChoice10.Enabled:=True;
  ButtonRandomChoice.Enabled:=True;
  Button1.Enabled:=True;
  SpinEdit1.Enabled:=True;
  ButtonLogin.Enabled:=False;
end;


end.

