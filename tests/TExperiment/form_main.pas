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
  ComCtrls, game_experiment
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonChoice1: TButton;
    ButtonChoice3: TButton;
    ButtonLogin: TButton;
    ButtonChoice2: TButton;
    ListBox1: TListBox;
    ListBox2: TListBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ButtonChoice1Click(Sender: TObject);
    procedure ButtonLoginClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Consequence(Sender: TObject);
  private
    FExperiment : TExperiment;
    procedure EndCondition(Sender: TObject);
    procedure EndCycle(Sender: TObject);
    procedure EndExperiment(Sender: TObject);
    procedure EndGeneration(Sender: TObject);
    procedure EndTurn(Sender: TObject);
    procedure Interlocking(Sender: TObject);
    procedure StartExperiment(Sender: TObject);
    procedure TargetInterlocking(Sender: TObject);
    procedure WriteReport(S: string);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  game_actors
  , game_resources
  , helpers
  , strutils
  ;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FExperiment := TExperiment.Create(Self);
  FExperiment.OnConsequence:=@Consequence;
  FExperiment.OnEndCondition:=@EndCondition;
  FExperiment.OnEndCycle:=@EndCycle;
  FExperiment.OnEndExperiment:=@EndExperiment;
  FExperiment.OnEndGeneration:=@EndGeneration;
  FExperiment.OnEndTurn:=@EndTurn;
  FExperiment.OnInterlocking:=@Interlocking;
  FExperiment.OnTargetInterlocking:=@TargetInterlocking;
  FExperiment.OnStartExperiment:=@StartExperiment;
  FExperiment.OnWriteReport:=@WriteReport;

  FExperiment.LoadFromFile('/home/rafael/git/free-mtrix/experiment_runner/Pesquisadores/Thais/Teste 1.Estudo 1.MC1.ini');

end;

procedure TForm1.ButtonLoginClick(Sender: TObject);
var
  PID , TS: string;
  P : TPlayer;
  i: LongInt;
begin
  PID := RandomString(32);
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
            FExperiment.Play;
          ListBox1.Items.Append('');

        end
      else
        ListBox1.Items.Append('Login Refused Full');
    end
  else
    ListBox1.Items.Append('Login Refused Playing');
end;

procedure TForm1.ButtonChoice1Click(Sender: TObject);
var
  LConsequences : string;
  P : TPlayer;
  S : string;
  LEndCondition,
  LEndCycle : Boolean;
  LEndGeneration: string;
begin
  LConsequences := '';

  P := FExperiment.PlayerFromID[FExperiment.Player[FExperiment.CurrentCondition.Turn.Count].ID];
  ListBox1.Items.Append('Player:'+P.Nicname);
  if Sender = ButtonChoice1 then
    begin
      P.Choice.Row := grTwo;
      P.Choice.Color := gcGreen;
    end;

  if Sender = ButtonChoice2 then
    begin
      P.Choice.Row := grThree;
      P.Choice.Color := gcBlue;
    end;

  if Sender = ButtonChoice3 then
    begin
      P.Choice.Row := grFour;
      P.Choice.Color := gcRed;
    end;

  //individual consequences
  S := FExperiment.ConsequenceStringFromChoice[P];
  if Pos('$NICNAME',S) > 0 then
    S := ReplaceStr(S,'$NICNAME',P.Nicname);


  // "NextGeneration" and "ShouldEndCycle" methods must be called before Experiment.NextTurn
  LEndCycle := FExperiment.ShouldEndCycle;
  LEndGeneration := FExperiment.NextGeneration;
  if LEndCycle then
    LConsequences := FExperiment.ConsequenceStringFromChoices;

  // update turn
  P.Turn := FExperiment.NextTurn;
  FExperiment.Player[FExperiment.PlayerIndexFromID[P.ID]] := P;

  // append results
  ListBox1.Items.Append('UpdateTurn:'+IntToStr(P.Turn));            //5
  ListBox1.Items.Append('IndividualConsequences:'+S);                //6
  if LEndCycle then // >7 = EndCycle
    begin
      ListBox1.Items.Append('GroupConsequences:'+LConsequences); //7

      if FExperiment.ShouldAskQuestion then  // DONE: prompt only when an odd row was selected
         ListBox1.Items.Append('ShouldAskQuestion:'+FExperiment.CurrentCondition.Prompt.Question) //8
      else
        begin
          ListBox1.Items.Append('ShouldAskQuestion:'+#32); // 8
          if Assigned(FExperiment.CurrentCondition.Prompt) then
            FExperiment.WriteReportRowPrompt; //TODO: FIND WHY OPTIMIZATION 3 GENERATES BUG HERE
          FExperiment.Clean;
        end;

      ListBox1.Items.Append('Generation:'+LEndGeneration); // 9, #32 resume, else NextGeneration = PlayerToKick AID
      LEndCondition := FExperiment.ShouldEndCondition;
      if FExperiment.IsLastCondition and LEndCondition then // 10
        // end experiment envelop item
        ListBox1.Items.Append('LastCondition:'+#27)
      else
        if LEndCondition then
          begin
            FExperiment.NextCondition;
            // end condition envelop item
            ListBox1.Items.Append('EndConditionTrue:'+FExperiment.CurrentConditionAsString);
          end
        else
          // do nothing envelop item
          ListBox1.Items.Append('EndConditionFalse:'+#32);
    end;
  ListBox1.Items.Append('');
  ListBox1.Selected[ListBox1.Count-1];
end;

procedure TForm1.Consequence(Sender: TObject);
begin
  ListBox1.Items.Append('Consequence >>');
end;

procedure TForm1.TargetInterlocking(Sender: TObject);
begin
  ListBox1.Items.Append('TargetInterlocking  >>');
end;

procedure TForm1.WriteReport(S: string);
begin
  ListBox2.Items.Append(S);
end;

procedure TForm1.Interlocking(Sender: TObject);
begin
  ListBox1.Items.Append('Interlocking >>');
end;

procedure TForm1.StartExperiment(Sender: TObject);
begin
  ListBox1.Items.Append('StartExperiment >>');
end;

procedure TForm1.EndTurn(Sender: TObject);
begin
  ListBox1.Items.Append('EndTurn >>');
end;

procedure TForm1.EndGeneration(Sender: TObject);
begin
  ListBox1.Items.Append('EndGeneration >>');
end;

procedure TForm1.EndExperiment(Sender: TObject);
begin
  ListBox1.Items.Append('EndExperiment >>');
end;

procedure TForm1.EndCycle(Sender: TObject);
begin
  ListBox1.Items.Append('EndCycle >>');
end;

procedure TForm1.EndCondition(Sender: TObject);
begin
  ListBox1.Items.Append('EndCondition >>');
end;

end.

