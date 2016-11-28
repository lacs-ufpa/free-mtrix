{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_control;

{$mode objfpc}{$H+}

//{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, Graphics, Grids
  , game_zmq_actors
  , game_experiment
  , game_actors
  , game_visual_elements
  ;

type


  { TGameControl }

  TGameControl = class(TComponent)
  private
    FID: UTF8string;
    FMustDrawDots: Boolean;
    FMustDrawDotsClear: Boolean;
    FRowBase : integer;
    FActor : TGameActor;
    FZMQActor : TZMQActor;
    FExperiment : TExperiment;
    function GetPlayerBox(AID:UTF8string) : TPlayerBox;
    function GetActorNicname(AID:UTF8string) : UTF8string;
    function GetSelectedColorF(AStringGrid : TStringGrid) : UTF8string;
    function GetSelectedRowF(AStringGrid : TStringGrid) : UTF8string;
    function MessageHas(const A_CONST : UTF8string; AMessage : TStringList; I:ShortInt=0): Boolean;
    procedure CreatePlayerBox(P:TPlayer; Me:Boolean);
    procedure SetMatrixType(AStringGrid : TStringGrid; AMatrixType:TGameMatrixType;
      var ARowBase:integer; var ADrawDots, ADrawClear : Boolean);
    procedure ReceiveMessage(AMessage : TStringList);
    procedure ReceiveRequest(var ARequest : TStringList);
    procedure ReceiveReply(AReply : TStringList);
    procedure SetMustDrawDots(AValue: Boolean);
    procedure SetMustDrawDotsClear(AValue: Boolean);
    procedure SetRowBase(AValue: integer);
  private
    function AskQuestion(AQuestion:string):UTF8string;
    procedure ShowPopUp(AText:string);
    procedure DisableConfirmationButton;
    procedure CleanMatrix(AEnabled : Boolean);
    procedure EnablePlayerMatrix(AID:UTF8string; ATurn:integer; AEnabled:Boolean);
  private
    function ShouldStartExperiment: Boolean;
    function ShouldEndCycle : Boolean;
    function ShouldAskQuestion : Boolean;
    procedure KickPlayer(AID:string);
    procedure NextTurn(Sender: TObject);
    procedure NextCycle(Sender: TObject);
    procedure NextLineage(Sender: TObject);
    procedure NextCondition(Sender: TObject);
    procedure Interlocking(Sender: TObject);
    procedure Consequence(Sender: TObject);
    procedure EndExperiment(Sender: TObject);
    procedure StartExperiment;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy; override;
    procedure SetMatrix;
    procedure SendRequest(ARequest : UTF8string);
    procedure SendMessage(AMessage : UTF8string);
    procedure Cancel;
    procedure Start;
    procedure Pause;
    procedure Resume;
    property Experiment : TExperiment read FExperiment write FExperiment;
    property ID : UTF8string read FID;
    property RowBase : integer read FRowBase write SetRowBase;
    property MustDrawDots: Boolean read FMustDrawDots write SetMustDrawDots;
    property MustDrawDotsClear:Boolean read FMustDrawDotsClear write SetMustDrawDotsClear;
  end;

  function GetRowColor(ARow : integer;ARowBase:integer) : TColor;

const
  K_FULLROOM = '.Full';
  K_PLAYING  = '.Playing';
  K_ARRIVED  = '.Arrived';
  K_REFUSED  = '.Refused';
  K_CHAT_M   = '.ChatM';
  K_CHOICE   = '.Choice';
  K_MESSAGE  = '.Message';
  K_START    = '.Start';
  K_LEFT     = '.Left';
  K_RESUME   = '.Resume';
  K_DATA_A   = '.Data';
  K_LOGIN    = '.Login';
  K_KICK     = '.Kick';
  K_QUESTION = '.Question';
  K_QMESSAGE = '.QMessage';
  //
  K_STATUS   = '.Status';
  K_CYCLES   = '.OnEndCycle';

  //K_RESPONSE =

implementation

uses ButtonPanel,Controls,ExtCtrls,StdCtrls,
     LazUTF8, Forms, strutils, zhelpers,
     form_matrixgame, form_chooseactor, game_resources, string_methods ;

const
  GA_ADMIN = 'Admin';
  GA_PLAYER = 'Player';
  //GA_WATCHER = 'Watcher';

function GetRowColor(ARow: integer; ARowBase:integer): TColor;
var LRow : integer;
begin
  if ARowBase = 1 then
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

{ TGameControl }

function TGameControl.ShouldStartExperiment: Boolean;
begin
  Result := FExperiment.PlayersCount = FExperiment.Condition[FExperiment.CurrentCondition].Turn.Value;
end;

function TGameControl.ShouldEndCycle: Boolean;
begin
  Result := FExperiment.Condition[FExperiment.CurrentCondition].Turn.Count = FExperiment.Condition[FExperiment.CurrentCondition].Turn.Value-1;
end;

function TGameControl.ShouldAskQuestion: Boolean; // end cycle, restart alias
begin
  // TODO: prompt only when an odd row was selected
  Result := ShouldEndCycle and FExperiment.Condition[FExperiment.CurrentCondition].Contingencies[3].Fired;
end;

procedure TGameControl.KickPlayer(AID: string);
begin
  FZMQActor.SendMessage([K_KICK, AID]);
end;

procedure TGameControl.NextTurn(Sender: TObject);
begin
  // update admin view
  FormMatrixGame.LabelExpCountTurn.Caption:=IntToStr(FExperiment.Condition[FExperiment.CurrentCondition].Turn.Count+1);

end;

procedure TGameControl.NextCycle(Sender: TObject);
var
  i,
  LCount : integer;
  LConsequences : string;
begin
  // prompt question to all players
  FormMatrixGame.LabelExpCountCycle.Caption:=IntToStr(FExperiment.Condition[FExperiment.CurrentCondition].Cycles.Count+1);
  {$IFDEF DEBUG}
  WriteLn('cycle:>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
  {$ENDIF}


  //P := FExperiment.PlayerFromID[Self.ID];
  LConsequences := FExperiment.ConsequenceStringFromChoices;
  LCount := WordCount(LConsequences,['+']);
  if LCount > 0 then
    for i := 1 to LCount do
      FZMQActor.SendMessage([K_CYCLES,ExtractDelimited(i,LConsequences,['+'])]);       // as string generates the pts result
end;

procedure TGameControl.NextLineage(Sender: TObject);
begin
  // pause, kick older player, wait for new player, resume
  FormMatrixGame.LabelExpCountGeneration.Caption:=IntToStr(FExperiment.Condition[FExperiment.CurrentCondition].Cycles.Generation+1);
end;

procedure TGameControl.NextCondition(Sender: TObject);
begin
  FormMatrixGame.LabelExpCountCondition.Caption:= FExperiment.Condition[FExperiment.CurrentCondition].ConditionName;

  // append OnStart data
  //FExperiment.Condition[FExperiment.CurrentCondition].Points.OnStart.A;
  //FExperiment.Condition[FExperiment.CurrentCondition].Points.OnStart.B;
  //FExperiment.Condition[FExperiment.CurrentCondition].Points.OnStart.G;

  // append which player
end;

procedure TGameControl.Interlocking(Sender: TObject);
begin
  FormMatrixGame.LabelExpCountInterlocks.Caption:= IntToStr(FExperiment.Condition[FExperiment.CurrentCondition].Interlocks.Count+1);

end;

procedure TGameControl.Consequence(Sender: TObject);
begin
{$IFDEF DEBUG}
  if Sender is TConsequence then
    FormMatrixGame.ChatMemoRecv.Lines.Append(('['+TConsequence(Sender).PlayerNicname+']: ')+TConsequence(Sender).AsString(''));
{$ENDIF}
end;

procedure TGameControl.EndExperiment(Sender: TObject);
begin

end;

procedure TGameControl.StartExperiment;
begin
  // all players arrived, lets begin
  FExperiment.State:=xsRunning;

  // wait some time, we just sent a message earlier
  Sleep(5);

  // enable matrix grid for the first player
  FZMQActor.SendMessage([K_START]);
end;

procedure TGameControl.Start;
begin
  // basic data/csv setup
  // wait for players

end;

procedure TGameControl.Pause;
begin

end;

procedure TGameControl.Resume;
begin

end;

function TGameControl.GetPlayerBox(AID: UTF8string): TPlayerBox;
var i : integer;
begin
  for i := 0 to FormMatrixGame.GBLastChoice.ComponentCount-1 do
    if TPlayerBox(FormMatrixGame.GBLastChoice.Components[i]).ID = AID then
      begin
        Result := TPlayerBox(FormMatrixGame.GBLastChoice.Components[i]);
        Break;
      end;
end;

function TGameControl.GetActorNicname(AID: UTF8string): UTF8string;
begin
  case FActor of
    gaPlayer: begin
      Result := 'UNKNOWN';
      if FExperiment.Player[AID].ID <> '' then
        Result := FExperiment.Player[AID].Nicname;
    end;

    gaAdmin: Result := FExperiment.Researcher;
  end;
end;

function TGameControl.MessageHas(const A_CONST: UTF8string; AMessage: TStringList;
  I: ShortInt): Boolean;
begin
  Result:= False;
  if not Assigned(AMessage) then Exit;
  Result := Pos(A_CONST,AMessage[I])>0;
end;

procedure TGameControl.CreatePlayerBox(P: TPlayer; Me: Boolean);
var i1 : integer;
begin
  with TPlayerBox.Create(FormMatrixGame.GBLastChoice,P.ID) do
    begin
      if Me then
        Caption := P.Nicname+SysToUtf8(' (Você)' )
      else
        Caption := P.Nicname;
      i1 := Integer(P.Choice.Row);
      if i1 > 0 then
        LabelLastRowCount.Caption := Format('%-*.*d', [1,2,i1])
      else
        LabelLastRowCount.Caption := 'NA';
      PanelLastColor.Color := GetColorFromCode(P.Choice.Color);
      Enabled := True;
      Parent := FormMatrixGame.GBLastChoice;
    end;
end;

procedure TGameControl.SetMatrixType(AStringGrid: TStringGrid;
  AMatrixType: TGameMatrixType; var ARowBase: integer; var ADrawDots,
  ADrawClear: Boolean);

  procedure WriteGridFixedNames(ASGrid: TStringGrid; WriteCols: boolean);
  var
    i: integer;
  begin
    with ASGrid do
      for i := 0 to 9 do
      begin
        Cells[0, i + ARowBase] := IntToStr(i + 1);
        if WriteCols then
          Cells[i + 1, 0] := chr(65 + i);
      end;
  end;

begin
  AStringGrid.Clean;
  if gmRows in AMatrixType then
    begin
      ARowBase := 0;
      AStringGrid.FixedRows := 0;
      AStringGrid.RowCount := 10;
      AStringGrid.Height:=305;
      AStringGrid.Options := [goFixedHorzLine, goHorzLine];
      WriteGridFixedNames(AStringGrid, False);
    end;

  if gmColumns in AMatrixType then
    begin
      ARowBase := 1;
      AStringGrid.Clean;
      AStringGrid.FixedRows := 1;
      AStringGrid.RowCount := 11;
      AStringGrid.Height:=335;
      AStringGrid.Options := [goFixedHorzLine, goHorzLine, goVertLine];
      WriteGridFixedNames(AStringGrid, True);
    end;

  ADrawDots := gmDots in AMatrixType;
  ADrawClear:= gmClearDots in AMatrixType;
end;

function TGameControl.GetSelectedColorF(AStringGrid: TStringGrid): UTF8string;
begin
  Result := GetColorString(GetRowColor(AStringGrid.Selection.Top,RowBase));
end;

function TGameControl.GetSelectedRowF(AStringGrid: TStringGrid): UTF8string;
var i : integer;
begin
  i := AStringGrid.Selection.Top;
  if RowBase = 0 then
    Inc(i);
  Result := Format('%-*.*d', [1,2,i]);
end;

procedure TGameControl.SetMustDrawDots(AValue: Boolean);
begin
  if FMustDrawDots=AValue then Exit;
  FMustDrawDots:=AValue;
end;

procedure TGameControl.SetMustDrawDotsClear(AValue: Boolean);
begin
  if FMustDrawDotsClear=AValue then Exit;
  FMustDrawDotsClear:=AValue;
end;

procedure TGameControl.SetRowBase(AValue: integer);
begin
  if FRowBase=AValue then Exit;
  case AValue of
    0 : FExperiment.MatrixType := [gmRows];
    1 : FExperiment.MatrixType := [gmRows,gmColumns];
  end;
  FRowBase:=AValue;
end;

// many thanks to howardpc for this:
// http://forum.lazarus.freepascal.org/index.php/topic,34559.msg227585.html#msg227585
function TGameControl.AskQuestion(AQuestion: string): UTF8string;
var
  Prompt: TForm;
  ButtonPanel: TButtonPanel;
  LabelQuestion: TLabel;
  mr: TModalResult;
begin
  Prompt:=TForm.CreateNew(nil);
  try
    with Prompt do begin
      BorderStyle:=bsNone;
      Position:=poScreenCenter;
      ButtonPanel:=TButtonPanel.Create(Prompt);
      with ButtonPanel do begin
        ButtonOrder:=boCloseOKCancel;
        OKButton.Caption:='Sim';
        CancelButton.Caption:='Não';
        ShowButtons:=[pbOK, pbCancel];
        ShowBevel:=True;
        ShowGlyphs:=[];
        Parent:=Prompt;
      end;
      LabelQuestion:=TLabel.Create(Prompt);
      with LabelQuestion do begin
        Align:=alClient;
        Caption:= AQuestion;
        Alignment := taCenter;
        Anchors := [akLeft,akRight];
        Layout := tlCenter;
        WordWrap := True;
        Parent:=Prompt;
      end;

      mr:=ShowModal;
      if mr = mrOK then
        Result := 'S'
      else Result := 'N';
    end;
  finally
    Prompt.Free;
  end;
end;

procedure TGameControl.ShowPopUp(AText: string);
var PopUpPos : TPoint;
begin
  PopUpPos.X := FormMatrixGame.GBIndividualAB.Left-110;
  PopUpPos.Y := FormMatrixGame.GBIndividualAB.Top+FormMatrixGame.GBIndividual.Height-10;
  PopUpPos := FormMatrixGame.ClientToScreen(PopUpPos);
  FormMatrixGame.PopupNotifier.Title:='';
  FormMatrixGame.PopupNotifier.Text:=AText;
  FormMatrixGame.PopupNotifier.ShowAtPos(PopUpPos.X,PopUpPos.Y);
  FormMatrixGame.Timer.OnTimer:=@FormMatrixGame.TimerTimer;
  FormMatrixGame.Timer.Enabled:=True;
end;

procedure TGameControl.DisableConfirmationButton;
begin
  FormMatrixGame.StringGridMatrix.Enabled:= False;
  FormMatrixGame.btnConfirmRow.Enabled:=False;
  FormMatrixGame.btnConfirmRow.Caption:='OK';
end;

procedure TGameControl.CleanMatrix(AEnabled : Boolean);
begin
  FormMatrixGame.StringGridMatrix.Enabled:=AEnabled;
  FormMatrixGame.StringGridMatrix.Options := FormMatrixGame.StringGridMatrix.Options-[goRowSelect];
  FormMatrixGame.btnConfirmRow.Enabled:=True;
  FormMatrixGame.btnConfirmRow.Caption:='Confirmar';
  FormMatrixGame.btnConfirmRow.Visible := False;
end;

procedure TGameControl.EnablePlayerMatrix(AID:UTF8string; ATurn:integer; AEnabled:Boolean);
begin
  if FExperiment.PlayerFromID[AID].Turn = ATurn then
    CleanMatrix(AEnabled);
end;

constructor TGameControl.Create(AOwner: TComponent);
begin
  FZMQActor := TZMQActor(AOwner);
  inherited Create(FZMQActor.Owner);
  FID := FZMQActor.ID;
  FZMQActor.OnMessageReceived:=@ReceiveMessage;
  FZMQActor.OnRequestReceived:=@ReceiveRequest;
  FZMQActor.OnReplyReceived:=@ReceiveReply;
  FZMQActor.Start;

  if FZMQActor.ClassType = TZMQAdmin then
    FActor := gaAdmin;
  if FZMQActor.ClassType = TZMQPlayer then
    FActor := gaPlayer;
  if FZMQActor.ClassType = TZMQWatcher then
    FActor := gaWatcher;

  RowBase:= 0;
  MustDrawDots:=False;
  MustDrawDotsClear:=False;

  FExperiment := TExperiment.Create(FZMQActor.Owner);
  FExperiment.State:=xsWaiting;
  FExperiment.OnEndTurn := @NextTurn;
  FExperiment.OnEndCycle := @NextCycle;
  FExperiment.OnEndCondition:= @NextCondition;
  FExperiment.OnEndGeneration:=@NextLineage;
  FExperiment.OnEndExperiment:= @EndExperiment;
  FExperiment.OnInterlocking:=@Interlocking;
  FExperiment.OnConsequence:=@Consequence;

  //NextTurn(Self);
  //NextCycle(Self);
  //NextLineage(Self);
  //NextCondition(Self);
  //Interlocking(Self);
  //Consequence(Self);

  SendRequest(K_LOGIN);
end;

destructor TGameControl.Destroy;
begin
  inherited Destroy;
end;

procedure TGameControl.SetMatrix;
begin
  SetMatrixType(FormMatrixGame.StringGridMatrix, FExperiment.MatrixType,FRowBase,FMustDrawDots,FMustDrawDotsClear);
end;

procedure TGameControl.SendRequest(ARequest: UTF8string);
var
  M : array of UTF8string;

  procedure SetM(A : array of UTF8string);
  var i : integer;
  begin
    SetLength(M,Length(A));
    for i := 0 to Length(A) -1 do
      M[i] := A[i];
  end;
begin
  case ARequest of
    K_LOGIN :SetM([
        FZMQActor.ID
        , ' '
        , ARequest
      ]);

    K_CHOICE  : SetM([
        FZMQActor.ID
        , ' '
        , ARequest
        , GetSelectedRowF(FormMatrixGame.StringGridMatrix)
        , GetSelectedColorF(FormMatrixGame.StringGridMatrix)
      ]);

  end;
  case FActor of
    gaAdmin: begin
      //M[2] := GA_ADMIN+M[2];// for now cannot Requests
    end;
    gaPlayer:begin
      M[2] := GA_PLAYER+M[2];
    end;
    //gaWatcher:begin
    //  M[0] := GA_WATCHER+M[0];
  end;
  FZMQActor.Request(M);
end;

// called from outside
procedure TGameControl.SendMessage(AMessage: UTF8string);
var
  M : array of UTF8string;

  procedure SetM(A : array of UTF8string);
  var i : integer;
  begin
    SetLength(M,Length(A));
    for i := 0 to Length(A) -1 do
      M[i] := A[i];
  end;
begin
  case AMessage of

    K_CHAT_M  : begin
      //if (FActor = gaAdmin) and (not FExperiment.ResearcherCanChat) then Exit;
      SetM([
        AMessage
        , GetActorNicname(FZMQActor.ID)
        , FormMatrixGame.ChatMemoSend.Lines.Text
      ]);
    end;
    K_CHOICE  : SetM([
        AMessage
        , FZMQActor.ID
        , GetSelectedRowF(FormMatrixGame.StringGridMatrix)
        , GetSelectedColorF(FormMatrixGame.StringGridMatrix)
      ]);
  end;

  case FActor of
    gaAdmin: begin
      M[0] := GA_ADMIN+M[0];
    end;
    gaPlayer:begin
      M[0] := GA_PLAYER+M[0];
    end;
    //gaWatcher:begin
    //  M[0] := GA_WATCHER+M[0];
  end;
  FZMQActor.SendMessage(M);
end;

procedure TGameControl.Cancel;
begin

end;

// Here FActor is garanted to be a TZMQPlayer
procedure TGameControl.ReceiveMessage(AMessage: TStringList);
  function MHas(const C : UTF8string) : Boolean;
  begin
    Result := MessageHas(C,AMessage);
  end;

  procedure ReceiveActor;
  var P : TPlayer;
  begin
    case FActor of
      gaPlayer:
        begin
          P := FExperiment.PlayerFromString[AMessage[1]];
          FExperiment.AppendPlayer(P);
          if Self.ID = P.ID then
            begin
              CreatePlayerBox(P, True)
            end
          else
            CreatePlayerBox(P,False);
        end;
    end;
  end;

  procedure ReceiveChoice;
  var P : TPlayer;
  begin
    P := FExperiment.PlayerFromID[AMessage[1]];

    // add last responses to player box
    with GetPlayerBox(P.ID) do
      begin
        LabelLastRowCount.Caption := AMessage[2];
        PanelLastColor.Color := GetColorFromString(AMessage[3]);
        PanelLastColor.Caption:='';
      end;

    case FActor of
      gaPlayer:begin
       if P.Turn = FExperiment.PlayersCount-1  then
        begin
          // update next turn
          if Self.ID = P.ID then
           begin
             P.Turn := StrToInt(AMessage[4]);
             FExperiment.Player[Self.ID] := P;
           end;

          //CleanMatrix;
          CleanMatrix(False);

          // no wait turns
          EnablePlayerMatrix(Self.ID,0, True);

          // wait for server
          Exit;
        end;

        if Self.ID = P.ID then
          begin
            // update confirmation button
            DisableConfirmationButton;

            // update next turn
            P.Turn := StrToInt(AMessage[4]);
            FExperiment.Player[Self.ID] := P;
          end
        else
          EnablePlayerMatrix(Self.ID,P.Turn+1, True);
      end;
    end;
  end;

  procedure NotifyPlayers;
  begin
    case FActor of
      gaPlayer:
          if FExperiment.PlayerFromID[Self.ID].Turn = 0 then
            begin
              EnablePlayerMatrix(Self.ID, 0, True);
              ShowPopUp('É sua vez! Clique sobre uma linha da matrix e confirme sua escolha.');
            end
          else
              ShowPopUp('Começou! Aguarde sua vez.');

    end;
  end;

  procedure OnEndCycle;
  var
    LConsequence : TConsequence;
  begin
    case FActor of
      gaPlayer:
        begin
          LConsequence := TConsequence.Create(nil,AMessage[1]);
          LConsequence.GenerateMessage(True);
          LConsequence.PresentPoints;
          LConsequence.PresentMessage;
        end;
    end;
  end;

  procedure ReceiveChat;
  begin
    FormMatrixGame.ChatMemoRecv.Lines.Append(('['+AMessage[1]+']: ')+AMessage[2]);
  end;

  procedure SayGoodBye;
  begin
    case FActor of
      gaPlayer:begin
        if Self.ID <> AMessage[1] then Exit;
        FormMatrixGame.Visible := False;
        FormChooseActor := TFormChooseActor.Create(nil);
        FormChooseActor.Style := K_LEFT;
        if FormChooseActor.ShowModal = 1 then
          begin
            FZMQActor.Request([K_RESUME,Self.ID]);
            FormMatrixGame.Visible := True;
          end
        else;
        FormChooseActor.Free;
      end;
    end;
  end;

  procedure ShowQuestion;
  begin
    case FActor of
      gaPlayer:FZMQActor.Request([
        FZMQActor.ID
        , ' '
        , GA_PLAYER+K_QUESTION
        , AskQuestion(AMessage[1])
      ]);
    end;
  end;
//
//  procedure ResumeActor;
//  begin
//    case FActor of
//      gaPlayer:begin
//
//      end;
//      gaAdmin:begin
//
//      end;
//    end;
//  end;


  //procedure QuestionMessages;
  //var
  //  LConsequence : TConsequence;
  //  i : integer;
  //  MID : string;
  //begin
  //  case FActor of
  //    //  AMessage[i] :=
  //    //  S             + '+' +
  //    //  IntToStr(Pts) +'|'+
  //    //  GetConsequenceStylesString(LCsqStyle)   +'|'+
  //    //  ExtractDelimited(3,LConsequence, ['|']) +'|'+
  //    //  ExtractDelimited(4,LConsequence, ['|']) +'|'+
  //    //  ExtractDelimited(5,LConsequence, ['|']);
  //    gaPlayer:begin
  //        if AMessage.Count > 1 then
  //          begin
  //            for i := 1 to AMessage.Count -1 do
  //              begin
  //                MID := ExtractDelimited(1,AMessage[i],['+']);
  //                if (MID = 'M') or (MID = Self.ID) then
  //                  begin
  //                    LConsequence := TConsequence.Create(FormMatrixGame,ExtractDelimited(2,AMessage[i],['+']));
  //                    //LConsequence.PlayerNicname := P.Nicname;
  //                    ShowPopUp(LConsequence.PointMessage(MID = 'M'));
  //                    while FormMatrixGame.PopupNotifier.Visible do
  //                      Application.ProcessMessages;
  //
  //                    {$IFDEF DEBUG}
  //                    WriteLn('A consequence should have shown.');
  //                    {$ENDIF}
  //                  end;
  //              end;
  //          end;
  //    end;
  //  end;
  //end;


begin
  if MHas(K_ARRIVED) then ReceiveActor;
  if MHas(K_CHAT_M)  then ReceiveChat;
  if MHas(K_CHOICE)  then ReceiveChoice;
  if MHas(K_MESSAGE) then ShowPopUp(AMessage[1]);
  if MHas(K_KICK)    then SayGoodBye;
  if MHas(K_START) then NotifyPlayers;
  if MHas(K_CYCLES) then OnEndCycle;
  //if MHas(K_QUESTION) then ShowQuestion;
  //if MHas(K_QMESSAGE) then  QuestionMessages;
end;

// Here FActor is garanted to be a TZMQAdmin
procedure TGameControl.ReceiveRequest(var ARequest: TStringList);
  function MHas(const C : UTF8string) : Boolean;
  begin
    Result := MessageHas(C,ARequest, 2);
  end;

  procedure ReplyLoginRequest;
  var i : integer;
      P : TPlayer;
      TS,
      PS : string;
  begin
    if not FExperiment.PlayerIsPlaying[ARequest[0]] then
      begin
        if FExperiment.PlayersCount < FExperiment.Condition[FExperiment.CurrentCondition].Turn.Value then
          begin
            // ok, let player login
            P.ID := ARequest[0];

            // check if we already know this player
            i := FExperiment.PlayerIndexFromID[P.ID];
            if i > -1then
              begin
                // then load p data
                P := FExperiment.Player[i]
              end
            else
              begin
                // if not then generate and save p data
                i := FExperiment.AppendPlayer;
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
            CreatePlayerBox(P,False);

            // Request is now a reply with the following standard:
            // [Requester.ID 0, ' ' 1, ReplyTag 2, PlayerData 3, PlayersPlaying 4 .. n, ChatData Last]
            ARequest[2] := GA_ADMIN+ARequest[2]+K_ARRIVED;

            // player
            PS := FExperiment.PlayerAsString[P];
            //ARequest.Append(PS);

            // append current players playing
            if FExperiment.PlayersCount > 0 then
              for i:=0 to FExperiment.PlayersCount -1 do
                if FExperiment.Player[i].ID <> P.ID then
                  begin
                    TS := FExperiment.PlayerAsString[FEXperiment.Player[i]];
                    ARequest.Append(TS);  // FROM 3 to COUNT-2
                  end;

            // append chat data if allowed at the last position
            if FExperiment.SendChatHistoryForNewPlayers then
              ARequest.Append(FormMatrixGame.ChatMemoRecv.Lines.Text) // LAST
            else
              ARequest.Append('[CHAT]'); // must append something to keep the message envelop standard

            // inform all players about the new player, including itself
            FZMQActor.SendMessage([K_ARRIVED,PS]);

            // start Experiment
            if ShouldStartExperiment then
              StartExperiment;

          end
        else
          begin
            ARequest[2] := GA_ADMIN+ARequest[2]+K_REFUSED+K_FULLROOM;
          end;
      end
    else
      begin
        ARequest[2] := GA_ADMIN+ARequest[2]+K_REFUSED+K_PLAYING;
      end;
  end;

  procedure ValidateChoice;
  var P : TPlayer;
      S : string;
  begin
    {$IFDEF DEBUG}
    WriteLn('Count:>>>>>>>>>>>>>>>>>>>>>>>>>>>',FExperiment.Condition[FExperiment.CurrentCondition].Turn.Count, '<', FExperiment.Condition[FExperiment.CurrentCondition].Turn.Value);
    {$ENDIF}
    P := FExperiment.PlayerFromID[ARequest[0]];
    P.Choice.Row:= GetRowFromString(ARequest[3]); // row
    P.Choice.Color:= GetGameColorFromString(ARequest[4]); // color
    ARequest[2] := K_CHOICE+K_ARRIVED;

    //individual consequences
    S := FExperiment.ConsequenceStringFromChoice[P];
    {$IFDEF DEBUG}
    WriteLn('ValidateChoice:',s);
    {$ENDIF}

    if Pos('$NICNAME',S) > 0 then
      S := ReplaceStr(S,'$NICNAME',P.Nicname);
    ARequest.Append(S);

    // update turn
    P.Turn := FExperiment.NextTurn;
    FExperiment.Player[P.ID] := P;

    // broadcast choice
    FZMQActor.SendMessage([K_CHOICE,P.ID,ARequest[3],ARequest[4],IntToStr(P.Turn)]);

    if ShouldEndCycle then
      begin
        while FormMatrixGame.PopupNotifier.Visible do
          Application.ProcessMessages;

        //if ShouldAskQuestion then  // TODO: prompt only when an odd row was selected
        //  begin
        //    P.Turn := 0;
        //    FZMQActor.SendMessage([K_QUESTION,FExperiment.Condition[FExperiment.CurrentCondition].Prompt.Question]);
        //  end;
      end;
  end;

  //procedure ValidateQuestionResponse;
  //var
  //  P : TPlayer;
  //  M : array of UTF8string;
  //  i : integer;
  //  LPromptConsequences : TStringList;
  //begin
  //  P := FExperiment.PlayerFromID[ARequest[0]];
  //  ARequest[2] := K_QUESTION+K_ARRIVED;
  //
  //  // append response of each player
  //  FExperiment.Condition[FExperiment.CurrentCondition].Prompt.AppendResponse(P.ID,ARequest[3]);
  //
  //  // return to experiment and present the prompt consequence, if any
  //  if FExperiment.Condition[FExperiment.CurrentCondition].Prompt.ResponsesCount = Experiment.PlayersCount then
  //    begin
  //      // M setup
  //
  //
  //      // generate messages
  //      LPromptConsequences := FExperiment.Condition[FExperiment.CurrentCondition].Prompt.AsString;
  //      if LPromptConsequences.Count > 0 then
  //        begin
  //          SetLength(M, 1+LPromptConsequences.Count);
  //          M[0] := GA_ADMIN+K_QUESTION+K_QMESSAGE;
  //          for i := 0 to LPromptConsequences.Count -1 do
  //            M[i+1] := LPromptConsequences[i]
  //        end;
  //
  //       // send identified messages; each player takes only its own message and ignore the rest
  //      FZMQActor.SendMessage(M);
  //    end;
  //end;
begin
  if MHas(K_LOGIN) then ReplyLoginRequest;
  if MHas(K_CHOICE) then ValidateChoice;
  //if MHas(K_QUESTION) then ValidateQuestionResponse;
end;

// Here FActor is garanted to be a TZMQPlayer, reply by:
// - sending private data to player player
// - sending data from early history to income players
procedure TGameControl.ReceiveReply(AReply: TStringList);
  function MHas(const C : UTF8string) : Boolean;
  begin
    Result := MessageHas(C,AReply,2);
  end;

  procedure LoginAccepted;
  var
    i: integer;
    P : TPlayer;
  begin
    if Self.ID = AReply[0] then
      begin
        for i:= 3 to AReply.Count -2 do
          begin
            P := FExperiment.PlayerFromString[AReply[i]];
            FExperiment.AppendPlayer(P);
            CreatePlayerBox(P, False);
          end;

        // add chat
        FormMatrixGame.ChatMemoRecv.Lines.Clear;
        FormMatrixGame.ChatMemoRecv.Lines.Add(AReply[AReply.Count-1]);
      end
    else
      begin
      {$IFDEF DEBUG}
        WriteLn(Self.ID +' sent but' + AReply[0]  +' received. <<<<<<<<<<<<<<<<<<<<<<< This must not occur >>>>>>>>>>>>>>>>>>>>>>>>>>');
      {$ENDIF}
      end;
  end;

  procedure ChoiceValidated;
  var
    LConsequence : TConsequence;
    LCount,
    i : integer;
    M : string;
    //P : TPlayer;
  begin
    if Self.ID = AReply[0] then
      begin
        //P := FExperiment.PlayerFromID[Self.ID];
        LCount := WordCount(AReply[5],['+']);
        {$IFDEF DEBUG}
        WriteLn('LCount:',LCount);
        {$ENDIF}
        if LCount > 0 then
          for i := 1 to LCount do
            begin
              LConsequence := TConsequence.Create(nil,ExtractDelimited(i,AReply[5],['+']));
              M := LConsequence.GenerateMessage(False);
              if LConsequence.ShouldPublishMessage then
                FZMQActor.SendMessage([K_MESSAGE,M])
              else
                LConsequence.PresentMessage;
              LConsequence.PresentPoints;
              {$IFDEF DEBUG}
              WriteLn('A consequence should have shown.');
              {$ENDIF}
            end;

      end;
  end;

  //procedure QuestionValidated;
  //begin
  //  // wait
  //end;

  procedure ResumePlayer;
  begin

  end;

begin
  if MHas(K_RESUME+K_ARRIVED) then ResumePlayer;
  if MHas(K_LOGIN+K_ARRIVED) then LoginAccepted;
  if MHas(K_CHOICE+K_ARRIVED) then ChoiceValidated;
  //if MHas(K_QUESTION+K_ARRIVED) then QuestionValidated;
end;



end.

