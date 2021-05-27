{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_control;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls
  , game_zmq_actors
  , game_actors
  , game_control_events
  , game_experiment
  , game_visual_board
  ;

type

  { TGameControl }

  TGameControl = class(TGameEvents)
  private
    FGameBoard : TGameBoard;
    FCurrentCause : TGameConsequenceStyle;
    FActor : TGameActor;
    FZMQActor : TZMQActor;
    FExperiment : TExperiment;
    function GetActorNicname(AID:string) : string;
    procedure ReceiveMessage(AMessage : TStringList);
    procedure ReceiveRequest(var ARequest : TStringList);
    procedure ReceiveReply(AReply : TStringList);

    procedure MovePlayerQueue(ANewPlayerAsString,AOldPlayerAsString:string);
    procedure SetGameBoard(AValue : TGameBoard);
    function ShowConsequence(AID,S:string;IsMeta:Boolean;
      ShowPopUp : Boolean = True) : string;

    procedure NextConditionSetup(IsConditionStart:Boolean=False);
    procedure NextGenerationSetup(AID : string);
  protected
    function GetID : string; overload;
    procedure StartExperiment(Sender : TObject); override;
  public
    constructor Create(AOwner : TComponent; AActor : TGameActor); overload;
    destructor Destroy; override;
    function LoadFromFile(AFilename : string):Boolean;
    function AsPlayer : TPlayer;
    procedure Login;
    procedure Cancel;
    procedure SendRequest(ARequest : string; AInputData : array of string);
    procedure SendMessage(AMessage : string; AInputData : array of string);
    property ID : string read GetID;
    property Actor : TGameActor read FActor;
    property GameBoard : TGameBoard read FGameBoard write SetGameBoard;
    property Experiment : TExperiment read FExperiment write FExperiment;
  end;

// TODO: PUT NORMAL STRING MESSAGES IN RESOURCESTRING INSTEAD

const
  K_ARRIVED  = '.Arrived';
  K_CHAT_M   = '.ChatM';
  K_CHOICE   = '.Choice';
  K_MESSAGE  = '.Message';
  K_GMESSAGE = '.GMessage';
  K_START    = '.Start';
  K_RESUME   = '.Resume';
  K_LOGIN    = '.Login';
  K_QUESTION = '.Question';
  K_QMESSAGE = '.QMessage';
  K_MOVQUEUE = '.Queue';
  K_END      = '.EndX';
  K_NXTCND   = '.NextCond';
  K_TO       = '.To';

  //
  K_STATUS   = '.Status';
  K_LEFT     = '.Left';
  K_WAIT     = '.Wait';
  K_FULLROOM = '.Full';
  K_PLAYING  = '.Playing';
  K_REFUSED  = '.Refused';

implementation

uses
  LazUTF8
  , strutils
  , game_report
  , game_resources
  , game_actors_helpers
  , presentation_classes
  , string_methods
  ;

{ TGameControl }

procedure TGameControl.StartExperiment(Sender : TObject);
begin
  // gui setup
  // enable matrix grid for the first player
  if FActor = gaAdmin then begin
    FZMQActor.SendMessage([K_START, #32, FExperiment.Turns]);
  end;
  inherited StartExperiment(Sender);
end;

function TGameControl.GetActorNicname(AID: string): string;
begin
  Result := '';
  case FActor of
    gaPlayer: begin
      if FExperiment.Player[AID].ID <> '' then begin
        Result := FExperiment.Player[AID].Nicname;
      end else begin
        Exception.Create('TGameControl.GetActorNicname Exception');
      end;
    end;

    gaAdmin: Result := FExperiment.Researcher;
    else { do nothing };
  end;
end;

procedure TGameControl.MovePlayerQueue(ANewPlayerAsString,
  AOldPlayerAsString: string);
var
  LNewPlayer : TPlayer;
  LOldPlayer : TPlayer;
begin
  LNewPlayer := FExperiment.PlayerFromString[ANewPlayerAsString];
  LOldPlayer := FExperiment.PlayerFromString[AOldPlayerAsString];
  FExperiment.ArquiveOldPlayer(LOldPlayer);

  LNewPlayer.Turn := LOldPlayer.Turn;
  FExperiment.Player[LOldPlayer.ID] := LNewPlayer;
  FExperiment.MovePlayersQueueLeft;

  if Self.ID = LOldPlayer.ID then begin
    FZMQActor.UpdateID(LNewPlayer.ID);
  end;

  FGameBoard.UpdatePlayerBox(LNewPlayer,
    Self.ID = LNewPlayer.ID, FActor=gaAdmin, LOldPlayer.ID);

  if FExperiment.ConditionMustBeUpdated then begin
      FExperiment.ConditionMustBeUpdated := False;
      NextConditionSetup;
  end;
  NextGenerationSetup(LNewPlayer.ID);
  if FActor = gaPlayer then begin
    if Self.AsPlayer.Turn = FExperiment.CurrentTurn then begin
      if Assigned(OnStartChoice) then begin
        OnStartChoice(Self);
      end;
    end;
  end;
end;

procedure TGameControl.SetGameBoard(AValue : TGameBoard);
begin
  if FGameBoard = AValue then begin
    Exit;
  end;
  FGameBoard := AValue;
  FGameBoard.Experiment := FExperiment;
end;

function TGameControl.ShowConsequence(AID, S: string;
  IsMeta: Boolean; ShowPopUp: Boolean):string;
var
  LConsequence : TConsequence;
  LStyle : TGameConsequenceStyle;
begin
  Result := '';
  LConsequence := TConsequence.Create(nil,S);
  Result := LConsequence.GenerateMessage(IsMeta);
  for LStyle in LConsequence.Style do
    case LStyle of
      gscA, gscB :
        FExperiment.IncPlayerPoints(LStyle, LConsequence.AsInteger, AID);
      gscG1, gscG2 :
        FExperiment.IncMetaPoints(LStyle, LConsequence.AsInteger);
      else { do nothing };
    end;

  FGameBoard.ShowConsequence(Self.ID, AID, LConsequence, IsMeta, ShowPopUp);
  LConsequence.Free;
end;

// update player points
procedure TGameControl.NextConditionSetup(IsConditionStart: Boolean);
var
  A, B, G1, G2 : integer;
  P : TPlayer;
  LCause : string;
begin
  A  := FExperiment.CurrentCondition.Points.OnStart.A;
  B  := FExperiment.CurrentCondition.Points.OnStart.B;
  G1 := FExperiment.CurrentCondition.Points.OnStart.G1;
  G2 := FExperiment.CurrentCondition.Points.OnStart.G2;
  LCause := FExperiment.CurrentCondition.Picture1;

  // workaround to present points
  // TODO: send TExperiment to player through zmq and load from configuration directly
  if LCause = 'SUSTAINABLE' then
    FCurrentCause := gscG1;

  if LCause = 'NON-SUSTAINABLE' then
    FCurrentCause := gscG2;

  if IsConditionStart then
    for P in FExperiment.Players do
      begin
        if A > 0 then
          FExperiment.IncPlayerPoints(gscA,A,P.ID);

        if B > 0 then
          FExperiment.IncPlayerPoints(gscB,B,P.ID);
      end;

  if G1 > 0 then
    FExperiment.IncMetaPoints(gscG1, G1);

  if G2 > 0 then
    FExperiment.IncMetaPoints(gscG2, G2);

  FGameBoard.NextConditionSetup;
  FGameBoard.InvalidateLabels(Self.ID);
end;

procedure TGameControl.NextGenerationSetup(AID: string); // [player_points]
var
  A : integer;
  B : integer;
  P  : TPlayer;
begin
  with FExperiment.CurrentCondition.Points do
    begin
      A := OnStart.A;
      B := OnStart.B;
    end;

  case FActor of
    gaPlayer:
      if Self.ID = AID then begin
        P := FExperiment.PlayerFromID[AID];
        if A > 0 then
          Inc(P.Points.A, A);

        if B > 0 then
          Inc(P.Points.B, B);
        FExperiment.PlayerFromID[AID] := P;
      end;

    gaAdmin:
      begin
        P := FExperiment.PlayerFromID[AID];
        if A > 0 then
          Inc(P.Points.A, A);

        if B > 0 then
          Inc(P.Points.B, B);
        FExperiment.PlayerFromID[AID] := P;
      end;
    else { do nothing };
  end;
end;

function TGameControl.GetID: string;
begin
  Result := FZMQActor.ID;
end;

constructor TGameControl.Create(AOwner : TComponent; AActor : TGameActor);
var
  LZMQActor : TZMQActor;
begin
  inherited Create(AOwner);
  case AActor of
   gaNone: Exception.Create('TFormMatrixGame.SetGameActor. FActor = gaNone');
   gaAdmin:
     LZMQActor := TZMQAdmin.Create(Self, TZMQActor.NewRandomID);
   gaPlayer:
     LZMQActor := TZMQPlayer.Create(Self, TZMQActor.NewRandomID);
   gaWatcher:
     LZMQActor := TZMQWatcher.Create(Self, TZMQActor.NewRandomID);
 end;

  FZMQActor := LZMQActor;
  FZMQActor.OnMessageReceived:=@ReceiveMessage;
  FZMQActor.OnRequestReceived:=@ReceiveRequest;
  FZMQActor.OnReplyReceived:=@ReceiveReply;
  FZMQActor.Start;
  FActor := GameActor(FZMQActor);

  FExperiment := TExperiment.Create(Self, AActor);
  With FExperiment do begin
    OnConsequence := @Self.Consequence;
    OnEndCondition := @Self.EndCondition;
    OnEndCycle := @Self.EndCycle;
    OnEndExperiment := @Self.EndExperiment;
    OnEndGeneration := @Self.EndGeneration;
    OnEndTurn := @Self.EndTurn;
    OnInterlocking := @Self.Interlocking;
    OnStartCondition := @Self.StartCondition;
    OnStartCycle := @Self.StartCycle;
    OnStartExperiment := @Self.StartExperiment;
    OnStartGeneration := @Self.StartGeneration;
    OnStartTurn := @Self.StartTurn;
    OnTargetInterlocking := @Self.TargetInterlocking;
    //OnCleanEvent := @CleanEvent;
    //OnPlayerExit := @PlayerExit;
  end;
end;

destructor TGameControl.Destroy;
begin
  inherited Destroy;
end;

function TGameControl.LoadFromFile(AFilename: string): Boolean;
begin
  Result := FExperiment.LoadFromFile(AFilename);
end;

function TGameControl.AsPlayer : TPlayer;
begin
  Result := FExperiment.PlayerFromID[Self.ID];
end;

procedure TGameControl.Login;
begin
  SendRequest(K_LOGIN,[]); // admin cannot send requests
end;

// called from outside
procedure TGameControl.SendRequest(ARequest: string;
  AInputData: array of string);
var
  M : array of string;

  procedure SetM(A : array of string);
  var i : integer;
  begin
    SetLength(M, Length(A));
    for i := 0 to Length(A) -1 do
      M[i] := A[i];
  end;
begin
  SetLength(M, 0);
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
        , AInputData[0] // row
        , AInputData[1] // color
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
    else { do nothing };
  end;
  FZMQActor.Request(M);
end;

// called from outside
procedure TGameControl.SendMessage(AMessage: string;
  AInputData: array of string);
var
  M : array of string;

  procedure SetM(A : array of string);
  var i : integer;
  begin
    SetLength(M,Length(A));
    for i := 0 to Length(A) -1 do
      M[i] := A[i];
  end;
begin
  case AMessage of
    K_CHAT_M  : begin
      SetM([
        AMessage
        , GetActorNicname(FZMQActor.ID)
        , AInputData[0]  // chat ln
      ]);
    end;
    K_CHOICE  : SetM([
        AMessage
        , FZMQActor.ID
        , AInputData[0]  // row
        , AInputData[1]  // color
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
    else { do nothing };
  end;
  FZMQActor.SendMessage(M);
end;

procedure TGameControl.Cancel;
begin
  FExperiment.SaveToFile(FExperiment.Filename+'.calceled');
  FZMQActor.SendMessage([K_END, Self.ID]);
end;

// Here FActor can be TZMQPlayer or TZMQAdmin
procedure TGameControl.ReceiveMessage(AMessage: TStringList);
  function MHas(const C : string) : Boolean;
  begin
    Result := MessageHas(C,AMessage);
  end;

  procedure ReceiveActor;
  var P : TPlayer;
  begin
    case FActor of
      gaPlayer: begin
        P := FExperiment.PlayerFromString[AMessage[1]];
        if Self.AsPlayer.ID <> P.ID then begin
          FExperiment.AppendPlayer(P);
          FGameBoard.CreatePlayerBox(P, Self.ID = P.ID);
        end else begin
          FGameBoard.ShowSystemPopUp(gmcNewPlayerLoggedIn, P.ID);
        end;
      end;
      gaAdmin:
        begin
          FGameBoard.ShowSystemPopUp(gmcNewPlayerLoginArrived);
          // start experiment
          if FExperiment.ShouldStartExperiment then begin
            //Sleep(500);
            FExperiment.Play;
          end;
        end
      else { do nothing };
    end;
  end;

  procedure ShowQuestion;
  begin
    case FActor of
      gaPlayer:FZMQActor.Request([
        FZMQActor.ID
        , ' '
        , GA_PLAYER+K_QUESTION
        {$IFDEF TEST_MODE}
        , RandomFrom(['Y', 'N'])
        {$ELSE}
        , GameBoard.AskQuestion(AMessage[1])
        {$ENDIF}
        , AMessage[2] // generation
        , AMessage[3] // conditions
      ]);
      else { do nothing };
    end;

  end;

  procedure ReceiveChoice;
  var
    P : TPlayer;
  begin
    P := FExperiment.PlayerFromID[AMessage[1]];

    // add last responses to player box
    FGameBoard.InvalidatePlayerBox(P.ID, AMessage[2], AMessage[3]);

    case FActor of
      gaPlayer:
        begin
          // syncronize counters internally for players
          FExperiment.NextTurn;

          // last turn // end cycle
          if FExperiment.IsEndCycle then begin
            // update players with server generated random turns
            if AMessage[4] <> #32 then
              FExperiment.UpdatePlayerTurns(AMessage[4]);

            {$IFDEF TEST_MODE}
            FGameBoard.DebugMessage(
              'Experiment.Turns: ' + FExperiment.Turns);
            {$ENDIF}

            // wait for server end cycle timing
            if Assigned(OnWaitForServer) then
              OnWaitForServer(Self);

            Exit;
          end;

          if Self.ID = P.ID then begin
            // player self choice
            if Assigned(OnEndChoice) then begin
              OnEndChoice(Self);
            end;
          end else begin
            // player next choice
            if Self.AsPlayer.Turn = FExperiment.CurrentTurn then begin
              if Assigned(OnStartChoice) then begin
                OnStartChoice(Self);
              end;
            end else begin
            // is waiting his choice

            end;

          end;

      end;
     else { do nothing };
    end;
  end;

  procedure NotifyPlayers;
  begin
    NextConditionSetup(True);
    case FActor of
      gaPlayer: begin
        // start experiment
        if FExperiment.ShouldStartExperiment then begin
          FExperiment.Turns := AMessage[2];
          FExperiment.Play;

          if Self.AsPlayer.Turn = FExperiment.CurrentTurn then begin
            if Assigned(OnStartChoice) then begin
              OnStartChoice(Self);
            end;
          end else begin
            FGameBoard.ShowSystemPopUp(gmcExperimentStart);
          end;
        end;
      end;

      gaAdmin:
        FGameBoard.ShowSystemPopUp(gmcExperimentStart);
      else { do nothing };
    end;
  end;

  procedure ReceiveChat;
  var
    ALn: string;
  begin
    ALn := '['+AMessage[1]+']: '+AMessage[2];
    FGameBoard.AppendToChat(ALn);
    if FActor = gaAdmin then begin
      FExperiment.Report.WriteChatLn(ALn);
    end;
  end;

  procedure SayGoodBye(AID:string); // [player_points]
  begin
    case FActor of
      gaPlayer: begin
        if Self.ID = AID then begin
          if GameBoard.PlayerSaidGoodBye(AID, K_LEFT) then begin
            FZMQActor.Request([AID,' ',K_RESUME]);
          end;

        end else begin
          GameBoard.ShowSystemPopUp(gmcPlayerExited, AID);
        end;
      end;

      else
        { do nothing };
    end;
  end;

  procedure EndExperimentMessage(AID:string);
  begin
    case FActor of
      gaPlayer: begin
        if Assigned(OnWaitForServer) then begin
          OnWaitForServer(Self);
        end;

        if Self.ID = AID then begin
          FZMQActor.Request([AID,' ',K_RESUME]);

          if GameBoard.PlayerSaidGoodBye(AID, K_END) then
            { do nothing };

          if Assigned(OnEndExperiment) then
            OnEndExperiment(Self);

        end else begin
          GameBoard.ShowSystemPopUp(gmcPlayerExitedEnd, AID);
        end;
      end;

      gaAdmin :
        {Stop};

      else
        { do nothing };
    end;
  end;

  procedure ResumeNextTurn;
  var
    P : TPlayer;
  begin
    if AMessage[2] <> #27 then begin
      case FActor of
        gaPlayer: begin
          if AMessage[1] <> #32 then begin
            SayGoodBye(AMessage[1])
          end else begin
            if Self.AsPlayer.Turn = FExperiment.CurrentTurn then begin
              if Assigned(OnStartChoice) then begin
                OnStartChoice(Self);
              end;
            end;
          end;
        end;

        gaAdmin: begin
          if AMessage[1] <> #32 then
            begin
              P := FExperiment.PlayerFromID[AMessage[1]];
              if Assigned(OnPlayerExit) then begin
                OnPlayerExit(P, AMessage[1]);
              end;
              GameBoard.ShowSystemPopUp(gmcPlayerExited,AMessage[1]);
            end;
        end;

        else
          { do nothing };
      end;

      if AMessage[1] = #32 then begin
        //
        //  if AMessage[2] <> #32 then begin
        //    NextConditionSetup(AMessage[2]);
        //  end;
        //
      end else begin
        if AMessage[2] <> #32 then begin
          FExperiment.ConditionMustBeUpdated := True;
        end;
      end;

    end else begin
      EndExperimentMessage(AMessage[1]);
    end;
  end;

  procedure QuestionMessages;
  var
    P : TPlayer;
    i : integer;
    MID : string;
    LQConsequence : string;
  {$IFDEF TEST_MODE}
    { do nothing }
  {$ELSE}
    LTime : integer;
  {$ENDIF}
  begin
    if AMessage[2] <> #27 then begin
      if AMessage.Count > 1 then begin
        // present only one popup with all messages
        LQConsequence := '';
        for i := 3 to AMessage.Count -1 do begin
            MID := ExtractDelimited(1,AMessage[i],['#']);
            P := FExperiment.PlayerFromID[MID];
            LQConsequence += DeduceNicname(ShowConsequence(MID, ExtractDelimited(2,AMessage[i],['#']),MID = 'M',False),P)+LineEnding;
        end;
      {$IFDEF TEST_MODE}
        FGameBoard.DebugMessage(LQConsequence);
      {$ELSE}
        if LQConsequence <> '' then begin
          if AMessage.Count > 1 then
            LTime := GLOBAL_MESSAGES_INTERVAL*AMessage.Count
          else
            LTime:= GLOBAL_MESSAGE_INTERVAL;

          GameBoard.ShowPopupNotifierHack(LQConsequence, LTime);
        end;
      {$ENDIF}
      end;
      ResumeNextTurn;
    end else begin
      EndExperimentMessage(AMessage[1]);
    end;
  end;

  procedure ShowGroupedMessage(AMessage:string);
  var
    LCount : integer;
    i : integer;
    MID : string;
    LConsequence : string;
    LGConsequence : string;
  {$IFDEF TEST_MODE}
    { do nothing }
  {$ELSE}
    LTime : integer;
  {$ENDIF}
  begin
    // present only one popup with all messages
    LConsequence := '';
    LGConsequence := '';
    LCount := WordCount(AMessage,['+']);
    if LCount > 0 then
      for i := 1 to LCount do
        begin
          LConsequence := ExtractDelimited(i,AMessage,['+']);
          MID := ExtractDelimited(1,LConsequence,['#']);
          LGConsequence += ShowConsequence(MID,
            ExtractDelimited(2,LConsequence,['#']),MID = 'M',False)+LineEnding;
        end;
  {$IFDEF TEST_MODE}
    FGameBoard.DebugMessage(LGConsequence);
  {$ELSE}
    if LGConsequence <> '' then begin
      if LCount > 1 then
        LTime := GLOBAL_MESSAGES_INTERVAL*LCount
      else
        LTime:= GLOBAL_MESSAGE_INTERVAL;
      GameBoard.ShowPopupNotifierHack(LGConsequence, LTime);
    end;
  {$ENDIF}
  end;

begin
{
  AMessage[1] AID
}
  if MHas(K_ARRIVED) then
    ReceiveActor;

  if MHas(K_CHAT_M)  then
    ReceiveChat;

  if MHas(K_CHOICE)  then
    ReceiveChoice;

  if MHas(K_MESSAGE) then
    ShowConsequence(AMessage[1],AMessage[2],StrToBool(AMessage[3]));

  if MHas(K_GMESSAGE) then
    ShowGroupedMessage(AMessage[1]);

  if MHas(K_START) then
    NotifyPlayers;

  if MHas(K_QUESTION) then
    ShowQuestion;

  if MHas(K_MOVQUEUE) then
      MovePlayerQueue(AMessage[1],AMessage[2]);

  if MHas(K_QMESSAGE) then
    QuestionMessages;

  if MHas(K_RESUME) then
    ResumeNextTurn;

  if MHas(K_NXTCND) then
    NextConditionSetup(True);

  if MHAs(K_END) then
    EndExperimentMessage(AMessage[1]);
end;

// Here FActor is garanted to be a TZMQAdmin
procedure TGameControl.ReceiveRequest(var ARequest: TStringList);
  function MHas(const C : string) : Boolean;
  begin
    Result := MessageHas(C, ARequest, 2);
  end;

  procedure ReplyLoginRequest;
  var i : integer;
      P : TPlayer;
      PS : string;
  begin
    if not FExperiment.PlayerIsPlaying[ARequest[0]] then begin
      if FExperiment.PlayersCount < FExperiment.CurrentCondition.Turn.Value then
      begin
        // Request is now a valid reply
        ARequest[2] := ARequest[2]+K_ARRIVED+K_TO+GA_ADMIN;

        // set new player id
        P.ID := ARequest[0];

        // check if we already know this player
        i := FExperiment.PlayerIndexFromID[P.ID];
        if i > -1 then begin
            // then load known player data
            P := FExperiment.Player[i]
        end else begin
          // set new player data
          i := FExperiment.AppendPlayer;
          P.Nicname := FGameBoard.GetPlayerNicname(gmcNewPlayerLogin);
          P.Points.A:=0;
          P.Points.B:=0;
          P.Data := nil;
          P.Status:=gpsPlaying;
          P.Choice.Color:=gcNone;
          P.Choice.Row:=grNone;
          P.Turn := FExperiment.FirstTurn[i];
          FExperiment.Player[i] := P;
        end;
        // create/config playerbox
        FGameBoard.CreatePlayerBox(P,False,True);

        // append experiment
        ARequest.Append(FExperiment.AsString);

        // append chat data
        // append dummy data to keep message envelop with a standard size
        if FExperiment.ShowChat and Assigned(FGameBoard.Chat) then begin
          if FExperiment.SendChatHistoryForNewPlayers then begin
            ARequest.Append(FGameBoard.Chat.Text);
          end else begin
            ARequest.Append('[CHAT]');
          end;
        end else begin
          ARequest.Append('[NOCHAT]');
        end;

        // append new player
        ARequest.Append(FExperiment.PlayerAsString[P]);

        // append current players playing
        if FExperiment.PlayersCount > 0 then
          for i:=0 to FExperiment.PlayersCount -1 do
            if FExperiment.Player[i].ID <> P.ID then begin
              PS := FExperiment.PlayerAsString[FExperiment.Player[i]];
              ARequest.Append(PS);
            end;
        end else begin
          ARequest[2] := GA_ADMIN+K_REFUSED+K_FULLROOM;
        end;
      end
    else begin
      ARequest[2] := GA_ADMIN+ARequest[2]+K_REFUSED+K_PLAYING;
    end;
  end;

  procedure ValidateChoice;
  var
    P : TPlayer;
    S : string;
    T : string;
  begin
    P := FExperiment.PlayerFromID[ARequest[0]];            // 0 = ID, 1 = #32
    P.Choice.Row:= GetRowFromString(ARequest[3]);          // 3 row
    P.Choice.Color:= GetGameColorFromString(ARequest[4]);  // 4 color
    ARequest[2] := K_CHOICE+K_ARRIVED;                     // 2 message topic

    // generate individual consequences and update player
    S := FExperiment.ConsequenceStringFromChoice[P];

    // update turn
    T := FExperiment.NextTurn;
    if T <> #32 then begin
      FExperiment.UpdatePlayerTurns(T);
    end;

  {$IFDEF TEST_MODE}
    FGameBoard.DebugMessage(
      'Experiment.Turns: ' +
      StringReplace(T,'+', LineEnding, [rfReplaceAll]));
  {$ENDIF}
    ARequest.Append(T);                                            // 5

    // individual consequences
    ARequest.Append(S);                                            // 6

    // if all participants played
    if FExperiment.IsEndCycle then begin
      // group consequences from choices of all players
      ARequest.Append(FExperiment.ConsequenceStringFromChoices); // 7

      // prompt question if an odd row was selected
      S := FExperiment.ShouldAskQuestion;
      ARequest.Append(S);                                        // 8

      // #32 resume else NextGeneration = PlayerToKick AID
      ARequest.Append(FExperiment.NextGeneration);               // 9

      // Check if we need to end the current condition
      if S <> #32 then begin
        ARequest.Append(#32); // ValidateQuestionResponse
      end else begin
        ARequest.Append(FExperiment.NextCondition);              // 10
      end;
    end;
  end;

  procedure ValidateQuestionResponse;
  var
    P : TPlayer;
    M : array of string;
    i : integer;
    LPromptConsequences : TStringList;
  begin
    M := nil;
    P := FExperiment.PlayerFromID[ARequest[0]];
    ARequest[2] := K_QUESTION+K_ARRIVED;

    // append response of each player
    FExperiment.CurrentCondition.Prompt.AppendResponse(P.ID,ARequest[3]);

    // return to FExperiment and present the prompt consequence, if any
    if FExperiment.CurrentCondition.Prompt.ResponsesCount =
       FExperiment.PlayersCount then
    begin
      // generate messages
      LPromptConsequences := FExperiment.CurrentCondition.Prompt.AsString;
      FExperiment.Report.WriteRowPrompt;
      SetLength(M, 3+LPromptConsequences.Count);
      M[0] := K_QMESSAGE;
      M[1] := ARequest[4]; // generation envelop
      M[2] := FExperiment.NextCondition;

      if LPromptConsequences.Count > 0 then begin
        for i := 0 to LPromptConsequences.Count-1 do begin
          P := FExperiment.PlayerFromID[ExtractDelimited(1,LPromptConsequences[i],['+'])];
          LPromptConsequences[i] := DeduceNicname(LPromptConsequences[i],P);
        end;

        for i := 0 to LPromptConsequences.Count -1 do begin
          M[i+3] := LPromptConsequences[i]; // messages envelop
        end;
      end;
      FExperiment.Clean;
      // send identified messages; each player takes only its own message and ignore the rest
      FZMQActor.SendMessage(M);
    end;
  end;

  procedure ReplyResume;// old player becomes a new player
  var
    LNewPlayer : TPlayer;
    LOldPlayer : TPlayer;
    LN, LO : string;
  begin
    LOldPlayer := FExperiment.PlayerFromID[ARequest[0]];
    ARequest[2] := K_RESUME+K_ARRIVED;
    LNewPlayer := C_PLAYER_TEMPLATE;
    LNewPlayer.Data := TStringList.Create;
    LNewPlayer.Nicname := FGameBoard.GetPlayerNicname(gmcNewPlayerArrived);
    repeat
      LNewPlayer.ID := TZMQActor.NewRandomID;
    until FExperiment.ValidID(LNewPlayer.ID);
    LN := FExperiment.PlayerAsString[LNewPlayer];
    LO := FExperiment.PlayerAsString[LOldPlayer];
    ARequest.Append(LN); // 3
    ARequest.Append(LO); // 4
  end;

begin
  if FExperiment.State = xsWaiting then begin
    if MHas(K_LOGIN) then
      ReplyLoginRequest;
  end;

  if FExperiment.State = xsRunning then begin
    if MHas(K_RESUME) then
      ReplyResume;

    if MHas(K_CHOICE) then
      ValidateChoice;

    if MHas(K_QUESTION) then
      ValidateQuestionResponse;
  end;
end;

// Here FActor is garanted to be a TZMQPlayer, replying by:
// - sending private data to player
// - sending data from early history to income players
procedure TGameControl.ReceiveReply(AReply: TStringList);
  function MHas(const C : string) : Boolean;
  begin
    Result := MessageHas(C, AReply, 2);
  end;

  procedure LoginAccepted;
  var
    LPath : string;
    LStringList : TStringList;
    i: integer;
    P : TPlayer;
    //R0, R1, R2, R3, R4, R5 : string;
  begin
    if Self.ID = AReply[0] then begin
      //R0 := AReply[0]; // Player Requester ID   ( ID )
      //R1 := AReply[1]; //                       #32
      //R2 := AReply[2]; // Request Code          (Player.Login.Arrived.ToAdmin)
      //R3 := AReply[3]; // Experiment Config     (Experiment)
      //R4 := AReply[4]; // Chat Setup            ([NO CHAT] or Chat Setup)
      //R5 := AReply[5]; // New Player            (PlayerAsString)
      // 6..7            // Other Players, if any (array of PlayerAsString)
      P := FExperiment.PlayerFromString[AReply[5]];
      FExperiment.AppendPlayer(P);
      FGameBoard.CreatePlayerBox(P, Self.ID = P.ID);

      // lets load already logged in players, if any
      for i:= 6 to AReply.Count -1 do
        begin
          LPath := AReply[i];
          P := FExperiment.PlayerFromString[AReply[i]];
          FExperiment.AppendPlayer(P);
          FGameBoard.CreatePlayerBox(P, False);
        end;

      // Experiment Config
      if (FExperiment.State = xsNone) then
      begin
        LStringList := TStringList.Create;
        try
          LPath :=
            'cache'+PathDelim+'P'+FExperiment.PlayersCount.ToString+PathDelim;
          ForceDirectories(LPath);
          LPath := LPath + 'experiment.ini';
          LStringList.Clear;
          LStringList.Text := AReply[3];
          LStringList.SaveToFile(LPath);
          FExperiment.LoadFromFile(LPath);
        finally
          LStringList.Free;
        end;
      end;

      // set global configs
      //FExperiment.ABPoints := StrToBool(AReply[AReply.Count-2]);
      FGameBoard.SetLabels;
      FGameBoard.Fullscreen;
      FGameBoard.SetMatrix;

      // chat Setup
      FGameBoard.SetupChat(AReply[4]);

      // inform all players about the new player
      // self.id will ignore it
      FZMQActor.SendMessage([K_ARRIVED, AReply[5]]);
    end else begin
  {$IFDEF DEBUG}
      WriteLn(Self.ID + ' sent but' + AReply[0] +
        ' received. <<<<<<<<<< This must never occur >>>>>>>>>>>>>>');
  {$ENDIF}
    end;
  end;

  procedure ChoiceValidated;
  var
    LConsequence : TConsequence;
    LCount,
    i : integer;
    LAnnouncer : TIntervalarAnnouncer;
  begin
{   AReply
    0  : ID                       ex.: 10E96004A9-C6A0BC07CC2B
    1  : Message Delimiter        #32
    2  : Message Tag/Code         .Choice.Arrived
    3  : Chosen Row AsString      1
    4  : Chosen Color AsString    Y
    5  : TurnsAsString
    6  : Consequence
    7  : Meta Consequence
    8  : ShouldAskQuestion
    9  : NextGeneration
    10 : Fuzzy Logic for Ending
 }
    if Self.ID = AReply[0] then begin
        // inform other players about self.id choice
        FZMQActor.SendMessage(
          [K_CHOICE, AReply[0], AReply[3], AReply[4], AReply[5]]);

        // The Announcer sends a message, waits interval time until
        // all messages have been sent and then destroys itself.
        LAnnouncer := TIntervalarAnnouncer.Create(nil);
        LAnnouncer.OnStart := @FZMQActor.SendMessage;
        LAnnouncer.Interval := GLOBAL_MESSAGE_INTERVAL;

        // individual consequences
        LCount := WordCount(AReply[6],['+']);
        if LCount > 0 then
          for i := 1 to LCount do begin
              LConsequence :=
                TConsequence.Create(nil,ExtractDelimited(i,AReply[6],['+']));
              LAnnouncer.Append([K_MESSAGE,
                Self.ID,
                ExtractDelimited(i,AReply[6],['+']),
                BoolToStr(False),
                BoolToStr(LConsequence.ShouldPublishMessage)]);
          end;
        LConsequence.Free;

        if AReply.Count > 7 then begin
          // meta/ group consequence
          LCount := WordCount(AReply[7],['+']);
          if LCount > 0 then begin
            LAnnouncer.Append([K_GMESSAGE,AReply[7]]);
          end;

          // should ask question or just resume (going to the next turn)?
          if AReply[8] <> #32 then begin
            LAnnouncer.Append([K_QUESTION,AReply[8],AReply[9],AReply[10]])
          end else begin
            LAnnouncer.Append([K_RESUME,AReply[9],AReply[10]]);
          end;

          // should end FExperiment or go to the next condition?
          if (AReply[10] = #27) and (AReply[8] = #32) then begin
            LAnnouncer.Append([K_END, AReply[0]])
          end else begin
            if (AReply[10] <> #32) then begin
              LAnnouncer.Append([K_NXTCND,AReply[10]])
            end;
          end;
        end;

        LAnnouncer.Reversed;
        LAnnouncer.Enabled := True;
      end;
  end;

  procedure ResumePlayer;
  begin
    // new player (AsString) and old player (AsString)
    FZMQActor.SendMessage([K_MOVQUEUE, AReply[3],AReply[4]]);
  end;

begin
  if MHas(K_RESUME+K_ARRIVED) then
    ResumePlayer;

  if MHas(K_LOGIN+K_ARRIVED) then
    LoginAccepted;

  if MHas(K_CHOICE+K_ARRIVED) then
    ChoiceValidated;
end;

end.

