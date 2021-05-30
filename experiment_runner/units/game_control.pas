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

    //procedure NextConditionSetup;
    procedure NextGenerationSetup(AID : string);
  protected
    function GetID : string; overload;
    procedure StartExperiment(Sender : TObject); override;
    procedure StartCondition(Sender : TObject); override;
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
  K_ARRIVED    = '.Arrived';
  K_CHAT_M     = '.ChatM';
  K_CHOICE     = '.Choice';
  K_MESSAGE    = '.Message';
  K_GMESSAGE   = '.GMessage';
  K_START      = '.Start';
  K_RESUME     = '.Resume';
  K_GENERATION = '.Generation';
  K_LOGIN      = '.Login';
  K_QUESTION   = '.Question';
  K_QMESSAGE   = '.QMessage';
  K_MOVQUEUE   = '.Queue';
  K_END        = '.EndX';
  K_NXTCND     = '.NextCond';
  K_TO         = '.To';
  K_CONTINUE   = '.Continue';

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
  , game_file_methods
  , presentation_classes
  , string_methods
  , helpers
  ;

{ TGameControl }

procedure TGameControl.StartExperiment(Sender : TObject);
var
  LMessage : array of string;
  LFilenames : array of string;
  i : Integer;
  LCount : integer;
  LStringList : TStringList;
  procedure Append(AItem : string);
  begin
    SetLength(LMessage, Length(LMessage)+1);
    LMessage[High(LMessage)] := AItem;
  end;
begin
  SetLength(LMessage, 0);
  if FActor = gaAdmin then begin
    // Topic
    Append(K_START);                                           // 0

    // Delimiter
    Append(#32);                                               // 1

    // Media Files
    LFilenames := FindMediaFiles;
    LCount := Length(LFilenames);
    Append(LCount.ToString);                                   // 2
    if LCount > 0 then begin
      LStringList := TStringList.Create;
      try
        for i := 0 to LCount -1 do begin                      // 3 .. n
          LStringList.LoadFromFile(LFilenames[i]);
          Append(ExtractFileName(LFilenames[i]) + '|' + LStringList.Text);
        end;
      finally
        LStringList.Free;
      end;
    end;

    // Experiment
    Append(FExperiment.AsString);                              // count -3

    // chat or dummy data to keep message envelop with a standard size
    if FExperiment.ShowChat and Assigned(FGameBoard.Chat) then begin
      if FExperiment.SendChatHistoryForNewPlayers then begin
        Append(FGameBoard.Chat.Text);
      end else begin
        Append('[CHAT]');
      end;
    end else begin
      Append('[NOCHAT]');                                      // count -2
    end;

    // Turns
    Append(FExperiment.Turns);                                // count -1

    FZMQActor.SendMessage(LMessage);
  end;
  inherited StartExperiment(Sender);
end;

procedure TGameControl.StartCondition(Sender : TObject);
var
  A, B, G1, G2 : integer;
  P : TPlayer;
begin
  A  := FExperiment.CurrentCondition.Points.OnStart.A;
  B  := FExperiment.CurrentCondition.Points.OnStart.B;
  G1 := FExperiment.CurrentCondition.Points.OnStart.G1;
  G2 := FExperiment.CurrentCondition.Points.OnStart.G2;
  if G1 > 0 then
    FExperiment.IncMetaPoints(gscG1, G1);

  if G2 > 0 then
    FExperiment.IncMetaPoints(gscG2, G2);

  for P in FExperiment.Players do
    begin
      if A > 0 then
        FExperiment.IncPlayerPoints(gscA,A,P.ID);

      if B > 0 then
        FExperiment.IncPlayerPoints(gscB,B,P.ID);
    end;
  inherited StartCondition(Sender);
  FGameBoard.InvalidateLabels(Self.ID);
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

  // copy immutable vars
  LNewPlayer.Turn := LOldPlayer.Turn;
  LNewPlayer.Index := LOldPlayer.Index;

  // override mutable vars
  FExperiment.Player[LOldPlayer.ID] := LNewPlayer;

  // move left the exit player queue
  FExperiment.MovePlayersQueueLeft;

  if Self.ID = LOldPlayer.ID then begin
    FZMQActor.UpdateID(LNewPlayer.ID);
  end;

  FGameBoard.UpdatePlayerBox(LNewPlayer,
    Self.ID = LNewPlayer.ID, FActor=gaAdmin, LOldPlayer.ID);

  //if FExperiment.ConditionMustBeUpdated then begin
  //  FExperiment.ConditionMustBeUpdated := False;
  //  NextConditionSetup;
  //end;
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

procedure TGameControl.NextGenerationSetup(AID: string); // [player_points]
var
  A : integer;
  B : integer;

  procedure IncrementOnStartPoints(APlayerID : string; AA, AB : integer);
  var
    P  : TPlayer;
  begin
    P := FExperiment.PlayerFromID[AID];
    if AA > 0 then
      Inc(P.Points.A, AA);

    if AB > 0 then
      Inc(P.Points.B, AB);
    FExperiment.PlayerFromID[AID] := P;
  end;

begin
  with FExperiment.CurrentCondition.Points do
    begin
      A := OnStart.A;
      B := OnStart.B;
    end;

  case FActor of
    gaPlayer: begin
      if Self.ID = AID then begin
        IncrementOnStartPoints(AID, A, B);
      end;
    end;

    gaAdmin: begin
      IncrementOnStartPoints(AID, A, B);
    end;

    else
      { do nothing };
  end;
  FGameBoard.InvalidateLabels(AID);
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
    K_LOGIN, K_CONTINUE : SetM([
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

  procedure ResumeGame;
  begin
    case FActor of
      gaPlayer: begin
        // player next choice
        if Self.AsPlayer.Turn = FExperiment.CurrentTurn then begin
          if Assigned(OnStartChoice) then begin
            OnStartChoice(Self);
          end;
        end else begin
        // player is waiting his choice

        end;
      end;

      else
        { do nothing };
    end;
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
    LCondition : string;
    LGeneration : string;
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

            // check if we need to increment condition
            LGeneration := FExperiment.NextGeneration;
            Lcondition := FExperiment.NextCondition;

            // update players with server generated random turns
            if AMessage[4] <> #32 then
              FExperiment.UpdatePlayerTurns(AMessage[4]);

            // wait for server end cycle timing
            if Assigned(OnWaitForServer) then
              OnWaitForServer(Self);

            if FExperiment.IsStartCondition then begin
              if FExperiment.HasSlidesToShow then begin
                if FGameBoard.ShowSlides(FExperiment.ConditionSlides) then begin
                  SendRequest(K_CONTINUE, []);
                  { and wait for server }
                end;
              end;
            end;

            Exit;
          end;

          if Self.ID = P.ID then begin
            // player self choice
            if Assigned(OnEndChoice) then begin
              OnEndChoice(Self);
            end;
          end else begin
            ResumeGame;
          end;

      end;
     else { do nothing };
    end;
  end;

  procedure NotifyPlayers;
  var
    LRootFolder : string;
    LFilename   : string;
    LCount : integer;
    i : integer;
  begin
    case FActor of
      gaPlayer: begin
        // setup a different root folder for each participant
        LRootFolder := FExperiment.PlayerRootFolderFromID(Self.ID);

        // if there are any slides save them to cache
        LCount := StrToIntDef(AMessage[2], 0);
        if LCount > 0 then begin
          for i := 0 to LCount - 1 do begin
            LFilename := LRootFolder + 'media';
            SaveToCache(
              LFilename+ExtractDelimited(1,AMessage[i+3],['|']),
              ExtractDelimited(2,AMessage[i+3],['|']));
          end;
        end;

        // Experiment Config
        LFilename := LRootFolder + 'experiment.ini';
        LFilename := SaveToCache(LFilename, AMessage[AMessage.Count-3]);
        FExperiment.LoadFromFile(LFilename);

        // set global configs
        //FExperiment.ABPoints := StrToBool(AReply[AReply.Count-2]);
        FGameBoard.SetLabels;
        FGameBoard.SetMatrix;

        // chat Setup
        FGameBoard.SetupChat(AMessage[AMessage.Count-2]);



        // start experiment for players
        if FExperiment.ShouldStartExperiment then begin
          FExperiment.Turns := AMessage[AMessage.Count-1];
          FExperiment.Play;
          if FExperiment.HasSlidesToShow then begin
            if FGameBoard.ShowSlides(FExperiment.ConditionSlides) then begin
              SendRequest(K_CONTINUE, []);
            { and wait for server }
            end;
          end else begin
            if Self.AsPlayer.Turn = FExperiment.CurrentTurn then begin
              if Assigned(OnStartChoice) then begin
                OnStartChoice(Self);
              end;
            end else begin
              FGameBoard.ShowSystemPopUp(gmcExperimentStart);
            end;
          end;
        end;
      end;

      gaAdmin:
        FGameBoard.ShowSystemPopUp(gmcExperimentStart);

      else
        { do nothing };
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
          if FExperiment.HasGenerationSlidesToShow then begin
            if GameBoard.SayGoodByeAndShowSlides(AID,
              K_LEFT, FExperiment.GenerationSlides) then begin
              FZMQActor.Request([AID,' ',K_GENERATION]);
            end;
          end else begin
            if GameBoard.PlayerSaidGoodBye(AID, K_LEFT) then begin
              FZMQActor.Request([AID,' ',K_GENERATION]);
            end;
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
          //FZMQActor.Request([AID,' ',K_END]);

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

  procedure GenerationChange;
  var
    P : TPlayer;
    LPlayerLeaving : string;
  begin
    LPlayerLeaving := AMessage[1];
    case FActor of
      gaPlayer: begin
        SayGoodBye(LPlayerLeaving);
      end;

      gaAdmin: begin
        P := FExperiment.PlayerFromID[LPlayerLeaving];
        if Assigned(OnPlayerExit) then begin
          OnPlayerExit(P, LPlayerLeaving);
        end;
        GameBoard.ShowSystemPopUp(gmcPlayerExited, LPlayerLeaving);
      end;

      else
        { do nothing };
    end;
  end;

  //procedure QuestionMessages;
  //var
  //  P : TPlayer;
  //  i : integer;
  //  MID : string;
  //  LQConsequence : string;
  //{$IFDEF TEST_MODE}
  //  { do nothing }
  //{$ELSE}
  //  LTime : integer;
  //{$ENDIF}
  //begin
  //  if AMessage[2] <> #27 then begin
  //    if AMessage.Count > 1 then begin
  //      // present only one popup with all messages
  //      LQConsequence := '';
  //      for i := 3 to AMessage.Count -1 do begin
  //          MID := ExtractDelimited(1,AMessage[i],['#']);
  //          P := FExperiment.PlayerFromID[MID];
  //          LQConsequence += DeduceNicname(ShowConsequence(MID, ExtractDelimited(2,AMessage[i],['#']),MID = 'M',False),P)+LineEnding;
  //      end;
  //    {$IFDEF TEST_MODE}
  //      FGameBoard.DebugMessage(LQConsequence);
  //    {$ELSE}
  //      if LQConsequence <> '' then begin
  //        if AMessage.Count > 1 then
  //          LTime := GLOBAL_MESSAGES_INTERVAL*AMessage.Count
  //        else
  //          LTime:= GLOBAL_MESSAGE_INTERVAL;
  //
  //        GameBoard.ShowPopupNotifierHack(LQConsequence, LTime);
  //      end;
  //    {$ENDIF}
  //    end;
  //    ResumeGame;
  //end;

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

  //if MHas(K_QMESSAGE) then
  //  QuestionMessages;

  if MHas(K_GENERATION) then
    GenerationChange;

  if MHas(K_CONTINUE) then
    ResumeGame;

  if MHas(K_RESUME) then
    ResumeGame;

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
          P.Points.G1:=0;
          P.Points.G2:=0;
          P.Data := nil;
          P.Status:=gpsPlaying;
          P.Choice.Color:=gcNone;
          P.Choice.Row:=grNone;
          P.Index := i;
          P.Turn := FExperiment.FirstTurn[i];
          FExperiment.Player[i] := P;
        end;
        // create/config playerbox
        FGameBoard.CreatePlayerBox(P,False,True);


        // append new player
        ARequest.Append(FExperiment.PlayerAsString[P]);  // 3

        // append current players playing
        if FExperiment.PlayersCount > 0 then
          for i:=0 to FExperiment.PlayersCount -1 do
            if FExperiment.Player[i].ID <> P.ID then begin
              PS := FExperiment.PlayerAsString[FExperiment.Player[i]];
              ARequest.Append(PS);                       // 4 .. 5
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
    i       : integer;
    LCount  : integer;
    LPlayer : TPlayer;
    LConsequenceS : string;
    LTurns : string;
    LConsequence : TConsequence;
    LQuestion   : string;
    LGeneration : string;
    LCondition  : string;
    LAnnouncer : TIntervalarAnnouncer;
  begin
    LPlayer := FExperiment.PlayerFromID[ARequest[0]];            // 0 = ID, 1 = #32
    LPlayer.Choice.Row:= GetRowFromString(ARequest[3]);          // 3 row
    LPlayer.Choice.Color:= GetGameColorFromString(ARequest[4]);  // 4 color
    ARequest[2] := K_CHOICE+K_ARRIVED;                     // 2 message topic

    // generate individual consequences and update player
    LConsequenceS := FExperiment.ConsequenceStringFromChoice[LPlayer];

    // update turn
    LTurns := FExperiment.NextTurn;
    if LTurns <> #32 then begin
      FExperiment.UpdatePlayerTurns(LTurns);
    end;

    // inform all players about Player's (ARequest[0]) response
    FZMQActor.SendMessage(
      [K_CHOICE, ARequest[0], ARequest[3], ARequest[4], LTurns]);

    // The Announcer sends a message, waits interval time until
    // all messages have been sent and then destroys itself.
    LAnnouncer := TIntervalarAnnouncer.Create(nil);
    LAnnouncer.OnStart := @FZMQActor.SendMessage;
    LAnnouncer.Interval := GLOBAL_MESSAGE_INTERVAL;

    // individual consequences
    LCount := WordCount(LConsequenceS ,['+']);
    if LCount > 0 then
     for i := 1 to LCount do begin
         LConsequence :=
           TConsequence.Create(nil,ExtractDelimited(i,LConsequenceS,['+']));
         LAnnouncer.Append([K_MESSAGE,
           ARequest[0],
           ExtractDelimited(i,LConsequenceS,['+']),
           BoolToStr(False),
           BoolToStr(LConsequence.ShouldPublishMessage)]);
     end;
    LConsequence.Free;

    // if all participants played
    if FExperiment.IsEndCycle then begin
      // group consequences from choices of all players
      LConsequenceS := FExperiment.ConsequenceStringFromChoices;
      LCount := WordCount(LConsequenceS,['+']);
      if LCount > 0 then begin
        LAnnouncer.Append([K_GMESSAGE, LConsequenceS]);
      end;

      // #32 resume else NextGeneration = PlayerToKick AID
      LGeneration := FExperiment.NextGeneration;

      // #32 resume, #27 end experiment else NextConditionAsString
      LCondition := FExperiment.NextCondition;

      if LCondition = #27 then begin
        // end experiment
        LAnnouncer.Append([K_END, ARequest[0]]);
      end else begin

        if LGeneration = #32 then begin
          // resume next turn
          LAnnouncer.Append([K_RESUME, LCondition]);
        end else begin
          // change generation then resume
          LAnnouncer.Append([K_GENERATION, LGeneration, LCondition]);
        end;
      end;
    end;
    LAnnouncer.Reversed;
    LAnnouncer.Enabled := True;
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
      FExperiment.WriteRowPrompt;
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

  procedure ValidateGeneration;// old player becomes a new player
  var
    LNewPlayer : TPlayer;
    LOldPlayer : TPlayer;
    LN, LO : string;
  begin
    LOldPlayer := FExperiment.PlayerFromID[ARequest[0]];
    ARequest[2] := K_GENERATION+K_ARRIVED;
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

  procedure ValidateContinueGame;
  var
    P : TPlayer;
  begin
    ARequest[2] := K_CONTINUE+K_ARRIVED;
    P := FExperiment.PlayerFromID[ARequest[0]];
    P.Status := gpsFinishedReading;
    FExperiment.PlayerFromID[ARequest[0]] := P;
    if FExperiment.LastParticipantReadSlides then begin
      ARequest.Append(BoolToStr(True));
      ARequest.Append(FExperiment.Turns);
    end else begin
      ARequest.Append(BoolToStr(False));
    end;
  end;

  procedure ValidateEndExperiment;
  begin

  end;

begin
  if FExperiment.State = xsWaiting then begin
    if MHas(K_LOGIN) then
      ReplyLoginRequest;
  end;

  if FExperiment.State = xsRunning then begin
    if MHas(K_GENERATION) then
      ValidateGeneration;

    if MHas(K_CHOICE) then
      ValidateChoice;

    if MHas(K_QUESTION) then
      ValidateQuestionResponse;

    if MHas(K_CONTINUE) then
      ValidateContinueGame;

    if MHas(K_END) then
      ValidateEndExperiment;
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
    i: integer;
    P : TPlayer;
    //R0, R1, R2, R3, R4, R5 : string;
  begin
    if Self.ID = AReply[0] then begin
      FGameBoard.Fullscreen;
      //R0 := AReply[0]; // Player Requester ID   ( ID )
      //R1 := AReply[1]; //                       #32
      //R2 := AReply[2]; // Request Code          (Player.Login.Arrived.ToAdmin)
      //R3 := AReply[3]; // New Player            (PlayerAsString)
      // 4..5            // Other Players, if any (array of PlayerAsString)
      P := FExperiment.PlayerFromString[AReply[3]];
      FExperiment.AppendPlayer(P);
      FGameBoard.CreatePlayerBox(P, Self.ID = P.ID);

      // lets load already logged in players, if any
      for i:= 4 to AReply.Count -1 do
        begin
          P := FExperiment.PlayerFromString[AReply[i]];
          FExperiment.AppendPlayer(P);
          FGameBoard.CreatePlayerBox(P, False);
        end;


      // inform all players about the new player
      // self.id will ignore it
      FZMQActor.SendMessage([K_ARRIVED, AReply[3]]);
    end else begin
  {$IFDEF DEBUG}
      WriteLn(Self.ID + ' sent but' + AReply[0] +
        ' received. <<<<<<<<<< This must never occur >>>>>>>>>>>>>>');
  {$ENDIF}
    end;
  end;

  procedure ChoiceValidated;
  begin
    if Self.ID = AReply[0] then begin
      // do nothing
    end;
  end;

  procedure MoveQueue;
  begin
    // new player (AsString) and old player (AsString)
    FZMQActor.SendMessage([K_MOVQUEUE, AReply[3],AReply[4]]);
  end;

  procedure ContinueGame;
  begin
    if StrToBool(AReply[3]) then begin     // last participant finished
      FZMQActor.SendMessage([K_CONTINUE, AReply[4]]); // reading of condition slides
    end;
  end;

  procedure ResumeGame;
  begin
    FZMQActor.SendMessage([K_CONTINUE, AReply[4]]); // reading of condition slides
  end;

  procedure EndExperiment;
  begin

  end;

begin
  if MHas(K_CONTINUE+K_ARRIVED) then  // from condition slides
    ContinueGame;

  if MHas(K_RESUME+K_ARRIVED) then
    ResumeGame;

  if MHas(K_GENERATION+K_ARRIVED) then
    MoveQueue;

  if MHas(K_LOGIN+K_ARRIVED) then
    LoginAccepted;

  if MHas(K_CHOICE+K_ARRIVED) then
    ChoiceValidated;

  if MHas(K_END+K_ARRIVED) then
    EndExperiment;
end;

end.

