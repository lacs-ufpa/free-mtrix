unit game_control;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}

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
    FID: string;
    FMustDrawDots: Boolean;
    FMustDrawDotsClear: Boolean;
    FRowBase : integer;
    FActor : TGameActor;
    FZMQActor : TZMQActor;
    FExperiment : TExperiment;
    function GetPlayerBox(AID:string) : TPlayerBox;
    function GetActorNicname(AID:string) : string;
    function GetSelectedColorF(AStringGrid : TStringGrid) : UTF8string;
    function GetSelectedRowF(AStringGrid : TStringGrid) : UTF8string;
    function MessageHas(const A_CONST : string; AMessage : TStringList; I:ShortInt=0): Boolean;
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
    function CanStartExperiment : Boolean;
    procedure KickPlayer(AID:string);
    procedure StartCycle;
    procedure StartCondition;
    procedure StartExperiment;
    procedure StartTurn;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy; override;
    procedure SetMatrix;
    procedure SendRequest(ARequest : UTF8string);
    procedure SendMessage(AMessage : UTF8string);
    property Experiment : TExperiment read FExperiment write FExperiment;
    property ID : string read FID;
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
  K_LEFT     = '.Left';
  K_RESUME   = '.Resume';
  K_DATA_A   = '.Data';
  K_LOGIN    = '.Login';
  K_KICK     = '.Kick'
  //
  K_STATUS   = '.Status';
  K_CYCLES   = '.OnCycleStart';

  //K_RESPONSE =

implementation

uses LazUTF8, form_matrixgame, form_chooseactor, game_resources, string_methods, zhelpers;

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

function TGameControl.CanStartExperiment: Boolean;
begin
  Result := FExperiment.PlayersCount = FExperiment.Condition[FExperiment.CurrentCondition].Turn.Value;
end;

procedure TGameControl.KickPlayer(AID: string);
begin
  FZMQActor.SendMessage([K_KICK, AID]);
end;

function TGameControl.GetPlayerBox(AID: string): TPlayerBox;
var i : integer;
begin
  for i := 0 to FormMatrixGame.GBLastChoice.ComponentCount-1 do
    if TPlayerBox(FormMatrixGame.GBLastChoice.Components[i]).ID = AID then
      begin
        Result := TPlayerBox(FormMatrixGame.GBLastChoice.Components[i]);
        Break;
      end;
end;

function TGameControl.GetActorNicname(AID: string): string;
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

function TGameControl.MessageHas(const A_CONST: string; AMessage: TStringList;
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
      i1 := Integer(P.Choice.Last.Row);
      if i1 > 0 then
        LabelLastRowCount.Caption := Format('%-*.*d', [1,2,i1])
      else
        LabelLastRowCount.Caption := 'NA';
      PanelLastColor.Color := GetColorFromCode(P.Choice.Last.Color);
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
  Result := GetRowColorString(GetRowColor(AStringGrid.Selection.Top,RowBase));
end;

function TGameControl.GetSelectedRowF(AStringGrid: TStringGrid): UTF8string;
begin
  Result := IntToStr(AStringGrid.Selection.Top);
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

procedure TGameControl.StartCycle;
begin

end;

procedure TGameControl.StartCondition;
begin
  // append OnStart data
  //FExperiment.Condition[FExperiment.CurrentCondition].Points.OnStart.A;
  //FExperiment.Condition[FExperiment.CurrentCondition].Points.OnStart.B;
  //FExperiment.Condition[FExperiment.CurrentCondition].Points.OnStart.G;

  // append which player
end;

procedure TGameControl.StartExperiment;
begin

end;

procedure TGameControl.StartTurn;
begin
  FormMatrixGame.StringGridMatrix.Options := FormMatrixGame.StringGridMatrix.Options-[goRowSelect];
  FormMatrixGame.btnConfirmRow.Enabled:=True;
  FormMatrixGame.btnConfirmRow.Visible := False;
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
  M : array of UTF8String;

  procedure SetM(A : array of UTF8String);
  var i : integer;
  begin
    SetLength(M,Length(A));
    for i := 0 to Length(A) -1 do
      M[i] := A[i];
  end;
begin
  SetM([
    FZMQActor.ID
    , ' '
    , ARequest
  ]);

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
  M : array of UTF8String;

  procedure SetM(A : array of UTF8String);
  var i : integer;
  begin
    SetLength(M,Length(A));
    for i := 0 to Length(A) -1 do
      M[i] := A[i];
  end;
begin
  case AMessage of

    K_CHOICE  : SetM([
      AMessage
      , FZMQActor.ID
      , GetSelectedRowF(FormMatrixGame.StringGridMatrix)
      , GetSelectedColorF(FormMatrixGame.StringGridMatrix)
    ]);

    K_CHAT_M  : begin
      //if (FActor = gaAdmin) and (not FExperiment.ResearcherCanChat) then Exit;
      SetM([
        AMessage
        , GetActorNicname(FZMQActor.ID)
        , FormMatrixGame.ChatMemoSend.Lines.Text
      ]);
    end;

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

// Here FActor is garanted to be a TZMQPlayer
procedure TGameControl.ReceiveMessage(AMessage: TStringList);
  function MHas(const C : string) : Boolean;
  begin
    Result := MessageHas(C,AMessage);
  end;

  procedure ReceiveActor;
  var P : TPlayer;
  begin
    case FActor of
      gaAdmin:
        begin
          // do nothing
        end;

      gaPlayer:
        begin
          P := FExperiment.PlayerFromString[AMessage[1]];
          if Self.ID = P.ID then
            begin
              FExperiment.AppendPlayer(P);
              CreatePlayerBox(P, True);
            end
          else
            CreatePlayerBox(P,False);
        end;
    end;

  end;

  procedure ReceiveChoice;
  begin
    case FActor of
      gaPlayer:begin

      end;
      gaAdmin:begin

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

  procedure ResumeActor;
  begin
    case FActor of
      gaPlayer:begin

      end;
      gaAdmin:begin

      end;
    end;
  end;

  procedure ReceiveLogin;
  begin
    case FActor of
      gaPlayer:begin

      end;
      gaAdmin:begin

      end;
    end;
  end;

  procedure ReceiveLogout;
  begin
    case FActor of
      gaPlayer:begin

      end;
      gaAdmin:begin

      end;
    end;
  end;

begin
  if MHas(K_ARRIVED) then ReceiveActor;
  if MHas(K_CHAT_M)  then ReceiveChat;
  if MHas(K_CHOICE)  then ReceiveChoice;
  if MHas(K_KICK)    then SayGoodBye;
  if MHas(K_STATUS) then ReceiveStatus;
end;

// Here FActor is garanted to be a TZMQAdmin
procedure TGameControl.ReceiveRequest(var ARequest: TStringList);
  function MHas(const C : string) : Boolean;
  begin
    Result := MessageHas(C,ARequest, 2);
  end;

  procedure ReplyLoginRequest;
  var i : integer;
      P : TPlayer;
      TS,
      PS : UTF8string;
  begin
    if not FExperiment.PlayerIsPlaying[ARequest[0]] then
      begin
        {$IFDEF DEBUG}
          WriteLn(FExperiment.PlayersCount,'<',FExperiment.Condition[FExperiment.CurrentCondition].Turn.Value);
        {$ENDIF}
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
                // if not generate and save p data
                i := FExperiment.AppendPlayer;
                P.Nicname := GenResourceName(i);
                P.Turn := FExperiment.NextTurn;
                P.Points.A:=0;
                P.Points.B:=0;
                P.Status:=gpsPlaying;
                P.Choice.Current.Color:=gcNone;
                P.Choice.Current.Row:=grNone;
                P.Choice.Last.Color:=gcNone;
                P.Choice.Last.Row:=grNone;
                // turns by entrance order
                P.Turn := FExperiment.PlayersCount;
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

            // start Experiment if allowed
            if CanStartExperiment then
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

begin
  if MHas(K_LOGIN) then ReplyLoginRequest;
end;

// Here FActor is garanted to be a TZMQPlayer, should be used to send all wanted history for new income players
procedure TGameControl.ReceiveReply(AReply: TStringList);
  function MHas(const C : string) : Boolean;
  begin
    Result := MessageHas(C,AReply,2);
  end;

  procedure LoginAccepted;
  var
    i: integer;
    P : TPlayer;
  begin
    {$IFDEF DEBUG}
      WriteLn(Self.ID +' self');
      WriteLn(AReply[0] +' reply');
    {$ENDIF}
    if Self.ID = AReply[0] then
      begin
        for i:= 3 to AReply.Count -2 do
          begin
            P := FExperiment.PlayerFromString[AReply[i]];
            CreatePlayerBox(P, False);
          end;

        // add chat
        FormMatrixGame.ChatMemoRecv.Lines.Clear;
        FormMatrixGame.ChatMemoRecv.Lines.Add(AReply[AReply.Count-1]);
      end
    else
      begin
      {$IFDEF DEBUG}
        WriteLn(Self.ID +' sent but' + AReply[0]  +' received. This must not occur.');
      {$ENDIF}
      end;
  end;

begin
  if MHas(K_RESUME+K_ARRIVED) then ResumePlayer;
  if MHas(K_LOGIN+K_ARRIVED) then LoginAccepted;
end;



end.

