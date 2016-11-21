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
    function CanStartCycle : Boolean;
    function GetPlayerBox(AID:string) : TPlayerBox;
    function GetActorNicname(AID:string) : string;
    function GetSelectedColorF(AStringGrid : TStringGrid) : UTF8string;
    function GetSelectedRowF(AStringGrid : TStringGrid) : UTF8string;
    function MessageHas(const A_CONST : string; AMessage : TStringList): Boolean;
    procedure SetMatrixType(AStringGrid : TStringGrid; AMatrixType:TGameMatrixType;
      var ARowBase:integer; var ADrawDots, ADrawClear : Boolean);
    procedure ReceiveMessage(AMessage : TStringList);
    procedure ReceiveRequest(var ARequest : TStringList);
    procedure ReceiveReply(AReply : TStringList);
    procedure SetMustDrawDots(AValue: Boolean);
    procedure SetMustDrawDotsClear(AValue: Boolean);
    procedure SetRowBase(AValue: integer);
    procedure SendSystemMessage(AMessage: array of UTF8String);
  public
    constructor Create(AZMQActor : TZMQActor;AID : string);overload;
    destructor Destroy; override;
    procedure SetMatrix;
    procedure SendRequest(ARequest : UTF8string);
    procedure SendMessage(AMessage : UTF8string);
    property ID : string read FID;
    property RowBase : integer read FRowBase write SetRowBase;
    property MustDrawDots: Boolean read FMustDrawDots write SetMustDrawDots;
    property MustDrawDotsClear:Boolean read FMustDrawDotsClear write SetMustDrawDotsClear;
  end;

  function GetRowColor(ARow : integer;ARowBase:integer) : TColor;

const
  K_ARRIVED = '.Arrived';
  K_CHAT_M = '.ChatM';
  K_CHOICE = '.Choice';
  K_LEFT = '.Left';
  K_RESUME = '.Resume';
  K_DATA_A = '.Data';
  K_LOGIN = '.login';

  //
  K_STATUS = '.Status';
  K_CYCLES = '.OnCycleStart';

  //K_RESPONSE =

implementation

uses form_matrixgame, game_resources, string_methods, zhelpers;

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

function TGameControl.CanStartCycle: Boolean;
begin
  Result := FExperiment.PlayersPlaying.Count = FExperiment.Condition[FExperiment.CurrentCondition].Turn.Value;
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

function TGameControl.MessageHas(const A_CONST: string; AMessage: TStringList): Boolean;
begin
  Result := Pos(A_CONST,AMessage[0])>0;
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
      AStringGrid.Options := [goFixedHorzLine, goHorzLine, goDrawFocusSelected, goRowSelect];
      WriteGridFixedNames(AStringGrid, False);
    end;

  if gmColumns in AMatrixType then
    begin
      ARowBase := 1;
      AStringGrid.Clean;
      AStringGrid.FixedRows := 1;
      AStringGrid.RowCount := 11;
      AStringGrid.Height:=335;
      AStringGrid.Options := [goFixedHorzLine, goHorzLine, goDrawFocusSelected, goRowSelect, goVertLine];
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

procedure TGameControl.SendSystemMessage(AMessage: array of UTF8String);
begin
  TZMQAdmin(FZMQActor).SendMessage(AMessage);
end;

constructor TGameControl.Create(AZMQActor: TZMQActor; AID: string);
begin
  inherited Create(AZMQActor.Owner);
  FZMQActor := AZMQActor;
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

  FZMQActor.SetID(AID);
  FID := AID;

  FExperiment := TExperiment.Create(AZMQActor.Owner);
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
begin

end;


procedure TGameControl.SendMessage(AMessage: UTF8string);
var
{$IFDEF DEBUG}
  i : integer;
{$ENDIF}
  M : array of UTF8string;

  procedure SetM(A: array of UTF8String);
  var i : integer;
  begin
    SetLength(M,Length(A));
    for i := 0 to Length(A) -1 do
      M[i] := A[i];
  end;

begin
  case AMessage of
    K_ARRIVED : SetM([
      AMessage
      , FZMQActor.ID
      //, FZMQActor.ClassType.ClassName;
      //,
    ]);

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

    K_LEFT  : SetM([
      AMessage
      , FZMQActor.ID
    ]);

    K_RESUME  : SetM([
      AMessage
      , FZMQActor.ID
    ]);
  end;

  case FActor of
    gaAdmin: begin
      M[0] := GA_ADMIN+M[0];
    end;
    gaPlayer:begin
      M[0] := GA_PLAYER+M[0];
    end;
    //gaWatcher:begin // for now cannot SendMessages
    //  M[0] := GA_WATCHER+M[0];
  end;
  FZMQActor.SendMessage(M);

{$IFDEF DEBUG}
  for i := 0 to Length(M)-1 do
    WriteLn(M[i]);
{$ENDIF}
end;

procedure TGameControl.ReceiveMessage(AMessage: TStringList);

  function MHas(const C : string) : Boolean;
  begin
    Result := MessageHas(C,AMessage);
  end;

  procedure ReceiveActor;
  var i : integer;
      P : TPlayer;
  begin
    //if FExperiment.PlayerIsPlaying[AMessage[1]] then Exit;
    //if FExperiment.PlayersPlaying.Count < FExperiment.Condition[FExperiment.CurrentCondition].Turn.Value then
    //  begin
    //    if FExperiment.GenPlayersAsNeeded then
    //      if FExperiment.PlayerFromID[AMessage[1]].ID = '' then
    //        begin
    //          TPlayerBox.Create(FormMatrixGame.GBLastChoice,AMessage[1]).Parent := FormMatrixGame.GBLastChoice;
    //          i := FExperiment.AppendPlayer;
    //        end;
    //
    //    case FActor of
    //      gaPlayer:begin
    //        // nothing special
    //      end;
    //
    //      gaAdmin:begin
    //        P.ID := AMessage[1];
    //        P.Nicname := GenResourceName(i);
    //        P.Turn := FExperiment.NextTurn;
    //        FExperiment.Player[i] := P;
    //
    //        with GetPlayerBox(P.ID) do
    //          begin
    //            ID := P.ID;
    //            if FExperiment.PlayerFromID[ID].ID <> '' then
    //            begin
    //              Caption := FExperiment.PlayerFromID[ID].Nicname;
    //              Parent := FormMatrixGame.GBLastChoice;
    //              SendSystemMessage([ // here we need to use admin as a repeater/switch, because it is acting as a resource generator
    //                GA_ADMIN+K_STATUS
    //                , ID
    //                , Caption
    //                , IntToStr(P.Turn)
    //                , IntToStr(i)
    //              ]);
    //            end;
    //          end;
    //      end;
    //    end;
    //  end
    //else
    //  WriteLn('Room is full, Player must wait someone''s leaving.');
end;


  procedure ReceiveStatus;
  var P : PPlayer;
      i : integer;
  begin
    //P := New(PPlayer);
    //case FActor of
    //  gaPlayer:begin
    //    with P^ do
    //      begin // local asignment of the admin's generated data
    //        ID := AMessage[1];
    //        Nicname:=AMessage[2];
    //        Turn:= StrToInt(AMessage[3]);
    //      end;
    //    i := StrToInt(AMessage[4]);
    //    FExperiment.Player[i] := P^;
    //    with GetPlayerBox(P^.ID) do
    //      begin
    //        if Self.ID = ID then
    //          begin
    //            Caption := P^.Nicname + ' (Você)';
    //            WriteLn(P^.Nicname +' Said: I am ready.');
    //          end
    //        else
    //          begin
    //            Caption := P^.Nicname;
    //            WriteLn(Self.ID +' said '+ P^.Nicname +' is ready.');
    //          end;
    //        Enabled := True;
    //      end;
    //
    //  end;
    //
    //  gaAdmin:begin
    //    P^ := FExperiment.PlayerFromID[AMessage[1]];
    //    // turns by entrance order
    //    //P^.Turn := FExperiment.PlayersPlaying.Count;
    //    FExperiment.PlayersPlaying.Add(P);
    //    with GetPlayerBox(AMessage[1]) do
    //      Enabled := True;
    //
    //    WriteLn(AMessage[2]+' is ready.');
    //    if CanStartCycle then
    //      SendSystemMessage([
    //        GA_ADMIN+K_CYCLES
    //        , FExperiment.NextTurnPlayerID
    //      ]);
    //  end;
    //end;
    //Dispose(P);
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

      end;
      gaAdmin:begin

      end;
    end;
    WriteLn('Good Bye');
  end;

  procedure ResumeActor;
  begin
    case FActor of
      gaPlayer:begin

      end;
      gaAdmin:begin

      end;
    end;
    WriteLn('Resumed.');
  end;

  procedure ReceiveLogin;
  begin
    case FActor of
      gaPlayer:begin

      end;
      gaAdmin:begin

      end;
    end;
    WriteLn('login');
  end;

  procedure ReceiveLogout;
  begin
    case FActor of
      gaPlayer:begin

      end;
      gaAdmin:begin

      end;
    end;
    WriteLn('logout');
  end;

begin
  if MHas(K_ARRIVED) then ReceiveActor;
  if MHas(K_CHAT_M)  then ReceiveChat;
  if MHas(K_CHOICE)  then ReceiveChoice;
  if MHas(K_LEFT)    then SayGoodBye;
  if MHas(K_RESUME)  then ResumeActor;
  if MHas(K_STATUS) then ReceiveStatus;
end;

procedure TGameControl.ReceiveRequest(var ARequest: TStringList);
begin

end;

procedure TGameControl.ReceiveReply(AReply: TStringList);
begin

end;

end.

