unit game_control;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, Graphics, Grids
  , game_zmq_actors
  , game_experiment
  , game_actors
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
    function GetActorNicname(AID:string; Brackets : Boolean = False) : string;
    function MessageHas(const A_CONST : string; AMessage : TStringList): Boolean;
    procedure SetMatrixType(AStringGrid : TStringGrid; AMatrixType:TGameMatrixType;
      var ARowBase:integer; var ADrawDots, ADrawClear : Boolean);
    procedure ReceiveMessage(AMessage : TStringList);
    function GetSelectedColorF(AStringGrid : TStringGrid) : UTF8string;
    function GetSelectedRowF(AStringGrid : TStringGrid) : UTF8string;
    procedure SetMustDrawDots(AValue: Boolean);
    procedure SetMustDrawDotsClear(AValue: Boolean);
    procedure SetRowBase(AValue: integer);
  public
    constructor Create(AZMQActor : TZMQActor); reintroduce;
    destructor Destroy; override;
    procedure SetID(S:string);
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

function TGameControl.GetActorNicname(AID: string; Brackets: Boolean): string;
var i : integer;
begin
  if FExperiment.PlayersCount > -1 then
    begin
      for i:= 0 to FExperiment.PlayersCount do
        if FExperiment.Player[i].ID = AID then
          begin
            if Brackets then
              Result := '['+FExperiment.Player[i].Nicname+']'
            else
              Result := FExperiment.Player[i].Nicname;
            Break;
          end
     end
  else
    begin
      WriteLn('TGameControl.GetActorNicname:Using Harcoded Nicame');
      Result := '[UNKNOWN]';
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

constructor TGameControl.Create(AZMQActor: TZMQActor);
begin
  inherited Create(AZMQActor.Owner);
  FZMQActor := AZMQActor;
  FZMQActor.SetID(ID);
  FZMQActor.OnMessageReceived:=@ReceiveMessage;
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

  {$IFDEF DEBUG}
  case FActor of
    gaAdmin:begin
      FExperiment := TExperiment.Create(AZMQActor.Owner);
    end;
    gaPlayer:begin

    end;
  end;
  {$ENDIF}
end;

destructor TGameControl.Destroy;
begin
  inherited Destroy;
end;

procedure TGameControl.SetID(S: string);
begin
  FID := S;
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
    ]);

    K_CHOICE  : SetM([
      AMessage
      , FZMQActor.ID
      , GetSelectedRowF(FormMatrixGame.StringGridMatrix)
      , GetSelectedColorF(FormMatrixGame.StringGridMatrix)
    ]);

    K_CHAT_M  : SetM([
      AMessage
      , GetActorNicname(FZMQActor.ID, True)
      , FormMatrixGame.ChatMemoSend.Lines.Text
    ]);

  end;

  case FActor of
    gaAdmin: begin
      if not FExperiment.ResearcherCanChat then Exit;
      M[0] := GA_ADMIN+M[0];
      TZMQAdmin(FZMQActor).SendMessage(M);
    end;
    gaPlayer:begin
      M[0] := GA_PLAYER+M[0];
      TZMQPlayer(FZMQActor).SendMessage(M);
    end;
    //gaWatcher:begin // Cannot SendMessages
    //  M[0] := GA_WATCHER+M[0];
    //  TZMQWatcher(FZMQActor).SendMessage(M);
  end;

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
  var Data: TStringList;
  begin
    Data := TStringList.Create;
    try
    WriteLn('arrived');

    finally
      Data.Free;
    end;
  end;

  procedure ReceiveChoice;
  begin

  end;

  procedure ReceiveChat;
  begin
    FormMatrixGame.ChatMemoRecv.Lines.Append(('['+AMessage[1]+']: ')+AMessage[2]);
  end;

  procedure ReceiveLogin;
  begin
    WriteLn('login');
  end;

  procedure ReceiveLogout;
  begin
    WriteLn('logout');
  end;

begin
  if MHas(K_ARRIVED) then ReceiveActor;
  if MHas(K_CHAT_M) then ReceiveChat;
  if MHas(K_CHOICE) then ReceiveChoice;
end;

end.

