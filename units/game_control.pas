{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_control;

{$mode objfpc}{$H+}

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
    procedure CreatePlayerBox(P:TPlayer; Me:Boolean;Admin:Boolean = False);
    procedure DeletePlayerBox(AID : string);
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
    procedure ShowConsequenceMessage(AID,S:string;ForGroup:Boolean);
    procedure DisableConfirmationButton;
    procedure CleanMatrix(AEnabled : Boolean);
    procedure NextConditionSetup(S : string);
    procedure EnablePlayerMatrix(AID:UTF8string; ATurn:integer; AEnabled:Boolean);
  private
    function IsLastCondition : Boolean;
    function ShouldStartExperiment: Boolean;
    function ShouldEndCycle : Boolean;
    function ShouldEndCondition : Boolean;
    //function ShouldEndGeneration : Boolean;
    function ShouldAskQuestion : Boolean;
    procedure NextTurn(Sender: TObject);
    procedure NextCycle(Sender: TObject);
    procedure NextLineage(Sender: TObject);
    procedure NextCondition(Sender: TObject);
    procedure Interlocking(Sender: TObject);
    procedure TargetInterlocking(Sender: TObject);
    procedure Consequence(Sender: TObject);
    procedure EndExperiment(Sender: TObject);
    procedure StartExperiment;
  public
    constructor Create(AOwner : TComponent;AppPath:string);overload;
    destructor Destroy; override;
    procedure SetMatrix;
    procedure SendRequest(ARequest : UTF8string);
    procedure SendMessage(AMessage : UTF8string);
    procedure Cancel;
    procedure Start;
    procedure Pause;
    procedure Resume;
    procedure Stop;
    property Experiment : TExperiment read FExperiment write FExperiment;
    property ID : UTF8string read FID;
    property RowBase : integer read FRowBase write SetRowBase;
    property MustDrawDots: Boolean read FMustDrawDots write SetMustDrawDots;
    property MustDrawDotsClear:Boolean read FMustDrawDotsClear write SetMustDrawDotsClear;
  end;

  function GetRowColor(ARow : integer;ARowBase:integer) : TColor;

// TODO: PUT NORMAL STRING MESSAGES IN RESOURCESTRING INSTEAD

const
  K_ARRIVED  = '.Arrived';
  K_CHAT_M   = '.ChatM';
  K_CHOICE   = '.Choice';
  K_MESSAGE  = '.Message';
  K_START    = '.Start';
  K_RESUME   = '.Resume';
  K_LOGIN    = '.Login';
  K_QUESTION = '.Question';
  K_QMESSAGE = '.QMessage';
  K_MOVQUEUE = '.Queue';
  K_END      = '.EndX';
  K_NXTCND   = '.NextCond';

  //
  K_STATUS   = '.Status';
  K_LEFT     = '.Left';
  K_WAIT     = '.Wait';
  K_FULLROOM = '.Full';
  K_PLAYING  = '.Playing';
  K_REFUSED  = '.Refused';

implementation

uses ButtonPanel,Controls,ExtCtrls,StdCtrls,LazUTF8, Forms, strutils
     , zhelpers
     , form_matrixgame
     , presentation_classes
     , form_chooseactor
     , game_resources
     , game_actors_helpers
     , string_methods
     ;

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

function TGameControl.ShouldEndCycle: Boolean; //CAUTION: MUST BE CALLED BEFORE EXPERIMENT.NEXTCYCLE
begin
  Result := FExperiment.Condition[FExperiment.CurrentCondition].Turn.Count = FExperiment.Condition[FExperiment.CurrentCondition].Turn.Value-1;
end;

function TGameControl.IsLastCondition: Boolean;
begin
  Result := FExperiment.CurrentCondition = FExperiment.ConditionsCount-1;
end;

function TGameControl.ShouldEndCondition: Boolean;
begin
  Result := FExperiment.ShouldEndCondition;
end;

//function TGameControl.ShouldEndGeneration: Boolean;
//begin
//  Result := FExperiment.Condition[FExperiment.CurrentCondition].Cycles.Count = FExperiment.Condition[FExperiment.CurrentCondition].Cycles.Value-1;
//end;

function TGameControl.ShouldAskQuestion: Boolean;
begin
  Result := Assigned(FExperiment.Condition[FExperiment.CurrentCondition].Prompt) and FExperiment.Condition[FExperiment.CurrentCondition].Contingencies[3].Fired;
end;

procedure TGameControl.EndExperiment(Sender: TObject);
begin
  ShowPopUp('O Experimento terminou.');
end;

procedure TGameControl.NextTurn(Sender: TObject);
begin
  // update admin view
  FormMatrixGame.LabelExpCountTurn.Caption:=IntToStr(FExperiment.Condition[FExperiment.CurrentCondition].Turn.Count+1);

end;

procedure TGameControl.NextCycle(Sender: TObject);
begin
  FormMatrixGame.LabelExpCountCycle.Caption:= IntToStr(FExperiment.Cycles+1);
  {$IFDEF DEBUG}
  WriteLn('cycle:>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
  {$ENDIF}
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
  NextConditionSetup(FExperiment.CurrentConditionAsString);
end;

procedure TGameControl.Interlocking(Sender: TObject);
var i : integer;
begin
  i := StrToInt(FormMatrixGame.LabelExpCountInterlocks.Caption);
  FormMatrixGame.LabelExpCountInterlocks.Caption:= IntToStr(i+1);
end;

procedure TGameControl.TargetInterlocking(Sender: TObject);
var i : integer;
begin
  i := StrToInt(FormMatrixGame.LabelExpCountTInterlocks.Caption);
  FormMatrixGame.LabelExpCounTtInterlocks.Caption:= IntToStr(i+1);
end;

procedure TGameControl.Consequence(Sender: TObject);
begin
{$IFDEF DEBUG}
  if Sender is TConsequence then
    FormMatrixGame.ChatMemoRecv.Lines.Append(('['+TConsequence(Sender).PlayerNicname+']: ')+TConsequence(Sender).AsString(''));
{$ENDIF}
end;

procedure TGameControl.StartExperiment;
begin
  // all players arrived, lets begin
  FExperiment.Play;

  // wait some time, we just sent a message earlier
  Sleep(5);

  // enable matrix grid for the first player
  FZMQActor.SendMessage([K_START]);

  //
  Start;
end;

procedure TGameControl.Start;
begin
  // basic gui setup

  // points
  //FormMatrixGame.GBIndividualAB.Visible := FExperiment.ABPoints;
  //FormMatrixGame.GBIndividual.Visible:= not FormMatrixGame.GBIndividualAB.Visible;

  // turns
  FormMatrixGame.LabelExpCountTurn.Caption:=IntToStr(FExperiment.Condition[FExperiment.CurrentCondition].Turn.Count+1);

  // cycle
  FormMatrixGame.LabelExpCountCycle.Caption := IntToStr(FExperiment.Cycles+1);

  // generation
  FormMatrixGame.LabelExpCountGeneration.Caption:=IntToStr(FExperiment.Condition[FExperiment.CurrentCondition].Cycles.Generation+1);

  // condition
  FormMatrixGame.LabelExpCountCondition.Caption:= FExperiment.Condition[FExperiment.CurrentCondition].ConditionName;

  // interlocks
  FormMatrixGame.LabelExpCountInterlocks.Caption:= '0';

  // target interlocks
  FormMatrixGame.LabelExpCountTInterlocks.Caption:= '0';

  // wait for players
end;

procedure TGameControl.Pause;
begin
  // save to file

  // inform players
end;

procedure TGameControl.Resume;
begin
  // load from file

  // wait for players
end;

procedure TGameControl.Stop;
begin
  // cleaning
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

procedure TGameControl.CreatePlayerBox(P: TPlayer; Me: Boolean; Admin: Boolean);
var i1 : integer;
begin
  with TPlayerBox.Create(FormMatrixGame.GBLastChoice,P.ID,Admin) do
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

procedure TGameControl.DeletePlayerBox(AID: string);
var i : integer;
begin
  for i := 0 to FormMatrixGame.GBLastChoice.ComponentCount -1 do
    if FormMatrixGame.GBLastChoice.Components[i] is TPlayerBox then
      if TPlayerBox(FormMatrixGame.GBLastChoice.Components[i]).ID = AID then
        begin
          TPlayerBox(FormMatrixGame.GBLastChoice.Components[i]).Free;
          Break;
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

procedure TGameControl.ShowConsequenceMessage(AID, S: string; ForGroup: Boolean);
var
  LConsequence : TConsequence;
begin
  LConsequence := TConsequence.Create(nil,S);
  LConsequence.GenerateMessage(ForGroup);
  LConsequence.PresentMessage(FormMatrixGame.GBIndividualAB);
  case FActor of
    gaPlayer:
      if ForGroup then
        LConsequence.PresentPoints(FormMatrixGame.LabelIndACount,FormMatrixGame.LabelIndBCount,
          FormMatrixGame.LabelIndCount,FormMatrixGame.LabelGroupCount)
      else
        if Self.ID = AID then
          LConsequence.PresentPoints(FormMatrixGame.LabelIndACount,FormMatrixGame.LabelIndBCount,
            FormMatrixGame.LabelIndCount,FormMatrixGame.LabelGroupCount);

    gaAdmin:
      begin
        {$IFDEF DEBUG}
        WriteLn(S);
        {$ENDIF}
        LConsequence.PresentPoints(GetPlayerBox(AID), FormMatrixGame.LabelGroupCount);
      end;
  end;
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

procedure TGameControl.NextConditionSetup(S: string);
var
  A, B, G : integer;
  P : TPlayer;
  PB : TPlayerBox;
begin
  if FExperiment.ABPoints then
    begin
      A := StrToInt(ExtractDelimited(1,S,['|']));
      B := StrToInt(ExtractDelimited(2,S,['|']));
      G := StrToInt(ExtractDelimited(3,S,['|']));

      G += StrToInt(FormMatrixGame.LabelGroupCount.Caption);
      FormMatrixGame.LabelGroupCount.Caption := IntToStr(G);
      case FActor of
        gaPlayer:
          begin
            A += StrToInt(FormMatrixGame.LabelIndACount.Caption);
            B += StrToInt(FormMatrixGame.LabelIndBCount.Caption);

            FormMatrixGame.LabelIndACount.Caption := IntToStr(A);
            FormMatrixGame.LabelIndBCount.Caption := IntToStr(B);
          end;
       gaAdmin:
         for P in FExperiment.Players do
          begin
            PB := GetPlayerBox(P.ID);
            A += StrToInt(PB.LabelPointsCount.Caption) + B;
            PB.LabelPointsCount.Caption := IntToStr(A);
          end;
      end;
    end
  else
    begin
      A := StrToInt(ExtractDelimited(1,S,['|']));
      G := StrToInt(ExtractDelimited(2,S,['|']));
      G += StrToInt(FormMatrixGame.LabelGroupCount.Caption);
      FormMatrixGame.LabelGroupCount.Caption := IntToStr(G);
      case FActor of
        gaPlayer:
          begin
            A += StrToInt(FormMatrixGame.LabelIndACount.Caption);
            FormMatrixGame.LabelIndCount.Caption := IntToStr(A);
          end;

       gaAdmin:
         for P in FExperiment.Players do
          begin
            PB := GetPlayerBox(P.ID);
            A += StrToInt(PB.LabelPointsCount.Caption) + B;
            PB.LabelPointsCount.Caption := IntToStr(A);
          end;
       end;
    end;
end;

procedure TGameControl.EnablePlayerMatrix(AID:UTF8string; ATurn:integer; AEnabled:Boolean);
begin
  if FExperiment.PlayerFromID[AID].Turn = ATurn then
    CleanMatrix(AEnabled);
end;

constructor TGameControl.Create(AOwner: TComponent;AppPath:string);
begin
  inherited Create(AOwner);
  FZMQActor := TZMQActor(AOwner);
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
  case FActor of
    gaAdmin:FExperiment := TExperiment.Create(FZMQActor.Owner,AppPath);
    gaPlayer:FExperiment := TExperiment.Create(FZMQActor.Owner);
    gaWatcher:FExperiment := TExperiment.Create(FZMQActor.Owner);
  end;
  FExperiment.State:=xsWaiting;
  FExperiment.OnEndTurn := @NextTurn;
  FExperiment.OnEndCycle := @NextCycle;
  FExperiment.OnEndCondition:= @NextCondition;
  FExperiment.OnEndGeneration:=@NextLineage;
  FExperiment.OnEndExperiment:= @EndExperiment;
  FExperiment.OnInterlocking:=@Interlocking;
  FExperiment.OnConsequence:=@Consequence;
  FExperiment.OnTargetInterlocking:=@TargetInterlocking;

  SendRequest(K_LOGIN); // admin cannot send requests
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
          CreatePlayerBox(P, Self.ID = P.ID)
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
        , AMessage[2] // generation
        , AMessage[3] // conditions
      ]);
    end;

  end;

  procedure ReceiveChoice;
  var
    P : TPlayer;
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

       // last turn// end cycle
       if P.Turn = FExperiment.PlayersCount-1  then
        begin
          // update next turn
          if Self.ID = P.ID then
           begin
             P.Turn := StrToInt(AMessage[4]);
             FExperiment.Player[Self.ID] := P;
           end;

          CleanMatrix(False);


          // no wait turns
          // if should continue then
          //if StrToBool(AMessage[6]) then
          //EnablePlayerMatrix(Self.ID,0, True)


          // wait for server
          Exit;
        end;

        // else
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

  procedure ReceiveChat;
  var
    ALn: string;
  begin
    ALn := '['+AMessage[1]+']: '+AMessage[2];
    FormMatrixGame.ChatMemoRecv.Lines.Append(ALn);
    if FActor = gaAdmin then
      FExperiment.WriteChatLn(ALn);
  end;

  procedure MovePlayerQueue;
  var
    P : TPlayer;
  begin
    P := FExperiment.PlayerFromString[AMessage[1]]; // new
    CreatePlayerBox(P,Self.ID = P.ID, FActor=gaAdmin);
    if FActor=gaPlayer then
      begin
        FExperiment.Player[FExperiment.PlayerIndexFromID[AMessage[2]]] := P;
        EnablePlayerMatrix(Self.ID,0, True);
      end;
  end;

  procedure SayGoodBye(AID:string);
  var Pts : string;
  begin
    DeletePlayerBox(AID); // old player
    case FActor of
      gaPlayer:begin
        if Self.ID = AID then
          begin
            if FExperiment.ABPoints then
              begin
                Pts := IntToStr(StrToInt(FormMatrixGame.LabelIndACount.Caption)+StrToInt(FormMatrixGame.LabelIndBCount.Caption));
                FormMatrixGame.LabelIndACount.Caption := '0';
                FormMatrixGame.LabelIndBCount.Caption := '0';
              end
            else
              begin
                Pts := FormMatrixGame.LabelIndCount.Caption;
                FormMatrixGame.LabelIndCount.Caption := '0';
              end;

            FormMatrixGame.Visible := False;
            FormChooseActor := TFormChooseActor.Create(nil);
            FormChooseActor.Style := K_LEFT;
            FormChooseActor.ShowPoints(
            'A tarefa terminou, obrigado por sua participação! Você produziu ' +
            Pts + ' pontos e ' +
            FormMatrixGame.LabelGroupCount.Caption + ' itens escolares serão doados!');

            if FormChooseActor.ShowModal = 1 then
              begin
                FZMQActor.Request([AID,' ',K_RESUME]);
                FormMatrixGame.Visible := True;
              end
            else;
            FormChooseActor.Free;
          end
        else
          ShowPopUp(FExperiment.PlayerFromID[AID].Nicname+ ' saiu. Por favor, aguarde a chegada de alguém para ocupar o lugar.');
      end;
    end;
  end;

  procedure EndExperimentMessage;
  var Pts : string;
  begin
    case FActor of
      gaPlayer:
        begin
          CleanMatrix(False);
          FormChooseActor := TFormChooseActor.Create(FormMatrixGame);
          FormChooseActor.Style := K_END;

          if FExperiment.ABPoints then
            Pts := IntToStr(StrToInt(FormMatrixGame.LabelIndACount.Caption)+StrToInt(FormMatrixGame.LabelIndBCount.Caption))
          else
            Pts := FormMatrixGame.LabelIndCount.Caption;

          FormChooseActor.ShowPoints(
          'A tarefa terminou, obrigado por sua participação! Você produziu ' +
          Pts + ' pontos e ' +
          FormMatrixGame.LabelGroupCount.Caption + 'itens escolares serão doados! Parabéns!');
          FormChooseActor.ShowModal;
          FormChooseActor.Free;
          FormMatrixGame.Close;
        end;
      gaAdmin:Stop;
    end;
  end;

  procedure ResumeNextTurn;
  begin
    if AMessage[2] <> #27 then
      begin
        case FActor of
          gaPlayer:
            begin
              if AMessage[1] <> #32 then
                SayGoodBye(AMessage[1])
              else
                EnablePlayerMatrix(Self.ID,0, True);
            end;

          gaAdmin:
            begin
              if AMessage[1] <> #32 then
                begin
                  DeletePlayerBox(AMessage[1]); // old player
                  ShowPopUp(
                          'O participante '+
                          FExperiment.PlayerFromID[AMessage[1]].Nicname+
                          ' saiu. Aguardando a entrada do próximo participante.'
                        );
                end;
            end;

        end;
        if AMessage[2] <> #32 then
          NextConditionSetup(AMessage[2]);
      end
    else EndExperimentMessage;
  end;

  procedure QuestionMessages;
  var
    i : integer;
    MID : string;
  begin
    if AMessage[2] <> #27 then
      begin
        if AMessage.Count > 1 then
          begin
            for i := 3 to AMessage.Count -1 do
              begin
                MID := ExtractDelimited(1,AMessage[i],['+']);
                ShowConsequenceMessage(MID, ExtractDelimited(2,AMessage[i],['+']),MID = 'M');
                // TODO: QMESSAGE present only one message with all information...
                {$IFDEF DEBUG}
                WriteLn('A Prompt consequence should have shown.');
                {$ENDIF}
              end;
          end;
        ResumeNextTurn;
        if AMessage[2] <> #32 then
          NextConditionSetup(AMessage[2]);
      end
    else EndExperimentMessage;
  end;

begin
  if MHas(K_ARRIVED) then ReceiveActor;
  if MHas(K_CHAT_M)  then ReceiveChat;
  if MHas(K_CHOICE)  then ReceiveChoice;
  if MHas(K_MESSAGE) then ShowConsequenceMessage(AMessage[1],AMessage[2],StrToBool(AMessage[3]));
  if MHas(K_START) then NotifyPlayers;
  if MHas(K_QUESTION) then ShowQuestion;
  if MHas(K_MOVQUEUE) then MovePlayerQueue;
  if MHas(K_QMESSAGE) then QuestionMessages;
  if MHas(K_RESUME) then ResumeNextTurn;
  if MHas(K_NXTCND) then NextConditionSetup(AMessage[1]);
  if MHAs(K_END) then EndExperimentMessage;
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
            CreatePlayerBox(P,False,True);

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
                    ARequest.Append(TS);  // FROM 3 to COUNT-4
                  end;

            // append chat data if allowed
            if FExperiment.SendChatHistoryForNewPlayers then
              ARequest.Append(FormMatrixGame.ChatMemoRecv.Lines.Text) // COUNT-3
            else
              ARequest.Append('[CHAT]'); // must append something to keep the message envelop standard

            // append global configs.
            ARequest.Append(BoolToStr(FExperiment.ABPoints)); // COUNT-2

            // append condition global data
            ARequest.Append(FExperiment.CurrentConditionAsString);

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
  var
    LConsequences : string;
    P : TPlayer;
    S : string;
    LEndCondition,
    LEndCycle : Boolean;
    LEndGeneration: string;
  begin
    {$IFDEF DEBUG}
    WriteLn('Count:',FExperiment.Condition[FExperiment.CurrentCondition].Turn.Count, '<', FExperiment.Condition[FExperiment.CurrentCondition].Turn.Value);
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

    // "NextGeneration" and "ShouldEndCycle" methods must be called before Experiment.NextTurn
    LEndCycle := ShouldEndCycle;
    LEndGeneration := FExperiment.NextGeneration;
    if LEndCycle then
      LConsequences := FExperiment.ConsequenceStringFromChoices;

    // update turn
    P.Turn := FExperiment.NextTurn;
    FExperiment.Player[FExperiment.PlayerIndexFromID[P.ID]] := P;

    // append results
    ARequest.Append(IntToStr(P.Turn)); //5
    ARequest.Append(S);                //6
    if LEndCycle then // >7 = EndCycle
      begin
        ARequest.Append(LConsequences); //7

        if ShouldAskQuestion then  // DONE: prompt only when an odd row was selected
           ARequest.Append(FExperiment.Condition[FExperiment.CurrentCondition].Prompt.Question) //8
        else
          begin
            ARequest.Append(#32); // 8
            if Assigned(FExperiment.Condition[FExperiment.CurrentCondition].Prompt) then
              FExperiment.WriteReportRowPrompt;
            FExperiment.Clean;
          end;

        ARequest.Append(LEndGeneration); // 9, #32 resume, else NextGeneration = PlayerToKick AID
        LEndCondition := ShouldEndCondition;
        if IsLastCondition and LEndCondition then // 10
          // end experiment envelop
          ARequest.Append(#27)
        else
          if LEndCondition then
            begin
              FExperiment.NextCondition;
              // end condition envelop
              ARequest.Append(FExperiment.CurrentConditionAsString);
            end
          else
            // do nothing envelop
            ARequest.Append(#32);
      end;
  end;

  procedure ValidateQuestionResponse;
  var
    P : TPlayer;
    M : array of UTF8string;
    i : integer;
    LPromptConsequences : TStringList;
  begin
    P := FExperiment.PlayerFromID[ARequest[0]];
    ARequest[2] := K_QUESTION+K_ARRIVED;

    // append response of each player
    FExperiment.Condition[FExperiment.CurrentCondition].Prompt.AppendResponse(P.ID,ARequest[3]);

    // return to experiment and present the prompt consequence, if any
    if FExperiment.Condition[FExperiment.CurrentCondition].Prompt.ResponsesCount = FExperiment.PlayersCount then
      begin

        // generate messages
        LPromptConsequences := FExperiment.Condition[FExperiment.CurrentCondition].Prompt.AsString;
        SetLength(M, 3+LPromptConsequences.Count);
        M[0] := K_QMESSAGE;
        M[1] := ARequest[4]; // generation envelop
        M[2] := ARequest[5]; // conditions
        if LPromptConsequences.Count > 0 then
          begin
            for i := 0 to LPromptConsequences.Count-1 do
              if Pos('$NICNAME',LPromptConsequences[i]) > 0 then
                begin
                  P := FExperiment.PlayerFromID[ExtractDelimited(1,LPromptConsequences[i],['+'])];
                  LPromptConsequences[i] := ReplaceStr(LPromptConsequences[i],'$NICNAME', P.Nicname);
                end;
            for i := 0 to LPromptConsequences.Count -1 do
              M[i+3] := LPromptConsequences[i]; // messages envelop
          end
        else;

         // send identified messages; each player takes only its own message and ignore the rest
        FZMQActor.SendMessage(M);
        FExperiment.WriteReportRowPrompt;
        FExperiment.Clean;
      end;
  end;

  procedure ReplyResume;// old player becomes a new player
  var
    P : TPlayer;
    S : string;
  begin
    P := FExperiment.PlayerFromID[ARequest[0]];
    ARequest[2] := K_RESUME+K_ARRIVED;
    if AskQuestion(
      'Um novo participante entrou no lugar do participante mais antigo. Criar um novo apelido para o novo participante?'
      ) = 'S' then
      P.Nicname := GenResourceName(-1);

    S := FExperiment.PlayerAsString[P];
    ARequest.Append(S); // 3
    FExperiment.NextGeneration := S;
  end;

begin
  if MHas(K_LOGIN) then ReplyLoginRequest;
  if MHas(K_RESUME) then ReplyResume;
  if MHas(K_CHOICE) then ValidateChoice;
  if MHas(K_QUESTION) then ValidateQuestionResponse;
end;

// Here FActor is garanted to be a TZMQPlayer, replying by:
// - sending private data to player
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
        for i:= 3 to AReply.Count -4 do
          begin
            P := FExperiment.PlayerFromString[AReply[i]];
            FExperiment.AppendPlayer(P);
            CreatePlayerBox(P, False);
          end;

        // add chat
        FormMatrixGame.ChatMemoRecv.Lines.Clear;
        FormMatrixGame.ChatMemoRecv.Lines.Add(AReply[AReply.Count-3]);

        // set global configs
        FExperiment.ABPoints := StrToBool(AReply[AReply.Count-2]);
        FormMatrixGame.GBIndividualAB.Visible := FExperiment.ABPoints;
        FormMatrixGame.GBIndividual.Visible:= not FormMatrixGame.GBIndividualAB.Visible;

        // set condition specific configurations
        NextConditionSetup(AReply[AReply.Count-1])
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
    LAnnouncer : TIntervalarAnnouncer;
    //P : TPlayer;
  begin
    if Self.ID = AReply[0] then
      begin
        //P := FExperiment.PlayerFromID[Self.ID];
        {$IFDEF DEBUG}
        WriteLn('LCount:',LCount);
        {$ENDIF}

        // inform other players about self.id choice
        FZMQActor.SendMessage([K_CHOICE,AReply[0],AReply[3],AReply[4],AReply[5]]);

        // The Announcer sends a message, waits interval time until all messages have been sent and then destroys itself.
        LAnnouncer := TIntervalarAnnouncer.Create(nil);
        LAnnouncer.OnStart := @FZMQActor.SendMessage;
        LAnnouncer.Interval := 5000;
        LCount := WordCount(AReply[6],['+']);

        // individual consequences
        if LCount > 0 then
          for i := 1 to LCount do
            begin
              LConsequence := TConsequence.Create(nil,ExtractDelimited(i,AReply[6],['+']));
              LConsequence.GenerateMessage(False);
              if LConsequence.ShouldPublishMessage then
                //FZMQActor.SendMessage([K_MESSAGE,Self.ID,ExtractDelimited(i,AReply[6],['+']),BoolToStr(False)])
                LAnnouncer.Append([K_MESSAGE,Self.ID,ExtractDelimited(i,AReply[6],['+']),BoolToStr(False)])
              else
                begin
                  LConsequence.PresentMessage(FormMatrixGame.GBIndividualAB);
                  LConsequence.PresentPoints(FormMatrixGame.LabelIndACount,FormMatrixGame.LabelIndBCount,
                    FormMatrixGame.LabelIndCount,FormMatrixGame.LabelGroupCount);
                end;
              {$IFDEF DEBUG}
              WriteLn('A consequence should have shown.');
              {$ENDIF}
            end;

        // group consequence
        if AReply.Count > 7 then
          begin
            LCount := WordCount(AReply[7],['+']);
            if LCount > 0 then
              for i := 1 to LCount do
                begin
                  LConsequence := TConsequence.Create(nil,ExtractDelimited(i,AReply[7],['+']));
                  LConsequence.GenerateMessage(True);
                  //FZMQActor.SendMessage([K_MESSAGE,'',ExtractDelimited(i,AReply[7],['+']),BoolToStr(True)]);
                  LAnnouncer.Append([K_MESSAGE,'',ExtractDelimited(i,AReply[7],['+']),BoolToStr(True)]);
                  {$IFDEF DEBUG}
                  WriteLn('A metaconsequence should have shown.');
                  {$ENDIF}
                end;

            // should ask question or just resume (going to the next turn)?
            if AReply[8] <> #32 then
              //FZMQActor.SendMessage([K_QUESTION,AReply[8],AReply[9]])
              LAnnouncer.Append([K_QUESTION,AReply[8],AReply[9],AReply[10]])
            else
              //FZMQActor.SendMessage([K_RESUME,AReply[9]]);
              LAnnouncer.Append([K_RESUME,AReply[9],AReply[10]]);

            // should end experiment or go to the next condition?
            if (AReply[10] = #27) and (AReply[8] = #32) then
              LAnnouncer.Append([K_END])
            else
              if (AReply[10] <> #32) then
                LAnnouncer.Append([K_NXTCND,AReply[10]])

          end;

        LAnnouncer.Reversed;
        LAnnouncer.Enabled := True;
      end;
  end;

  procedure ResumePlayer;
  begin
    FZMQActor.SendMessage([K_MOVQUEUE, AReply[3],AReply[0]]); //new player,old player (self.id)
  end;

begin
  if MHas(K_RESUME+K_ARRIVED) then ResumePlayer;
  if MHas(K_LOGIN+K_ARRIVED) then LoginAccepted;
  if MHas(K_CHOICE+K_ARRIVED) then ChoiceValidated;
end;



end.

