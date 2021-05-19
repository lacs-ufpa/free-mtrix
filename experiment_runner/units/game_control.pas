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
  Classes, SysUtils, Graphics, StdCtrls, PopupNotifier, Grids, ExtCtrls
  , game_zmq_actors
  , game_experiment
  , game_actors
  , game_visual_elements
  ;

type

  TCleanEvent = procedure (Sender:TObject; B : Boolean) of object;

  { TGameControl }

  TGameControl = class(TComponent)
  private
    FCurrentCause : TGameConsequenceStyle;
    FActor : TGameActor;
    FZMQActor : TZMQActor;
    FExperiment : TExperiment;
    function GetPlayerBox(AID:string) : TPlayerBox;
    function GetActorNicname(AID:string) : string;
    function MessageHas(const A_CONST : string; AMessage : TStringList; I:ShortInt=0): Boolean;
    procedure CreatePlayerBox(P:TPlayer; Me:Boolean;Admin:Boolean = False);
    procedure UpdatePlayerBox(P:TPlayer; Me:Boolean;Admin:Boolean = False; AOldPlayerID: string = '');
    //procedure DeletePlayerBox(AID : string);
    //procedure MovePlayerBox(AID : string);
    procedure SetMatrixType(AStringGrid : TStringGrid; AMatrixType:TGameMatrixType);
    procedure ReceiveMessage(AMessage : TStringList);
    procedure ReceiveRequest(var ARequest : TStringList);
    procedure ReceiveReply(AReply : TStringList);
    procedure UpdatePlayerTurns(ANextTurnString : string);
    procedure MovePlayerQueue(ANewPlayerAsString,AOldPlayerAsString:string);
  private
    function AskQuestion(AQuestion:string):string;
    function ShowConsequence(AID,S:string;IsMeta:Boolean;ShowPopUp : Boolean = True) : string;

    procedure NextConditionSetup(S : string; IsConditionStart:Boolean=False);
    procedure NextGenerationSetup(AID : string);
    procedure EnablePlayerMatrix(AID:string; ATurn:integer; AEnabled:Boolean);
    procedure InvalidateLabels;
  private
  {$IFDEF TEST_MODE}
    FFallbackMessages: TListBox;
  {$ENDIF}
    FGroupBoxPlayers: TGroupBox;
    FLabelGroup1Count: TLabel;
    FLabelGroup2Count: TLabel;
    FLabelGroup1Name : TLabel;
    FLabelGroup2Name : TLabel;
    FLabelPointACount: TLabel;
    FLabelPointBCount: TLabel;
    FLabelPointI: TLabel;
    FOnCleanEvent: TCleanEvent;
    FOnEndChoice: TNotifyEvent;
    FOnEndExperiment: TNotifyEvent;
    FOnInterlocking: TNotifyEvent;
    FOnPlayerExit: TPlayerEvent;
    // FOnStartChoice: TNotifyEvent;
    FOnStartCondition: TNotifyEvent;
    FOnStartCycle: TNotifyEvent;
    FOnStartExperiment: TNotifyEvent;
    FOnStartGeneration: TNotifyEvent;
    FOnStartTurn: TNotifyEvent;
    FOnTargetInterlocking: TNotifyEvent;
    FPictureGroup1: TImage;
    FSystemPopUp: TPopupNotifier;
    function GetID: string;
    procedure Interlocking(Sender: TObject);
  {$IFDEF TEST_MODE}
    procedure SetFallbackMessages(AValue: TListBox);
  {$ENDIF}
    procedure SetGroupBoxPlayers(AValue: TGroupBox);
    procedure SetLabelGroup1(AValue: TLabel);
    procedure SetLabelGroup1Name(AValue: TLabel);
    procedure SetLabelGroup2(AValue: TLabel);
    procedure SetLabelGroup2Name(AValue: TLabel);
    procedure SetLabelPointA(AValue: TLabel);
    procedure SetLabelPointB(AValue: TLabel);
    procedure SetLabelPointI(AValue: TLabel);
    procedure SetOnCleanEvent(AValue: TCleanEvent);
    procedure SetOnEndChoice(AValue: TNotifyEvent);
    procedure SetOnEndExperiment(AValue: TNotifyEvent);
    procedure SetOnInterlocking(AValue: TNotifyEvent);
    procedure SetOnPlayerExit(AValue: TPlayerEvent);
    procedure SetOnStartCondition(AValue: TNotifyEvent);
    procedure SetOnStartCycle(AValue: TNotifyEvent);
    procedure SetOnStartExperiment(AValue: TNotifyEvent);
    procedure SetOnStartGeneration(AValue: TNotifyEvent);
    procedure SetOnStartTurn(AValue: TNotifyEvent);
    procedure SetOnTargetInterlocking(AValue: TNotifyEvent);
    procedure EndExperiment(Sender : TObject);
    procedure SetPictureGroup1(AValue: TImage);
    procedure SetSystemPopUp(AValue: TPopupNotifier);
    procedure StartCondition(Sender: TObject);
    procedure StartCycle(Sender: TObject);
    procedure StartExperiment(Sender: TObject);
    procedure StartGeneration(Sender: TObject);
    procedure StartTurn(Sender: TObject);
    procedure TargetInterlocking(Sender: TObject);
  public
    constructor Create(AOwner : TComponent;AppPath:string = '');overload;
    destructor Destroy; override;
    function LoadFromFile(AFilename : string):Boolean;
    procedure SetMatrix;
    procedure SetLabels;
    procedure SendRequest(ARequest : string; AInputData : array of string);
    procedure SendMessage(AMessage : string; AInputData : array of string);
    procedure Cancel;
    procedure Start;
    procedure Pause;
    procedure Resume;
    procedure Stop;
    property Experiment : TExperiment read FExperiment write FExperiment;
    property ID : string read GetID;
  public
    procedure ShowSystemPopUp(AText:string;AInterval : integer);
    property SystemPopUp : TPopupNotifier read FSystemPopUp write SetSystemPopUp;
    property LabelGroup1Count : TLabel read FLabelGroup1Count write SetLabelGroup1;
    property LabelGroup2Count : TLabel read FLabelGroup2Count write SetLabelGroup2;
    property LabelPointA : TLabel read FLabelPointACount write SetLabelPointA;
    property LabelPointB : TLabel read FLabelPointBCount write SetLabelPointB;
    property LabelPointI : TLabel read FLabelPointI write SetLabelPointI;
    property ImageGroup1 : TImage read FPictureGroup1 write SetPictureGroup1;
  {$IFDEF TEST_MODE}
    property FallbackMessages : TListBox read FFallbackMessages write SetFallbackMessages;
  {$ENDIF}
    property LabelGroup1Name : TLabel read FLabelGroup1Name write SetLabelGroup1Name;
    property LabelGroup2Name : TLabel read FLabelGroup2Name write SetLabelGroup2Name;
    property GroupBoxPlayers : TGroupBox read FGroupBoxPlayers write SetGroupBoxPlayers;
    property OnEndExperiment : TNotifyEvent read FOnEndExperiment write SetOnEndExperiment;
    property OnInterlocking : TNotifyEvent read FOnInterlocking write SetOnInterlocking;
    property OnStartCondition : TNotifyEvent read FOnStartCondition write SetOnStartCondition;
    property OnStartCycle : TNotifyEvent read FOnStartCycle write SetOnStartCycle;
    property OnStartExperiment : TNotifyEvent read FOnStartExperiment write SetOnStartExperiment;
    property OnStartGeneration : TNotifyEvent read FOnStartGeneration write SetOnStartGeneration;
    property OnStartTurn : TNotifyEvent read FOnStartTurn write SetOnStartTurn;
    property OnTargetInterlocking : TNotifyEvent read FOnTargetInterlocking write SetOnTargetInterlocking;
    property OnEndChoice : TNotifyEvent read FOnEndChoice write SetOnEndChoice;
    property OnCleanEvent : TCleanEvent read FOnCleanEvent write SetOnCleanEvent;
    property OnPlayerExit : TPlayerEvent read FOnPlayerExit write SetOnPlayerExit;
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

  //
  K_STATUS   = '.Status';
  K_LEFT     = '.Left';
  K_WAIT     = '.Wait';
  K_FULLROOM = '.Full';
  K_PLAYING  = '.Playing';
  K_REFUSED  = '.Refused';

implementation

uses ButtonPanel,Controls, LazUTF8, Forms, Dialogs, strutils
     , game_visual_matrix_a
  {$IFDEF TEST_MODE}
     { do nothing }
  {$ELSE}
     , popup_hack
  {$ENDIF}
     , form_matrixgame
     , form_chooseactor
     , presentation_classes
     , game_resources
     , game_actors_helpers
     , string_methods
     ;

const
  GA_ADMIN = 'Admin';
  GA_PLAYER = 'Player';
  //GA_WATCHER = 'Watcher';

{ TGameControl }

//function TGameControl.ShouldEndGeneration: Boolean;
//begin
//  Result := FExperiment.CurrentCondition.Cycles.Count = FExperiment.CurrentCondition.Cycles.Value-1;
//end;

procedure TGameControl.EndExperiment(Sender: TObject);
var
  LMessage: String;
begin
  LMessage := 'The experiment ended.';
  ShowSystemPopUp(LMessage, GLOBAL_SYSTEM_MESSAGE_INTERVAL);
  if Assigned(FOnEndExperiment) then FOnEndExperiment(Sender);
end;

procedure TGameControl.SetPictureGroup1(AValue: TImage);
begin
  if FPictureGroup1=AValue then Exit;
  FPictureGroup1:=AValue;
end;

procedure TGameControl.SetSystemPopUp(AValue: TPopupNotifier);
begin
  if FSystemPopUp=AValue then Exit;
  FSystemPopUp:=AValue;
end;

procedure TGameControl.StartTurn(Sender: TObject);
begin
  if Assigned(FOnStartTurn) then FOnStartTurn(Sender);
end;

procedure TGameControl.StartCycle(Sender: TObject);
begin
  if Assigned(FOnStartCycle) then FOnStartCycle(Sender);
end;

procedure TGameControl.StartGeneration(Sender: TObject);
begin
  if Assigned(FOnStartGeneration) then FOnStartGeneration(Sender);
end;

procedure TGameControl.StartCondition(Sender: TObject);
begin
  if Assigned(FOnStartCondition) then FOnStartCondition(Sender);
end;

procedure TGameControl.Interlocking(Sender: TObject);
begin
  //i := StrToInt(FormMatrixGame.LabelExpCountInterlocks.Caption);
  //FormMatrixGame.LabelExpCountInterlocks.Caption:= IntToStr(i+1);
  if Assigned(FOnInterlocking) then FOnInterlocking(Sender);
end;

function TGameControl.GetID: string;
begin
  Result := FZMQActor.ID;
end;

{$IFDEF TEST_MODE}
procedure TGameControl.SetFallbackMessages(AValue: TListBox);
begin
  if FFallbackMessages=AValue then Exit;
  FFallbackMessages:=AValue;
end;
{$ENDIF}

procedure TGameControl.SetGroupBoxPlayers(AValue: TGroupBox);
begin
  if FGroupBoxPlayers=AValue then Exit;
  FGroupBoxPlayers:=AValue;
end;

procedure TGameControl.SetLabelGroup1(AValue: TLabel);
begin
  if FLabelGroup1Count=AValue then Exit;
  FLabelGroup1Count:=AValue;
end;

procedure TGameControl.SetLabelGroup1Name(AValue: TLabel);
begin
  if LabelGroup1Name=AValue then Exit;
  FLabelGroup1Name:=AValue;
end;

procedure TGameControl.SetLabelGroup2(AValue: TLabel);
begin
  if FLabelGroup2Count=AValue then Exit;
  FLabelGroup2Count:=AValue;
end;

procedure TGameControl.SetLabelGroup2Name(AValue: TLabel);
begin
  if LabelGroup2Name=AValue then Exit;
  FLabelGroup2Name:=AValue;
end;

procedure TGameControl.SetLabelPointA(AValue: TLabel);
begin
  if FLabelPointACount=AValue then Exit;
  FLabelPointACount:=AValue;
end;

procedure TGameControl.SetLabelPointB(AValue: TLabel);
begin
  if FLabelPointBCount=AValue then Exit;
  FLabelPointBCount:=AValue;
end;

procedure TGameControl.SetLabelPointI(AValue: TLabel);
begin
  if FLabelPointI=AValue then Exit;
  FLabelPointI:=AValue;
end;

procedure TGameControl.SetOnCleanEvent(AValue: TCleanEvent);
begin
  if FOnCleanEvent=AValue then Exit;
  FOnCleanEvent:=AValue;
end;

procedure TGameControl.SetOnEndChoice(AValue: TNotifyEvent);
begin
  if FOnEndChoice=AValue then Exit;
  FOnEndChoice:=AValue;
end;

procedure TGameControl.TargetInterlocking(Sender: TObject);
begin
  if Assigned(FOnTargetInterlocking) then FOnTargetInterlocking(Sender);
end;

//procedure TGameControl.Consequence(Sender: TObject);
//begin
//  if Assigned(FOnConsequence) then FOnConsequence(Sender);
//end;

procedure TGameControl.StartExperiment(Sender: TObject);
begin
  // gui setup
  // enable matrix grid for the first player
  FZMQActor.SendMessage([K_START]);

  if Assigned(FOnStartExperiment) then FOnStartExperiment(Sender);
end;
procedure TGameControl.SetOnEndExperiment(AValue: TNotifyEvent);
begin
  if FOnEndExperiment=AValue then Exit;
  FOnEndExperiment:=AValue;
end;

procedure TGameControl.SetOnInterlocking(AValue: TNotifyEvent);
begin
  if FOnInterlocking=AValue then Exit;
  FOnInterlocking:=AValue;
end;

procedure TGameControl.SetOnPlayerExit(AValue: TPlayerEvent);
begin
  if FOnPlayerExit=AValue then Exit;
  FOnPlayerExit:=AValue;
end;

procedure TGameControl.SetOnStartCondition(AValue: TNotifyEvent);
begin
  if FOnStartCondition=AValue then Exit;
  FOnStartCondition:=AValue;
end;

procedure TGameControl.SetOnStartCycle(AValue: TNotifyEvent);
begin
  if FOnStartCycle=AValue then Exit;
  FOnStartCycle:=AValue;
end;

procedure TGameControl.SetOnStartExperiment(AValue: TNotifyEvent);
begin
  if FOnStartExperiment=AValue then Exit;
  FOnStartExperiment:=AValue;
end;

procedure TGameControl.SetOnStartGeneration(AValue: TNotifyEvent);
begin
  if FOnStartGeneration=AValue then Exit;
  FOnStartGeneration:=AValue;
end;

procedure TGameControl.SetOnStartTurn(AValue: TNotifyEvent);
begin
  if FOnStartTurn=AValue then Exit;
  FOnStartTurn:=AValue;
end;

procedure TGameControl.SetOnTargetInterlocking(AValue: TNotifyEvent);
begin
  if FOnTargetInterlocking=AValue then Exit;
  FOnTargetInterlocking:=AValue;
end;


procedure TGameControl.Start;
begin
  // check if experiment is running
end;

procedure TGameControl.Pause;
begin
  //FExperiment.State:=xsPaused;
  // save to file

  // inform players
end;

procedure TGameControl.Resume;
begin
  //FExperiment.State:=xsRunning;
  // load from file

  // wait for players
end;

procedure TGameControl.Stop;
begin
  // cleaning
  FExperiment.Clean;
end;

function TGameControl.GetPlayerBox(AID: string): TPlayerBox;
var i : integer;
begin
  Result := nil;
  for i := 0 to GroupBoxPlayers.ComponentCount-1 do
    if TPlayerBox(GroupBoxPlayers.Components[i]).ID = AID then
      begin
        Result := TPlayerBox(GroupBoxPlayers.Components[i]);
        Break;
      end;
end;

function TGameControl.GetActorNicname(AID: string): string;
begin
  Result := '';
  case FActor of
    gaPlayer: begin
      if FExperiment.Player[AID].ID <> '' then
        Result := FExperiment.Player[AID].Nicname
      else
        Exception.Create('TGameControl.GetActorNicname Exception');
    end;

    gaAdmin: Result := FExperiment.Researcher;
    else { do nothing };
  end;
end;


function TGameControl.MessageHas(const A_CONST: string; AMessage: TStringList;
  I: ShortInt): Boolean;
begin
  Result:= False;
  if not Assigned(AMessage) then Exit;
  Result := Pos(A_CONST,AMessage[I])>0;
end;

procedure TGameControl.CreatePlayerBox(P: TPlayer; Me: Boolean; Admin: Boolean);
var i1 : integer;
begin
  with TPlayerBox.Create(GroupBoxPlayers,P.ID,Admin) do
    begin
      if Me then
        Caption := P.Nicname+SysToUtf8(' (You)' )
      else
        Caption := P.Nicname;
      i1 := PtrInt(P.Choice.Row);
      if i1 > 0 then
        LabelLastRowCount.Caption := Format('%-*.*d', [1,2,i1])
      else
        LabelLastRowCount.Caption := 'NA';
      PanelLastColor.Color := GetColorFromCode(P.Choice.Color);
      Enabled := True;
      Parent := GroupBoxPlayers;
    end;
end;

procedure TGameControl.UpdatePlayerBox(P: TPlayer; Me: Boolean; Admin: Boolean;
  AOldPlayerID: string);
var
  LPlayerBox : TPlayerBox;
begin
  if AOldPlayerID <> '' then begin
    LPlayerBox := GetPlayerBox(AOldPlayerID);
    LPlayerBox.ID := P.ID;
  end else begin
    LPlayerBox := GetPlayerBox(P.ID);
  end;

  with LPlayerBox do
    begin
      if Me then
        Caption := P.Nicname+SysToUtf8(' (You)' )
      else
        Caption := P.Nicname;
      if Admin then
        begin
          LabelPointsRedCount.Caption := '0';
          LabelPointsBlueCount.Caption := '0';
        end
      else
        begin;
          LabelLastRowCount.Caption := 'NA';
          PanelLastColor.Color := GetColorFromCode(P.Choice.Color);
        end;
    end;
end;

//procedure TGameControl.DeletePlayerBox(AID: string);
//var i : integer;
//begin
//  for i := 0 to GroupBoxPlayers.ComponentCount -1 do
//    if GroupBoxPlayers.Components[i] is TPlayerBox then
//      if TPlayerBox(GroupBoxPlayers.Components[i]).ID = AID then
//        begin
//          TPlayerBox(GroupBoxPlayers.Components[i]).Free;
//          Break;
//        end;
//end;

//procedure TGameControl.MovePlayerBox(AID: string);
//var i : integer;
//begin
//  for i := 0 to GroupBoxPlayers.ComponentCount -1 do
//    if GroupBoxPlayers.Components[i] is TPlayerBox then
//      if TPlayerBox(GroupBoxPlayers.Components[i]).ID = AID then
//        begin
//          TPlayerBox(GroupBoxPlayers.Components[i]).Parent := FormMatrixGame.GBOldPlayers;
//          TPlayerBox(GroupBoxPlayers.Components[i]).InvisibleLineRow;
//          Break;
//        end;
//end;

procedure TGameControl.SetMatrixType(AStringGrid: TStringGrid;
  AMatrixType: TGameMatrixType);
begin
  if gmRows in AMatrixType then
    TStringGridA(AStringGrid).HasRows:=True;

  if gmColumns in AMatrixType then
    TStringGridA(AStringGrid).HasCols:=True;

  TStringGridA(AStringGrid).DrawFilledDots := gmDots in AMatrixType;
  TStringGridA(AStringGrid).DrawClearDots := gmClearDots in AMatrixType;
  TStringGridA(AStringGrid).UpdateSizeAndNames;
end;

//procedure TGameControl.MovePlayerQueue(ANewPlayerString,AOldPlayerID:string);
//var
//  P : TPlayer;
//  procedure CommonSetup;
//  begin
//    UpdatePlayerBox(P,Self.ID = P.ID, FActor=gaAdmin, AOldPlayerID);
//    if FExperiment.ConditionMustBeUpdated <> '' then begin
//      NextConditionSetup(FExperiment.ConditionMustBeUpdated);
//      FExperiment.ConditionMustBeUpdated := '';
//    end;
//    NextGenerationSetup(P.ID);
//  end;
//
//begin
//  P := FExperiment.PlayerFromString[ANewPlayerString];
//  case FActor of
//    gaPlayer : begin
//      if Self.ID = AOldPlayerID then
//        FZMQActor.UpdateID(P.ID);
//
//      P.Turn := FExperiment.Player[AOldPlayerID].Turn;
//      FExperiment.NextGeneration := FExperiment.PlayerAsString[P];
//
//      CommonSetup;
//
//      EnablePlayerMatrix(Self.ID, 0, True);
//    end;
//
//    gaAdmin :
//      CommonSetup;
//    else { do nothing };
//  end;
//end;

procedure TGameControl.MovePlayerQueue(ANewPlayerAsString,
  AOldPlayerAsString: string);
var
  LNewPlayer : TPlayer;
  LOldPlayer : TPlayer;
  S : string;
  i : integer;
begin
  LNewPlayer := FExperiment.PlayerFromString[ANewPlayerAsString];
  LOldPlayer := FExperiment.PlayerFromString[AOldPlayerAsString];
  FExperiment.ArquiveOldPlayer(LOldPlayer);
  {$IFDEF TEST_MODE}
  S :=
    'BEFORE:' + Self.ID + LineEnding +
    'NextTurnPlayerID: ' +FExperiment.NextTurnPlayerID + LineEnding +
    'PlayerTurn: ' +FExperiment.PlayerTurn.ToString + LineEnding;

  for i := 0 to FExperiment.PlayersCount-1 do
    begin
      S +=
        'Player'+ i.ToString +'.ID: ' +FExperiment.Player[i].ID + LineEnding +
        'Player'+ i.ToString +'.Turn: ' +FExperiment.Player[i].Turn.ToString + LineEnding;
    end;
  {$ENDIF}
  LNewPlayer.Turn := LOldPlayer.Turn;
  FExperiment.Player[LOldPlayer.ID] := LNewPlayer;
  FExperiment.MovePlayersQueueLeft;

  if Self.ID = LOldPlayer.ID then
    FZMQActor.UpdateID(LNewPlayer.ID);

  {$IFDEF TEST_MODE}
  S += LineEnding+
    'AFTER:' + Self.ID + LineEnding+
    'NextTurnPlayerID: ' +FExperiment.NextTurnPlayerID + LineEnding +
    'PlayerTurn: ' +FExperiment.PlayerTurn.ToString + LineEnding;

  for i := 0 to FExperiment.PlayersCount-1 do
    begin
      S +=
        'Player'+ i.ToString +'.ID: ' +FExperiment.Player[i].ID + LineEnding +
        'Player'+ i.ToString +'.Turn: ' +FExperiment.Player[i].Turn.ToString + LineEnding;
    end;
  FallbackMessages.Items.Append(S);
  {$ENDIF}
  UpdatePlayerBox(LNewPlayer,
    Self.ID = LNewPlayer.ID, FActor=gaAdmin, LOldPlayer.ID);
  if FExperiment.ConditionMustBeUpdated <> '' then
    begin
      NextConditionSetup(FExperiment.ConditionMustBeUpdated);
      FExperiment.ConditionMustBeUpdated := '';
    end;
  NextGenerationSetup(LNewPlayer.ID);
  if FActor=gaPlayer then
    EnablePlayerMatrix(Self.ID, 0, True);
end;

// many thanks to howardpc for this:
// http://forum.lazarus.freepascal.org/index.php/topic,34559.msg227585.html#msg227585
function TGameControl.AskQuestion(AQuestion: string): string;
var
  Prompt: TForm;
  ButtonPanel: TButtonPanel;
  LabelQuestion: TLabel;
  mr: TModalResult;
begin
  Prompt:=TForm.CreateNew(nil);
  try
    with Prompt do
      begin
        BorderStyle:=bsNone;
        Position:=poScreenCenter;
        ButtonPanel:=TButtonPanel.Create(Prompt);
        with ButtonPanel do
          begin
            ButtonOrder:=boCloseOKCancel;
            OKButton.Caption:='Yes';
            CancelButton.Caption:='No';
            ShowButtons:=[pbOK, pbCancel];
            ShowBevel:=True;
            ShowGlyphs:=[];
            Parent:=Prompt;
          end;

        LabelQuestion:=TLabel.Create(Prompt);
        with LabelQuestion do
          begin
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
        else
          Result := 'N';
      end;
  finally
    Prompt.Free;
  end;
end;

procedure TGameControl.ShowSystemPopUp(AText: string; AInterval: integer);
{$IFDEF TEST_MODE}
  { do nothing }
{$ELSE}
var
  PopUpPos : TPoint;
  // temporary hack
  L : TLabel;
  r: TRect;
  w:integer;
{$ENDIF}
begin
{$IFDEF TEST_MODE}
  FallbackMessages.Items.Append(AText);
{$ELSE}
  SystemPopUp.vNotifierForm.AutoSize:=True;
  L := TLabel(SystemPopUp.vNotifierForm.FindComponent('UglyHack'));
  L.Caption := AText;
  SystemPopUp.Show;
  w := L.Width;
  SystemPopUp.Hide;
  SystemPopUp.vNotifierForm.AutoSize:=False;
  r:=SystemPopUp.vNotifierForm.CalcHintRect(w, AText, nil);
  SystemPopUp.vNotifierForm.HintRect:=r;
  SystemPopUp.vNotifierForm.Width:=r.Right-r.Left + 52;
  SystemPopUp.vNotifierForm.Height:=r.Bottom-r.Top + 52;

  if Assigned(FormMatrixGame) then
    begin
      PopUpPos.X := (FormMatrixGame.StringGridMatrix.Width div 2) - (SystemPopUp.vNotifierForm.Width div 2);
      PopUpPos.Y := (FormMatrixGame.StringGridMatrix.Height div 2) - (SystemPopUp.vNotifierForm.Height div 2);
      PopUpPos := FormMatrixGame.StringGridMatrix.ClientToScreen(PopUpPos);

      //SystemPopUp.Text:=AText;
      SystemPopUp.ShowAtPos(PopUpPos.X,PopUpPos.Y);
      FormMatrixGame.Timer.OnTimer:=@FormMatrixGame.TimerTimer;
      FormMatrixGame.Timer.Interval:=AInterval;
      FormMatrixGame.Timer.Enabled:=True;
    end
  else
    SystemPopUp.ShowAtPos(0,0);
{$ENDIF}
end;

function TGameControl.ShowConsequence(AID, S: string;
  IsMeta: Boolean; ShowPopUp: Boolean):string;
var
  LConsequence : TConsequence;
  LStyle : TGameConsequenceStyle;

  procedure PresentMetaPoints;
  begin
    for LStyle in LConsequence.Style do
      case LStyle of
          gscG1, gscG2 : //same visual control for both types
            FLabelGroup1Count.Caption :=
              FExperiment.GlobalPoints(LStyle).ToString;
          else { do nothing };
      end;
  end;

  procedure PresentPointsPlayer;
  begin
    for LStyle in LConsequence.Style do
      case LStyle of
          gscA :
            FLabelPointACount.Caption :=
              FExperiment.PlayerPointsFromID(Self.ID).A.ToString;

          gscB :
            FLabelPointBCount.Caption :=
              FExperiment.PlayerPointsFromID(Self.ID).B.ToString;

          else { do nothing };
      end;
  end;

  procedure PresentPointsAdmin;
  var
    PB : TPlayerBox;
  begin
    for LStyle in LConsequence.Style do
      case LStyle of
          gscA :
            begin
              PB := GetPlayerBox(AID);
              PB.LabelPointsRedCount.Caption :=
                FExperiment.PlayerPointsFromID(AID).A.ToString;
            end;
          gscB :
            begin
              PB := GetPlayerBox(AID);
              PB.LabelPointsBlueCount.Caption :=
                FExperiment.PlayerPointsFromID(AID).B.ToString;
            end;

          gscG1, gscG2 :
            PresentMetaPoints;
          else { do nothing };
      end;
  end;
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

  case FActor of
    gaPlayer:
      if IsMeta then
        PresentMetaPoints
      else
        if Self.ID = AID then
          PresentPointsPlayer;

    gaAdmin:
      PresentPointsAdmin;
    else { do nothing };
  end;

{$IFDEF TEST_MODE}
  FFallbackMessages.Items.Append(Result);
{$ELSE}
  if ShowPopUp then begin
    if Assigned(FormMatrixGame) then
      LConsequence.PresentMessage(FormMatrixGame.GBPoints)
    else
      LConsequence.PresentMessage(TWinControl(Owner.Owner))
  end;
{$ENDIF}
  LConsequence.Free;
end;

procedure TGameControl.NextConditionSetup(S: string; IsConditionStart: Boolean); // [player_points]
var
  A, B, G1, G2 : integer;
  P : TPlayer;
  C : TCondition;
  LCause : string;
begin
  if Assigned(LabelGroup1Name) then
    LabelGroup1Name.Caption := Sanitize(ExtractDelimited(1,S,['|']));

  if Assigned(LabelGroup2Name) then
    LabelGroup2Name.Caption := Sanitize(ExtractDelimited(2,S,['|']));
  LCause := ExtractDelimited(3,S,['|']);

  if Assigned(ImageGroup1) then
    ImageGroup1.Picture.LoadFromResourceName(HInstance, LCause);

  // workaround to present points
  // TODO: send TExperiment to player through zmq and load from configuration directly
  if LCause = 'SUSTAINABLE' then FCurrentCause := gscG1;
  if LCause = 'NON-SUSTAINABLE' then FCurrentCause := gscG2;

  if FExperiment.ABPoints then
    begin
      A := StrToIntDef(ExtractDelimited(4,S,['|']), 0);
      B := StrToIntDef(ExtractDelimited(5,S,['|']), 0);
      G1 := StrToIntDef(ExtractDelimited(6,S,['|']), 0);
      G2 := StrToIntDef(ExtractDelimited(7,S,['|']), 0);
    end
  else
    begin
      A := StrToIntDef(ExtractDelimited(4,S,['|']), 0);
      G1 := StrToIntDef(ExtractDelimited(5,S,['|']), 0);
      G2 := StrToIntDef(ExtractDelimited(6,S,['|']), 0);
    end;

  case FActor of
    gaPlayer:
      begin
        if IsConditionStart then
        begin
          // we need at least one valid fake condition
          // in memory for player internals (on start, on generation points).
          FExperiment.AppendCondition(C_CONDITION_TEMPLATE);
        end;
        C := FExperiment.Condition[FExperiment.ConditionsCount-1];
        with C.Points do
          begin
            OnStart.A := A;
            OnStart.B := B;
          end;
        FExperiment.Condition[FExperiment.ConditionsCount-1] := C;
      end;
    else { do nothing };
  end;

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

  InvalidateLabels;
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

procedure TGameControl.EnablePlayerMatrix(AID:string; ATurn:integer;
  AEnabled:Boolean);
const
  LMessage : string = 'It is your turn! Click at a row and confirm your choice.';
begin
  if FExperiment.PlayerFromID[AID].Turn = ATurn then begin
    if Assigned(OnCleanEvent) then
      OnCleanEvent(Self, AEnabled);

    if AEnabled then begin
      {$IFDEF TEST_MODE}
        FallbackMessages.Items.Append(LMessage);
      {$ELSE}
        ShowSystemPopUp(LMessage, GLOBAL_SYSTEM_MESSAGE_INTERVAL);
      {$ENDIF}
    end;
  end;
end;

procedure TGameControl.InvalidateLabels;
var
  P : TPlayer;
  PB : TPlayerBox;
begin
  if Assigned(FLabelGroup1Count) then
    FLabelGroup1Count.Caption :=
      FExperiment.CurrentCondition.Points.Count.G1.ToString;
  if Assigned(FLabelGroup2Count) then
    FLabelGroup2Count.Caption :=
      FExperiment.CurrentCondition.Points.Count.G2.ToString;

  case FActor of
    gaAdmin:begin
        for P in FExperiment.Players do
        begin
          PB := GetPlayerBox(P.ID);
          PB.LabelPointsRedCount.Caption :=
            FExperiment.PlayerFromID[Self.ID].Points.A.ToString;
          PB.LabelPointsBlueCount.Caption :=
            FExperiment.PlayerFromID[Self.ID].Points.B.ToString;
          if FExperiment.ABPoints then begin
            { do nothing }
          end else begin
            PB.LabelPointsBlueCount.Caption := 'NA';
          end;
        end;
    end;

    gaPlayer:begin
      P := FExperiment.PlayerFromID[Self.ID];
      FLabelPointACount.Caption := P.Points.A.ToString;
      FLabelPointBCount.Caption := P.Points.B.ToString;
    end;
    else { do nothing };
  end;
end;

constructor TGameControl.Create(AOwner: TComponent;AppPath:string);
begin
  inherited Create(AOwner);
  FZMQActor := TZMQActor(AOwner);
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

  case FActor of
    gaNone: Exception.Create('TGameControl.Create exception. FActor = gaNone');
    gaAdmin:FExperiment := TExperiment.Create(FZMQActor.Owner, FActor, AppPath);
    gaPlayer:FExperiment := TExperiment.Create(FZMQActor.Owner, FActor);
    gaWatcher:FExperiment := TExperiment.Create(FZMQActor.Owner, FActor);
  end;
  FExperiment.OnStartTurn:=@StartTurn;
  FExperiment.OnStartCondition:=@StartCondition;
  FExperiment.OnStartCycle:=@StartCycle;
  FExperiment.OnStartExperiment:=@StartExperiment;
  FExperiment.OnStartGeneration:=@StartGeneration;
  FExperiment.OnEndExperiment:= @EndExperiment;
  FExperiment.OnInterlocking:=@Interlocking;
  FExperiment.OnTargetInterlocking:=@TargetInterlocking;

  //FExperiment.OnEndTurn := @NextTurn;
  //FExperiment.OnEndCycle := @NextCycle;
  //FExperiment.OnEndCondition:= @NextCondition;
  //FExperiment.OnEndGeneration:=@NextLineage;
  //FExperiment.OnConsequence:=@Consequence;

  SendRequest(K_LOGIN,[]); // admin cannot send requests
end;

destructor TGameControl.Destroy;
begin
  inherited Destroy;
end;

function TGameControl.LoadFromFile(AFilename: string): Boolean;
begin
  Result := FExperiment.LoadFromFile(AFilename);
  if not Result then Exit;

  SetMatrix;
  SetLabels;
  if Assigned(FormMatrixGame) then
    if Experiment.ShowChat then
      FormMatrixGame.ChatPanel.Visible := Experiment.ResearcherCanChat
    else
      FormMatrixGame.ChatPanel.Visible := Experiment.ShowChat;
end;

procedure TGameControl.SetMatrix;
begin
  if Assigned(FormMatrixGame) then
    SetMatrixType(FormMatrixGame.StringGridMatrix, FExperiment.MatrixType);
end;

procedure TGameControl.SetLabels;
begin
  if Assigned(FormMatrixGame) then
  with FormMatrixGame do begin
    // a b points
    LabelIndA.Visible := FExperiment.ABPoints and (FActor = gaPlayer);
    LabelIndB.Visible := FExperiment.ABPoints and (FActor = gaPlayer);
    LabelIndACount.Visible := FExperiment.ABPoints and (FActor = gaPlayer);
    LabelIndBCount.Visible := FExperiment.ABPoints and (FActor = gaPlayer);
    ImageIndA.Visible := FExperiment.ABPoints and (FActor = gaPlayer);
    ImageIndB.Visible := FExperiment.ABPoints and (FActor = gaPlayer);

    // i points
    LabelInd.Visible := (not FExperiment.ABPoints) and (FActor = gaPlayer);
    LabelIndCount.Visible := (not FExperiment.ABPoints) and (FActor = gaPlayer);
    ImageInd.Visible := (not FExperiment.ABPoints) and (FActor = gaPlayer);
  end;
end;

// called from outside
procedure TGameControl.SendRequest(ARequest: string;
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
  SetLength(M,0);
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
        , AInputData[0]
        , AInputData[1]
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
      //if (FActor = gaAdmin) and (not FExperiment.ResearcherCanChat) then Exit;
      SetM([
        AMessage
        , GetActorNicname(FZMQActor.ID)
        , AInputData[0]
      ]);
    end;
    K_CHOICE  : SetM([
        AMessage
        , FZMQActor.ID
        , AInputData[0]
        , AInputData[1]
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
  FZMQActor.SendMessage([K_END]);
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
      gaPlayer:
        begin
          P := FExperiment.PlayerFromString[AMessage[1]];
          FExperiment.AppendPlayer(P);
          CreatePlayerBox(P, Self.ID = P.ID)
        end;
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
        , AskQuestion(AMessage[1])
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
    FPlayerBox : TPlayerBox;
  begin
    P := FExperiment.PlayerFromID[AMessage[1]];
    // add last responses to player box
    FPlayerBox := GetPlayerBox(P.ID);
    FPlayerBox.LabelLastRowCount.Caption := AMessage[2];
    FPlayerBox.PanelLastColor.Color := GetColorFromString(AMessage[3]);
    FPlayerBox.PanelLastColor.Caption:='';
    FPlayerBox.Invalidate;

    case FActor of
      gaPlayer:
        begin
          // last turn // end cycle
          if FExperiment.PlayerTurn = FExperiment.PlayersCount-1 then
            begin
              // update next turn if necessary
              if AMessage[4] <> #32 then
                UpdatePlayerTurns(AMessage[4]);

              if Assigned(OnCleanEvent) then
                OnCleanEvent(Self, False);

              // wait for server
              FExperiment.PlayerTurn := 0;
              Exit;
            end;
          Inc(FExperiment.PlayerTurn);

          if Self.ID = P.ID then
            begin
              if Assigned(OnEndChoice) then
                OnEndChoice(Self);
            end
          else
            EnablePlayerMatrix(Self.ID,FExperiment.PlayerTurn, True);
      end;
     else { do nothing };
    end;
  end;

  procedure NotifyPlayers;
  var
    LMessage: String;
  begin
    case FActor of
      gaPlayer:
          if FExperiment.PlayerFromID[Self.ID].Turn = 0 then
            EnablePlayerMatrix(Self.ID, 0, True)
          else begin
            LMessage := 'It is started! Wait for your turn.';
            ShowSystemPopUp(LMessage, GLOBAL_SYSTEM_MESSAGE_INTERVAL);
          end;
      gaAdmin:
        NextConditionSetup(FExperiment.CurrentConditionAsString,True);
      else { do nothing };
    end;
  end;

  procedure ReceiveChat;
  var
    ALn: string;
  begin
    ALn := '['+AMessage[1]+']: '+AMessage[2];
    if Assigned(FormMatrixGame) then
      FormMatrixGame.ChatMemoRecv.Lines.Append(ALn);
    if FActor = gaAdmin then
      FExperiment.WriteChatLn(ALn);
  end;

  procedure SayGoodBye(AID:string); // [player_points]
  var Pts , LMessage: string;
  begin
    case FActor of
      gaPlayer:
        begin
          //DeletePlayerBox(AID); // old player
          if Self.ID = AID then begin
            Pts := FExperiment.PlayerPointsSummationFromID(Self.ID).ToString;

            // TODO: EXPERIMENT2, remove G2 from LMessage
            LMessage :=
              'The task is over, thank you for your collaboration!'+LineEnding+
              'You produced ' + Pts + ' tokens for you, ' +
              FExperiment.GlobalPoints(gscG1).ToString +
              ' tokens for a cause and ' +
              FExperiment.GlobalPoints(gscG2).ToString +
              ' tokens for another cause.';
          {$IFDEF TEST_MODE}
            FallbackMessages.Items.Append(LMessage);
            FZMQActor.Request([AID,' ',K_RESUME]);
          {$ELSE}
            if Assigned(FormMatrixGame) then
              FormMatrixGame.Visible := False;
            FormChooseActor := TFormChooseActor.Create(nil);
            FormChooseActor.Style := K_LEFT;
            FormChooseActor.ShowPoints(LMessage);

            if FormChooseActor.ShowModal = 1 then begin
              if Assigned(FormMatrixGame) then
                FormMatrixGame.Visible := True;
              FZMQActor.Request([AID,' ',K_RESUME]);
            end;

            FormChooseActor.Free;
          {$ENDIF}
          end else begin
            LMessage := FExperiment.PlayerFromID[AID].Nicname +
              ' exited. Please, wait for someone else to arrive.';
          {$IFDEF TEST_MODE}
            FallbackMessages.Items.Append(LMessage);
          {$ELSE}
            ShowSystemPopUp(LMessage, GLOBAL_SYSTEM_MESSAGE_INTERVAL);
          {$ENDIF}
          end;
        end;
      else { do nothing };
    end;
  end;

  procedure EndExperimentMessage;
{$IFDEF TEST_MODE}
{$ELSE}
  var Pts : string;
{$ENDIF}
  begin
    case FActor of
      gaPlayer:
        begin
          if Assigned(OnCleanEvent) then
            OnCleanEvent(Self, False);

          //if Assigned(FormMatrixGame) then
          //  FormChooseActor := TFormChooseActor.Create(FormMatrixGame)
          //else
          {$IFNDEF TEST_MODE}
          FormChooseActor := TFormChooseActor.Create(nil);
          FormChooseActor.Style := K_END;

          if FExperiment.ABPoints then
            Pts := IntToStr(StrToInt(LabelPointA.Caption)+StrToInt(LabelPointB.Caption))
          else
            Pts := LabelPointI.Caption;

          FormChooseActor.ShowPoints(
          'The task is over, thank you for your collaboration!'+LineEnding+
          'You produced ' + Pts + ' tokens for you and ' +
          LabelGroup1Count.Caption + ' ' + LowerCase(LabelGroup1Name.Caption) + '.');
          FormChooseActor.ShowModal;
          FormChooseActor.Free;

          if Assigned(FormMatrixGame) then
            FormMatrixGame.Close;
          {$ENDIF}
        end;
      gaAdmin:Stop;
      else { do nothing };
    end;
  end;

  procedure ResumeNextTurn;
  var
    //LPlayerBox : TPlayerBox;
    P : TPlayer;
    LMessage: String;
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
                  //LPlayerBox := GetPlayerBox(AMessage[1]);
                  P := FExperiment.PlayerFromID[AMessage[1]];
                  if Assigned(OnPlayerExit) then
                    OnPlayerExit(P, AMessage[1]);
                  //DeletePlayerBox(AMessage[1]);

                  LMessage := 'Participant '+ P.Nicname +
                    ' exited. Waiting for the next participant to arrive.';
                {$IFDEF TEST_MODE}
                  FallbackMessages.Items.Append(LMessage);
                {$ELSE}
                  ShowSystemPopUp(LMessage, GLOBAL_SYSTEM_MESSAGE_INTERVAL);
                {$ENDIF}
                end;
            end;
          else { do nothing };
        end;

        if AMessage[1] = #32 then
          //begin
          //  if AMessage[2] <> #32 then
          //    NextConditionSetup(AMessage[2]);
          //end
        else
          if AMessage[2] <> #32 then
            FExperiment.ConditionMustBeUpdated := AMessage[2];
      end
    else EndExperimentMessage;
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
    LPopUpHack : TPopupNotifierHack;
    LTime : integer;
  {$ENDIF}
  begin
    if AMessage[2] <> #27 then
      begin
        if AMessage.Count > 1 then
          begin
            // present only one popup with all messages
            LQConsequence := '';
            for i := 3 to AMessage.Count -1 do
              begin
                MID := ExtractDelimited(1,AMessage[i],['#']);
                P := FExperiment.PlayerFromID[MID];
                LQConsequence += DeduceNicname(ShowConsequence(MID, ExtractDelimited(2,AMessage[i],['#']),MID = 'M',False),P)+LineEnding;
              end;

            if LQConsequence <> '' then
              begin
                begin
                {$IFDEF TEST_MODE}
                  FallbackMessages.Items.Append(LQConsequence);
                {$ELSE}
                  if AMessage.Count > 1 then
                    LTime := GLOBAL_MESSAGES_INTERVAL
                  else
                    LTime:= GLOBAL_MESSAGE_INTERVAL;
                  LPopUpHack := TPopupNotifierHack.Create(nil);
                  if Assigned(FormMatrixGame) then
                    LPopUpHack.ShowAndAutoDestroy(LQConsequence,FormMatrixGame,AMessage.Count*LTime)
                  else
                    LPopUpHack.ShowAndAutoDestroy(LQConsequence,nil,AMessage.Count*LTime);
                {$ENDIF}

                end;
              end;
          end;
        ResumeNextTurn;
        //if AMessage[2] <> #32 then
        //  NextConditionSetup(AMessage[2]);
      end
    else EndExperimentMessage;
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
    LPopUpHack : TPopupNotifierHack;
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

    if LGConsequence <> '' then
      begin
        {$IFDEF TEST_MODE}
          FallbackMessages.Items.Append(LGConsequence);
        {$ELSE}
        if LCount > 1 then
          LTime := GLOBAL_MESSAGES_INTERVAL*LCount
        else
          LTime:= GLOBAL_MESSAGE_INTERVAL;
        LPopUpHack := TPopupNotifierHack.Create(nil);
        if Assigned(FormMatrixGame) then
          LPopUpHack.ShowAndAutoDestroy(LGConsequence,FormMatrixGame,LTime)
        else
          LPopUpHack.ShowAndAutoDestroy(LGConsequence,nil,LTime);
        {$ENDIF}
      end;
  end;

begin
  if MHas(K_ARRIVED) then ReceiveActor;
  if MHas(K_CHAT_M)  then ReceiveChat;
  if MHas(K_CHOICE)  then ReceiveChoice;
  if MHas(K_MESSAGE) then ShowConsequence(AMessage[1],AMessage[2],StrToBool(AMessage[3]));
  if MHas(K_GMESSAGE) then ShowGroupedMessage(AMessage[1]);
  if MHas(K_START) then NotifyPlayers;
  if MHas(K_QUESTION) then ShowQuestion;
  if MHas(K_MOVQUEUE) then
      MovePlayerQueue(AMessage[1],AMessage[2]);

  if MHas(K_QMESSAGE) then QuestionMessages;
  if MHas(K_RESUME) then ResumeNextTurn;
  if MHas(K_NXTCND) then NextConditionSetup(AMessage[1],True);
  if MHAs(K_END) then EndExperimentMessage;
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
      PS : string;
  begin
    if not FExperiment.PlayerIsPlaying[ARequest[0]] then
      begin
        if FExperiment.PlayersCount < FExperiment.CurrentCondition.Turn.Value then
          begin
            // ok, let player login
            P.ID := ARequest[0];

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

                {$IFDEF TEST_MODE}
                  P.Nicname := GenResourceName(-1);
                {$ELSE}
                if FExperiment.GenPlayersAsNeeded then
                  P.Nicname := GenResourceName(i)
                else
                  P.Nicname := InputBox
                    (
                      'A new participant arrived.',
                      'What is his/her nickname?',
                      GenResourceName(i)
                    );
                {$ENDIF}
                P.Points.A:=0;
                P.Points.B:=0;
                P.Status:=gpsPlaying;
                P.Choice.Color:=gcNone;
                P.Choice.Row:=grNone;
                P.Turn := FExperiment.FirstTurn[i];
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
                    ARequest.Append(TS);  // FROM 3 to COUNT-5
                  end;
            // appen matrix type
            ARequest.Append(FExperiment.MatrixTypeAsString); // COUNT-4

            // append chat data
            if FExperiment.ShowChat and Assigned(FormMatrixGame) then
              begin
                if FExperiment.SendChatHistoryForNewPlayers then
                  ARequest.Append(FormMatrixGame.ChatMemoRecv.Lines.Text) // COUNT-3
                else
                  ARequest.Append('[CHAT]'); // must append something to keep the message envelop with standard size
              end
            else
              ARequest.Append('[NOCHAT]'); // must append something to keep the message envelop with standard size

            // append global configs.
            ARequest.Append(BoolToStr(FExperiment.ABPoints)); // COUNT-2

            // append condition global data
            ARequest.Append(FExperiment.CurrentConditionAsString);

            // inform all players about the new player, including itself
            FZMQActor.SendMessage([K_ARRIVED,PS]);

            // start Experiment
            if FExperiment.ShouldStartExperiment then
              FExperiment.Play;

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
    P : TPlayer;
    S : string;
    T : string;
  begin
    P := FExperiment.PlayerFromID[ARequest[0]];                    // 0 = ID, 1 = #32
    P.Choice.Row:= GetRowFromString(ARequest[3]);                  // 3 row
    P.Choice.Color:= GetGameColorFromString(ARequest[4]);          // 4 color
    ARequest[2] := K_CHOICE+K_ARRIVED;                             // 2 message topic

    // generate individual consequences and update player
    S := FExperiment.ConsequenceStringFromChoice[P];

    // update turn
    T := FExperiment.NextTurn;
    if T <> #32 then
      UpdatePlayerTurns(T);
  {$IFDEF TEST_MODE}
    FallbackMessages.Items.Append(
      'Experiment.Turns: '+StringReplace(T,'+',LineEnding,[rfReplaceAll]));
  {$ENDIF}
    ARequest.Append(T);                         // 5

    // individual consequences
    ARequest.Append(S);                                            // 6

    // if all participants played
    if FExperiment.IsEndCycle then
      begin
        // group consequences from choices of all players
        ARequest.Append(FExperiment.ConsequenceStringFromChoices); // 7

        // prompt question if an odd row was selected
        S := FExperiment.ShouldAskQuestion;
        ARequest.Append(S);                                        // 8

        // #32 resume else NextGeneration = PlayerToKick AID
        ARequest.Append(FExperiment.NextGeneration);               // 9

        // Check if we need to end the current condition
        if S <> #32 then
          ARequest.Append(#32)// ValidateQuestionResponse
        else
          ARequest.Append(FExperiment.NextCondition);              // 10
      end;
  end;

  procedure ValidateQuestionResponse;
  var
    P : TPlayer;
    M : array of string;
    i : integer;
    LPromptConsequences : TStringList;
  begin
    P := FExperiment.PlayerFromID[ARequest[0]];
    ARequest[2] := K_QUESTION+K_ARRIVED;

    // append response of each player
    FExperiment.CurrentCondition.Prompt.AppendResponse(P.ID,ARequest[3]);

    // return to experiment and present the prompt consequence, if any
    if FExperiment.CurrentCondition.Prompt.ResponsesCount = FExperiment.PlayersCount then
      begin
        // generate messages
        LPromptConsequences := FExperiment.CurrentCondition.Prompt.AsString;
        FExperiment.WriteReportRowPrompt;
        SetLength(M, 3+LPromptConsequences.Count);
        M[0] := K_QMESSAGE;
        M[1] := ARequest[4]; // generation envelop
        M[2] := FExperiment.NextCondition;

        if LPromptConsequences.Count > 0 then
          begin
            for i := 0 to LPromptConsequences.Count-1 do
              begin
                P := FExperiment.PlayerFromID[ExtractDelimited(1,LPromptConsequences[i],['+'])];
                LPromptConsequences[i] := DeduceNicname(LPromptConsequences[i],P);
              end;

            for i := 0 to LPromptConsequences.Count -1 do
              M[i+3] := LPromptConsequences[i]; // messages envelop
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
  {$IFDEF TEST_MODE}
    LNewPlayer.Nicname := GenResourceName(-1);
  {$ELSE}
    if FExperiment.GenPlayersAsNeeded then
      LNewPlayer.Nicname := GenResourceName(-1)
    else
      LNewPlayer.Nicname := InputBox
        (
          'A new generation has started.',
          'A new participant replaced the oldest one. What is the nickname of the new participant?',
          GenResourceName(-1)
        );
  {$ENDIF}
    repeat
      LNewPlayer.ID := TZMQActor.NewRandomID;
    until FExperiment.ValidID(LNewPlayer.ID);
    LN := FExperiment.PlayerAsString[LNewPlayer];
    LO := FExperiment.PlayerAsString[LOldPlayer];
    ARequest.Append(LN); // 3
    ARequest.Append(LO); // 4
  end;

begin
  if FExperiment.State = xsWaiting then
    begin
      if MHas(K_LOGIN) then ReplyLoginRequest;
    end;

  if FExperiment.State = xsRunning then
    begin
      if MHas(K_RESUME) then ReplyResume;
      if MHas(K_CHOICE) then ValidateChoice;
      if MHas(K_QUESTION) then ValidateQuestionResponse;
    end;
end;

// Here FActor is garanted to be a TZMQPlayer, replying by:
// - sending private data to player
// - sending data from early history to income players
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
    if Self.ID = AReply[0] then
      begin
        // lets load already logged in players, if any
        for i:= 3 to AReply.Count -5 do
          begin
            P := FExperiment.PlayerFromString[AReply[i]];
            FExperiment.AppendPlayer(P);
            CreatePlayerBox(P, False);
          end;

        // set matrix type/ stringgrid
        FExperiment.MatrixTypeAsString:=AReply[AReply.Count-4];

        // setup chat
        if Assigned(FormMatrixGame) then
          if AReply[AReply.Count-3] = '[NOCHAT]' then
            FormMatrixGame.ChatPanel.Visible := False
          else
            begin
              FormMatrixGame.ChatPanel.Visible := True;
              FormMatrixGame.ChatMemoRecv.Lines.Clear;
              FormMatrixGame.ChatMemoRecv.Lines.Append(AReply[AReply.Count-3]);
            end;

        // set global configs
        FExperiment.ABPoints := StrToBool(AReply[AReply.Count-2]);
        SetLabels;

        //// we need at least one valid fake condition
        //// in memory for player internals (on start, on generation points).
        //FExperiment.AppendCondition(C_CONDITION_TEMPLATE);

        // set condition specific configurations
        NextConditionSetup(AReply[AReply.Count-1], True);

        // set fullscreen
        if Assigned(FormMatrixGame) then
          FormMatrixGame.SetFullscreen;
        SetMatrix;
      end
    else
      begin
      {$IFDEF DEBUG}
        WriteLn(Self.ID + ' sent but' + AReply[0] +
          ' received. <<<<<<<<<<<<<<<<<<<<<<< This must never occur >>>>>>>>>>>>>>>>>>>>>>>>>>');
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
    if Self.ID = AReply[0] then
      begin
        // inform other players about self.id choice
        FZMQActor.SendMessage([K_CHOICE,AReply[0],AReply[3],AReply[4],AReply[5]]);

        // The Announcer sends a message, waits interval time until all messages have been sent and then destroys itself.
        LAnnouncer := TIntervalarAnnouncer.Create(nil);
        LAnnouncer.OnStart := @FZMQActor.SendMessage;
        LAnnouncer.Interval := GLOBAL_MESSAGE_INTERVAL;

        // individual consequences
        LCount := WordCount(AReply[6],['+']);
        if LCount > 0 then
          for i := 1 to LCount do
            begin
              LConsequence := TConsequence.Create(nil,ExtractDelimited(i,AReply[6],['+']));
              LAnnouncer.Append([K_MESSAGE,
                                 Self.ID,
                                 ExtractDelimited(i,AReply[6],['+']),
                                 BoolToStr(False),
                                 BoolToStr(LConsequence.ShouldPublishMessage)]);
            end;
        LConsequence.Free;

        if AReply.Count > 7 then
          begin
            // meta/ group consequence
            LCount := WordCount(AReply[7],['+']);
            if LCount > 0 then
              LAnnouncer.Append([K_GMESSAGE,AReply[7]]);

            // should ask question or just resume (going to the next turn)?
            if AReply[8] <> #32 then
              LAnnouncer.Append([K_QUESTION,AReply[8],AReply[9],AReply[10]])
            else
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
    // new player (AsString) and old player (AsString)
    FZMQActor.SendMessage([K_MOVQUEUE, AReply[3],AReply[4]]);
  end;

begin
  if MHas(K_RESUME+K_ARRIVED) then ResumePlayer;
  if MHas(K_LOGIN+K_ARRIVED) then LoginAccepted;
  if MHas(K_CHOICE+K_ARRIVED) then ChoiceValidated;
end;

procedure TGameControl.UpdatePlayerTurns(ANextTurnString: string);
var
  LCount: integer;
  LCode : string;
  LPlayerID : string;
  LTurnID, i: integer;
  P: TPlayer;
begin
  LCount := WordCount(ANextTurnString,['+']);
  if LCount > 0 then
    for i := 1 to LCount do
      begin
        LCode     := ExtractDelimited(i,ANextTurnString,['+']);
        LPlayerID := ExtractDelimited(1,LCode,['|']);
        LTurnID   := StrToInt(ExtractDelimited(2,LCode,['|']));
        P := FExperiment.PlayerFromID[LPlayerID];
        P.Turn := LTurnID;
        FExperiment.PlayerFromID[LPlayerID] := P;
        //if Self.ID = LPlayerID then
        //  begin

        //  end;
      end;
end;



end.

