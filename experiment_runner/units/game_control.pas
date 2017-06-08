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
  Classes, SysUtils, Graphics, StdCtrls, PopupNotifier, Grids
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
    FID: UTF8string;
    FActor : TGameActor;
    FZMQActor : TZMQActor;
    FExperiment : TExperiment;
    function GetPlayerBox(AID:UTF8string) : TPlayerBox;
    function GetActorNicname(AID:UTF8string) : UTF8string;
    function MessageHas(const A_CONST : UTF8string; AMessage : TStringList; I:ShortInt=0): Boolean;
    procedure CreatePlayerBox(P:TPlayer; Me:Boolean;Admin:Boolean = False);
    procedure UpdatePlayerBox(P:TPlayer; Me:Boolean;Admin:Boolean = False);
    //procedure DeletePlayerBox(AID : string);
    //procedure MovePlayerBox(AID : string);
    procedure SetMatrixType(AStringGrid : TStringGrid; AMatrixType:TGameMatrixType);
    procedure ReceiveMessage(AMessage : TStringList);
    procedure ReceiveRequest(var ARequest : TStringList);
    procedure ReceiveReply(AReply : TStringList);
    procedure MovePlayerQueue(ANewPlayerString,AOldPlayerID:string);
  private
    function AskQuestion(AQuestion:string):UTF8string;
    function ShowConsequence(AID,S:string;ForGroup:Boolean;ShowPopUp : Boolean = True) : string;

    procedure NextConditionSetup(S : string; IsConditionStart:Boolean=False);
    procedure NextGenerationSetup(AID : string);
    procedure EnablePlayerMatrix(AID:UTF8string; ATurn:integer; AEnabled:Boolean);
  private
    FGroupBoxPlayers: TGroupBox;
    FLabelGroup: TLabel;
    FLabelPointA: TLabel;
    FLabelPointB: TLabel;
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
    FSystemPopUp: TPopupNotifier;
    procedure Interlocking(Sender: TObject);
    procedure SetGroupBoxPlayers(AValue: TGroupBox);
    procedure SetLabelGroup(AValue: TLabel);
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
    procedure SendRequest(ARequest : UTF8string; AInputData : array of UTF8String);
    procedure SendMessage(AMessage : UTF8string; AInputData : array of UTF8String);
    procedure Cancel;
    procedure Start;
    procedure Pause;
    procedure Resume;
    procedure Stop;
    property Experiment : TExperiment read FExperiment write FExperiment;
    property ID : UTF8string read FID;
  public
    procedure ShowSystemPopUp(AText:string;AInterval : integer);
    property SystemPopUp : TPopupNotifier read FSystemPopUp write SetSystemPopUp;
    property LabelGroup : TLabel read FLabelGroup write SetLabelGroup;
    property LabelPointA : TLabel read FLabelPointA write SetLabelPointA;
    property LabelPointB : TLabel read FLabelPointB write SetLabelPointB;
    property LabelPointI : TLabel read FLabelPointI write SetLabelPointI;
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

uses ButtonPanel,Controls,ExtCtrls, LazUTF8, Forms, Dialogs, strutils
     , game_visual_matrix_a
     , popup_hack
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
begin
  ShowSystemPopUp('O Experimento terminou.',GLOBAL_SYSTEM_MESSAGE_INTERVAL);
  if Assigned(FOnEndExperiment) then FOnEndExperiment(Sender);
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

procedure TGameControl.SetGroupBoxPlayers(AValue: TGroupBox);
begin
  if FGroupBoxPlayers=AValue then Exit;
  FGroupBoxPlayers:=AValue;
end;

procedure TGameControl.SetLabelGroup(AValue: TLabel);
begin
  if FLabelGroup=AValue then Exit;
  FLabelGroup:=AValue;
end;

procedure TGameControl.SetLabelPointA(AValue: TLabel);
begin
  if FLabelPointA=AValue then Exit;
  FLabelPointA:=AValue;
end;

procedure TGameControl.SetLabelPointB(AValue: TLabel);
begin
  if FLabelPointB=AValue then Exit;
  FLabelPointB:=AValue;
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

function TGameControl.GetPlayerBox(AID: UTF8string): TPlayerBox;
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

function TGameControl.GetActorNicname(AID: UTF8string): UTF8string;
begin
  Result := '';
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
  with TPlayerBox.Create(GroupBoxPlayers,P.ID,Admin) do
    begin
      if Me then
        Caption := P.Nicname+SysToUtf8(' (Você)' )
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

procedure TGameControl.UpdatePlayerBox(P: TPlayer; Me: Boolean; Admin: Boolean);
begin
  with GetPlayerBox(P.ID) do
    begin
      if Me then
        Caption := P.Nicname+SysToUtf8(' (Você)' )
      else
        Caption := P.Nicname;
      if Admin then
        begin
          LabelPointsCount.Caption := '0';
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

procedure TGameControl.MovePlayerQueue(ANewPlayerString,AOldPlayerID:string);
var
  P : TPlayer;
begin
  P := FExperiment.PlayerFromString[ANewPlayerString]; // new
  UpdatePlayerBox(P,Self.ID = P.ID, FActor=gaAdmin);
  if FExperiment.ConditionMustBeUpdated <> '' then
    begin
      NextConditionSetup(FExperiment.ConditionMustBeUpdated);
      FExperiment.ConditionMustBeUpdated := '';
    end;
  NextGenerationSetup(P.ID);
  if FActor=gaPlayer then
    begin
      P.Turn := FExperiment.Player[AOldPlayerID].Turn;
      FExperiment.Player[AOldPlayerID] := P;
      EnablePlayerMatrix(Self.ID,0, True);
    end;
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
    with Prompt do
      begin
        BorderStyle:=bsNone;
        Position:=poScreenCenter;
        ButtonPanel:=TButtonPanel.Create(Prompt);
        with ButtonPanel do
          begin
            ButtonOrder:=boCloseOKCancel;
            OKButton.Caption:='Sim';
            CancelButton.Caption:='Não';
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
var
  PopUpPos : TPoint;
  // temporary hack
  L : TLabel;
  r: TRect;
  w:integer;
begin
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
end;

function TGameControl.ShowConsequence(AID, S: string;
  ForGroup: Boolean; ShowPopUp: Boolean):string;
var
  LConsequence : TConsequence;
begin
  Result := '';
  LConsequence := TConsequence.Create(nil,S);
  Result := LConsequence.GenerateMessage(ForGroup);
  case FActor of
    gaPlayer:
      if ForGroup then
        LConsequence.PresentPoints(LabelPointA,LabelPointB,LabelPointI,LabelGroup)
      else
        if Self.ID = AID then
          LConsequence.PresentPoints(LabelPointA,LabelPointB,LabelPointI,LabelGroup);

    gaAdmin:
      begin
        // player box is ignored for group points
        // LabelGroupCount is ignored for player points
        LConsequence.PresentPoints(GetPlayerBox(AID),LabelGroup);
      end;
  end;

  if ShowPopUp then
    begin
      if Assigned(FormMatrixGame) then
        LConsequence.PresentMessage(FormMatrixGame.GBPoints)
      else
        LConsequence.PresentMessage(TWinControl(Owner.Owner))
    end
  else
    LConsequence.Free;
end;

procedure TGameControl.NextConditionSetup(S: string; IsConditionStart: Boolean); // [player_points]
var
  A, B, G : integer;
  LNewA : integer = 0;
  LNewB : integer = 0;
  P : TPlayer;
  PB : TPlayerBox;
  C : TCondition;
begin
  if FExperiment.ABPoints then
    begin
      A := StrToInt(ExtractDelimited(1,S,['|']));
      B := StrToInt(ExtractDelimited(2,S,['|']));
      G := StrToInt(ExtractDelimited(3,S,['|']));
    end
  else
    begin
      A := StrToInt(ExtractDelimited(1,S,['|']));
      G := StrToInt(ExtractDelimited(2,S,['|']));
    end;
  IncLabel(LabelGroup,G);

  case FActor of
    gaPlayer:
      begin
        C := FExperiment.Condition[0];
        //P := FExperiment.PlayerFromID[ID]; should refactor this to use inc instead inclabel
        with C.Points do
          begin
            OnStart.A := A;
            OnStart.B := B;
          end;
        FExperiment.Condition[0] := C;
        {$IFDEF DEBUG}
        FormMatrixGame.ChatMemoRecv.Append(IntToStr(FExperiment.CurrentCondition.Points.OnStart.A));
        {$ENDIF}
        if IsConditionStart then
          if FExperiment.ABPoints then
            begin
              //Inc(P.Points.A, A);
              //Inc(P.Points.B, B);
              IncLabel(LabelPointA,A);
              IncLabel(LabelPointB,B);
            end
          else
            begin
              //Inc(P.Points.A, A);
              IncLabel(LabelPointA,A);
            end;
        //FExperiment.PlayerFromID[ID] := P;
      end;

    gaAdmin:
      if IsConditionStart then
        for P in FExperiment.Players do
          begin
            PB := GetPlayerBox(P.ID);
            LNewA := A;
            LNewB := B;
            if FExperiment.ABPoints then
              begin
                LNewA += LNewB;
                IncLabel(PB.LabelPointsCount,LNewA);
              end
            else
              IncLabel(PB.LabelPointsCount,LNewA);
          end;
  end;
end;

procedure TGameControl.NextGenerationSetup(AID: string); // [player_points]
var
  A : integer;
  B : integer;
  LNewA : integer = 0;
  LNewB : integer = 0;
  PB : TPlayerBox;
begin
  with FExperiment.CurrentCondition.Points do
    begin
      A := OnStart.A;
      B := OnStart.B;
      LNewA := A;
      LNewB := B;
    end;

  case FActor of
    gaPlayer:
      if Self.ID = AID then
        if FExperiment.ABPoints then
          begin
            IncLabel(LabelPointA,A);
            IncLabel(LabelPointB,B);
          end
        else
          IncLabel(LabelPointA,A);

    gaAdmin:
      begin
        PB := GetPlayerBox(AID);
        LNewA := A;
        LNewB := B;
        if FExperiment.ABPoints then
          LNewA += LNewB;
        IncLAbel(PB.LabelPointsCount,LNewA);
      end;
  end;
end;

procedure TGameControl.EnablePlayerMatrix(AID:UTF8string; ATurn:integer; AEnabled:Boolean);
begin
  if FExperiment.PlayerFromID[AID].Turn = ATurn then
    begin
      if Assigned(OnCleanEvent) then
        OnCleanEvent(Self, AEnabled);

      if AEnabled then
        begin
          ShowSystemPopUp(
            'É sua vez! Clique sobre uma linha da matriz e confirme sua escolha.',
            GLOBAL_SYSTEM_MESSAGE_INTERVAL
          );
          {$IFDEF DEBUG}
            {$IFDEF WINDOWS}
              // todo:
            {$ELSE}
              FormMatrixGame.BringToFront;
            {$ENDIF}
          {$ENDIF}
        end;
    end;
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

  case FActor of
    gaAdmin:FExperiment := TExperiment.Create(FZMQActor.Owner,AppPath);
    gaPlayer:FExperiment := TExperiment.Create(FZMQActor.Owner);
    gaWatcher:FExperiment := TExperiment.Create(FZMQActor.Owner);
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
    with FormMatrixGame do
      begin
        // a b points
        LabelIndA.Visible := FExperiment.ABPoints;
        LabelIndB.Visible := FExperiment.ABPoints;
        LabelIndACount.Visible := FExperiment.ABPoints;
        LabelIndBCount.Visible := FExperiment.ABPoints;
        ImageIndA.Visible := FExperiment.ABPoints;
        ImageIndB.Visible := FExperiment.ABPoints;

        // i points
        LabelInd.Visible := not FExperiment.ABPoints;
        LabelIndCount.Visible := not FExperiment.ABPoints;
        ImageInd.Visible:= not FExperiment.ABPoints;;
      end;
end;

// called from outside
procedure TGameControl.SendRequest(ARequest: UTF8string;
  AInputData: array of UTF8String);
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
  end;
  FZMQActor.Request(M);
end;

// called from outside
procedure TGameControl.SendMessage(AMessage: UTF8string;
  AInputData: array of UTF8String);
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
  end;
  FZMQActor.SendMessage(M);
end;

procedure TGameControl.Cancel;
begin
  FZMQActor.SendMessage([K_END]);
end;

// Here FActor can be TZMQPlayer or TZMQAdmin
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
    FPlayerBox : TPlayerBox;
    LIDTurn: String;
    LCount, i: Integer;
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
                begin
                  LCount := WordCount(AMessage[4],['+']);
                  if LCount > 0 then
                    for i := 1 to LCount do
                      begin
                        LIDTurn := ExtractDelimited(i,AMessage[4],['+']);
                        if Self.ID = ExtractDelimited(1,LIDTurn,['|']) then
                          begin
                            P := FExperiment.PlayerFromID[Self.ID];
                            P.Turn := StrToInt(ExtractDelimited(2,LIDTurn,['|']));
                            FExperiment.PlayerFromID[Self.ID] := P;
                          end;
                      end;
                end;
              if Assigned(OnCleanEvent) then
                OnCleanEvent(Self, False);

              // do not wait for server
              // if should continue then
              // if StrToBool(AMessage[6]) then
              // EnablePlayerMatrix(Self.ID,0, True)

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
    end;
  end;

  procedure NotifyPlayers;
  begin
    case FActor of
      gaPlayer:
          if FExperiment.PlayerFromID[Self.ID].Turn = 0 then
              EnablePlayerMatrix(Self.ID, 0, True)
          else
              ShowSystemPopUp('Começou! Aguarde sua vez.',GLOBAL_SYSTEM_MESSAGE_INTERVAL);

      gaAdmin:
        NextConditionSetup(FExperiment.CurrentConditionAsString,True);

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
  var Pts : string;
  begin
    case FActor of
      gaPlayer:
        begin
          //DeletePlayerBox(AID); // old player
          if Self.ID = AID then
            begin
              if FExperiment.ABPoints then
                begin
                  Pts := IntToStr(StrToInt(LabelPointA.Caption)+StrToInt(LabelPointB.Caption));
                  LabelPointA.Caption := '0';
                  LabelPointB.Caption := '0';
                end
              else
                begin
                  Pts := LabelPointI.Caption;
                  LabelPointI.Caption := '0';
                end;

              if Assigned(FormMatrixGame) then
                FormMatrixGame.Visible := False;
              FormChooseActor := TFormChooseActor.Create(nil);
              FormChooseActor.Style := K_LEFT;
              FormChooseActor.ShowPoints(
                'A tarefa terminou, obrigado por sua participação!'+LineEnding+
                'Você produziu ' + Pts + ' fichas e ' +
                LabelGroup.Caption + ' itens escolares serão doados a uma escola pública.'
              );

              if FormChooseActor.ShowModal = 1 then
                begin
                  if Assigned(FormMatrixGame) then
                    FormMatrixGame.Visible := True;
                  FZMQActor.Request([AID,' ',K_RESUME]);
                end
              else;
              FormChooseActor.Free;
            end
          else
            ShowSystemPopUp(
              FExperiment.PlayerFromID[AID].Nicname+ ' saiu. Por favor, aguarde a chegada de alguém para ocupar o lugar.',
              GLOBAL_SYSTEM_MESSAGE_INTERVAL
            );
        end;
    end;
  end;

  procedure EndExperimentMessage;
  var Pts : string;
  begin
    case FActor of
      gaPlayer:
        begin
          if Assigned(OnCleanEvent) then
            OnCleanEvent(Self, False);

          //if Assigned(FormMatrixGame) then
          //  FormChooseActor := TFormChooseActor.Create(FormMatrixGame)
          //else
          FormChooseActor := TFormChooseActor.Create(nil);
          FormChooseActor.Style := K_END;

          if FExperiment.ABPoints then
            Pts := IntToStr(StrToInt(LabelPointA.Caption)+StrToInt(LabelPointB.Caption))
          else
            Pts := LabelPointI.Caption;

          FormChooseActor.ShowPoints(
          'A tarefa terminou, obrigado por sua participação!'+LineEnding+
          'Você produziu ' + Pts + ' fichas e ' +
          LabelGroup.Caption + ' itens escolares serão doados a uma escola pública.');
          FormChooseActor.ShowModal;
          FormChooseActor.Free;
          if Assigned(FormMatrixGame) then
            FormMatrixGame.Close;
        end;
      gaAdmin:Stop;
    end;
  end;

  procedure ResumeNextTurn;
  var
    LPlayerBox : TPlayerBox;
    P : TPlayer;
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
                  LPlayerBox := GetPlayerBox(AMessage[1]);
                  P := FExperiment.PlayerFromID[AMessage[1]];
                  if Assigned(OnPlayerExit) then
                    OnPlayerExit(P, LPlayerBox.Caption+','+LPlayerBox.LabelPointsCount.Caption);
                  //DeletePlayerBox(AMessage[1]);
                  ShowSystemPopUp(
                          'O participante '+ P.Nicname +
                          ' saiu. Aguardando a entrada do próximo participante.',
                          GLOBAL_SYSTEM_MESSAGE_INTERVAL
                        );
                end;
            end;
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
    LPopUpHack : TPopupNotifierHack;
    LTime : integer;
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
                  if AMessage.Count > 1 then
                    LTime := GLOBAL_MESSAGES_INTERVAL
                  else
                    LTime:= GLOBAL_MESSAGE_INTERVAL;

                  LPopUpHack := TPopupNotifierHack.Create(nil);
                  if Assigned(FormMatrixGame) then
                    LPopUpHack.ShowAndAutoDestroy(LQConsequence,FormMatrixGame,AMessage.Count*LTime)
                  else
                    LPopUpHack.ShowAndAutoDestroy(LQConsequence,nil,AMessage.Count*LTime);

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
    LCount,
    LTime,
    i : integer;
    MID : string;
    LConsequence : string;
    LGConsequence : string;
    LPopUpHack : TPopupNotifierHack;
  begin
    // present only one popup with all messages
    LConsequence := '';
    LGConsequence := '';
    LCount := WordCount(AMessage,['+']);
    if LCount > 1 then
      LTime := GLOBAL_MESSAGES_INTERVAL*LCount
    else
      LTime:= GLOBAL_MESSAGE_INTERVAL;

    if LCount > 0 then
      for i := 1 to LCount do
        begin
          LConsequence := ExtractDelimited(i,AMessage,['+']);
          MID := ExtractDelimited(1,LConsequence,['#']);
          LGConsequence += ShowConsequence(MID, ExtractDelimited(2,LConsequence,['#']),MID = 'M',False)+LineEnding;
        end;

    if LGConsequence <> '' then
      begin
        LPopUpHack := TPopupNotifierHack.Create(nil);
        if Assigned(FormMatrixGame) then
          LPopUpHack.ShowAndAutoDestroy(LGConsequence,FormMatrixGame,LTime)
        else
          LPopUpHack.ShowAndAutoDestroy(LGConsequence,nil,LTime);
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
    if FActor = gaPlayer then
      MovePlayerQueue(AMessage[1],AMessage[2]);

  if MHas(K_QMESSAGE) then QuestionMessages;
  if MHas(K_RESUME) then ResumeNextTurn;
  if MHas(K_NXTCND) then NextConditionSetup(AMessage[1],True);
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
                if FExperiment.GenPlayersAsNeeded then
                  P.Nicname := GenResourceName(i)
                else
                  P.Nicname := InputBox
                    (
                      'Um participante entrou no experimento.',
                      'Qual o apelido do novo participante?',
                      GenResourceName(i)
                    );
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
  begin
    P := FExperiment.PlayerFromID[ARequest[0]];                    // 0 = ID, 1 = #32
    P.Choice.Row:= GetRowFromString(ARequest[3]);                  // 3 row
    P.Choice.Color:= GetGameColorFromString(ARequest[4]);          // 4 color
    ARequest[2] := K_CHOICE+K_ARRIVED;                             // 2 message topic

    // generate individual consequences and update player
    S := FExperiment.ConsequenceStringFromChoice[P];

    // update turn
    ARequest.Append(FExperiment.NextTurn);                         // 5

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
    M : array of UTF8string;
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
    P : TPlayer;
    S : string;
  begin
    P := FExperiment.PlayerFromID[ARequest[0]];
    ARequest[2] := K_RESUME+K_ARRIVED;
    if FExperiment.GenPlayersAsNeeded then
      P.Nicname := GenResourceName(-1)
    else
      P.Nicname := InputBox
        (
          'Mudança de geração',
          'Um novo participante entrou no lugar do participante mais antigo. Qual o apelido do novo participante?',
          GenResourceName(-1)
        );

    S := FExperiment.PlayerAsString[P];
    FExperiment.NextGeneration := S;
    MovePlayerQueue(S,ARequest[0]);
    ARequest.Append(S); // 3
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

        // we need a valid fake condition in memory for player internals (on start, on generation points).
        FExperiment.AppendCondition(C_CONDITION_TEMPLATE);

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
    FZMQActor.SendMessage([K_MOVQUEUE, AReply[3],AReply[0]]); //new player,old player (self.id)
  end;

begin
  if MHas(K_RESUME+K_ARRIVED) then ResumePlayer;
  if MHas(K_LOGIN+K_ARRIVED) then LoginAccepted;
  if MHas(K_CHOICE+K_ARRIVED) then ChoiceValidated;
end;



end.

