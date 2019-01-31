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
    FID: string;
    FActor : TGameActor;
    FZMQActor : TZMQActor;
    function GetPlayerBox(AID:string) : TPlayerBox;
    function GetActorNicname(AID:string) : string;
    function MessageHas(const A_CONST : string; AMessage : TStringList; I:ShortInt=0): Boolean;
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
    function AskQuestion(AQuestion:string):string;
    function ShowConsequence(AID,S:string;ForGroup:Boolean;ShowPopUp : Boolean = True) : string;

    procedure NextConditionSetup(S : string; IsConditionStart:Boolean=False);
    procedure NextGenerationSetup(AID : string);
    procedure EnablePlayerMatrix(AID:string; ATurn:integer; AEnabled:Boolean);
  private
    FGroupBoxPlayers: TGroupBox;
    FCountGroup2: integer;
    FLabelGroup1Name : string;
    FLabelGroup2Name : string;
    FCountPointA: integer;
    FCountPointB: integer;
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
    procedure CummulativeEffect(AValue : integer);
    procedure EndCycle(Sender: TObject);
    procedure Interlocking(Sender: TObject);
    procedure SetGroupBoxPlayers(AValue: TGroupBox);
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
    procedure UpdateGroupPoints(Sender: TObject);
  public
    constructor Create(AOwner : TComponent;AppPath:string = '');overload;
    destructor Destroy; override;
    function LoadFromFile(AFilename : string):Boolean;
    procedure SetMatrix;
    procedure SendRequest(ARequest : string; AInputData : array of string);
    procedure SendMessage(AMessage : string; AInputData : array of string);
    procedure Cancel;
    procedure Start;
    procedure Pause;
    procedure Resume;
    procedure Stop;
    property ID : string read FID;
  public
    procedure ShowSystemPopUp(AText:string; AInterval : integer);
    property SystemPopUp : TPopupNotifier read FSystemPopUp write SetSystemPopUp;
    property CountGroup2 : integer read FCountGroup2;
    property CountPointA : integer read FCountPointA;
    property CountPointB : integer read FCountPointB;
    property LabelGroup1Name : string read FLabelGroup1Name;
    property LabelGroup2Name : string read FLabelGroup2Name;
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
  K_Readjust = '.Reajust';
  K_SMessage = '.SMessage';
  K_UPDATE = '.Update';
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
     , form_points
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
//end

procedure TGameControl.EndExperiment(Sender: TObject);
begin
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

procedure TGameControl.UpdateGroupPoints(Sender: TObject);
begin
  FormPoints.UpdateCaptions;
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
  //Experiment.State:=xsPaused;
  // save to file

  // inform players
end;

procedure TGameControl.Resume;
begin
  //Experiment.State:=xsRunning;
  // load from file

  // wait for players
end;

procedure TGameControl.Stop;
begin
  // cleaning
  FormPoints.EndExperiment;
  ShowSystemPopUp('O Experimento terminou.',GLOBAL_SYSTEM_MESSAGE_INTERVAL);
  Experiment.Clean;
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
      Result := 'UNKNOWN';
      if Experiment.Player[AID].ID <> '' then
        Result := Experiment.Player[AID].Nicname;
    end;

    gaAdmin: Result := Experiment.Researcher;
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
var
  i1 : integer;
  LPlayerLabel : TLabel;
  LPlayerCounterLabel : TPlayerCounterLabel;
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

  if Admin then
    begin
      LPlayerLabel := TLabel.Create(FormPoints.GBTokens);
      with LPlayerLabel do
        begin
          Caption := P.Nicname;
          Alignment:=taLeftJustify;
          Layout:=tlCenter;
          Parent := FormPoints.GBTokens;
        end;
      LPlayerCounterLabel :=
        TPlayerCounterLabel.Create(FormPoints.GBTokens, P.ID);
      LPlayerCounterLabel.Sister := LPlayerLabel;
      LPlayerCounterLabel.Parent := FormPoints.GBTokens;
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
  if Admin then
    with TPlayerCounterLabel.FromId(FormPoints.GBTokens, P.ID) do
      begin
        Caption := '0';
        Sister.Caption := P.Nicname;
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
  P := Experiment.PlayerFromString[ANewPlayerString]; // new
  UpdatePlayerBox(P,Self.ID = P.ID, FActor=gaAdmin);
  if Experiment.ConditionMustBeUpdated <> '' then
    begin
      NextConditionSetup(Experiment.ConditionMustBeUpdated);
      Experiment.ConditionMustBeUpdated := '';
    end;
  NextGenerationSetup(P.ID);
  if FActor=gaPlayer then
    begin
      P.Turn := Experiment.Player[AOldPlayerID].Turn;
      Experiment.Player[AOldPlayerID] := P;
      EnablePlayerMatrix(Self.ID,0, True);
    end;
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
  LPlayerBox : TPlayerBox;
  LPlayerLabel : TPlayerCounterLabel;
  LPlayer : TPlayer;
  //LGroupItems : integer;
begin
  LConsequence := TConsequence.Create(nil,S);
  Result := LConsequence.GenerateMessage(ForGroup);
  case FActor of
    gaPlayer:
      begin
        //// we must update it from an admin
        //LGroupItems := 0;
        //if ForGroup then
        //  LConsequence. (FCountPointA,FCountPointB,
        //    LGroupItems, FCountGroup2)
        //else
        //  if Self.ID = AID then
        //    LConsequence.CalculatePoints(FCountPointA,FCountPointB,
        //      LGroupItems, FCountGroup2);
      end;
    gaAdmin:
      begin
        //if Assigned(FormPoints) then
        //  FormPoints.UpdateCaptions;
        //LGroupItems := Experiment.CurrentCondition.Points.Count.G1;
        if ForGroup then
        begin
          //LConsequence.CalculatePoints(FCountPointA, FCountPointB,
          //  LGroupItems, FCountGroup2);
          //Experiment.UpdateGroupItems(LGroupItems);
        end
        else
          begin
            LPlayer := Experiment.PlayerFromID[AID];
            LPlayerLabel :=
              TPlayerCounterLabel.FromId(FormPoints.GBTokens, AID);
            LPlayerBox := GetPlayerBox(AID);
            LConsequence.PresentPoints(LPlayer, LPlayerBox, LPlayerLabel);
            Experiment.PlayerFromID[AID] := LPlayer;
          end;
      end;
  end;

  if (Result <> '') and ShowPopUp then
    begin
      if Assigned(FormMatrixGame) then
        LConsequence.PresentMessage(FormMatrixGame.StringGridMatrix)
      else
        LConsequence.PresentMessage(TWinControl(Owner.Owner))
    end
  else
    LConsequence.Free;
end;

procedure TGameControl.NextConditionSetup(S: string; IsConditionStart: Boolean); // [player_points]
var
  A, B, G1, G2 , i: integer;
  P : TPlayer;
  PB : TPlayerBox;
  C : TCondition;
  LConditionName, LInitialMessage : string;
begin
  LConditionName := Sanitize(ExtractDelimited(1,S,['|']));
  LInitialMessage := Sanitize(ExtractDelimited(2,S,['|']));
  FLabelGroup1Name := Sanitize(ExtractDelimited(3,S,['|']));
  FLabelGroup2Name := Sanitize(ExtractDelimited(4,S,['|']));
  if Experiment.ABPoints then
    begin
      A := StrToInt(ExtractDelimited(5,S,['|']));
      B := StrToInt(ExtractDelimited(6,S,['|']));
      G1 := StrToIntDef(ExtractDelimited(7,S,['|']), 0);
      G2 := StrToIntDef(ExtractDelimited(8,S,['|']), 0);
    end
  else
    begin
      A := StrToInt(ExtractDelimited(5,S,['|']));
      G1 := StrToIntDef(ExtractDelimited(6,S,['|']), 0);
      G2 := StrToIntDef(ExtractDelimited(7,S,['|']), 0);
    end;

  case FActor of
    gaPlayer:
      begin
        // here condition 0 is used as a template
        // because Experiment is admin only
        C := Experiment.Condition[0];
        C.ConditionName := LConditionName;
        C.InitialMessage := LInitialMessage;
        //P := Experiment.PlayerFromID[ID]; should refactor this to use inc instead inclabel
        with C.Points do
          begin
            OnStart.A := A;
            OnStart.B := B;
          end;
        Experiment.Condition[0] := C;
        {$IFDEF DEBUG}
        FormMatrixGame.ChatMemoRecv.Append(FExperiment.CurrentCondition.Points.OnStart.A.ToString);
        {$ENDIF}
        if IsConditionStart then
          if C.InitialMessage <> '' then
            ShowSystemPopUp(C.InitialMessage, 10000);

          if Experiment.ABPoints then
            begin
              //Inc(P.Points.A, A);
              //Inc(P.Points.B, B);
              IncCount(FCountPointA, A);
              IncCount(FCountPointB, B);
            end
          else
            begin
              //Inc(P.Points.A, A);
              IncCount(FCountPointA, A);
            end;
        //Experiment.PlayerFromID[ID] := P;
      end;

    gaAdmin:
      if IsConditionStart then
      begin
        //Experiment.UpdateGroupItems(G1);
        if Assigned(FormPoints) then
        begin
          if LInitialMessage <> '' then
            FormPoints.ShowSystemMessage(LInitialMessage);
          FormPoints.SetupCondition;
        end;
        for i := Low(Experiment.Players) to High(Experiment.Players) do
          begin
            P := Experiment.Players[i];
            PB := GetPlayerBox(P.ID);

            if Experiment.ABPoints then
              begin
                P.Points.A := P.Points.A+A;
                P.Points.B := P.Points.B+B;
                PB.LabelPointsCount.Caption:=(P.Points.A+P.Points.B).ToString;
              end
            else
              begin
                P.Points.A := P.Points.A+A;
                PB.LabelPointsCount.Caption:=(P.Points.A+P.Points.B).ToString;
              end;
            Experiment.Players[i] := P;
          end;
      end;
  end;
end;

procedure TGameControl.NextGenerationSetup(AID: string); // [player_points]
var
  A : integer;
  B : integer;
  LNewA : integer = 0;
  LNewB : integer = 0;
  //PB : TPlayerBox;
begin
  with Experiment.CurrentCondition.Points do
    begin
      A := OnStart.A;
      B := OnStart.B;
      LNewA := A;
      LNewB := B;
    end;

  case FActor of
    gaPlayer:
      if Self.ID = AID then
        if Experiment.ABPoints then
          begin
            IncCount(FCountPointA,A);
            IncCount(FCountPointB,B);
          end
        else
          IncCount(FCountPointA,A);

    gaAdmin:
      begin
        //PB := GetPlayerBox(AID);
        LNewA := A;
        LNewB := B;
        if Experiment.ABPoints then
          LNewA += LNewB;
        //IncCount(PB.LabelPointsCount,LNewA); todo:fix this
      end;
  end;
end;

procedure TGameControl.EnablePlayerMatrix(AID:string; ATurn:integer; AEnabled:Boolean);
begin
  if Experiment.PlayerFromID[AID].Turn = ATurn then
    begin
      if Assigned(OnCleanEvent) then
        OnCleanEvent(Self, AEnabled);

      if AEnabled then
        begin
          FormMatrixGame.SetFocus;
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

procedure TGameControl.CummulativeEffect(AValue : integer);
begin
  FormPoints.UpdateCummulativeEffect(AValue);
end;

procedure TGameControl.EndCycle(Sender: TObject);
begin
  FormPoints.UpdateCanTalkCount;
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
    gaAdmin:Experiment := TExperiment.Create(FZMQActor.Owner,AppPath);
    gaPlayer:Experiment := TExperiment.Create(FZMQActor.Owner);
    gaWatcher:Experiment := TExperiment.Create(FZMQActor.Owner);
  end;
  Experiment.OnCummulativeEffect:=@CummulativeEffect;
  Experiment.OnStartTurn:=@StartTurn;
  Experiment.OnStartCondition:=@StartCondition;
  Experiment.OnStartCycle:=@StartCycle;
  Experiment.OnStartExperiment:=@StartExperiment;
  Experiment.OnStartGeneration:=@StartGeneration;
  Experiment.OnEndExperiment:= @EndExperiment;
  Experiment.OnInterlocking:=@Interlocking;
  Experiment.OnTargetInterlocking:=@TargetInterlocking;
  Experiment.OnUpdateGroupPoints:=@UpdateGroupPoints;
  //Experiment.OnEndTurn := @NextTurn;
  Experiment.OnEndCycle := @EndCycle;
  //Experiment.OnEndCondition:= @NextCondition;
  //Experiment.OnEndGeneration:=@NextLineage;
  //Experiment.OnConsequence:=@Consequence;

  SendRequest(K_LOGIN,[]); // admin cannot send requests
end;

destructor TGameControl.Destroy;
begin
  inherited Destroy;
end;

function TGameControl.LoadFromFile(AFilename: string): Boolean;
begin
  Result := Experiment.LoadFromFile(AFilename);
  if not Result then Exit;
  case Experiment.ExperimentName of
    'Experimento 1' : ExperimentType := Experiment1;
    'Experimento 2' : ExperimentType := Experiment2;
    else
      begin
        raise Exception.Create(
          Experiment.ExperimentName + #32 +
          'não é um nome válido.' + #32 +
          'Use "Experimento 1" ou "Experimento 2".'
        );
        Halt;
      end
  end;
  SetMatrix;
  //SetLabels;
  if Assigned(FormMatrixGame) then
    if Experiment.ShowChat then
      FormMatrixGame.ChatPanel.Visible := Experiment.ResearcherCanChat
    else
      FormMatrixGame.ChatPanel.Visible := Experiment.ShowChat;
end;

procedure TGameControl.SetMatrix;
begin
  if Assigned(FormMatrixGame) then
    SetMatrixType(FormMatrixGame.StringGridMatrix, Experiment.MatrixType);
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
      //if (FActor = gaAdmin) and (not Experiment.ResearcherCanChat) then Exit;
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
    K_UPDATE : SetM([
        AMessage
        , FZMQActor.ID  // sender
        , AInputData[0] // key
        , AInputData[1] // value
    ]);
    K_SMessage : SetM([
        AMessage
        , FZMQActor.ID  // sender
        , AInputData[0] // kind
        , AInputData[1] // message
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
          P := Experiment.PlayerFromString[AMessage[1]];
          Experiment.AppendPlayer(P);
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
        {$IFDEF TEST_MODE}
        , RandomFrom(['Y', 'N'])
        {$ELSE}
        , AskQuestion(AMessage[1])
        {$ENDIF}
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
    P := Experiment.PlayerFromID[AMessage[1]];
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
          if Experiment.PlayerTurn = Experiment.PlayersCount-1 then
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
                            P := Experiment.PlayerFromID[Self.ID];
                            P.Turn := StrToInt(ExtractDelimited(2,LIDTurn,['|']));
                            Experiment.PlayerFromID[Self.ID] := P;
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
              Experiment.PlayerTurn := 0;
              Exit;
            end;
          Inc(Experiment.PlayerTurn);

          if Self.ID = P.ID then
            begin
              if Assigned(OnEndChoice) then
                OnEndChoice(Self);
            end
          else
            EnablePlayerMatrix(Self.ID,Experiment.PlayerTurn, True);
      end;
    end;
  end;

  procedure NotifyPlayers;
  begin
    case FActor of
      gaPlayer:
          if Experiment.PlayerFromID[Self.ID].Turn = 0 then
              EnablePlayerMatrix(Self.ID, 0, True)
          else
              ShowSystemPopUp('Começou! Aguarde sua vez.',GLOBAL_SYSTEM_MESSAGE_INTERVAL);

      gaAdmin:
        NextConditionSetup(Experiment.CurrentConditionAsString,True);

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
      Experiment.WriteChatLn(ALn);
  end;

  procedure SayGoodBye(AID:string); // [player_points]
  //var Pts : string;
  begin
    case FActor of
      gaPlayer:
        begin
          //DeletePlayerBox(AID); // old player
          if Self.ID = AID then
            begin
              //if Experiment.ABPoints then
              //  Pts := IntToStr(CountPointA+CountPointB)
              //else
              //  Pts := CountPointA.ToString;

              if Assigned(FormMatrixGame) then
                FormMatrixGame.Visible := False;
              FormChooseActor := TFormChooseActor.Create(nil);
              FormChooseActor.Style := K_LEFT;
              FormChooseActor.ShowPoints(
                'A tarefa terminou, obrigado por sua participação!'
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
              Experiment.PlayerFromID[AID].Nicname+ ' saiu. Por favor, aguarde a chegada de alguém para ocupar o lugar.',
              GLOBAL_SYSTEM_MESSAGE_INTERVAL
            );
        end;
    end;
  end;

  procedure EndExperimentMessage;
  //var Pts : string;
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

          //if Experiment.ABPoints then
          //  Pts := (CountPointA+CountPointB).ToString
          //else
          //  Pts := CountPointA.ToString;

          FormChooseActor.ShowPoints(
            'A tarefa terminou, obrigado por sua participação!'
          );
          FormChooseActor.ShowModal;
          FormChooseActor.Free;

          if Assigned(FormMatrixGame) then
            FormMatrixGame.Close;
          {$ENDIF}
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
                  P := Experiment.PlayerFromID[AMessage[1]];
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
            Experiment.ConditionMustBeUpdated := AMessage[2];
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
                P := Experiment.PlayerFromID[MID];
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
    LCount : Cardinal;
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
          LConsequence := ShowConsequence(MID, ExtractDelimited(2,LConsequence,['#']),MID = 'M',False);
          if LConsequence <> '' then
            LGConsequence += LConsequence+LineEnding;
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

  procedure Update;
  var
    LKey, LValue : string;
  begin
    LKey := AMessage[2];
    LValue := AMessage[3];
    case LKey of
      'items': Experiment.AllItems := LValue.ToInteger;
    end;
  end;

  procedure HandleSystemMessage;
  //var
  //  Code : string = '';
  //  Message : string = '';
  begin
    //Code := AMessage[2];
    //Message := AMessage[3];
    if FActor = gaPlayer then
    case AMessage[2] of
      'show': FormMatrixGame.ShowSystemMessage(AMessage[3]);
      'hide': FormMatrixGame.HideSystemMessage;
    end;
  end;
begin
  if MHas(K_ARRIVED) then ReceiveActor;
  if MHas(K_CHAT_M)  then ReceiveChat;
  if MHas(K_CHOICE)  then ReceiveChoice;
  if MHas(K_MESSAGE) then ShowConsequence(AMessage[1],AMessage[2],StrToBool(AMessage[3]));
  if MHas(K_GMESSAGE) then ShowGroupedMessage(AMessage[1]);
  if MHas(K_SMessage) then HandleSystemMessage;
  if MHas(K_START) then NotifyPlayers;
  if Mhas(K_UPDATE) then Update;
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
    if not Experiment.PlayerIsPlaying[ARequest[0]] then
      begin
        if Experiment.PlayersCount < Experiment.CurrentCondition.Turn.Value then
          begin
            // ok, let player login
            P.ID := ARequest[0];

            // check if we already know this player
            i := Experiment.PlayerIndexFromID[P.ID];
            if i > -1 then
              begin
                // then load p data
                P := Experiment.Player[i]
              end
            else
              begin
                // if not then generate and save p data
                i := Experiment.AppendPlayer;
                if Experiment.GenPlayersAsNeeded then
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
                P.Turn := Experiment.FirstTurn[i];
                Experiment.Player[i] := P;
              end;

            // create/config playerbox
            CreatePlayerBox(P,False,True);

            // Request is now a reply with the following standard:
            // [Requester.ID 0, ' ' 1, ReplyTag 2, PlayerData 3, PlayersPlaying 4 .. n, ChatData Last]
            ARequest[2] := GA_ADMIN+ARequest[2]+K_ARRIVED;

            // player
            PS := Experiment.PlayerAsString[P];
            //ARequest.Append(PS);

            // append current players playing
            if Experiment.PlayersCount > 0 then
              for i:=0 to Experiment.PlayersCount -1 do
                if Experiment.Player[i].ID <> P.ID then
                  begin
                    TS := Experiment.PlayerAsString[Experiment.Player[i]];
                    ARequest.Append(TS);  // FROM 3 to COUNT-5
                  end;
            // appen matrix type
            ARequest.Append(Experiment.MatrixTypeAsString); // COUNT-4

            // append chat data
            if Experiment.ShowChat and Assigned(FormMatrixGame) then
              begin
                if Experiment.SendChatHistoryForNewPlayers then
                  ARequest.Append(FormMatrixGame.ChatMemoRecv.Lines.Text) // COUNT-3
                else
                  ARequest.Append('[CHAT]'); // must append something to keep the message envelop with standard size
              end
            else
              ARequest.Append('[NOCHAT]'); // must append something to keep the message envelop with standard size

            // append global configs.
            ARequest.Append(BoolToStr(Experiment.ABPoints)); // COUNT-2

            // append condition global data
            ARequest.Append(Experiment.CurrentConditionAsString);

            // inform all players about the new player, including itself
            FZMQActor.SendMessage([K_ARRIVED,PS]);

            // start Experiment
            if Experiment.ShouldStartExperiment then
              Experiment.Play;

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
    P := Experiment.PlayerFromID[ARequest[0]];                    // 0 = ID, 1 = #32
    P.Choice.Row:= GetRowFromString(ARequest[3]);                  // 3 row
    P.Choice.Color:= GetGameColorFromString(ARequest[4]);          // 4 color
    ARequest[2] := K_CHOICE+K_ARRIVED;                             // 2 message topic

    // generate individual consequences and update player
    S := Experiment.ConsequenceStringFromChoice[P];

    // update turn
    ARequest.Append(Experiment.NextTurn);                         // 5

    // individual consequences
    ARequest.Append(S);                                            // 6

    // if all participants played
    if Experiment.IsEndCycle then
      begin
        // group consequences from choices of all players
        ARequest.Append(Experiment.ConsequenceStringFromChoices); // 7

        // prompt question if an odd row was selected
        S := Experiment.ShouldAskQuestion;
        ARequest.Append(S);                                        // 8

        // #32 resume else NextGeneration = PlayerToKick AID
        ARequest.Append(Experiment.NextGeneration);               // 9

        // Check if we need to end the current condition
        if S <> #32 then
          ARequest.Append(#32)// ValidateQuestionResponse
        else
          ARequest.Append(Experiment.NextCondition);              // 10
      end;
  end;

  procedure ValidateQuestionResponse;
  var
    P : TPlayer;
    M : array of string;
    i : integer;
    LPromptConsequences : TStringList;
  begin
    P := Experiment.PlayerFromID[ARequest[0]];
    ARequest[2] := K_QUESTION+K_ARRIVED;

    // append response of each player
    Experiment.CurrentCondition.Prompt.AppendResponse(P.ID,ARequest[3]);

    // return to experiment and present the prompt consequence, if any
    if Experiment.CurrentCondition.Prompt.ResponsesCount = Experiment.PlayersCount then
      begin
        // generate messages
        LPromptConsequences := Experiment.CurrentCondition.Prompt.AsString;
        Experiment.WriteReportRowPrompt;
        SetLength(M, 3+LPromptConsequences.Count);
        M[0] := K_QMESSAGE;
        M[1] := ARequest[4]; // generation envelop
        M[2] := Experiment.NextCondition;

        if LPromptConsequences.Count > 0 then
          begin
            for i := 0 to LPromptConsequences.Count-1 do
              begin
                P := Experiment.PlayerFromID[ExtractDelimited(1,LPromptConsequences[i],['+'])];
                LPromptConsequences[i] := DeduceNicname(LPromptConsequences[i],P);
              end;

            for i := 0 to LPromptConsequences.Count -1 do
              M[i+3] := LPromptConsequences[i]; // messages envelop
          end;
        Experiment.Clean;
        // send identified messages; each player takes only its own message and ignore the rest
        FZMQActor.SendMessage(M);
      end;
  end;

  procedure ReplyResume;// old player becomes a new player
  var
    P : TPlayer;
    S : string;
  begin
    P := Experiment.PlayerFromID[ARequest[0]];
    ARequest[2] := K_RESUME+K_ARRIVED;
    if Experiment.GenPlayersAsNeeded then
      P.Nicname := GenResourceName(-1)
    else
      P.Nicname := InputBox
        (
          'Mudança de geração',
          'Um novo participante entrou no lugar do participante mais antigo. Qual o apelido do novo participante?',
          GenResourceName(-1)
        );

    S := Experiment.PlayerAsString[P];
    Experiment.NextGeneration := S;
    MovePlayerQueue(S,ARequest[0]);
    ARequest.Append(S); // 3
  end;

begin
  if Experiment.State = xsWaiting then
    begin
      if MHas(K_LOGIN) then ReplyLoginRequest;
    end;

  if Experiment.State = xsRunning then
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
            P := Experiment.PlayerFromString[AReply[i]];
            Experiment.AppendPlayer(P);
            CreatePlayerBox(P, False);
          end;

        // set matrix type/ stringgrid
        Experiment.MatrixTypeAsString:=AReply[AReply.Count-4];

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
        Experiment.ABPoints := StrToBool(AReply[AReply.Count-2]);
        //SetLabels;

        // we need a valid fake condition in memory for player internals (on start, on generation points).
        Experiment.AppendCondition(C_CONDITION_TEMPLATE);

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
        LAnnouncer.Interval := 500;

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

