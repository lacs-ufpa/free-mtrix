{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_visual_board;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, PopupNotifier, Grids, ExtCtrls, Forms
  , game_actors
  , game_control_events
  , game_experiment
  , game_visual_experiment
  , game_visual_elements
  ;

type

  { TGameBoard }

  TGameBoard = class(TGameEvents)
  private
    //FCurrentCause : TGameConsequenceStyle;
    FCause : string;
    FActor : TGameActor;
    FButtonConfirm : TButton;
    FExperiment : TExperiment;
    FGroupBoxAdmin : TGroupBox;
    FGroupBoxExperiment : TGroupBoxExperiment;
    FBackgroundForm : TForm;
    FChat : TStrings;
    FChatPanel : TPanel;
    FGroupBoxPoints : TGroupBox;
    FLabelPointAName : TLabel;
    FLabelPointBName : TLabel;
    FLabelPointICount : TLabel;
    FListBoxOldPlayers : TListBox;
    FStringGridMatrix : TStringGrid;
    FTimer : TTimer;
    FTimerEvent : TNotifyEvent;
    FImagePointA : TImage;
    FImagePointB : TImage;
    FImagePointI : TImage;
    FGroupBoxPlayers  : TGroupBox;
    FLabelGroup1Count : TLabel;
    FLabelGroup2Count : TLabel;
    FLabelGroup1Name  : TLabel;
    FLabelGroup2Name  : TLabel;
    FLabelPointACount : TLabel;
    FLabelPointBCount : TLabel;
    FLabelPointIName      : TLabel;
    FImageGroup1      : TImage;
    FImageGroup2      : TImage;
    FSystemPopUp      : TPopupNotifier;
    FSystemMessage : TLabel;
    procedure VisibleEarningsBeforeGenerationChange(AValue : Boolean);
    procedure SetBackgroundForm(AValue : TForm);
    procedure SetButtonConfirm(AValue : TButton);
    procedure SetChat(AValue : TStrings);
    procedure SetChatPanel(AValue : TPanel);
    procedure SetExperiment(AValue : TExperiment);
    {$IFDEF TEST_MODE}
    procedure SetFallbackMessages(AValue : TListBox);
    {$ENDIF}
    procedure SetGroupBoxAdmin(AValue : TGroupBox);
    procedure SetGroupBoxExperiment(AValue : TGroupBoxExperiment);
    procedure SetGroupBoxPlayers(AValue : TGroupBox);
    procedure SetGroupBoxPoints(AValue : TGroupBox);
    procedure SetImageGroup1(AValue : TImage);
    procedure SetImageGroup2(AValue : TImage);
    procedure SetImagePointA(AValue : TImage);
    procedure SetImagePointB(AValue : TImage);
    procedure SetImagePointI(AValue : TImage);
    procedure SetLabelGroup1(AValue : TLabel);
    procedure SetLabelGroup1Name(AValue : TLabel);
    procedure SetLabelGroup2(AValue : TLabel);
    procedure SetLabelGroup2Name(AValue : TLabel);
    procedure SetLabelPointACount(AValue : TLabel);
    procedure SetLabelPointAName(AValue : TLabel);
    procedure SetLabelPointBCount(AValue : TLabel);
    procedure SetLabelPointBName(AValue : TLabel);
    procedure SetLabelPointICount(AValue : TLabel);
    procedure SetLabelPointIName(AValue : TLabel);
    procedure SetListBoxOldPlayers(AValue : TListBox);
    procedure SetStringGridMatrix(AValue : TStringGrid);
    procedure SetSystemPopUp(AValue : TPopupNotifier);
    procedure SetTimer(AValue : TTimer);
    procedure SetTimerEvent(AValue : TNotifyEvent);
  public
    function GetPlayerBox(AID:string) : TPlayerBox;
    function GetPlayerNicname(AGameContext : TGameContext) : string;
    function AskQuestion(AQuestion:string):string;
    procedure CreatePlayerBox(P:TPlayer; Me:Boolean;Admin:Boolean = False);
    procedure UpdatePlayerBox(P:TPlayer; Me:Boolean;Admin:Boolean = False; AOldPlayerID: string = '');
    procedure SetMatrixType(AStringGrid : TStringGrid; AMatrixType:TGameMatrixType);
    {$IFNDEF TEST_MODE}
    procedure ShowPopupNotifierHack(AMessage : string; AAutoDestroyTime : integer);
    {$ENDIF}
    procedure ShowConsequence(ACallerID, AID : string;
      AConsequence : TConsequence; IsMeta: Boolean; ShowPopUp: Boolean);
    procedure NextConditionSetup;
    procedure InvalidateLabels(AID : string);
    procedure InvalidatePlayerBox(AID, ARow, AColor : string);
  public
  {$IFDEF TEST_MODE}
    FFallbackMessages: TListBox;
    procedure DebugMessage(AMessage : string);
  {$ENDIF}
  protected
    procedure StartExperiment(Sender : TObject); override;
    procedure StartCondition(Sender : TObject); override;
    procedure EndExperiment(Sender : TObject); override;
    procedure PlayerExit(P : TPlayer; AMessage : string); override;
    procedure StartChoice(Sender : TObject); override;
    procedure EndChoice(Sender : TObject); override;
    procedure WaitForServer(Sender : TObject); override;
  public
    constructor Create(AOwner : TComponent; AGameEvents: TGameEvents;
      AActor : TGameActor; ABackgroundForm : TForm; AButtonConfirm: TButton;
      AGroupBoxPlayers: TGroupBox; AGroupBoxPoints : TGroupBox;
      AGroupBoxAdmin : TGroupBox; ASystemPopUp : TPopupNotifier); reintroduce;
    destructor Destroy; override;
    function PlayerSaidGoodBye(AID, AStyle : string) : Boolean;
    function SayGoodByeAndShowSlides(AID : string;
      AGoodByeSlides : TStringArray) : Boolean;
    function ShowSlidesAndSayHello(AID : string;
      AWelcomeSlides : TStringArray) : Boolean;
    procedure BeforeStartExperimentSetup;
    procedure StartSetup;
    procedure SetupChat(ASetup : string);
    procedure FullScreen;
    procedure SetMatrix;
    procedure SetLabels;
    procedure AppendToChat(ALn : string);
    function ShowSlides(ASlides : TStringArray) : Boolean;
    procedure ShowSystemPopUp(AGameContext: TGameContext; AID : string = '');
    procedure ShowSystemMessage(AGameContext: TGameContext; AID : string = '');
    procedure HideSystemMessage;
    function GetPlayerExitMessage(AID : string) : string;
    property LabelGroup1Count : TLabel read FLabelGroup1Count write SetLabelGroup1;
    property LabelGroup2Count : TLabel read FLabelGroup2Count write SetLabelGroup2;
    property LabelPointACount : TLabel read FLabelPointACount write SetLabelPointACount;
    property LabelPointBCount : TLabel read FLabelPointBCount write SetLabelPointBCount;
    property LabelPointICount : TLabel read FLabelPointICount write SetLabelPointICount;
    property ImagePointA : TImage read FImagePointA write SetImagePointA;
    property ImagePointB : TImage read FImagePointB write SetImagePointB;
    property ImagePointI : TImage read FImagePointI write SetImagePointI;
    property ImageGroup1 : TImage read FImageGroup1 write SetImageGroup1;
    property ImageGroup2 : TImage read FImageGroup2 write SetImageGroup2;
    property LabelPointAName : TLabel read FLabelPointAName write SetLabelPointAName;
    property LabelPointBName : TLabel read FLabelPointBName write SetLabelPointBName;
    property LabelPointIName : TLabel read FLabelPointIName write SetLabelPointIName;
    property LabelGroup1Name : TLabel read FLabelGroup1Name write SetLabelGroup1Name;
    property LabelGroup2Name : TLabel read FLabelGroup2Name write SetLabelGroup2Name;
    property SystemPopUp : TPopupNotifier read FSystemPopUp write SetSystemPopUp;
    property Chat : TStrings read FChat write SetChat;
    property BackgroundForm : TForm read FBackgroundForm write SetBackgroundForm;
    property StringGridMatrix : TStringGrid read FStringGridMatrix write SetStringGridMatrix;
    property Timer : TTimer read FTimer write SetTimer;
    property TimerEvent : TNotifyEvent read FTimerEvent write SetTimerEvent;
    property ChatPanel : TPanel read FChatPanel write SetChatPanel;
    property ButtonConfirm : TButton read FButtonConfirm write SetButtonConfirm;
    property GroupBoxPlayers : TGroupBox read FGroupBoxPlayers write SetGroupBoxPlayers;
    property GroupBoxAdmin : TGroupBox read FGroupBoxAdmin write SetGroupBoxAdmin;
    property GroupBoxPoints : TGroupBox read FGroupBoxPoints write SetGroupBoxPoints;
    // TODO: check why lazarus complais about that (internal bug)
    property GroupBoxExperiment : TGroupBoxExperiment read FGroupBoxExperiment write FGroupBoxExperiment;
    property ListBoxOldPlayers : TListBox read FListBoxOldPlayers write SetListBoxOldPlayers;
    property Experiment : TExperiment read FExperiment write SetExperiment;
    {$IFDEF TEST_MODE}
    property FallbackMessages : TListBox read FFallbackMessages write SetFallbackMessages;
    {$ENDIF}
  end;

resourcestring
  //CAP_ID = 'ID';
  //CAP_NAME = 'Name';
  //CAP_RED_TOKENS = 'Red Tokens';
  //CAP_BLUE_TOKENS = 'Blue Tokens';
  //CAP_TOKENS_SUM = 'Red+Blue';
  //CAP_TOKENS_MONEY = 'Money';
  //CAP_G1 = 'Cultural Tokens (Sustainable)';
  //CAP_G2 = 'Cultural Tokens (Non-sustainable)';
  //CAP_CONDITION_CYCLE = 'Condition Cycle';
  //CAP_CYCLES_GENERATION = 'Cycles (Generation)';
  //CAP_PARTICIPANT_ARRIVED_CAPTION = 'A new participant arrived.';
  //CAP_PARTICIPANT_ARRIVED_PROMPT = 'What is his/her nickname?';
  //CAP_NEW_GENERATION_CAPTION = 'A new generation has started.';
  //CAP_NEW_GENERATION_PROMPT =
  //  'A new participant replaced the oldest one. ' +
  //  'What is the nickname of the new participant?';
  //CAP_NEW_PARTICIPANT_LOGGED_IN = 'New participant logged in.';
  //CAP_WELCOME = 'Welcome';
  //CAP_EXPERIMENT_START_PLAYER = 'It is started! Wait for your turn.';
  //CAP_EXPERIMENT_START_ADMIN = 'It is started!';
  //CAP_CHOICE_START = 'It is your turn! Click at a row and confirm your choice.';
  //CAP_PLAYER_EXITED_PLAYER = 'exited. Please, wait while your group member is replaced.';
  //CAP_PLAYER_EXITED_ADMIN = 'exited. Waiting...';
  //CAP_PLAYER_EXITED_END = ' exited the game.';
  //CAP_EXPERIMENT_END = 'The experiment ended.';
  //CAP_WAITING_FOR_SERVER = 'Waiting for server...';
  //CAP_CONFIRM = 'Confirm';
  //CAP_OK = 'OK';
  //CAP_YOU = 'You';
  //CAP_YES = 'Yes';
  //CAP_NO  = 'No';
  CAP_ID = 'ID';
  CAP_NAME = 'Nome';
  CAP_RED_TOKENS = 'Fichas Vermelhas';
  CAP_BLUE_TOKENS = 'Fichas Azuis';
  CAP_TOKENS_SUM = 'Vermelhas+Azuis';
  CAP_TOKENS_MONEY = 'Dinheiro';
  CAP_G1 = 'Fichas Culturais (G1)';
  CAP_G2 = 'Fichas Culturais (G2)';
  CAP_CONDITION_CYCLE = 'Ciclo da Condição';
  CAP_CYCLES_GENERATION = 'Ciclos (Geração)';
  CAP_PARTICIPANT_ARRIVED_CAPTION = 'Um novo participante chegou.';
  CAP_PARTICIPANT_ARRIVED_PROMPT = 'Qual o nome dele?';
  CAP_NEW_GENERATION_CAPTION = 'Uma nova geração foi iniciada.';
  CAP_NEW_GENERATION_PROMPT =
    'Um novo participante substituiu aquele mais antigo. ' +
    'Qual o nome do novo participante?';
  CAP_NEW_PARTICIPANT_LOGGED_IN = 'Um novo participante entrou.';
  CAP_WELCOME = 'Seja bem vind@';
  CAP_EXPERIMENT_START_PLAYER = 'Começou! Aguarde por seu turno.';
  CAP_EXPERIMENT_START_ADMIN = 'Começou!';
  CAP_CHOICE_START = 'É seu turno! Clique em uma linha e confirme sua escolha.';
  CAP_PLAYER_EXITED_PLAYER = 'saiu. Por favor, aguarde enquanto o membro do seu grupo é substituido.';
  CAP_PLAYER_EXITED_ADMIN = 'saiu. Aguardando...';
  CAP_PLAYER_EXITED_END = 'saiu do jogo.';
  CAP_EXPERIMENT_END = 'O experimentou finalizou.';
  CAP_WAITING_FOR_SERVER = 'Aguardando o servidor...';
  CAP_CONFIRM = 'Confirmar';
  CAP_OK = 'OK';
  CAP_YOU = 'Você';
  CAP_YES = 'Sim';
  CAP_NO  = 'Não';


implementation

uses
  ButtonPanel
  , Controls
  , LazUTF8
  , Dialogs
  , game_control
  , game_visual_matrix_a
  , form_chooseactor
  , form_slide
  , game_resources
  , game_actors_helpers
  , string_methods
  , strutils
  , Helpers.Fullscreen
  {$IFDEF TEST_MODE}
    { do nothing }
  {$ELSE}
    , popup_hack
  {$ENDIF}
  ;

procedure VisibleControl(AControl : TControl; AVisible : Boolean);
begin
  if Assigned(AControl) then begin
    AControl.Visible := AVisible;
  end;
end;

procedure TGameBoard.EndExperiment(Sender : TObject);
var
  LPath : string;
begin
  ShowSystemPopUp(gmcExperimentEnd);
  if FActor = gaAdmin then begin
    if Assigned(ListBoxOldPlayers) then begin
      LPath := FExperiment.Report.FileName+'.generations.txt';
      ListBoxOldPlayers.Items.SaveToFile(LPath);
    end;
  end;
  inherited EndExperiment(Sender);
end;

procedure TGameBoard.PlayerExit(P : TPlayer; AMessage : string);
var
  LMessage : string;
const
  SEP : string = ': ';
begin
  if Assigned(ListBoxOldPlayers) then
  begin
    LMessage :=
      CAP_ID + SEP +
        AMessage + LineEnding +
      CAP_NAME + SEP +
        P.Nicname + LineEnding +
      CAP_RED_TOKENS + SEP +
        P.Points.A.ToString + LineEnding +
      CAP_BLUE_TOKENS + SEP +
        P.Points.B.ToString + LineEnding +
      CAP_TOKENS_SUM + SEP +
        (P.Points.A + P.Points.B).ToString + LineEnding +
      CAP_TOKENS_MONEY + SEP +
        PointsToMoney(P.Points.A + P.Points.B, 10) + LineEnding +
      CAP_G1 + SEP +
        FExperiment.GlobalPoints(gscG1).ToString + LineEnding +
      CAP_G2 + SEP +
        FExperiment.GlobalPoints(gscG2).ToString + LineEnding +
      CAP_CONDITION_CYCLE + SEP +
        FExperiment.CurrentCondition.Cycles.Count.ToString + LineEnding +
      CAP_CYCLES_GENERATION + SEP +
        (FExperiment.LastGenerationCount+1).ToString;
    ListBoxOldPlayers.Items.Append(LMessage);
  end;
  inherited PlayerExit(P, AMessage);
end;

procedure TGameBoard.StartChoice(Sender : TObject);
begin
  if Assigned(BackgroundForm) then begin
    StringGridMatrix.Enabled := True;
    StringGridMatrix.Options := StringGridMatrix.Options-[goRowSelect];
    ButtonConfirm.Enabled:=True;
    ButtonConfirm.Caption:=CAP_CONFIRM;
    ButtonConfirm.Visible := False;
    BackgroundForm.Invalidate;
  end;

  ShowSystemPopUp(gmcChoiceStart);
  inherited StartChoice(Sender);
end;

procedure TGameBoard.EndChoice(Sender : TObject);
begin
  if Assigned(BackgroundForm) then begin
    StringGridMatrix.Enabled:= False;
    ButtonConfirm.Enabled:=False;
    ButtonConfirm.Caption:=CAP_OK;
    BackgroundForm.Invalidate;
  end;
  inherited EndChoice(Sender);
end;

procedure TGameBoard.WaitForServer(Sender : TObject);
begin
  if Assigned(BackgroundForm) then begin
    StringGridMatrix.Enabled := False;
    StringGridMatrix.Options := StringGridMatrix.Options-[goRowSelect];
    ButtonConfirm.Enabled:=True;
    ButtonConfirm.Caption:=CAP_CONFIRM;
    ButtonConfirm.Visible := False;
    BackgroundForm.Invalidate;
  end;

  ShowSystemPopUp(gmcWaitingForServer);

  inherited WaitForServer(Sender);
end;

{$IFDEF TEST_MODE}
procedure TGameBoard.DebugMessage(AMessage : string);
begin
  FFallbackMessages.Items.Append(AMessage);
end;

procedure TGameBoard.SetFallbackMessages(AValue : TListBox);
begin
  if FFallbackMessages = AValue then Exit;
  FFallbackMessages := AValue;
end;
{$ENDIF}

procedure TGameBoard.SetBackgroundForm(AValue : TForm);
begin
  if FBackgroundForm = AValue then Exit;
  FBackgroundForm := AValue;
end;

procedure TGameBoard.SetButtonConfirm(AValue : TButton);
begin
  if FButtonConfirm = AValue then Exit;
  FButtonConfirm := AValue;
end;

procedure TGameBoard.SetChat(AValue : TStrings);
begin
  if FChat = AValue then Exit;
  FChat := AValue;
end;

procedure TGameBoard.SetChatPanel(AValue : TPanel);
begin
  if FChatPanel = AValue then Exit;
  FChatPanel := AValue;
end;

procedure TGameBoard.SetExperiment(AValue : TExperiment);
begin
  if FExperiment = AValue then Exit;
  FExperiment := AValue;
end;

procedure TGameBoard.SetGroupBoxAdmin(AValue : TGroupBox);
begin
  if FGroupBoxAdmin = AValue then Exit;
  FGroupBoxAdmin := AValue;
end;

procedure TGameBoard.SetGroupBoxExperiment(AValue : TGroupBoxExperiment);
begin
  if FGroupBoxExperiment = AValue then Exit;
  FGroupBoxExperiment := AValue;
end;

procedure TGameBoard.SetGroupBoxPlayers(AValue : TGroupBox);
begin
  if FGroupBoxPlayers = AValue then Exit;
  FGroupBoxPlayers := AValue;
end;

procedure TGameBoard.SetGroupBoxPoints(AValue : TGroupBox);
begin
  if FGroupBoxPoints = AValue then Exit;
  FGroupBoxPoints := AValue;
end;

procedure TGameBoard.SetImageGroup1(AValue : TImage);
begin
  if FImageGroup1 = AValue then Exit;
  FImageGroup1 := AValue;
end;

procedure TGameBoard.SetImageGroup2(AValue : TImage);
begin
  if FImageGroup2 = AValue then Exit;
  FImageGroup2 := AValue;
end;

procedure TGameBoard.SetImagePointA(AValue : TImage);
begin
  if FImagePointA = AValue then Exit;
  FImagePointA := AValue;
end;

procedure TGameBoard.SetImagePointB(AValue : TImage);
begin
  if FImagePointB = AValue then Exit;
  FImagePointB := AValue;
end;

procedure TGameBoard.SetImagePointI(AValue : TImage);
begin
  if FImagePointI = AValue then Exit;
  FImagePointI := AValue;
end;

procedure TGameBoard.SetLabelGroup1(AValue : TLabel);
begin
  if FLabelGroup1Count = AValue then Exit;
  FLabelGroup1Count := AValue;
end;

procedure TGameBoard.SetLabelGroup1Name(AValue : TLabel);
begin
  if FLabelGroup1Name = AValue then Exit;
  FLabelGroup1Name := AValue;
end;

procedure TGameBoard.SetLabelGroup2(AValue : TLabel);
begin
  if FLabelGroup2Count = AValue then Exit;
  FLabelGroup2Count := AValue;
end;

procedure TGameBoard.SetLabelGroup2Name(AValue : TLabel);
begin
  if FLabelGroup2Name = AValue then Exit;
  FLabelGroup2Name := AValue;
end;

procedure TGameBoard.SetLabelPointACount(AValue : TLabel);
begin
  if FLabelPointACount = AValue then Exit;
  FLabelPointACount := AValue;
end;

procedure TGameBoard.SetLabelPointAName(AValue : TLabel);
begin
  if FLabelPointAName = AValue then Exit;
  FLabelPointAName := AValue;
end;

procedure TGameBoard.SetLabelPointBCount(AValue : TLabel);
begin
  if FLabelPointBCount = AValue then Exit;
  FLabelPointBCount := AValue;
end;

procedure TGameBoard.SetLabelPointBName(AValue : TLabel);
begin
  if FLabelPointBName = AValue then Exit;
  FLabelPointBName := AValue;
end;

procedure TGameBoard.SetLabelPointICount(AValue : TLabel);
begin
  if FLabelPointICount = AValue then Exit;
  FLabelPointICount := AValue;
end;

procedure TGameBoard.SetLabelPointIName(AValue : TLabel);
begin
  if FLabelPointIName = AValue then Exit;
  FLabelPointIName := AValue;
end;

procedure TGameBoard.SetListBoxOldPlayers(AValue : TListBox);
begin
  if FListBoxOldPlayers = AValue then Exit;
  FListBoxOldPlayers := AValue;
end;

procedure TGameBoard.SetStringGridMatrix(AValue : TStringGrid);
begin
  if FStringGridMatrix = AValue then Exit;
  FStringGridMatrix := AValue;
end;

procedure TGameBoard.SetSystemPopUp(AValue : TPopupNotifier);
begin
  if FSystemPopUp = AValue then Exit;
  FSystemPopUp := AValue;
end;

procedure TGameBoard.SetTimer(AValue : TTimer);
begin
  if FTimer = AValue then Exit;
  FTimer := AValue;
end;

procedure TGameBoard.SetTimerEvent(AValue : TNotifyEvent);
begin
  if FTimerEvent = AValue then Exit;
  FTimerEvent := AValue;
end;

function TGameBoard.GetPlayerBox(AID: string): TPlayerBox;
var i : integer;
begin
  Result := nil;
  for i := 0 to GroupBoxPlayers.ComponentCount-1 do
    if TPlayerBox(GroupBoxPlayers.Components[i]).ID = AID then begin
      Result := TPlayerBox(GroupBoxPlayers.Components[i]);
      Break;
    end;
end;

function TGameBoard.GetPlayerNicname(AGameContext : TGameContext) : string;
var
  LCaption , LPrompt: String;
begin
  case AGameContext of
    gmcNewPlayerLogin : begin
      LCaption := CAP_PARTICIPANT_ARRIVED_CAPTION;
      LPrompt  := CAP_PARTICIPANT_ARRIVED_PROMPT;
    end;

    gmcNewPlayerArrived : begin
      LCaption := CAP_NEW_GENERATION_CAPTION;
      LPrompt  := CAP_NEW_GENERATION_PROMPT;
    end;

    else
      { do nothing };
  end;
  if Experiment.GenPlayersAsNeeded then
    Result := GenResourceName
  else
    Result := InputBox(LCaption, LPrompt, GenResourceName);
end;

{$IFNDEF TEST_MODE}
procedure TGameBoard.ShowPopupNotifierHack(AMessage : string;
  AAutoDestroyTime : integer);
var
  LPopUpHack : TPopupNotifierHack;
begin
  LPopUpHack := TPopupNotifierHack.Create(nil);
  if Assigned(BackgroundForm) then
    LPopUpHack.ShowAndAutoDestroy(AMessage,BackgroundForm,AAutoDestroyTime)
  else
    LPopUpHack.ShowAndAutoDestroy(AMessage,nil,AAutoDestroyTime);
end;
{$ENDIF}

procedure TGameBoard.CreatePlayerBox(P: TPlayer; Me: Boolean; Admin: Boolean);
var i1 : integer = 0;
begin
  with TPlayerBox.Create(GroupBoxPlayers,P.ID,Admin) do begin
    if Me then begin
      Caption := P.Nicname+SysToUtf8(' ('+ CAP_YOU +')' );
    end else begin
      Caption := P.Nicname;
    end;

    i1 := PtrInt(P.Choice.Row);
    if i1 > 0 then begin
      LabelLastRowCount.Caption := Format('%-*.*d', [1,2,i1])
    end else begin
      LabelLastRowCount.Caption := CAP_NA;
    end;

    PanelLastColor.Color := GetColorFromCode(P.Choice.Color);
    Enabled := True;
    Parent := GroupBoxPlayers;
  end;
end;

procedure TGameBoard.UpdatePlayerBox(P: TPlayer; Me: Boolean; Admin: Boolean;
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

  with LPlayerBox do begin
    if Me then begin
      Caption := P.Nicname+SysToUtf8(' ('+CAP_YOU+')' )
    end else begin
      Caption := P.Nicname;
    end;

    if Admin then begin
      LabelPointsRedCount.Caption := '0';
      LabelPointsBlueCount.Caption := '0';
    end else begin
      LabelLastRowCount.Caption := CAP_NA;
      PanelLastColor.Color := GetColorFromCode(P.Choice.Color);
    end;
  end;
end;

procedure TGameBoard.SetMatrixType(AStringGrid: TStringGrid;
  AMatrixType: TGameMatrixType);
begin
  if gmRows in AMatrixType then begin
    TStringGridA(AStringGrid).HasRows:=True;
  end;

  if gmColumns in AMatrixType then begin
    TStringGridA(AStringGrid).HasCols:=True;
  end;

  TStringGridA(AStringGrid).DrawFilledDots := gmDots in AMatrixType;
  TStringGridA(AStringGrid).DrawClearDots := gmClearDots in AMatrixType;
  TStringGridA(AStringGrid).UpdateSizeAndNames;
end;

// many thanks to howardpc for this:
// http://forum.lazarus.freepascal.org/index.php/topic,34559.msg227585.html#msg227585
function TGameBoard.AskQuestion(AQuestion: string): string;
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
            OKButton.Caption:=CAP_YES;
            CancelButton.Caption:=CAP_NO;
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

procedure TGameBoard.ShowSystemPopUp(AGameContext : TGameContext; AID : string);
var
  LMessage : string;
  LInterval : integer;
{$IFDEF TEST_MODE}
  LCaption : string;
{$ELSE}
  PopUpPos : TPoint;
  // temporary hack
  L : TLabel;
  r: TRect;
  w:integer;
{$ENDIF}
begin
  LInterval := GLOBAL_SYSTEM_MESSAGE_INTERVAL;
  case AGameContext of
    gmcNewPlayerLogin, gmcNewPlayerArrived: begin
      case AGameContext of
        gmcNewPlayerLogin : begin
          {$IFDEF TEST_MODE}
          LCaption := CAP_PARTICIPANT_ARRIVED_CAPTION;
          {$ENDIF}
          LMessage  := CAP_PARTICIPANT_ARRIVED_PROMPT;
        end;

        gmcNewPlayerArrived : begin
          {$IFDEF TEST_MODE}
          LCaption := CAP_NEW_GENERATION_CAPTION;
          {$ENDIF}
          LMessage  := CAP_NEW_GENERATION_PROMPT;
        end;
        else
          begin
            { do nothing }
          end;
      end;
    {$IFDEF TEST_MODE}
      DebugMessage(
        LCaption + LineEnding +
        LMessage + LineEnding +
        'Interval(ms): ' + LInterval.ToString
      );
    {$ENDIF}
      Exit;
    end;
    gmcNewPlayerLoginArrived : begin
      LMessage  := CAP_NEW_PARTICIPANT_LOGGED_IN;
    end;

    gmcNewPlayerLoggedIn : begin
      LMessage  := CAP_WELCOME + #32 + FExperiment.PlayerFromID[AID].Nicname;
    end;

    gmcExperimentStart : begin
      case FActor of
        gaPlayer : begin
          LMessage := CAP_EXPERIMENT_START_PLAYER;
        end;

        gaAdmin  : begin
          LMessage := CAP_EXPERIMENT_START_ADMIN;
        end;

        else
          { do nothing};
      end;
    end;

    gmcChoiceStart : begin
      LMessage := CAP_CHOICE_START;
    end;

    gmcPlayerExited : begin
      case FActor of
        gaPlayer : begin
          LMessage := FExperiment.PlayerFromID[AID].Nicname + #32 +
            CAP_PLAYER_EXITED_PLAYER;
        end;

        gaAdmin : begin
          LMessage := FExperiment.PlayerFromID[AID].Nicname + #32 +
            CAP_PLAYER_EXITED_ADMIN;
        end;

        else
          { do nothing};
      end;
    end;

    gmcPlayerExitedEnd : begin
      LMessage := FExperiment.PlayerFromID[AID].Nicname + #32 +
        CAP_PLAYER_EXITED_END;
    end;

    gmcExperimentEnd : begin
      LMessage := CAP_EXPERIMENT_END;
    end;

    gmcWaitingForServer : begin
      LMessage := CAP_WAITING_FOR_SERVER;
    end;
  end;
{$IFDEF TEST_MODE}
  DebugMessage(LMessage);
{$ELSE}
  SystemPopUp.vNotifierForm.AutoSize:=True;
  L := TLabel(SystemPopUp.vNotifierForm.FindComponent('UglyHack'));
  L.Caption := LMessage;
  SystemPopUp.Show;
  w := L.Width;
  SystemPopUp.Hide;
  SystemPopUp.vNotifierForm.AutoSize:=False;
  r:=SystemPopUp.vNotifierForm.CalcHintRect(w, LMessage, nil);
  SystemPopUp.vNotifierForm.HintRect:=r;
  SystemPopUp.vNotifierForm.Width:=r.Right-r.Left + 52;
  SystemPopUp.vNotifierForm.Height:=r.Bottom-r.Top + 52;

  // TODO: REFACTOR SHOW SYSTEM POPUP
  if Assigned(StringGridMatrix) then
    begin
      PopUpPos.X := (StringGridMatrix.Width div 2) - (SystemPopUp.vNotifierForm.Width div 2);
      PopUpPos.Y := (StringGridMatrix.Height div 2) - (SystemPopUp.vNotifierForm.Height div 2);
      PopUpPos := StringGridMatrix.ClientToScreen(PopUpPos);
      SystemPopUp.ShowAtPos(PopUpPos.X,PopUpPos.Y);
      Timer.OnTimer:=TimerEvent;
      Timer.Interval:=LInterval;
      Timer.Enabled:=True;
    end
  else
    SystemPopUp.ShowAtPos(0,0);
{$ENDIF}
end;

procedure TGameBoard.ShowSystemMessage(AGameContext : TGameContext; AID : string
  );
var
  LMessage : String;
begin
  case AGameContext of
    gmcPlayerExited : begin
      LMessage := FExperiment.PlayerFromID[AID].Nicname + #32 +
        CAP_PLAYER_EXITED_PLAYER;
    end;
  else
    { do nothing };
  end;
  if Assigned(BackgroundForm) then begin
    StringGridMatrix.Visible := False;
    GroupBoxPlayers.Visible := False;
    GroupBoxPoints.Visible := False;
    FSystemMessage.Caption := LMessage;
    FSystemMessage.Visible := True;
  end;
end;

procedure TGameBoard.HideSystemMessage;
begin
  if Assigned(BackgroundForm) then begin
    if FSystemMessage.Visible then begin
      FSystemMessage.Visible := False;
      FSystemMessage.Caption := '';
      StringGridMatrix.Visible := True;
      GroupBoxPlayers.Visible := True;
      GroupBoxPoints.Visible := True;
    end;
  end;
end;

function TGameBoard.GetPlayerExitMessage(AID : string) : string;
var
  Pts : String;
  //Name1 : string;
  //Name2 : string;
begin
  //Name1 := 'The Consumer Energy Alliance';
  //Name2 := 'Natural Resource Defense Council';
  //Pts := FExperiment.PlayerPointsSummationFromID(AID).ToString;
  //Result :=
  //  'The task is over, thank you for your participation!'+LineEnding+
  //  'You earned ' + Pts + ' tokens for yourself,' + LineEnding +
  //  FExperiment.GlobalPoints(gscG1, AID).ToString +
  //  ' tokens for '+Name1+' and' + LineEnding +
  //  FExperiment.GlobalPoints(gscG2, AID).ToString +
  //  ' tokens for '+Name2+'.'+ LineEnding +
  //  'So far the group has donated '+ LineEnding +
  //  FExperiment.GlobalPoints(gscG1).ToString +
  //  ' tokens for '+Name1+' and' + LineEnding +
  //  FExperiment.GlobalPoints(gscG2).ToString +
  //  ' tokens for '+Name2+'.';
  Pts := FExperiment.PlayerPointsSummationFromID(AID).ToString;
  Result :=
    'O estudo chegou ao fim, obrigado por sua participação!'+LineEnding+
    'Você ganhou ' + Pts + ' fichas para você mesmo,' + LineEnding +
    FExperiment.GlobalPoints(gscG1, AID).ToString +
    ' itens escolares e' + LineEnding +
    FExperiment.GlobalPoints(gscG2, AID).ToString +
    ' fichas coletivas.'+ LineEnding +
    'Ao todo, o grupo conseguiu o total de'+ LineEnding +
    FExperiment.GlobalPoints(gscG1).ToString +
    ' itens escolares e' + LineEnding +
    FExperiment.GlobalPoints(gscG2).ToString +
    ' fichas coletivas.';
end;

// TODO: REFACTOR SHOW CONSEQUENCE SHOW POPUP
procedure TGameBoard.ShowConsequence(ACallerID, AID : string;
  AConsequence : TConsequence; IsMeta : Boolean; ShowPopUp : Boolean);
var
  LStyle : TGameConsequenceStyle;
  procedure PresentMetaPoints;
  begin
    for LStyle in AConsequence.Style do
      case LStyle of
          gscG1 :
            FLabelGroup1Count.Caption :=
              FExperiment.GlobalPoints(LStyle).ToString;
          gscG2 :
            FLabelGroup2Count.Caption :=
              FExperiment.GlobalPoints(LStyle).ToString;
          else { do nothing };
      end;
  end;

  procedure PresentPointsPlayer;
  var
    PointA : string;
  begin
    for LStyle in AConsequence.Style do
      case LStyle of
          gscA : begin
            PointA := FExperiment.PlayerPointsFromID(ACallerID).A.ToString;
            if FExperiment.ABPoints then begin
              FLabelPointACount.Caption := PointA;
            end else begin
              FLabelPointICount.Caption := PointA;
            end;
          end;

          gscB :
            FLabelPointBCount.Caption :=
              FExperiment.PlayerPointsFromID(ACallerID).B.ToString;

          else { do nothing };
      end;
  end;

  procedure PresentPointsAdmin;
  var
    PB : TPlayerBox;
  begin
    for LStyle in AConsequence.Style do
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
  case FActor of
    gaPlayer:
      if IsMeta then
        PresentMetaPoints
      else
        if ACallerID = AID then
          PresentPointsPlayer;

    gaAdmin:
      PresentPointsAdmin;
    else { do nothing };
  end;

{$IFNDEF TEST_MODE}
  if ShowPopUp then begin
    if Assigned(GroupBoxPoints) then begin
      AConsequence.PresentMessage(GroupBoxPoints)
    end else begin
      if Assigned(BackgroundForm) then begin
        AConsequence.PresentMessage(BackgroundForm)
      end else begin
        Exception.Create('TGameBoard.ShowConsequence Exception');
      end;
    end;
  end;
{$ENDIF}
end;

procedure TGameBoard.NextConditionSetup; // [player_points]
begin

end;

procedure TGameBoard.InvalidateLabels(AID : string);
var
  P : TPlayer;
  PB : TPlayerBox;
begin
  case FActor of
    gaAdmin:begin
        for P in FExperiment.Players do
        begin
          PB := GetPlayerBox(P.ID);
          PB.LabelPointsRedCount.Caption :=
            FExperiment.PlayerFromID[P.ID].Points.A.ToString;
          PB.LabelPointsBlueCount.Caption :=
            FExperiment.PlayerFromID[P.ID].Points.B.ToString;
          if FExperiment.ABPoints then begin
            { do nothing }
          end else begin
            PB.LabelPointsBlueCount.Caption := CAP_NA;
          end;
        end;
    end;

    gaPlayer:begin
      P := FExperiment.PlayerFromID[AID];
      FLabelPointACount.Caption := P.Points.A.ToString;
      FLabelPointBCount.Caption := P.Points.B.ToString;
    end;
    else { do nothing };
  end;
end;

procedure TGameBoard.InvalidatePlayerBox(AID, ARow, AColor : string);
var
  PlayerBox : TPlayerBox;
begin
  PlayerBox := GetPlayerBox(AID);
  PlayerBox.LabelLastRowCount.Caption := ARow;
  PlayerBox.PanelLastColor.Color := GetColorFromString(AColor);
  PlayerBox.PanelLastColor.Caption:='';
  PlayerBox.Invalidate;
end;

procedure TGameBoard.StartExperiment(Sender : TObject);
begin
  if Assigned(BackgroundForm) then begin
    StringGridMatrix.Enabled := False;
    StringGridMatrix.Options := StringGridMatrix.Options-[goRowSelect];
    ButtonConfirm.Enabled:=True;
    ButtonConfirm.Caption:=CAP_CONFIRM;
    ButtonConfirm.Visible := False;
  end;
  inherited StartExperiment(Sender);
end;

procedure TGameBoard.StartCondition(Sender : TObject);
begin
  if Assigned(LabelGroup1Name) then begin
    LabelGroup1Name.Caption := Sanitize(FExperiment.CurrentCondition.Label1);
  end;

  if Assigned(LabelGroup2Name) then begin
    LabelGroup2Name.Caption := Sanitize(FExperiment.CurrentCondition.Label2);
  end;

  if Assigned(FLabelGroup1Count) then begin
    FLabelGroup1Count.Caption :=
      FExperiment.GlobalPoints(gscG1).ToString;
  end;

  if Assigned(FLabelGroup2Count) then begin
    FLabelGroup2Count.Caption :=
      FExperiment.GlobalPoints(gscG2).ToString;
  end;

  VisibleControl(FImagePointA, False);
  VisibleControl(FLabelPointAName, False);
  VisibleControl(FLabelPointACount, False);

  VisibleControl(FImagePointB, False);
  VisibleControl(FLabelPointBName, False);
  VisibleControl(FLabelPointBCount, False);
  FCause := FExperiment.CurrentCondition.TargetMetacontingency;
  if FActor = gaPlayer then begin
    case FCause of
      'A' : begin
        VisibleControl(FImageGroup1, False);
        VisibleControl(FLabelGroup1Name, False);
        VisibleControl(FLabelGroup1Count, False);

        VisibleControl(FImageGroup2, True);
        VisibleControl(FLabelGroup2Name, True);
        VisibleControl(FLabelGroup2Count, True);
      end;

      'B' : begin
        VisibleControl(FImageGroup2, False);
        VisibleControl(FLabelGroup2Name, False);
        VisibleControl(FLabelGroup2Count, False);

        VisibleControl(FImageGroup1, True);
        VisibleControl(FLabelGroup1Name, True);
        VisibleControl(FLabelGroup1Count, True);
      end;

      'C' : begin
        VisibleControl(FImageGroup1, True);
        VisibleControl(FLabelGroup1Name, True);
        VisibleControl(FLabelGroup1Count, True);

        VisibleControl(FImageGroup2, True);
        VisibleControl(FLabelGroup2Name, True);
        VisibleControl(FLabelGroup2Count, True);
      end;
    end;
  end;

  inherited StartCondition(Sender);
end;

constructor TGameBoard.Create(AOwner : TComponent; AGameEvents : TGameEvents;
  AActor : TGameActor; ABackgroundForm : TForm; AButtonConfirm : TButton;
  AGroupBoxPlayers : TGroupBox; AGroupBoxPoints : TGroupBox;
  AGroupBoxAdmin : TGroupBox; ASystemPopUp : TPopupNotifier);
var
  L : TLabel;
  i : integer;
begin
  inherited Create(AOwner);
  FActor := AActor;
  BackgroundForm := ABackgroundForm;
  ButtonConfirm := AButtonConfirm;
  GroupBoxAdmin := AGroupBoxAdmin;
  GroupBoxPlayers := AGroupBoxPlayers;
  GroupBoxPoints  := AGroupBoxPoints;
  SystemPopUp:= ASystemPopUp;

  case FActor of
    gaAdmin:
      begin
        GroupBoxExperiment := TGroupBoxExperiment.Create(Self);
      {$IFNDEF TEST_MODE}
        GroupBoxExperiment.Parent := GroupBoxAdmin;
      {$ENDIF}
      end
    else { do nothing };
  end;

  if Assigned(BackgroundForm) then begin
    StringGridMatrix := TStringGridA.Create(Self);
    TStringGridA(StringGridMatrix).ConfirmationButton := ButtonConfirm;
    StringGridMatrix.Parent := BackgroundForm;

    i := StringGridMatrix.Width+ButtonConfirm.Width+GroupBoxPoints.Width+10;
    i := i div 2;
    i := (Screen.Width div 2)-i;
    StringGridMatrix.Left:=i;

    i := StringGridMatrix.Height+GroupBoxPlayers.Height+10;
    i := i div 2;
    i := (Screen.Height div 2) - i;
    StringGridMatrix.Top:=i;

    ButtonConfirm.Hide;
    ButtonConfirm.AnchorSideLeft.Control := StringGridMatrix;
    ButtonConfirm.AnchorSideLeft.Side := asrRight;
    ButtonConfirm.BorderSpacing.Left := 5;
    ButtonConfirm.Top := StringGridMatrix.Top;
    ButtonConfirm.Left := StringGridMatrix.BoundsRect.Right+5;

    GroupBoxPoints.Left := ButtonConfirm.Left+ButtonConfirm.Width+10;
    GroupBoxAdmin.Left := GroupBoxPoints.Left+GroupBoxPoints.Width+5;

    SystemPopUp.Icon.Assign(Application.Icon);
    SystemPopUp.Title:='';
    SystemPopUp.Text:='';
    // SystemPopUp.vNotifierForm.Font.Size:=12;
    L := TLabel.Create(SystemPopUp.vNotifierForm);
    L.Name:='UglyHack';
    L.Align:=alClient;
    L.Anchors := [akLeft,akRight];
    L.Alignment := taCenter;
    L.Font.Color:=clBlack;
    L.AutoSize:=True;
    L.Layout := tlCenter;
    L.WordWrap := False;
    L.BorderSpacing.Top:=26;
    L.BorderSpacing.Left:=26;
    L.BorderSpacing.Right:=26;
    L.BorderSpacing.Bottom:=26;
    L.OnClick := SystemPopUp.vNotifierForm.OnClick;
    L.Parent := SystemPopUp.vNotifierForm;

    FSystemMessage := TLabel.Create(BackgroundForm);
    with FSystemMessage do begin
      Name := 'LabelGoodBye';
      Align:=alClient;
      Caption:= '';
      Alignment := taCenter;
      Anchors := [akLeft,akRight];
      Layout := tlCenter;
      WordWrap := True;
      Font.Size := 30;
      BringToFront;
      Visible := False;
      Parent:=BackgroundForm;
    end;
  end;

  if AGameEvents is TGameControl then
    with TGameControl(AGameEvents) do
    begin
      OnConsequence := @Self.Consequence;
      OnEndChoice := @Self.EndChoice;
      OnEndCondition := @Self.EndCondition;
      OnEndCycle := @Self.EndCycle;
      OnEndExperiment := @Self.EndExperiment;
      OnEndGeneration := @Self.EndGeneration;
      OnEndTurn := @Self.EndTurn;
      OnInterlocking := @Self.Interlocking;
      OnPlayerExit := @Self.PlayerExit;
      OnStartChoice := @Self.StartChoice;
      OnStartCondition := @Self.StartCondition;
      OnStartCycle := @Self.StartCycle;
      OnStartExperiment := @Self.StartExperiment;
      OnStartGeneration := @Self.StartGeneration;
      OnStartTurn := @Self.StartTurn;
      OnTargetInterlocking := @Self.TargetInterlocking;
      OnWaitForServer := @Self.WaitForServer;
      OnClose := @Self.Close;
    end;
end;

destructor TGameBoard.Destroy;
begin
  inherited Destroy;
end;

procedure TGameBoard.BeforeStartExperimentSetup;
begin
  case FActor of
    gaAdmin: begin
      if Assigned(BackgroundForm) then begin
        GroupBoxAdmin.Visible:= True;
        StringGridMatrix.Visible := False;
        GroupBoxAdmin.Left := 10;
      end;

      VisibleControl(FImagePointI, False);
      VisibleControl(FLabelPointIName, False);
      VisibleControl(FLabelPointICount, False);
      VisibleControl(FImagePointA, False);
      VisibleControl(FLabelPointAName, False);
      VisibleControl(FLabelPointACount, False);
      VisibleControl(FImagePointB, False);
      VisibleControl(FLabelPointBName, False);
      VisibleControl(FLabelPointBCount, False);
    end;
    gaPlayer: begin
      VisibleControl(FImagePointI, False);
      VisibleControl(FLabelPointIName, False);
      VisibleControl(FLabelPointICount, False);
      VisibleControl(FImageGroup1, False);
      VisibleControl(FLabelGroup1Name, False);
      VisibleControl(FLabelGroup1Count, False);
      VisibleControl(FImageGroup2, False);
      VisibleControl(FLabelGroup2Name, False);
      VisibleControl(FLabelGroup2Count, False);
    end;
    else { do nothing }
  end;
end;

procedure TGameBoard.VisibleEarningsBeforeGenerationChange(AValue : Boolean);
begin
  if AValue then begin
    case FCause of
      'C' : begin
        VisibleControl(FImageGroup1, AValue);
        VisibleControl(FLabelGroup1Name, AValue);
        VisibleControl(FLabelGroup1Count, AValue);
        VisibleControl(FImageGroup2, AValue);
        VisibleControl(FLabelGroup2Name, AValue);
        VisibleControl(FLabelGroup2Count, AValue);
      end;

      'B' : begin
        VisibleControl(FImageGroup1, AValue);
        VisibleControl(FLabelGroup1Name, AValue);
        VisibleControl(FLabelGroup1Count, AValue);
      end;

      'A' : begin
        VisibleControl(FImageGroup2, AValue);
        VisibleControl(FLabelGroup2Name, AValue);
        VisibleControl(FLabelGroup2Count, AValue);
      end;
    end;
  end else begin
    VisibleControl(FImageGroup1, AValue);
    VisibleControl(FLabelGroup1Name, AValue);
    VisibleControl(FLabelGroup1Count, AValue);

    VisibleControl(FImageGroup2, AValue);
    VisibleControl(FLabelGroup2Name, AValue);
    VisibleControl(FLabelGroup2Count, AValue);
  end;

  VisibleControl(FImagePointA, AValue);
  VisibleControl(FLabelPointAName, AValue);
  VisibleControl(FLabelPointACount, AValue);

  VisibleControl(FImagePointB, AValue);
  VisibleControl(FLabelPointBName, AValue);
  VisibleControl(FLabelPointBCount, AValue);
end;

procedure TGameBoard.StartSetup;
begin
  SetMatrix;
  SetLabels;
  if Assigned(ChatPanel) then
    if FExperiment.ShowChat then
      ChatPanel.Visible := FExperiment.ResearcherCanChat
    else
      ChatPanel.Visible := FExperiment.ShowChat;
end;

procedure TGameBoard.SetupChat(ASetup : string);
begin
  if Assigned(ChatPanel) then
    if ASetup = '[NOCHAT]' then
      ChatPanel.Visible := False
    else begin
      ChatPanel.Visible := True;
      Chat.Clear;
      Chat.Append(ASetup);
    end;
end;

procedure TGameBoard.FullScreen;
begin
  if Assigned(BackgroundForm) then
    with BackgroundForm do begin
      SetFullscreen(BackgroundForm);
    end;
end;

procedure TGameBoard.SetMatrix;
begin
  if Assigned(StringGridMatrix) then
    SetMatrixType(StringGridMatrix, FExperiment.MatrixType);
end;

procedure TGameBoard.SetLabels;
var
  IsPlayer : Boolean;
  LVisible : Boolean;
begin
  IsPlayer := FActor = gaPlayer;

  LVisible := FExperiment.ABPoints and IsPlayer;
  VisibleControl(FImagePointA, LVisible);
  VisibleControl(FLabelPointAName, LVisible);
  VisibleControl(FLabelPointACount, LVisible);
  VisibleControl(FImagePointA, LVisible);
  VisibleControl(FLabelPointBName, LVisible);
  VisibleControl(FLabelPointBCount, LVisible);

  LVisible := (not FExperiment.ABPoints) and IsPlayer;
  VisibleControl(FImagePointI, LVisible);
  VisibleControl(FLabelPointINAme, LVisible);
  VisibleControl(FLabelPointICount, LVisible);
end;

function TGameBoard.PlayerSaidGoodBye(AID, AStyle : string) : Boolean;
var
  LActorForm : TFormChooseActor;
  LMessage : string;
begin
  Result := False;
  LMessage := GetPlayerExitMessage(AID);

  if Assigned(BackgroundForm) then
    BackgroundForm.Visible := False;
  LActorForm := TFormChooseActor.Create(nil);
  try
    LActorForm.Style := AStyle;
    LActorForm.ShowPoints(LMessage);
    if LActorForm.ShowModal = 1 then begin
      if Assigned(BackgroundForm) then begin
        BackgroundForm.Visible := True;
      end;
      Result := True;
    end;
  finally
    FreeAndNil(LActorForm);
  end;
end;

function TGameBoard.SayGoodByeAndShowSlides(AID: string;
  AGoodByeSlides : TStringArray) : Boolean;
var
  P : TPlayer;
  LMessage : string;
  i : integer;
  function ReplacePlaceholders(P: TPlayer; S : string) : string;
  var
    LS : string;
  begin
    Result := S;
    if (Pos('$NICNAME', S) > 0) and (P.Nicname <> '') then begin
      Result := ReplaceStr(S,'$NICNAME', P.Nicname);
    end;

    if (Pos('$PLAYER_POINTS_SUM', S) > 0) then begin
      LS := (P.Points.A+P.Points.B).ToString;
      Result := ReplaceStr(Result, '$PLAYER_POINTS_SUM', LS);
    end;

    if (Pos('$PLAYER_MONEY_SUM', S) > 0) then begin
      LS := PointsToMoney((P.Points.A+P.Points.B), 10);
      Result := ReplaceStr(Result, '$PLAYER_MONEY_SUM', LS);
    end;

    if (Pos('$GROUP_POINTS_1', S) > 0) then begin
      LS := (FExperiment.GlobalPoints(gscG1).ToString);
      Result := ReplaceStr(Result, '$GROUP_POINTS_1', LS);
    end;

    if (Pos('$GROUP_POINTS_2', S) > 0) then begin
      LS := (FExperiment.GlobalPoints(gscG2).ToString);
      Result := ReplaceStr(Result, '$GROUP_POINTS_2', LS);
    end;

    if (Pos('$PLAYER_GROUP_POINTS_1', S) > 0) then begin
      LS := (FExperiment.GlobalPoints(gscG1, P.ID).ToString);
      Result := ReplaceStr(Result, '$PLAYER_GROUP_POINTS_1', LS);
    end;

    if (Pos('$PLAYER_GROUP_POINTS_2', S) > 0) then begin
      LS := (FExperiment.GlobalPoints(gscG2, P.ID).ToString);
      Result := ReplaceStr(Result, '$PLAYER_GROUP_POINTS_2', LS);
    end;
  end;
begin
  VisibleEarningsBeforeGenerationChange(False);

  P := FExperiment.PlayerFromID[AID];
  for i := Low(AGoodByeSlides) to High(AGoodByeSlides) do begin
    AGoodByeSlides[i] := ReplacePlaceholders(P, AGoodByeSlides[i]);
  end;

  Result := False;
  if Assigned(BackgroundForm) then
    BackgroundForm.Visible := False;

  if ShowSlides(AGoodByeSlides) then begin
    Result := True;
  end;
end;

function TGameBoard.ShowSlidesAndSayHello(AID : string;
  AWelcomeSlides : TStringArray) : Boolean;
begin
  Result := False;
  if Assigned(BackgroundForm) then
    BackgroundForm.Visible := False;

  if ShowSlides(AWelcomeSlides) then begin
    InvalidateLabels(AID);
    VisibleEarningsBeforeGenerationChange(True);
    Result := True;
  end;
end;

procedure TGameBoard.AppendToChat(ALn : string);
begin
  if Assigned(Chat) then
    Chat.Append(ALn);
end;

function TGameBoard.ShowSlides(ASlides : TStringArray) : Boolean;
var
  S : string;
  LSlides : TFormSlide;
begin
  Result := False;
  if Assigned(BackgroundForm) then begin
    BackgroundForm.Visible := False;
  end;
  LSlides := TFormSlide.Create(nil);
  LSlides.FullScreen;
  for S in ASlides do
    LSlides.Append(S);
  //LSlides.Append('<h1>Press close and wait for your turn.</h1>');
  try
    if LSlides.ShowModal = 1 then begin
      if Assigned(BackgroundForm) then begin
        BackgroundForm.Visible := True;
      end;
      Result := True;
    end;
  finally
    FreeAndNil(LSlides);
  end;
end;


end.

