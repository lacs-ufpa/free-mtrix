unit game_actors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,PopupNotifier
  , game_actors_point
  ;
type

  TGameActor = ( gaNone, gaAdmin, gaPlayer, gaWatcher );
  TGamePlayerStatus = (gpsWaiting, gpsPlaying, gpsPlayed);

  TGameMatrix = (gmRows,gmColumns, gmDots,gmClearDots);
  TGameMatrixType = set of TGameMatrix;

  TGameRow = (grNone,
              grOne,grTwo,grThree,grFour,grFive,grSix,grSeven,grEight,grNine,grTen,  // 10 rows
              grEven,grOdd,
              grDiff,grEqual,grAll,grNot,grSome); //meta only

  TGameRows = set of TGameRow;

  TGameColor = (gcNone,
                gcYellow,gcRed,gcGreen,gcBlue,gcMagenta, // 5 colors
                gcDiff,gcEqual,gcAll,gcNot,gcSome); //meta only

  TGameColors = set of TGameColor;

  TGameEndCondition = (gecInterlockingPorcentage,gecAbsoluteCycles,gecWhichComeFirst);
  //TGameOperator = (goNONE, goAND, goOR);
  TGameStyle = (gtNone, gtRowsOnly, gtColorsOnly, gtRowsAndColors, gtRowsOrColors);

  TGameConsequenceStyle = (gscNone, gscMessage, gscBroadcastMessage, gscPoints, gscVariablePoints, gscA, gscB,gscG);
  TConsequenceStyle = set of TGameConsequenceStyle;

  TGamePromptStyle = (gsYes, gsNo, gsAll, gsMetacontingency, gsContingency, gsBasA, gsRevertPoints);

  TPromptStyle = set of TGamePromptStyle;


type

  TPLayerPoints = record
    A, B : integer
  end;

  TPlayerChoice =  record
    Row : TGameRow;
    Color : TGameColor;
  end;

  TPlayer = record
    ID,
    Nicname,
    Login,
    Password : UTF8string;
    Status : TGamePlayerStatus;
    Data : TStringList;
    Choice : TPlayerChoice;
    Points : TPLayerPoints;
    Turn : ShortInt;
  end;

  TPlayers = array of TPlayer;

   { TCriteria }

   TCriteria = record
     Style : TGameStyle;
     Rows : TGameRows;
     Colors : TGameColors;
   end;

   { TConsequence }

   TConsequence = class(TComponent)
   private
     FAppendicePlural: UTF8String;
     FAppendiceSingular: UTF8String;
     FNicname: UTF8String;
   protected
     FStyle : TConsequenceStyle;
     FP : TGamePoint;
     FMessage : TPopupNotifier;
     procedure StopTimer(Sender:TObject;var ACloseAction:TCloseAction);
     procedure TimerTimer(Sender:TOBject);virtual;
   public
     constructor Create(AOwner:TComponent; AP:TGamePoint; AStyle:TConsequenceStyle; AAppendiceSingular,AAppendicePlural:UTF8String);overload;
     constructor Create(AOwner:TComponent; AP:integer; AStyle: TConsequenceStyle; AMessage:array of UTF8string);overload;
     constructor Create(AOwner:TComponent; AConsequenceString: UTF8String);overload;
     destructor Destroy;override;
     function AsString: utf8string;
     procedure Present(Sender:TObject;ForGroup:Boolean);virtual;
     property PlayerNicname : UTF8String read FNicname write FNicname;
     property AppendiceSingular : UTF8String read FAppendiceSingular;
     property AppendicePlural : UTF8String read FAppendicePlural;
   end;

  { TContingency }

  TContingency = class(TComponent)
  private
    FFired,
    FMeta : Boolean; //True: Consequence occurs OnEndTurn, False: Consequence occurs OnEndCycle
    FConsequence : TConsequence;
    FCriteria : TCriteria;
    FOnCriteria: TNotifyEvent;
    function RowMod(R:TGameRow):TGameRow;
    procedure CriteriaEvent;
  public
    constructor Create(AOwner:TComponent;AConsequence:TConsequence;ACriteria:TCriteria;IsMeta:Boolean);overload;
    function CriteriaString : UTF8String;
    function ResponseMeetsCriteriaI(R : TGameRow; C : TGameColor):Boolean; // Does response meets operant criteria?
    function ResponseMeetsCriteriaG(Players : TPlayers):Boolean;
    property OnCriteria : TNotifyEvent read FOnCriteria write FOncriteria;
    property Fired : Boolean read FFired;
    property Consequence : TConsequence read FConsequence;
    property Criteria : TCriteria read FCriteria;
    property Meta : Boolean read FMeta;
  end;

  { TContingencies }

  TContingencies = array of TContingency;

  { TPrompt }

  TPrompt = class(TConsequence)
  private
    FPromptTargets : TContingencies; // need to test this
  public
    PromptStyle : TPromptStyle;
    PromptMessage : string;
  public
    procedure Present(Sender:TObject;ForGroup:Boolean);override;
    property APromptTargets: TContingencies read FPromptTargets;
  end;

  TEndConditionCriterium = record
    Value : TGameEndCondition;
    InterlockingPorcentage,
    LastCycles,
    AbsoluteCycles: integer;
  end;

  TPoints = record
    A, B, G: integer;
  end;

  TCondition = record
    ConditionName : string;
    Contingencies : TContingencies; // for producing points during the condition

    Points : record
      Count : TPoints; // sum of points produced during the condition
      OnStart : TPoints; // points to add at the beginning of the condition
    end;

    Turn : record // for changing cycles
      Count,  // current turn
      Value : integer; // PlayersPerCycle, TurnsPerCycle
      Random: Boolean; // if we should change Players[i].Turn OnCycle
    end;

    Cycles : record // for changing generations
      Count, // current cycle
      Value, // CyclesPerLineage, CyclesPerGeneration
      Generation : integer;
    end;
    Prompt : TPrompt; // onEndCycle
    EndCriterium : TEndConditionCriterium; // to change from one condition to another
  end;

implementation

uses ButtonPanel,Controls,ExtCtrls,strutils, string_methods,
  form_matrixgame{,StdCtrls};

{ TContingency }

function TContingency.RowMod(R: TGameRow): TGameRow;
var
  LEvenSet : TGameRows;
  LOddSet : TGameRows;
begin
  Result := grNone;
  LEvenSet := [grTwo, grFour, grSix, grEight, grTen];
  LOddSet  := [grOne, grThree, grFive, grSeven, grNine];
  if R in LEvenSet then
    Result := grEven;

  if R in LOddSet then
    Result := grOdd;
end;

procedure TContingency.CriteriaEvent;
begin
  // FConsequence.Present(FMeta);
  // do admin internals
end;

constructor TContingency.Create(AOwner:TComponent;AConsequence:TConsequence;ACriteria:TCriteria;IsMeta:Boolean);
begin
  inherited Create(AOwner);
  FConsequence :=  AConsequence;
  FCriteria := ACriteria;
  FMeta := IsMeta;
  FFired := False;
end;

function TContingency.CriteriaString: UTF8String;
var R : TGameRow;
    C : TGameColor;
begin
  Result := '';
  for R in FCriteria.Rows do
    Result += GetRowString(R) + ',';
  Result += '|';

  case FCriteria.Style of
    gtNone : Result += 'INDIFERENTE';
    gtRowsAndColors : Result  += 'E';
    gtRowsOrColors : Result  += 'OU';
    gtRowsOnly: Result += 'LINHAS';
    gtColorsOnly:Result += 'CORES';
  end;
  Result += ',';
  Result += '|';

  for C in FCriteria.Colors do
    Result += GetColorString(C) + ',';

  Result += '|';
end;

function TContingency.ResponseMeetsCriteriaI(R : TGameRow; C : TGameColor): Boolean;
var
  LMod : TGameRow;
  LRow, LColor:Boolean;
begin
  Result := False;

  LMod := RowMod(R);
  LColor := C in Criteria.Colors;
  LRow := (R in Criteria.Rows) or (LMod in Criteria.Rows);

  case Criteria.Style of
    gtNone: Exit;
    gtColorsOnly: Result := LColor;
    gtRowsOnly: Result := LRow;
    gtRowsAndColors: Result := LColor and LRow;
    gtRowsOrColors: Result := LRow or LColor;
  end;
  if Result then
    if Assigned(FOnCriteria) then FOnCriteria(Self);
end;

function TContingency.ResponseMeetsCriteriaG(Players: TPlayers): Boolean;
var i : integer;
    Cs : array of TGameColor;
    Rs : array of TGameRow;
    //C : TGameColor;
    R : TGameRow;
    Len : Byte;

    function AllColorsEqual:Boolean;
    var i : integer;
    begin
      Result := True;
      for i := 0 to Len-2 do
        if Cs[i] <> Cs[i+1] then
          begin
           Result := False;
           Break;
          end;
    end;

    function AllColorsDiff:Boolean;
    var i : integer;
    begin
      Result := True;
      for i := 0 to Len-2 do
        if Cs[i] = Cs[i+1] then
          begin
           Result := False;
           Break;
          end;
    end;

    function AllRowsOdd: Boolean;
    begin
      for R in Rs do
        if RowMod(R) = grEven then
          begin
            Result := False;
            Exit;
          end;
    end;

    function AllRowsEven: Boolean;
    begin
      for R in Rs do
        if RowMod(R) = grOdd then
          begin
            Result := False;
            Exit;
          end;
    end;

begin // grDiff,grEqual,grAll
  Result := False;
  Len := Length(Players);
  SetLength(Cs,Len);
  SetLength(Rs,Len);

  for i :=0 to Length(Players)-1 do
    Cs[i] := Players[i].Choice.Color;

  for i :=0 to Length(Players)-1 do
    Rs[i] := Players[i].Choice.Row;

  case Criteria.Style of
    gtNone: Exit;
    gtColorsOnly:
      begin
        if gcDiff in Criteria.Colors then
          Result := AllColorsDiff;

        if gcEqual in Criteria.Colors then
          Result := AllColorsEqual;
      end;

    gtRowsOnly:
      begin
        if grOdd in Criteria.Rows then
          Result := AllRowsOdd;

        if grEven in Criteria.Rows then
          Result := AllRowsEven;
      end;

    gtRowsAndColors:
      begin
        if (gcDiff in Criteria.Colors) and (grOdd in Criteria.Rows) then
          Result := AllColorsDiff and AllRowsOdd;

        if (gcDiff in Criteria.Colors) and (grEven in Criteria.Rows) then
           Result := AllColorsDiff and AllRowsEven;

        if (gcEqual in Criteria.Colors) and (grOdd in Criteria.Rows) then
           Result := AllColorsEqual and AllRowsOdd;

        if (gcEqual in Criteria.Colors) and (grEven in Criteria.Rows) then
           Result := AllColorsEqual and AllRowsEven;
      end;
    gtRowsOrColors:
      begin
        if (gcDiff in Criteria.Colors) and (grOdd in Criteria.Rows) then
          Result := AllColorsDiff or AllRowsOdd;

        if (gcDiff in Criteria.Colors) and (grEven in Criteria.Rows) then
           Result := AllColorsDiff or AllRowsEven;

        if (gcEqual in Criteria.Colors) and (grOdd in Criteria.Rows) then
           Result := AllColorsEqual or AllRowsOdd;

        if (gcEqual in Criteria.Colors) and (grEven in Criteria.Rows) then
           Result := AllColorsEqual or AllRowsEven;
      end;
  end;
  if Result then
    if Assigned(FOnCriteria) then FOnCriteria(Self);
end;


{ TPrompt }

procedure TPrompt.Present(Sender: TObject; ForGroup: Boolean);

  function AskQuestion: boolean;
  var
    dlg: TForm;
    buttonPanel: TButtonPanel;
    mainPanel: TPanel;
    mr: TModalResult;
  begin
    dlg:=TForm.CreateNew(nil);
    try
      with dlg do begin
        BorderStyle:=bsNone;
        WindowState:=wsFullScreen;
        //Position:=poScreenCenter;
        Caption:='Task ' + IntToStr(0 {Succ(0)});
        buttonPanel:=TButtonPanel.Create(dlg);
        with buttonPanel do begin
          ShowButtons:=[pbCancel, pbOK];
          ShowBevel:=False;
          Parent:=dlg;
        end;
        mainPanel:=TPanel.Create(dlg);
        with mainPanel do begin
          Align:=alClient;
          Caption:=Format('Task %d - GUI buttons/edits etc. go here',[0]);
          Parent:=dlg;
        end;

        mr:=ShowModal;
        Result:=(mr = mrOK);
      end;
    finally
      dlg.Free;
    end;
  end;
begin
  inherited Present(Sender, ForGroup);
  //SendMessage(AskQuestion);
end;

{ TConsequence }

constructor TConsequence.Create(AOwner: TComponent; AP: TGamePoint;
  AStyle: TConsequenceStyle; AAppendiceSingular, AAppendicePlural: UTF8String);
begin
  inherited Create(AOwner);
  FStyle:=AStyle;
  FNicname:='';
  FAppendiceSingular:=AAppendiceSingular;
  FAppendicePlural:=AAppendicePlural;
  FP := AP;
  FMessage := TPopupNotifier.Create(AOwner);
end;

constructor TConsequence.Create(AOwner: TComponent; AP: integer;
  AStyle:TConsequenceStyle; AMessage: array of UTF8string);
begin
  inherited Create(AOwner);
  FStyle:=AStyle;
  FNicname:=AMessage[0];
  FAppendiceSingular:=AMessage[1];
  FAppendicePlural:=AMessage[2];
  FP := TGamePoint.Create(AOwner,AP);
  FMessage := TPopupNotifier.Create(AOwner);
end;

constructor TConsequence.Create(AOwner: TComponent;
  AConsequenceString: UTF8String);

  function GetConsequenceStyleFromString(S:UTF8String):TConsequenceStyle;
  var
    LCount,
    i : integer;
  begin
    Result := [];
    LCount := WordCount(S,[#0,',']);
    for i:= 1 to LCount do
      case ExtractDelimited(i,S,[',']) of
        '0':Result+=[gscNone];
        'M':Result+=[gscMessage];
        'C':Result+=[gscBroadcastMessage];
        'P':Result+=[gscPoints];
        'V':Result+=[gscVariablePoints];
        'A':Result+=[gscA];
        'B':Result+=[gscB];
      end;
  end;

begin
  inherited Create(AOwner);
  FP := TGamePoint.Create(AOwner,ExtractDelimited(1,AConsequenceString,['|']));
  FStyle:=GetConsequenceStyleFromString(ExtractDelimited(2,AConsequenceString,['|']));
  FNicname:=ExtractDelimited(3,AConsequenceString,['|']);
  FAppendiceSingular:=ExtractDelimited(4,AConsequenceString,['|']);
  FAppendicePlural:=ExtractDelimited(5,AConsequenceString,['|']);
  FMessage := TPopupNotifier.Create(AOwner);
end;

destructor TConsequence.Destroy;
begin
  inherited Destroy;
end;

function TConsequence.AsString: utf8string;
  function GetConsequenceStyleString(CS:TConsequenceStyle): UTF8String;
  var ConsequenceStyle : TGameConsequenceStyle;
  begin
    Result := '';
    for ConsequenceStyle in CS do
      begin
        case ConsequenceStyle of
          gscNone: Result += '0';
          gscMessage:Result += 'M';
          gscBroadcastMessage:Result += 'C';
          gscPoints:Result += 'P';
          gscVariablePoints:Result += 'V';
          gscA:Result += 'A';
          gscB:Result += 'B';
        end;
        Result += ',';
      end;
  end;

begin
  Result := IntToStr(FP.Value)+','+IntToStr(FP.Variation) + '|';
  Result += GetConsequenceStyleString(FStyle)+'|';
  Result += FNicname +'|';
  Result += FAppendiceSingular + '|';
  Result += FAppendicePlural + '|';
end;


procedure TConsequence.Present(Sender: TObject; ForGroup: Boolean);
var
  PopUpPos : TPoint;
begin
  PopUpPos.X := FormMatrixGame.StringGridMatrix.Left+FormMatrixGame.StringGridMatrix.Width;
  PopUpPos.Y := FormMatrixGame.StringGridMatrix.Top;
  PopUpPos := FormMatrixGame.StringGridMatrix.ClientToScreen(PopUpPos);

  FMessage.Text := FP.PointMessage(FNicname,FAppendicePlural, FAppendiceSingular,ForGroup);
  FMessage.OnClose:=@StopTimer;
  FormMatrixGame.Timer.OnTimer := @TimerTimer;

  if gscA in FStyle then
    FormMatrixGame.LabelIndACount.Caption := IntToStr(StrToInt(FormMatrixGame.LabelIndACount.Caption) + FP.ResultAsInteger);

  if gscB in FStyle then
    FormMatrixGame.LabelIndBCount.Caption := IntToStr(StrToInt(FormMatrixGame.LabelIndBCount.Caption) + FP.ResultAsInteger);

  if gscG in FStyle then
    FormMatrixGame.LabelGroupCount.Caption:= IntToStr(StrToInt(FormMatrixGame.LabelGroupCount.Caption) + FP.ResultAsInteger);

  FMessage.ShowAtPos(PopUpPos.X, PopUpPos.Y);
  FormMatrixGame.Timer.Enabled:=True;
end;

procedure TConsequence.StopTimer(Sender: TObject; var ACloseAction: TCloseAction
  );
begin
  FormMatrixGame.Timer.Enabled:=False;
  Free;
end;

procedure TConsequence.TimerTimer(Sender: TOBject);
begin
  FMessage.Visible:=False;
end;


end.

