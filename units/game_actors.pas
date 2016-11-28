{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_actors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,PopupNotifier, ExtCtrls
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
    Password : string;
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
     FAppendicePlural: string;
     FAppendiceSingular: string;
     FNicname: string;
     FStyle : TConsequenceStyle;
     FP : TGamePoint;
     FTimer : TTimer;
     FMessage : TPopupNotifier;
     function GetShouldPublishMessage: Boolean;
   protected
     FConsequenceByPlayerID : TStringList;
     procedure StopTimer(Sender:TObject;var ACloseAction:TCloseAction);
     procedure SelfDestroy(Sender:TOBject);virtual;
   public
     constructor Create(AOwner:TComponent; AP:TGamePoint; AStyle:TConsequenceStyle; AAppendiceSingular,AAppendicePlural:string);overload;
     constructor Create(AOwner:TComponent; AP:integer; AStyle: TConsequenceStyle; AMessage:array of string);overload;
     constructor Create(AOwner:TComponent; AConsequenceString: string);virtual;overload;
     destructor Destroy;override;
     function AsString(AID :string): string;
     function GenerateMessage(ForGroup: Boolean):string;
     procedure PresentMessage;
     procedure PresentPoints;
     property ShouldPublishMessage : Boolean read GetShouldPublishMessage;
     property PlayerNicname : string read FNicname write FNicname;
     property AppendiceSingular : string read FAppendiceSingular;
     property AppendicePlural : string read FAppendicePlural;
     property ConsequenseByPlayerID : TStringList read FConsequenceByPlayerID;
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
    function CriteriaString : string;
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
    FResponses : array of string;
    FResult : string;
    FPromptTargets : TContingencies; // need to test this
    FPromptStyle : TPromptStyle;
    FPromptMessage : string;
    procedure ClearResponses;
  public
    constructor Create(AOwner:TComponent; APStyle:TPromptStyle; APTarget : TContingencies; AMessage:string);reintroduce;
    function ResponsesCount : integer;
    procedure AppendResponse(AID,R:string);
    function AsString: TStringList; overload;
    property Question: string read FPromptMessage;
    property PromptResult:string read FResult;

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
    Interlocks : record
      Count : integer; // culturant,
      History: array of Boolean; // to calculate interlock porcentage in the last cycles. sync with OnCycles
    end;

    Points : record
      Count : TPoints; // sum of points produced during the condition
      OnStart : TPoints; // points to add at the beginning of the condition
    end;

    Turn : record // for changing cycles
      Count,  // current turn
      Value : integer; // PlayersPerCycle, TurnsPerCycle
      Random: Boolean; // if we should change Players[i].Turn OnCycle
    end;

    Cycles : record // for changing generations //absolute value is (Value * Generation)+Count
      Count, // current cycle
      Value, // CyclesPerLineage, CyclesPerGeneration
      Generation : integer;
    end;
    Prompt : TPrompt; // onEndCycle
    EndCriterium : TEndConditionCriterium; // to change from one condition to another
  end;

implementation

uses Graphics, strutils, string_methods,
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
  FFired:=True;
  if Assigned(FOnCriteria) then FOnCriteria(Self);
end;

constructor TContingency.Create(AOwner:TComponent;AConsequence:TConsequence;ACriteria:TCriteria;IsMeta:Boolean);
begin
  inherited Create(AOwner);
  FConsequence :=  AConsequence;
  FCriteria := ACriteria;
  FMeta := IsMeta;
  FFired := False;
end;

function TContingency.CriteriaString: string;
var R : TGameRow;
    C : TGameColor;
begin
  Result := '';
  for R in FCriteria.Rows do
    Result += GetRowString(R) + ',';
  Result += '|';

  Result += GetCriteriaStyleString(FCriteria.Style);
  Result += ',';
  Result += '|';

  for C in FCriteria.Colors do
    Result += GetColorString(C) + ',';

  Result += '|';
end;

function TContingency.ResponseMeetsCriteriaI(R : TGameRow; C : TGameColor): Boolean;  // should be for admin only
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
    CriteriaEvent;
end;

function TContingency.ResponseMeetsCriteriaG(Players: TPlayers): Boolean; // must be for admin only
var i : integer;
    Cs : array of TGameColor;
    Rs : array of TGameRow;
    //C : TGameColor;
    R : TGameRow;
    Len : Byte;

    function AllColorsEqual:Boolean;
    var i : integer;
    begin
      Result := not (gcNot in Criteria.Colors);
      for i := 0 to Len-2 do
        if Cs[i] <> Cs[i+1] then
          begin
            Result := not Result;
            Break;
          end;
    end;

    function AllColorsDiff:Boolean;
    var i : integer;
    begin
      Result := not (gcNot in Criteria.Colors);
      for i := 0 to Len-2 do
        if Cs[i] = Cs[i+1] then
          begin
            Result := not Result;
            Break;
          end;
    end;

    function AllRowsOdd: Boolean;
    begin
      Result := not (grNot in Criteria.Rows);
      for R in Rs do
        if RowMod(R) = grEven then
          begin
            Result := not Result;
            Break;
          end;
    end;

    function AllRowsEven: Boolean;
    begin
      Result := not (grNot in Criteria.Rows);
      for R in Rs do
        if RowMod(R) = grOdd then
          begin
            Result := not Result;
            Break;
          end;
    end;

begin // All -> (Diff,Equal,Even, Odd) or not all
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
    CriteriaEvent;
end;


{ TPrompt }

procedure TPrompt.ClearResponses;
begin
  FResponses := nil;
end;

constructor TPrompt.Create(AOwner: TComponent; APStyle: TPromptStyle;
  APTarget: TContingencies; AMessage: string);
begin
  inherited Create(AOwner);
  FPromptStyle := APStyle;
  FPromptTargets := APTarget;
  FPromptMessage := AMessage;
end;

function TPrompt.ResponsesCount: integer;
begin
  Result := Length(FResponses);
end;

procedure TPrompt.AppendResponse(AID, R: string);
begin
  SetLength(FResponses,Length(FResponses)+1);
  FResponses[High(FResponses)] := AID+'|'+R+'|';
end;

function TPrompt.AsString: TStringList;
var
  j,i : integer;
  LID,LConsequence : string;
  LCsqStyle : TConsequenceStyle;
  Pts : integer;

  function AllPlayersClickedYes: Boolean;
  var i : integer;
  begin
    Result := True;
    for i := 0 to Length(FResponses)-1 do
      if ExtractDelimited(2,FResponses[i],['|']) = 'N' then
        begin
          Result := False;
        end;
  end;

  procedure ApplyPointsConditions(IsMeta:Boolean);
  var
    S : string;
  begin
    Pts := StrToInt(ExtractDelimited(1,LConsequence, ['|']));
    if gsRevertPoints in FPromptStyle then
      Pts := Pts*-1;

    if (gscB in LCsqStyle) and (gsBasA in FPromptStyle) then
      begin
        LCsqStyle += [gscB];
        LCsqStyle -= [gscA];
      end;

    if IsMeta then
      S := 'M'
    else
      S := LID;

    LConsequence := S + '+' +
      IntToStr(Pts) +'|'+
      GetConsequenceStylesString(LCsqStyle) +'|'+
      ExtractDelimited(3,LConsequence, ['|']) +'|'+
      ExtractDelimited(4,LConsequence, ['|']) +'|'+
      ExtractDelimited(5,LConsequence, ['|']);
  end;
begin
  // to do, sanitize FPromptStyle first
  Pts:= 0;
  if (gsAll in FPromptStyle) and (gsYes in FPromptStyle) then
    if AllPlayersClickedYes then
      for i := 0 to Length(FPromptTargets)-1 do
        for j := 0 to FPromptTargets[i].Consequence.ConsequenseByPlayerID.Count do
          begin
            LID := FPromptTargets[i].Consequence.ConsequenseByPlayerID.Names[j];
            LConsequence := FPromptTargets[i].Consequence.ConsequenseByPlayerID.Values[LID];
            LCsqStyle := GetConsequenceStylesFromString(ExtractDelimited(2,LConsequence, ['|']));

            if gsContingency in FPromptStyle then
              if (FPromptTargets[i].Fired) and (not FPromptTargets[i].Meta) then
                if (gscA in LCsqStyle) or (gscB in LCsqStyle) then
                  ApplyPointsConditions(False);


            if gsMetacontingency in FPromptStyle then
              if (FPromptTargets[i].Fired) and FPromptTargets[i].Meta then
                if gscG in LCsqStyle then
                  ApplyPointsConditions(True);

            Result := TStringList.Create;
            Result.Add(LConsequence);
          end;

end;

{ TConsequence }

constructor TConsequence.Create(AOwner: TComponent; AP: TGamePoint;
  AStyle: TConsequenceStyle; AAppendiceSingular, AAppendicePlural: string);
begin
  inherited Create(AOwner);
  FStyle:=AStyle;
  FNicname:='';
  FAppendiceSingular:=AAppendiceSingular;
  FAppendicePlural:=AAppendicePlural;
  FP := AP;
  FMessage := TPopupNotifier.Create(AOwner);
  FConsequenceByPlayerID := TStringList.Create;
end;

constructor TConsequence.Create(AOwner: TComponent; AP: integer;
  AStyle:TConsequenceStyle; AMessage: array of string);
begin
  inherited Create(AOwner);
  FP := TGamePoint.Create(AOwner,AP);
  FStyle:=AStyle;
  FNicname:=AMessage[0];
  FAppendiceSingular:=AMessage[1];
  FAppendicePlural:=AMessage[2];

  FMessage := TPopupNotifier.Create(AOwner);
  FConsequenceByPlayerID := TStringList.Create;
end;

constructor TConsequence.Create(AOwner: TComponent;
  AConsequenceString: string);
begin
  inherited Create(AOwner);
  FP := TGamePoint.Create(AOwner,ExtractDelimited(1,AConsequenceString,['|']));
  FStyle:=GetConsequenceStylesFromString(ExtractDelimited(2,AConsequenceString,['|']));
  FNicname:=ExtractDelimited(3,AConsequenceString,['|']);
  FAppendiceSingular:=ExtractDelimited(4,AConsequenceString,['|']);
  FAppendicePlural:=ExtractDelimited(5,AConsequenceString,['|']);

  FMessage := TPopupNotifier.Create(Self);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled:=False;
  FTimer.Interval:=6000;
  FTimer.OnTimer:=@SelfDestroy;
  FConsequenceByPlayerID := TStringList.Create;
end;

destructor TConsequence.Destroy;
begin
  FConsequenceByPlayerID.Free;
  inherited Destroy;
end;

function TConsequence.AsString(AID: string): string;
begin
  Result := IntToStr(FP.ValueWithVariation) + '|';
  Result += GetConsequenceStylesString(FStyle)+'|';
  Result += FNicname +'|';
  Result += FAppendiceSingular + '|';
  Result += FAppendicePlural + '|+';
  FConsequenceByPlayerID.Values[AID]:=Result;
end;

function TConsequence.GenerateMessage(ForGroup: Boolean): string;
begin
  Result := FP.PointMessage(FNicname,FAppendicePlural, FAppendiceSingular,ForGroup);
  FMessage.Text := Result;
end;

procedure TConsequence.PresentMessage;
var
  PopUpPos : TPoint;
begin
  if gscA in FStyle then
    begin
      PopUpPos.X := FormMatrixGame.GBIndividualAB.Left-110;
      PopUpPos.Y := FormMatrixGame.GBIndividualAB.Top+FormMatrixGame.GBIndividual.Height-10;
    end;

  if gscB in FStyle then
    begin
      PopUpPos.X := FormMatrixGame.GBIndividualAB.Left+110;
      PopUpPos.Y := FormMatrixGame.GBIndividualAB.Top+FormMatrixGame.GBIndividual.Height-10;
    end;

  if gscG in FStyle then
    begin
      PopUpPos.X := FormMatrixGame.GBIndividualAB.Left-110;
      PopUpPos.Y := FormMatrixGame.GBIndividualAB.Top+FormMatrixGame.GBIndividual.Height+100;
    end;
  PopUpPos := FormMatrixGame.ClientToScreen(PopUpPos);
  FMessage.Color:=clTeal;
  FMessage.Title:='';
  FMessage.ShowAtPos(PopUpPos.X, PopUpPos.Y);
  FTimer.Enabled:=True;
end;

procedure TConsequence.PresentPoints;
begin
  //is gscPoints in FStyle then just in case...
  if gscA in FStyle then
    FormMatrixGame.LabelIndACount.Caption := IntToStr(StrToInt(FormMatrixGame.LabelIndACount.Caption) + FP.ResultAsInteger);

  if gscB in FStyle then
    FormMatrixGame.LabelIndBCount.Caption := IntToStr(StrToInt(FormMatrixGame.LabelIndBCount.Caption) + FP.ResultAsInteger);

  if gscG in FStyle then
    FormMatrixGame.LabelGroupCount.Caption:= IntToStr(StrToInt(FormMatrixGame.LabelGroupCount.Caption) + FP.ResultAsInteger);
end;

function TConsequence.GetShouldPublishMessage: Boolean; // for players only
begin
  Result := gscBroadcastMessage in FStyle;
end;

procedure TConsequence.StopTimer(Sender: TObject; var ACloseAction: TCloseAction
  );
begin
  //FormMatrixGame.Timer.Enabled:=False;
end;

procedure TConsequence.SelfDestroy(Sender: TOBject);
begin
  FMessage.Visible:=False;
  Free;
end;


end.

