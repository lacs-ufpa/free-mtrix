{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_actors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Forms, PopupNotifier
  , game_actors_point
  , game_visual_elements
  ;
type

  TGameActor = ( gaNone, gaAdmin, gaPlayer, gaWatcher );
  TGamePlayerStatus = (gpsWaiting, gpsPlaying, gpsPlayed);

  TGameMatrix = (gmColors, gmRows, gmColumns, gmDots, gmClearDots,gmDotsClearDots);

  TGameMatrixType = set of TGameMatrix;

  TGameRow = (
    grNone,

    // For contingencies only. It means 'a participant choosen a row'.
    grOne,grTwo,grThree,grFour,grFive,grSix,grSeven,grEight,grNine,grTen,  // 10 rows

    // For metacontingencies it means 'all participants choosen an even row'.
    // For contingencies it means 'a participant choosen an even row'.
    grEven,

    // For metacontingencies it means 'all participants choosen an odd row'.
    // For contingencies it means 'a participant choosen an odd row'.
    grOdd,

    // For metacontingencies only. It means 'all choosen rows are different from each other'.
    grDiff,

    // For metacontingencies only. It means 'all choosen rows are equal to each other'.
    grEqual,

    // Negates results.
    grNot,

    // todo: Negates the result.
    grNot_DIFF_EQUAL_ONLY,

    // todo: Negates the result.
    grNot_EVEN_ODD_ONLY
  );

  TGameRows = set of TGameRow;

  TGameColor = (
    gcNone,

    // For contingencies only. It means 'a participant choosen a color'.
    gcYellow,gcRed,gcGreen,gcBlue,gcMagenta, // 5 colors

    // For metacontingencies only. It means 'all choosen rows are different from each other'.
    gcDiff,

    // For metacontingencies only. It means 'all choosen rows are different from each other'.
    gcEqual,

    // For metacontingencies only. Has the following meanings:
    // 1) With grDiff:  Everything except different colors.
    // 2) With grEqual: Everything except equal colors.
    gcNot
  );

  TGameColors = set of TGameColor;

  TGameEndCondition = (gecInterlockingPorcentage,gecAbsoluteCycles,gecWhichComeFirst);
  //TGameOperator = (goNONE, goAND, goOR);
  TGameStyle = (gtNone, gtRowsOnly, gtColorsOnly, gtRowsAndColors, gtRowsOrColors);

  TGameConsequenceStyle = (gscNone, gscMessage, gscBroadcastMessage, gscPoints, gscVariablePoints, gscA, gscB,gscG,gscI);
  TConsequenceStyle = set of TGameConsequenceStyle;

  TGamePromptStyle = (gsNone, gsYes, gsNo, gsAll,
                      gsMetacontingency, gsContingency,
                      gsBasA, gsRevertPoints,gsRevertMetaPoints,gsRevertIndiPoints);

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

  TPlayerEvent = procedure (P : TPlayer; AMessage : string) of object;

   { TCriteria }

   TCriteria = record
     Style : TGameStyle;
     Rows : TGameRows;
     Colors : TGameColors;
   end;

   { TConsequence }

   TConsequence = class(TComponent)
   private
     FPrependEarn,
     FPrependLoss,
     FAppendiceZero,
     FAppendiceLossPlural,
     FAppendiceLossSingular,
     FAppendiceEarnPlural,
     FAppendiceEarnSingular,
     FPrepend: string;
     FStyle : TConsequenceStyle;
     FP : TGamePoint;
     FTimer : TTimer;
     FMessage : TPopupNotifier;
     function GetCsqString: string;
     function GetShouldPublishMessage: Boolean;
   protected
     FConsequenceByPlayerID : TStringList;
     procedure StopTimer(Sender:TObject;var ACloseAction:TCloseAction);
     procedure SelfDestroy(Sender:TOBject);virtual;
   public
     constructor Create(AOwner:TComponent; ACsqString, APrepend,
       APrependLoss,AAppendiceLossSingular,AAppendiceLossPlural,
       APrependEarn,AAppendiceEarnSingular,AAppendiceEarnPlural,AAppendiceZero:string);overload;
     constructor Create(AOwner:TComponent; AP:integer; AStyle: TConsequenceStyle; AMessage:array of string);overload;
     constructor Create(AOwner:TComponent; AConsequenceString: string);virtual;overload;
     destructor Destroy;override;
     function AsString(AID :string): string;
     function GenerateMessage(ForGroup: Boolean):string;
     procedure Clean; virtual;
     procedure PresentMessage(AControl : TWinControl);
     procedure PresentPoints(ALabelA, ALabelB, ALabelI, ALabelG : TLabel;
       var ACounterA, ACounterB, ACounterI, ACounterG : integer);
     procedure PresentPoints(APlayer: TPlayer; APlayerBox: TPlayerBox;
       var ACounterG: integer; ALabelG: TLabel); overload;
     property ShouldPublishMessage : Boolean read GetShouldPublishMessage;
     property Prepend : string read FPrepend;
     property AppendiceLossSingular : string read FAppendiceLossSingular;
     property AppendiceLossPlural : string read FAppendiceLossPlural;
     property AppendiceEarnSingular : string read FAppendiceEarnSingular;
     property AppendiceEarnPlural : string read FAppendiceEarnPlural;
     property AppendiceZero : string read FAppendiceZero;
     property PrependLoss : string read FPrependLoss;
     property PrependEarn : string read FPrependEarn;
     property CsqString : string read GetCsqString;
     property Style : TConsequenceStyle read FStyle;
     property ByPlayerID : TStringList read FConsequenceByPlayerID;
   end;

  { TContingency }

  TContingency = class(TComponent)
  private
    FFired,
    FMeta : Boolean; //True: Consequence occurs OnEndTurn, False: Consequence occurs OnEndCycle
    FConsequence : TConsequence;
    FCriteria : TCriteria;
    FName: string;
    FOnCriteria: TNotifyEvent;
    FOnTargetCriteria : TNotifyEvent;
    FStyle: TPromptStyle;
    function RowMod(R:TGameRow):TGameRow;
    procedure CriteriaEvent;
    procedure SetStyle(AValue: TPromptStyle);
  public
    constructor Create(AOwner:TComponent;AConsequence:TConsequence;ACriteria:TCriteria;IsMeta:Boolean);overload;
    function CriteriaString : string;
    function ResponseMeetsCriteriaI(R : TGameRow; C : TGameColor):Boolean; // Does response meets operant criteria?
    function ResponseMeetsCriteriaG(Players : TPlayers):Boolean;
    function ConsequenceFromPlayerID(AID:string):string;
    procedure Clean;
    property Consequence : TConsequence read FConsequence;
    property ContingencyName : string read FName write FName;
    property Criteria : TCriteria read FCriteria;
    property Fired : Boolean read FFired;
    property Meta : Boolean read FMeta;
    property OnCriteria : TNotifyEvent read FOnCriteria write FOncriteria;
    property OnTargetCriteria : TNotifyEvent read FOnTargetCriteria write FOnTargetCriteria;
    property Style : TPromptStyle read FStyle write SetStyle;
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
    FTarget : string;
    procedure ClearResponses;
  public
    constructor Create(AOwner:TComponent; APStyle:TPromptStyle;
      APTarget : TContingencies; AMessage:string; ATarget: string);reintroduce;
    function ResponsesCount : integer;
    function Response(I:integer):string;
    function AsString: TStringList; overload;
    procedure AppendResponse(AID,R:string);
    procedure Clean;override;
    property Question: string read FPromptMessage;
    property PromptResult:string read FResult;
    property PromptStyle : TPromptStyle read FPromptStyle;
    property TargetMetacontingencyName : string read FTarget;

  end;

  TEndConditionCriterium = record
    Style : TGameEndCondition;
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
    //Interlocks : record
    //  Count : integer; // culturant,
    //  History: array of Boolean; // to calculate interlock porcentage in the last cycles. sync with OnCycles
    //end;

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

uses Graphics,strutils, string_methods, game_actors_helpers, game_resources;

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
  if Assigned(FOnTargetCriteria) then FOnTargetCriteria(Self);
end;

procedure TContingency.SetStyle(AValue: TPromptStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
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


function TContingency.ResponseMeetsCriteriaG(Players: TPlayers): Boolean; // must be called from admin only
var
  i : integer;
  Cs : array of TGameColor;
  Rs : array of TGameRow;
  //C : TGameColor;
  R : TGameRow;
  Len : Byte;

const
  LDIFF = 0;
  LEQUL = 1;

  // AComparisonType = 0, a pair of different items exists
  // AComparisonType = 1, a pair of equal items exists
  function ColorRelationExists(AComparisonType:byte;
    InvertedResult:Boolean=False):Boolean;
  var
    i : integer;
    j : integer;
  begin
    Result := InvertedResult;
    for i := 0 to Len-2 do
      for j := i to Len-1 do
        case AComparisonType of
          0:
            if (Cs[i] <> Cs[j]) and (i <> j) then
              begin
                Result := not Result;
                Exit;
              end;
          1:
            if (Cs[i] = Cs[j]) and (i <> j) then
              begin
                Result := not Result;
                Exit;
              end;
        end;
  end;

  // AComparisonType = 0, find first pair of different items
  // AComparisonType = 1, find first pair of equal items
  function RowRelationExists(AComparisonType:byte;
    InvertedResult:Boolean=False):Boolean;
  var
    i : integer;
    j : integer;
  begin
    Result := InvertedResult;
    for i := 0 to Len-2 do
      for j := i to Len-1 do
        case AComparisonType of
          0:
            if (Rs[i] <> Rs[j]) and (i <> j) then
              begin
                Result := not Result;
                Exit;
              end;
          1:
            if (Rs[i] = Rs[j]) and (i <> j) then
              begin
                Result := not Result;
                Exit;
              end;
        end;
  end;

  // find first row of type AGamerow in player responses
  function RowExists(AGameRow:TGameRow;InvertedResult:Boolean=False): Boolean;
  var
    LGameRow : TGameRow;
    LRequireMod : byte;
  begin
    Result := InvertedResult;
    if (AGameRow = grEqual) or (AGameRow = grDiff) then
      raise Exception.Create('Do not apply');

    if (AGameRow = grEven) or (AGameRow = grOdd) then
      LRequireMod := 0
    else
      LRequireMod := 1;

    for R in Rs do
      begin
        case LRequireMod of
          0 :LGameRow := RowMod(R);
          1 :LGameRow := R;
        end;

        if LGameRow = AGameRow then
          begin
            Result := not Result;
            Break;
          end;

      end;
  end;

  function RowsResult:Boolean;
  begin
    Result := False;
    if (grDiff in Criteria.Rows) xor (grEqual in Criteria.Rows) then
      begin
        if (not (grOdd in Criteria.Rows)) and (not (grEven in Criteria.Rows)) then
          begin
            if (grDiff in Criteria.Rows) then
              if grNot in Criteria.Rows then
                Result := RowRelationExists(LEQUL)
              else
                Result := not RowRelationExists(LEQUL);

            if (grEqual in Criteria.Rows) then
              if grNot in Criteria.Rows then
                Result := RowRelationExists(LDIFF)
              else
                Result := not RowRelationExists(LDIFF);

            Exit;
          end;

        if (grDiff in Criteria.Rows) and (grOdd in Criteria.Rows) then
          begin
            if grNot in Criteria.Rows then
              begin
                if RowExists(grEven) then
                  Result := True
                else
                  if RowRelationExists(LEQUL) then
                    Result := True;
              end
            else
              begin
                if RowExists(grEven) then
                  Exit
                else
                  if RowRelationExists(LEQUL) then
                    Exit
                  else
                    Result := True;

              end;
            Exit;
          end;

        if (grEqual in Criteria.Rows) and (grOdd in Criteria.Rows) then
          begin
            if grNot in Criteria.Rows then
              begin
                if RowExists(grEven) then
                  Result := True
                else
                  if RowRelationExists(LDIFF) then
                    Result := True;
              end
            else
              begin
                if RowExists(grEven) then
                  Exit
                else
                  if RowRelationExists(LDIFF) then
                    Exit
                  else
                    Result := True;

              end;
            Exit;
          end;

        if (grDiff in Criteria.Rows) and (grEven in Criteria.Rows) then
          begin
            if grNot in Criteria.Rows then
              begin
                if RowExists(grOdd) then
                  Result := True
                else
                  if RowRelationExists(LEQUL) then
                    Result := True;
              end
            else
              begin
                if RowExists(grOdd) then
                  Exit
                else
                  if RowRelationExists(LEQUL) then
                    Exit
                  else
                    Result := True;
              end;
            Exit;
          end;

        if (grEqual in Criteria.Rows) and (grEven in Criteria.Rows) then
          begin
            if grNot in Criteria.Rows then
              begin
                if RowExists(grOdd) then
                  Result := True
                else
                  if RowRelationExists(LDIFF) then
                    Result := True;
              end
            else
              begin
                if RowExists(grOdd) then
                  Exit
                else
                  if RowRelationExists(LDIFF) then
                    Exit
                  else
                    Result := True;

              end;
          end;
      end
    else
      begin
        if grOdd in Criteria.Rows then
          if grNot in Criteria.Rows then
            Result := RowExists(grEven)
          else
            Result := not RowExists(grEven);

        if grEven in Criteria.Rows then
          if grNot in Criteria.Rows then
            Result := RowExists(grOdd)
          else
            Result := not RowExists(grOdd);
      end;
  end;

  //function AllDifferent : Boolean;
  //begin
  //  Result := not RelationExists(LEQUL) or RelationExists(LEQUL,True);
  //end;

  function ColorsResult: Boolean;
  begin
    if gcDiff in Criteria.Colors then
      if gcNot in Criteria.Colors then
        Result := ColorRelationExists(LEQUL)
      else
        Result := not ColorRelationExists(LEQUL);

    if gcEqual in Criteria.Colors then
      if gcNot in Criteria.Colors then
        Result := ColorRelationExists(LDIFF)
      else
        Result := not ColorRelationExists(LDIFF);
  end;

begin
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
    gtColorsOnly: Result := ColorsResult;
    gtRowsOnly: Result := RowsResult;
    gtRowsAndColors: Result := RowsResult and ColorsResult;
    gtRowsOrColors: Result := RowsResult or ColorsResult;
  end;
  if Result then
    CriteriaEvent;
end;

function TContingency.ConsequenceFromPlayerID(AID: string): string;
begin
  Result := Consequence.ByPlayerID.Values[AID];
end;

procedure TContingency.Clean;
begin
  FFired := False;
  Consequence.Clean;
end;


{ TPrompt }

procedure TPrompt.ClearResponses;
begin
  SetLength(FResponses,0);
end;

constructor TPrompt.Create(AOwner: TComponent; APStyle: TPromptStyle;
  APTarget: TContingencies; AMessage: string; ATarget: string);
begin
  inherited Create(AOwner);
  FPromptStyle := APStyle;
  FPromptTargets := APTarget;
  FPromptMessage := AMessage;
  FTarget := ATarget;
end;

function TPrompt.ResponsesCount: integer;
begin
  Result := Length(FResponses);
end;

function TPrompt.Response(I: integer): string;
begin
  Result := FResponses[I]
end;

procedure TPrompt.AppendResponse(AID, R: string);
begin
  SetLength(FResponses,Length(FResponses)+1);
  FResponses[High(FResponses)] := AID+'|'+R+'|';
end;

procedure TPrompt.Clean;
begin
  //inherited Clean;
  SetLength(FResponses,0);
end;

function TPrompt.AsString: TStringList;
  function AllPlayersClickedYes: Boolean;
  var i : integer;
  begin
    Result := True;
    for i := 0 to Length(FResponses)-1 do
      if ExtractDelimited(2,FResponses[i],['|']) = 'N' then
        Result := False;
  end;

begin
  Result := TStringList.Create;
  if (gsAll in FPromptStyle) and (gsYes in FPromptStyle) then
    if AllPlayersClickedYes then
      Result := GetMessagesFromPromptStyle(FPromptStyle,FPromptTargets);
end;

{ TConsequence }

constructor TConsequence.Create(AOwner: TComponent; ACsqString, APrepend,
  APrependLoss, AAppendiceLossSingular, AAppendiceLossPlural, APrependEarn,
  AAppendiceEarnSingular, AAppendiceEarnPlural, AAppendiceZero: string);
var
  LP : string;
  LGamePointFormat : TGamePointFormat;
begin
  inherited Create(AOwner);

  // custom message
  FPrepend:=APrepend;
  FPrependLoss:=APrependLoss;
  FAppendiceLossSingular:=AAppendiceLossSingular;
  FAppendiceLossPlural:=AAppendiceLossPlural;
  FPrependEarn:=APrependEarn;
  FAppendiceEarnSingular:=AAppendiceEarnSingular;
  FAppendiceEarnPlural:=AAppendiceEarnPlural;
  FAppendiceZero:=AAppendiceZero;

  // extract game point string
  LP := ExtractDelimited(1,ACsqString,['|']);

  // consequence style string
  FStyle := GetConsequenceStyleFromString(ExtractDelimited(2,ACsqString,['|']));

  if (gscA in FStyle) or (gscB in FStyle) then
    LGamePointFormat := gpfRealMoney
  else
    LGamePointFormat := gpfInteger;

  // [value,variation]
  FP := TGamePoint.Create(AOwner,
    [StrToInt(ExtractDelimited(1,LP,[','])),StrToInt(ExtractDelimited(2,LP,[',']))],
    LGamePointFormat);

  FMessage := TPopupNotifier.Create(AOwner);
  FConsequenceByPlayerID := TStringList.Create;
end;

constructor TConsequence.Create(AOwner: TComponent; AP: integer;
  AStyle:TConsequenceStyle; AMessage: array of string);
var
  LGamePointFormat : TGamePointFormat;
begin
  inherited Create(AOwner);
  FStyle:=AStyle;
  if (gscA in FStyle) or (gscB in FStyle) then
    LGamePointFormat := gpfRealMoney
  else
    LGamePointFormat := gpfInteger;
  FP := TGamePoint.Create(AOwner, AP, LGamePointFormat);

  FPrepend:=AMessage[0];
  FPrependLoss:=AMessage[1];
  FAppendiceLossSingular:=AMessage[2];
  FAppendiceLossPlural:=AMessage[3];
  FPrependEarn:=AMessage[4];
  FAppendiceEarnSingular:=AMessage[5];
  FAppendiceEarnPlural:=AMessage[6];
  FAppendiceZero:=AMessage[7];
  FMessage := TPopupNotifier.Create(AOwner);
  FConsequenceByPlayerID := TStringList.Create;
end;

constructor TConsequence.Create(AOwner: TComponent;
  AConsequenceString: string);
var
  LGamePointFormat : TGamePointFormat;
begin
  inherited Create(AOwner);
  FStyle:=GetConsequenceStyleFromString(ExtractDelimited(2,AConsequenceString,['|']));

  if (gscA in FStyle) or (gscB in FStyle) then
    LGamePointFormat := gpfRealMoney
  else
    LGamePointFormat := gpfInteger;
  FP := TGamePoint.Create(AOwner,ExtractDelimited(1,AConsequenceString,['|']), LGamePointFormat);
  FPrepend:=ExtractDelimited(3,AConsequenceString,['|']);
  FPrependLoss:=ExtractDelimited(4,AConsequenceString,['|']);
  FAppendiceLossSingular:=ExtractDelimited(5,AConsequenceString,['|']);
  FAppendiceLossPlural:=ExtractDelimited(6,AConsequenceString,['|']);
  FPrependEarn:=ExtractDelimited(7,AConsequenceString,['|']);
  FAppendiceEarnSingular:=ExtractDelimited(8,AConsequenceString,['|']);
  FAppendiceEarnPlural:=ExtractDelimited(9,AConsequenceString,['|']);
  FAppendiceZero:=ExtractDelimited(10,AConsequenceString,['|']);

  FMessage := TPopupNotifier.Create(Self);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled:=False;
  FTimer.Interval:=GLOBAL_MESSAGE_INTERVAL;
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
  Result += GetConsequenceStyleString(FStyle)+'|';
  Result += FPrepend +'|';
  Result += FPrependLoss + '|';
  Result += FAppendiceLossSingular + '|';
  Result += FAppendiceLossPlural + '|';
  Result += FPrependEarn + '|';
  Result += FAppendiceEarnSingular + '|';
  Result += FAppendiceEarnPlural + '|';
  Result += FAppendiceZero + '|';

  FConsequenceByPlayerID.Values[AID]:=Result;
end;

function TConsequence.GenerateMessage(ForGroup: Boolean): string;
begin
  FMessage.vNotifierForm.Font.Size:=12;
  Result := FP.PointMessage(FPrepend,FPrependLoss,FAppendiceLossSingular,FAppendiceLossPlural,
    FPrependEarn,FAppendiceEarnSingular,FAppendiceEarnPlural,FAppendiceZero, ForGroup);
  FMessage.Text := Result;
end;

procedure TConsequence.Clean;
begin
  FConsequenceByPlayerID.Clear;
end;

procedure TConsequence.PresentMessage(AControl : TWinControl);
var
  PopUpPos : TPoint;
begin
  PopUpPos.X := 0;
  if (gscA in FStyle) or (gscI in FStyle) then
    PopUpPos.Y := AControl.Top+AControl.Height;

  if gscB in FStyle then
    PopUpPos.Y := AControl.Top+AControl.Height+FMessage.vNotifierForm.Height;

  if gscG in FStyle then
    PopUpPos.Y := AControl.Top+AControl.Height+(FMessage.vNotifierForm.Height*2);

  PopUpPos := AControl.ClientToScreen(PopUpPos);
  FMessage.Color:=clTeal;
  FMessage.Title:='';
  FMessage.ShowAtPos(PopUpPos.X, PopUpPos.Y);
  FTimer.Enabled:=True;
end;


// for players
procedure TConsequence.PresentPoints(ALabelA, ALabelB, ALabelI, ALabelG : TLabel;
  var ACounterA, ACounterB, ACounterI, ACounterG : integer); // [player_points]
begin
  //is gscPoints in FStyle then just in case...
  if gscI in FStyle then
    IncLabel(ALabelI, ACounterI, FP.ResultAsInteger, lafInteger);

  if gscA in FStyle then
    IncLabel(ALabelA, ACounterA, FP.ResultAsInteger, lafRealMoney);

  if gscB in FStyle then
    IncLabel(ALabelB, ACounterB, FP.ResultAsInteger, lafRealMoney);

  if gscG in FStyle then
    IncLabel(ALabelG, ACounterG, FP.ResultAsInteger, lafInteger);
end;

// for admins
procedure TConsequence.PresentPoints(APlayer: TPlayer; APlayerBox: TPlayerBox;
  var ACounterG: integer; ALabelG: TLabel);
var
  LFakeCounter : integer = 0;
begin
  if Assigned(APlayerBox) then
  begin
    if gscI in FStyle then
      IncLabel(APlayerBox.LabelPointsCount,
        APlayer.Points.A, FP.ResultAsInteger, lafInteger);

    if (gscA in FStyle) or (gscB in FStyle) then
    begin
      if gscA in FStyle then
        Inc(APlayer.Points.A, FP.ResultAsInteger);

      if gscB in FStyle then
        Inc(APlayer.Points.B, FP.ResultAsInteger);

      IncLabel(APlayerBox.LabelPointsCount, LFakeCounter,
        APlayer.Points.A+APlayer.Points.B, lafRealMoney);
    end;
  end;

  if gscG in FStyle then
    IncLabel(ALabelG, ACounterG, FP.ResultAsInteger, lafInteger);
end;

function TConsequence.GetShouldPublishMessage: Boolean; // for players only
begin
  Result := gscBroadcastMessage in FStyle;
end;

function TConsequence.GetCsqString: string;
begin
  Result := IntToStr(FP.Value)+','+IntToStr(FP.Variation) + '|';
  Result += GetConsequenceStyleString(FStyle);
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

