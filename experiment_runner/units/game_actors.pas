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

    // Todo:For metacontingencies only. It means 'all choosen rows are different from each other'.
    grDiff,

    // Todo:For metacontingencies only. It means 'all choosen rows are equal to each other'.
    grEqual,

    // For metacontingencies only. Has the following meanings:
    // 1) Todo:With grDiff:  Everything except different rows.
    // 2) Todo:With grEqual: Everything except equal equal.
    // 3) With grEven: Some odd row was choosen.
    // 4) With grOdd: Some even row was choosen.
    grNot
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
     procedure PresentPoints(A, B, I, G : TLabel);
     procedure PresentPoints(APlayerBox: TPlayerBox; G: TLabel); overload;
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
    procedure ClearResponses;
  public
    constructor Create(AOwner:TComponent; APStyle:TPromptStyle; APTarget : TContingencies; AMessage:string);reintroduce;
    function ResponsesCount : integer;
    function Response(I:integer):string;
    function AsString: TStringList; overload;
    procedure AppendResponse(AID,R:string);
    procedure Clean;override;
    property Question: string read FPromptMessage;
    property PromptResult:string read FResult;
    property PromptStyle : TPromptStyle read FPromptStyle;

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

uses Graphics,strutils, string_methods, game_actors_helpers;

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


function TContingency.ResponseMeetsCriteriaG(Players: TPlayers): Boolean; // must be for admin only
var i : integer;
    Cs : array of TGameColor;
    Rs : array of TGameRow;
    //C : TGameColor;
    R : TGameRow;
    Len : Byte;

    {
      without gcNot/grNot returns
      1) true if all colors are diff  // brute-force
      2) true if all colors are equal // brute-force
      3) true if all rows are even    // pick first
      4) true if all rows are odd     // pick first

      with gcNot/grNot returns
      1) true if everything except different colors,
      2) true if everything except equal colors,
      3) true if some odd row
      4) trus if some even row
    }
    // brute-force
    function AllColorsEqual:Boolean;
    var
      i : integer;
      j : integer;
    begin
      Result := not (gcNot in Criteria.Colors);
      for i := 0 to Len-2 do
        for j := i to Len-1 do
          if (Cs[i] <> Cs[j]) and (i <> j) then
            begin
              Result := not Result;
              Exit;
            end;
    end;

    // brute-force
    function AllColorsDiff:Boolean;
    var i : integer;
        j : integer;
    begin
      Result := not (gcNot in Criteria.Colors);
      for i := 0 to Len-1 do
        for j := i to Len-1 do
          if (Cs[i] = Cs[j]) and (i <> j) then
            begin
              Result := not Result;
              Exit;
            end;
    end;

    // pick first
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

    // pick first
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

  // [value,variation]
  FP := TGamePoint.Create(AOwner,[StrToInt(ExtractDelimited(1,LP,[','])),StrToInt(ExtractDelimited(2,LP,[',']))]);

  // consequesen style string
  FStyle := GetConsequenceStyleFromString(ExtractDelimited(2,ACsqString,['|']));

  FMessage := TPopupNotifier.Create(AOwner);
  FConsequenceByPlayerID := TStringList.Create;
end;

constructor TConsequence.Create(AOwner: TComponent; AP: integer;
  AStyle:TConsequenceStyle; AMessage: array of string);
begin
  inherited Create(AOwner);
  FP := TGamePoint.Create(AOwner,AP);
  FStyle:=AStyle;
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
begin
  inherited Create(AOwner);
  FP := TGamePoint.Create(AOwner,ExtractDelimited(1,AConsequenceString,['|']));
  FStyle:=GetConsequenceStyleFromString(ExtractDelimited(2,AConsequenceString,['|']));
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
  FTimer.Interval:=5000;
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

procedure TConsequence.PresentPoints(A, B, I, G : TLabel);
begin
  //is gscPoints in FStyle then just in case...
  if gscI in FStyle then
    I.Caption := IntToStr(StrToInt(I.Caption) + FP.ResultAsInteger);

  if gscA in FStyle then
    A.Caption := IntToStr(StrToInt(A.Caption) + FP.ResultAsInteger);

  if gscB in FStyle then
    B.Caption := IntToStr(StrToInt(B.Caption) + FP.ResultAsInteger);

  if gscG in FStyle then
    G.Caption:= IntToStr(StrToInt(G.Caption) + FP.ResultAsInteger);
end;

procedure TConsequence.PresentPoints(APlayerBox: TPlayerBox; G: TLabel);
begin
  if gscG in FStyle then
    G.Caption:= IntToStr(StrToInt(G.Caption) + FP.ResultAsInteger);

  if (gscI in FStyle) or (gscA in FStyle) or (gscB in FStyle) then
    if Assigned(APlayerBox) then
      APlayerBox.LabelPointsCount.Caption := IntToStr(StrToInt(APlayerBox.LabelPointsCount.Caption) + FP.ResultAsInteger);
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

