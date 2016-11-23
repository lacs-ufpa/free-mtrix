unit game_actors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PopupNotifier
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
              grDiff,grAll,grNot,grSome); //meta only

  TGameRows = set of TGameRow;

  TGameColor = (gcNone,
                gcYellow,gcRed,gcGreen,gcBlue,gcMagenta, // 5 colors
                gcDiff,gcEqual,gcAll,gcNot,gcSome); //meta only

  TGameColors = set of TGameColor;

  TGameEndCondition = (gecInterlockingPorcentage,gecAbsoluteCycles,gecWhichComeFirst);
  TGameOperator = (goNONE, goAND, goOR);
  TGameStyle = (gtNone, gtRowsOnly, gtColorsOnly, gtRowsAndColors, gtRowsOrColors);

  TGameConsequenceStyle = (gscNone, gscMessage, gscBroadcastMessage, gscPoints, gscVariablePoints);
  TConsequenceStyle = set of TGameConsequenceStyle;

  TGamePromptStyle = (gsYes, gsNo, gsAll, gsMetacontingency, gsContingency, gsBasA, gsRevertPoints);

  TPromptStyle = set of TGamePromptStyle;


type

   { TCriteria }

   TCriteria = record
     Style : TGameStyle;
     Rows : TGameRows;
     Colors : TGameColors;
   end;

   { TConsequence }

   TConsequence = class(TComponent)
   public
     Style : TConsequenceStyle;
     Message : TPopupNotifier;
     Points : record
      A, B, G : TGamePoint;
     end;
     procedure Present; virtual;
   end;

  { TContingency }

  TContingency = class(TComponent)
  private
    FFired: Boolean;
    FOnCriteria: TNotifyEvent;
    procedure CriteriaEvent;
  public
    Meta : Boolean; //True: Consequence occurs OnEndTurn, False: Consequence occurs OnEndCycle
    Consequence : TConsequence;
    Criteria : TCriteria;
    property OnCriteria : TNotifyEvent read FOnCriteria write FOncriteria;
    property Fired : Boolean read FFired;
  end;

  { TContingencies }

  TContingencies = array of TContingency;

  { TPrompt }

  TPrompt = class(TConsequence)
  public
    PromptStyle : TPromptStyle;
    PromptTargets : ^TContingencies;
    PromptMessage : string;
    procedure Present; override;
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

  TPLayerPoints = record
    A, B : integer
  end;

  TPlayerChoice =  record
    Row : TGameRow;
    Color : TGameColor;
  end;

  PPlayer = ^TPlayer;

  TPlayer = record
    ID,
    Nicname,
    Login,
    Password : UTF8string;
    Status : TGamePlayerStatus;
    Data : TStringList;
    Choice : record
      Current, Last : TPlayerChoice;
    end;
    Points : TPLayerPoints;
    Turn : ShortInt;
  end;

implementation

uses Forms,ButtonPanel,Controls,StdCtrls,ExtCtrls;

{ TContingency }

procedure TContingency.CriteriaEvent;
begin

end;

{ TPrompt }

procedure TPrompt.Present;

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
  inherited Present;
  //SendMessage(AskQuestion);
end;

{ TConsequence }

procedure TConsequence.Present;
begin
  AbstractError;
end;

end.

