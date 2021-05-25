unit game_report;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , game_actors
  , regdata
  , report_reader
  ;

type

  TNotifyOnWriteReport = procedure (S : string) of object;

  { TGameReport }

  TGameReport = class
  private
    FActor               : TGameActor;
    FRegData             : TRegData;
    FRegChat             : TRegData;
    FReportReader        : TReportReader;
    FPCurrentCondition   : PInteger;
    FPConditions         : PConditions;
    FPPlayers            : PPlayers;
    FPCycles             : PCycles;
    FReportFolder        : string;
    FLastReportColNames2 : string;
    FLastReportColNames  : string;
    function GetCurrentCondition : TCondition;
    procedure WriteHeader(AResearcher, AExperimentName : string);
    procedure WriteRowNames;
  private
    FOnWriteReport: TNotifyOnWriteReport;
    procedure SetOnWriteReport(AValue: TNotifyOnWriteReport);
  public
    constructor Create(AGameActor: TGameActor); reintroduce;
    destructor Destroy; override;
    procedure Clean;
    procedure NextCondition;
    procedure WriteFooter;
    procedure WriteRow;
    procedure WriteRowPrompt;
    procedure WriteChatLn(ALn : string);
    procedure Start(AFilename, AResearcher, AExperimentName : string;
      ACurrentCondition: PInteger; ACondition : PConditions;
      ACycles : PCycles; APlayers : PPLayers);
  public
    property Reader        : TReportReader read FReportReader;
    property OnWriteReport : TNotifyOnWriteReport read FOnWriteReport write SetOnWriteReport;
  end;

implementation

uses
  game_resources
  , string_methods;

const
  TabDelimiter : Char = #9;

procedure TGameReport.WriteFooter;
var
  LFooter : string;
begin
  if Assigned(FRegData) then begin
    LFooter := LineEnding +
      VAL_END_TIME    + ':' + TabDelimiter +
      DateTimeToStr(Date) + TabDelimiter +
      TimeToStr(Time) + TabDelimiter  + LineEnding;

    FRegData.SaveData(LFooter);
    if Assigned(OnWriteReport) then OnWriteReport(LFooter);
  end;
end;

function TGameReport.GetCurrentCondition : TCondition;
begin
  Result := FPConditions^[FPCurrentCondition^];
end;

procedure TGameReport.WriteHeader(AResearcher, AExperimentName : string);
var
  LHeader : string;
begin
  if Assigned(FRegData) then begin
    LHeader :=
      VAL_RESEARCHER+':'  + TabDelimiter +
      AResearcher         + TabDelimiter + LineEnding +
      VAL_EXPERIMENT+':'  + TabDelimiter +
      AExperimentName     + TabDelimiter + LineEnding +
      VAL_BEGIN_TIME+':'  + TabDelimiter +
      DateTimeToStr(Date) + TabDelimiter +
      TimeToStr(Time)     + TabDelimiter + LineEnding +
      TabDelimiter        + LineEnding;

    FRegData.SaveData(LHeader);
    if Assigned(FOnWriteReport) then FOnWriteReport(LHeader);
  end;
end;

procedure TGameReport.WriteRowNames;
var
  j,i: integer;
  LCondition : TCondition;
  LContingencies : TContingencies;
  LPlayers : TPlayers;
  LNames1, LNames2 : string;
begin
  LCondition := GetCurrentCondition;
  LContingencies := LCondition.Contingencies;
  LPlayers   := FPPlayers^;

  // column names, line 1
  LNames1 := 'Experiment'+TabDelimiter+TabDelimiter+TabDelimiter;
  for i:=0 to LCondition.Turn.Value-1 do // player's response
    begin
      LNames1 += LPlayers[i].Nicname+TabDelimiter+TabDelimiter;

      for j:= Low(LContingencies) to High(LContingencies) do
        if not LContingencies[j].Meta then
          LNames1 += TabDelimiter;
    end;

  LNames1 += VAL_INTERLOCKING+'s';
  for j:= Low(LContingencies) to High(LContingencies) do
    if LContingencies[j].Meta then
      LNames1 += TabDelimiter;

  if Assigned(LCondition.Prompt) then
    begin
      LNames1 += 'Responses to the question';
      for i:=0 to LCondition.Turn.Value-1 do
        LNames1 += TabDelimiter;
    end;
  LNames1 += LineEnding;

  // column names, line 2
  LNames2 := 'Condition'+TabDelimiter+'Generation'+TabDelimiter+'Cycles'+TabDelimiter;
  for i:=0 to LCondition.Turn.Value-1 do
    begin
      LNames2 += 'Row'+TabDelimiter+'Color'+TabDelimiter;
      for j:= Low(LContingencies) to High(LContingencies) do
        if not LContingencies[j].Meta then
          LNames2 += LContingencies[j].ContingencyName+TabDelimiter;
    end;

  for j:= Low(LContingencies) to High(LContingencies) do
    if LContingencies[j].Meta then
      LNames2 += LContingencies[j].ContingencyName+TabDelimiter;

  if Assigned(LCondition.Prompt) then
    for i:=0 to LCondition.Turn.Value-1 do
      LNames2 += 'R'+IntToStr(i+1)+TabDelimiter;
  LNames2 += LineEnding;

  if FLastReportColNames <> LNames1+LNames2 then
    begin
      FLastReportColNames := LNames1+LNames2;
      FLastReportColNames2 := LNames2;
      if Assigned(FRegData) then begin
        FRegData.SaveData(FLastReportColNames);
      end;
      if FReportReader.Cols = 0 then
        FReportReader.UpdateCols(FLastReportColNames2);
      if Assigned(FOnWriteReport) then FOnWriteReport(FLastReportColNames);
    end;
end;

procedure TGameReport.WriteRow;
var
  j,i: integer;
  LRow : string;
  LCondition : TCondition;
  LContingencies : TContingencies;
  LCycles : TCycles;
  LPlayers : TPlayers;
begin
  WriteRowNames;
  LCondition := GetCurrentCondition;
  LContingencies := LCondition.Contingencies;
  LCycles := FPCycles^;
  LPlayers := FPPlayers^;

  LRow := IntToStr(LCondition.Index+1)+TabDelimiter+
          IntToStr(LCycles.Generations+1)+TabDelimiter+
          IntToStr(LCycles.Global+1)+TabDelimiter;

  for i:=0 to LCondition.Turn.Value-1 do
    begin
    LRow += GetRowAsString(
      LPlayers[i].Choice.Row)+TabDelimiter+
      GetColorStringFromGameColor(LPlayers[i].Choice.Color)+TabDelimiter;
    for j:= Low(LContingencies) to High(LContingencies) do
      if not LContingencies[j].Meta then
        if LContingencies[j].ConsequenceFromPlayerID(LPlayers[i].ID) <> '' then
          LRow += '1'+TabDelimiter
        else
          LRow += '0'+TabDelimiter;
    end;

  for j:= Low(LContingencies) to High(LContingencies) do
    if LContingencies[j].Meta then
      if LContingencies[j].Fired then
        LRow += '1'+TabDelimiter
      else
        LRow += '0'+TabDelimiter;

  if Assigned(LCondition.Prompt) then
    // do nothing
  else
    LRow += LineEnding;

  if Assigned(FRegData) then begin
    FRegData.SaveData(LRow);
  end;
  FReportReader.Append(LRow);
  if Assigned(FOnWriteReport) then FOnWriteReport(LRow);

end;

procedure TGameReport.SetOnWriteReport(AValue : TNotifyOnWriteReport);
begin
  if FOnWriteReport = AValue then Exit;
  FOnWriteReport := AValue;
end;

constructor TGameReport.Create(AGameActor : TGameActor);
begin
  FActor := AGameActor;
end;

procedure TGameReport.NextCondition;
var
  LCondition : TCondition;
begin
  LCondition := GetCurrentCondition;
  FReportReader.Clean;
  FReportReader.SetXLastRows(LCondition.EndCriterium.LastCycles);
  if Assigned(FRegData) then begin
    FRegData.SaveData(LineEnding);
  end;
  WriteRowNames;
  FReportReader.UpdateCols(FLastReportColNames2);
end;

destructor TGameReport.Destroy;
begin
  if Assigned(FReportReader) then
    FReportReader.Free;

  if Assigned(FRegData) then
    FRegData.Free;

  if Assigned(FRegChat) then
    FRegChat.Free;
  inherited Destroy;
end;

procedure TGameReport.Clean;
begin
  if Assigned(FRegData) then
    FRegData.CloseAndOpen;
end;

procedure TGameReport.WriteRowPrompt;
var
  i : integer;
  LRow : string;
  LPlayerID : string;
  LPlayerResponse : string;
  LCondition : TCondition;
  function PlayerIndexFromID(AID : string) : string;
  var
    LPlayers : TPlayers;
    i : integer;
  begin
    LPlayers := FPPlayers^;
    Result := '?';
    for i := Low(LPlayers) to High(LPlayers) do
      if LPlayers[i].ID = AID then
        begin
          Result := (i+1).ToString;
          Break;
        end;
  end;
begin
  LCondition := FPConditions^[FPCurrentCondition^];
  if Assigned(LCondition.Prompt) then
    begin
      LRow := '';
      if LCondition.Prompt.ResponsesCount = LCondition.Turn.Value then
        for i:=0 to LCondition.Prompt.ResponsesCount-1 do begin
          LPlayerID := Delimited(1,LCondition.Prompt.Response(i));
          LPlayerID := PlayerIndexFromID(LPlayerID);
          LPlayerResponse := Delimited(2,LCondition.Prompt.Response(i));
          LRow += 'P' + LPlayerID + '|' + LPlayerResponse + TabDelimiter;
        end
      else
        for i:=0 to LCondition.Turn.Value-1 do
          LRow += 'NA'+TabDelimiter;
      LRow += LineEnding;
      if Assigned(FRegData) then begin
        FRegData.SaveData(LRow);
      end;
      FReportReader.Extend(LRow);  // Write, i.e, extend last row
      if Assigned(FOnWriteReport) then FOnWriteReport(LRow);
    end;
end;

procedure TGameReport.WriteChatLn(ALn : string);
begin
  if Assigned(FRegChat) then
    begin
      FRegChat.SaveData(ALn);
      FRegChat.CloseAndOpen;
    end;
end;

procedure TGameReport.Start(AFilename, AResearcher, AExperimentName : string;
  ACurrentCondition : PInteger; ACondition : PConditions; ACycles : PCycles;
  APlayers : PPLayers);
var
  LCondition : TCondition;
begin;
  FPCurrentCondition := ACurrentCondition;
  FPConditions := ACondition;
  FPCycles := ACycles;
  FPPlayers := APlayers;

  LCondition := GetCurrentCondition;
  FReportReader := TReportReader.Create;
  FReportReader.UseRange:=True;
  FReportReader.SetXLastRows(LCondition.EndCriterium.LastCycles);

  FReportFolder := ExtractFilePath(AFilename)+AExperimentName+PathDelim;
  if FActor = gaAdmin then begin
    FRegData := TRegData.Create(nil, FReportFolder+'000.data');
    FRegChat := TRegData.Create(nil, FReportFolder+'000.chat');
  end;
  WriteHeader(AResearcher, AExperimentName);
end;

end.

