unit game_file_methods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, IniFiles
  , game_experiment
  , game_actors
  ;

type

  { TCIniFile }

  TCIniFile = class (TIniFile)
  public
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
  end;
  function LoadExperimentFromResource(var AExperiment: TExperiment):Boolean;
  function LoadExperimentFromFile(var AExperiment: TExperiment; AFilename : string):Boolean;
  procedure SaveExperimentToFile(AExperiment: TExperiment; AFilename : string);

resourcestring
  ERROR_SECTION_NOT_FOUND = 'O arquivo não pode ser aberto, pois não foi encontrada a secção: ';
  ERROR_FILE_NOT_FOUND = 'O arquivo não pode ser aberto, pois ele não existe.';
  WARN_CONDITION_WITH_NO_END = 'Condição sem critério de encerramento: ';
  WARN_END = ' será usado.';

implementation

uses LCLIntf, game_actors_point, game_resources, string_methods, regdata, zhelpers;

function LoadExperimentFromResource(var AExperiment: TExperiment): Boolean;
var i,j : integer;
  C : TCondition;
begin
  Result := False;
  with AExperiment do
    begin
      Researcher := VAL_RESEARCHER;
      ResearcherCanPlay:=False;
      ResearcherCanChat:=True;
      SendChatHistoryForNewPlayers:=True;
      ExperimentName:='Test Experiment';
      ExperimentAim:='This is a test experiment.';
      GenPlayersAsNeeded:=True;
      CurrentCondition := 0;
      MatrixType:=[gmRows];
      //AppendPlayer(C_PLAYER_TEMPLATE);
      //AppendPlayer(C_PLAYER_TEMPLATE);
      C := C_CONDITION_TEMPLATE;
      with C  do
        begin
          ConditionName := SEC_CONDITION+IntToStr(1);
          Turn.Count:=0;
          Turn.Value:=2;
          Turn.Random:=False;
        end;
      i := AppendCondition(C);
    end;
end;

function LoadExperimentFromFile(var AExperiment: TExperiment; AFilename: string):Boolean;
var
  LIniFile : TCIniFile;
  LExperiment : TExperiment;

  //procedure HandleRootPath(var APath : string);
  //begin
  //  while Pos(PathDelim, s1) <> 0 do Delete(s1, 1, Pos(PathDelim, s1));
  //  APath:= ExtractFilePath(AFileName) + s1;
  //  if not (APath[Length(APath)] = PathDelim) then APath:= APath + PathDelim;
  //end;

  function GetEndCriteria(S:string) : TEndConditionCriterium;
  var
    LS : string;
  begin
    // 2,20,10,10,
    LS := S + VV_SEP;
    case StrToIntDef(GetAndDelFirstValue(LS),2) of
      0: Result.Value := gecAbsoluteCycles;
      1: Result.Value := gecInterlockingPorcentage;
      2: Result.Value := gecWhichComeFirst;
    end;
    Result.AbsoluteCycles := StrToIntDef(GetAndDelFirstValue(LS), 20);
    Result.InterlockingPorcentage := StrToIntDef(GetAndDelFirstValue(LS),10);
    Result.LastCycles := StrToIntDef(GetAndDelFirstValue(LS), 10);
  end;

  function GetPoints(S: string) : TPoints;
  var
    LS : string;
  begin
    // A,B,G,
    LS := S + VV_SEP;
    Result.A := StrToIntDef(GetAndDelFirstValue(LS),0);
    Result.B := StrToIntDef(GetAndDelFirstValue(LS),0);
    Result.G := StrToIntDef(GetAndDelFirstValue(LS),0);
  end;

  function GetConsequence(S: string) : TConsequence;
  var
    CS : TGameConsequenceStyle;
    LS : string;
  begin
    // 0,0,0,0,0,0,NON
    LS := UpperCase(S + VV_SEP);
    Result.Points.A.Value := StrToIntDef(GetAndDelFirstValue(LS),0);
    Result.Points.A.Variation:=StrToIntDef(GetAndDelFirstValue(LS),0);

    Result.Points.B.Value := StrToIntDef(GetAndDelFirstValue(LS),0);
    Result.Points.B.Variation:=StrToIntDef(GetAndDelFirstValue(LS),0);

    Result.Points.G.Value := StrToIntDef(GetAndDelFirstValue(LS),0);
    Result.Points.G.Variation:=StrToIntDef(GetAndDelFirstValue(LS),0);

    Result.Style := [];
    for CS in TGameConsequenceStyle do
      Result.Style += [GetConsequenceStyleFromString(GetAndDelFirstValue(LS))]

  end;

  function GetChoiceFromString(S:string) : TPlayerChoice;
  var
    LS : string;
  begin
    // 0,NONE,
    LS := S + VV_SEP;
    Result.Row := GetRowFromString(GetAndDelFirstValue(LS));
    Result.Color := GetColorFromString(GetAndDelFirstValue(LS));
  end;

  function GetPPointsFromString(S:string) : TPlayerPoints;
  var
    LS : string;
  begin
    // 0,0,
    LS := S + VV_SEP;
    Result.A := StrToIntDef(GetAndDelFirstValue(LS),0);
    Result.B := StrToIntDef(GetAndDelFirstValue(LS),0);
  end;

  function GetStatusFromString(S : string): TGamePlayerStatus;
  var
    LS : string;
  begin
    LS := S + VV_SEP;
    case GetAndDelFirstValue(LS) of
      'esperando': Result := gpsWaiting;
      'jogou': Result := gpsPlayed;
      'jogando': Result := gpsPlaying;
    end;
  end;

  function GetPromptStyle(S:string):TPromptStyle;
  var
    LS : string;
    i : integer;
  begin
    // Yes,All,Metacontingency,RecoverLostPoints,
    Result := [];
    LS := S + VV_SEP;
    for i := 0 to 3 do
        Result := Result + GetPromptStyleFromString(GetAndDelFirstValue(LS));
  end;

  procedure ReadExperiment;
  begin
    // Experiment;
    with LIniFile do
      begin
        LExperiment.Researcher := ReadString(SEC_EXPERIMENT, KEY_RESEARCHER,VAL_RESEARCHER);
        LExperiment.ExperimentName:=ReadString(SEC_EXPERIMENT, KEY_NAME,'');
        LExperiment.ExperimentAim:=ReadString(SEC_EXPERIMENT, KEY_AIM,'');
        LExperiment.GenPlayersAsNeeded:=ReadBool(SEC_EXPERIMENT, KEY_GEN_PLAYER_AS_NEEDED,True);
        LExperiment.CurrentCondition := ReadInteger(SEC_EXPERIMENT, KEY_CURRENT_CONDITION,0)-1; //zero based
      end;
  end;

  procedure ReadPlayers;
  var
    LS : string;
    i : integer;
  begin
    i := 0;
    LS := SEC_PLAYER+IntToStr(i+1);
    with LIniFile do
      while SectionExists(LS) do
        with LExperiment.Player[LExperiment.AppendPlayer] do
          begin
            Turn := ReadInteger(LS,KEY_PLAYER_TURN,i);
            Choice.Current := GetChoiceFromString(ReadString(LS,KEY_PLAYER_CHOICE_CURRENT,'0,NONE,'));
            Choice.Last := GetChoiceFromString(ReadString(LS,KEY_PLAYER_CHOICE_LAST,'0,NONE,'));
            ID := ReadString(LS,KEY_PLAYER_ID,s_random(20));
            Nicname := ReadString(LS,KEY_PLAYER_NICNAME,GenResourceName(i));
            Login := ReadString(LS,KEY_PLAYER_LOGIN,'jogador'+IntToStr(i+1));
            Password := ReadString(LS,KEY_PLAYER_PASSWORD,'1234');
            Points := GetPPointsFromString(ReadString(LS,KEY_PLAYER_POINTS,'0,0,'));
            Status := GetStatusFromString(ReadString(LS,KEY_PLAYER_STATUS,'esperando'));
            Data.Values[KEY_PLAYER_TEMP] := ReadString(LS,KEY_PLAYER_TEMP,'');
          end;
  end;

  procedure ReadContingencies(ACondition:integer;IsMeta : Boolean);
  var i : integer;
      LS,LCK : string;
    procedure SetLCK;
    begin
      if IsMeta then
        LCK := KEY_METACONTINGENCY+IntToStr(i+1)
      else
        LCK := KEY_CONTINGENCY+IntToStr(i+1);
    end;
  begin
    LS := SEC_CONDITION+IntToStr(ACondition+1);
    i := 0;
    SetLCK;
    with LIniFile do
      while ValueExists(LS, LCK+KEY_CONSEQUE) do
        with LExperiment.Condition[ACondition].Contingencies[LExperiment.AppendContingency(ACondition)] do
          begin
            Meta:=IsMeta;
            Consequence := GetConsequence(ReadString(LS,LCK+KEY_CONSEQUE,DEF_CONSEQUENCE));
            if IsMeta then
              Consequence.Message.Text := ReadString(LS,LCK+KEY_CONSEQUE_MESSAGE,DEF_CONSEQUENCE_MESSAGE)
            else
              Consequence.Message.Text := ReadString(LS,LCK+KEY_CONSEQUE_MESSAGE,DEF_CONSEQUENCE_MESSAGE);

            Criteria := GetResponseFromString(ReadString(LS,LCK+KEY_RESPONSE,DEF_RESPONSE));

            Inc(i);
            SetLCK;
          end;
  end;

  procedure ReadConditions;
  var
    s1, LS : string;
    LCondition : integer;
  begin
    LCondition := 0;
    LS := SEC_CONDITION+IntToStr(LCondition+1);
    with LIniFile do
      while SectionExists(LS) do
        with LExperiment.Condition[LExperiment.AppendCondition] do
          begin
            s1 := ReadString(LS, KEY_ENDCRITERIA,'');
            if s1 = '' then
              begin
                {$IFDEF DEBUG}
                WriteLn(WARN_CONDITION_WITH_NO_END+LS+'. '+KEY_ENDCRITERIA+KV_SEP+DEF_END+WARN_END);
                {$ENDIF}
                s1 := DEF_END;
              end;
            EndCriterium := GetEndCriteria(s1);
            ConditionName := ReadString(LS,KEY_COND_NAME,LS);
            Points.Count := GetPoints(ReadString(LS, KEY_POINTS_COUNT,DEF_POINTS));
            Points.OnStart := GetPoints(ReadString(LS, KEY_POINTS_ONSTART,DEF_POINTS));
            Turn.Count:= ReadInteger(LS, KEY_TURN_COUNT,1);
            Turn.Value:= ReadInteger(LS, KEY_TURN_VALUE,2);
            Turn.Random:= ReadBool(LS, KEY_TURN_RANDOM,False);
            Cycles.Count:= ReadInteger(LS, KEY_CYCLES_COUNT,1);
            Cycles.Value:= ReadInteger(LS, KEY_CYCLES_VALUE,10);
            Cycles.Generation:= ReadInteger(LS, KEY_CYCLES_GEN,1);

            // todo: create and initialize prompt based on its values
            ///////////////////////////////////
            // need to create classes first ///
            ///////////////////////////////////

            Prompt.PromptStyle:= GetPromptStyle(ReadString(LS,KEY_PROMPT_STYLE,'todos,sim,metacontingência,recuperar pontos,'));
            Prompt.PromptMessage := ReadString(LS,KEY_PROMPT_MESSAGE,DEF_PROMPTMESSAGE);

            ReadContingencies(LCondition,True);
            ReadContingencies(LCondition,False);

            Prompt.PromptTargets:=@Contingencies;

            Inc(LCondition);
            LS := SEC_CONDITION+IntToStr(LCondition+1);
          end;
  end;

begin
  Result := False;
  if FileExists(AFileName) then
    begin
      LIniFile:= TCIniFile.Create(AFileName);
      with LIniFile do
        if SectionExists(SEC_EXPERIMENT) then
          begin
            LExperiment.Create(AExperiment.Owner);
            ReadExperiment;
            ReadPlayers;
            ReadConditions;
          end
        else
          begin
            ShowMessage(ERROR_SECTION_NOT_FOUND+SEC_EXPERIMENT);
            LIniFile.Free;
            Exit;
          end;
      Result := True;
      LIniFile.Free;
      AExperiment := LExperiment;
      LExperiment.Free;
    end
  else
    ShowMessage(ERROR_FILE_NOT_FOUND);
end;

procedure SaveExperimentToFile(AExperiment: TExperiment; AFilename: string);
var
  i,j : Integer;
  LWriter : TRegData;
  LIniFile : TCIniFile;
  LC,
  LCK : string;

  function GetEndCriteriaString(AEndCriterium:TEndConditionCriterium) : string;
  begin
    // 2,20,10,10,
    case AEndCriterium.Value of
      gecAbsoluteCycles: Result := '0';
      gecInterlockingPorcentage: Result := '1';
      gecWhichComeFirst: Result := '2';
    end;
    Result := Result + VV_SEP;
    Result := Result + IntToStr(AEndCriterium.AbsoluteCycles) + VV_SEP;
    Result := Result + IntToStr(AEndCriterium.InterlockingPorcentage) + VV_SEP;
    Result := Result + IntToStr(AEndCriterium.LastCycles) + VV_SEP;
  end;

  function GetPointsString(APoints : TPoints) : string;
  begin
    Result := IntToStr(APoints.A) + VV_SEP;
    Result := Result + IntToStr(APoints.B) + VV_SEP;
    Result := Result + IntToStr(APoints.G) + VV_SEP;
  end;

  function GetConsequenceString(AConsequence : TConsequence) : string;
  var CS : TGameConsequenceStyle;
  begin
    Result := IntToStr(AConsequence.Points.A.Value);
    Result := Result + IntToStr(AConsequence.Points.A.Variation) + VV_SEP;

    Result := Result + IntToStr(AConsequence.Points.B.Value) + VV_SEP;
    Result := Result + IntToStr(AConsequence.Points.B.Variation) + VV_SEP;

    Result := Result + IntToStr(AConsequence.Points.G.Value) + VV_SEP;
    Result := Result + IntToStr(AConsequence.Points.G.Variation) + VV_SEP;

    for CS in AConsequence.Style do
      Result := Result + GetConsequenceStyleString(CS) + VV_SEP;
  end;


  function GetChoiceString(AChoice : TPlayerChoice) : string;
  begin
    Result := GetRowString(AChoice.Row) + VV_SEP;
    Result := Result+ GetColorString(AChoice.Color) + VV_SEP;
  end;

  function GetPPointsString(APPoints : TPlayerPoints) : string;
  begin
    Result := IntToStr(APPoints.A)+VV_SEP+IntToStr(APPoints.B);
  end;

  function GetStatusString(AStatus : TGamePlayerStatus): string;
  begin
    case AStatus of
      gpsWaiting: Result := 'esperando';
      gpsPlayed: Result := 'jogou';
      gpsPlaying: Result := 'jogando';
    end;
  end;

begin
  LWriter := TRegData.Create(nil,AFilename);
  LIniFile:= TCIniFile.Create(LWriter.FileName);
  LWriter.Free;

  LIniFile.WriteString(SEC_EXPERIMENT,KEY_RESEARCHER,AExperiment.Researcher);

  with LIniFile do
    for i := 0 to AExperiment.ConditionsCount do
      begin
        LC := SEC_CONDITION+IntToStr(i+1);
        with AExperiment.Condition[i] do
          begin
            WriteString(LC,KEY_ENDCRITERIA,GetEndCriteriaString(EndCriterium));
            WriteString(LC, KEY_POINTS_COUNT,GetPointsString(Points.Count));
            WriteString(LC, KEY_POINTS_ONSTART,GetPointsString(Points.OnStart));
            WriteInteger(LC, KEY_TURN_COUNT,Turn.Count);
            WriteInteger(LC, KEY_TURN_VALUE,Turn.Value);
            WriteBool(LC, KEY_TURN_RANDOM,Turn.Random);
            WriteInteger(LC, KEY_CYCLES_COUNT,Cycles.Count);
            WriteInteger(LC, KEY_CYCLES_VALUE,Cycles.Value);
            WriteInteger(LC, KEY_CYCLES_GEN,Cycles.Generation);
            //WriteBool(LC, KEY_PROMPT_VALUE,Prompt.Value);
            WriteString(LC, KEY_PROMPT_MESSAGE, Prompt.PromptMessage);
            WriteString(LC, KEY_PROMPT_STYLE, GetPromptStyleString(Prompt.PromptStyle));

            for j := 0 to High(Contingencies) do
              begin
                if Contingencies[j].Meta then
                  LCK := KEY_METACONTINGENCY+IntToStr(j+1)
                else
                  LCK := KEY_CONTINGENCY+IntToStr(j+1);

                with Contingencies[j] do
                  begin
                    WriteString(LC,LCK+KEY_CONSEQUE,GetConsequenceString(Consequence));
                    WriteString(LC,LCK+KEY_RESPONSE,GetResponseString(Criteria));
                  end;
              end;
          end;
      end;

  with LIniFile do
    for i:=0 to AExperiment.PlayersCount do
      with AExperiment.Player[i] do
        begin
          LC := SEC_PLAYER+IntToStr(i+1);
          WriteInteger(LC,KEY_PLAYER_TURN,AExperiment.Player[i].Turn);
          WriteString(LC,KEY_PLAYER_CHOICE_CURRENT,GetChoiceString(AExperiment.Player[i].Choice.Current));
          WriteString(LC,KEY_PLAYER_CHOICE_LAST,GetChoiceString(AExperiment.Player[i].Choice.Last));
          WriteString(LC,KEY_PLAYER_ID,AExperiment.Player[i].ID);
          WriteString(LC,KEY_PLAYER_NICNAME,AExperiment.Player[i].Nicname);
          WriteString(LC,KEY_PLAYER_LOGIN,AExperiment.Player[i].Login);
          WriteString(LC,KEY_PLAYER_PASSWORD,AExperiment.Player[i].Password);
          WriteString(LC,KEY_PLAYER_POINTS,GetPPointsString(AExperiment.Player[i].Points));
          WriteString(LC,KEY_PLAYER_STATUS,GetStatusString(AExperiment.Player[i].Status));
          WriteString(LC,KEY_PLAYER_TEMP,AExperiment.Player[i].Data.Values[KEY_PLAYER_TEMP]);
        end;
end;


procedure TCIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
  KeyList: TStringList;
  I: Integer;
begin
  KeyList := TStringList.Create;
  //KeyList.Sorted := False;
  KeyList.CaseSensitive := False;
  KeyList.Duplicates := dupIgnore;
  try
    ReadSection(Section, KeyList);
    //showmessage(Keylist.Text);
    //Strings.BeginUpdate;
    //try
      for I := 0 to KeyList.Count - 1 do
        Strings.Add(KeyList[I] + '=' + ReadString(Section, KeyList[I], ''))
    //finally
    //  Strings.EndUpdate;
    //end;
  finally
    KeyList.Free;
  end;
end;

end.

