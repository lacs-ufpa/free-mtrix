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

uses LCLIntf, game_actors_point, game_resources, string_methods, regdata, zhelpers, strutils;

function LoadExperimentFromResource(var AExperiment: TExperiment): Boolean;
var
  C : TCondition;
  LConcequence : TConsequence;
  LCriteria1 : TCriteria = (
    Style:(gtRowsOnly);
    Rows:[grEven];
    Colors:[];
  );

  LCriteria2 : TCriteria = (
    Style:(gtRowsOnly);
    Rows:[grOdd];
    Colors:[];
  );

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
          SetLength(Contingencies, 2);
          LConcequence := TConsequence.Create(AExperiment,1,[gscPoints, gscB, gscMessage,gscBroadcastMessage],['NICNAME','queijo','queijos']);
          Contingencies[0] := TContingency.Create(AExperiment,LConcequence,LCriteria1,False);
          LConcequence := TConsequence.Create(AExperiment,3,[gscPoints, gscA, gscMessage,gscBroadcastMessage],['NICNAME','queijo','queijos']);
          Contingencies[1] := TContingency.Create(AExperiment,LConcequence,LCriteria2,False);

          ConditionName := SEC_CONDITION+IntToStr(1);
          Turn.Count:=0;
          Turn.Value:=2;
          Turn.Random:=False;
          Cycles.Count:=0;
          Cycles.Value:=4;
          Cycles.Generation:=0;
        end;
      AppendCondition(C);
    end;
end;

function LoadExperimentFromFile(var AExperiment: TExperiment; AFilename: string):Boolean;
var
  LIniFile : TCIniFile;

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
        AExperiment.Researcher := ReadString(SEC_EXPERIMENT, KEY_RESEARCHER,VAL_RESEARCHER);
        AExperiment.ExperimentName:=ReadString(SEC_EXPERIMENT, KEY_NAME,'');
        AExperiment.ExperimentAim:=ReadString(SEC_EXPERIMENT, KEY_AIM,'');
        AExperiment.GenPlayersAsNeeded:=ReadBool(SEC_EXPERIMENT, KEY_GEN_PLAYER_AS_NEEDED,True);
        AExperiment.CurrentCondition := ReadInteger(SEC_EXPERIMENT, KEY_CURRENT_CONDITION,0)-1; //zero based
      end;
  end;

  procedure ReadPlayers;
  var
    LS : string;
    i : integer;
    P : TPlayer;
  begin
    i := 0;
    LS := SEC_PLAYER+IntToStr(i+1);
    with LIniFile do
      while SectionExists(LS) do
        begin
          if i = 0 then
            i := AExperiment.AppendPlayer;
          with P do
            begin
              Turn := ReadInteger(LS,KEY_PLAYER_TURN,i);
              Choice := GetChoiceFromString(ReadString(LS,KEY_PLAYER_CHOICE_LAST,'0,NONE,'));
              ID := ReadString(LS,KEY_PLAYER_ID,s_random(20));
              Nicname := ReadString(LS,KEY_PLAYER_NICNAME,GenResourceName(i));
              Login := ReadString(LS,KEY_PLAYER_LOGIN,'jogador'+IntToStr(i+1));
              Password := ReadString(LS,KEY_PLAYER_PASSWORD,'1234');
              Points := GetPPointsFromString(ReadString(LS,KEY_PLAYER_POINTS,'0,0,'));
              Status := GetStatusFromString(ReadString(LS,KEY_PLAYER_STATUS,'esperando'));
              Data.Values[KEY_PLAYER_TEMP] := ReadString(LS,KEY_PLAYER_TEMP,'');
            end;
          AExperiment.Player[i] := P;
          i := AExperiment.AppendPlayer;
          LS := SEC_PLAYER+IntToStr(i+1);
        end;
  end;

  procedure ReadContingencies(ACondition:integer;IsMeta : Boolean);
  var
    i : integer;
    LS,LCK : string;
    LConsequence : TConsequence;
    LCriteria:TCriteria;

    function GetCriteriaFromString(S:string):TCriteria;
    var
      LS : string;
      i,
      LCount: integer;
    begin
      LS := ExtractDelimited(1,S,['|']);
      LCount := WordCount(LS,[#0,',']);
      Result.Rows := [];
      for i := 1 to LCount do
        Result.Rows += [GetRowFromString(ExtractDelimited(i,LS,[',']))];

       case ExtractDelimited(2,S,['|'])of
        'NONE':Result.Style:=gtNone;
        'CORES':Result.Style:=gtColorsOnly;
        'E':Result.Style:=gtRowsAndColors;
        'LINHAS':Result.Style:=gtRowsOnly;
        'OU':Result.Style:=gtRowsOrColors;
      end;

      LS := ExtractDelimited(3,S,['|']);
      LCount := WordCount(LS,[#0,',']);
      Result.Colors := [];
      for i := 1 to LCount do
        Result.Colors += [GetColorFromString(ExtractDelimited(i,LS,[',']))];
    end;

    procedure SetLCK(i:integer);
    begin
      if IsMeta then
        LCK := KEY_METACONTINGENCY+IntToStr(i+1)
      else
        LCK := KEY_CONTINGENCY+IntToStr(i+1);
    end;
  begin
    LS := SEC_CONDITION+IntToStr(ACondition+1);
    i := AExperiment.AppendContingency(ACondition);
    SetLCK(i);
    with LIniFile do
      while ValueExists(LS, LCK+KEY_CONSEQUE) and ValueExists(LS, LCK+KEY_CRITERIA)do
        begin
          LConsequence := TConsequence.Create(AExperiment,ReadString(LS,LCK+KEY_CONSEQUE,DEF_CONSEQUENCE));
          LCriteria := GetCriteriaFromString(ReadString(LS,LCK+KEY_CRITERIA,DEF_CRITERIA));
          AExperiment.Condition[ACondition].Contingencies[i] := TContingency.Create(AExperiment,LConsequence,LCriteria,IsMeta);
          i := AExperiment.AppendContingency(ACondition);
          SetLCK(i);
        end;
  end;

  procedure ReadConditions;
  var
    s1, LS : string;
    i : integer;
    C :TCondition;
  begin
    i := 0;
    LS := SEC_CONDITION+IntToStr(i+1);
    with LIniFile do
      while SectionExists(LS) do
        begin
          if i = 0 then
            i := AExperiment.AppendCondition;

          with C do
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

              ReadContingencies(i,True);
              ReadContingencies(i,False);

              // if no contingencies, return false...

              Prompt := TPrompt.Create(AExperiment,ReadString(LS,KEY_PROMPT_STYLE,'todos,sim,metacontingência,recuperar pontos,'));
              Prompt.PromptStyle:= GetPromptStyle(ReadString(LS,KEY_PROMPT_STYLE,'todos,sim,metacontingência,recuperar pontos,'));
              Prompt.PromptMessage := ReadString(LS,KEY_PROMPT_MESSAGE,DEF_PROMPTMESSAGE);

            end;
            AExperiment.Condition[i]:= C;
            i := AExperiment.AppendCondition;
            LS := SEC_CONDITION+IntToStr(i+1);
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
            AExperiment := TExperiment.Create(AExperiment.Owner);
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
                    WriteString(LC,LCK+KEY_CONSEQUE,Consequence.AsString);
                    WriteString(LC,LCK+KEY_CRITERIA,CriteriaString);
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
          WriteString(LC,KEY_PLAYER_CHOICE_LAST,GetChoiceString(AExperiment.Player[i].Choice));
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

