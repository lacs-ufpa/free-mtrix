unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, ExtCtrls, StdCtrls, XMLPropStorage, IniFiles, Spin, PropertyStorage;

type

  { TFormDesigner }

  TFormDesigner = class(TForm)
    BtnAppendCond: TButton;
    BtnAppendContingency: TButton;
    BtnRemoveCond: TButton;
    BtnRemoveContingency: TButton;
    BtnReorderCond: TButton;
    BtnReorderContingency: TButton;
    CGGlobal: TCheckGroup;
    ChkColors: TCheckBox;
    ChkRows: TCheckBox;
    ChkCols: TCheckBox;
    ChkDots: TCheckBox;
    ChkCleanDots: TCheckBox;
    CheckBoxBroadcast: TCheckBox;
    CheckBoxIsMeta: TCheckBox;
    CheckBoxShouldAskQuestion: TCheckBox;
    CGQuestion: TCheckGroup;
    CBPointsType: TComboBox;
    ComboCurrentCondition: TComboBox;
    ComboCurrentContingency: TComboBox;
    EditMessPrefix: TEdit;
    EditMessSufix: TEdit;
    EditContingencyName: TEdit;
    EditQuestion: TEdit;
    EditConditionName: TEdit;
    EditExperimentName: TEdit;
    EditResearcherName: TEdit;
    GBContingencies: TGroupBox;
    GBContingencyColors: TGroupBox;
    GBExperimentAim: TGroupBox;
    GBConditions: TGroupBox;
    GBEndCriteria: TGroupBox;
    GBEndCriteriaLastCycles: TGroupBox;
    GBQuestion: TGroupBox;
    GBContingencyRows: TGroupBox;
    GBContingencyConsequence: TGroupBox;
    GBMatrix: TGroupBox;
    LabelPointsOnConditionBegin: TLabel;
    LabelC1: TLabel;
    LabelC2: TLabel;
    LabelC4: TLabel;
    LabelC5: TLabel;
    LabelC6: TLabel;
    LabelThen: TLabel;
    LabelOperator: TLabel;
    LabelContingencyName: TLabel;
    LabelEndCriteriaInt: TLabel;
    LabelEndCriteriaLastCycles: TLabel;
    LabelEndCriteriaAbsCycles: TLabel;
    LabelCyclesValue: TLabel;
    LabelIf: TLabel;
    LabelTurnValue: TLabel;
    LabelConditionName: TLabel;
    LabelExperimentName: TLabel;
    LabelResearcherName: TLabel;
    ListBoxConditions: TListBox;
    ListBoxContingencies: TListBox;
    MainMenu1: TMainMenu;
    MemoExperimentAim: TMemo;
    MenuItemFile: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    PanelConditionButtons: TPanel;
    PanelContingenciesButtons: TPanel;
    RGContingencyStyle: TRadioGroup;
    RGEndCriteriaStyle: TRadioGroup;
    RGPoints: TRadioGroup;
    SpinEditContingencyPoints: TSpinEdit;
    SpinEditEndCriteriaInterlockingPorcentage: TSpinEdit;
    SpinEditEndCriteriaLastCycles: TSpinEdit;
    SpinEditEndCriteriaAbsCycles: TSpinEdit;
    SpinEditCyclesValue: TSpinEdit;
    SpinEditOnConditionBegin: TSpinEdit;
    SpinEditTurnValue: TSpinEdit;
    TabSheetContingencies: TTabSheet;
    TabSheetConditions: TTabSheet;
    TabSheetExperiment: TTabSheet;
    XMLPropStorage: TXMLPropStorage;
    procedure BtnAppendCondClick(Sender: TObject);
    procedure BtnAppendContingencyClick(Sender: TObject);
    procedure BtnRemoveCondClick(Sender: TObject);
    procedure CGQuestionItemClick(Sender: TObject; Index: integer);
    procedure CheckBoxBroadcastChange(Sender: TObject);
    procedure CheckBoxIsMetaChange(Sender: TObject);
    procedure CheckBoxShouldAskQuestionChange(Sender: TObject);
    procedure ChkCleanDotsChange(Sender: TObject);
    procedure ChkDotsChange(Sender: TObject);
    procedure ComboCurrentConditionChange(Sender: TObject);
    procedure EditConditionNameChange(Sender: TObject);
    procedure EditConditionNameEditingDone(Sender: TObject);
    procedure EditQuestionChange(Sender: TObject);
    procedure EditQuestionEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure RGContingencyStyleClick(Sender: TObject);
    procedure RGEndCriteriaStyleClick(Sender: TObject);
    procedure RGPointsClick(Sender: TObject);
    procedure SpinEditCyclesValueEditingDone(Sender: TObject);
    procedure SpinEditEndCriteriaAbsCyclesEditingDone(Sender: TObject);
    procedure SpinEditEndCriteriaInterlockingEditingDone(
      Sender: TObject);
    procedure SpinEditTurnValueEditingDone(Sender: TObject);
    procedure XMLPropStorageRestoreProperties(Sender: TObject);
    procedure XMLPropStorageSavingProperties(Sender: TObject);
    procedure XMLPropStorageStoredValuesFileNameRestore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure XMLPropStorageStoredValuesFileNameSave(Sender: TStoredValue;
      var Value: TStoredType);
  private
    FExperiment: TIniFile;
    procedure LoadExperiment;
    procedure LoadSectionExperiment;
    procedure LoadSectionCondition(LS: string);
    procedure LoadContingency(LS, LC: string);
    procedure SaveExperiment;
    procedure SaveSectionExperiment;
    procedure SaveSectionCondition(LS: string);
    procedure SaveContingency(LS, LC: string);
  private
    function GetLC(IsMeta:Boolean):string;
    function GetConsequenceStyle: string;
    function GetContingencyCriteria: string;
    function GetEndCriteriaPorcentage: string;
    function GetEndCriteriaStyleFromRGEndCriteriaStyle: string;
    function GetMatrixTypeStringFromCGMatrix: string;
    function GetPromptQuestionStringFromCGQuestion: string;
    procedure SetCGMatrix(AMatrixType: string);
    procedure SetCGQuestion(AQuestionStyle: string);
    procedure SetRGEndCriteriaStyle(AStyle: string);
    procedure SetContingencyCriteria(S: string);
  public
    { public declarations }
  end;

var
  FormDesigner: TFormDesigner;

implementation

uses game_resources, game_actors, string_methods, strutils;

{$R *.lfm}

{ TFormDesigner }

procedure TFormDesigner.MenuItemOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    if FExperiment.FileName = OpenDialog.FileName then
      Exit;
    FExperiment.Free;
    FExperiment := TIniFile.Create(OpenDialog.FileName);
    XMLPropStorage.StoredValue['FileName'] := FExperiment.FileName;
  end;
end;

procedure TFormDesigner.RGContingencyStyleClick(Sender: TObject);
var
  LVisible: boolean;
begin
  LVisible := True;
  case TRadioGroup(Sender).ItemIndex of
    0:
    begin
      LabelIf.Visible := not LVisible;
      LabelThen.Visible := not LVisible;
      LabelOperator.Visible := not LVisible;
      GBContingencyRows.Visible := not LVisible;
      GBContingencyColors.Visible := not LVisible;
      GBContingencyConsequence.Visible := not LVisible;
    end;

    1:
    begin
      LabelIf.Visible := LVisible;
      LabelThen.Visible := LVisible;
      LabelOperator.Visible := not LVisible;
      GBContingencyRows.Visible := LVisible;
      GBContingencyColors.Visible := not LVisible;
      GBContingencyConsequence.Visible := LVisible;
    end;

    2:
    begin
      LabelIf.Visible := LVisible;
      LabelThen.Visible := LVisible;
      LabelOperator.Visible := not LVisible;
      GBContingencyRows.Visible := not LVisible;
      GBContingencyColors.Visible := LVisible;
      GBContingencyConsequence.Visible := LVisible;
    end;
    3:
    begin
      LabelIf.Visible := LVisible;
      LabelThen.Visible := LVisible;
      LabelOperator.Caption := 'E';
      LabelOperator.Visible := LVisible;
      GBContingencyRows.Visible := LVisible;
      GBContingencyColors.Visible := LVisible;
      GBContingencyConsequence.Visible := LVisible;
    end;
    4:
    begin
      LabelIf.Visible := LVisible;
      LabelThen.Visible := LVisible;
      LabelOperator.Caption := 'OU';
      LabelOperator.Visible := LVisible;
      GBContingencyRows.Visible := LVisible;
      GBContingencyColors.Visible := LVisible;
      GBContingencyConsequence.Visible := LVisible;
    end;
  end;
  CheckBoxIsMetaChange(CheckBoxIsMeta);
end;

procedure TFormDesigner.RGEndCriteriaStyleClick(Sender: TObject);
var
  LS: String;
begin
  case TRadioGroup(Sender).ItemIndex of
    0:
    begin
      LabelEndCriteriaAbsCycles.Visible := True;
      SpinEditEndCriteriaAbsCycles.Visible := True;
      GBEndCriteriaLastCycles.Visible := False;

    end;
    1:
    begin
      LabelEndCriteriaAbsCycles.Visible := False;
      SpinEditEndCriteriaAbsCycles.Visible := False;
      GBEndCriteriaLastCycles.Visible := True;
    end;
    2:
    begin
      LabelEndCriteriaAbsCycles.Visible := True;
      SpinEditEndCriteriaAbsCycles.Visible := True;
      GBEndCriteriaLastCycles.Visible := True;
    end;
  end;

  with FExperiment do
    if ComboCurrentCondition.ItemIndex <> -1 then
      begin
        LS := SEC_CONDITION+IntToStr(ComboCurrentCondition.ItemIndex+1);
        WriteString(LS, KEY_ENDCRITERIA, GetEndCriteriaStyleFromRGEndCriteriaStyle);
      end;
end;

procedure TFormDesigner.RGPointsClick(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0:
    begin
      CBPointsType.Items.Clear;
      CBPointsType.Items.Append('Individual A');
      CBPointsType.Items.Append('Individual B');
      CBPointsType.Items.Append('Para o Grupo');
    end;

    1:
    begin
      CBPointsType.Items.Clear;
      CBPointsType.Items.Append('Individual');
      CBPointsType.Items.Append('Para o Grupo');
    end;
  end;
end;

procedure TFormDesigner.SpinEditCyclesValueEditingDone(Sender: TObject);
var
  LS: string;
begin
  with FExperiment do
    if ComboCurrentCondition.ItemIndex <> -1 then
      begin
        LS := SEC_CONDITION+IntToStr(ComboCurrentCondition.ItemIndex+1);
        WriteInteger(LS, KEY_CYCLES_VALUE, SpinEditCyclesValue.Value);
      end;
end;

procedure TFormDesigner.SpinEditEndCriteriaAbsCyclesEditingDone(Sender: TObject
  );
var
  LS: String;
begin
  with FExperiment do
    if ComboCurrentCondition.ItemIndex <> -1 then
      begin
        LS := SEC_CONDITION+IntToStr(ComboCurrentCondition.ItemIndex+1);
        WriteInteger(LS, KEY_ENDCRITERIA_CYCLES, SpinEditEndCriteriaAbsCycles.Value);
      end;
end;

procedure TFormDesigner.SpinEditEndCriteriaInterlockingEditingDone(
  Sender: TObject);
var
  LS: string;
begin
  with FExperiment do
    if ComboCurrentCondition.ItemIndex <> -1 then
      begin
        LS := SEC_CONDITION+IntToStr(ComboCurrentCondition.ItemIndex+1);
        WriteString(LS, KEY_ENDCRITERIA_PORCENTAGE, GetEndCriteriaPorcentage);
      end;
end;

procedure TFormDesigner.SpinEditTurnValueEditingDone(Sender: TObject);
var
  LS: string;
begin
  with FExperiment do
    if ComboCurrentCondition.ItemIndex <> -1 then
      begin
        LS := SEC_CONDITION+IntToStr(ComboCurrentCondition.ItemIndex+1);
        WriteInteger(LS, KEY_TURN_VALUE, SpinEditTurnValue.Value);
      end;
end;

procedure TFormDesigner.XMLPropStorageRestoreProperties(Sender: TObject);
begin
  LoadExperiment;
  ListBoxConditions.Items.Text := ComboCurrentCondition.Items.Text;
  ListBoxContingencies.Items.Text := ComboCurrentContingency.Items.Text;
  RGPointsClick(RGPoints);
  RGEndCriteriaStyleClick(RGEndCriteriaStyle);
end;

procedure TFormDesigner.XMLPropStorageSavingProperties(Sender: TObject);
begin
  SaveExperiment;
end;


procedure TFormDesigner.XMLPropStorageStoredValuesFileNameRestore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  FExperiment := TIniFile.Create(Value);
end;


procedure TFormDesigner.XMLPropStorageStoredValuesFileNameSave(Sender: TStoredValue;
  var Value: TStoredType);
begin
  Value := FExperiment.FileName;
end;

procedure TFormDesigner.LoadExperiment;
var
  LS: string;
  LC: string;
begin
  LoadSectionExperiment;

  if ComboCurrentCondition.ItemIndex <> -1 then
  begin
    LS := SEC_CONDITION + IntToStr(ComboCurrentCondition.ItemIndex + 1);
    LoadSectionCondition(LS);
    if ComboCurrentContingency.ItemIndex <> -1 then
    begin
      LC := Delimited(1, ComboCurrentContingency.Items[ComboCurrentContingency.ItemIndex]);
      LoadContingency(LS, LC);
    end;
  end;
  //if NamesOnly then
  //  begin
  //    ListBoxConditions.Items.Append(LS+'|'+EditConditionName.Text);
  //    ComboCurrentCondition.Items.Append(LS+'|'+EditConditionName.Text);
  //    Exit;
  //  end;
end;

procedure TFormDesigner.SaveExperiment;
var
  LS, LC: string;
begin
  SaveSectionExperiment;
  if ComboCurrentCondition.ItemIndex <> -1 then
  begin
    LS := SEC_CONDITION + IntToStr(ComboCurrentCondition.ItemIndex + 1);
    SaveSectionCondition(LS);
    if ComboCurrentContingency.ItemIndex <> -1 then
    begin
      LC := Delimited(1, ComboCurrentContingency.Items[ComboCurrentContingency.ItemIndex]);
      SaveContingency(LS, LC);
    end;
  end;
end;

function TFormDesigner.GetMatrixTypeStringFromCGMatrix: string;
begin
  Result := '';
  if ChkColors.Checked then
    Result += 'CORES,';
  if ChkRows.Checked then
    Result += 'LINHAS,';
  if ChkCols.Checked then
    Result += 'COLUNAS,';
  if ChkDots.Checked then
    Result += 'CÍRCULOS PREENCHIDOS,';
  if ChkCleanDots.Checked then
    Result += 'CÍRCULOS VAZADOS,';
end;

function TFormDesigner.GetPromptQuestionStringFromCGQuestion: string;
var
  PS: TPromptStyle;
begin
  PS := [];
  if CGQuestion.Checked[0] then
    PS += [gsAll, gsYes, gsMetacontingency, gsContingency, gsRevertPoints];

  if CGQuestion.Checked[1] then
    PS += [gsBasA];

  Result := GetPromptStyleString(PS);
end;

procedure TFormDesigner.SetCGMatrix(AMatrixType: string);
begin
  if Pos('CORES', UpperCase(AMatrixType)) > 0 then
    ChkColors.Checked := True
  else
    ChkColors.Checked := False;

  if Pos('LINHAS', UpperCase(AMatrixType)) > 0 then
    ChkRows.Checked := True
  else
    ChkRows.Checked := False;

  if Pos('COLUNAS', UpperCase(AMatrixType)) > 0 then
    ChkCols.Checked := True
  else
    ChkCols.Checked := False;

  if Pos('CÍRCULOS PREENCHIDOS', UpperCase(AMatrixType)) > 0 then
    ChkDots.Checked := True
  else
    ChkDots.Checked := False;

  if Pos('CÍRCULOS VAZADOS', UpperCase(AMatrixType)) > 0 then
    ChkCleanDots.Checked := True
  else
    ChkCleanDots.Checked := False;
end;

procedure TFormDesigner.SetCGQuestion(AQuestionStyle: string);
var
  PS: TPromptStyle;
begin
  PS := GetPromptStyleFromString(AQuestionStyle);
  if (gsAll in PS) and (gsYes in PS) and (gsMetacontingency in PS) and
    (gsContingency in PS) and (gsRevertPoints in PS) then
    CGQuestion.Checked[0] := True
  else
    CGQuestion.Checked[0] := False;

  if (gsBasA) in PS then
    CGQuestion.Checked[1] := True
  else
    CGQuestion.Checked[1] := False;
end;

procedure TFormDesigner.SetRGEndCriteriaStyle(AStyle: string);
begin
  case UpperCase(AStyle) of
    'CICLOS': RGEndCriteriaStyle.ItemIndex := 0;
    'PORCENTAGEM': RGEndCriteriaStyle.ItemIndex := 1;
    'O QUE OCORRER PRIMEIRO': RGEndCriteriaStyle.ItemIndex := 2;
  end;
end;

procedure TFormDesigner.SetContingencyCriteria(S: string);
var
  C: TCriteria;

  procedure SetContingencyRows(GR: TGameRows);
  var
    i: integer;
  begin
    for i := 0 to GBContingencyRows.ComponentCount - 1 do
      case GBContingencyRows.Components[i].Name of
        'Chk1': TCheckBox(GBContingencyColors.Components[i]).Checked := grOne in GR;
        'Chk2': TCheckBox(GBContingencyColors.Components[i]).Checked := grTwo in GR;
        'Chk3': TCheckBox(GBContingencyColors.Components[i]).Checked := grThree in GR;
        'Chk4': TCheckBox(GBContingencyColors.Components[i]).Checked := grFour in GR;
        'Chk5': TCheckBox(GBContingencyColors.Components[i]).Checked := grFive in GR;
        'Chk6': TCheckBox(GBContingencyColors.Components[i]).Checked := grSix in GR;
        'Chk7': TCheckBox(GBContingencyColors.Components[i]).Checked := grSeven in GR;
        'Chk8': TCheckBox(GBContingencyColors.Components[i]).Checked := grEight in GR;
        'Chk9': TCheckBox(GBContingencyColors.Components[i]).Checked := grNine in GR;
        'Chk10': TCheckBox(GBContingencyColors.Components[i]).Checked := grTen in GR;
        'ChkEven': TCheckBox(GBContingencyColors.Components[i]).Checked := grEven in GR;
        'ChkOdd': TCheckBox(GBContingencyColors.Components[i]).Checked := grOdd in GR;
      end;
  end;

  procedure SetContingencyColors(GC: TGameColors);
  var
    i: integer;
  begin
    for i := 0 to GBContingencyColors.ComponentCount - 1 do
      case GBContingencyColors.Components[i].Name of
        'ChkEqual': TCheckBox(GBContingencyColors.Components[i]).Checked := gcEqual in GC;
        'ChkDiff': TCheckBox(GBContingencyColors.Components[i]).Checked := gcDiff in GC;
        'ChkNot': TCheckBox(GBContingencyColors.Components[i]).Checked := gcNot in GC;
        'ChkY': TCheckBox(GBContingencyColors.Components[i]).Checked := gcYellow in GC;
        'ChkR': TCheckBox(GBContingencyColors.Components[i]).Checked := gcRed in GC;
        'ChkM': TCheckBox(GBContingencyColors.Components[i]).Checked := gcMagenta in GC;
        'ChkB': TCheckBox(GBContingencyColors.Components[i]).Checked := gcBlue in GC;
        'ChkG': TCheckBox(GBContingencyColors.Components[i]).Checked := gcDiff in GC;
      end;
  end;

begin
  C := GetCriteriaFromString(S);
  case C.Style of
    gtNone: RGContingencyStyle.ItemIndex := 0;
    gtRowsOnly:
    begin
      RGContingencyStyle.ItemIndex := 1;
      SetContingencyRows(C.Rows);
    end;
    gtColorsOnly:
    begin
      RGContingencyStyle.ItemIndex := 2;
      SetContingencyColors(C.Colors);
    end;
    gtRowsAndColors:
    begin
      RGContingencyStyle.ItemIndex := 3;
      SetContingencyRows(C.Rows);
      SetContingencyColors(C.Colors);
    end;
    gtRowsOrColors:
    begin
      RGContingencyStyle.ItemIndex := 4;
      SetContingencyRows(C.Rows);
      SetContingencyColors(C.Colors);
    end;
  end;
end;

procedure TFormDesigner.SaveSectionExperiment;
begin
  with FExperiment do
  begin
    WriteString(SEC_EXPERIMENT, KEY_RESEARCHER, EditResearcherName.Text);
    WriteString(SEC_EXPERIMENT, KEY_NAME, EditExperimentName.Text);
    WriteString(SEC_EXPERIMENT, KEY_AIM, MemoExperimentAim.Text);
    WriteBool(SEC_EXPERIMENT, KEY_CHAT_HISTORY_FOR_NEW_PLAYERS, CGGlobal.Checked[0]);
    WriteBool(SEC_EXPERIMENT, KEY_GEN_PLAYER_AS_NEEDED, CGGlobal.Checked[1]);
    WriteBool(SEC_EXPERIMENT, KEY_RESEARCHER_CANPLAY, CGGlobal.Checked[2]);
    WriteBool(SEC_EXPERIMENT, KEY_RESEARCHER_CANCHAT, CGGlobal.Checked[3]);
    case RGPoints.ItemIndex of
      0: WriteBool(SEC_EXPERIMENT, KEY_POINTS_TYPE, True);
      1: WriteBool(SEC_EXPERIMENT, KEY_POINTS_TYPE, False);
    end;
    WriteString(SEC_EXPERIMENT, KEY_MATRIX_TYPE, GetMatrixTypeStringFromCGMatrix);
  end;
end;

procedure TFormDesigner.LoadSectionExperiment;
begin
  with FExperiment do
  begin
    EditResearcherName.Text := ReadString(SEC_EXPERIMENT, KEY_RESEARCHER, '');
    EditExperimentName.Text := ReadString(SEC_EXPERIMENT, KEY_NAME, '');
    MemoExperimentAim.Text := ReadString(SEC_EXPERIMENT, KEY_AIM, '');
    CGGlobal.Checked[0] :=
      ReadBool(SEC_EXPERIMENT, KEY_CHAT_HISTORY_FOR_NEW_PLAYERS, False);
    CGGlobal.Checked[1] := ReadBool(SEC_EXPERIMENT, KEY_GEN_PLAYER_AS_NEEDED, False);
    CGGlobal.Checked[2] := ReadBool(SEC_EXPERIMENT, KEY_RESEARCHER_CANPLAY, False);
    CGGlobal.Checked[3] := ReadBool(SEC_EXPERIMENT, KEY_RESEARCHER_CANCHAT, False);
    if ReadBool(SEC_EXPERIMENT, KEY_POINTS_TYPE, True) then
      RGPoints.ItemIndex := 0
    else
      RGPoints.ItemIndex := 1;
    SetCGMatrix(ReadString(SEC_EXPERIMENT, KEY_MATRIX_TYPE, 'CORES,LINHAS'));
  end;
end;

procedure TFormDesigner.SaveSectionCondition(LS: string);
begin
  with FExperiment do
  begin
    WriteString(LS, KEY_COND_NAME, EditConditionName.Text);
    WriteInteger(LS, KEY_TURN_VALUE, SpinEditTurnValue.Value);
    WriteInteger(LS, KEY_POINTS_ONSTART,SpinEditOnConditionBegin.Value);
    WriteInteger(LS, KEY_CYCLES_VALUE, SpinEditCyclesValue.Value);
    WriteString(LS, KEY_PROMPT_MESSAGE, EditQuestion.Text);
    WriteString(LS, KEY_PROMPT_STYLE, GetPromptQuestionStringFromCGQuestion);
    WriteString(LS, KEY_ENDCRITERIA, GetEndCriteriaStyleFromRGEndCriteriaStyle);
    WriteInteger(LS, KEY_ENDCRITERIA_CYCLES, SpinEditEndCriteriaAbsCycles.Value);
    WriteString(LS, KEY_ENDCRITERIA_PORCENTAGE, GetEndCriteriaPorcentage);
  end;
end;

procedure TFormDesigner.SaveContingency(LS, LC: string);
begin
  with FExperiment do
  begin
    WriteString(LS, LC + KEY_NAME, EditContingencyName.Text);
    WriteString(LS, LC + KEY_CRITERIA, GetContingencyCriteria);
    WriteString(LS, LC + KEY_CONSEQUE, GetConsequenceStyle);
    WriteString(LS, LC + KEY_CONSEQUE_MESSAGE_APPENDP,{todo: consequence sufix plural}'');
    WriteString(LS, LC + KEY_CONSEQUE_MESSAGE_APPENDS, EditMessSufix.Text);
    WriteString(LS, LC + KEY_CONSEQUE_MESSAGE_PREPEND, EditMessPrefix.Text);
  end;
end;

function TFormDesigner.GetLC(IsMeta: Boolean): string;
var
  LCount,
  i : integer;
  S : string;
  Extension : string;
  ContingencyKey : string;
begin
  if ComboCurrentContingency.Items.Count > 0 then
    for i := ComboCurrentContingency.Items.Count-1 downto 0 do
      begin
        S := ExtractDelimited(1,ComboCurrentContingency.Items[i],['|']);
        ContingencyKey := ExtractFileNameWithoutExt(S);

        if IsMeta then
          begin
            if ContingencyKey = KEY_METACONTINGENCY[16] then
              begin
                Result := KEY_METACONTINGENCY;
                Extension := ExtractFileExt(S);
                LCount := StrToInt(Extension);
                Inc(LCount);
                Result += IntToStr(LCount);
                Break;
              end;
          end
        else
          begin
            if ContingencyKey = KEY_CONTINGENCY[12] then
              begin
                Result := KEY_CONTINGENCY;
                Extension := ExtractFileExt(S);
                Lcount := StrToInt(Extension);
                Inc(LCount);
                Result += IntToStr(LCount);
                Break;
              end;
          end;
      end
  else
    if IsMeta then
      Result := KEY_METACONTINGENCY+'1'
    else
      Result := KEY_CONTINGENCY+'1';
end;

function TFormDesigner.GetConsequenceStyle: string;
var
  CS : TConsequenceStyle;
begin
  CS := [gscMessage,gscPoints];
  case RGPoints.ItemIndex of
    0: { A & B }
      case CBPointsType.ItemIndex of
        0 {'Individual A'} : CS += [gscA];
        1 {'Individual B'} : CS += [gscB];
        2 {'Para o Grupo'} : CS += [gscG];
      end;
    1: { I }
      case CBPointsType.ItemIndex of
        0 {'Individual'} : CS += [gscI];
        1 {'Para o Grupo'} : CS += [gscG];
      end;
  end;

  case CheckBoxBroadcast.State of
    cbChecked : CS += [gscBroadcastMessage];
    cbUnchecked: { do nothing };
    cbGrayed: CS -= [gscMessage];
  end;

  Result := GetConsequenceStylesString(CS);
end;

function TFormDesigner.GetContingencyCriteria: string;
begin

end;

procedure TFormDesigner.LoadSectionCondition(LS: string);
begin
  with FExperiment do
  begin
    EditConditionName.Text := ReadString(LS, KEY_COND_NAME, LS);
    SpinEditTurnValue.Value := ReadInteger(LS, KEY_TURN_VALUE, 2);
    SpinEditOnConditionBegin.Value := ReadInteger(LS, KEY_POINTS_ONSTART,0);
    SpinEditCyclesValue.Value := ReadInteger(LS, KEY_CYCLES_VALUE, 2);

    CheckBoxShouldAskQuestion.Checked := False;
    if ValueExists(LS, KEY_PROMPT_STYLE) and ValueExists(LS, KEY_PROMPT_MESSAGE) then
    begin
      EditQuestion.Text := ReadString(LS, KEY_PROMPT_MESSAGE, '');
      SetCGQuestion(ReadString(LS, KEY_PROMPT_STYLE, ''));
      if (EditQuestion.Text <> '') or (ReadString(LS, KEY_PROMPT_STYLE, '') <> '') then
        CheckBoxShouldAskQuestion.Checked := True;
    end;

    SetRGEndCriteriaStyle(ReadString(LS, KEY_ENDCRITERIA, 'O QUE OCORRER PRIMEIRO'));
    SpinEditEndCriteriaAbsCycles.Value := ReadInteger(LS, KEY_ENDCRITERIA_CYCLES, 20);
    SpinEditEndCriteriaLastCycles.Value :=
      GetEndCriteriaLastCyclesFromString(ReadString(LS, KEY_ENDCRITERIA_PORCENTAGE, '80,20'));
    SpinEditEndCriteriaInterlockingPorcentage.Value :=
      GetEndCriteriaPorcentageFromString(ReadString(LS, KEY_ENDCRITERIA_PORCENTAGE, '80,20'));
  end;
end;

procedure TFormDesigner.LoadContingency(LS, LC: string);
begin
  if Pos('Metacontingency', LC) > 0 then
    CheckBoxIsMeta.Checked := True
  else
    CheckBoxIsMeta.Checked := False;

  with FExperiment do
    if ValueExists(LS, LC + KEY_CONSEQUE) and ValueExists(LS, LC + KEY_CRITERIA) then
    begin
      EditContingencyName.Text := ReadString(LS, LC + KEY_NAME, '');
      SetContingencyCriteria(ReadString(LS, LC + KEY_CRITERIA, ''));
      ReadString(LS, LC + KEY_CONSEQUE, '');
    end;
end;

function TFormDesigner.GetEndCriteriaPorcentage: string;
begin
  Result := IntToStr(SpinEditEndCriteriaInterlockingPorcentage.Value) + ',' +
    IntToStr(SpinEditEndCriteriaLastCycles.Value);
end;

function TFormDesigner.GetEndCriteriaStyleFromRGEndCriteriaStyle: string;
begin
  case RGEndCriteriaStyle.ItemIndex of
    0: Result := 'CICLOS';
    1: Result := 'PORCENTAGEM';
    2: Result := 'O QUE OCORRER PRIMEIRO';
  end;
end;

procedure TFormDesigner.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormDesigner.FormCreate(Sender: TObject);
begin
  //IPSExperiment.IniSection := SEC_EXPERIMENT;
  //IPSConditions.IniSection := SEC_CONDITION;
  //IPSPlayers.IniSection := SEC_PLAYER;

end;

procedure TFormDesigner.FormDestroy(Sender: TObject);
begin
  FExperiment.Free;
end;

procedure TFormDesigner.CheckBoxShouldAskQuestionChange(Sender: TObject);
var
  LS: String;
begin
  if TCheckBox(Sender).Checked then
    TCheckBox(Sender).Caption := 'Sim'
  else
    begin
      TCheckBox(Sender).Caption := 'Não';
      with FExperiment do
        if ComboCurrentCondition.ItemIndex <> -1 then
          begin
            LS := SEC_CONDITION+IntToStr(ComboCurrentCondition.ItemIndex+1);
            WriteString(LS, KEY_PROMPT_MESSAGE, '');
            WriteString(LS, KEY_PROMPT_STYLE, '');
          end;
    end;

  EditQuestion.Visible := CheckBoxShouldAskQuestion.Checked;
  CGQuestion.Visible := CheckBoxShouldAskQuestion.Checked;
end;

procedure TFormDesigner.ChkCleanDotsChange(Sender: TObject);
begin
  if ChkCleanDots.Checked then
    ChkDots.Checked := not ChkCleanDots.Checked;
end;

procedure TFormDesigner.ChkDotsChange(Sender: TObject);
begin
  if ChkDots.Checked then
    ChkCleanDots.Checked := not ChkDots.Checked;
end;

procedure TFormDesigner.ComboCurrentConditionChange(Sender: TObject);
begin
  LoadSectionCondition(SEC_CONDITION + IntToStr(ComboCurrentCondition.ItemIndex + 1));
end;

procedure TFormDesigner.EditConditionNameChange(Sender: TObject);
begin

end;

procedure TFormDesigner.EditConditionNameEditingDone(Sender: TObject);
var
  LS: string;
begin
  with FExperiment do
    if ComboCurrentCondition.ItemIndex <> -1 then
      begin
        LS := SEC_CONDITION+IntToStr(ComboCurrentCondition.ItemIndex+1);
        WriteString(LS, KEY_COND_NAME, EditConditionName.Text);
        ComboCurrentCondition.Items[ComboCurrentCondition.ItemIndex] :=
          SEC_CONDITION + IntToStr(ComboCurrentCondition.ItemIndex + 1) + '|' + EditConditionName.Text;
        ListBoxConditions.Items.Text := ComboCurrentCondition.Items.Text;
      end;
end;

procedure TFormDesigner.EditQuestionChange(Sender: TObject);
begin

end;

procedure TFormDesigner.EditQuestionEditingDone(Sender: TObject);
var
  LS: string;
begin
  with FExperiment do
    if ComboCurrentCondition.ItemIndex <> -1 then
      begin
        LS := SEC_CONDITION+IntToStr(ComboCurrentCondition.ItemIndex+1);
        WriteString(LS, KEY_PROMPT_MESSAGE, EditQuestion.Text);
      end;
end;

procedure TFormDesigner.CheckBoxBroadcastChange(Sender: TObject);
begin
  case TCheckBox(Sender).State of
    cbChecked: TCheckBox(Sender).Caption := 'a todos';
    cbUnchecked: TCheckBox(Sender).Caption := 'ao participante';
    cbGrayed: TCheckBox(Sender).Caption := 'a ninguém';
  end;
end;

procedure TFormDesigner.BtnAppendCondClick(Sender: TObject);
var
  i: integer;
begin
  i := ComboCurrentCondition.Items.Add('');
  ComboCurrentCondition.Items[i] :=
    SEC_CONDITION + IntToStr(i + 1) + '|' + EditConditionName.Text;
  ComboCurrentCondition.ItemIndex := i;
  SaveSectionCondition(SEC_CONDITION + IntToStr(i + 1));
  ListBoxConditions.Items.Text := ComboCurrentCondition.Items.Text;
end;

procedure TFormDesigner.BtnAppendContingencyClick(Sender: TObject);
var
  i: integer;
  LS, LC : string;

begin
  i := ComboCurrentContingency.Items.Add('');
  LS := ExtractDelimited(1,ComboCurrentCondition.Items[ComboCurrentCondition.ItemIndex],['|']);
  LC := GetLC(CheckBoxIsMeta.Checked);
  ComboCurrentContingency.Items[i] :=
    LC + '|' + EditContingencyName.Text;
  ComboCurrentContingency.ItemIndex := i;
  SaveContingency(LS,LC);
  ListBoxContingencies.Items.Text := ComboCurrentContingency.Items.Text;
end;

procedure TFormDesigner.BtnRemoveCondClick(Sender: TObject);
var
  i: integer;
  MustReorder: boolean;

  procedure Reorder(index : integer);
  var
    j, i: integer;
    Section: TStringList;
    KeyName,Line,SectionName: string;
  begin
    Section := TStringList.Create;
    with FExperiment do
      for i := index to ComboCurrentCondition.Items.Count - 1 do
      begin
        SectionName := ExtractDelimited(1, ComboCurrentCondition.Items[i], ['|']);
        ReadSectionValues(SectionName, Section);
        EraseSection(SectionName);
        SectionName := SEC_CONDITION + IntToStr(i + 1);
        for Line in Section do
          begin
            KeyName := Section.ExtractName(Line);
            WriteString(SectionName, KeyName, Section.Values[KeyName]);
          end;
        Section.Clear;
        ComboCurrentCondition.Items[i] :=
          SectionName + '|' + ExtractDelimited(2, ComboCurrentCondition.Items[i], ['|']);
      end;
    Section.Free;
  end;

begin
  i := ComboCurrentCondition.ItemIndex;
  MustReorder := i < ComboCurrentCondition.Items.Count - 1;
  ComboCurrentCondition.Items.Delete(i);
  if MustReorder then
    Reorder(i);
  case ComboCurrentCondition.Items.Count of
    0: {do nothing};
    1..MaxInt:
      if i = 1 then
        ComboCurrentCondition.ItemIndex := i
      else
        ComboCurrentCondition.ItemIndex := i -1;
  end;
  ListBoxConditions.Items.Text := ComboCurrentCondition.Items.Text;
end;

procedure TFormDesigner.CGQuestionItemClick(Sender: TObject; Index: integer);
var
  LS: String;
begin
  with FExperiment do
    if ComboCurrentCondition.ItemIndex <> -1 then
      begin
        LS := SEC_CONDITION+IntToStr(ComboCurrentCondition.ItemIndex+1);
        WriteString(LS, KEY_PROMPT_STYLE, GetPromptQuestionStringFromCGQuestion);
      end;
end;

procedure TFormDesigner.CheckBoxIsMetaChange(Sender: TObject);
var
  i: integer;
  CH: TCheckBox;

  procedure CreateChkBox(N, C: string; AOwner: TWinControl);
  begin
    CH := TCheckBox.Create(AOwner);
    CH.Name := N;
    CH.Caption := C;
    CH.Parent := AOwner;
  end;

begin
  while GBContingencyRows.ComponentCount > 0 do
    GBContingencyRows.Components[0].Free;

  while GBContingencyColors.ComponentCount > 0 do
    GBContingencyColors.Components[0].Free;

  if TCheckBox(Sender).Checked then
    begin
      CreateChkBox('ChkEven', 'PARES', GBContingencyRows);
      CreateChkBox('ChkOdd', 'IMPARES', GBContingencyRows);

      CreateChkBox('ChkNot', 'TUDO EXCETO', GBContingencyColors);
      CreateChkBox('ChkEqual', 'CORES IGUAIS', GBContingencyColors);
      CreateChkBox('ChkDiff', 'CORES DIFERENTES', GBContingencyColors);
      LabelIf.Caption := 'SE OS PARTICIPANTES ESCOLHEREM';
    end
  else
    begin
      for i := 0 to 9 do
        CreateChkBox('Chk' + IntToStr(i + 1), IntToStr(i + 1), GBContingencyRows);

      CreateChkBox('ChkEven', 'PAR', GBContingencyRows);
      CreateChkBox('ChkOdd', 'IMPAR', GBContingencyRows);

      CreateChkBox('ChkY', 'AMARELO', GBContingencyColors);
      CreateChkBox('ChkR', 'VERMELHO', GBContingencyColors);
      CreateChkBox('ChkM', 'ROXO', GBContingencyColors);
      CreateChkBox('ChkB', 'AZUL', GBContingencyColors);
      CreateChkBox('ChkG', 'VERDE', GBContingencyColors);
      LabelIf.Caption := 'SE O PARTICIPANTE ESCOLHER';
    end;
end;

end.
