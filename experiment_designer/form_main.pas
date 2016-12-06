unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, ExtCtrls, StdCtrls, XMLPropStorage, IniFiles, Spin;

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
    CGMatrix: TCheckGroup;
    CheckBoxBroadcast: TCheckBox;
    CheckBoxIsMeta: TCheckBox;
    CheckBoxShouldAskQuestion: TCheckBox;
    CGQuestion: TCheckGroup;
    ComboBoxRows: TComboBox;
    ComboBoxColors: TComboBox;
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
    SpinEditTurnValue: TSpinEdit;
    TabSheetContingencies: TTabSheet;
    TabSheetConditions: TTabSheet;
    TabSheetExperiment: TTabSheet;
    XMLPropStorage: TXMLPropStorage;
    procedure CheckBoxBroadcastChange(Sender: TObject);
    procedure CheckBoxIsMetaChange(Sender: TObject);
    procedure CheckBoxShouldAskQuestionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure RGContingencyStyleClick(Sender: TObject);
    procedure RGEndCriteriaStyleClick(Sender: TObject);
    procedure RGPointsClick(Sender: TObject);
    procedure XMLPropStorageRestoreProperties(Sender: TObject);
    procedure XMLPropStorageSavingProperties(Sender: TObject);
  private
    FExperiment : TIniFile;
  public
    { public declarations }
  end;

var
  FormDesigner: TFormDesigner;

implementation

uses game_resources, game_file_methods;

{$R *.lfm}

{ TFormDesigner }


procedure TFormDesigner.MenuItemOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    begin
      if FExperiment.FileName = OpenDialog.FileName then Exit;
      FExperiment := TIniFile.Create(OpenDialog.FileName);
    end;
end;

procedure TFormDesigner.RGContingencyStyleClick(Sender: TObject);
var LVisible : Boolean;
begin
  LVisible := True;
  case TRadioGroup(Sender).ItemIndex of
    0:
      begin
        LabelIf.Visible:= not LVisible;
        LabelThen.Visible:= not LVisible;
        LabelOperator.Visible:= not LVisible;
        GBContingencyRows.Visible:= not LVisible;
        GBContingencyColors.Visible:= not LVisible;
        GBContingencyConsequence.Visible:= not LVisible;
      end;

    1:
      begin
        LabelIf.Visible:= LVisible;
        LabelThen.Visible:= LVisible;
        LabelOperator.Visible:= not LVisible;
        GBContingencyRows.Visible:= LVisible;
        GBContingencyColors.Visible:= not LVisible;
        GBContingencyConsequence.Visible:= LVisible;
      end;

    2:
      begin
        LabelIf.Visible:= LVisible;
        LabelThen.Visible:= LVisible;
        LabelOperator.Visible:= not LVisible;
        GBContingencyRows.Visible:= not LVisible;
        GBContingencyColors.Visible:= LVisible;
        GBContingencyConsequence.Visible:= LVisible;
      end;
    3:
      begin
        LabelIf.Visible:= LVisible;
        LabelThen.Visible:= LVisible;
        LabelOperator.Caption:='E';
        LabelOperator.Visible:= LVisible;
        GBContingencyRows.Visible:= LVisible;
        GBContingencyColors.Visible:= LVisible;
        GBContingencyConsequence.Visible:= LVisible;
      end;
    4:
      begin
        LabelIf.Visible:= LVisible;
        LabelThen.Visible:= LVisible;
        LabelOperator.Caption:='OU';
        LabelOperator.Visible:= LVisible;
        GBContingencyRows.Visible:= LVisible;
        GBContingencyColors.Visible:= LVisible;
        GBContingencyConsequence.Visible:= LVisible;
      end;
  end;
  CheckBoxIsMetaChange(CheckBoxIsMeta);
end;

procedure TFormDesigner.RGEndCriteriaStyleClick(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0:
      begin
        LabelEndCriteriaAbsCycles.Visible := True;
        SpinEditEndCriteriaAbsCycles.Visible:= True;
        GBEndCriteriaLastCycles.Visible:= False;

      end;
    1:
      begin
        LabelEndCriteriaAbsCycles.Visible := False;
        SpinEditEndCriteriaAbsCycles.Visible:= False;
        GBEndCriteriaLastCycles.Visible:= True;
      end;
    2:
      begin
        LabelEndCriteriaAbsCycles.Visible := True;
        SpinEditEndCriteriaAbsCycles.Visible:= True;
        GBEndCriteriaLastCycles.Visible:= True;
      end;
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

procedure TFormDesigner.XMLPropStorageRestoreProperties(Sender: TObject);
//var
//  i: Integer;
begin
  //for i := 0 to CGGlobal.Items.Count-1 do
  //  CGGlobal.Checked[i] := StrToBool(XMLPropStorage.StoredValue['CGGLobal.Checked'+IntToStr(i)]);
  //
  //for i := 0 to CGMatrix.Items.Count-1 do
  //  CGMatrix.Checked[i] := StrToBool(XMLPropStorage.StoredValue['CGMatrix.Checked'+IntToStr(i)]);
end;

procedure TFormDesigner.XMLPropStorageSavingProperties(Sender: TObject);
//var
//  i: Integer;
begin
  //for i := 0 to CGGlobal.Items.Count-1 do
  //  XMLPropStorage.StoredValue['CGGLobal.Checked'+IntToStr(i)] := BoolToStr(CGGLobal.Checked[i]);
  //
  //for i := 0 to CGMatrix.Items.Count-1 do
  //  XMLPropStorage.StoredValue['CGMatrix.Checked'+IntToStr(i)] := BoolToStr(CGMatrix.Checked[i]);
end;

procedure TFormDesigner.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormDesigner.FormCreate(Sender: TObject);
//var
//  LFileName: String;
begin
  //LFileName := 'default.txt';
  //IPSExperiment.IniFileName := LFileName;
  //IPSExperiment.IniSection := SEC_EXPERIMENT;
  //IPSConditions.IniFileName := LFileName;
  //IPSConditions.IniSection := SEC_CONDITION;
  //IPSPlayers.IniFileName := LFileName;
  //IPSPlayers.IniSection := SEC_PLAYER;
  RGPointsClick(RGPoints);
  RGEndCriteriaStyleClick(RGEndCriteriaStyle);
  FExperiment := TCIniFile.Create('default.txt');
  //FExperiment.FileName;
end;

procedure TFormDesigner.FormDestroy(Sender: TObject);
begin
  FExperiment.Destroy;
end;

procedure TFormDesigner.CheckBoxShouldAskQuestionChange(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
     TCheckBox(Sender).Caption := 'Sim'
  else
     TCheckBox(Sender).Caption := 'Não';

  EditQuestion.Visible := CheckBoxShouldAskQuestion.Checked;
  CGQuestion.Visible := CheckBoxShouldAskQuestion.Checked;
end;

procedure TFormDesigner.CheckBoxBroadcastChange(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
     TCheckBox(Sender).Caption := 'a todos'
  else
     TCheckBox(Sender).Caption := 'ao participante';

end;

procedure TFormDesigner.CheckBoxIsMetaChange(Sender: TObject);
var
  i: Integer;
begin
  if TCheckBox(Sender).Checked then
     begin
      ComboBoxRows.Items.Clear;
      ComboBoxRows.Items.Append('PARES');
      ComboBoxRows.Items.Append('IMPARES');

      ComboBoxColors.Items.Clear;
      ComboBoxColors.Items.Append('CORES IGUAIS');
      ComboBoxColors.Items.Append('CORES DIFERENTES');
      ComboBoxColors.Items.Append('TUDO EXCETO CORES IGUAIS');
      ComboBoxColors.Items.Append('TUDO EXCETO CORES DIFERENTES');
     end
  else
    begin
      ComboBoxRows.Items.Clear;
      for i:=0 to 9 do
        ComboBoxRows.Items.Append(IntToStr(i+1));

      ComboBoxRows.Items.Append('PAR');
      ComboBoxRows.Items.Append('IMPAR');

      ComboBoxColors.Items.Clear;
      ComboBoxColors.Items.Append('AMARELO');
      ComboBoxColors.Items.Append('VERMELHO');
      ComboBoxColors.Items.Append('ROXO');
      ComboBoxColors.Items.Append('AZUL');
      ComboBoxColors.Items.Append('VERDE');
    end
end;

end.

