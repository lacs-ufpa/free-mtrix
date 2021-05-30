{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit game_slide;

{$mode objfpc}{$H+}

interface

uses Controls, Classes, SysUtils, StdCtrls, Graphics, IpHtml;

type

  { TGameSlide }

  TGameSlide = class(TComponent)
    procedure DoNextClick(Sender : TObject);
  private
    FEndButton : TButton;
    FHTMLMessage : string;
    FMessage : TIpHtmlPanel;
    FOnNextClick : TNotifyEvent;
    function GetHTMLDocumentType(AMessage : string) : string;
    function GetVisible : Boolean;
    procedure SetOnNextClick(AValue : TNotifyEvent);
    procedure SetVisible(AValue : Boolean);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy;override;
    procedure ChangeEndButton;
    procedure LoadFromString(AString : string);
    property OnNextClick : TNotifyEvent read FOnNextClick write SetOnNextClick;
    property Visible : Boolean read GetVisible write SetVisible;
  end;

  { TGameSlideList }

  TGameSlideList = class(TStringList)
    private
      FCurrentItemIndex : integer;
      FCurrentSlide : TGameSlide;
      FOnEndSlides : TNotifyEvent;
      function CurrentItemExists : Boolean;
      procedure DoNextItem(Sender : TObject);
      function NextItem: string;
      function EndOfList : Boolean;
      function LastItem: Boolean;
      procedure SetCurrentSlide(AValue : TGameSlide);
      procedure SetOnEndSlides(AValue : TNotifyEvent);
    public
      constructor Create; reintroduce;
      procedure Play;
      property CurrentSlide : TGameSlide read FCurrentSlide write SetCurrentSlide;
      property OnEndSlides : TNotifyEvent read FOnEndSlides write SetOnEndSlides;
  end;

implementation

uses Forms;

{ TGameSlide }

procedure TGameSlide.DoNextClick(Sender : TObject);
begin
  if Assigned(OnNextClick) then begin
    OnNextClick(Sender);
  end;
end;

function TGameSlide.GetHTMLDocumentType(AMessage : string) : string;
begin
  Result :=
  '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"'+
  '<html>' +
    '<head>' +
      '<meta http-equiv="content-type" content="text/html; charset=UTF-8">' +
      '<style type="text/css">' +
        '.container {text-align: left;}' +
      '</style>' +
    '</head>' +
    '<body><div class=container>'+AMessage+'</div></body>' +
  '</html>';
end;

function TGameSlide.GetVisible : Boolean;
begin
  Result := FMessage.Visible;
end;

procedure TGameSlide.SetOnNextClick(AValue : TNotifyEvent);
begin
  if FOnNextClick = AValue then Exit;
  FOnNextClick := AValue;
end;

procedure TGameSlide.SetVisible(AValue : Boolean);
begin
  if FMessage.Visible = AValue then Exit;
  FMessage.Visible := AValue;
  FEndButton.Visible := AValue;
end;

constructor TGameSlide.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FMessage := TIpHtmlPanel.Create(AOwner);
  with FMessage do begin
    Anchors := [akLeft,akTop];
    Height := (Screen.Height div 4) * 3;
    Width := (Screen.Width div 8) * 6;
    Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
    AllowTextSelect := False;
    FixedTypeface := 'Times New Roman';
    DefaultTypeFace := 'default';
    DefaultFontSize := 20;
    FlagErrors := False;
    ShowHints := False;
    WantTabs := False;
    Parent := TCustomControl(AOwner);
  end;

  FEndButton := TButton.Create(Self);
  with FEndButton do begin
    Caption := 'Continue';
    AutoSize := False;
    Font.Name:='Times New Roman';
    Font.Size := 30;
    Width := (FMessage.Width div 4);
    Height := 50;
    OnClick := @DoNextClick;
    Anchors := [akTop, akRight];
    AnchorSideTop.Control := FMessage;
    AnchorSideTop.Side := asrBottom;
    AnchorSideRight.Control := FMessage;
    AnchorSideRight.Side := asrBottom;
    Parent := TCustomControl(AOwner);
  end;
end;

destructor TGameSlide.Destroy;
begin
  inherited Destroy;
end;

procedure TGameSlide.ChangeEndButton;
begin
  FEndButton.Caption := 'Close';
end;

procedure TGameSlide.LoadFromString(AString : string);
begin
  FHTMLMessage := AString;
  FMessage.SetHtmlFromStr(GetHTMLDocumentType(FHTMLMessage));
end;

{ TGameSlideList }

constructor TGameSlideList.Create;
begin
  inherited Create;
  FCurrentItemIndex := 0;
end;

procedure TGameSlideList.Play;
begin
  CurrentSlide.Visible := True;
  CurrentSlide.LoadFromString(NextItem);
end;

function TGameSlideList.NextItem: string;
begin
  if CurrentItemExists then begin
    Result := Strings[FCurrentItemIndex];
    Inc(FCurrentItemIndex);
  end else begin;
    Result := '';
  end;
end;

function TGameSlideList.EndOfList : Boolean;
begin
  Result := FCurrentItemIndex >= Count;
end;

function TGameSlideList.LastItem : Boolean;
begin
  Result := FCurrentItemIndex = (Count - 1);
end;

procedure TGameSlideList.SetCurrentSlide(AValue : TGameSlide);
begin
  if FCurrentSlide = AValue then Exit;
  FCurrentSlide := AValue;
  FCurrentSlide.OnNextClick := @DoNextItem;
end;

procedure TGameSlideList.SetOnEndSlides(AValue : TNotifyEvent);
begin
  if FOnEndSlides = AValue then Exit;
  FOnEndSlides := AValue;
end;

procedure TGameSlideList.DoNextItem(Sender : TObject);
begin
  if Assigned(CurrentSlide) then begin
    if EndOfList then begin
      if Assigned(OnEndSlides) then begin
        OnEndSlides(Sender);
      end;
    end else begin;
      if LastItem then begin
        CurrentSlide.ChangeEndButton;
        CurrentSlide.LoadFromString(NextItem);
      end else begin
        CurrentSlide.LoadFromString(NextItem);
      end;
    end;
  end;
end;

function TGameSlideList.CurrentItemExists : Boolean;
begin
  Result := FCurrentItemIndex < Count;
end;

end.
