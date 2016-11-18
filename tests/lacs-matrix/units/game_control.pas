unit game_control;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , game_zmq_actors
  , game_experiment
  ;

type

  { TGameControl }

  TGameControl = class(TComponent)
  private
    FZMQActor : TZMQActor;
    FExperiment : TExperiment;
  public
    constructor Create(AZMQActor : TZMQActor; AOwner : TComponent); overload;
    procedure SendMessage(AMessage : array of UTF8string);
    procedure ReceiveMessage(AMessage : TStringList);
  end;

implementation

{ TGameControl }

constructor TGameControl.Create(AZMQActor: TZMQActor; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FZMQActor := AZMQActor;
end;

procedure TGameControl.SendMessage(AMessage: array of UTF8string);
begin

end;

procedure TGameControl.ReceiveMessage(AMessage: TStringList);
begin

end;

end.

