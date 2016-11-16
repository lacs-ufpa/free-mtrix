unit game_actors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , game_zmq_actors
  ;
type

  { TActor }

  TActor = class(TComponent)
  private
    FZMQActor : TZMQActor;
  public
    constructor Create(AZMQActor : TZMQActor; AOwner : TComponent);
    procedure SendMessage(AMessage : array of UTF8string);
  end;

  TAdmin = record

  end;

  TChoice = record
    Row : ShortInt;
    Color : integer;
  end;

  TPlayer = record
    ID : UTF8string;
    Choice,
    ChoiceLast : TChoice;
  end;

  TWatcher = record

  end;


implementation

{ TActor }

constructor TActor.Create(AZMQActor: TZMQActor; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FZMQActor := AZMQActor;
end;

procedure TActor.SendMessage(AMessage: array of UTF8string);
begin

end;

end.

