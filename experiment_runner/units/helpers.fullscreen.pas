unit Helpers.Fullscreen;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms;

  procedure SetFullscreen(Form : TForm);

implementation

uses Controls;

procedure SetFullscreen(Form: TForm);
begin
  with Form do begin
    Position:=poDesigned;
    FormStyle:=fsNormal;
    {$IFDEF MSWINDOWS}
    BorderStyle:=bsNone;
    BoundsRect := Monitor.BoundsRect;
    {$ENDIF}
    {$IFDEF LINUX}
    WindowState:=wsFullScreen;
    {$ENDIF}
  end;
end;

end.

