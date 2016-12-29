unit helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function RandomString(ALength : Integer): Utf8String;

implementation

function RandomString( ALength: Integer ): Utf8String;
const
  Chars = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ';
var
  i: integer;
begin
  Result := '';
  for i := 1 to ALength do
    Result := Result + Chars[Random(Length(Chars)) + 1];
end;

initialization

  Randomize;

end.

