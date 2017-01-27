{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function RandomString(ALength : Integer): Utf8String;
procedure PrintZmqVersion(AAplicationPath:string);


implementation

uses zmq;

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

procedure PrintZmqVersion(AAplicationPath : string);
var
  LPatch, LMinor, LMajor: Integer;
  S : TStringList;
begin
  LMajor := 0; LMinor := 0; LPatch := 0;
  zmq_version(LMajor,LMinor,LPatch);
  try
    WriteLn('ZMQVERSION:',LMajor,'.',LMinor,'.',LPatch);
  except
    on E : Exception do
      begin
         S := TStringList.Create;
         S.Append(IntToStr(LMajor)+IntToStr(LMinor)+IntToStr(LPatch));
         S.SaveToFile(AAplicationPath+PathDelim+'ZMQVERSION');
         S.Free;
      end;
  end;
end;

initialization

  Randomize;

end.

