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
  Classes, SysUtils
  , fileinfo         // reads exe resources as long as you register the appropriate units
  {$IFDEF WINDOWS}
  , winpeimagereader // need this for reading exe info
  {$ENDIF}

  {$IFDEF LINUX}
  , elfreader        // needed for reading ELF executables
  {$ENDIF}

  {$IFDEF DARWIN}
  , machoreader      // needed for reading MACH-O executables}
  {$ENDIF}

  ;

procedure PrintVersions(AAplicationPath:string);
function CreateDebugFoldersForPlayers:Boolean;

type
  TGlobalContainer = record
    AppPath : string;
    MediaPath : string;
    CachePath : string;
    MediaRoot : string;
  end;

var
  Global : TGlobalContainer;

implementation

uses FileUtil, LazFileUtils, StrUtils, Forms
    {$IFDEF LINUX}
    , BaseUnix
    {$ENDIF}
    , zmq
    ;

function CreateDebugFoldersForPlayers:Boolean;
var
  i : integer;
  F : string;
  zmq : string = 'libzmq.dll';
begin
  Result := True;
  for i := 0 to 2 do
    begin
      if Pos(('Participant'), Application.ExeName) > 0 then
        Break;
      F := ExtractFilePath(Application.ExeName)+'Participant'+IntToStr(i+1)+PathDelim;
      if ForceDirectoriesUTF8(F) then // ensure we have always the newer version for tests
        begin
          {$IFDEF WINDOWS}
          CopyFile(Application.ExeName,F+ApplicationName+'.exe',[cffOverwriteFile]);
          CopyFile(ExtractFilePath(Application.ExeName)+zmq,F+zmq,[cffOverwriteFile]);
          {$ENDIF}

          {$IFDEF LINUX}
            CopyFile(Application.ExeName,F+PathDelim+ApplicationName,[cffOverwriteFile]);
            FpChmod(F+PathDelim+ApplicationName,S_IRWXU);
          {$ENDIF}
        end
      else Result := False;
    end;
end;

procedure PrintVersions(AAplicationPath : string);
var
  LPatch, LMinor, LMajor: Integer;
  LVERSION : string;

  {$IFDEF WINDOWS}
  S : TstringList;
  {$ENDIF}

  function FreeMtrixVersion : string;
  var
    FileVerInfo: TFileVersionInfo;
  begin
    FileVerInfo:=TFileVersionInfo.Create(nil);
    try
      FileVerInfo.FileName:=ParamStr(0);
      FileVerInfo.ReadFileInfo;
      Result := FileVerInfo.VersionStrings.Values['ProductName']+ #32;
      Result += FileVerInfo.VersionStrings.Values['FileVersion']+ ' - ';
      Result += FileVerInfo.VersionStrings.Values['FileDescription']+LineEnding;
      Result += FileVerInfo.VersionStrings.Values['LegalCopyright']+LineEnding;
      //Result += FileVerInfo.VersionStrings.Values['CompanyName']+LineEnding;
      //Result += 'Internal name      :'+FileVerInfo.VersionStrings.Values['InternalName']+LineEnding;
      //Result += 'Original filename  :'+FileVerInfo.VersionStrings.Values['OriginalFilename']+LineEnding;
      //Result += FileVerInfo.VersionStrings.Values['ProductVersion']+LineEnding;
    finally
      FileVerInfo.Free;
    end;
  end;
begin
  zmq_version(LMajor,LMinor,LPatch);
  LVERSION := FreeMtrixVersion + 'ZMQ v'+IntToStr(LMajor)+'.'+IntToStr(LMinor)+'.'+IntToStr(LPatch);
  {$IFDEF WINDOWS}
     S := TStringList.Create;
     S.Append(LVERSION);
     S.SaveToFile(AAplicationPath+PathDelim+'version_info.txt');
     S.Free;
  {$ELSE}
    WriteLn(LVERSION);
  {$ENDIF}
end;
end.

