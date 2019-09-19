{   
  Free-mtrix - Free cultural selection and social behavior experiments.   
  Copyright (C) 2016-2019 Carlos Rafael Fernandes Picanço.   
  Copyright (C) 2016-2019 Thais Maria Monteiro Guimarães.   
  Copyright (C) 2016-2019 Universidade Federal do Pará.

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


implementation

uses zmq;

procedure PrintVersions(AAplicationPath : string);
var
  LPatch, LMinor, LMajor: Integer;
  S : TStringList;
  LVERSION : string;

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
  try
    WriteLn(LVERSION);
  except
    on E : Exception do
      begin
         S := TStringList.Create;
         S.Append(LVERSION);
         S.SaveToFile(AAplicationPath+PathDelim+'version_info.txt');
         S.Free;
      end;
  end;
end;

end.

