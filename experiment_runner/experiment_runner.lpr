{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
program experiment_runner;

{$mode objfpc}{$H+}

//{$DEFINE DEBUG}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces  // this includes the LCL widgetset
  {$IFDEF DEBUG}
  , Dialogs, FileUtil, LazFileUtils
    {$IFDEF LINUX}
    , BaseUnix
    {$ENDIF}
  {$ENDIF}
  , StrUtils, Forms, Classes, SysUtils
  , helpers
  , form_matrixgame, game_view
  ;


var
  ApplicationPath,
  InitParameter,
  F : string;

const
  PAdmin : array [0..4] of string = ('--admin','--adm','-admin','-adm', '-a');
  PPlayer : array [0..4] of string = ('--player','--play','-player','-play', '-p');
  PWatcher : array [0..4] of string = ('--watcher','--watch','-watcher','-watch', '-w');

{$R *.res}


{$IFDEF DEBUG}
  function CreateDebugFoldersForPlayers:Boolean;
  var
    i : integer;
  begin
    Result := True;
    for i := 0 to 2 do
      begin
        if Pos(('Participant'), Application.ExeName) > 0 then
          Break;
        F := ApplicationPath+'Participant'+IntToStr(i+1);
        // WriteLn(F);
        if ForceDirectoriesUTF8(F) then // ensure we have always the newer version for tests
          begin
            CopyFile(Application.ExeName,F+PathDelim+ApplicationName,[cffOverwriteFile]);
            {$IFDEF LINUX}
              FpChmod(F+PathDelim+ApplicationName,S_IRWXU);
            {$ENDIF}
          end
        else Result := False;
      end;
  end;
{$ENDIF}

  function GetZMQNetworkID(var AF:string):Boolean;
  var ID : TStringList;
  begin
    Result := True;
    ID := TStringList.Create;
    if FileExists(AF) then
      try
        ID.LoadFromFile(AF);
        F := ID[0];
      finally
        ID.Free;
      end
    else
      try
        ID.Append(Format('%12X-%12X',[Random($1000000000000), Random($1000000000000)]));
        ID.SaveToFile(F);
        F := ID[0];
      except
        on E: Exception do
          begin
            ID.Free;
            {$IFDEF DEBUG}
            ShowMessage(E.Message);
            {$ENDIF}
            Result := False;
          end;
      end;
  end;

begin
  Randomize;
  ApplicationPath := ExtractFilePath(Application.ExeName);
  PrintVersions(ApplicationPath);
  {$IFDEF DEBUG}
  if not CreateDebugFoldersForPlayers then Exit;
  {$ENDIF}
  Application.Initialize;
  F := ApplicationPath+PathDelim+'id';
  if not GetZMQNetworkID(F) then exit;
  Application.CreateForm(TFormMatrixGame, FormMatrixGame);
  InitParameter := '';
  if Paramcount > 0 then
    begin
      if AnsiMatchStr(lowercase(ParamStr(1)), PAdmin) then
        InitParameter := 'a';
      if AnsiMatchStr(lowercase(ParamStr(1)), PPlayer) then
        InitParameter := 'p';
      if AnsiMatchStr(lowercase(ParamStr(1)), PWatcher) then
        InitParameter := 'w';
    end;
  FormMatrixGame.SetID(F, InitParameter);
  Application.Run;
end.

