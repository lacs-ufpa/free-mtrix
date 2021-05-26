{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
program experiment_runner;

{$mode objfpc}{$H+}

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
  , form_matrixgame;


var
  ApplicationPath : string;
  InitParameter : string = '';

const
  PAdmin : array [0..3] of string = ('--admin','--adm','-admin','-adm');
  PPlayer : array [0..3] of string = ('--player','--play','-player','-play');
  PWatcher : array [0..3] of string = ('--watcher','--watch','-watcher','-watch');

{$R *.res}

begin
  Randomize;
  ApplicationPath := ExtractFilePath(Application.ExeName);
  PrintVersions(ApplicationPath);
  {$IFDEF DEBUG}
  if not CreateDebugFoldersForPlayers then Exit;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TFormMatrixGame, FormMatrixGame);
  if Paramcount > 0 then
    begin
      if AnsiMatchStr(lowercase(ParamStr(1)), PAdmin) then
        InitParameter := 'a';
      if AnsiMatchStr(lowercase(ParamStr(1)), PPlayer) then
        InitParameter := 'p';
      if AnsiMatchStr(lowercase(ParamStr(1)), PWatcher) then
        InitParameter := 'w';
    end;
  FormMatrixGame.SetParameters(InitParameter);
  Application.Run;
end.

