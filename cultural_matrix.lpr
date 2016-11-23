{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
program cultural_matrix;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}

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
  , StrUtils, Forms, Classes, sysutils
  , form_matrixgame, game_actors
  , zhelpers, form_chooseactor
  ;


var
  {$IFDEF DEBUG}
  I : integer;
  {$ENDIF}
  ID : TStringList;
  F : string;
const
  PAdmin : array [0..3] of string = ('--admin','--adm','-admin','-adm');
  PPlayer : array [0..3] of string = ('--player','--play','-player','-play');
  PWatcher : array [0..3] of string = ('--watcher','--watch','-watcher','-watch');

{$R *.res}

begin
  {$IFDEF DEBUG}
  for i:= 0 to 2 do
    begin
      if Pos((PathDelim+'P'+IntToStr(i+1)+PathDelim), Application.ExeName) > 0 then
        Break;
      F := ExtractFilePath(Application.ExeName)+'P'+IntToStr(i+1);
      WriteLn(F);
      if ForceDirectoriesUTF8(F) then // ensure we have always the newer version for tests
        begin
          CopyFile(Application.ExeName,F+PathDelim+ApplicationName,[cffOverwriteFile]);
          {$IFDEF LINUX}
            FpChmod(F+PathDelim+ApplicationName,S_IRWXU);
          {$ENDIF}
        end
      else Exit;
    end;
  {$ENDIF}
  Application.Initialize;
  F := ExtractFilePath(Application.ExeName)+PathDelim+'id';
  ID := TStringList.Create;
  if FileExists(F) then
    try
      ID.LoadFromFile(F);
      F := Copy(ID.Text,0,Length(ID.Text)-2);
    finally
      ID.Free;
    end
  else
    try
      ID.Text := s_random(32);
      ID.SaveToFile(F);
      F := Copy(ID.Text,0,Length(ID.Text)-2);
    except
      on E: Exception do
        begin
          ID.Free;
          {$IFDEF DEBUG}
          ShowMessage(E.Message);
          {$ENDIF}
          Exit;
        end;
    end;
    Application.CreateForm(TFormMatrixGame, FormMatrixGame);

  FormMatrixGame.SetID(F);
  if Paramcount > 0 then
    begin
      if AnsiMatchStr(lowercase(ParamStr(0)), PAdmin) then
        FormMatrixGame.SetGameActor(gaAdmin);
      if AnsiMatchStr(lowercase(ParamStr(0)), PPlayer) then
        FormMatrixGame.SetGameActor(gaPlayer);
      if AnsiMatchStr(lowercase(ParamStr(0)), PWatcher) then
        FormMatrixGame.SetGameActor(gaWatcher);
    end;
  Application.Run;
end.

