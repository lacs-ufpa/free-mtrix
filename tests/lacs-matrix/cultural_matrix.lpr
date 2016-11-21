{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
program cultural_matrix;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces  // this includes the LCL widgetset
  , StrUtils, Forms, Classes, sysutils, Dialogs
  , form_matrixgame, form_chooseactor, game_actors
  , zhelpers
  ;


var
  ID : TStringList;
  F : string;
const
  PAdmin : array [0..3] of string = ('--admin','--adm','-admin','-adm');
  PPlayer : array [0..3] of string = ('--player','--play','-player','-play');
  PWatcher : array [0..3] of string = ('--watcher','--watch','-watcher','-watch');

{$R *.res}

begin
  //RequireDerivedFormResource := True;
  Application.Initialize;
  F := ExtractFilePath(Application.ExeName)+PathDelim+'id';
  ID := TStringList.Create;
  if FileExists(F) then
    try
      ID.LoadFromFile(F);
      F := ID.Text;
    finally
      ID.Free;
    end
  else
    try
      ID.Text := s_random(32);
      ID.SaveToFile(F);
      F := ID.Text;
    except
      on E: Exception do
        begin
          ID.Free;
          ShowMessage(E.Message);
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
    end
  else
    begin
      FormChooseActor := TFormChooseActor.Create(nil);
      FormChooseActor.Style := '.Arrived';
      try
        if FormChooseActor.ShowModal = 1 then
          begin
            case FormChooseActor.GameActor of
              gaAdmin:FormMatrixGame.SetGameActor(gaAdmin);
              gaPlayer: FormMatrixGame.SetGameActor(gaPlayer);
              gaWatcher: FormMatrixGame.SetGameActor(gaWatcher);
            end;
          end
        else Exit;
      finally
        FormChooseActor.Free;
      end;
    end;
  Application.Run;
end.

