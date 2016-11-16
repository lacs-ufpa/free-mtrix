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
  , StrUtils
  , Forms
  , form_matrixgame
  , form_chooseactor, game_message
  { you can add units after this };

const
  PAdmin : array [0..3] of string = ('--admin','--adm','-admin','-adm');
  PPlayer : array [0..3] of string = ('--player','--play','-player','-play');
  PWatcher : array [0..3] of string = ('--watcher','--watch','-watcher','-watch');

{$R *.res}

begin
  //RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMatrixGame, FormMatrixGame);
  if Paramcount > 0 then
    begin
      if AnsiMatchStr(lowercase(ParamStr(0)), PAdmin) then
        FormMatrixGame.GameActor := gaAdmin;
      if AnsiMatchStr(lowercase(ParamStr(0)), PPlayer) then
        FormMatrixGame.GameActor := gaPlayer;
      if AnsiMatchStr(lowercase(ParamStr(0)), PWatcher) then
        FormMatrixGame.GameActor := gaWatcher;
    end
  else
    begin
      Form1 := TForm1.Create(nil);
      if Form1.ShowModal = 1 then
        begin
          case Form1.GameActor of
            gaAdmin:FormMatrixGame.GameActor := gaAdmin;
            gaPlayer: {FormMatrixGame.GameActor := gaPlayer};
            gaWatcher: {FormMatrixGame.GameActor := gaWatcher};
          end;
        end
      else Exit;
      Form1.Free;
    end;
  Application.Run;
end.

