{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
program tgamecontrol_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, form_main, game_actors, game_control, game_experiment,
  game_file_methods, game_visual_elements, game_actors_helpers, string_methods,
  game_visual_matrix_a, game_visual_experiment, game_actors_point,
  presentation_classes, helpers, game_resources, zmq_network, popup_hack,
  game_zmq_actors, report_reader, regdata
  { you can add units after this };

{$R *.res}

begin
  Randomize;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

