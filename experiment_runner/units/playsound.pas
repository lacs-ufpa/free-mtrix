{
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit PlaySound;

{$mode objfpc}{$H+}

interface

uses
  CastleSoundEngine;
  // use this at initialization:
var
  SoundImpusive : TSoundBuffer;
  SoundAutoCont : TSoundBuffer;

procedure Play(ASound : TSoundBuffer);

implementation

uses CastleFilesUtils;

procedure LoadBuffers;
begin
  SoundImpusive := SoundEngine.LoadBuffer(ApplicationData('culturante-impulsivo.wav'));
  SoundAutoCont := SoundEngine.LoadBuffer(ApplicationData('culturante-autocontrolado.wav'));
end;

procedure Play(ASound: TSoundBuffer);
begin
  SoundEngine.PlaySound(ASound);
end;

initialization
  LoadBuffers;

end.


