unit PlaySound;

{$mode objfpc}{$H+}

interface

uses
  CastleSoundEngine;
  // use this at initialization:
var
  CanTalkBuffer    : TSoundBuffer;
  CanNotTalkBuffer : TSoundBuffer;
  NewConditionBuffer : TSoundBuffer;

procedure Play(ASound : TSoundBuffer);

implementation

uses CastleFilesUtils;

procedure LoadSoundBuffers;
begin
  CanTalkBuffer := SoundEngine.LoadBuffer(ApplicationData('pode-conversar.wav'));
  CanNotTalkBuffer := SoundEngine.LoadBuffer(ApplicationData('nao-pode-conversar.wav'));
  NewConditionBuffer := SoundEngine.LoadBuffer(ApplicationData('nova-condicao.wav'));
end;

procedure Play(ASound: TSoundBuffer);
begin
  SoundEngine.PlaySound(ASound);
end;

initialization
  LoadSoundBuffers;

end.

