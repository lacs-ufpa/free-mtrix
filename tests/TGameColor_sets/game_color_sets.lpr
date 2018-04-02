program game_color_sets;

type
  TGameColor = (
    gcNone,

    // For contingencies it means 'a participant choosen a color'.
    // For metacontingencies it specifies colors for the 'gcThis' option.
    gcYellow,gcRed,gcGreen,gcBlue,gcMagenta, // 5 colors

    // For metacontingencies only. It means 'all choosen rows are different from each other'.
    gcDiff,

    // For metacontingencies only. It means 'all choosen rows are different from each other'.
    gcEqual,

    // For metacontingencies only. It meand 'a participant chose a specified color'.
    gcThis,

    // For metacontingencies only. Has the following meanings:
    // 1) With grDiff:  Everything except different colors.
    // 2) With grEqual: Everything except equal colors.
    gcNot
  );

  TGameColors = set of TGameColor;

var
  Cs : array of TGameColor;
  LColor : TGameColor;
  CriteriaColors : TGameColors = [gcBlue, gcGreen, gcMagenta];
  LColorCriteria : TGameColors = [gcBlue, gcGreen, gcMagenta, gcYellow, gcRed];
  LChosenColors : TGameColors = [];

begin
  SetLength(Cs, 3);
  Cs[0] := gcBlue;
  Cs[1] := gcGreen;
  Cs[2] := gcMagenta;

  //Cs[0] := gcBlue;
  //Cs[1] := gcGreen;
  //Cs[2] := gcGreen;

  // colors chosen by participants
  for LColor in Cs do Include(LChosenColors, LColor);
  Write('Participants colors:');
  for LColor in LChosenColors do Write(LColor, #32);
  WriteLn('');

  // colors specified by researchers
  LColorCriteria := LColorCriteria * CriteriaColors;
  Write('Researcher colors:');
  for LColor in LColorCriteria do Write(LColor, #32);
  WriteLn('');

  // result
  WriteLn(LColorCriteria <= LChosenColors);
end.

