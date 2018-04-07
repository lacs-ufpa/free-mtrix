program game_color_sets;

uses sysutils;

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
  TGameAColors = array of TGameColor;

function fact(n: integer): longint;
begin
    if (n = 0) then
        fact := 1
    else
        fact := n * fact(n - 1);
end;

// hardcoded for now
function GetCombination(AArrayOfColors: TGameAColors; AN, ATotal : integer): TGameAColors;
begin
  if ATotal <> 6 then
      raise Exception.Create('game_actors.GetCombinations not implemented');
  SetLength(Result, Length(AArrayOfColors));
  case AN of
    0 :
      begin
        Result[0] := AArrayOfColors[0];
        Result[1] := AArrayOfColors[1];
        Result[2] := AArrayOfColors[2];
      end;
    1 :
      begin
        Result[0] := AArrayOfColors[0];
        Result[1] := AArrayOfColors[2];
        Result[2] := AArrayOfColors[1];
      end;
    2 :
      begin
        Result[0] := AArrayOfColors[1];
        Result[1] := AArrayOfColors[2];
        Result[2] := AArrayOfColors[0];
      end;
    3 :
      begin
        Result[0] := AArrayOfColors[1];
        Result[1] := AArrayOfColors[0];
        Result[2] := AArrayOfColors[2];
      end;
    4 :
      begin
        Result[0] := AArrayOfColors[2];
        Result[1] := AArrayOfColors[0];
        Result[2] := AArrayOfColors[1];
      end;
    5 :
      begin
        Result[0] := AArrayOfColors[2];
        Result[1] := AArrayOfColors[1];
        Result[2] := AArrayOfColors[0];
      end;
  end;
end;

function EqualAColors(const A : TGameAColors; const B : TGameAColors):Boolean;
var
  i : integer;
begin
  Result := True;
  if Length(A) = Length(B) then
    for i := Low(A) to High(B) do
      if A[i] = B[i] then Continue else
        begin
          Result := False;
          Break;
        end;
end;

operator in(const AColor: TGameColor; const AArrayOfColors: TGameAColors): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := Low(AArrayOfColors) to High(AArrayOfColors) do
    if AColor = AArrayOfColors[i] then Exit;
  Result := False;
end;

var
  i, j, Len : integer;
  Cs,
  LColorSet,
  CriteriaColors,
  LColorCriteria : TGameAColors;
  Result : Boolean;

begin
  SetLength(CriteriaColors, 4);
  CriteriaColors[0] := gcYellow;
  CriteriaColors[1] := gcYellow;
  CriteriaColors[2] := gcRed;
  CriteriaColors[3] := gcThis;

  SetLength(Cs, 3);
  Cs[0] := gcYellow;
  Cs[1] := gcRed;
  Cs[2] := gcYellow;

  //Cs[0] := gcBlue;
  //Cs[1] := gcGreen;
  //Cs[2] := gcGreen;

  SetLength(LColorSet, 5);
  LColorSet[0] := gcBlue;
  LColorSet[1] := gcGreen;
  LColorSet[2] := gcMagenta;
  LColorSet[3] := gcYellow;
  LColorSet[4] := gcRed;

  // colors specified by researchers, LColorSet * Criteria.Colors
  LColorCriteria := nil;
  for i := Low(CriteriaColors) to High(CriteriaColors) do
    if CriteriaColors[i] in LColorSet then
    begin
      Len := Length(LColorCriteria);
      SetLength(LColorCriteria, Len+1);
      Inc(Len);
      LColorCriteria[Len-1] := CriteriaColors[i];
    end;

  j := fact(Length(Cs));
  Result := False;
  for i := 0 to j-1 do
    Result := Result or EqualAColors(GetCombination(Cs, i, j), LColorCriteria);

  for i := Low(LColorCriteria) to High(LColorCriteria) do WriteLn(LColorCriteria[i]);
  WriteLn('---');
  for i := Low(Cs) to High(Cs) do WriteLn(Cs[i]);
  WriteLn(Result);
end.

