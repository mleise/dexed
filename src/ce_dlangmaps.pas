unit ce_dlangmaps;

{$I ce_defines.inc}

interface

(**
 * Perfect static hash-map that detects the D2 "special" keywords such as
 * __LINE__ or __FILE__.
 *)
type

  specialKeywordsMap = record
  private
    const fWords: array [0..15] of string =
    (
      '__PRETTY_FUNCTION__', '__VENDOR__', '', '__VERSION__', '__DATE__',
      '__FILE__', '__LINE__', '', '__FUNCTION__', '', '__TIMESTAMP__', '__TIME__',
      '__EOF__', '', '', '__MODULE__'
    );
    const fHasEntry: array [0..15] of boolean =
    (
      true, true, false, true, true, true, true, false, true, false, true, true,
      true, false, false, true
    );
    const fCoeffs: array[0..255] of Byte =
    (
      50, 243, 103, 74, 140, 54, 86, 48, 32, 12, 76, 146, 95, 139, 178, 149, 255,
      167, 77, 13, 101, 143, 16, 6, 221, 208, 221, 79, 217, 253, 102, 24, 243,
      208, 70, 196, 133, 33, 208, 26, 203, 72, 234, 222, 92, 240, 162, 139, 102,
      174, 240, 48, 10, 173, 208, 107, 85, 176, 211, 77, 246, 56, 253, 2, 164,
      108, 181, 37, 35, 11, 111, 224, 51, 16, 170, 123, 245, 147, 183, 250, 15,
      202, 106, 126, 199, 31, 2, 174, 221, 81, 207, 50, 170, 86, 71, 12, 51, 3,
      30, 192, 132, 159, 74, 35, 62, 90, 10, 135, 33, 23, 15, 100, 2, 69, 250, 248,
      36, 120, 134, 108, 134, 54, 89, 89, 219, 86, 165, 72, 244, 130, 60, 44, 84,
      129, 130, 253, 90, 104, 25, 52, 103, 109, 239, 100, 16, 188, 87, 132, 201,
      110, 175, 152, 181, 178, 196, 61, 52, 60, 169, 26, 52, 48, 90, 236, 244, 26,
      28, 117, 65, 155, 24, 60, 216, 220, 80, 152, 202, 173, 43, 17, 48, 83, 135,
      188, 251, 254, 232, 167, 196, 3, 222, 73, 169, 156, 222, 215, 217, 6, 105,
      171, 130, 169, 27, 9, 147, 176, 207, 45, 43, 47, 11, 163, 215, 117, 11, 100,
      253, 70, 242, 169, 92, 61, 198, 236, 243, 26, 144, 220, 76, 40, 178, 158,
      164, 80, 112, 61, 157, 26, 224, 53, 123, 105, 27, 170, 126, 101, 3, 65, 113,
      101, 157, 109, 110, 252, 207, 0
    );
    class function hash(const w: string): Byte; static;
  public
    class function match(const w: string): boolean; static;
  end;


implementation

{$IFDEF DEBUG}{$PUSH}{$R-}{$ENDIF}
class function specialKeywordsMap.hash(const w: string): Byte;
var
  i: integer;
begin
  Result := 0;
  for i := 2 to length(w) do
    Result += fCoeffs[(Byte(w[i]) + (Byte(i-1) xor Byte(w[i-1]))) and $FF];
  Result := Result and $F;
end;
{$IFDEF DEBUG}{$POP}{$ENDIF}

class function specialKeywordsMap.match(const w: string): boolean;
var
  h: Byte;
begin
  result := false;
  if length(w) < 7 then
    exit;
  h := hash(w);
  if fHasEntry[h] then
    result := fWords[h] = w;
end;

end.

