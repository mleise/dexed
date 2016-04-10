unit ce_dlangmaps;

{$I ce_defines.inc}

interface


type

  (**
   * Perfect static hash-map that detects the D2 "special" keywords such as
   * __LINE__ or __FILE__.
   *)
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
    class function hash(const w: string): Byte; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    class function match(const w: string): boolean; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  (**
   * Perfect static hash-map that detects the "straight" D2 keywords plus a few
   * exception for the library types related to immutable strings.
   *)
  keywordsMap = record
  private
    const fWords: array [0..255] of string =
    (
      '', '', 'scope', '', 'creal', '', '', '', '', '', '', '', '', '', 'delegate',
      '', 'dstring', '', 'override', '', '', '', 'is', 'while', 'asm', '', '',
      '', 'struct', '', 'cast', '', '', '', 'long', '', '', 'wstring', '', '',
      'super', 'else', 'real', '', '', '', '', 'mixin', '', '', '', '', '', 'align',
      '', 'dchar', '', '__vector', '', 'bool', '', '', '', '', '', 'unittest',
      'ireal', '', '', '', 'nothrow', 'pragma', '', 'null', '', 'do', '', 'cfloat',
      'cent', '', '', 'true', '', '', 'macro', 'enum', '', '', '', 'immutable', '',
      '', 'private', 'interface', '', 'foreach_reverse', '', '', 'delete', '', '',
      'abstract', 'template', '', '', 'idouble', 'volatile', '', '', 'alias', 'version',
      'char', 'catch', '', '__traits', 'break', '', 'byte', '', '', 'short', '',
      'typeid', 'assert', '', 'goto', '', '', 'protected', '', 'this', '', '', '',
      'default', '', '', '', 'deprecated', '', 'uint', '', '', 'false', '', '', '',
      'ushort', '', '', 'class', '', '', '', 'ref', '', 'if', 'typeof', 'try', '',
      '', 'return', 'void', '', 'throw', '', '', 'pure', 'static', '', 'export', '',
      '', 'typedef', 'ucent', 'finally', '', 'union', 'lazy', '', '', 'with', 'case',
      'body', '__parameters', '', 'float', '', '', 'invariant', '', '', 'string', 'new',
      'ulong', '', '', '', 'function', 'inout', '', '', '', 'switch', '', 'int', '',
      'wchar', 'module', '', '', '', '', '', '', 'import', 'for', '', '', '', '', '',
      '', 'public', '__gshared', 'shared', 'const', '', 'final', 'foreach', '',
      'ifloat', 'out', 'synchronized', '', 'continue', '', '', 'extern', 'package',
      '', 'in', '', '', '', 'debug', '', '', 'double', '', '', 'cdouble', '', 'ubyte',
      'auto', ''
    );
    const fHasEntry: array [0..255] of boolean =
    (
      false, false, true, false, true, false, false, false, false, false, false,
      false, false, false, true, false, true, false, true, false, false, false,
      true, true, true, false, false, false, true, false, true, false, false,
      false, true, false, false, true, false, false, true, true, true, false,
      false, false, false, true, false, false, false, false, false, true, false,
      true, false, true, false, true, false, false, false, false, false, true,
      true, false, false, false, true, true, false, true, false, true, false,
      true, true, false, false, true, false, false, true, true, false, false,
      false, true, false, false, true, true, false, true, false, false, true,
      false, false, true, true, false, false, true, true, false, false, true,
      true, true, true, false, true, true, false, true, false, false, true, false,
      true, true, false, true, false, false, true, false, true, false, false, false,
      true, false, false, false, true, false, true, false, false, true, false, false,
      false, true, false, false, true, false, false, false, true, false, true, true,
      true, false, false, true, true, false, true, false, false, true, true, false,
      true, false, false, true, true, true, false, true, true, false, false, true,
      true, true, true, false, true, false, false, true, false, false, true, true,
      true, false, false, false, true, true, false, false, false, true, false, true,
      false, true, true, false, false, false, false, false, false, true, true, false,
      false, false, false, false, false, true, true, true, true, false, true, true,
      false, true, true, true, false, true, false, false, true, true, false, true,
      false, false, false, true, false, false, true, false, false, true, false, true,
      true, false
    );
    const fCoeffs: array[0..255] of Byte =
    (
      52, 97, 140, 119, 15, 140, 19, 72, 97, 210, 250, 188, 57, 103, 183, 37, 46,
      56, 13, 166, 218, 23, 103, 109, 208, 28, 53, 198, 197, 249, 112, 136, 245,
      167, 160, 217, 160, 35, 91, 70, 207, 80, 9, 131, 0, 102, 137, 201, 201, 236,
      161, 10, 120, 104, 42, 66, 179, 30, 76, 137, 43, 160, 178, 192, 113, 214,
      208, 213, 9, 226, 182, 248, 107, 4, 227, 0, 44, 168, 54, 135, 93, 54, 179,
      49, 127, 36, 114, 213, 191, 59, 205, 253, 99, 47, 4, 33, 105, 152, 134, 204,
      63, 7, 38, 110, 46, 227, 60, 136, 193, 218, 165, 122, 168, 156, 239, 143,
      255, 233, 189, 244, 39, 50, 219, 95, 8, 219, 231, 44, 104, 114, 59, 90, 240,
      28, 50, 39, 90, 144, 70, 15, 57, 53, 198, 219, 126, 49, 14, 100, 75, 215,
      90, 208, 147, 57, 240, 103, 141, 183, 65, 51, 14, 246, 49, 5, 102, 33, 156,
      122, 135, 160, 212, 193, 195, 133, 86, 74, 182, 187, 115, 239, 64, 161, 16,
      112, 28, 82, 18, 112, 139, 9, 250, 117, 16, 34, 40, 223, 113, 158, 26, 230,
      2, 218, 158, 134, 136, 14, 156, 53, 193, 237, 238, 162, 75, 230, 241, 211,
      140, 154, 137, 22, 193, 112, 118, 231, 220, 130, 151, 229, 78, 62, 21, 253,
      30, 161, 223, 3, 220, 125, 140, 243, 86, 180, 166, 127, 40, 156, 212, 44,
      104, 140, 251, 36, 211, 254, 77, 25
    );
    class function hash(const w: string): Byte; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    class function match(const w: string): boolean; static; {$IFNDEF DEBUG}inline;{$ENDIF}
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
  if (length(w) < 7) or (length(w) > 19) then
    exit;
  h := hash(w);
  if fHasEntry[h] then
    result := fWords[h] = w;
end;

{$IFDEF DEBUG}{$PUSH}{$R-}{$ENDIF}
class function keywordsMap.hash(const w: string): Byte;
var
  i: integer;
begin
  Result := 0;
  for i := 2 to length(w) do
    Result += fCoeffs[(Byte(w[i]) + (Byte(i-1) xor Byte(w[i-1]))) and $FF];
  Result := Result and $FF;
end;
{$IFDEF DEBUG}{$POP}{$ENDIF}

class function keywordsMap.match(const w: string): boolean;
var
  h: Byte;
begin
  result := false;
  if (length(w) < 2) or (length(w) > 15) then
    exit;
  h := hash(w);
  if fHasEntry[h] then
    result := fWords[h] = w;
end;

end.

