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
      '__FILE__', '', '__TIME__', '', '', '__EOF__', '__VENDOR__', '__LINE__',
      '', '__PRETTY_FUNCTION__', '__DATE__', '__MODULE__', '__FUNCTION__',
      '__TIMESTAMP__', '__VERSION__', ''
    );
    const fHasEntry: array [0..15] of boolean =
    (
      true, false, true, false, false, true, true, true, false, true, true, true,
      true, true, true, false
    );
    const fCoeffs: array[0..255] of Byte =
    (
      23, 54, 252, 239, 192, 69, 42, 70, 63, 3, 89, 171, 21, 218, 140, 187, 191,
      74, 12, 230, 108, 85, 152, 3, 56, 199, 164, 61, 172, 64, 225, 99, 179, 182,
      174, 189, 171, 243, 108, 60, 208, 146, 6, 17, 236, 103, 127, 4, 219, 83,
      85, 138, 228, 29, 243, 85, 155, 246, 32, 93, 255, 80, 135, 160, 38, 224, 3,
      134, 54, 8, 27, 129, 142, 243, 201, 39, 198, 171, 242, 78, 141, 183, 57,
      187, 168, 138, 251, 219, 96, 186, 112, 117, 220, 164, 203, 21, 127, 101,
      77, 41, 187, 233, 66, 20, 108, 205, 192, 246, 49, 38, 71, 106, 176, 176,
      253, 21, 18, 136, 165, 120, 50, 144, 62, 90, 56, 10, 0, 109, 109, 70, 108,
      191, 244, 103, 237, 145, 214, 39, 227, 200, 40, 135, 49, 12, 7, 203, 37,
      101, 142, 4, 156, 251, 234, 144, 54, 251, 17, 82, 59, 108, 178, 60, 229,
      43, 72, 97, 193, 36, 253, 36, 154, 219, 246, 59, 165, 144, 136, 136, 231,
      243, 222, 108, 73, 148, 208, 148, 147, 57, 9, 1, 193, 247, 251, 116, 48,
      72, 165, 80, 49, 86, 220, 197, 227, 71, 24, 98, 21, 132, 2, 164, 133, 186,
      43, 19, 138, 166, 119, 202, 109, 124, 91, 32, 159, 155, 145, 124, 164, 19,
      200, 143, 191, 31, 199, 229, 240, 115, 135, 52, 181, 14, 140, 174, 12, 139,
      45, 158, 171, 146, 186, 72, 34, 12, 6, 207, 151, 117
    );
    class function hash(const w: string): Byte; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    class function match(const w: string): boolean; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  (**
   * Perfect static hash-map that detects the 'straight' D2 keywords plus a few
   * exception for the library types related to the strings and registry-wide integers.
   *)
  keywordsMap = record
  private
    const fWords: array [0..511] of string =
    (
      'double', '', '', '', '', 'volatile', 'synchronized', '', 'wchar', '', '',
      '', '', '', 'goto', '', 'assert', '', '', 'void', '', '', '', 'override',
      'pure', '', '', '', '', '', '', 'delegate', '', '', 'super', '', 'case',
      '', '', '', 'pragma', '', '', '', 'string', '', 'debug', '', '', '', '',
      '', 'module', '', '', '', '', '', '', '', '', '', '', 'immutable', '',
      'template', 'dstring', '', '__parameters', '', '', '', '', '__vector', '',
      '', '', '', '', '', 'invariant', '', 'unittest', '', '', 'protected', '',
      '', 'break', 'alias', '', '', '', '', '', '', '', '', '', 'wstring', '',
      '', 'private', 'final', '', 'false', '', 'catch', 'float', '', '', '', '',
      '', '', '', '', '', '', '', '', '', '', 'align', '', '', '', '', '', '',
      'ptrdiff_t', '', '', '', '', '', '', 'delete', '', '', '', '', '', '', '',
      'do', '', 'mixin', '', 'ireal', '', '', '', '', 'static', 'extern', '', '',
      'null', '', '', 'creal', '', '', 'typeid', '', 'idouble', '', '', '', 'try',
      '', '', '', 'finally', '', 'is', '', 'cdouble', '', 'in', '', '', '', '',
      '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
      'scope', '', '', 'package', '', '', '', '', '', 'interface', '', '', 'macro',
      '', '', '', '', '', '', '', '', '', 'default', '', '', '', '', '', 'out',
      '', '', '', '', 'size_t', '', '', '', '', 'new', 'int', '', '', '', '', '',
      '', '', '', 'this', '', '', '', '', '', '', '', 'public', '', '', '',
      'continue', '', '', '', 'body', '', '', '', '', '', '', 'ifloat', '', '',
      '', '', 'version', '', '', 'deprecated', '', '', '', 'cfloat', '', 'uint',
      'function', '', '', '', '', 'short', '', 'with', 'typeof', '', '', '', '',
      '', '', '', '', '', '', '', 'import', '', '', '', '', '', '', '', '',
      '__traits', '', '', '', '', '', 'export', '', '', '', '', '', '', '', '',
      '', '', '', '', '', '', 'throw', 'ushort', '', '', '', '', '', '', '', '',
      '', 'asm', '', '', '', '', '', 'byte', '', '', '', '', '', 'abstract',
      'union', 'if', '', 'true', '', 'typedef', '', '', '', '', '', '', '', '',
      '', '', '', '', '', '', '', 'enum', '', '', 'const', '', '', '', '', '', '',
      '', 'bool', '', '', '', '', '', '', 'ubyte', 'else', 'long', '', '', 'for',
      '', '', '', 'inout', '', '', '', '', '', '', '', 'auto', '', '', '', '', '',
      '', 'cent', '', '', '', '', '', '', '', '', 'class', '', '', 'cast', '', '',
      '', '', '', 'struct', '', 'foreach', '', '', '', 'ulong', '', '', '__gshared',
      '', 'while', 'ref', '', '', '', '', '', '', '', '', 'char', 'return', '',
      'foreach_reverse', 'lazy', '', '', 'ucent', '', '', '', 'nothrow', '', '',
      '', '', '', '', '', 'switch', '', '', 'dchar', '', '', '', 'shared', '', '',
      '', 'real', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''
    );
    const fHasEntry: array [0..511] of boolean =
    (
      true, false, false, false, false, true, true, false, true, false, false,
      false, false, false, true, false, true, false, false, true, false, false,
      false, true, true, false, false, false, false, false, false, true, false,
      false, true, false, true, false, false, false, true, false, false, false,
      true, false, true, false, false, false, false, false, true, false, false,
      false, false, false, false, false, false, false, false, true, false, true,
      true, false, true, false, false, false, false, true, false, false, false,
      false, false, false, true, false, true, false, false, true, false, false,
      true, true, false, false, false, false, false, false, false, false, false,
      true, false, false, true, true, false, true, false, true, true, false, false,
      false, false, false, false, false, false, false, false, false, false, false,
      false, true, false, false, false, false, false, false, true, false, false,
      false, false, false, false, true, false, false, false, false, false, false,
      false, true, false, true, false, true, false, false, false, false, true,
      true, false, false, true, false, false, true, false, false, true, false,
      true, false, false, false, true, false, false, false, true, false, true,
      false, true, false, true, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, true, false, false, true, false, false, false,
      false, false, true, false, false, true, false, false, false, false, false,
      false, false, false, false, true, false, false, false, false, false, true,
      false, false, false, false, true, false, false, false, false, true, true,
      false, false, false, false, false, false, false, false, true, false, false,
      false, false, false, false, false, true, false, false, false, true, false,
      false, false, true, false, false, false, false, false, false, true, false,
      false, false, false, true, false, false, true, false, false, false, true,
      false, true, true, false, false, false, false, true, false, true, true, false,
      false, false, false, false, false, false, false, false, false, false, true,
      false, false, false, false, false, false, false, false, true, false, false,
      false, false, false, true, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, true, true, false, false,
      false, false, false, false, false, false, false, true, false, false, false,
      false, false, true, false, false, false, false, false, true, true, true,
      false, true, false, true, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, true, false, false,
      true, false, false, false, false, false, false, false, true, false, false,
      false, false, false, false, true, true, true, false, false, true, false,
      false, false, true, false, false, false, false, false, false, false, true,
      false, false, false, false, false, false, true, false, false, false, false,
      false, false, false, false, true, false, false, true, false, false, false,
      false, false, true, false, true, false, false, false, true, false, false,
      true, false, true, true, false, false, false, false, false, false, false,
      false, true, true, false, true, true, false, false, true, false, false,
      false, true, false, false, false, false, false, false, false, true, false,
      false, true, false, false, false, true, false, false, false, true, false,
      false, false, false, false, false, false, false, false, false, false, false,
      false, false, false, false
    );
    const fCoeffs: array[0..255] of Byte =
    (
      93, 12, 147, 37, 246, 76, 204, 47, 77, 0, 217, 84, 225, 244, 62, 63, 81, 2,
      46, 137, 104, 245, 184, 87, 229, 148, 69, 207, 24, 10, 239, 172, 27, 34, 60,
      251, 113, 66, 175, 29, 10, 1, 158, 38, 157, 120, 224, 173, 11, 199, 49, 173,
      88, 229, 213, 191, 217, 177, 90, 19, 83, 212, 97, 12, 136, 154, 243, 105,
      97, 29, 94, 226, 71, 60, 28, 245, 38, 212, 156, 116, 254, 70, 207, 211, 93,
      67, 32, 42, 149, 101, 98, 4, 83, 160, 228, 128, 231, 188, 100, 178, 22, 172,
      198, 218, 13, 166, 45, 54, 49, 152, 14, 123, 232, 223, 86, 10, 62, 46, 220,
      55, 161, 22, 210, 86, 14, 79, 8, 28, 66, 67, 84, 116, 159, 144, 37, 46, 199,
      218, 233, 188, 207, 168, 89, 64, 245, 3, 6, 199, 144, 165, 216, 145, 141, 70,
      69, 20, 149, 252, 119, 75, 153, 97, 14, 196, 74, 48, 91, 145, 70, 90, 59, 69,
      92, 252, 233, 161, 169, 155, 9, 28, 234, 103, 172, 225, 164, 49, 161, 95, 81,
      201, 217, 217, 58, 119, 169, 230, 11, 8, 137, 65, 165, 159, 4, 243, 225, 236,
      178, 209, 133, 35, 68, 222, 237, 114, 64, 158, 72, 66, 151, 208, 169, 232, 83,
      229, 157, 233, 123, 135, 65, 187, 161, 100, 217, 63, 124, 36, 108, 198, 2,
      103, 156, 241, 140, 163, 128, 196, 45, 166, 41, 61, 19, 139, 25, 115, 72, 175
    );
    class function hash(const w: string): Word; static; {$IFNDEF DEBUG}inline;{$ENDIF}
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
  for i := 1 to length(w) do
    Result += fCoeffs[Byte(w[i])];
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
class function keywordsMap.hash(const w: string): Word;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to length(w) do
    Result += fCoeffs[Byte(w[i])];
  Result := Result and $1FF;
end;
{$IFDEF DEBUG}{$POP}{$ENDIF}

class function keywordsMap.match(const w: string): boolean;
var
  h: Word;
begin
  result := false;
  if (length(w) < 2) or (length(w) > 15) then
    exit;
  h := hash(w);
  if fHasEntry[h] then
    result := fWords[h] = w;
end;

end.

