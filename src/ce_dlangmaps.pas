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

