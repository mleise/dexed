unit u_dlangmaps;

{$I u_defines.inc}

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
      '__VERSION__', '', '__FILE_FULL_PATH__', '__TIME__', '__FILE__', '__VENDOR__',
      '', '__DATE__', '__FUNCTION__', '__LINE__', '__EOF__', '__MODULE__',
      '__PRETTY_FUNCTION__', '', '', '__TIMESTAMP__'
    );
    const fHasEntry: array [0..15] of boolean =
    (
      true, false, true, true, true, true, false, true, true, true, true, true,
      true, false, false, true
    );
    const fCoeffs: array[0..255] of Byte =
    (
      162, 105, 225, 180, 180, 12, 125, 73, 237, 109, 3, 67, 160, 192, 35, 42,
      131, 170, 41, 106, 103, 53, 105, 74, 29, 64, 247, 248, 184, 146, 172, 142,
      239, 232, 158, 168, 29, 243, 40, 241, 255, 85, 184, 38, 44, 242, 193, 222,
      86, 131, 181, 101, 161, 209, 115, 124, 91, 118, 188, 67, 172, 115, 24, 221,
      142, 99, 17, 30, 231, 80, 185, 182, 185, 55, 4, 23, 152, 63, 126, 37, 158,
      36, 28, 235, 65, 220, 243, 62, 169, 129, 127, 76, 149, 232, 21, 119, 134,
      144, 20, 89, 103, 65, 109, 12, 95, 200, 41, 14, 52, 25, 56, 228, 4, 227,
      86, 113, 77, 158, 46, 246, 90, 25, 210, 214, 149, 219, 219, 27, 95, 203,
      43, 21, 191, 94, 216, 113, 100, 222, 245, 224, 127, 174, 214, 44, 78, 89,
      213, 184, 73, 77, 236, 131, 46, 90, 58, 171, 34, 215, 201, 104, 138, 251,
      54, 103, 75, 235, 12, 149, 49, 19, 128, 72, 138, 224, 73, 174, 151, 50,
      152, 32, 135, 238, 132, 34, 3, 230, 201, 166, 31, 119, 50, 155, 125, 103,
      133, 250, 253, 218, 48, 167, 207, 107, 235, 53, 214, 213, 49, 8, 13, 247,
      37, 251, 21, 43, 34, 108, 162, 160, 133, 199, 169, 218, 189, 1, 128, 17,
      67, 186, 55, 2, 23, 23, 133, 114, 240, 176, 124, 127, 217, 231, 129, 220,
      250, 17, 136, 92, 191, 172, 16, 137, 23, 109, 37, 191, 74, 218
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
    // 100017a
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

  uniAlpha = record
  private
    const fRanges: array[0..244] of array[0..1] of word =
    (
      ($00AA, $00AA),
      ($00B5, $00B5),
      ($00B7, $00B7),
      ($00BA, $00BA),
      ($00C0, $00D6),
      ($00D8, $00F6),
      ($00F8, $01F5),
      ($01FA, $0217),
      ($0250, $02A8),
      ($02B0, $02B8),
      ($02BB, $02BB),
      ($02BD, $02C1),
      ($02D0, $02D1),
      ($02E0, $02E4),
      ($037A, $037A),
      ($0386, $0386),
      ($0388, $038A),
      ($038C, $038C),
      ($038E, $03A1),
      ($03A3, $03CE),
      ($03D0, $03D6),
      ($03DA, $03DA),
      ($03DC, $03DC),
      ($03DE, $03DE),
      ($03E0, $03E0),
      ($03E2, $03F3),
      ($0401, $040C),
      ($040E, $044F),
      ($0451, $045C),
      ($045E, $0481),
      ($0490, $04C4),
      ($04C7, $04C8),
      ($04CB, $04CC),
      ($04D0, $04EB),
      ($04EE, $04F5),
      ($04F8, $04F9),
      ($0531, $0556),
      ($0559, $0559),
      ($0561, $0587),
      ($05B0, $05B9),
      ($05BB, $05BD),
      ($05BF, $05BF),
      ($05C1, $05C2),
      ($05D0, $05EA),
      ($05F0, $05F2),
      ($0621, $063A),
      ($0640, $0652),
      ($0660, $0669),
      ($0670, $06B7),
      ($06BA, $06BE),
      ($06C0, $06CE),
      ($06D0, $06DC),
      ($06E5, $06E8),
      ($06EA, $06ED),
      ($06F0, $06F9),
      ($0901, $0903),
      ($0905, $0939),
      ($093D, $094D),
      ($0950, $0952),
      ($0958, $0963),
      ($0966, $096F),
      ($0981, $0983),
      ($0985, $098C),
      ($098F, $0990),
      ($0993, $09A8),
      ($09AA, $09B0),
      ($09B2, $09B2),
      ($09B6, $09B9),
      ($09BE, $09C4),
      ($09C7, $09C8),
      ($09CB, $09CD),
      ($09DC, $09DD),
      ($09DF, $09E3),
      ($09E6, $09F1),
      ($0A02, $0A02),
      ($0A05, $0A0A),
      ($0A0F, $0A10),
      ($0A13, $0A28),
      ($0A2A, $0A30),
      ($0A32, $0A33),
      ($0A35, $0A36),
      ($0A38, $0A39),
      ($0A3E, $0A42),
      ($0A47, $0A48),
      ($0A4B, $0A4D),
      ($0A59, $0A5C),
      ($0A5E, $0A5E),
      ($0A66, $0A6F),
      ($0A74, $0A74),
      ($0A81, $0A83),
      ($0A85, $0A8B),
      ($0A8D, $0A8D),
      ($0A8F, $0A91),
      ($0A93, $0AA8),
      ($0AAA, $0AB0),
      ($0AB2, $0AB3),
      ($0AB5, $0AB9),
      ($0ABD, $0AC5),
      ($0AC7, $0AC9),
      ($0ACB, $0ACD),
      ($0AD0, $0AD0),
      ($0AE0, $0AE0),
      ($0AE6, $0AEF),
      ($0B01, $0B03),
      ($0B05, $0B0C),
      ($0B0F, $0B10),
      ($0B13, $0B28),
      ($0B2A, $0B30),
      ($0B32, $0B33),
      ($0B36, $0B39),
      ($0B3D, $0B43),
      ($0B47, $0B48),
      ($0B4B, $0B4D),
      ($0B5C, $0B5D),
      ($0B5F, $0B61),
      ($0B66, $0B6F),
      ($0B82, $0B83),
      ($0B85, $0B8A),
      ($0B8E, $0B90),
      ($0B92, $0B95),
      ($0B99, $0B9A),
      ($0B9C, $0B9C),
      ($0B9E, $0B9F),
      ($0BA3, $0BA4),
      ($0BA8, $0BAA),
      ($0BAE, $0BB5),
      ($0BB7, $0BB9),
      ($0BBE, $0BC2),
      ($0BC6, $0BC8),
      ($0BCA, $0BCD),
      ($0BE7, $0BEF),
      ($0C01, $0C03),
      ($0C05, $0C0C),
      ($0C0E, $0C10),
      ($0C12, $0C28),
      ($0C2A, $0C33),
      ($0C35, $0C39),
      ($0C3E, $0C44),
      ($0C46, $0C48),
      ($0C4A, $0C4D),
      ($0C60, $0C61),
      ($0C66, $0C6F),
      ($0C82, $0C83),
      ($0C85, $0C8C),
      ($0C8E, $0C90),
      ($0C92, $0CA8),
      ($0CAA, $0CB3),
      ($0CB5, $0CB9),
      ($0CBE, $0CC4),
      ($0CC6, $0CC8),
      ($0CCA, $0CCD),
      ($0CDE, $0CDE),
      ($0CE0, $0CE1),
      ($0CE6, $0CEF),
      ($0D02, $0D03),
      ($0D05, $0D0C),
      ($0D0E, $0D10),
      ($0D12, $0D28),
      ($0D2A, $0D39),
      ($0D3E, $0D43),
      ($0D46, $0D48),
      ($0D4A, $0D4D),
      ($0D60, $0D61),
      ($0D66, $0D6F),
      ($0E01, $0E3A),
      ($0E40, $0E5B),
      ($0E81, $0E82),
      ($0E84, $0E84),
      ($0E87, $0E88),
      ($0E8A, $0E8A),
      ($0E8D, $0E8D),
      ($0E94, $0E97),
      ($0E99, $0E9F),
      ($0EA1, $0EA3),
      ($0EA5, $0EA5),
      ($0EA7, $0EA7),
      ($0EAA, $0EAB),
      ($0EAD, $0EAE),
      ($0EB0, $0EB9),
      ($0EBB, $0EBD),
      ($0EC0, $0EC4),
      ($0EC6, $0EC6),
      ($0EC8, $0ECD),
      ($0ED0, $0ED9),
      ($0EDC, $0EDD),
      ($0F00, $0F00),
      ($0F18, $0F19),
      ($0F20, $0F33),
      ($0F35, $0F35),
      ($0F37, $0F37),
      ($0F39, $0F39),
      ($0F3E, $0F47),
      ($0F49, $0F69),
      ($0F71, $0F84),
      ($0F86, $0F8B),
      ($0F90, $0F95),
      ($0F97, $0F97),
      ($0F99, $0FAD),
      ($0FB1, $0FB7),
      ($0FB9, $0FB9),
      ($10A0, $10C5),
      ($10D0, $10F6),
      ($1E00, $1E9B),
      ($1EA0, $1EF9),
      ($1F00, $1F15),
      ($1F18, $1F1D),
      ($1F20, $1F45),
      ($1F48, $1F4D),
      ($1F50, $1F57),
      ($1F59, $1F59),
      ($1F5B, $1F5B),
      ($1F5D, $1F5D),
      ($1F5F, $1F7D),
      ($1F80, $1FB4),
      ($1FB6, $1FBC),
      ($1FBE, $1FBE),
      ($1FC2, $1FC4),
      ($1FC6, $1FCC),
      ($1FD0, $1FD3),
      ($1FD6, $1FDB),
      ($1FE0, $1FEC),
      ($1FF2, $1FF4),
      ($1FF6, $1FFC),
      ($203F, $2040),
      ($207F, $207F),
      ($2102, $2102),
      ($2107, $2107),
      ($210A, $2113),
      ($2115, $2115),
      ($2118, $211D),
      ($2124, $2124),
      ($2126, $2126),
      ($2128, $2128),
      ($212A, $2131),
      ($2133, $2138),
      ($2160, $2182),
      ($3005, $3007),
      ($3021, $3029),
      ($3041, $3093),
      ($309B, $309C),
      ($30A1, $30F6),
      ($30FB, $30FC),
      ($3105, $312C),
      ($4E00, $9FA5),
      ($AC00, $D7A3)
    );
  public
    class function match(c: UCS4Char): boolean; static; {$IFNDEF DEBUG}inline;{$ENDIF}
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

class function uniAlpha.match(c: UCS4Char): boolean;
var
  low, high, mid: dword;
begin
  low := 0;
  high := length(fRanges) - 1;
  if (c >= fRanges[low][0]) and (c <= fRanges[high][1]) then
  begin
    while low <= high do
    begin
      mid := (low + high) div 2;
      if (c < fRanges[mid][0]) then
        high := mid - 1
      else if (c > fRanges[mid][1]) then
        low := mid + 1
      else
        exit(true);
    end;
  end;
  exit(false);
end;

end.

