unit u_dlang;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, u_dlangutils, u_dlangmaps;


type

  (**
   * Represents the pointer in a source file.
   * Automatically updates the line and the column.
   *)
  TReaderHead = object
  private
    fLineIndex: Integer;
    fColumnIndex: Integer;
    fAbsoluteIndex: Integer;
    fReaderHead: PChar;
    fPreviousLineColum: Integer;
    fBegColumnIndex: Integer;
    fBegLineIndex: Integer;
    fBegOffset: Integer;
    function getColAndLine: TPoint;
  public
    constructor Create(const text: PChar; const colAndLine: TPoint);
    procedure setReader(const text: PChar; const colAndLine: TPoint);
    //
    function Next: PChar;
    function previous: PChar;
    procedure saveBeginning;
    //
    property AbsoluteIndex: Integer read fAbsoluteIndex;
    property LineIndex: Integer read fLineIndex;
    property ColumnIndex: Integer read fColumnIndex;
    property LineAndColumn: TPoint read getColAndLine;
    property SavedLine: Integer read fBegLineIndex;
    property SavedColumn: Integer read fBegColumnIndex;
    property SavedOffset: Integer read fBegOffset;
    //
    property head: PChar read fReaderHead;
  end;

  TLexTokenKind = (ltkIllegal, ltkChar, ltkComment, ltkIdentifier, ltkKeyword,
    ltkNumber, ltkOperator, ltkString, ltkSymbol, ltkWhite, ltkDirective, ltkEOF);

const
  LexTokenKindString: array[TLexTokenKind] of string =
   ('Illegal   ',
    'Character ',
    'Comment   ',
    'Identifier',
    'Keyword   ',
    'Number    ',
    'Operator  ',
    'String    ',
    'Symbol    ',
    'White     ',
    'Directive ',
    'EOF       ');

type

  (**
   * Lexer token.
   *)
  PLexToken = ^TLexToken;

  TLexToken = record
    offset: integer;
    position: TPoint;
    kind: TLexTokenKind;
    Data: string;
  end;

  TLexOption = (lxoNoComments, lxoNoWhites);
  TLexOptions = set of TLexOption;

  TLexFoundEvent = procedure(const token: PLexToken; out stop: boolean) of Object;

  (**
   * List of lexer tokens.
   *)
  TLexTokenList = class(TFPList)
  private
    function getToken(index: integer): PLexToken;
  public
    procedure Clear;
    procedure addToken(value: PLexToken);
    procedure saveToFile(const fname: string);
    property token[index: integer]: PLexToken read getToken; default;
  end;

  TLexTokenEnumerator = class
    fList: TLexTokenList;
    fIndex: Integer;
    function GetCurrent: PLexToken;
    function MoveNext: Boolean;
    property Current: PLexToken read GetCurrent;
  end;

  (**
   * Lexer error.
   *)
  PLexError = ^TLexError;

  TLexError = record
    position: TPoint;
    msg: string;
  end;

  (**
   * Error list.
   *)
  TLexErrorList = class(TFPList)
  private
    function getError(index: integer): TLexError;
  public
    procedure Clear;
    procedure addError(value: PLexError);
    property error[index: integer]: TLexError read getError;
  end;

  TLexErrorEnumerator = class
    fList: TLexErrorList;
    fIndex: Integer;
    function GetCurrent: TLexError;
    function MoveNext: Boolean;
    property Current: TLexError read GetCurrent;
  end;

operator enumerator(list: TLexTokenList): TLexTokenEnumerator;
operator enumerator(list: TLexErrorList): TLexErrorEnumerator;

(**
 * Lexes text and fills list with the TLexToken found.
 *)
procedure lex(const text: string; list: TLexTokenList; clbck: TLexFoundEvent = nil; Options: TLexOptions = []);

(**
 * Outputs the module name from a tokenized D source.
 *)
function getModuleName(list: TLexTokenList): string;

(**
 * Fills a list with all the modules imported in a tokenized D source.
 *)
procedure getImports(list: TLexTokenList; imports: TStrings);

(**
 * Compares two TPoints.
 *)
operator = (lhs: TPoint; rhs: TPoint): boolean;
operator > (lhs: TPoint; rhs: TPoint): boolean;
operator < (lhs: TPoint; rhs: TPoint): boolean;
operator <= (lhs: TPoint; rhs: TPoint): boolean;

implementation

{$REGION TReaderHead -----------------------------------------------------------}
operator = (lhs: TPoint; rhs: TPoint): boolean;
begin
  exit((lhs.y = rhs.y) and (lhs.x = rhs.x));
end;

operator > (lhs: TPoint; rhs: TPoint): boolean;
begin
  exit((lhs.y > rhs.y) or ((lhs.y = rhs.y) and (lhs.x > rhs.x)));
end;

operator < (lhs: TPoint; rhs: TPoint): boolean;
begin
  exit(rhs > lhs);
end;

operator <= (lhs: TPoint; rhs: TPoint): boolean;
begin
  exit((lhs = rhs) or (lhs < rhs));
end;

constructor TReaderHead.Create(const text: PChar; const colAndLine: TPoint);
begin
  setReader(text, colAndLine);
end;

procedure TReaderHead.setReader(const text: PChar; const colAndLine: TPoint);
begin
  fLineIndex := colAndLine.y;
  fColumnIndex := colAndLine.x;
  assert((fLineIndex >= 1) and (fColumnIndex >= 1),
    'Line and column have to be greater or equal to 1');
  fReaderHead := text;
end;

function TReaderHead.getColAndLine: TPoint;
begin
  exit(Point(fColumnIndex, fLineIndex));
end;

function TReaderHead.Next: PChar;
begin
  if (fReaderHead^ = #10) then
  begin
    Inc(fLineIndex);
    fPreviousLineColum := fColumnIndex;
    fColumnIndex := 0;
  end;
  Inc(fReaderHead);
  Inc(fAbsoluteIndex);
  Inc(fColumnIndex);
  exit(fReaderHead);
end;

function TReaderHead.previous: PChar;
begin
  Dec(fReaderHead);
  Dec(fColumnIndex);
  Dec(fAbsoluteIndex);
  if (fReaderHead^ = #10) then
  begin
    Dec(fLineIndex);
    fColumnIndex:= fPreviousLineColum;
  end;
  exit(fReaderHead);
end;

procedure TReaderHead.saveBeginning;
begin
  fBegColumnIndex:= fColumnIndex;
  fBegLineIndex:= fLineIndex;
  fBegOffset:= fAbsoluteIndex;
end;

{$ENDREGION}

{$REGION Lexing ----------------------------------------------------------------}
function TLexTokenList.getToken(index: integer): PLexToken;
begin
  Result := PLexToken(Items[index]);
end;

procedure TLexTokenList.Clear;
begin
  while Count > 0 do
  begin
    Dispose(PLexToken(Items[Count - 1]));
    Delete(Count - 1);
  end;
end;

procedure TLexTokenList.addToken(value: PLexToken);
begin
  add(Pointer(value));
end;

procedure TLexTokenList.saveToFile(const fname: string);
var
  tok: PLexToken;
  i: integer;
begin
  with TStringList.Create do
  try
    for i:= 0 to self.count-1 do
    begin
      tok := getToken(i);
      add(format('line %.5d - col %.3d (%.8d): (%s): %s',
        [tok^.position.Y, tok^.position.X, tok^.offset,
          LexTokenKindString[tok^.kind], tok^.Data]));
    end;
  finally
    SaveToFile(fname);
    free;
  end;
end;

function TLexTokenEnumerator.GetCurrent: PLexToken;
begin
  exit(fList.token[fIndex]);
end;

function TLexTokenEnumerator.MoveNext: Boolean;
begin
  Inc(fIndex);
  exit(fIndex < fList.Count);
end;

operator enumerator(list: TLexTokenList): TLexTokenEnumerator;
begin
  Result := TLexTokenEnumerator.Create;
  Result.fList := list;
  Result.fIndex := -1;
end;


procedure lex(const text: string; list: TLexTokenList; clbck: TLexFoundEvent = nil; Options: TLexOptions = []);
var
  reader: TReaderHead;
  tk: TLexTokenKind;
  nestedCom: integer;
  c: UCS4Char;

  function addToken(): boolean;
  var
    ptk: PLexToken;
  begin
    result := false;
    if ((tk <> ltkWhite) or not (lxoNoWhites in Options)) and
      ((tk <> ltkComment) or not (lxoNoComments in Options)) then
    begin
      // add token to the list
      ptk := new(PLexToken);
      ptk^.kind := tk;
      ptk^.position.X := reader.SavedColumn;
      ptk^.position.Y := reader.SavedLine;
      ptk^.offset := reader.savedOffset;
      if tk = ltkWhite then
        ptk^.data := ''
      else
        ptk^.data := Copy(text, ptk^.offset+1, reader.absoluteIndex - ptk^.offset);
      list.Add(ptk);

      // ask caller if we should stop here
      if (tk <> ltkWhite) and (tk <> ltkIllegal) and (clbck <> nil) then
        clbck(list[list.Count-1], result);
    end;
    reader.saveBeginning;
  end;

  function inreal: boolean;
  var
    hex: boolean = false;
    expSet: boolean = false;
  begin
    tk := ltkNumber;
    if (reader.head^ = '0') and (reader.next^ in ['x', 'X']) then
    begin
      hex := true;
      reader.next;
    end;
    while (true) do
    begin
      if reader.head^ = '.' then
      begin
        reader.next;
        break;
      end;
      if isNumber(reader.head^) or (hex and isHex(reader.head^))
        or (reader.head^ = '_') then
      begin
        reader.next;
        continue;
      end;
      break;
    end;
    while (true) do
    begin
      if isNumber(reader.head^) or (hex and isHex(reader.head^))
        or (reader.head^ = '_') then
      begin
        reader.next;
        continue;
      end;
      break;
    end;
    if (reader.head^ in ['e', 'E']) or (hex and (reader.head^ in ['p', 'P'])) then
    begin
      if reader.next^ in ['+', '-'] then
        reader.next;
      while (true) do
      begin
        if isNumber(reader.head^) then
          expSet := true
        else if reader.head^ <> '_' then
          break;
        reader.next;
      end;
      if not expSet then // exit early if no exponent digits
        exit(addToken);
    end
    else if hex then // exit early if hex float without required exponent
      exit(addToken);
    if reader.head^ in ['F', 'f', 'L'] then
      reader.next;
    if reader.head^ = 'i' then
      reader.next;
    exit(addToken);
  end;

  function number: boolean;
  var
    base: integer = 10;
    d: integer = 0;
    done: boolean = false;
    haveDigits: boolean = false;
  begin
    tk := ltkNumber;
    if reader.head^ = '0' then
    begin
      case reader.next^ of
        '0'..'9':
          base := 8;
        'x', 'X':
          begin
            reader.next;
            base := 16;
          end;
        'b', 'B':
          begin
            reader.next;
            base := 2;
          end;
        '.':
          begin
            if (reader.next^ = '.') or isAlpha(reader.head^) or
              (reader.head^ = '_') or (ord(reader.head^) >= $80) then
            begin
              // . is already part of following .. or identifier
              reader.previous;
              done := true;
            end
            else
            begin
              reader.previous;
              reader.previous;
              exit(inreal);
            end;
          end;
        'i', 'f', 'F':
          begin
            reader.previous;
            exit(inreal);
          end;
        '_':
          begin
            reader.next;
            base := 8;
          end;
        'L':
          begin
            if reader.next^ = 'i' then
            begin
              reader.previous;
              reader.previous;
              exit(inreal);
            end;
          end;
      end;
    end;
    if not done then
    begin
      while (true) do
      begin
        case reader.head^ of
          '0'..'9':
            begin
              d := Ord(reader.head^) - Ord('0');
              reader.next;
            end;
          'a'..'f', 'A'..'F':
            begin
              if (base <> 16) and (reader.head^ in ['e', 'E', 'f', 'F']) then
              begin
                repeat
                  reader.previous;
                until reader.absoluteIndex = reader.savedOffset;
                exit(inreal);
              end;
              if reader.head^ in ['a'..'f'] then
                d := ord(reader.head^) - ord('a') + 10
              else
                d := ord(reader.head^) - ord('A') + 10;
              reader.next;
            end;
          'L':
            begin
              if reader.next^ = 'i' then
              begin
                repeat
                  reader.previous;
                until reader.absoluteIndex = reader.savedOffset;
                exit(inreal);
              end;
              reader.previous;
              break;
            end;
          '.':
            begin
              if (reader.next^ = '.') or ((base = 10) and (isAlpha(reader.head^) or
                (reader.head^ = '_') or (ord(reader.head^) >= $80))) then
              begin
                // . is already part or following .. or identifier
                reader.previous;
                break;
              end
              else
              begin
                repeat
                  reader.previous;
                until reader.absoluteIndex = reader.savedOffset;
                exit(inreal);
              end;
            end;
          'p', 'P', 'i':
            begin
              repeat
                reader.previous;
              until reader.absoluteIndex = reader.savedOffset;
              exit(inreal);
            end;
          '_':
            begin
              reader.next;
              continue;
            end;
          otherwise
            break;
        end;
        haveDigits := true;
        if d >= base then
        begin
          // digit too large for chosen base
          reader.previous;
          break;
        end;
      end;
    end;
    if haveDigits or ((base <> 2) and (base <> 16)) then
    begin
      if reader.head^ in ['U', 'u'] then
      begin
        if reader.next^ = 'L' then
          reader.next;
      end
      else if reader.head^ = 'L' then
        if reader.next^ in ['U', 'u'] then
          reader.next;
    end;
    exit(addToken);
  end;

  procedure escapeSequence;
  var
    ndigits: integer;
  begin
    case reader.head^ of
      #39, '"', '?', '\', 'a', 'b', 'f', 'n', 'r', 't', 'v':
      begin
        reader.next;
        exit;
      end;
      'x': // single-byte (i.e. ascii)
        ndigits := 2;
      'u': // base plane code point
        ndigits := 4;
      'U': // full unicode range
        ndigits := 8;
      '&': // xml-style named character entity
        begin
          if (reader.next^ <> ';') and not isNumber(reader.head^) then
          begin
            while isAlNum(reader.head^) do
              reader.next;
            if reader.head^ = ';' then
              reader.next;
          end;
          exit;
        end;
      #0, #26: // end-of-file
        exit;
      otherwise
        if isOctal(reader.head^) then
        begin
          // 0-255 in octal notation
          if isOctal(reader.next^) then
            if isOctal(reader.next^) then
              reader.next;
        end
        else // unrecognized escape sequence, skip the char
          reader.next;
        exit;
    end;
    // parsing hex digits for 'x', 'u', 'U', otherwise unreachable
    while isHex(reader.next^) and (ndigits > 0) do
      dec(ndigits);
  end;

  function decodeUTF(advance: boolean; out c: UCS4Char): boolean;
  var
    p: PChar;
    len: integer;
  begin
    result := true;
    c := 0;
    p := reader.head;
    if (ord(p^) and $F8) = $F0 then
    begin
      len := 4;
      c := ord(p^) and $07;
    end
    else if (ord(p^) and $F0) = $E0 then
    begin
      len := 3;
      c := ord(p^) and $0F;
    end
    else if (ord(p^) and $E0) = $C0 then
    begin
      len := 2;
      c := ord(p^) and $1F;
    end
    else
    begin
      assert(ord(p^) >= $80, 'decodeUTF need not be called for ASCII chars');
      // invalid start code, handle as 1 byte
      result := false;
      len := 1;
      c := ord(p^);
    end;
    while len > 1 do
    begin
      inc(p);
      if (ord(p^) and $C0) <> $80 then
      begin
        // invalid following byte, handle as 1 byte
        result := false;
        c := ord(reader.head^);
        p := reader.head;
        break;
      end;
      c *= $40;
      c += ord(p^) and $3F;
      dec(len);
    end;
    if advance then
      while reader.head <= p do
        reader.next;
  end;

  function isIdentifierStart: boolean;
  var
    c: UCS4Char;
  begin
    result := false;
    if isAlpha(reader.head^) or (reader.head^ = '_') then
      result := true
    else if ord(reader.head^) >= $80 then
      if decodeUTF(false, c) then
        result := uniAlpha.match(c)
  end;

  function readIdentifier: string;
  var
    start, p: PChar;
    c: UCS4Char;
    len: integer;
  begin
    start := reader.head;
    while (true) do
    begin
      if isAlNum(reader.head^) or (reader.head^ = '_') then
        reader.next
      else if ord(reader.head^) >= $80 then
      begin
        p := reader.head;
        if decodeUTF(true, c) then
          if uniAlpha.match(c) then
            continue;
        while reader.previous <> p do;
        break;
      end
      else
        break;
    end;
    len := reader.head - start;
    result := copy(text, reader.absoluteIndex - len + 1, len);
  end;

  function ident: boolean;
  var
    s: string;
  begin
    tk := ltkIdentifier;
    s := readIdentifier;
    if keywordsMap.match(s) or specialKeywordsMap.match(s) then
      tk := ltkKeyword;
    exit(addToken);
  end;

  function wysiwygStringConstant: boolean;
  var
    delim: char;
  begin
    tk := ltkString;
    delim := reader.head^;
    while (true) do
    begin
      if reader.next^ in [#0, #26] then // EOF
        break;
      if reader.head^ = delim then
      begin
        if reader.next^ in stringPostfixes then
          reader.next;
        break;
      end;
    end;
    exit(addToken);
  end;

  function delimitedStringConstant: boolean;
  var
    startline: boolean = false;
    blankrol: boolean = false;
    hereid: string = '';
    delimleft: char = #0;
    delimright: char = #0;
    nest: boolean = true;
    nestcount: dword = 0;
    psave: PChar;
  begin
    tk := ltkString;
    reader.next;
    while (true) do
    begin
      case reader.head^ of
        #10:
          begin
            startline := true;
            if blankrol then
            begin
              blankrol := false;
              reader.next;
              continue;
            end;
            if hereid <> '' then
            begin
              reader.next;
              continue;
            end;
          end;
        #0, #26:
          exit(addToken);
      end;
      if delimleft = #0 then
      begin
        delimleft := reader.head^;
        nest := true;
        nestcount := 1;
        case delimleft of
          '(':
            delimright := ')';
          '{':
            delimright := '}';
          '[':
            delimright := ']';
          '<':
            delimright := '>';
          otherwise
            if isIdentifierStart then
            begin
              hereid := readIdentifier;
              blankrol := true;
              nest := false;
              continue;
            end
            else
            begin
              if isSpace(delimleft) then // delimiter cannot be whitespace
                exit(addToken);
              delimright := delimleft;
              nest := false;
            end;
        end;
      end
      else
      begin
        if blankrol then // heredoc rest of line should be blank
          exit(addToken);
        if nest then
        begin
          if reader.head^ = delimleft then
            inc(nestcount)
          else if reader.head^ = delimright then
          begin
            dec(nestcount);
            if nestcount = 0 then
            begin
              reader.next;
              break;
            end;
          end;
        end
        else if reader.head^ = delimright then
        begin
          reader.next;
          break;
        end;
        if startline and (hereid <> '') and isIdentifierStart then
        begin
          psave := reader.head;
          if readIdentifier = hereid then
            break;
          while reader.previous <> psave do;
        end;
        startline := false;
      end;
      reader.next;
    end;
    if reader.head^ = '"' then
      if reader.next^ in stringPostfixes then
        reader.next;
    exit(addToken);
  end;

  function escapeStringConstant: boolean;
  var
    c: UCS4Char;
  begin
    tk := ltkString;
    reader.next;
    while (true) do
    begin
      case reader.head^ of
        '\':
          begin
            reader.next;
            escapeSequence;
          end;
        '"':
          begin
            reader.next;
            break;
          end;
        #0, #26: // EOF
          break;
        otherwise
          if ord(reader.head^) >= $80 then
            decodeUTF(true, c)
          else
            reader.next;
      end;
    end;
    exit(addToken);
  end;

begin
  // based on dmd/lexer.d, commit 77260f78e45675bb1418feb9836ed0c6252fd1b9
  // - all characters belonging to a token are always captured
  // - the line endings are assumed to be converted to #10

  reader.create(PChar(text), Point(1, 1));
  reader.saveBeginning;
  while (true) do
  begin
    case reader.head^ of
      #0, #26:
        begin
          tk := ltkEOF;
          addToken();
          break;
        end;
      ' ', #9, #10, #11, #12, #13:
        begin
          tk := ltkWhite;
          repeat until not isSpace(reader.next^);
          addToken;
        end;
      '0'..'9':
        begin
          tk := ltkNumber;
          if reader.head^ = '0' then
          begin
            if not isZeroSecond(reader.next^) then
            begin
              if addToken then
                exit;
              continue;
            end;
          end
          else if not isDigitSecond(reader.next^) then
          begin
            if addToken then
              exit;
            continue;
          end;
          reader.previous;
          if number then
            exit;
        end;
      #39:
        begin
          tk := ltkChar;
          if isSingleChar(reader.next^) then
          begin
            if reader.next^ = #39 then
            begin
              reader.next;
              if addToken then
                exit;
              continue;
            end;
            reader.previous;
          end;
          case reader.head^ of
            '\':
              begin
                reader.next;
                escapeSequence;
              end;
            #0, #10, #13, #26, #39:
              ; // don't skip over these EOL and EOF chars
            #128..#255:
              decodeUTF(true, c);
            otherwise
              reader.next;
          end;
          if reader.head^ = #39 then
            reader.next;
          if addToken then
            exit;
        end;
      'r', '`':
        begin
          if reader.head^ = 'r' then
          begin
            if reader.next^ <> '"' then
            begin
              reader.previous;
              if ident then
                exit;
              continue;
            end;
            // skipping over the 'r', to the delimiter '"'
          end;
          if wysiwygStringConstant then
            exit;
        end;
      'q':
        begin
          if reader.next^ = '"' then
          begin
            if delimitedStringConstant then
              exit;
            continue;
          end
          else if reader.head^ = '{' then
          begin
            tk := ltkSymbol;
            reader.next;
            if addToken then
              exit;
            continue;
          end;
          reader.previous;
          if ident then
            exit;
        end;
      '"':
        begin
          if escapeStringConstant then
            exit;
        end;
      '/':
        begin
          case reader.next^ of
            '=':
              begin
                tk := ltkOperator;
                reader.next;
                if addToken then
                  exit;
              end;
            '*':
              begin
                tk := ltkComment;
                reader.next;
                while not (reader.head^ in ['/', #0, #26]) or
                  ((reader.head^ = '/') and (((reader.head-1)^ <> '*') or
                  (reader.savedOffset = reader.absoluteIndex-2))) do
                  reader.next;
                if reader.head^ = '/' then
                  reader.next;
                if addToken then
                  exit;
              end;
            '/':
              begin
                tk := ltkComment;
                reader.next;
                while not (reader.head^ in [#0, #10, #13, #26]) do
                  reader.next;
                if addToken then
                  exit;
              end;
            '+':
              begin
                tk := ltkComment;
                reader.next;
                nestedCom := 1;
                while not (reader.head^ in [#0, #26]) do
                begin
                  if reader.head^ = '/' then
                  begin
                    if reader.next^ = '+' then
                    begin
                      reader.next;
                      nestedCom += 1;
                    end;
                  end
                  else if reader.head^ = '+' then
                  begin
                    if reader.next^ = '/' then
                    begin
                      reader.next;
                      nestedCom -= 1;
                      if nestedCom = 0 then
                        break;
                    end;
                  end
                  else
                    reader.next;
                end;
                if addToken then
                  exit;
              end;
            otherwise // division operator
              tk := ltkOperator;
              if addToken then
                exit;
          end;
        end;
      '.':
        begin
          if isNumber(reader.next^) then // float literal
          begin
            reader.previous;
            if inreal then
              exit;
            continue;
          end;
          tk := ltkSymbol; // .
          if reader.head^ = '.' then // ..
            if reader.next^ = '.' then // ...
              reader.next;
          if addToken then
            exit;
        end;
      '&', '|', '-', '+': // &= && |= || -= -- += ++
        begin
          tk := ltkOperator;
          if (reader.head^ = reader.next^) or (reader.head^ = '=') then
            reader.next;
          if addToken then
            exit;
        end;
      '<': // < <= << <<=
        begin
          tk := ltkOperator;
          if reader.next^ = '=' then
            reader.next
          else if reader.head^ = '<' then
            if reader.next^ = '=' then
              reader.next;
          if addToken then
            exit;
        end;
      '>': // > >= >> >>= >>> >>>=
        begin
          tk := ltkOperator;
          if reader.next^ = '=' then
            reader.next
          else if reader.head^ = '>' then
            if reader.next^ = '=' then
              reader.next
            else if reader.head^ = '>' then
              if reader.next^ = '=' then
                reader.next;
          if addToken then
            exit;
        end;
      '!', '~', '*', '%': // ! != ~ ~= * *= % %=
        begin
          tk := ltkOperator;
          if reader.next^ = '=' then
            reader.next;
          if addToken then
            exit;
        end;
      '=': // = == =>
        begin
          tk := ltkOperator;
          if reader.next^ in ['=', '>'] then
            reader.next;
          if addToken then
            exit;
        end;
      '^': // ^ ^= ^^ ^^=
        begin
          tk := ltkOperator;
          if reader.head^ = '=' then
            reader.next
          else if reader.next^ = '^' then
            if reader.next^ = '=' then
              reader.next;
          if addToken then
            exit;
        end;
      '(', ')', '[', ']', '{', '}', '?', ',', ';', ':', '$', '@':
        begin
          tk := ltkSymbol;
          reader.next;
          if addToken then
            exit;
        end;
      '#': // directive
        begin
          tk := ltkDirective;
          while not (reader.next^ in [#0, #10, #13, #26]) do;
          if addToken then
            exit;
        end;
    otherwise
      if isIdentifierStart then
      begin
        if ident then
          exit;
        continue;
      end;
      // only add a new illegal token if we aren't already inside one
      if list.count <> 0 then
      begin
        if list[list.count-1]^.kind = ltkIllegal then
        begin
          list[list.count-1]^.data += reader.head^;
          reader.next;
          reader.saveBeginning;
          continue;
        end;
      end;
      tk := ltkIllegal;
      reader.next;
      addToken;
    end;
  end;
end;
{$ENDREGION}

{$REGION Utils}
function TLexErrorList.getError(index: integer): TLexError;
begin
  Result := PLexError(Items[index])^;
end;

procedure TLexErrorList.Clear;
begin
  while Count > 0 do
  begin
    Dispose(PLexError(Items[Count - 1]));
    Delete(Count - 1);
  end;
end;

procedure TLexErrorList.addError(value: PLexError);
begin
  add(Pointer(value));
end;

function TLexErrorEnumerator.GetCurrent: TLexError;
begin
  exit(fList.error[fIndex]);
end;

function TLexErrorEnumerator.MoveNext: Boolean;
begin
  Inc(fIndex);
  exit(fIndex < fList.Count);
end;

operator enumerator(list: TLexErrorList): TLexErrorEnumerator;
begin
  Result := TLexErrorEnumerator.Create;
  Result.fList := list;
  Result.fIndex := -1;
end;

function getModuleName(list: TLexTokenList): string;
var
  ltk: PLexToken;
  mtok: boolean = false;
begin
  Result := '';
  for ltk in list do
  begin
    if mtok then
    begin
      case ltk^.kind of
        ltkIdentifier, ltkKeyword:
          Result += ltk^.Data;
        ltkSymbol:
          case ltk^.Data of
            '.': Result += ltk^.Data;
            ';': exit;
          end;
      end;
    end
    else
    if ltk^.kind = ltkKeyword then
      if ltk^.Data = 'module' then
        mtok := True;
  end;
end;

procedure getImports(list: TLexTokenList; imports: TStrings);
var
  i: integer;
  imp: boolean = false;
  tok: PLexToken;
  itm: string = '';
begin
  for i:= 0 to list.Count-1 do
  begin
    tok := list[i];
    if (tok^.kind = ltkKeyword) and (tok^.Data = 'import') then
    begin
      imp := true;
      continue;
    end;
    if not imp then
      continue;
    //
    if (tok^.Data = '=') then
      itm := ''
    else if (tok^.Data = ';') or (tok^.Data = ':') or (tok^.Data = ',') then
    begin
      if (length(itm) <> 0) and (imports.IndexOf(itm) = -1) then
        imports.Add(itm);
      itm := '';
      if (tok^.Data = ';') or (tok^.Data = ':') then
        imp := false;
    end else
      itm += tok^.Data;
  end;
end;
{$ENDREGION}
end.

