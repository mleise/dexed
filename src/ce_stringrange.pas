unit ce_stringrange;

{$I ce_defines.inc}

interface

uses
  SysUtils;

type

  PStringRange = ^TStringRange;

  (**
   * Iterator specialized for strings.
   *
   * This structure allows to easily scan strings.
   * Most of the operations can be chained because the functions
   * return either a pointer to a TStringRange (in this case this is always
   * the "Self") or a new TStringRange (in this case this is always a copy).
   *
   * This is based on a more generic work which tries to implement some kind
   * of "D" ranges in Object Pascal (see https://github.com/BBasile/ArrayOps).
   * Even if Object Pascal doesn't provide the expressivness required to mimic
   * D ranges, a few good stuff are still possible.
   *)
  TStringRange = record
  private
    ptr: PChar;
    pos: integer;
    len: integer;

  public

    // returns a new range initialized with a string.
    class function create(const str: string): TStringRange; static;
    // returns a new range initialized from a pointer.
    class function create(const pchr: PChar; length: integer): TStringRange; static;

    // initializes the range with a string.
    function init(const str: string): PStringRange; inline;
    // initialized the range from a pointer.
    function init(const pchr: PChar; length: integer): PStringRange; inline;

    // advances.
    procedure popFront; inline;
    // returns the current element.
    function front: char; inline;
    // indicates wether the range is consumed.
    function empty: boolean; inline;

    // yields the state of the range to a string.
    function yield: string; inline;
    // returns a copy.
    function save: TStringRange; inline;
    // resets the range.
    function reset: PStringRange; inline;

    // advances the range while the front is in value, returns a copy.
    function takeWhile(value: TSysCharSet): TStringRange; overload; inline;
    function takeWhile(value: Char): TStringRange; overload; inline;
    // advances the range until the front is in value, returns a copy.
    function takeUntil(value: TSysCharSet): TStringRange; overload; inline;
    function takeUntil(value: Char): TStringRange; overload; inline;
    // advances the range while the front is in value.
    function popWhile(value: TSysCharSet): PStringRange; overload; inline;
    function popWhile(value: Char): PStringRange; overload; inline;
    // advances the range until the front is in value.
    function popUntil(value: TSysCharSet): PStringRange; overload; inline;
    function popUntil(value: Char): PStringRange; overload; inline;

    // returns the next word.
    function nextWord: string; inline;
    // returns the next line.
    function nextLine: string; inline;
    // indicates wether the range starts with value.
    function startsWith(const value: string): boolean; inline;
    // indicates wether the range starts with value.
    function startsWith(var value: TStringRange): boolean; inline;
  end;

implementation

class function TStringRange.create(const str: string): TStringRange;
begin
  result.ptr := @str[1];
  result.pos := 0;
  result.len := length(str);
end;

class function TStringRange.create(const pchr: PChar; length: integer): TStringRange;
begin
  result.ptr := pchr;
  result.pos := 0;
  result.len := length;
end;

function TStringRange.init(const str: string): PStringRange;
begin
  ptr := nil;
  pos := 0;
  len := 0;
  if str = '' then
    exit;
  ptr := @str[1];
  pos := 0;
  len := length(str);
  Result := @self;
end;

function TStringRange.init(const pchr: PChar; length: integer): PStringRange;
begin
  ptr := pchr;
  pos := 0;
  len := length;
  Result := @self;
end;

procedure TStringRange.popFront;
begin
  pos += 1;
end;

function TStringRange.front: char;
begin
  result := (ptr + pos)^;
end;

function TStringRange.empty: boolean;
begin
  result := pos >= len;
end;

function TStringRange.yield: string;
begin
  Result := ptr[pos .. len-1];
end;

function TStringRange.save: TStringRange;
begin
  Result.len:= len;
  Result.pos:= pos;
  Result.ptr:= ptr;
end;

function TStringRange.reset: PStringRange;
begin
  pos := 0;
  Result := @Self;
end;

function TStringRange.takeWhile(value: TSysCharSet): TStringRange;
begin
  Result.ptr := ptr + pos;
  Result.pos := 0;
  Result.len := 0;
  while true do
  begin
    if empty or not (front in value) then
      break;
    Result.len += 1;
    popFront;
  end;
end;

function TStringRange.takeWhile(value: Char): TStringRange;
begin
  Result.ptr := ptr + pos;
  Result.pos := 0;
  Result.len := 0;
  while true do
  begin
    if empty or not (front = value) then
      break;
    Result.len += 1;
    popFront;
  end;
end;

function TStringRange.takeUntil(value: TSysCharSet): TStringRange;
begin
  Result.ptr := ptr + pos;
  Result.pos := 0;
  Result.len := 0;
  while true do
  begin
    if empty or (front in value) then
      break;
    Result.len += 1;
    popFront;
  end;
end;

function TStringRange.takeUntil(value: Char): TStringRange;
begin
  Result.ptr := ptr + pos;
  Result.pos := 0;
  Result.len := 0;
  while true do
  begin
    if empty or (front = value) then
      break;
    Result.len += 1;
    popFront;
  end;
end;

function TStringRange.popWhile(value: TSysCharSet): PStringRange;
begin
  while true do
  begin
    if empty or not (front in value) then
      break;
    popFront;
  end;
  Result := @self;
end;

function TStringRange.popWhile(value: Char): PStringRange;
begin
  while true do
  begin
    if empty or not (front = value) then
      break;
    popFront;
  end;
  Result := @self;
end;

function TStringRange.popUntil(value: TSysCharSet): PStringRange;
begin
  while true do
  begin
    if empty or (front in value) then
      break;
    popFront;
  end;
  Result := @self;
end;

function TStringRange.popUntil(value: Char): PStringRange;
begin
  while true do
  begin
    if empty or (front = value) then
      break;
    popFront;
  end;
  Result := @self;
end;

function TStringRange.nextWord: string;
const
  blk = [#0 .. #32];
begin
  Result := popWhile(blk)^.takeUntil(blk).yield;
end;

function TStringRange.nextLine: string;
const
  lsp = [#10, #13];
begin
  Result := popWhile(lsp)^.takeUntil(lsp).yield;
end;

function TStringRange.startsWith(const value: string): boolean;
begin
  Result := false;
  if len - pos <= length(value) then
    Result := ptr[pos .. pos + length(value)] = value;
end;

function TStringRange.startsWith(var value: TStringRange): boolean;
var
  p0, p1: integer;
begin
  p0 := pos;
  p1 := value.pos;
  Result := true;
  while not empty and not value.empty do
  begin
    if front <> value.front then
    begin
      Result := false;
      break;
    end;
    popFront;
    value.popFront;
  end;
  pos := p0;
  value.pos := p1;
end;

end.

