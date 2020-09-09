unit VisibleDSA.Board;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils.UString;

type
  TArray_str = array of UString;
  TArr2D_chr = array of array of UChar;

  TBoard = class(TObject)
  public const
    EMPTY: UChar = '.';

  private
    _n: integer;
    _m: integer;
    _data: TArr2D_chr;

    function __getItems(x, y: integer): UChar;
    procedure __drop;
    function __match: boolean;

  public
    constructor Create(b: TBoard); overload;
    constructor Create(strs: TArray_str); overload;
    destructor Destroy; override;
    function InArea(x, y: integer): boolean;
    function IsWin: boolean;
    procedure Print;
    procedure Run;
    procedure Swap(x1, y1, x2, y2: integer);

    property N: integer read _n;
    property M: integer read _m;
    property Items[x, y: integer]: UChar read __getItems; default;
  end;

implementation

{ TBoard }

constructor TBoard.Create(strs: TArray_str);
var
  i: integer;
begin
  if strs = nil then
    raise Exception.Create('lines cannot be null in Board constructor.');

  _n := Length(strs);
  if _n = 0 then
    raise Exception.Create('lines cannot be empty in Board constructor.');

  _m := Length(strs[0]);
  SetLength(_data, _n, _m);

  for i := 0 to _n - 1 do
  begin
    if Length(_data[i]) <> _m then
      raise Exception.Create('All line''s length must be same in Board constructor.');

    _data[i] := strs[i].ToCharArray;
  end;
end;

constructor TBoard.Create(b: TBoard);
var
  i, j: integer;
begin
  _n := b.N;
  _m := b.M;

  SetLength(_data, _n, _m);

  for i := 0 to High(b._data) do
  begin
    for j := 0 to High(b._data[i]) do
    begin
      _data[i, j] := b._data[i, j];
    end;
  end;
end;

destructor TBoard.Destroy;
begin
  inherited Destroy;
end;

function TBoard.InArea(x, y: integer): boolean;
begin
  Result := (x >= 0) and (x < N) and (y >= 0) and (y < M);
end;

function TBoard.IsWin: boolean;
var
  i, j: integer;
begin
  for i := 0 to N - 1 do
    for j := 0 to M - 1 do
      if _data[i, j] <> EMPTY then
        Exit(false);

  Result := true;
end;

procedure TBoard.Print;
var
  i, j: integer;
  s: UString;
begin
  for i := 0 to High(_data) do
  begin
    s := '';
    for j := 0 to High(_data[i]) do
      s += _data[i, j];

    WriteLn(s);
  end;
end;

procedure TBoard.Run;
begin
  repeat
    __drop;
  until __match = false;
end;

procedure TBoard.Swap(x1, y1, x2, y2: integer);
var
  temp: UChar;
begin
  temp := _data[x1, y1];
  _data[x1, y1] := _data[x2, y2];
  _data[x2, y2] := temp;
end;

procedure TBoard.__drop;
var
  j, cur, i: integer;
begin
  for j := 0 to M - 1 do
  begin
    cur := N - 1;

    for i := N - 1 downto 0 do
    begin
      if _data[i, j] <> EMPTY then
      begin
        Swap(i, j, cur, j);
        cur -= 1;
      end;
    end;
  end;
end;

function TBoard.__getItems(x, y: integer): UChar;
begin
  if not InArea(x, y) then
    raise Exception.Create('x, y are out of index in getData!');

  Result := _data[x, y];
end;

function TBoard.__match: boolean;
const
  D: array[0..1, 0..1] of integer = ((0, 1), (1, 0));
var
  x, y, i, newX1, newY1, newX2, newY2: integer;
  tag: array of array of boolean;
  res: boolean;
begin
  res := false;
  SetLength(tag, N, M);

  for x := 0 to N - 1 do
  begin
    for y := 0 to M - 1 do
    begin
      if _data[x, y] <> EMPTY then
      begin
        for i := 0 to High(D) do
        begin
          newX1 := x + D[i, 0];
          newY1 := y + D[i, 1];
          newX2 := newX1 + D[i, 0];
          newY2 := newY1 + D[i, 1];

          if InArea(newX1, newY1) and InArea(newX2, newY2) and
            (_data[x, y] = _data[newX1, newY1]) and (_data[x, y] = _data[newX2, newY2]) then
          begin
            tag[x, y] := true;
            tag[newX1, newY1] := true;
            tag[newX2, newY2] := true;

            res := true;
          end;
        end;
      end;
    end;
  end;

  for x := 0 to N - 1 do
  begin
    for y := 0 to M - 1 do
    begin
      if tag[x, y] then
        _data[x, y] := EMPTY;
    end;
  end;

  Result := res;
end;

end.
