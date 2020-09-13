unit VisibleDSA.Board;

interface

uses
  System.Classes,
  System.SysUtils,
  DeepStar.Utils.UString;

type
  TArray_str = TArray<UString>;
  TArr2D_chr = TArray<TArray<UChar>>;

  TBoard = class(TObject)
  public const
    EMPTY: UChar = '.';

  private
    _n: integer;
    _m: integer;
    _data: TArr2D_chr;
    _preBorad: TBoard;
    _swapString: UString;

    function __getItems(x, y: integer): UChar;
    procedure __drop;
    function __match: boolean;

  public
    constructor Create(board: TBoard; preBorad: TBoard = nil; swapString: UString = ''); overload;
    constructor Create(strs: TArray_str); overload;
    destructor Destroy; override;
    function InArea(x, y: integer): boolean;
    procedure Print;
    function IsWin: boolean;
    procedure Run;
    procedure Swap(x1, y1, x2, y2: integer);
    procedure PrintSwapInfo;

    property N: integer read _n;
    property M: integer read _m;
    property Items[x, y: integer]: UChar read __getItems; default;
  end;

implementation

uses
  VisibleDSA.AlgoForm;

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

constructor TBoard.Create(board: TBoard; preBorad: TBoard = nil; swapString: UString = '');
var
  i, j: integer;
begin
  _n := board.N;
  _m := board.M;

  SetLength(_data, _n, _m);

  for i := 0 to High(board._data) do
    for j := 0 to High(board._data[i]) do
      _data[i, j] := board._data[i, j];

  _preBorad := preBorad;
  _swapString := swapString;
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
  for i := 0 to High(_data) do
    for j := 0 to High(_data[i]) do
      if _data[i, j] <> EMPTY then
        Exit(false);

  PrintSwapInfo;
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
      s := s + _data[i, j];

    WriteLn(s);
  end;
end;

procedure TBoard.PrintSwapInfo;
begin
  if _preBorad <> nil then
    _preBorad.PrintSwapInfo;

  WriteLn(_swapString);
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
        cur := cur - 1;
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
  D: array [0 .. 1, 0 .. 1] of integer = ((0, 1), (1, 0));
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
