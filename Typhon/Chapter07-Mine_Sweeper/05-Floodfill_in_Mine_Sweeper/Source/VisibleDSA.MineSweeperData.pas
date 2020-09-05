unit VisibleDSA.MineSweeperData;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils.UString;

type
  TArr2D_int = array of array of integer;
  TArr2D_chr = array of array of UChar;
  TArr2D_bool = array of array of boolean;

  TMineSweeperData = class(TObject)
  public const
    PNG_BLOCK: UString = 'BLOCK';
    PNG_FLAG: UString = 'FLAG';
    PNG_MINE: UString = 'MINE';

  public
    class function PNG_NUM(n: integer): UString;

  private
    _n: integer;
    _m: integer;

    _flags: TArr2D_bool;
    _mines: TArr2D_bool;
    _numbers: TArr2D_int;
    _opened: TArr2D_bool;

    procedure __calculateNumbers;
    procedure __generateMines(mineNumber: integer);
    procedure __swap(x1, y1, x2, y2: integer);

  public
    constructor Create(n, m, mineNumber: integer);
    destructor Destroy; override;
    function InArea(x, y: integer): boolean;
    function IsMine(x, y: integer): boolean;
    procedure Open(x, y: integer);

    property N: integer read _N;
    property M: integer read _M;

    property Mines: TArr2D_bool read _mines write _mines;
    property Opened: TArr2D_bool read _opened write _opened;
    property Flags: TArr2D_bool read _flags write _flags;
    property Numbers: TArr2D_int read _numbers;
  end;

implementation

{ TMineSweeperData }

constructor TMineSweeperData.Create(n, m, mineNumber: integer);
var
  i, j: integer;
begin
  if (n <= 0) or (m <= 0) then
    raise Exception.Create('Mine sweeper size is invalid!');

  if (n < 0) or (mineNumber > n * m) then
    raise Exception.Create('Mine number is larger than the size of mine sweeper board!');

  _n := n;
  _m := m;

  SetLength(_mines, N, M);
  SetLength(_opened, N, M);
  SetLength(_flags, N, M);
  SetLength(_numbers, N, M);

  for i := 0 to N - 1 do
  begin
    for j := 0 to M - 1 do
    begin
      _mines[i, j] := false;
      _opened[i, j] := false;
      _flags[i, j] := false;
      _numbers[i, j] := 0;
    end;
  end;

  __generateMines(mineNumber);
  __calculateNumbers;
end;

destructor TMineSweeperData.Destroy;
begin
  inherited Destroy;
end;

function TMineSweeperData.InArea(x, y: integer): boolean;
begin
  Result := (x >= 0) and (x < N) and (y >= 0) and (y < M);
end;

function TMineSweeperData.IsMine(x, y: integer): boolean;
begin
  if not InArea(x, y) then
    raise Exception.Create('Out of index in isMine function!');

  Result := _mines[x, y];
end;

procedure TMineSweeperData.Open(x, y: integer);
var
  i, j: integer;
begin
  if not InArea(x, y) then
    raise Exception.Create('Out of index in open function!');

  if IsMine(x, y) then
    raise Exception.Create('Cannot open an mine block in open function.');

  _opened[x, y] := true;

  if _numbers[x, y] > 0 then
    Exit;

  for i := x - 1 to x + 1 do
  begin
    for j := y - 1 to y + 1 do
    begin
      if InArea(i, j) and (not IsMine(i, j)) and (not _opened[i, j]) then
        Open(i, j);
    end;
  end;
end;

class function TMineSweeperData.PNG_NUM(n: integer): UString;
begin
  if (n < 0) or (n > 8) then
    raise Exception.Create('No such a number image!');

  Result := UString(n.ToString);
end;

procedure TMineSweeperData.__calculateNumbers;
var
  i, j, jj, ii: integer;
begin
  for i := 0 to N - 1 do
  begin
    for j := 0 to M - 1 do
    begin
      if IsMine(i, j) then
        _numbers[i, j] := -1;

      for ii := i - 1 to i + 1 do
      begin
        for jj := j - 1 to j + 1 do
        begin
          if InArea(ii, jj) and IsMine(ii, jj) then
            _numbers[i, j] += 1;
        end;
      end;
    end;
  end;
end;

procedure TMineSweeperData.__generateMines(mineNumber: integer);
var
  i: integer;
  x, y, iX, iY, randX, randY, rand: integer;
begin
  for i := 0 to mineNumber - 1 do
  begin
    x := i div _m;
    y := i mod _m;
    _mines[x, y] := true;
  end;

  for i := (_n * _m) - 1 downto 0 do
  begin
    iX := i div _m;
    iY := i mod _m;

    rand := Random(i + 1);
    randX := rand div _m;
    randY := rand mod _m;

    __swap(iX, iY, randX, randY);
  end;
end;

procedure TMineSweeperData.__swap(x1, y1, x2, y2: integer);
var
  temp: boolean;
begin
  temp := _mines[x1, y1];
  _mines[x1, y1] := _mines[x2, y2];
  _mines[x2, y2] := temp;
end;

end.
