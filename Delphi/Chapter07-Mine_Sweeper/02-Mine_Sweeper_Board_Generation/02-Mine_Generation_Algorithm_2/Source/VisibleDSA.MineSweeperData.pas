unit VisibleDSA.MineSweeperData;

interface

uses
  System.Classes,
  System.SysUtils;

type
  UString = string;
  UChar = Char;

  TArr2D_int = TArray<TArray<integer>>;
  TArr2D_chr = TArray<TArray<UChar>>;
  TArr2D_bool = TArray<TArray<boolean>>;

  TMineSweeperData = class(TObject)
  public const
    PNG_0: USTRING = 'PngImage_1';
    PNG_1: USTRING = 'PngImage_2';
    PNG_2: USTRING = 'PngImage_3';
    PNG_3: USTRING = 'PngImage_4';
    PNG_4: USTRING = 'PngImage_5';
    PNG_5: USTRING = 'PngImage_6';
    PNG_6: USTRING = 'PngImage_7';
    PNG_7: USTRING = 'PngImage_8';
    PNG_8: USTRING = 'PngImage_9';
    PNG_BLOCK: USTRING = 'PngImage_10';
    PNG_FLAG: USTRING = 'PngImage_11';
    PNG_MINE: USTRING = 'PngImage_12';

  private
    _n: integer;
    _m: integer;

    _mines: TArr2D_bool;

    procedure __generateMines(mineNumber: integer);
    procedure __swap(x1, y1, x2, y2: integer);

  public
    constructor Create(n, m, mineNumber: integer);
    destructor Destroy; override;
    function InArea(x, y: integer): boolean;
    function IsMine(x, y: integer): boolean;

    property N: integer read _N;
    property M: integer read _M;

    property Mines: TArr2D_bool read _mines write _mines;
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

  for i := 0 to N - 1 do
  begin
    for j := 0 to M - 1 do
    begin
      _mines[i, j] := false;
    end;
  end;

  __generateMines(mineNumber);
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

procedure TMineSweeperData.__generateMines(mineNumber: integer);
var
  i: integer;
  x1, y1, x2, y2, swapTimes: integer;
begin
  for i := 0 to mineNumber - 1 do
  begin
    x1 := i div _m;
    y1 := i mod _m;
    _mines[x1, y1] := true;
  end;

  swapTimes := 10000;
  for i := 0 to swapTimes-1 do
  begin
    x1 := Random(_m);
    y1 := Random(_n);

    x2 := Random(_m);
    y2 := Random(_n);

    __swap(x1, y1, x2, y2);
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
