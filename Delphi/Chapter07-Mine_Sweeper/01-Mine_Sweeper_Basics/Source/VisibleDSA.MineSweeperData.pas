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
  private
    _maze: TArr2D_chr;
    _n: integer;
    _m: integer;

    _mines: TArr2D_bool;

  public
    constructor Create(n, m, mineNumber: integer);
    destructor Destroy; override;
    function InArea(x, y: integer): boolean;
    function IsMine(x, y: integer): boolean;

    property N: integer read _N;
    property M: integer read _M;

    property Maze: TArr2D_chr read _maze write _maze;
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

  _mines[0, 0] := true;
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
  if InArea(x, y) then
    raise Exception.Create('Out of index in isMine function!');

  Result := _mines[x, y];
end;

end.
