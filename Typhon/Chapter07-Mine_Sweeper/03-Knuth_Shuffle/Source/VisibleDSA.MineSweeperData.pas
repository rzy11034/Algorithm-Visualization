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
    PNG_0: UString = '0';
    PNG_1: UString = '1';
    PNG_2: UString = '2';
    PNG_3: UString = '3';
    PNG_4: UString = '4';
    PNG_5: UString = '5';
    PNG_6: UString = '6';
    PNG_7: UString = '7';
    PNG_8: UString = '8';
    PNG_BLOCK: UString = 'BLOCK';
    PNG_FLAG: UString = 'FLAG';
    PNG_MINE: UString = 'MINE';


  private
    _n: integer;
    _m: integer;

    _mines: TArr2D_bool;

    procedure _generateMines(mineNumber: integer);
    procedure _swap(x1, y1, x2, y2: integer);

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

  _generateMines(mineNumber);
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

procedure TMineSweeperData._generateMines(mineNumber: integer);
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

  for i := (_n * _m) - 1 downto 0 do
  begin
    { TODO -oAuthor : Note }
  end;
end;

procedure TMineSweeperData._swap(x1, y1, x2, y2: integer);
var
  temp: boolean;
begin
  temp := _mines[x1, y1];
  _mines[x1, y1] := _mines[x2, y2];
  _mines[x2, y2] := temp;
end;

end.
