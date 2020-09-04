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
    PNG_0: USTRING = '0';
    PNG_1: USTRING = '1';
    PNG_2: USTRING = '2';
    PNG_3: USTRING = '3';
    PNG_4: USTRING = '4';
    PNG_5: USTRING = '5';
    PNG_6: USTRING = '6';
    PNG_7: USTRING = '7';
    PNG_8: USTRING = '8';
    PNG_BLOCK: USTRING = 'BLOCK';
    PNG_FLAG: USTRING = 'FLAG';
    PNG_MINE: USTRING = 'MINE';


  private
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
  if not InArea(x, y) then
    raise Exception.Create('Out of index in isMine function!');

  Result := _mines[x, y];
end;

end.
