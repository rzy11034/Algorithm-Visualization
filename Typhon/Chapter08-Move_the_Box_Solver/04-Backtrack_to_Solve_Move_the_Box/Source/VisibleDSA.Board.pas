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

  public
    constructor Create(strs: TArray_str); overload;
    constructor Create(b: TBoard); overload;
    destructor Destroy; override;
    function InArea(x, y: integer): boolean;
    function IsWin: boolean;
    procedure Print;

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
  for i := 0 to High(_data) do
    for j := 0 to High(_data) do
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

    //WriteLn(s);
  end;
end;

function TBoard.__getItems(x, y: integer): UChar;
begin
  if not InArea(x, y) then
    raise Exception.Create('x, y are out of index in getData!');

  Result := _data[x, y];
end;

end.
