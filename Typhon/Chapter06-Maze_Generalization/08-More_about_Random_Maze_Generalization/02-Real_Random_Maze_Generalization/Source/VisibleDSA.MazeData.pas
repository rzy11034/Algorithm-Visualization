unit VisibleDSA.MazeData;

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

  TMazeData = class(TObject)
  const
    ROAD: UChar = ' ';
    WALL: UChar = '#';

  private

    _n: integer;
    _m: integer;

    _entranceX: integer;
    _entranceY: integer;
    _exitX: integer;
    _exitY: integer;

    _maze: TArr2D_chr;
    _visited: TArr2D_bool;
    _inMist: TArr2D_bool;

  public
    constructor Create(n, m: integer);
    destructor Destroy; override;
    function GetMaze(i, j: integer): UChar;
    function InArea(x, y: integer): boolean;
    procedure PrintMaze;
    procedure OpenMist(x, y: integer);

    property N: integer read _N;
    property M: integer read _M;

    property EntranceX: integer read _entranceX;
    property EntranceY: integer read _entranceY;
    property ExitX: integer read _exitX;
    property ExitY: integer read _exitY;

    property Maze: TArr2D_chr read _maze write _maze;
    property Visited: TArr2D_bool read _visited write _visited;
    property InMist: TArr2D_bool read _InMist write _InMist;
  end;


implementation

{ TMazeData }

constructor TMazeData.Create(n, m: integer);
var
  i, j: integer;
begin
  if (n mod 2 = 0) or (m mod 2 = 0) then
    raise Exception.Create('Maze Generalization Algorihtm requires the width and height of the maze are odd numbers');

  _n := n;
  _m := m;

  SetLength(_maze, N, M);
  SetLength(_visited, N, M);
  SetLength(_InMist, N, M);

  for i := 0 to N - 1 do
  begin
    for j := 0 to M - 1 do
    begin
      if (i mod 2 = 1) and (j mod 2 = 1) then
        _maze[i, j] := ROAD
      else
        _maze[i, j] := WALL;

      _visited[i, j] := False;
      _inMist[i, j] := True;
    end;
  end;

  _entranceX := 1;
  _entranceY := 0;
  _exitX := N - 2;
  _exitY := M - 1;

  _maze[_entranceX, _entranceY] := ROAD;
  _maze[_exitX, _exitY] := ROAD;
end;

destructor TMazeData.Destroy;
begin
  inherited Destroy;
end;

function TMazeData.GetMaze(i, j: integer): UChar;
begin
  if not InArea(i, j) then
    raise Exception.Create('i or j is out of index in getMaze!');

  Result := _maze[i, j];
end;

function TMazeData.InArea(x, y: integer): boolean;
begin
  Result := (x >= 0) and (x < N) and (y >= 0) and (y < M);
end;

procedure TMazeData.OpenMist(x, y: integer);
var
  i, j: integer;
begin
  if not InArea(x, y) then
    raise Exception.Create('x or y is out of index in openMist function!');

  for i := x - 1 to x + 1 do
  begin
    for j := y - 1 to y + 1 do
    begin
      if InArea(x, y) then
        _inMist[i, j] := False;
    end;
  end;
end;

procedure TMazeData.PrintMaze;
var
  i, j: integer;
begin
  for i := 0 to High(_maze) do
  begin
    for j := 0 to High(_maze[i]) do
    begin
      Write(_maze[i, j]);
    end;

    WriteLn;
  end;
end;

end.
