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
    FILE_NAME: UString = '..\..\..\..\..\Resources\maze_01.txt';

  private
    _n: integer;
    _m: integer;

    _entranceX: integer;
    _entranceY: integer;
    _exitX: integer;
    _exitY: integer;

    _maze: TArr2D_chr;

  public
    Path: TArr2D_bool;
    Visited: TArr2D_bool;

    constructor Create(fileName: UString);
    destructor Destroy; override;
    function GetMaze(i, j: integer): UChar;
    function InArea(x, y: integer): boolean;
    procedure PrintMaze;

    property N: integer read _N;
    property M: integer read _M;
    property EntranceX: integer read _entranceX;
    property EntranceY: integer read _entranceY;
    property ExitX: integer read _exitX;
    property ExitY: integer read _exitY;
  end;


implementation

{ TMazeData }

constructor TMazeData.Create(fileName: UString);
var
  strList: TStringList;
  line: UString;
  i, j: integer;
begin
  strList := TStringList.Create;
  try
    strList.LoadFromFile(string(fileName));
    _n := strList.Count;
    _m := UString(strList[0]).Length;

    SetLength(_maze, N, M);
    SetLength(Path, N, M);
    SetLength(Visited, N, M);

    for i := 0 to N - 1 do
    begin
      line := UString(strList[i]);

      for j := 0 to line.Length - 1 do
        _maze[i, j] := line.Chars[j];
    end;

    _entranceX := 1;
    _entranceY := 0;
    _exitX := N - 2;
    _exitY := M - 1;
  finally
    FreeAndNil(strList);
  end;
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
