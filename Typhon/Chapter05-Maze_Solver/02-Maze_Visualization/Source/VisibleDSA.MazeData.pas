unit VisibleDSA.MazeData;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils.UString;

type
  TMazeData = class(TObject)
  const
    ROAD: UChar = ' ';
    WALL: UChar = '#';
    FILE_NAME: UString = '..\..\..\..\..\Resources\maze_01.txt';

  private
    _n: integer;
    _m: integer;

    _maze: array of array of UChar;

  public
    constructor Create(fileName: UString);
    destructor Destroy; override;
    function GetMaze(i, j: integer): UChar;
    function InArea(x, y: integer): boolean;
    procedure PrintMaze;

    property N: integer read _N;
    property M: integer read _M;
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
    strList.LoadFromFile(fileName);
    _n := strList.Count;
    _m := UString(strList[0]).Length;

    SetLength(_maze, N, M);

    for i := 0 to N - 1 do
    begin
      line := strList[i];

      for j := 0 to line.Length - 1 do
        _maze[i, j] := line.Chars[j];
    end;
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
