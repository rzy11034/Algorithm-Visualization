unit MazeData;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  UChar = UnicodeChar;
  UString = UnicodeString;

  TMazeData = class(TObject)
  private
    _n: integer;
    _m: integer;

    _maze: array of array of UChar;

  public
    constructor Create(fileName: UString);
    destructor Destroy; override;
    function GetMaze(i, j: integer): UChar;
    function InArea(x, y: integer): boolean;

    property N: integer read _N;
    property M: integer read _M;
  end;

implementation

{ TMazeData }

constructor TMazeData.Create(fileName: UString);
var
  strlist: TStringList;
begin
  try

  finally
    FreeAndNil(strlist);
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

end.
