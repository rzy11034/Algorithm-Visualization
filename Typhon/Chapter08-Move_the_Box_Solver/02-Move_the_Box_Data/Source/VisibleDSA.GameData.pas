unit VisibleDSA.GameData;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils.UString,
  VisibleDSA.Board;

type
  TGameData = class(TObject)
  private
    _m: integer;
    _n: integer;
    _maxTurn: integer;
    _starterBoard: TBoard;

  public
    constructor Create;
    destructor Destroy; override;
    function InArea(x, y: integer): boolean;
    procedure PrintStarterBoard;

    property N: integer read _n;
    property M: integer read _m;
  end;

implementation

{ TGameData }

constructor TGameData.Create;
var
  fileName: UString;
  strList1, strList2: TStringList;
  i: integer;
  strs: array of UString;
begin
  fileName := '..\..\..\..\..\Resources\boston_09.txt';

  strList1 := TStringList.Create;
  try
    strList1.LoadFromFile(string(fileName));
    _maxTurn := strList1[0].ToInteger;

    strList2 := TStringList.Create;
    try
      for i := 1 to strList1.Count - 1 do
        strList2.Add(strList1[i]);

      SetLength(strs, strList2.Count);

      for i := 0 to strList2.Count - 1 do
        strs[i] := UString(strList2[i]);

      _starterBoard := TBoard.Create(strs);

      _n := _starterBoard.N;
      _m := _starterBoard.M;

    finally
      strList2.Free;
    end;
  finally
    strList1.Free;
  end;
end;

destructor TGameData.Destroy;
begin
  inherited Destroy;
end;

function TGameData.InArea(x, y: integer): boolean;
begin
  Result := (x >= 0) and (x < N) and (y >= 0) and (y < M);
end;

procedure TGameData.PrintStarterBoard;
begin
  _starterBoard.Print;
end;

end.
