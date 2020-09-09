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
    _showBoard: TBoard;

  public
    constructor Create;
    destructor Destroy; override;
    function InArea(x, y: integer): boolean;
    procedure Print;
    function Solve: boolean;

    property N: integer read _n;
    property M: integer read _m;
    property ShowBoard: TBoard read _showBoard;
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
  fileName := '..\..\..\..\..\Resources\boston_16.txt';

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

      _showBoard := TBoard.Create(_starterBoard);

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

procedure TGameData.Print;
begin
  _starterBoard.Print;
end;

function TGameData.Solve: boolean;
const
  D: array[0..2, 0..1] of integer = ((-1, 0), (0, 1), (0, -1));

  function __solve__(borad: TBoard; turn: integer): boolean;
  var
    x, y, newX, newY, i: integer;
    nextBorad: TBoard;
    swapString: UString;
  begin
    if borad = nil then
      raise Exception.Create('board can not be null in solve function!');

    if turn = 0 then
      Exit(borad.IsWin);

    if borad.IsWin then
      Exit(true);

    for x := 0 to N - 1 do
    begin
      for y := 0 to M - 1 do
      begin
        if borad.Items[x, y] <> TBoard.EMPTY then
        begin
          for i := 0 to High(D) do
          begin
            newX := x + D[i, 0];
            newY := y + D[i, 1];

            if InArea(newX, newY) then
            begin
              swapString := UString(Format('swap (%d, %d) and (%d, %d)', [x, y, newX, newY]));
              nextBorad := TBoard.Create(borad, borad, swapString);
              nextBorad.Swap(x, y, newX, newY);
              nextBorad.Run;
            end;

            if __solve__(nextBorad, turn - 1) then
              Exit(true);
          end;
        end;
      end;
    end;

    Result := false;
  end;

begin
  if _maxTurn < 0 then
    Exit(false);

  Result := __solve__(TBoard.Create(_starterBoard), _maxTurn);
end;

end.
