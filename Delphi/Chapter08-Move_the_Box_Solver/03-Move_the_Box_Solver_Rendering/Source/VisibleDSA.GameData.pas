unit VisibleDSA.GameData;

interface

uses
  System.Classes,
  System.SysUtils,
  VisibleDSA.Board,
  DeepStar.Utils.UString;

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
  strs: TArray<UString>;
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
  FreeAndNil(_starterBoard);
  FreeAndNil(_showBoard);
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

end.
