unit VisibleDSA.QuickSortData;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  ArrType = (Default, NearlyOrdered, Identical);

  TQuickSortData = class
  public
    L: integer;
    R: integer;
    FixedPivots: TArray<boolean>;
    CurPivot: integer;
    CurElement: integer;

    Numbers: TArray<integer>;

    constructor Create(n, randomBound: integer; dataType: ArrType = Default);
    destructor Destroy; override;

    procedure Swap(i, j: integer);

    function Length: integer;
    function GetValue(index: integer): integer;
  end;

implementation

type
  TArrayHelper_int = class
  public
    class procedure Sort(arr: TArray<Integer>);
  end;

{ TQuickSortData }

constructor TQuickSortData.Create(n, randomBound: integer; dataType: ArrType);
var
  i, swapTime, a, b: integer;
  lBound, rBound, avgNumber: integer;
begin
  Randomize;
  SetLength(Numbers, n);
  SetLength(FixedPivots, n);

  lBound := 1;
  rBound := randomBound;

  if dataType = ArrType.Identical then
  begin
    avgNumber := (rBound + lBound) div 2;
    lBound := avgNumber;
    rBound := avgNumber;
  end;

  for i := 0 to n - 1 do
  begin
    Numbers[i] := Random(rBound - lBound + 1) + lBound;
    FixedPivots[i] := false;
  end;

  if dataType = ArrType.NearlyOrdered then
  begin
    TArrayHelper_int.Sort(Numbers);

    swapTime := Trunc(0.02 * n);
    for i := 0 to swapTime - 1 do
    begin
      a := Random(n);
      b := Random(n);

      Swap(a, b);
    end;
  end;
end;

destructor TQuickSortData.Destroy;
begin
  inherited Destroy;
end;

function TQuickSortData.Length: integer;
begin
  Result := System.Length(Numbers);
end;

function TQuickSortData.GetValue(index: integer): integer;
begin
  if (index < 0) or (index >= Length) then
    raise Exception.Create('Invalid index to access Sort Data.');

  Result := Numbers[index];
end;

procedure TQuickSortData.Swap(i, j: integer);
var
  temp: integer;
begin
  if (i < 0) or (i >= Self.Length) or (j < 0) or (j >= Self.Length) then
    raise Exception.Create('Invalid index to access Sort Data.');

  temp := Numbers[j];
  Numbers[j] := Numbers[i];
  Numbers[i] := temp;
end;

{ TArrayHelper_int }

class procedure TArrayHelper_int.Sort(arr: TArray<Integer>);
begin
  TArray.Sort<Integer>(arr);
end;

end.
