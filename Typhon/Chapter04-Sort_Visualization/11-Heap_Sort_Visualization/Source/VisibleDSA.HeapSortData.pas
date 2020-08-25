unit VisibleDSA.HeapSortData;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type
  TArray_int = array of integer;
  ArrType = (Default, NearlyOrdered, Identical);

  THeapSortData = class
  public
    HeapIndex: integer;   // numbers[heapIndex...N) 已经排好序
    Numbers: TArray_int;

    constructor Create(n, randomBound: integer; dataType: ArrType = ArrType.Default);
    destructor Destroy; override;

    procedure Swap(i, j: integer);

    function Length: integer;
    function GetValue(index: integer): integer;
  end;

implementation

type
  TArrayHelper_int = specialize TArrayHelper<integer>;

{ THeapSortData }

constructor THeapSortData.Create(n, randomBound: integer; dataType: ArrType);
var
  i, swapTime, a, b: integer;
  lBound, rBound, avgNumber: integer;
begin
  Randomize;
  SetLength(Numbers, n);

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

destructor THeapSortData.Destroy;
begin
  inherited Destroy;
end;

function THeapSortData.Length: integer;
begin
  Result := System.Length(Numbers);
end;

function THeapSortData.GetValue(index: integer): integer;
begin
  if (index < 0) or (index >= Length) then
    raise Exception.Create('Invalid index to access Sort Data.');

  Result := Numbers[index];
end;

procedure THeapSortData.Swap(i, j: integer);
var
  temp: integer;
begin
  if (i < 0) or (i >= Self.Length) or (j < 0) or (j >= Self.Length) then
    raise Exception.Create('Invalid index to access Sort Data.');

  temp := Numbers[j];
  Numbers[j] := Numbers[i];
  Numbers[i] := temp;
end;

end.
