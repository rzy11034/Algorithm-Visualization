unit VisibleDSA.InsertionSortData;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  ArrType = (Default, NearlyOrdered);

  TInsertionSortData = class
  private
    _numbers: TArray<integer>;

  public
    OrderedIndex: integer; // [0...orderedIndex) 是有序的
    CurrentIndex: integer; // 当前元素的索引

    constructor Create(n, randomBound: integer; dataType: ArrType = NearlyOrdered);
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

{ TInsertionSortData }

constructor TInsertionSortData.Create(n, randomBound: integer; dataType: ArrType);
var
  i, swapTime, a, b: integer;
begin
  OrderedIndex := -1;
  CurrentIndex := -1;

  Randomize;
  SetLength(_numbers, n);

  for i := 0 to n - 1 do
    _numbers[i] := Random(randomBound) + 1;

  if dataType = ArrType.NearlyOrdered then
  begin
    TArrayHelper_int.Sort(_numbers);

    swapTime := Trunc(0.02 * n);
    for i := 0 to swapTime - 1 do
    begin
      a := Random(n);
      b := Random(n);

      Swap(a, b);
    end;
  end;
end;

destructor TInsertionSortData.Destroy;
begin
  inherited Destroy;
end;

function TInsertionSortData.Length: integer;
begin
  Result := System.Length(_numbers);
end;

function TInsertionSortData.GetValue(index: integer): integer;
begin
  if (index < 0) or (index >= Length) then
    raise Exception.Create('Invalid index to access Sort Data.');

  Result := _numbers[index];
end;

procedure TInsertionSortData.Swap(i, j: integer);
var
  temp: integer;
begin
  if (i < 0) or (i >= Self.Length) or (j < 0) or (j >= Self.Length) then
    raise Exception.Create('Invalid index to access Sort Data.');

  temp := _numbers[j];
  _numbers[j] := _numbers[i];
  _numbers[i] := temp;
end;

{ TArrayHelper_int }

class procedure TArrayHelper_int.Sort(arr: TArray<Integer>);
begin
  TArray.Sort<Integer>(arr);
end;

end.
