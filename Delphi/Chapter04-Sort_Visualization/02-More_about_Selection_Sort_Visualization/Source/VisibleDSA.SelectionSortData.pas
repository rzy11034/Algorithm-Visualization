unit VisibleDSA.SelectionSortData;

interface

uses
  System.SysUtils;

type
  TSelectionSortData = class
  private
    _numbers: TArray<integer>;

  public
    OrderedIndex: integer; // [0...orderedIndex) 是有序的
    CurrentCompareIndex: integer; // 当前正在比较的元素索引
    CurrentMinIndex: integer; // 当前找到的最小元素的索引

    constructor Create(n, randomBound: integer);

    procedure Swap(i, j: integer);

    function Length: integer;
    function GetValue(index: integer): integer;
  end;

implementation

{ TSelectionSortData }

constructor TSelectionSortData.Create(n, randomBound: integer);
var
  i: integer;
begin
  Randomize;

  SetLength(_numbers, n);

  for i := 0 to n - 1 do
    _numbers[i] := Random(randomBound) + 1;
end;

function TSelectionSortData.Length: integer;
begin
  Result := System.Length(_numbers);
end;

function TSelectionSortData.GetValue(index: integer): integer;
begin
  if (index < 0) or (index >= Length) then
    raise Exception.Create('Invalid index to access Sort Data.');

  Result := _numbers[index];
end;

procedure TSelectionSortData.Swap(i, j: integer);
var
  temp: integer;
begin
  temp := _numbers[j];
  _numbers[j] := _numbers[i];
  _numbers[i] := temp;
end;

end.
