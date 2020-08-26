unit VisibleDSA.SortTestHelper;

interface

uses
  System.SysUtils;

type
  TArray_int = TArray<integer>;

  TSortTestHelper = class
  public
    // 生成有n个元素的随机数组,每个元素的随机范围为[rangeL, rangeR]
    class function GenerateRandomArray(n, rangeL, rangeR: integer): TArray_int;

    // 生成一个近乎有序的数组
    // 首先生成一个含有[0...n-1]的完全有序数组, 之后随机交换swapTimes对数据
    // swapTimes定义了数组的无序程度:
    // swapTimes == 0 时, 数组完全有序
    // swapTimes 越大, 数组越趋向于无序
    class function GenerateNearlyOrderedArray(n, swapTimes: integer): TArray_int;

    // 打印arr数组的所有内容
    class procedure PrintArray(arr: TArray_int);

    // 判断arr数组是否有序
    class function IsSorted(arr: TArray_int): boolean;
  end;

implementation

{ TSortTestHelper }

class function TSortTestHelper.GenerateNearlyOrderedArray(n, swapTimes: integer): TArray_int;
var
  arr: TArray_int;
  i, a, b, temp: integer;
begin
  SetLength(arr, n);

  for i := low(arr) to High(arr) do
    arr[i] := i;

  Randomize;
  for i := 0 to swapTimes - 1 do
  begin
    a := Random(i);
    b := Random(i);

    temp := arr[a];
    arr[a] := arr[b];
    arr[b] := temp;
  end;

  Result := arr;
end;

class function TSortTestHelper.GenerateRandomArray(n, rangeL, rangeR: integer): TArray_int;
var
  arr: TArray_int;
  i: integer;
begin
  assert(rangeL <= rangeR);

  SetLength(arr, n);

  Randomize;
  for i := low(arr) to High(arr) do
    arr[i] := Random((rangeR - rangeL + 1) + rangeL);

  Result := arr;
end;

class function TSortTestHelper.IsSorted(arr: TArray_int): boolean;
var
  i: integer;
begin
  for i := 0 to Length(arr) - 2 do
  begin
    if arr[i] > arr[i + 1] then
      Exit(false);
  end;

  Result := true;
end;

class procedure TSortTestHelper.PrintArray(arr: TArray_int);
var
  i: integer;
begin
  for i := 0 to Length(arr) - 1 do
  begin
    Write(arr[i].ToString);
    Write(' ');
  end;

  Writeln;
end;

end.
