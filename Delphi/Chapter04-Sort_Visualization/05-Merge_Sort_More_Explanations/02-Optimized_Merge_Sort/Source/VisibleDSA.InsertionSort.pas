unit VisibleDSA.InsertionSort;

interface

uses
  System.SysUtils;

type
  TArray_int = TArray<integer>;

  TInsertionSort = class
  public
    // 对整个arr数组使用InsertionSort排序
    class procedure Sort(arr: TArray_int); overload;

    // 对arr[l...r]的区间使用InsertionSort排序
    class procedure Sort(arr: TArray_int; l, r: integer); overload;
  end;

implementation

{ TInsertionSort }

class procedure TInsertionSort.Sort(arr: TArray_int);
var
  e, i, j: integer;
begin
  for i := 1 to Length(arr) - 1 do
  begin
    e := arr[i];

    for j := i downto 1 do
    begin
      if arr[j - 1] > e then
        arr[j] := arr[j - 1]
      else
        Break;
    end;

    arr[j] := e;
  end;
end;

class procedure TInsertionSort.Sort(arr: TArray_int; l, r: integer);
var
  e, i, j: integer;
begin
  for i := l + 1 to r do
  begin
    e := arr[i];

    for j := i downto l + 1 do
    begin
      if arr[j - 1] > e then
        arr[j] := arr[j - 1]
      else
        Break;
    end;

    arr[j] := e;
  end;
end;

end.
