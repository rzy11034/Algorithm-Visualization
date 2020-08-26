unit VisibleDSA.MergeSort;

interface

uses
  System.SysUtils;

type
  UChar = Char;
  UString = String;

  TArray_int = TArray<integer>;

  TMergeSort = class
  private
    // 将arr[l...mid]和arr[mid+1...r]两部分进行归并
    class procedure __merge(arr: TArray_int; l, mid, r: integer);

    // 递归使用归并排序,对arr[l...r]的范围进行排序
    class procedure __sort(arr: TArray_int; l, r, depth: integer);

    class function __repeatCharacters(character: UChar; length: integer): UString;

  public
    class procedure Sort(arr: TArray_int);

  end;

implementation

{ TMergeSort }

class procedure TMergeSort.Sort(arr: TArray_int);
var
  n: integer;
begin
  n := length(arr);

  __sort(arr, 0, n - 1, 0);
end;

class procedure TMergeSort.__merge(arr: TArray_int; l, mid, r: integer);
var
  aux: TArray_int;
  i: integer;
  leftIndex, rightIndex: integer;
begin
  SetLength(aux, r - l + 1);
  for i := l to r do
    aux[i - l] := arr[i];

  // 初始化，leftIndex指向左半部分的起始索引位置l；
  // rightIndex指向右半部分起始索引位置mid+1
  leftIndex := l;
  rightIndex := mid + 1;

  for i := l to r do
  begin
    if leftIndex > mid then // 如果左半部分元素已经全部处理完毕
    begin
      arr[i] := aux[rightIndex - l];
      Inc(rightIndex);
    end
    else if rightIndex > r then // 如果右半部分元素已经全部处理完毕
    begin
      arr[i] := aux[leftIndex - l];
      Inc(leftIndex);
    end
    else if aux[leftIndex - l] < aux[rightIndex - l] then // 左半部分所指元素 < 右半部分所指元素
    begin
      arr[i] := aux[leftIndex - l];
      Inc(leftIndex);
    end
    else // 左半部分所指元素 >= 右半部分所指元素
    begin
      arr[i] := aux[rightIndex - l];
      Inc(rightIndex);
    end;
  end;
end;

class function TMergeSort.__repeatCharacters(character: UChar; length: integer): UString;
var
  s: TStringBuilder;
  i: integer;
begin
  s := TStringBuilder.Create;
  try
    for i := 0 to length - 1 do
    begin
      s.Append(character);
    end;

    Result := s.ToString;
  finally
    s.Free;
  end;
end;

class procedure TMergeSort.__sort(arr: TArray_int; l, r, depth: integer);
var
  mid: integer;
begin
  Write(__repeatCharacters('-', depth * 2));
  Writeln(' Deal with [' + l.ToString + ', ' + r.ToString + ']');

  if (l >= r) then
    Exit;

  mid := l + (r - l) shr 1;
  __sort(arr, l, mid, depth + 1);
  __sort(arr, mid + 1, r, depth + 1);
  __merge(arr, l, mid, r);
end;

end.
