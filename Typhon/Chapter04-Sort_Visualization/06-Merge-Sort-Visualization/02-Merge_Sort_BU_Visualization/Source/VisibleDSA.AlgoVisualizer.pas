unit VisibleDSA.AlgoVisualizer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
  Graphics,
  Forms,
  BGRACanvas2D,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.MergeSortData;

type
  TAlgoVisualizer = class(TObject)
  private
    _width: integer;
    _height: integer;
    _data: TMergeSortData;
    _form: TForm;

    procedure __setData(l, r, mergeIndex: integer);

  public
    constructor Create(form: TForm; sceneWidth, sceneHeight, n: integer);
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
    procedure Run;
  end;

implementation

uses
  VisibleDSA.AlgoForm;

type
  TArray_int = array of integer;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm; sceneWidth, sceneHeight, n: integer);
begin
  _form := form;
  _width := form.ClientWidth;
  _height := form.ClientHeight;
  _data := TMergeSortData.Create(n, _height);

  _form.Caption := 'Insertion Sort Visualization';

end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TBGRACanvas2D);
var
  w: integer;
  i: integer;
begin
  w := _width div _data.Length;

  for i := 0 to _data.Length - 1 do
  begin

    if (i >= _data.L) and (i <= _data.R) then
      TAlgoVisHelper.SetFill(CL_GREEN)
    else
      TAlgoVisHelper.SetFill(CL_GREY);

    if (i >= _data.L) and (i <= _data.MergeIndex) then
      TAlgoVisHelper.SetFill(CL_RED);

    TAlgoVisHelper.FillRectangle(canvas, i * w, _height - _data.GetValue(i), w - 1, _data.GetValue(i));
  end;
end;

procedure TAlgoVisualizer.Run;

  function __arrayCopyOfRange__(arr: TArray_int; l, r: integer): TArray_int;
  var
    res: TArray_int;
    i: integer;
  begin
    SetLength(res, r - l + 1);

    for i := l to r do
      res[i - l] := arr[i];

    Result := res;
  end;

  procedure __merge__(l, mid, r: integer);
  var
    aux: array of integer;
    i, leftIndex, rightIndex: integer;
  begin
    aux := __arrayCopyOfRange__(_data.Numbers, l, r);

    // 初始化，leftIndex 指向左半部分的起始索引位置 l；
    // rightIndex 指向右半部分起始索引位置 mid+1
    leftIndex := l;
    rightIndex := mid + 1;

    for i := l to r do
    begin
      if leftIndex > mid then // 如果左半部分元素已经全部处理完毕
      begin
        _data.Numbers[i] := aux[rightIndex - l];
        rightIndex += 1;
      end
      else if rightIndex > r then // 如果右半部分元素已经全部处理完毕
      begin
        _data.Numbers[i] := aux[leftIndex - l];
        leftIndex += 1;
      end
      else if aux[leftIndex - l] < aux[rightIndex - l] then // 左半部分所指元素 < 右半部分所指元素
      begin
        _data.Numbers[i] := aux[leftIndex - l];
        leftIndex += 1;
      end
      else // 左半部分所指元素 >= 右半部分所指元素
      begin
        _data.Numbers[i] := aux[rightIndex - l];
        rightIndex += 1;
      end;

      __setData(l, r, i);

      if AlgoForm.Stop then
        Exit;
    end;
  end;

var
  sz, i: integer;
begin
  __setData(-1, -1, -1);

  sz := 1;
  while sz < _data.Length do
  begin
    i := 0;
    while i < _data.Length - sz do
    begin
      // 对 arr[i...i+sz-1] 和 arr[i+sz...i+2*sz-1] 进行归并
      __merge__(i, i + sz - 1, Min(i + sz + sz - 1, _data.Length - 1));

      i += sz * 2;

      if AlgoForm.Stop then
        Exit;
    end;

    sz *= 2;

    if AlgoForm.Stop then
      Exit;
  end;

  __setData(0, _data.Length - 1, _data.Length - 1);
end;

procedure TAlgoVisualizer.__setData(l, r, mergeIndex: integer);
begin
  _data.L := l;
  _data.R := r;
  _data.MergeIndex := mergeIndex;

  TAlgoVisHelper.Pause(10);
  AlgoForm.BGRAVirtualScreen.RedrawBitmap;
end;

end.
