unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.SysUtils,
  System.Types,
  System.Math,
  FMX.Graphics,
  FMX.Forms,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.HeapSortData;

type
  TAlgoVisualizer = class(TObject)
  private
    _width: integer;
    _height: integer;
    _data: THeapSortData;
    _form: TForm;

    procedure __setData(heapIndex: integer);

  public
    constructor Create(form: TForm; sceneWidth, sceneHeight, n: integer);
    destructor Destroy; override;

    procedure Paint(canvas: TCanvas);
    procedure Run;
  end;

implementation

uses
  VisibleDSA.AlgoForm;

type
  TArray_int = TArray<integer>;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm; sceneWidth, sceneHeight, n: integer);
begin
  _form := form;
  _width := form.ClientWidth;
  _height := form.ClientHeight;
  _data := THeapSortData.Create(n, _height);

  _form.Caption := 'Heap Sort Visualization';
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TCanvas);
var
  w: integer;
  i: integer;
begin
  w := _width div _data.Length;

  for i := 0 to _data.Length - 1 do
  begin

    if i >= _data.HeapIndex then
      TAlgoVisHelper.SetFill(CL_RED)
    else
      TAlgoVisHelper.SetFill(CL_GREY);

    TAlgoVisHelper.FillRectangle(canvas, i * w, _height - _data.GetValue(i), w - 1, _data.GetValue(i));
  end;
end;

procedure TAlgoVisualizer.Run;
  procedure __shiftDown__(n, k: integer);
  var
    j: integer;
  begin
    while 2 * k + 1 < n do
    begin
      j := 2 * k + 1;
      if (j + 1 < n) and (_data.GetValue(j + 1) > _data.GetValue(j)) then
        j := j + 1;

      if _data.GetValue(k) >= _data.GetValue(j) then
        Break;

      _data.Swap(k, j);
      __setData(_data.HeapIndex);

      k := j;
    end;
  end;

var
  i: integer;
begin
  __setData(_data.Length);

  // 建堆
  for i := (_data.Length() - 1 - 1) div 2 downto 0 do
  begin
    __shiftDown__(_data.Length(), i);
  end;

  // 堆排序
  for i := _data.Length() - 1 downto 1 do
  begin
    _data.Swap(0, i);
    __shiftDown__(i, 0);
    __setData(i);
  end;

  __setData(0);
end;

procedure TAlgoVisualizer.__setData(heapIndex: integer);
begin
  _data.HeapIndex := heapIndex;

  TAlgoVisHelper.Pause(10);
  AlgoForm.PaintBox.Repaint;
end;

end.
