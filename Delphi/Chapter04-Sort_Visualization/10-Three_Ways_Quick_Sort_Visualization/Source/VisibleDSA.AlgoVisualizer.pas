unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.SysUtils,
  System.Types,
  System.Math,
  FMX.Graphics,
  FMX.Forms,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.QuickSortData;

type
  TAlgoVisualizer = class(TObject)
  private
    _width: integer;
    _height: integer;
    _data: TQuickSortData;
    _form: TForm;

    procedure __setData(l, r, fixedPivot, curPivot, curL, curR: integer);

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
  _data := TQuickSortData.Create(n, _height);

  _form.Caption := 'Quick Sort Visualization';
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

    if (i >= _data.L) and (i <= _data.R) then
      TAlgoVisHelper.SetFill(CL_GREEN)
    else
      TAlgoVisHelper.SetFill(CL_GREY);

    if i = _data.CurPivot then
      TAlgoVisHelper.SetFill(CL_INDIGO);
    if (i >= _data.L + 1) and (i <= _data.CurL) then
      TAlgoVisHelper.SetFill(CL_LIGHTBLUE);
    if (i >= _data.CurR) and (i <= _data.R) then
      TAlgoVisHelper.SetFill(CL_LIGHTBLUE);
    if _data.FixedPivots[i] then
      TAlgoVisHelper.SetFill(CL_RED);

    TAlgoVisHelper.FillRectangle(canvas, i * w, _height - _data.GetValue(i), w - 1, _data.GetValue(i));
  end;
end;

procedure TAlgoVisualizer.Run;

  procedure __quickSort3Ways__(l, r: integer);
  var
    p, v, lt, gt, i: integer;
  begin
    if l > r then
      Exit;

    if l = r then
    begin
      __setData(l, r, l, -1, -1, -1);
      Exit;
    end;

    __setData(l, r, -1, -1, -1, -1);

    // 随机在arr[l...r]的范围中, 选择一个数值作为标定点pivot
    Randomize;
    p := Random(r - l + 1) + l;
    __setData(l, r, -1, p, -1, -1);

    _data.Swap(l, p);
    v := _data.GetValue(l);
    __setData(l, r, -1, -1, -1, -1);

    lt := l; // arr[l+1...lt] < v
    gt := r + 1; // arr[gt...r] > v
    i := l + 1; // arr[lt+1...i) == v
    __setData(l, r, -1, l, lt, gt);

    while (i < gt) do
    begin
      if _data.GetValue(i) < v then
      begin
        _data.Swap(i, lt + 1);
        i := i + 1;
        lt := lt + 1;
      end
      else if _data.GetValue(i) > v then
      begin
        _data.Swap(i, gt - 1);
        gt := gt - 1;
      end
      else // arr[i] == v
      begin
        i := i + 1;
      end;

      __setData(l, r, -1, l, i, gt);
    end;

    _data.Swap(l, lt);
    __setData(l, r, lt, -1, -1, -1);

    __quickSort3Ways__(l, lt - 1);
    __quickSort3Ways__(gt, r);
  end;

begin
  __setData(-1, -1, -1, -1, -1, -1);
  __quickSort3Ways__(0, _data.Length - 1);
  __setData(-1, -1, -1, -1, -1, -1);
end;

procedure TAlgoVisualizer.__setData(l, r, fixedPivot, curPivot, curL, curR: integer);
var
  i: integer;
begin
  _data.L := l;
  _data.R := r;
  _data.CurPivot := curPivot;
  _data.CurL := curL;
  _data.CurR := curR;

  if fixedPivot <> -1 then
  begin
    _data.FixedPivots[fixedPivot] := True;

    i := fixedPivot;
    while (i < _data.Length) and (_data.GetValue(i) = _data.GetValue(fixedPivot)) do
    begin
      _data.FixedPivots[i] := True;
      i := i + 1;
    end;
  end;

  TAlgoVisHelper.Pause(10);
  AlgoForm.PaintBox.Repaint;
end;

end.
