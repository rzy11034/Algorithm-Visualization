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
  _data := TQuickSortData.Create(n, _height, ArrType.Identical);

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

  function __partition__(l, r: integer): integer;
  var
    v, j, i, p: integer;
  begin
    Randomize;

    p := Random(r - l + 1) + l;
    __setData(l, r, -1, p, -1, -1);

    _data.Swap(l, p);
    v := _data.GetValue(l);
    __setData(l, r, -1, l, -1, -1);

    // arr[l+1...i) <= v; arr(j...r] >= v
    i := l + 1;
    j := r;
    __setData(l, r, -1, l, i, j);
    while True do
    begin
      while (i <= r) and (_data.GetValue(i) < v) do
      begin
        i := i + 1;
        __setData(l, r, -1, l, i, j);
      end;

      while (j >= l + 1) and (_data.GetValue(j) > v) do
      begin
        j := j - 1;
        __setData(l, r, -1, l, i, j);
      end;

      if i > j then
        Break;

      _data.swap(i, j);
      i := i + 1;
      j := j - 1;
      __setData(l, r, -1, l, i, j);
    end;

    _data.Swap(l, j);
    __setData(l, r, j, -1, -1, -1);

    Result := j;
  end;

  procedure __quickSort2Ways__(l, r: integer);
  var
    p: integer;
  begin
    if l > r then
      Exit;

    if l = r then
    begin
      __setData(l, r, l, -1, -1, -1);
      Exit;
    end;

    __setData(l, r, -1, -1, -1, -1);

    p := __partition__(l, r);
    __quickSort2Ways__(l, p - 1);
    __quickSort2Ways__(p + 1, r);
  end;

begin
  __setData(-1, -1, -1, -1, -1, -1);
  __quickSort2Ways__(0, _data.Length - 1);
  __setData(-1, -1, -1, -1, -1, -1);
end;

procedure TAlgoVisualizer.__setData(l, r, fixedPivot, curPivot, curL, curR: integer);
begin
  _data.L := l;
  _data.R := r;
  _data.CurPivot := curPivot;
  _data.CurL := curL;
  _data.CurR := curR;

  if fixedPivot <> -1 then
    _data.FixedPivots[fixedPivot] := True;

  TAlgoVisHelper.Pause(10);
  AlgoForm.PaintBox.Repaint;
end;

end.
