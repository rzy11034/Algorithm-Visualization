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

    procedure __setData(l, r, fixedPivot, curPivot, curElement: integer);

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
    if i = _data.CurElement then
      TAlgoVisHelper.SetFill(CL_LIGHTBLUE);
    if _data.FixedPivots[i] then
      TAlgoVisHelper.SetFill(CL_RED);

    TAlgoVisHelper.FillRectangle(canvas, i * w, _height - _data.GetValue(i), w - 1, _data.GetValue(i));
  end;
end;

procedure TAlgoVisualizer.Run;

  function __partition__(l, r: integer): integer;
  var
    v, j, i: integer;
  begin
    v := _data.GetValue(l);
    __setData(l, r, -1, l, -1);

    j := l; // arr[l+1...j] < v ; arr[j+1...i) > v
    for i := l + 1 to r do
    begin
      __setData(l, r, -1, l, i);

      if (_data.GetValue(i) < v) then
      begin
        j := j + 1;
        _data.Swap(j, i);
        __setData(l, r, -1, l, i);
      end;
    end;

    _data.swap(l, j);
    __setData(l, r, j, -1, -1);

    Result := j;
  end;

  procedure __quickSort__(l, r: integer);
  var
    p: integer;
  begin
    if l > r then
      Exit;

    if l = r then
    begin
      __setData(l, r, l, -1, -1);
      Exit;
    end;

    __setData(l, r, -1, -1, -1);

    p := __partition__(l, r);
    __quickSort__(l, p - 1);
    __quickSort__(p + 1, r);
  end;

begin
  __setData(-1, -1, -1, -1, -1);
  __quickSort__(0, _data.Length - 1);
  __setData(-1, -1, -1, -1, -1);
end;

procedure TAlgoVisualizer.__setData(l, r, fixedPivot, curPivot, curElement: integer);
begin
  _data.L := l;
  _data.R := r;
  _data.CurPivot := curPivot;
  _data.CurElement := curElement;

  if fixedPivot <> -1 then
    _data.FixedPivots[fixedPivot] := true;

  TAlgoVisHelper.Pause(10);
  AlgoForm.PaintBox.Repaint;
end;

end.
