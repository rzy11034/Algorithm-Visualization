unit VisibleDSA.AlgoVisualizer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  Forms,
  BGRACanvas2D,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.SelectionSortData;

type
  TAlgoVisualizer = class(TObject)
  private
    _width: integer;
    _height: integer;
    _data: TSelectionSortData;
    _form: TForm;

    procedure __setData(orderedIndex, currentMinIndex, currentCompareIndex: integer);

  public
    constructor Create(form: TForm; sceneWidth, sceneHeight, n: integer);
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
    procedure Run;
  end;

implementation

uses
  VisibleDSA.AlgoForm;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm; sceneWidth, sceneHeight, n: integer);
begin
  _form := form;
  _width := form.ClientWidth;
  _height := form.ClientHeight;
  _data := TSelectionSortData.Create(n, _height);

  _form.Caption := 'Selection Sort Visualization';

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

    if i < _data.OrderedIndex then
      TAlgoVisHelper.SetFill(CL_RED)
    else
      TAlgoVisHelper.SetFill(CL_GREY);

    if i = _data.CurrentMinIndex then
      TAlgoVisHelper.SetFill(CL_LIGHTBLUE);

    if i = _data.CurrentCompareIndex then
      TAlgoVisHelper.SetFill(CL_INDIGO);

    TAlgoVisHelper.FillRectangle(canvas, i * w, _height - _data.GetValue(i), w - 1, _data.GetValue(i));
  end;
end;

procedure TAlgoVisualizer.Run;
var
  i, j, minIndex: integer;
begin
  __setData(0, -1, -1);

  for i := 0 to _data.Length - 1 do
  begin
    minIndex := i;
    __setData(i, minIndex, -1);

    for j := i + 1 to _data.Length - 1 do
    begin
      __setData(i, minIndex, j);

      if _data.GetValue(j) < _data.GetValue(minIndex) then
      begin
        minIndex := j;
        __setData(i, minIndex, j);
      end;

      if AlgoForm.Stop then
        Exit;
    end;

    _data.swap(i, minIndex);
    __setData(i+1, -1, -1);

    if AlgoForm.Stop then
      Exit;
  end;

  __setData(_data.Length, -1, -1);
end;

procedure TAlgoVisualizer.__setData(orderedIndex, currentMinIndex, currentCompareIndex: integer);
begin
  _data.OrderedIndex := orderedIndex;
  _data.CurrentMinIndex := currentMinIndex;
  _data.CurrentCompareIndex := currentCompareIndex;

  TAlgoVisHelper.Pause(40);
  AlgoForm.BGRAVirtualScreen.RedrawBitmap;
end;

end.
