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
  VisibleDSA.InsertionSortData;

type
  TAlgoVisualizer = class(TObject)
  private
    _width: integer;
    _height: integer;
    _data: TInsertionSortData;
    _form: TForm;

    procedure _setData(orderedIndex, currentIndex: integer);

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
  _data := TInsertionSortData.Create(n, _height);

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

    if i < _data.OrderedIndex then
      TAlgoVisHelper.SetFill(CL_RED)
    else
      TAlgoVisHelper.SetFill(CL_GREY);

    if i = _data.CurrentIndex then
      TAlgoVisHelper.SetFill(CL_LIGHTBLUE);

    TAlgoVisHelper.FillRectangle(canvas, i * w, _height - _data.GetValue(i), w - 1, _data.GetValue(i));
  end;
end;

procedure TAlgoVisualizer.Run;
var
  i, j: integer;
begin
  _setData(0, -1);

  for i := 0 to _data.Length - 1 do
  begin

    j := i;
    while (j > 0) and (_data.GetValue(j) < _data.GetValue(j - 1)) do
    begin
      _data.swap(j, j - 1);
      _setData(i + 1, j - 1);

      j -= 1;

      if AlgoForm.Stop then
        Exit;
    end;

    if AlgoForm.Stop then
      Exit;
  end;

  _setData(_data.Length, -1);
end;

procedure TAlgoVisualizer._setData(orderedIndex, currentIndex: integer);
begin
  _data.OrderedIndex := orderedIndex;
  _data.CurrentIndex := currentIndex;

  TAlgoVisHelper.Pause(400);
  AlgoForm.BGRAVirtualScreen.RedrawBitmap;
end;

end.
