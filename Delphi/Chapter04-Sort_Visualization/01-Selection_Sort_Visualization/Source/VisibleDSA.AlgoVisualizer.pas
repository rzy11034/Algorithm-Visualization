﻿unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.SysUtils,
  System.Types,
  FMX.Graphics,
  FMX.Forms,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.SelectionSortData;

type
  TAlgoVisualizer = class(TObject)
  private
    _width: integer;
    _height: integer;
    _data: TSelectionSortData;
    _form: TForm;

  public
    constructor Create(form: TForm; sceneWidth, sceneHeight, n: integer);
    destructor Destroy; override;

    procedure Paint(canvas: TCanvas);
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

procedure TAlgoVisualizer.Paint(canvas: TCanvas);
var
  w: integer;
  i: integer;
begin
  w := _width div _data.Length;

  TAlgoVisHelper.SetFill(CL_LIGHTBLUE);
  for i := 0 to _data.Length - 1 do
  begin
    TAlgoVisHelper.FillRectangle(canvas, i * w, _height - _data.GetValue(i), w - 1, _data.GetValue(i));
  end;
end;

procedure TAlgoVisualizer.Run;
var
  i, j, minIndex: integer;
begin
  for i := 0 to _data.Length - 1 do
  begin
    minIndex := i;

    for j := i + 1 to _data.Length - 1 do
    begin
      if _data.GetValue(j) < _data.GetValue(minIndex) then
        minIndex := j;
    end;

    _data.swap(i, minIndex);

    TAlgoVisHelper.Pause(40);
    AlgoForm.PaintBox.Repaint;

    if AlgoForm.Stop then
      Break;
  end;
end;

end.
