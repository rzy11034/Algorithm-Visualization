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
  VisibleDSA.CircleData;

type
  TAlgoVisualizer = class(TObject)
  private
    _data: TCircleData;

    procedure __setData;

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
    procedure Run;
  end;

implementation

uses
  VisibleDSA.AlgoForm;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  w, h, r: integer;
begin
  w := 800;
  h := 800;

  form.ClientWidth := w;
  form.ClientHeight := h;
  form.Caption := 'Fractal Visualizer --- ' + Format('W: %d, H: %d', [w, h]);

  r := w div 2 - 2;
  _data := TCircleData.Create(w div 2, h div 2, r, r div 2, 2);
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TBGRACanvas2D);
  procedure __draw__(x, y, r, depth: integer);
  begin
    if (depth = _data.Depth) or (r <= 1) then
      Exit;

    if depth mod 2 = 0 then
      TAlgoVisHelper.SetFill(CL_RED)
    else
      TAlgoVisHelper.SetFill(CL_LIGHTBLUE);

    TAlgoVisHelper.FillCircle(canvas, x, y, r);
    __draw__(x, y, r - _data.Step, depth - 1);
  end;

begin
  __draw__(_data.StartX, _data.StartY, _data.StartR, 0);
end;

procedure TAlgoVisualizer.Run;
begin
  __setData;
end;

procedure TAlgoVisualizer.__setData;
begin
  AlgoForm.BGRAVirtualScreen.RedrawBitmap;
end;

end.
