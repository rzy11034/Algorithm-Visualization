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
  VisibleDSA.FractalData;

type
  TAlgoVisualizer = class(TObject)
  private
    _data: TFractalData;

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
  depth, w, h: integer;
begin
  depth := 6;
  w := 3 ** depth;
  h := 3 ** depth;

  form.ClientWidth := w;
  form.ClientHeight := h;
  form.Caption := 'Fractal Visualizer --- ' +
    Format('W: %d, H: %d', [form.ClientWidth, form.ClientHeight]);

  _data := TFractalData.Create(depth);
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TBGRACanvas2D);
const
  OFFSET: array of array of integer = ((0, 0), (2, 0), (1, 1), (0, 2), (2, 2));

  procedure __drawFractal__(x, y, w, h, depth: integer);
  var
    h_3, w_3, i: integer;
  begin
    if depth = _data.Depth then
    begin
      TAlgoVisHelper.SetFill(CL_INDIGO);
      TAlgoVisHelper.FillRectangle(canvas, x, y, w, h);
      Exit;
    end;

    if (w <= 1) or (h <= 1) then
    begin
      TAlgoVisHelper.SetFill(CL_INDIGO);
      TAlgoVisHelper.FillRectangle(canvas, x, y, Max(w, 1), Max(h, 1));
      Exit;
    end;

    w_3 := w div 3;
    h_3 := h div 3;
    for i := 0 to High(OFFSET) do
    begin
      __drawFractal__(x + OFFSET[i, 0] * w_3, y + OFFSET[i, 1] * h_3, w_3, h_3, depth + 1);
    end;
  end;

begin
  __drawFractal__(0, 0, canvas.Width, canvas.Height, 0);
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
