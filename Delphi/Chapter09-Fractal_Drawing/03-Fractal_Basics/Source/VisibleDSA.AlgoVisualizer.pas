unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Math,
  FMX.Graphics,
  FMX.Forms,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.FractalData;

type
  TAlgoVisualizer = class(TObject)
  private
    _data: TFractalData;

    procedure __setData;
    procedure __desktopCenter(form: TForm);

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TCanvas);
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
  w := Trunc(Power(3, depth));
  h := Trunc(Power(3, depth));

  form.ClientWidth := w;
  form.ClientHeight := h;
  form.Caption := 'Fractal Visualizer --- ' + Format('W: %d, H: %d', [w, h]);
  __desktopCenter(form);

  _data := TFractalData.Create(depth);
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TCanvas);
const
  OFFSET: array of array of integer = [[0, 0], [2, 0], [1, 1], [0, 2], [2, 2]];

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
  AlgoForm.PaintBox.Repaint;
end;

procedure TAlgoVisualizer.__desktopCenter(form: TForm);
var
  top, left: Double;
begin
  top := (Screen.Height div 2) - (form.ClientHeight div 2);
  left := (Screen.Width div 2) - (form.ClientWidth div 2);

  form.top := Trunc(top);
  form.left := Trunc(left);
end;

end.
