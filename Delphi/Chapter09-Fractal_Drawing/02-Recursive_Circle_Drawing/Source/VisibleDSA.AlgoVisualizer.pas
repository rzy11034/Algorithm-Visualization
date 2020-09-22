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
  VisibleDSA.CircleData;

type
  TAlgoVisualizer = class(TObject)
  private
    _data: TCircleData;

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
  w, h, r: integer;
begin
  w := 800;
  h := 800;

  form.ClientWidth := w;
  form.ClientHeight := h;
  form.Caption := 'Fractal Visualizer --- ' + Format('W: %d, H: %d', [w, h]);
  __desktopCenter(form);

  r := w div 2 -2;
  _data := TCircleData.Create(w div 2, h div 2, r, r div 2, 2);
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TCanvas);
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
