unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.SysUtils,
  System.Types,
  FMX.Graphics,
  FMX.Forms,
  System.Generics.Collections,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.Circle,
  VisibleDSA.MonteCarloPiData;

type
  TAlgoVisualizer = class(TObject)
  private
    _circle: TCircle;
    _form: TForm;
    _count: integer;
    _data: TMonteCarloPiData;

  public
    constructor Create(form: TForm; n: integer);
    destructor Destroy; override;

    procedure Paint(canvas: TCanvas);
    procedure Run;
  end;

implementation

uses
  Winapi.Windows, VisibleDSA.AlgoForm;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm; n: integer);
var
  x, y, r: integer;
begin
  inherited Create;

  _form := form;
  _count := n;

  if _form.ClientWidth <> _form.ClientHeight then
    raise Exception.Create('This demo must be run in a square windows.');

  x := form.ClientWidth div 2;
  y := form.ClientHeight div 2;
  r := form.ClientWidth div 2;

  _circle := TCircle.Create(x, y, r);
  _data := TMonteCarloPiData.Create(_circle);

  AllocConsole;
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  FreeAndNil(_circle);
  FreeConsole;

  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TCanvas);
var
  p: TPoint;
  i: integer;
begin
  TAlgoVisHelper.SetStroke(3, CL_BLUE);

  TAlgoVisHelper.DrawCircle(canvas, _circle.X, _circle.Y, _circle.R);

  for i := 0 to _data.GetPointNumber - 1 do
  begin
    p := _data.GetPoint(i);

    if _circle.Contain(p) then
    begin
      TAlgoVisHelper.SetFill(CL_GREEN);
    end
    else
    begin
      TAlgoVisHelper.SetFill(CL_RED);
    end;

    TAlgoVisHelper.FillCircle(canvas, p.X, p.Y, 1);
  end;
end;

procedure TAlgoVisualizer.Run;
var
  x, y, i: integer;
begin
  Randomize;

  for i := 0 to _count - 1 do
  begin
    x := Random(_form.ClientWidth);
    y := Random(_form.ClientHeight);

    _data.AddPoint(TPoint.Create(x, y));
    Writeln(FloatToStr(_data.EstimatePI));
  end;

  TAlgoVisHelper.Pause(10);
  AlgoForm.PaintBox.Repaint;
end;

end.
