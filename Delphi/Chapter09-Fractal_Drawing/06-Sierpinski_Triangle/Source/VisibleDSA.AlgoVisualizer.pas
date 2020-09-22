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
    _times: integer;
    _isForward: boolean;

    procedure __setData;
    procedure __desktopCenter(form: TForm);
    procedure __formShow(Sender: TObject);

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
  _isForward := true;
  depth := 6;
  _times := 1;

  w := Trunc(Power(3, depth));
  h := Trunc(Power(3, depth));

  form.ClientWidth := w;
  form.ClientHeight := h;
  form.Caption := 'Fractal Visualizer --- ' + Format('W: %d, H: %d', [w, h]);
  form.OnShow := __formShow;
  __desktopCenter(form);

  _data := TFractalData.Create(depth);
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TCanvas);
var
  Count: integer;

  procedure __drawFractal__(Ax, Ay, side, depth: integer);
  var
    Bx, By, h, Cx, Cy: integer;
    AB_centerX, AB_centerY, AC_centerX, AC_centerY: integer;
  begin
    Count := Count + 1;

    if side <= 1 then
    begin
      TAlgoVisHelper.SetFill(CL_INDIGO);
      TAlgoVisHelper.FillRectangle(canvas, Ax, Ay, 1, 1);
      Exit;
    end;

    Bx := Ax + side;
    By := Ay;
    h := Round(sin(60 * Pi / 180) * side);
    Cx := Ax + side div 2;
    Cy := Ay - h;

    if depth = _data.Depth then
    begin
      TAlgoVisHelper.SetFill(CL_INDIGO);
      TAlgoVisHelper.FillTriangle(canvas, Point(Ax, Ay), Point(Bx, By), Point(Cx, Cy));
      Exit;
    end;

    AB_centerX := (Ax + Bx) div 2;
    AB_centerY := (Ay + By) div 2;

    AC_centerX := (Ax + Cx) div 2;
    AC_centerY := (Ay + Cy) div 2;

    __drawFractal__(Ax, Ay, side div 2, depth + 1);
    __drawFractal__(AC_centerx, AC_centery, side div 2, depth + 1);
    __drawFractal__(AB_centerX, AB_centerY, side div 2, depth + 1);
  end;

begin
  Count := 0;
  __drawFractal__(0, canvas.Height, canvas.Width, 0);
  AlgoForm.Caption := Format(' Count: %d', [Count]);
  //TAlgoVisHelper.SetFill(CL_INDIGO);
  //TAlgoVisHelper.FillTriangle(canvas, Point(0, canvas.Height),
  //  Point(canvas.Width, canvas.Height), Point(canvas.Width div 2, 0));
end;

procedure TAlgoVisualizer.Run;
begin
  while true do
  begin
    _data.Depth := _times;
    __setData;

    if _isForward then
    begin
      _times := _times + 1;

      if _times >= 9 then
        _isForward := not _isForward;
    end
    else
    begin
      _times := _times - 1;

      if _times < 1 then
        _isForward := not _isForward;
    end;
  end;
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

procedure TAlgoVisualizer.__formShow(Sender: TObject);
var
  thread: TThread;
begin
  thread := TThread.CreateAnonymousThread(Run);
  thread.FreeOnTerminate := true;
  thread.Start;
end;

procedure TAlgoVisualizer.__setData;
begin
  TAlgoVisHelper.Pause(100);
  AlgoForm.PaintBox.Repaint;
end;

end.
