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
    _times: integer;
    _isForward: boolean;

    procedure __setData;
    procedure __formShow(Sender: TObject);


  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
    procedure Run;

  end;

implementation

uses
  VisibleDSA.AlgoForm;

var
  Static_Vis: TAlgoVisualizer;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  depth, w, h: integer;
begin
  Static_Vis := self;

  _isForward := true;
  depth := 6;
  _times := 1;

  w := 3 ** depth;
  h := 3 ** depth;

  form.ClientWidth := w;
  form.ClientHeight := h;
  form.Caption := 'Fractal Visualizer --- ' + Format('W: %d, H: %d', [w, h]);
  form.OnShow := @__formShow;

  _data := TFractalData.Create(depth);
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TBGRACanvas2D);
var
  Count: integer;

  procedure __drawFractal__(Ax, Ay, side, depth: integer);
  var
    Bx, By, h, Cx, Cy: integer;
    AB_centerX, AB_centerY, AC_centerX, AC_centerY: integer;
  begin
    Count += 1;

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
      _times += 1;

      if _times >= 9 then
        _isForward := not _isForward;
    end
    else
    begin
      _times -= 1;

      if _times < 1 then
        _isForward := not _isForward;
    end;
  end;
end;

procedure TAlgoVisualizer.__formShow(Sender: TObject);
  procedure __threadExecute__;
  begin
    Static_Vis.Run;
  end;

var
  thread: TThread;
begin
  thread := TThread.CreateAnonymousThread(TProcedure(@__threadExecute__));
  thread.FreeOnTerminate := true;
  thread.Start;
end;

procedure TAlgoVisualizer.__setData;
begin
  TAlgoVisHelper.Pause(100);
  AlgoForm.BGRAVirtualScreen.DiscardBitmap;
end;

end.
