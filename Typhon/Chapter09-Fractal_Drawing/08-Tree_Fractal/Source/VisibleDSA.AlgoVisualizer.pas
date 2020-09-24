unit VisibleDSA.AlgoVisualizer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Types,
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
    procedure __keyPress(Sender: TObject; var Key: char);

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
  splitAngle: double;
begin
  Static_Vis := self;

  _isForward := true;
  depth := 0;
  _times := 1;
  splitAngle := 60;

  w := 800;
  h := 800;

  form.ClientWidth := w;
  form.ClientHeight := h;
  form.Caption := 'Fractal Visualizer --- ' + Format('W: %d, H: %d', [w, h]);
  form.OnShow := @__formShow;
  form.OnKeyPress := @__keyPress;

  _data := TFractalData.Create(depth, splitAngle);
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TBGRACanvas2D);
var
  Count: integer;

  function __pointF__(x, y: double): TPointF;
  var
    res: TPointF;
  begin
    res.x := x;
    res.y := y;

    Result := res;
  end;

  procedure __drawFractal__(x1, y1, side, angle, depth: double);
  var
    x2, y2: double;
  begin
    Count += 1;

    if side <= 0 then
      Exit;

    if depth = _data.Depth then
    begin
      x2 := x1 - side * 2 * Sin(angle * Pi / 180.0);
      y2 := y1 - side * 2 * Cos(angle * Pi / 180.0);
      TAlgoVisHelper.SetStroke(1, CL_INDIGO);
      TAlgoVisHelper.DrawLine(canvas, __pointF__(x1, y1), __pointF__(x2, y2));
      Exit;
    end;

    x2 := x1 - side * Sin(angle * Pi / 180.0);
    y2 := y1 - side * Cos(angle * Pi / 180.0);
    TAlgoVisHelper.SetStroke(1, CL_INDIGO);
    TAlgoVisHelper.DrawLine(canvas, __pointF__(x1, y1), __pointF__(x2, y2));

    __drawFractal__(x2, y2, side / 2, angle + _data.SplitAngle / 2, depth + 1);
    __drawFractal__(x2, y2, side / 2, angle - _data.SplitAngle / 2, depth + 1);
  end;

begin
  Count := 0;

  __drawFractal__(canvas.Width / 2, canvas.Height, canvas.Height / 2, 0, 0);

  AlgoForm.Caption := Format(' Count: %d', [Count]);
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

      if _times >= 6 then
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

  //var
  //  thread: TThread;
begin
  //thread := TThread.CreateAnonymousThread(TProcedure(@__threadExecute__));
  //thread.FreeOnTerminate := true;
  //thread.Start;
end;

procedure TAlgoVisualizer.__keyPress(Sender: TObject; var Key: char);
begin
  case key of
    '0': _data.Depth := 0;
    '1': _data.Depth := 1;
    '2': _data.Depth := 2;
    '3': _data.Depth := 3;
    '4': _data.Depth := 4;
    '5': _data.Depth := 5;
    '6': _data.Depth := 6;
    '7': _data.Depth := 7;
    '8': _data.Depth := 8;
    '9': _data.Depth := 9;
  end;

  __setData;
end;

procedure TAlgoVisualizer.__setData;
begin
  //TAlgoVisHelper.Pause(100);
  AlgoForm.BGRAVirtualScreen.RedrawBitmap;
end;

end.
