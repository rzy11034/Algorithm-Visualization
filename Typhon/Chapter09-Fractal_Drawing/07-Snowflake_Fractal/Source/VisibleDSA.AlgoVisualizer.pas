﻿unit VisibleDSA.AlgoVisualizer;

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
begin
  Static_Vis := self;

  _isForward := true;
  depth := 0;
  _times := 1;

  w := 1024;
  h := 350;

  form.ClientWidth := w;
  form.ClientHeight := h;
  form.Caption := 'Fractal Visualizer --- ' + Format('W: %d, H: %d', [w, h]);
  form.OnShow := @__formShow;
  form.OnKeyPress := @__keyPress;

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

  procedure __get_px_py__(x, y, side, angle: double; out px, py: double);
  var
    radian: double;
  begin
    // 角度换算为弧度
    radian := angle * Pi / 180;

    px := x + side * cos(radian);
    py := y - side * sin(radian);
  end;

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
    side_3, y2, x2, x3, y3, x4, y4: double;
  begin
    Count += 1;

    if side <= 0 then
      Exit;

    if depth = _data.Depth then
    begin
      __get_px_py__(x1, y1, side, angle, x2, y2);
      TAlgoVisHelper.SetStroke(1, CL_INDIGO);
      TAlgoVisHelper.DrawLine(canvas, __pointF__(x1, y1), __pointF__(x2, y2));
      Exit;
    end;

    side_3 := side / 3;

    __get_px_py__(x1, y1, side_3, angle, x2, y2);
    __drawFractal__(x1, y1, side_3, angle, depth + 1);

    __get_px_py__(x2, y2, side_3, angle + 60, x3, y3);
    __drawFractal__(x2, y2, side_3, angle + 60, depth + 1);

    __get_px_py__(x3, y3, side_3, angle - 60, x4, y4);
    __drawFractal__(x3, y3, side_3, angle - 60, depth + 1);

    //__get_px_py__(x4, y4, side_3, angle, x5, y5);
    __drawFractal__(x4, y4, side_3, angle, depth + 1);
  end;

begin
  Count := 0;

  __drawFractal__(0, canvas.Height - 3, canvas.Width, 0, 0);

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
  AlgoForm.BGRAVirtualScreen.DiscardBitmap;
end;

end.
