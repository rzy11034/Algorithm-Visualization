unit VisibleDSA.AlgoVisHelper;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  FMX.Types,
  FMX.Forms,
  FMX.Graphics;

const
  CL_RED = $FFF44336;
  CL_PINK = $FFE91E63;
  CL_PURPLE = $FF9C27B0;
  CL_DEEPPURPLE = $FF673AB7;
  CL_INDIGO = $FF3F51B5;
  CL_BLUE = $FF2196F3;
  CL_LIGHTBLUE = $FF03A9F4;
  CL_CYAN = $FF00BCD4;
  CL_TEAL = $FF009688;
  CL_GREEN = $FF4CAF50;
  CL_LIGHTGREEN = $FF8BC34A;
  CL_LIME = $FFCDDC39;
  CL_YELLOW = $FFFFEB3B;
  CL_AMBER = $FFFFC107;
  CL_ORANGE = $FFFF9800;
  CL_DEEPORANGE = $FFFF5722;
  CL_BROWN = $FF795548;
  CL_GREY = $FF9E9E9E;
  CL_BLUEGREY = $FF607D8B;
  CL_BLACK = $FF000000;
  CL_WHITE = $FFFFFFFF;

type
  TColor = type Cardinal;

  TAlgoVisHelper = class
  private
    class var _fillColor: TColor;
    class var _fillStyle: TBrushKind;
    class var _strokeColor: TColor;
    class var _strokeWidth: integer;
    class var _strokeStyle: TBrushKind;

  public
    class procedure SetStroke(strokeWidth: integer; color: TColor = CL_BLACK; style: TBrushKind = TBrushKind.Solid);
    class procedure SetFill(color: TColor = CL_BLACK; style: TBrushKind = TBrushKind.Solid);

    // 正圆形
    class procedure DrawCircle(canvas: TCanvas; x, y, r: integer);
    class procedure FillCircle(canvas: TCanvas; x, y, r: integer);

    // 矩形
    class procedure FillRectangle(canvas: TCanvas; x, y, w, h: Single);

    // 直角坐标
    class procedure DrawCoordinates(canvas: TCanvas);

    // Pause
    class procedure Pause(interval: integer);

    // 从Resource Stream 名称绘图
    class procedure DrawImageFormResourceStream(canvas: TCanvas; stm: TStream; x1, y1, x2, y2: integer);
  end;

implementation

{ TAlgoVisHelper }

class procedure TAlgoVisHelper.DrawCircle(canvas: TCanvas; x, y, r: integer);
var
  a: TRectF;
begin
  a := TRectF.Create(x - r, y - r, x + r, y + r);
  canvas.Stroke.Color := _strokeColor;
  canvas.Stroke.Thickness := 1;
  canvas.Stroke.Kind := TBrushKind.Solid;

  if canvas.BeginScene then
  begin
    try
      canvas.DrawEllipse(a, 1);
    finally
      canvas.EndScene;
    end;
  end;
end;

class procedure TAlgoVisHelper.DrawCoordinates(canvas: TCanvas);
var
  x1, y1, x2, y2: Integer;
begin
  canvas.Stroke.Color := CL_INDIGO;
  canvas.Stroke.Thickness := 1;
  canvas.Stroke.Kind := TBrushKind.Solid;
  canvas.Stroke.Dash := TStrokeDash.Dot;

  if canvas.BeginScene then
  begin
    try
      // 横轴
      x1 := 0;
      y1 := canvas.Height div 2;
      x2 := canvas.Width;
      y2 := y1;
      canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(x2, y2), 1);

      // 竖轴
      x1 := canvas.Width div 2;
      y1 := 0;
      x2 := x1;
      y2 := canvas.Height;
      canvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(x2, y2), 1);
    finally
      canvas.EndScene;
    end;
  end;
end;

class procedure TAlgoVisHelper.DrawImageFormResourceStream(canvas: TCanvas; stm: TStream;
  x1, y1, x2, y2: integer);
var
  bmp: TBitmap;
  srcRect, dstRect: TRectF;
begin
  bmp := TBitmap.CreateFromStream(stm);
  srcRect := bmp.Bounds;
  dstRect := TRectF.Create(x1, y1, x1 + x2, y1 + y2);

  if canvas.BeginScene() then
  begin
    try
      canvas.DrawBitmap(bmp, srcRect, dstRect, 1);
    finally
      canvas.EndScene;
    end;
  end;
end;

class procedure TAlgoVisHelper.FillRectangle(canvas: TCanvas; x, y, w, h: Single);
var
  a: TRectF;
begin
  a := TRectF.Create(x, y, x + w, y + h);
  canvas.Fill.Color := _fillColor;
  canvas.Fill.Kind := _fillStyle;

  if canvas.BeginScene then
  begin
    try
      canvas.FillRect(a, 0, 0, AllCorners, 1);
    finally
      canvas.EndScene;
    end;
  end;
end;

class procedure TAlgoVisHelper.Pause(interval: integer);
begin
  Sleep(interval);
end;

class procedure TAlgoVisHelper.FillCircle(canvas: TCanvas; x, y, r: integer);
var
  a: TRectF;
begin
  a := TRectF.Create(x - r, y - r, x + r, y + r);
  canvas.Fill.Color := _fillColor;
  canvas.Fill.Kind := _fillStyle;

  if canvas.BeginScene then
  begin
    try
      canvas.FillEllipse(a, 1);
    finally
      canvas.EndScene;
    end;
  end;
end;

class procedure TAlgoVisHelper.SetFill(color: TColor; style: TBrushKind);
begin
  _fillColor := color;
  _fillStyle := style;
end;

class procedure TAlgoVisHelper.SetStroke(strokeWidth: integer; color: TColor; style: TBrushKind);
begin
  _strokeColor := color;
  _strokeWidth := strokeWidth;
  _strokeStyle := style;
end;

end.
