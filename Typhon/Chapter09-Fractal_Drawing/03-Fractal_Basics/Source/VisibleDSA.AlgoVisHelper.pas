unit VisibleDSA.AlgoVisHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  Forms,
  DeepStar.Utils.UString,
  BGRABitmap,
  BGRABitmapTypes,
  BGRACanvas2D;

const
  CL_RED = $F4 or ($43 shl 8) or ($36 shl 16);
  CL_PINK = $E9 or ($1E shl 8) or ($63 shl 16);
  CL_PURPLE = $9C or (27 shl 8) or ($B0 shl 16);
  CL_DEEPPURPLE = 67 or ($3A shl 8) or ($B7 shl 16);
  CL_INDIGO = $3F or ($51 shl 8) or ($B5 shl 16);
  CL_BLUE = $21 or ($96 shl 8) or ($F3 shl 16);
  CL_LIGHTBLUE = $03 or ($A9 shl 8) or ($F4 shl 16);
  CL_CYAN = $00 or ($BC shl 8) or ($D4 shl 16);
  CL_TEAL = $00 or ($96 shl 8) or ($88 shl 16);
  CL_GREEN = $4C or ($AF shl 8) or ($50 shl 16);
  CL_LIGHTGREEN = $8B or ($C3 shl 8) or ($4A shl 16);
  CL_LIME = $CD or ($DC shl 8) or ($39 shl 16);
  CL_YELLOW = $FF or ($EB shl 8) or ($3B shl 16);
  CL_AMBER = $FF or ($C1 shl 8) or ($07 shl 16);
  CL_ORANGE = $FF or ($98 shl 8) or ($00 shl 16);
  CL_DEEPORANGE = $FF or ($57 shl 8) or ($22 shl 16);
  CL_BROWN = $79 or ($55 shl 8) or ($48 shl 16);
  CL_GREY = $9E or ($9E shl 8) or ($9E shl 16);
  CL_BLUEGREY = $60 or ($7D shl 8) or ($8B shl 16);
  CL_BLACK = $00 or ($00 shl 8) or ($00 shl 16);
  CL_WHITE = $FF or ($FF shl 8) or ($FF shl 16);

type
  TAlgoVisHelper = class
  private
    class var _fillColor: TColor;
    class var _fillStyle: TBrushStyle;
    class var _strokeColor: TColor;
    class var _strokeWidth: integer;
    class var _strokeStyle: TPenStyle;

  public
    class procedure SetStroke(strokeWidth: integer; color: TColor = CL_BLACK; style: TPenStyle = psSolid);
    class procedure SetFill(color: TColor = CL_BLACK; style: TBrushStyle = bsClear);

    // 正圆形
    class procedure DrawCircle(canvas: TBGRACanvas2D; x, y, r: integer);
    class procedure FillCircle(canvas: TBGRACanvas2D; x, y, r: integer);

    // 矩形
    class procedure FillRectangle(canvas: TBGRACanvas2D; x1, y1, x2, y2: integer);

    // 直角坐标
    class procedure DrawCoordinates(canvas: TBGRACanvas2D);

    // Pause
    class procedure Pause(interval: integer);

    // 从Resource Stream 名称绘图
    class procedure DrawImageFormResourceStream(canvas: TBGRACanvas2D; stm: TStream; x1, y1, x2, y2: integer);

    // 绘制文字
    class procedure DrawText(canvas: TBGRACanvas2D; str: UString; x, y: integer);
  end;

implementation

{ TAlgoVisHelper }

class procedure TAlgoVisHelper.DrawCircle(canvas: TBGRACanvas2D; x, y, r: integer);
begin
  canvas.lineWidth := _strokeWidth;
  canvas.lineStyle(_strokeStyle);
  canvas.strokeStyle(_strokeColor);

  canvas.beginPath;
  canvas.circle(x, y, r);
  canvas.stroke;
end;

class procedure TAlgoVisHelper.DrawCoordinates(canvas: TBGRACanvas2D);
begin
  canvas.beginPath;
  canvas.lineStyle(_strokeStyle);
  canvas.strokeStyle(_strokeColor);
  canvas.lineWidth := _strokeWidth;
  canvas.MoveTo(canvas.Width div 2, 0);
  canvas.LineTo(canvas.Width div 2, canvas.Height);
  canvas.MoveTo(0, canvas.Height div 2);
  canvas.LineTo(canvas.Width, canvas.Height div 2);
  canvas.stroke;
end;

class procedure TAlgoVisHelper.FillRectangle(canvas: TBGRACanvas2D; x1, y1, x2, y2: integer);
begin
  canvas.fillStyle(_fillColor);
  canvas.beginPath;
  canvas.rect(x1, y1, x2, y2);
  canvas.fill;
end;

class procedure TAlgoVisHelper.DrawImageFormResourceStream(canvas: TBGRACanvas2D; stm: TStream;
  x1, y1, x2, y2: integer);
var
  bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(stm);
  canvas.drawImage(bmp, x1, y1, x2, y2);
  bmp.Free;
end;

class procedure TAlgoVisHelper.DrawText(canvas: TBGRACanvas2D; str: UString; x, y: integer);
var
  bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(canvas.Width, canvas.Height);
  bmp.FontHeight := 14;
  bmp.TextRect(Rect(x, y, x + 80, y + 80), string(str),
    TAlignment.taCenter, TTextLayout.tlCenter, ColorToBGRA(_fillColor));

  canvas.drawImage(bmp, 0, 0);
  bmp.Free;
end;

class procedure TAlgoVisHelper.Pause(interval: integer);
begin
  Sleep(interval);
end;

class procedure TAlgoVisHelper.FillCircle(canvas: TBGRACanvas2D; x, y, r: integer);
begin
  canvas.beginPath;
  canvas.fillStyle(_fillColor);
  canvas.ellipse(x, y, r, r);
  canvas.fill;
end;

class procedure TAlgoVisHelper.SetFill(color: TColor; style: TBrushStyle);
begin
  _fillColor := color;
  _fillStyle := style;
end;

class procedure TAlgoVisHelper.SetStroke(strokeWidth: integer; color: TColor; style: TPenStyle);
begin
  _strokeWidth := strokeWidth;
  _strokeColor := color;
  _strokeStyle := style;
end;

end.
