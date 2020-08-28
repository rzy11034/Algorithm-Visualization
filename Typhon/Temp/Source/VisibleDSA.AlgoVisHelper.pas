unit VisibleDSA.AlgoVisHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  Forms;

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


    // 矩形
    class procedure FillRectangle(canvas: TCanvas; x1, y1, x2, y2: integer);


    // Pause
    class procedure Pause(interval: integer);
  end;

implementation

{ TAlgoVisHelper }

class procedure TAlgoVisHelper.FillRectangle(canvas: TCanvas; x1, y1, x2, y2: integer);
begin
  canvas.Lock;
  canvas.Brush.Color := _fillColor;
  canvas.FillRect(x1, y1, x1 + x2, y1 + y2);
  canvas.Unlock;
end;

class procedure TAlgoVisHelper.Pause(interval: integer);
begin
  Sleep(interval);
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


