unit Unit2;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  FMX.Types,
  FMX.Forms,
  FMX.Graphics;

procedure DrawCoordinates(canvas: TCanvas);

implementation

procedure DrawCoordinates(canvas: TCanvas);
var
  x1, y1, x2, y2: Integer;
begin
  canvas.Stroke.Color := $FFF44336;
  canvas.Stroke.Thickness := 1;
  canvas.Stroke.Kind := TBrushKind.Solid;
  canvas.Stroke.Dash := TStrokeDash.Solid;

  if canvas.BeginScene then
  begin
    try
      // 横轴
      x1 := 0;
      y1 := canvas.Height div 2;
      x2 := canvas.Width;
      y2 := y1;
      canvas.DrawLine(Point(x1, y1), Point(x2, y2), 1);

      // 竖轴
      x1 := canvas.Width div 2;
      y1 := 0;
      x2 := x1;
      y2 := canvas.Height;
      canvas.DrawLine(Point(x1, y1), Point(x2, y2), 1);
    finally
      canvas.EndScene;
    end;
  end;
end;

end.
