unit Unit1;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Platform.Win,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
    procedure p(canvas: TCanvas);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Unit2;

{$R *.fmx}


procedure TForm1.FormCreate(Sender: TObject);
begin
  TWinWindowHandle.SetForcedScale(1);
  ClientWidth := 600;
  ClientHeight := 600;
  Position := TFormPosition.ScreenCenter;
  BorderStyle := TFmxFormBorderStyle.Single;
  Caption := 'AlgoForm: ';
  //Position := TFormPosition.DesktopCenter;
  //Caption := 'BGRACanvas2D';
end;

procedure TForm1.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  DrawCoordinates(canvas);
end;

procedure TForm1.p(canvas: TCanvas);
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

procedure TForm1.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var
  w, h, w_2, h_2: integer;
begin
  w := Canvas.Width;
  h := Canvas.Height;
  w_2 := w div 2;
  h_2 := h div 2;

  canvas.Stroke.Color := TAlphaColorRec.Black;
  canvas.Stroke.Thickness := 1;
  canvas.Stroke.Kind := TBrushKind.Solid;
  canvas.Stroke.Dash := TStrokeDash.Solid;

  if canvas.BeginScene then
  begin
    try
        // 横轴
      canvas.DrawLine(Pointf(0, h_2), Pointf(w, h_2), 1);

        // 竖轴
      canvas.DrawLine(Pointf(w_2, 0), Pointf(w_2, h), 1);
    finally
      canvas.EndScene;
    end;
  end;
end;

end.
