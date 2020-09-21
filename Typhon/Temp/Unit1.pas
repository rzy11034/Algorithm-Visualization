unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  BGRABitmap,
  BGRACanvas2D,
  BGRABitmapTypes,
  BGRACanvas;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    PenWidth: single;
  end;

var
  Form1: TForm1;

implementation

uses
  Unit2,
  Unit3,
  Unit4;

{$R *.frm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Height := 600;
  Width := 600;
  Position := TPosition.poDefaultPosOnly;
  Caption := 'BGRACanvas2D';
  PenWidth := 1;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  bmp: TBGRABitmap;
  ctx: TBGRACanvas2D;
  cta: TBGRACanvas;
begin
  bmp := TBGRABitmap.Create(Canvas.Width, Canvas.Height, clForm);
  try

    ctx := bmp.Canvas2D;
    ctx.antialiasing := false;

    ctx.strokeStyle(clBlack);
    ctx.lineStyle(TPenStyle.psSolid);
    ctx.fillStyle(clBlack);
    ctx.lineWidth := PenWidth;

    ctx.beginPath;
    ctx.MoveTo(ctx.Width div 2, 0);
    ctx.LineTo(ctx.Width div 2, ctx.Height);
    ctx.MoveTo(0, ctx.Height div 2);
    ctx.LineTo(ctx.Width, ctx.Height div 2);
    ctx.closePath;
    ctx.stroke;
    ctx.save;
    //bmp.Draw(Canvas, 0, 0);

    ctx.beginPath;
    ctx.strokeStyle(clRed);
    ctx.rect(20, 20, 40, 40);
    ctx.stroke;
    ctx.restore;

    ctx.beginPath;
    ctx.circle(100, 100, 50);
    ctx.antialiasing := true;
    ctx.stroke;

    ctx.beginPath;
    ctx.antialiasing := not false;
    ctx.fontName := 'Times New Roman';
    ctx.fontEmHeight := 100;
    //ctx.fontRenderer.FontName := ;
    //ctx.textAlignLCL := TAlignment.taLeftJustify;
    ctx.textBaseline := 'bottom';
    ctx.fontRenderer.TextOut(bmp, 0, ctx.Height div 2, 'ABCD', clBlack, TAlignment.taLeftJustify);
    ctx.fontRenderer.


    //cta := bmp.CanvasBGRA;
    //cta.Brush.Opacity := 0;
    //cta.Font.Name := 'Times New Roman';
    //cta.Font.Height := 200;
    //cta.TextOut(10, 10, 'ABCD');


    bmp.Draw(Canvas, 0, 0);
  finally
    bmp.Free;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Form2 := TForm2.Create(self);
  Form2.Show;
  Form3 := TForm3.Create(self);
  Form3.Show;
  //Form4.Show;
end;

end.
