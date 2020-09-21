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
  BGRACanvas2D;

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
  ClientHeight := 600;
  ClientWidth := 600;
  Position := TPosition.poDefaultPosOnly;
  Caption := 'BGRACanvas2D';
  PenWidth := 1;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  bmp: TBGRABitmap;
  ctx: TBGRACanvas2D;
begin
  bmp := TBGRABitmap.Create(Canvas.Width, Canvas.Height, clForm);
  try

    ctx := bmp.Canvas2D;
    //ctx.antialiasing := false;

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

    ctx.rect(20, 20, 40, 40);
    ctx.circle(100, 100, 50);

    ctx.stroke;
    bmp.Draw(Canvas, 0, 0);
  finally
    bmp.Free;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Form2.Show;
  Form3.Show;
  //Form4.Show;
end;

end.
