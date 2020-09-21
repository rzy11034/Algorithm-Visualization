unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs;

type
  TForm2 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private

  public

  end;

var
  Form2: TForm2;

implementation

uses
  BGRABitmap,
  BGRACanvas,
  Unit1;

{$R *.frm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  ClientHeight := Form1.ClientHeight;
  ClientWidth := Form1.ClientWidth;
  Position := Form1.Position;
  Caption := 'BGRACanvasBGRA';
end;

procedure TForm2.FormPaint(Sender: TObject);
var
  bmp: TBGRABitmap;
  ctx: TBGRACanvas;
begin
  bmp := TBGRABitmap.Create(Canvas.Width, Canvas.Height, clForm);
  try
    ctx := bmp.CanvasBGRA;

    ctx.Pen.Color := clBlack;
    ctx.Pen.Style := TPenStyle.psSolid;
    ctx.Pen.Width := Round(Form1.PenWidth);

    ctx.MoveTo(ctx.Width div 2, 0);
    ctx.LineTo(ctx.Width div 2, ctx.Height);
    ctx.MoveTo(0, ctx.Height div 2);
    ctx.LineTo(ctx.Width, ctx.Height div 2);

    ctx.Rectangle(20, 20, 60, 60, false);

    bmp.Draw(Canvas, 0, 0);
  finally
    bmp.Free;
  end;
end;

end.
