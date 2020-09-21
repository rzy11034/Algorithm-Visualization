unit Unit3;

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
  TForm3 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private

  public

  end;

var
  Form3: TForm3;

implementation

uses
  Unit1;

{$R *.frm}

{ TForm3 }

procedure TForm3.FormCreate(Sender: TObject);
begin
  ClientHeight := Form1.ClientHeight;
  ClientWidth := Form1.ClientWidth;
  Position := Form1.Position;
  Caption := 'Canvas';
end;

procedure TForm3.FormPaint(Sender: TObject);
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Style := TPenStyle.psSolid;
  Canvas.Pen.Width := Round(Form1.PenWidth);

  Canvas.MoveTo(Canvas.Width div 2, 0);
  Canvas.LineTo(Canvas.Width div 2, Canvas.Height);
  Canvas.MoveTo(0, Canvas.Height div 2);
  Canvas.LineTo(Canvas.Width, Canvas.Height div 2);

  Canvas.Rectangle(20, 20, 60, 60);
end;

end.
