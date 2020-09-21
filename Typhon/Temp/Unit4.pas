unit Unit4;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  BGRABitmap;

type
  TForm4 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private

  public

  end;

var
  Form4: TForm4;

implementation

uses
  Unit1;

{$R *.frm}

{ TForm4 }

procedure TForm4.FormCreate(Sender: TObject);
begin
  ClientHeight := Form1.ClientHeight;
  ClientWidth := Form1.ClientWidth;
  Position := Form1.Position;
  Caption := 'asd';
end;

procedure TForm4.FormPaint(Sender: TObject);
var
  bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(Canvas.Width, Canvas.Height, clForm);
  //bmp.dr
end;

end.
