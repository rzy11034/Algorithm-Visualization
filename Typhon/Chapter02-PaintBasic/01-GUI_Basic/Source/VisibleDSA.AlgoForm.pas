unit VisibleDSA.AlgoForm;

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
  BGRAVirtualScreen;

type
  TAlgoForm = class(TForm)
    BGRAVirtualScreen: TBGRAVirtualScreen;
    procedure BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  AlgoForm: TAlgoForm;

implementation

uses
  VisibleDSA.AlgoVisHelper;

{$R *.dfm}

{ TAlgoForm }

procedure TAlgoForm.BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  TAlgoVisHelper.SetStroke(10, CL_RED);
  TAlgoVisHelper.DrawCircle(Bitmap.Canvas2D, 300, 300, 200);

  TAlgoVisHelper.SetStroke(1, CL_INDIGO, TPenStyle.psDot);
  TAlgoVisHelper.DrawCoordinates(Bitmap.Canvas2D);

  TAlgoVisHelper.SetFill(CL_BLUEGREY);
  TAlgoVisHelper.FillCircle(Bitmap.Canvas2D, 400, 400, 100);

  TAlgoVisHelper.FillRectangle(Bitmap.Canvas2D, 10, 10, 20, 100);
end;

procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  ClientWidth := 1024;
  ClientHeight := 768;
  Position := TPosition.poDesktopCenter;
  BorderStyle := TFormBorderStyle.bsSingle;
  DoubleBuffered := True;
  Caption := 'AlgoForm';

  BGRAVirtualScreen.Color := clForm;
end;

end.
