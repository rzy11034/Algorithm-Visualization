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
  BGRAVirtualScreen,
  VisibleDSA.AlgoVisualizer;

type
  TAlgoForm = class(TForm)
    BGRAVirtualScreen: TBGRAVirtualScreen;
    procedure BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    _av: TAlgoVisualizer;

  public

  end;

var
  AlgoForm: TAlgoForm;

implementation

uses VisibleDSA.AlgoVisHelper;

{$R *.dfm}

{ TAlgoForm }

procedure TAlgoForm.BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  _av.Paint(Bitmap.Canvas2D);
end;

procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  ClientWidth := 600;
  ClientHeight := 600;
  Position := TPosition.poDesktopCenter;
  BorderStyle := TFormBorderStyle.bsSingle;
  DoubleBuffered := true;
  Caption := 'AlgoForm';

  BGRAVirtualScreen.Color := clForm;

  _av := TAlgoVisualizer.Create(self);
end;

procedure TAlgoForm.FormShow(Sender: TObject);
begin
  _av.Run;
end;

end.
