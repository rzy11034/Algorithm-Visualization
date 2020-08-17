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
  ExtCtrls,
  BGRABitmap,
  BGRAVirtualScreen,
  VisibleDSA.AlgoVisualizer;

type
  TAlgoForm = class(TForm)
    BGRAVirtualScreen: TBGRAVirtualScreen;
    procedure BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    _av: TAlgoVisualizer;
    _stop: boolean;

  public
    property Stop: boolean read _stop;

  end;

var
  AlgoForm: TAlgoForm;

implementation

{$R *.frm}

{ TAlgoForm }

procedure TAlgoForm.BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  _av.Paint(Bitmap.Canvas2D);
end;

procedure TAlgoForm.FormActivate(Sender: TObject);
begin
  _av.Run;
end;

procedure TAlgoForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  _stop := True;
end;

procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  ClientWidth := 800;
  ClientHeight := 800;
  Position := TPosition.poDesktopCenter;
  BorderStyle := TFormBorderStyle.bsSingle;
  DoubleBuffered := True;
  Caption := 'AlgoForm';
  _stop := False;

  BGRAVirtualScreen.Color := clForm;

  _av := TAlgoVisualizer.Create(self, ClientWidth, ClientHeight, 100);
end;

end.
