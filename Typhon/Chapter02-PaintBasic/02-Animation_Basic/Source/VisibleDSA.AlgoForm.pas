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
  VisibleDSA.AlgoVisualizer,
  BGRABitmap,
  BGRAVirtualScreen;

type
  TAlgoForm = class(TForm)
    BGRAVirtualScreen: TBGRAVirtualScreen;
    procedure BGRAVirtualScreenMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  private
    _av: TAlgoVisualizer;
    _stop: boolean;

  public

  end;

var
  AlgoForm: TAlgoForm;

implementation

{$R *.dfm}

{ TAlgoForm }

procedure TAlgoForm.BGRAVirtualScreenMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = TMouseButton.mbLeft then
  begin
    _av.CheckIsFill(X, Y);
  end;
end;

procedure TAlgoForm.BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  _av.Paint(Bitmap.Canvas2D);
end;

procedure TAlgoForm.FormActivate(Sender: TObject);
begin
  repeat
    _av.Run;
  until _stop = True;
end;

procedure TAlgoForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  _stop := True;
end;

procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  ClientWidth := 600;
  ClientHeight := 600;
  Position := TPosition.poDesktopCenter;
  BorderStyle := TFormBorderStyle.bsSingle;
  DoubleBuffered := True;
  Caption := 'AlgoForm';

  BGRAVirtualScreen.Color := clForm;

  _av := TAlgoVisualizer.Create(Self, 10);
  _stop := False;
end;

procedure TAlgoForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = Ord(' ') then
  begin
    _Stop := not _Stop;
    Self.FormActivate(Sender);
  end;
end;

end.
