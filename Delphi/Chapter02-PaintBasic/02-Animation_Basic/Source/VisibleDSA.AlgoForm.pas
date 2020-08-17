unit VisibleDSA.AlgoForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.AlgoVisualizer;

type
  TAlgoForm = class(TForm)
    PaintBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
    _av: TAlgoVisualizer;
    _stop: Boolean;

    procedure _desktopCenter;

  public
    { Public declarations }
  end;

var
  AlgoForm: TAlgoForm;

implementation

{$R *.fmx}


procedure TAlgoForm.FormActivate(Sender: TObject);
begin
  while True do
  begin
    _av.Run;

    if _stop then
      Break;
  end;
end;

procedure TAlgoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  _stop := True;
end;

procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  ClientWidth := 600;
  ClientHeight := 600;
  Position := TFormPosition.ScreenCenter;
  BorderStyle := TFmxFormBorderStyle.Single;
  Caption := 'AlgoForm: ';
  _desktopCenter;

  _av := TAlgoVisualizer.Create(Self, 10);
  _stop := False;
end;

procedure TAlgoForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if KeyChar = ' ' then
  begin
    _stop := not _stop;
    Self.FormActivate(Sender);
  end;
end;

procedure TAlgoForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then
  begin
    _av.CheckIsFill(X, Y);
  end;
end;

procedure TAlgoForm.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
begin
  _av.Paint(Canvas);
end;

procedure TAlgoForm._desktopCenter;
var
  top, left: Double;
begin
  top := ((Screen.Height div 2) - (Self.Height div 2)) * (Self.Height / Screen.Height);
  left := ((Screen.Width div 2) - (Self.Width div 2)) * 0.9;

  Self.Top := Trunc(top);
  Self.Left := Trunc(left);
end;

end.
