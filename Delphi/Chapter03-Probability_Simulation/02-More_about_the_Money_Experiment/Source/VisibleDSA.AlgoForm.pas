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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);

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
  repeat
    _av.Run;
  until (_stop = True);
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

  _av := TAlgoVisualizer.Create;
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
