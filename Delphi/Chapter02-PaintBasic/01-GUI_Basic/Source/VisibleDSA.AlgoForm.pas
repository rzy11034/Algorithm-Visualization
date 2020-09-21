unit VisibleDSA.AlgoForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Platform.Win,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  VisibleDSA.AlgoVisHelper;

type
  TAlgoForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);

  private
    procedure _desktopCenter;

  public
    { Public declarations }
  end;

var
  AlgoForm: TAlgoForm;

implementation

{$R *.fmx}


procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  TWinWindowHandle.SetForcedScale(1);
  ClientWidth := 600;
  ClientHeight := 600;
  Position := TFormPosition.ScreenCenter;
  BorderStyle := TFmxFormBorderStyle.Single;
  Caption := 'AlgoForm: ';
  _desktopCenter;
end;

procedure TAlgoForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  TAlgoVisHelper.SetStroke(1, CL_RED);
  TAlgoVisHelper.DrawCircle(Canvas, 100, 100, 50);

  TAlgoVisHelper.SetFill(CL_RED);
  TAlgoVisHelper.FillCircle(Canvas, 150, 150, 50);

  TAlgoVisHelper.SetStroke(1, CL_RED);
  TAlgoVisHelper.DrawCoordinates(Canvas);
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
