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
  FMX.Objects,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.AlgoVisualizer;

type
  TAlgoForm = class(TForm)
    PaintBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
    _thread: TThread;
    _av: TAlgoVisualizer;

    procedure _desktopCenter;

  public

  end;

var
  AlgoForm: TAlgoForm;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

procedure TAlgoForm.FormActivate(Sender: TObject);
begin
  _thread := TThread.CreateAnonymousThread(_av.Run);
  _thread.FreeOnTerminate := True;
  _thread.Start;
end;

procedure TAlgoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if _thread.Finished = False then
  begin
    _thread.Suspended := True;
  end;
end;

procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  TWinWindowHandle.SetForcedScale(1);
  Position := TFormPosition.ScreenCenter;
  BorderStyle := TFmxFormBorderStyle.Single;
  Caption := 'AlgoForm: ';
  _desktopCenter;

  _av := TAlgoVisualizer.Create(self);
end;

procedure TAlgoForm.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
begin
  _av.Paint(Canvas);
//  TAlgoVisHelper.SetFill(CL_RED);
//  TAlgoVisHelper.FillRectangle(Canvas, 0, 0, 50, 50);
//  TAlgoVisHelper.FillRectangle(Canvas, 50, 0, 50, 50);
//  TAlgoVisHelper.FillRectangle(Canvas, 0, 50, 50, 50);
end;

procedure TAlgoForm._desktopCenter;
var
  top, left: Double;
begin
  top := ((Screen.Height div 2) - (self.Height div 2)) * (self.Height / Screen.Height);
  left := ((Screen.Width div 2) - (self.Width div 2)) * 0.9;

  self.top := Trunc(top);
  self.left := Trunc(left);
end;

end.
