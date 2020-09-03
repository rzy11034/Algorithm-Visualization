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
  FMX.Platform.Win,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.AlgoVisualizer;

type
  TAlgoForm = class(TForm)
    PaintBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormShow(Sender: TObject);

  private
    _av: TAlgoVisualizer;

    procedure _desktopCenter;

  public

  end;

var
  AlgoForm: TAlgoForm;

implementation

{$R *.fmx}


procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  TWinWindowHandle.SetForcedScale(1);
  Position := TFormPosition.DesktopCenter;
  BorderStyle := TFmxFormBorderStyle.Single;
  Caption := 'AlgoForm: ';
  //_desktopCenter;

  _av := TAlgoVisualizer.Create(self);
end;

procedure TAlgoForm.FormShow(Sender: TObject);
var
  thread: TThread;
begin
  thread := TThread.CreateAnonymousThread(_av.Run);
  thread.FreeOnTerminate := True;
  thread.Start;
end;

procedure TAlgoForm.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
begin
  _av.Paint(Canvas);
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
