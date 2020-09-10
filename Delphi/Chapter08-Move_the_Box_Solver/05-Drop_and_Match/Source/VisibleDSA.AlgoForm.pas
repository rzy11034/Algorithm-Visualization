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

  _av := TAlgoVisualizer.Create(self);
end;

procedure TAlgoForm.FormShow(Sender: TObject);
begin
  _av.Run;
end;

procedure TAlgoForm.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
begin
  _av.Paint(Canvas);
end;

end.
