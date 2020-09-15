unit VisibleDSA.AlgoVisualizer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  Forms,
  BGRACanvas2D,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.FractalData;

type
  TAlgoVisualizer = class(TObject)
  private
    _data: TFractalData;

    procedure __setData;

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
    procedure Run;
  end;

implementation

uses
  Math,
  VisibleDSA.AlgoForm;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  r: integer;
begin
  form.ClientWidth := 800;
  form.ClientHeight := 800;
  form.Caption := 'Fractal Visualizer --- ' +
    Format('W: %d, H: %d', [form.ClientWidth, form.ClientHeight]);

  r := Min(form.Width, form.Height) div 2 - 2;
  _data := TFractalData.Create(2);
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TBGRACanvas2D);
begin

end;

procedure TAlgoVisualizer.Run;
begin
  __setData;
end;

procedure TAlgoVisualizer.__setData;
begin

  AlgoForm.BGRAVirtualScreen.RedrawBitmap;
end;

end.
