unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.SysUtils,
  FMX.Graphics,
  FMX.Forms,
  VisibleDSA.AlgoVisHelper;

type
  TAlgoVisualizer = class(TObject)
  private
    _money: TArray<integer>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Paint(canvas: TCanvas);
    procedure Run;
  end;

implementation

uses
  System.Generics.Collections,
  VisibleDSA.AlgoForm;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create;
var
  i: integer;
begin
  SetLength(_money, 100);

  for i := 0 to Length(_money) - 1 do
    _money[i] := 100;
end;

destructor TAlgoVisualizer.Destroy;
begin
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TCanvas);
var
  w, h, i: integer;
begin
  TArray.Sort<Integer>(_money);

  w := canvas.Width div Length(_money);
  h := canvas.Height div 2;

  for i := 0 to Length(_money) - 1 do
  begin
    if _money[i] > 0 then
    begin
      TAlgoVisHelper.SetFill(CL_BLUE);
      TAlgoVisHelper.FillRectangle(canvas, i * w + 1, h - _money[i], w - 1, _money[i]);
    end
    else if _money[i] < 0 then
    begin
      TAlgoVisHelper.SetFill(CL_RED);
      TAlgoVisHelper.FillRectangle(canvas, i * w + 1, h, w - 1, Abs(_money[i]));
    end;
  end;
end;

procedure TAlgoVisualizer.Run;
var
  i, j: integer;
begin
  Randomize;

  for i := 0 to Length(_money) - 1 do
  begin
    j := Random(Length(_money));
    Inc(_money[j]);
    Dec(_money[i]);
  end;

  TAlgoVisHelper.Pause(0);
  AlgoForm.PaintBox.Repaint;
end;

end.
