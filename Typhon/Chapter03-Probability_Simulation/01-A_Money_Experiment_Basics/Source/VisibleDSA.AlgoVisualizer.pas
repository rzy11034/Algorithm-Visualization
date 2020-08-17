unit VisibleDSA.AlgoVisualizer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  Forms,
  BGRACanvas2D,
  VisibleDSA.AlgoVisHelper;

type
  TAlgoVisualizer = class(TObject)
  private
    _money: array of integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
    procedure Run;
  end;

implementation

uses
  Generics.Collections,
  VisibleDSA.AlgoForm;

type
  TArrayInt = specialize TArrayHelper<integer>;

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

procedure TAlgoVisualizer.Paint(canvas: TBGRACanvas2D);
var
  w, i: integer;
begin
  TArrayInt.Sort(_money);
  w := canvas.Width div Length(_money);

  TAlgoVisHelper.SetFill(CL_BLUE);

  for i := 0 to Length(_money) - 1 do
  begin
    TAlgoVisHelper.FillRectangle(canvas, i * w + 1, canvas.Height - _money[i], w - 1, _money[i]);
  end;
end;

procedure TAlgoVisualizer.Run;
var
  i, j: integer;
begin
  Randomize;

  for i := 0 to Length(_money) - 1 do
  begin
    if _money[i] > 0 then
    begin
      j := Random(Length(_money));
      Inc(_money[j]);
      Dec(_money[i]);
    end;
  end;

  TAlgoVisHelper.Pause(0);
  AlgoForm.BGRAVirtualScreen.RedrawBitmap;
end;

end.
