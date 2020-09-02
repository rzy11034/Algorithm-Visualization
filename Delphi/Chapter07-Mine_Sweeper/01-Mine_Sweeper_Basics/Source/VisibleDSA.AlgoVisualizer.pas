unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.SysUtils,
  System.Types,
  System.Math,
  FMX.Graphics,
  FMX.Forms,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.MineSweeperData;

type
  TAlgoVisualizer = class(TObject)
  const
    D: TArr2D_int = [[-1, 0], [0, 1], [1, 0], [0, -1]];

  private
    _runningStatus: integer;
    _width: integer;
    _height: integer;
    _data: TMineSweeperData;
    _form: TForm;

    procedure __setData(finished: boolean);

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TCanvas);
    procedure Run;
  end;

implementation

uses
  VisibleDSA.AlgoForm;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  blockSide, n, m: integer;
begin
  blockSide := 16;
  n := 20;
  m := 30;

  _data := TMineSweeperData.Create(n, m, 1);

  _form := form;
  _form.ClientWidth := blockSide * _data.M;
  _form.ClientHeight := blockSide * _data.N;

  _width := _form.ClientWidth;
  _height := _form.ClientHeight;

  _form.Caption := 'Maze solver visualization' +
    Format('W: %d, H: %d', [_Width, _Height]);;
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TCanvas);
var
  w, h: integer;
  i, j: integer;
begin
  w := _width div _data.N;
  h := _height div _data.M;

end;

procedure TAlgoVisualizer.Run;
begin
  __setData(true);
end;

procedure TAlgoVisualizer.__setData(finished: boolean);
begin
  if finished or (_runningStatus >= 5) then
  begin
    TAlgoVisHelper.Pause(0);
    AlgoForm.PaintBox.Repaint;
    _runningStatus := 0;
  end
  else
  begin
    _runningStatus := _runningStatus + 1;
  end;
end;

end.
