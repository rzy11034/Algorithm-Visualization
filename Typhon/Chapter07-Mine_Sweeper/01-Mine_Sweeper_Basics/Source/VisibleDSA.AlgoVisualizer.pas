unit VisibleDSA.AlgoVisualizer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  Forms,
  LCLType,
  BGRACanvas2D,
  BGRABitmap,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.MineSweeperData, DeepStar.Utils.UString;

type
  TAlgoVisualizer = class(TObject)
  const
    D: TArr2D_int = ((-1, 0), (0, 1), (1, 0), (0, -1));

  private
    _runningStatus: integer;
    _width: integer;
    _height: integer;
    _data: TMineSweeperData;

    procedure __setData(finished: boolean);

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
    procedure Run;
  end;

implementation

uses
  Buttons,
  VisibleDSA.AlgoForm;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  blockSide, n, m: integer;
begin
  blockSide := 32;
  n := 20;
  m := 30;

  _data := TMineSweeperData.Create(n, m, 1);

  _width := blockSide * _data.M;
  _height := blockSide * _data.N;

  form.ClientWidth := _width;
  form.ClientHeight := _height;

  form.Caption := 'Maze solver visualization --- ' + Format('W: %d, H: %d', [_width, _height]);
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TBGRACanvas2D);
var
  w, h: integer;
  i, j: integer;
  s: UString;
  png: TBGRABitmap;
begin
  w := _width div _data.M;
  h := _height div _data.N;

  for i := 0 to _data.N - 1 do
  begin
    for j := 0 to _data.M - 1 do
    begin
      if not _data.Mines[i, j] then
        s := TMineSweeperData.PNG_BLOCK
      else
        s := TMineSweeperData.PNG_MINE;

      png := TBGRABitmap.Create(TResourceStream.Create(HINSTANCE, string(s), RT_RCDATA));
      canvas.drawImage(png, j*w, i*h, w, h);
    end;
  end;


end;

procedure TAlgoVisualizer.Run;
begin
  __setData(true);
end;

procedure TAlgoVisualizer.__setData(finished: boolean);
begin
  if finished or (_runningStatus = 0) then
  begin
    TAlgoVisHelper.Pause(0);
    AlgoForm.BGRAVirtualScreen.RedrawBitmap;
    _runningStatus := 0;
  end
  else
  begin
    _runningStatus += 1;
  end;
end;

end.
