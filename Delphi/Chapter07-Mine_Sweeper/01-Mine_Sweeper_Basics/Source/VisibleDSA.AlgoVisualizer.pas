unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.Classes,
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
  blockSide := 32;
  n := 20;
  m := 30;

  _data := TMineSweeperData.Create(n, m, 1);
  _width := blockSide * _data.M;
  _height := blockSide * _data.N;

  _form := form;
  _form.ClientWidth := _width;
  _form.ClientHeight := _height;

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
  str: UString;
  stm: TResourceStream;
  bmp: TBitmap;
  r,d: TRect;
begin
  w := _width div _data.M;
  h := _height div _data.N;

//  stm := TResourceStream.Create(HINSTANCE, TMineSweeperData.PNG_FLAG, RT_RCDATA);
//  bmp := TBitmap.CreateFromStream(stm);
//  r := TRect.Create(0, 0, bmp.Width, bmp.Height);
//  canvas.DrawBitmap(bmp, r, r, 1);
//  d :=  TRect.Create(100, 100, 132, 132);
//  canvas.DrawBitmap(bmp, r, d, 1);
//  d :=  TRect.Create(132, 132, 164, 164);
//  canvas.DrawBitmap(bmp, r, d, 1);
//  d :=  TRect.Create(150, 150, bmp.Width, bmp.Height);
//  canvas.DrawBitmap(bmp, r, d, 1);

  for i := 0 to _data.N - 1 do
  begin
    for j := 0 to _data.M - 1 do
    begin
      if _data.Mines[i, j] then
        str := TMineSweeperData.PNG_MINE
      else
        str := TMineSweeperData.PNG_BLOCK;

      stm := TResourceStream.Create(HINSTANCE, str, RT_RCDATA);
      TAlgoVisHelper.DrawImageFormResourceStream(canvas, stm, j * w, i * h, w, h);
    end;
  end;
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
