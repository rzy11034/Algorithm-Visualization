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
  public const
    D: TArr2D_int = [[-1, 0], [0, 1], [1, 0], [0, -1]];

  private
    _runningStatus: integer;
    _width: integer;
    _height: integer;
    _data: TMineSweeperData;

    procedure __setData(finished: boolean);
    procedure __desktopCenter(form: TForm);

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
  blockSide, n, m, numberMine: integer;
begin
  blockSide := 32;
  n := 20;
  m := 20;
  numberMine := 50;

  _data := TMineSweeperData.Create(n, m, numberMine);
  _width := blockSide * _data.M;
  _height := blockSide * _data.N;

  form.ClientWidth := _width;
  form.ClientHeight := _height;

  form.Caption := 'Mine Sweeper --- ' + Format('W: %d, H: %d', [_Width, _Height]);
  __desktopCenter(form);
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
begin
  w := _width div _data.M;
  h := _height div _data.N;

  for i := 0 to _data.N - 1 do
  begin
    for j := 0 to _data.M - 1 do
    begin
      if not _data.Opened[i, j] then
      begin
        if _data.IsMine(i, j) then
          str := TMineSweeperData.PNG_MINE
        else
          str := TMineSweeperData.PNG_NUM(_data.Numbers[i, j]);
      end
      else
      begin
        if _data.Flags[i, j] then
          str := TMineSweeperData.PNG_FLAG
        else
          str := TMineSweeperData.PNG_BLOCK;
      end;

      stm := TResourceStream.Create(HINSTANCE, string(str), RT_RCDATA);
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

procedure TAlgoVisualizer.__desktopCenter(form: TForm);
var
  top, left: Double;
begin
  top := (Screen.Height div 2) - (form.ClientHeight div 2);
  left := (Screen.Width div 2) - (form.ClientWidth div 2);

  form.top := Trunc(top);
  form.left := Trunc(left);
end;

end.
