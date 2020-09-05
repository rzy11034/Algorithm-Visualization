unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Math,
  FMX.Graphics,
  System.UITypes,
  FMX.Forms,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.MineSweeperData;

type
  TAlgoVisualizer = class(TObject)
  public const
    D: TArr2D_int = [[-1, 0], [0, 1], [1, 0], [0, -1]];

  private
    _width: integer;
    _height: integer;
    _data: TMineSweeperData;

    procedure __setData(mbLeft: boolean; x, y: integer);
    procedure __mouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
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
  TAlgoForm(form).PaintBox.OnMouseDown := __mouseDown;
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
      if _data.Opened[i, j] then
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
  //__setData(true);
end;

procedure TAlgoVisualizer.__setData(mbLeft: boolean; x, y: integer);
begin
  if _data.InArea(x, y) and (not _data.Opened[x, y]) then
  begin
    if mbLeft then
    begin
      if _data.IsMine(x, y) then
        _data.Opened[x, y] := true
      else
        _data.Open(x, y);
    end
    else
      _data.Flags[x, y] := not _data.Flags[x, y];

    AlgoForm.PaintBox.Repaint;
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

procedure TAlgoVisualizer.__mouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  w, h: integer;
  i, j: integer;
  isLeft: boolean;
begin
  w := Trunc(AlgoForm.PaintBox.Width / _data.M);
  h := Trunc(AlgoForm.PaintBox.Height / _data.N);

  i := Trunc(y) div h;
  j := Trunc(X) div W;

  if Button = TMouseButton.mbLeft then
    isLeft := true
  else
    isLeft := false;

  __setData(isLeft, i, j);
end;

end.
