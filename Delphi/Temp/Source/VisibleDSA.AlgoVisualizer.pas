unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Types,
  System.Math,
  Vcl.Graphics,
  Vcl.Forms,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.MazeData;

type
  TAlgoVisualizer = class(TObject)
  const
    D: TArr2D_int = [[-1, 0], [0, 1], [1, 0], [0, -1]];

  private
    _width: integer;
    _height: integer;
    _data: TMazeData;
    _form: TForm;

    procedure __setData(x, y: integer);

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
  blockSide: integer;
begin
  blockSide := 6;
  _data := TMazeData.Create(TMazeData.FILE_NAME);

  _form := form;
  _form.ClientWidth := blockSide * _data.M;
  _form.ClientHeight := blockSide * _data.N;

  _width := _form.ClientWidth;
  _height := _form.ClientHeight;
  _form.Caption := 'Maze solver visualization';
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

  for i := 0 to _data.N - 1 do
  begin
    for j := 0 to _data.M - 1 do
    begin
      if _data.GetMaze(i, j) = TMazeData.WALL then
        TAlgoVisHelper.SetFill(CL_LIGHTBLUE)
      else
        TAlgoVisHelper.SetFill(CL_WHITE);

      if _data.Path[i, j] then
        TAlgoVisHelper.SetFill(CL_YELLOW);

      TAlgoVisHelper.FillRectangle(canvas, j * w, i * h, h, w);
    end;
  end;

  //canvas.Brush.Color := CL_RED;
  //canvas.FillRect(0, 0, 50, 50);
  //canvas.FillRect(50, 50, 100, 100);
end;

procedure TAlgoVisualizer.Run;

  procedure __go__(x, y: integer);
  var
    i, newX, newY: integer;
  begin
    if not _data.InArea(x, y) then
      raise Exception.Create('X, Y are out of index in go function!');

    _data.Visited[x, y] := true;
    __setData(x, y);

    if (x = _data.ExitX) and (y = _data.ExitY) then
      Exit;

    for i := 0 to High(D) do
    begin
      newX := x + D[i, 0];
      newY := y + D[i, 1];

      if (_data.InArea(newX, newY)) and
        (_data.GetMaze(newX, newY) = TMazeData.ROAD) and
        (_data.Visited[newX, newY] = false) then
      begin
        __go__(newX, newY);
      end;
    end;

  end;

begin
  __setData(-1, -1);
  __go__(_data.EntranceX, _data.EntranceY);
  __setData(-1, -1);
end;

procedure TAlgoVisualizer.__setData(x, y: integer);
begin
  if _data.InArea(x, y) then
    _data.Path[x, y] := true;

  TAlgoVisHelper.Pause(10);
  AlgoForm.Repaint;

  //if can < 10 then
  //  can += 1
  //else
  //begin
  //  AlgoForm.BGRAVirtualScreen.RedrawBitmap;
  //  can := 0;
  //end;
end;

end.
