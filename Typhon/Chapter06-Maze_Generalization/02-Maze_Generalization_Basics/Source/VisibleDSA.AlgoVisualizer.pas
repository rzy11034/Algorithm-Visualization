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
  VisibleDSA.MazeData;

type
  TAlgoVisualizer = class(TObject)
  const
    D: TArr2D_int = ((-1, 0), (0, 1), (1, 0), (0, -1));

  private
    _width: integer;
    _height: integer;
    _data: TMazeData;

    procedure __setData(x, y: integer; finished: boolean);

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
    procedure Run;
  end;

implementation

uses
  VisibleDSA.AlgoForm;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  blockSide, size: integer;
begin
  size := 101;
  blockSide := 808 div size;
  _data := TMazeData.Create(size, size);

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

      TAlgoVisHelper.FillRectangle(canvas, j * w, i * h, w, h);
    end;
  end;
end;

procedure TAlgoVisualizer.Run;
  procedure __go__(x, y: integer);
  var
    i, newX, newY: integer;
  begin
    if not _data.InArea(x, y) then
      raise Exception.Create('X, Y are out of index in go function!');

    _data.Visited[x, y] := True;

    for i := 0 to High(D) do
    begin
      newX := x + D[i, 0] * 2;
      newY := y + D[i, 1] * 2;

      if _data.InArea(newX, newY) and (not _data.Visited[newX, newY]) then
      begin
        __setData(x + D[i, 0], y + D[i, 1], False);
        __go__(newX, newY);
      end;
    end;

  end;

begin
  __setData(-1, -1, False);
  __go__(_data.EntranceX, _data.EntranceY + 1);
  __setData(-1, -1, True);
end;

var can: integer;

procedure TAlgoVisualizer.__setData(x, y: integer; finished: boolean);
begin
  if _data.InArea(x, y) then
    _data.Maze[x, y] := TMazeData.ROAD;

  if finished or (can >= 5) then
  begin
    TAlgoVisHelper.Pause(0);
    AlgoForm.BGRAVirtualScreen.RedrawBitmap;
    can := 0;
  end
  else
  begin
    can += 1;
  end;
end;

end.
