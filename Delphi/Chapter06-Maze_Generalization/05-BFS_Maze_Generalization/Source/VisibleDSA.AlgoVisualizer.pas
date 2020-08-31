unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.SysUtils,
  System.Types,
  System.Math,
  FMX.Graphics,
  FMX.Forms,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.MazeData;

type
  TAlgoVisualizer = class(TObject)
  const
    D: TArr2D_int = [[-1, 0], [0, 1], [1, 0], [0, -1]];

  private
    _runningStatus: integer;
    _width: integer;
    _height: integer;
    _data: TMazeData;
    _form: TForm;

    procedure __setData(x, y: integer; finished: boolean);

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TCanvas);
    procedure Run;
  end;

implementation

uses
  System.Generics.Collections,
  VisibleDSA.AlgoForm,
  VisibleDSA.Position;

type
  TTQueue_Position = TQueue<TPosition>;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  blockSide, size: integer;
begin
  size := 101;
  blockSide := 808 div size;
  _data := TMazeData.Create(size, size);

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
var
  queue: TTQueue_Position;
  curPos: TPosition;
  i, newX, newY: integer;
begin
  queue := TTQueue_Position.Create;
  try
    curPos := TPosition.Create(_data.EntranceX, _data.EntranceY + 1);
    queue.Enqueue(curPos);
    _data.Visited[curPos.X, curPos.Y] := true;

    while queue.Count > 0 do
    begin
      curPos := queue.Dequeue;

      for i := 0 to High(D) do
      begin
        newX := curPos.X + D[i, 0] * 2;
        newY := curPos.Y + D[i, 1] * 2;

        if _data.InArea(newX, newY) and (_data.Visited[newX, newY] = false) then
        begin
          queue.Enqueue(TPosition.Create(newX, newY));
          _data.Visited[newX, newY] := true;
          __setData(curPos.X + D[i, 0], curPos.Y + D[i, 1], false);
        end;
      end;
    end;
  finally
    queue.Free;
  end;

  __setData(-1, -1, true);
end;

procedure TAlgoVisualizer.__setData(x, y: integer; finished: boolean);
begin
  if _data.InArea(x, y) then
    _data.Maze[x, y] := TMazeData.ROAD;

  if finished or (_runningStatus = 0) then
  begin
    TAlgoVisHelper.Pause(1);
    AlgoForm.PaintBox.Repaint;
    _runningStatus := 0;
  end
  else
  begin
    _runningStatus := _runningStatus + 1;
  end;
end;

end.
