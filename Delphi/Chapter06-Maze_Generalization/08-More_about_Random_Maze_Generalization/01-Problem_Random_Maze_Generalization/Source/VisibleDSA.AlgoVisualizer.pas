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
  VisibleDSA.MazeData;

type
  TAlgoVisualizer = class(TObject)
  private const
    D: TArr2D_int = [[-1, 0], [0, 1], [1, 0], [0, -1]];

  private
    _runningStatus: integer;
    _width: integer;
    _height: integer;
    _data: TMazeData;
    _form: TForm;

    procedure __setRoadData(x, y: integer; finished: boolean);
    procedure __setPathdata(x, y: integer; isPath, finished: boolean);
    procedure __KeyPress(Sender: TObject; var Key: Word;  var KeyChar: Char; Shift: TShiftState);

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TCanvas);
    procedure Run;
    procedure Go;
  end;

implementation

uses
  VisibleDSA.RandomQueue,
  VisibleDSA.AlgoForm,
  VisibleDSA.Position;

type
  TQueue_Position = TRandomQueue<TPosition>;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  blockSide, size: integer;
begin
  size := 31;
  blockSide := 808 div size;
  _data := TMazeData.Create(size, size);

  _form := form;
  _form.ClientWidth := blockSide * _data.M;
  _form.ClientHeight := blockSide * _data.N;

  _width := _form.ClientWidth;
  _height := _form.ClientHeight;

  _form.OnKeyDown := __KeyPress;

  _form.Caption := 'Maze solver visualization' +
    Format('W: %d, H: %d', [_Width, _Height]);;
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Go;
  function __go__(x, y: integer): boolean;
  var
    i, newX, newY: integer;
  begin
    _data.Visited[x, y] := true;
    __setPathdata(x, y, true, false);

    if (x = _data.ExitX) and (y = _data.ExitY) then
    begin
      Result := true;
      Exit;
    end;

    for i := 0 to High(D) do
    begin
      newX := x + D[i, 0];
      newY := y + D[i, 1];

      if (_data.InArea(newX, newY)) and
        (_data.GetMaze(newX, newY) = TMazeData.ROAD) and
        (_data.Visited[newX, newY] = false) then
      begin
        if __go__(newX, newY) then
        begin
          Result := true;
          Exit;
        end;
      end;
    end;

    // 回溯
    __setPathdata(x, y, false, false);
    _data.Path[x, y] := false;
    Result := false;
  end;

var
  i, j: integer;
begin
  // 重置 _data.Visited 为遍历迷宫做准备
  for i := 0 to High(_data.Visited) do
    for j := 0 to High(_data.Visited[i]) do
      _data.Visited[i, j] := false;

  if not __go__(_data.EntranceX, _data.EntranceY) then
    raise Exception.Create('The maze has NO solution!');

  __setPathdata(-1, -1, false, true);
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
      if _data.InMist[i, j] = true then
        TAlgoVisHelper.SetFill(CL_BLACK)
      else if _data.Path[i, j] = true then
        TAlgoVisHelper.SetFill(CL_YELLOW)
      else if _data.Maze[i, j] = TMazeData.WALL then
        TAlgoVisHelper.SetFill(CL_LIGHTBLUE)
      else
        TAlgoVisHelper.SetFill(CL_WHITE);

      TAlgoVisHelper.FillRectangle(canvas, j * w, i * h, w, h);
    end;
  end;
end;

procedure TAlgoVisualizer.Run;
var
  queue: TQueue_Position;
  curPos: TPosition;
  i, newX, newY: integer;
begin
  queue := TQueue_Position.Create;
  try
    curPos := TPosition.Create(_data.EntranceX, _data.EntranceY + 1);
    queue.Enqueue(curPos);
    _data.Visited[curPos.X, curPos.Y] := true;
    _data.OpenMist(curPos.X, curPos.Y);

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
          _data.OpenMist(newX, newY);
          __setRoadData(curPos.X + D[i, 0], curPos.Y + D[i, 1], false);
        end;
      end;
    end;
  finally
    queue.Free;
  end;

  __setRoadData(-1, -1, true);
end;

procedure TAlgoVisualizer.__KeyPress(Sender: TObject; var Key: Word;  var KeyChar: Char; Shift: TShiftState);
var
  i, j: Integer;
begin
  for i := 0 to High(_data.Path) do
    for j := 0 to High(_data.Path[i]) do
      _data.Path[i, j] := false;

  if KeyChar = ' ' then
    Go;
end;

procedure TAlgoVisualizer.__setPathdata(x, y: integer; isPath, finished: boolean);
begin
  if _data.InArea(x, y) then
    _data.Path[x, y] := isPath;

  if finished or (_runningStatus >= 20) then
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

procedure TAlgoVisualizer.__setRoadData(x, y: integer; finished: boolean);
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
    TAlgoVisHelper.Pause(0);
    _runningStatus := _runningStatus + 1;
  end;
end;

end.
