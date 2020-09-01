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
  VisibleDSA.MazeData,
  VisibleDSA.Position;

type
  TAlgoVisualizer = class(TObject)
  const
    D: TArr2D_int = ((-1, 0), (0, 1), (1, 0), (0, -1));

  private
    _runningStatus: integer;
    _width: integer;
    _height: integer;
    _data: TMazeData;

    procedure __setRoadData(x, y: integer; finished: boolean);
    procedure __setPathData(x, y: integer; isPath, finished: boolean);
    procedure __keyPress(Sender: TObject; var Key: char);

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
    procedure Run;
  end;

implementation

uses
  VisibleDSA.AlgoForm,
  VisibleDSA.RandomQueue;

type
  TQueue_Position = specialize TRandomQueue<TPosition>;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  blockSide, size: integer;
begin
  size := 101;
  blockSide := 606 div size;
  _data := TMazeData.Create(size, size);

  _width := blockSide * _data.M;
  _height := blockSide * _data.N;
  _runningStatus := 0;

  form.ClientWidth := _width;
  form.ClientHeight := _height;

  form.OnKeyPress := @__keyPress;

  form.Caption := 'Maze solver visualization --- ' +
    Format('W: %d, H: %d', [_width, _height]);
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

procedure TAlgoVisualizer.__keyPress(Sender: TObject; var Key: char);

  procedure __go__;

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
    for i := 0 to _data.N - 1 do
    begin
      for j := 0 to _data.M - 1 do
      begin
        _data.Visited[i, j] := false;
        _data.Path[i, j] := false;
      end;
    end;

    if __go__(_data.EntranceX, _data.EntranceY) = false then
      raise Exception.Create('The maze has NO solution!');

    __setPathData(-1, -1, false, true);
  end;

begin
  if Key = ' ' then
    __go__;
end;

procedure TAlgoVisualizer.__setPathdata(x, y: integer; isPath, finished: boolean);
begin
  if _data.InArea(x, y) then
    _data.Path[x, y] := isPath;

  if finished or (_runningStatus >= 10) then
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

procedure TAlgoVisualizer.__setRoadData(x, y: integer; finished: boolean);
begin
  if _data.InArea(x, y) then
    _data.Maze[x, y] := TMazeData.ROAD;

  //TAlgoVisHelper.Pause(1);
  //if finished then
  //begin
  //  AlgoForm.BGRAVirtualScreen.RedrawBitmap;
  //  _runningStatus := 0;
  //end
  //else
  //begin
  //  _runningStatus += 1;
  //end;
end;

end.
