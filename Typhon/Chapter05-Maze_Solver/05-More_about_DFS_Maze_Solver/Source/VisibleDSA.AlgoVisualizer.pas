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
    _form: TForm;

    procedure __setData(x, y: integer; isPath: boolean);

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
  blockSide: integer;
begin
  blockSide := 8;
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

      if _data.Path[i, j] then
        TAlgoVisHelper.SetFill(CL_YELLOW);

      TAlgoVisHelper.FillRectangle(canvas, j * w, i * h, h, w);
    end;
  end;
end;

procedure TAlgoVisualizer.Run;

// 从(x,y)的位置开始求解迷宫，如果求解成功，返回true；否则返回false
  function __go__(x, y: integer): boolean;
  var
    i, newX, newY: integer;
  begin
    if not _data.InArea(x, y) then
      raise Exception.Create('X, Y are out of index in go function!');

    _data.Visited[x, y] := True;
    __setData(x, y, True);

    if (x = _data.ExitX) and (y = _data.ExitY) then
    begin
      Result := True;
      Exit;
    end;

    for i := 0 to High(D) do
    begin
      newX := x + D[i, 0];
      newY := y + D[i, 1];

      if (_data.InArea(newX, newY)) and
        (_data.GetMaze(newX, newY) = TMazeData.ROAD) and
        (_data.Visited[newX, newY] = False) then
      begin
        if __go__(newX, newY) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;

    // 回溯
    __setData(x, y, False);

    Result := False;
  end;

begin
  __setData(-1, -1, False);

  if not __go__(_data.EntranceX, _data.EntranceY) then
    WriteLn('The maze has NO solution!');

  __setData(-1, -1, False);
end;

var
  can: integer;

procedure TAlgoVisualizer.__setData(x, y: integer; isPath: boolean);
begin
  if _data.InArea(x, y) then
    _data.Path[x, y] := isPath;

  //TAlgoVisHelper.Pause(0);
  //AlgoForm.BGRAVirtualScreen.RedrawBitmap;

  if can < 10 then
    can += 1
  else
  begin
    AlgoForm.BGRAVirtualScreen.RedrawBitmap;
    can := 0;
  end;
end;

end.
