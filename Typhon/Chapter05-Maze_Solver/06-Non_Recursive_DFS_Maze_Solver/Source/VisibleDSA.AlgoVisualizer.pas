﻿unit VisibleDSA.AlgoVisualizer;

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

    procedure __setData(x, y: integer; isPath, finished: boolean);

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
    procedure Run;
  end;

implementation

uses
  Generics.Collections,
  VisibleDSA.AlgoForm,
  VisibleDSA.Position;

type
  TStack_TPosition = specialize TStack<TPosition>;

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
var
  stack: TStack_TPosition;
  curPos: TPosition;
  i, newX, newY: integer;
begin
  __setData(-1, -1, False, False);

  stack := TStack_TPosition.Create;
  _data.Visited[_data.EntranceX, _data.EntranceY] := True;
  stack.Push(TPosition.Create(_data.EntranceX, _data.EntranceY));

  while stack.Count <> 0 do
  begin
    curPos := stack.Pop;
    __setData(curPos.X, curPos.Y, True, False);

    if (curPos.X = _data.ExitX) and (curPos.Y = _data.ExitY) then
      Break;

    for i := 0 to High(D) do
    begin
      newX := curPos.X + D[i, 0];
      newY := curPos.Y + D[i, 1];

      if (_data.InArea(newX, newY)) and
        (_data.GetMaze(newX, newY) = TMazeData.ROAD) and
        (_data.Visited[newX, newY] = False) then
      begin
        stack.Push(TPosition.Create(newX, newY));
        _data.Visited[newX, newY] := True;
      end;
    end;
  end;

  __setData(-1, -1, False, True);
end;

var
  can: integer;

procedure TAlgoVisualizer.__setData(x, y: integer; isPath, finished: boolean);
begin
  if _data.InArea(x, y) then
    _data.Path[x, y] := isPath;

  //TAlgoVisHelper.Pause(0);
  //AlgoForm.BGRAVirtualScreen.RedrawBitmap;

  if (finished) or (can >= 10) then
  begin
    AlgoForm.BGRAVirtualScreen.RedrawBitmap;
    can := 0;
  end
  else
  begin
    can += 1;
  end;
end;

end.
