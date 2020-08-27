unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils.UString;

const
  END_OF_PROGRAM_CH: UString = '按任意键继续...';

procedure Run;

implementation

uses
  VisibleDSA.MazeData;

procedure Run;
var
  maze: TMazeData;
begin
  maze := TMazeData.Create(TMazeData.FILE_NAME);
  maze.PrintMaze;
  maze.Free;
end;

end.
