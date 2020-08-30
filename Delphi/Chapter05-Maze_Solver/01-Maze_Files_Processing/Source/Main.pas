unit Main;

interface

uses
  System.Classes,
  System.SysUtils,
  VisibleDSA.MazeData;

const
  END_OF_PROGRAM_CH: UString = '按任意键继续...';

procedure Run;

implementation

procedure Run;
var
  maze: TMazeData;
begin
  maze := TMazeData.Create(TMazeData.FILE_NAME);
  maze.PrintMaze;
  maze.Free;
end;

end.
