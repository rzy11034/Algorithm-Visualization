program Maze_Files_Processing;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Main in 'Source\Main.pas',
  VisibleDSA.MazeData in 'Source\VisibleDSA.MazeData.pas';

begin
  try
    Run;

    Writeln(END_OF_PROGRAM_CH);
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
