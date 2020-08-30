program D;

uses
  System.StartUpCopy,
  FMX.Forms,
  VisibleDSA.AlgoForm in 'Source\VisibleDSA.AlgoForm.pas' {AlgoForm},
  VisibleDSA.AlgoVisHelper in 'Source\VisibleDSA.AlgoVisHelper.pas',
  VisibleDSA.AlgoVisualizer in 'Source\VisibleDSA.AlgoVisualizer.pas',
  VisibleDSA.MazeData in 'Source\VisibleDSA.MazeData.pas',
  VisibleDSA.Maze in 'Source\VisibleDSA.Maze.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAlgoForm, AlgoForm);
  Application.Run;
end.
