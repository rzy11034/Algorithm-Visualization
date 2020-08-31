program Real_Random_Maze_Generalization;

uses
  System.StartUpCopy,
  FMX.Forms,
  VisibleDSA.AlgoForm in 'Source\VisibleDSA.AlgoForm.pas' {AlgoForm},
  VisibleDSA.AlgoVisHelper in 'Source\VisibleDSA.AlgoVisHelper.pas',
  VisibleDSA.AlgoVisualizer in 'Source\VisibleDSA.AlgoVisualizer.pas',
  VisibleDSA.MazeData in 'Source\VisibleDSA.MazeData.pas',
  VisibleDSA.Position in 'Source\VisibleDSA.Position.pas',
  VisibleDSA.RandomQueue in 'Source\VisibleDSA.RandomQueue.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAlgoForm, AlgoForm);
  Application.Run;
end.
