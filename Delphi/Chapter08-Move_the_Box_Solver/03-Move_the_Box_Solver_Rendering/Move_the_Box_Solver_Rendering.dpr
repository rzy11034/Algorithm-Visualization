program Move_the_Box_Solver_Rendering;

uses
  System.StartUpCopy,
  FMX.Forms,
  DeepStar.Utils.UString in 'Source\DeepStar.Utils.UString.pas',
  VisibleDSA.AlgoForm in 'Source\VisibleDSA.AlgoForm.pas' {AlgoForm},
  VisibleDSA.AlgoVisHelper in 'Source\VisibleDSA.AlgoVisHelper.pas',
  VisibleDSA.AlgoVisualizer in 'Source\VisibleDSA.AlgoVisualizer.pas',
  VisibleDSA.Board in 'Source\VisibleDSA.Board.pas',
  VisibleDSA.GameData in 'Source\VisibleDSA.GameData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAlgoForm, AlgoForm);
  Application.Run;
end.
