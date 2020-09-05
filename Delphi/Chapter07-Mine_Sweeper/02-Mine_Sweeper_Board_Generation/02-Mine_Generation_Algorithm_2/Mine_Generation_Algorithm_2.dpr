program Mine_Generation_Algorithm_2;



{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  VisibleDSA.AlgoForm in 'Source\VisibleDSA.AlgoForm.pas' {AlgoForm},
  VisibleDSA.AlgoVisHelper in 'Source\VisibleDSA.AlgoVisHelper.pas',
  VisibleDSA.AlgoVisualizer in 'Source\VisibleDSA.AlgoVisualizer.pas',
  VisibleDSA.MineSweeperData in 'Source\VisibleDSA.MineSweeperData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAlgoForm, AlgoForm);
  Application.Run;
end.
