program Insertion_Sort_Visualization;

uses
  System.StartUpCopy,
  FMX.Forms,
  VisibleDSA.AlgoForm in 'Source\VisibleDSA.AlgoForm.pas' {AlgoForm},
  VisibleDSA.AlgoVisHelper in 'Source\VisibleDSA.AlgoVisHelper.pas',
  VisibleDSA.AlgoVisualizer in 'Source\VisibleDSA.AlgoVisualizer.pas',
  VisibleDSA.InsertionSortData in 'Source\VisibleDSA.InsertionSortData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAlgoForm, AlgoForm);
  Application.Run;
end.
