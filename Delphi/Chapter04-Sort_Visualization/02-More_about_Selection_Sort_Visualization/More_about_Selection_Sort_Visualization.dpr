program More_about_Selection_Sort_Visualization;

uses
  System.StartUpCopy,
  FMX.Forms,
  VisibleDSA.AlgoForm in 'Source\VisibleDSA.AlgoForm.pas' {AlgoForm},
  VisibleDSA.AlgoVisHelper in 'Source\VisibleDSA.AlgoVisHelper.pas',
  VisibleDSA.AlgoVisualizer in 'Source\VisibleDSA.AlgoVisualizer.pas',
  VisibleDSA.SelectionSortData in 'Source\VisibleDSA.SelectionSortData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAlgoForm, AlgoForm);
  Application.Run;
end.
