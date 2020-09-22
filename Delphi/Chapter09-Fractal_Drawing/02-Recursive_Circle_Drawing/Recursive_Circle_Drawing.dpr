program Recursive_Circle_Drawing;

uses
  System.StartUpCopy,
  FMX.Forms,
  DeepStar.Utils.UString in 'Source\DeepStar.Utils.UString.pas',
  VisibleDSA.AlgoForm in 'Source\VisibleDSA.AlgoForm.pas' {AlgoForm},
  VisibleDSA.AlgoVisHelper in 'Source\VisibleDSA.AlgoVisHelper.pas',
  VisibleDSA.AlgoVisualizer in 'Source\VisibleDSA.AlgoVisualizer.pas',
  VisibleDSA.CircleData in 'Source\VisibleDSA.CircleData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAlgoForm, AlgoForm);
  Application.Run;
end.
