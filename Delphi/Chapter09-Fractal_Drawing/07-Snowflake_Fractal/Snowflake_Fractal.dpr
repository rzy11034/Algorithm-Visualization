program Snowflake_Fractal;

uses
  System.StartUpCopy,
  FMX.Forms,
  DeepStar.Utils.UString in 'Source\DeepStar.Utils.UString.pas',
  VisibleDSA.AlgoForm in 'Source\VisibleDSA.AlgoForm.pas' {AlgoForm},
  VisibleDSA.AlgoVisHelper in 'Source\VisibleDSA.AlgoVisHelper.pas',
  VisibleDSA.AlgoVisualizer in 'Source\VisibleDSA.AlgoVisualizer.pas',
  VisibleDSA.FractalData in 'Source\VisibleDSA.FractalData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAlgoForm, AlgoForm);
  Application.Run;
end.
