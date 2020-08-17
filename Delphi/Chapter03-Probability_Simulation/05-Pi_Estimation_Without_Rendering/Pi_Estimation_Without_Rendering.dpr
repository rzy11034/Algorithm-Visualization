program Pi_Estimation_Without_Rendering;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  VisibleDSA.Circle in 'Source\VisibleDSA.Circle.pas',
  VisibleDSA.MonteCarloExperiment in 'Source\VisibleDSA.MonteCarloExperiment.pas',
  VisibleDSA.MonteCarloPiData in 'Source\VisibleDSA.MonteCarloPiData.pas';

var
  Exp: TMonteCarloExperiment;
  squareSide: integer = 800;
  N: integer = 100000000;

begin
  try
    Exp := TMonteCarloExperiment.Create(squareSide, N);
    Exp.SetOutputInterval(100000);
    Exp.Run;
    WriteLn('------------');
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
