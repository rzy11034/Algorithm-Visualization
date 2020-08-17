program Three_Gates_Problem;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ThreeGatesExperiment in 'Source\ThreeGatesExperiment.pas';

var
  N: integer = 10000000;
  exp: tThreeGatesExperiment;

begin
  try
    exp := tThreeGatesExperiment.Create(N);
    exp.Run(True);
    Writeln;
    exp.Run(False);

    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
