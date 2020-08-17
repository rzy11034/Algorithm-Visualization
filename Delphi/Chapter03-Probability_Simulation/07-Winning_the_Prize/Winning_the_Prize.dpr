program Winning_the_Prize;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  WinningPrize in 'Source\WinningPrize.pas';

var
  chance: double = 0.2;
  playTime: integer = 5;
  n: integer = 1000000;
  exp: TWinningPrize;

begin
  try
    exp := TWinningPrize.Create(chance, playTime, n);
    exp.Run;
    exp.Free;

    WriteLn('-------------------');
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
