unit ThreeGatesExperiment;

interface

uses
  System.SysUtils,
  System.StrUtils;

type
  TThreeGatesExperiment = class(TObject)
  private
    _count: integer;

    function __play(changeDoor: boolean): boolean;

  public
    constructor Create(n: integer);
    procedure Run(changeDoor: boolean);
  end;

implementation

{ TThreeGatesExperiment }

constructor TThreeGatesExperiment.Create(n: integer);
begin
  if n <= 0 then
    raise Exception.Create('N must be larger than 0!');

  _count := n;
end;

procedure TThreeGatesExperiment.Run(changeDoor: boolean);
var
  i, wins: integer;
begin
  wins := 0;
  for i := 0 to _count do
  begin
    if __play(changeDoor) then
      Inc(wins);
  end;

  Writeln(IfThen(changeDoor, 'Change', 'not Change '));
  Writeln(' winning rate: ' + (wins / _count).ToString);
end;

function TThreeGatesExperiment.__play(changeDoor: boolean): boolean;
var
  prizeDoor: Integer;
  playerChoice: Integer;
  wins: Boolean;
begin
  prizeDoor := Random(3);
  playerChoice := Random(3);

  if (playerChoice = prizeDoor) then
  begin
    if changeDoor then
      wins := False
    else
      wins := true
  end
  else
  begin
    if changeDoor then
      wins := true
    else
      wins := False
  end;

  Result := wins;
end;

end.
