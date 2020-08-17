unit WinningPrize;

interface

uses
  System.SysUtils;

type
  TWinningPrize = class(TObject)
  private
    _chance: double;
    _playTime: integer;
    _n: integer;

    function __play: boolean;

  public
    constructor Create(chance: double; playTime: integer; n: integer);
    destructor Destroy; override;

    procedure Run;
  end;

implementation

{ TWinningPrize }

constructor TWinningPrize.Create(chance: double; playTime: integer; n: integer);
begin
  if (chance < 0.0) or (chance > 1.0) then
    raise Exception.Create('chance must be between 0 and 1!');

  if (playTime <= 0) or (n <= 0) then
    raise Exception.Create('playTime or N must be larger than 0!');

  _chance := chance;
  _playTime := playTime;
  _n := n;
end;

destructor TWinningPrize.Destroy;
begin
  inherited Destroy;
end;

procedure TWinningPrize.Run;
var
  wins, i: integer;
begin
  wins := 0;

  for i := 0 to _n - 1 do
  begin
    if __play then
      Inc(wins);
  end;

  WriteLn('winning rate: ' + (wins / _n).ToString);
end;

function TWinningPrize.__play: boolean;
var
  i: integer;
begin
  Randomize;

  for i := 0 to _playTime - 1 do
  begin
    if Random < _chance then
      Exit(True);
  end;

  Result := False;
end;

end.
