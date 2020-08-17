unit VisibleDSA.MonteCarloExperiment;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Types,
  VisibleDSA.Circle,
  VisibleDSA.MonteCarloPiData;

type
  TMonteCarloExperiment = class
  private
    _squareSide: integer;
    _count: integer;
    _outputInterval: integer;

  public
    constructor Create(squareSide, n: integer);
    destructor Destroy; override;

    procedure Run;
    procedure SetOutputInterval(interval: integer);
  end;

implementation

{ TMonteCarloExperiment }

constructor TMonteCarloExperiment.Create(squareSide, n: integer);
begin
  if (squareSide <= 0) or (n <= 0) then
    raise Exception.Create('SquareSide and N must larger than zero!');

  _squareSide := squareSide;
  _count := n;
  _outputInterval := 100;
end;

destructor TMonteCarloExperiment.Destroy;
begin
  inherited Destroy;
end;

procedure TMonteCarloExperiment.Run;
var
  Circle: TCircle;
  Data: TMonteCarloPiData;
  i: integer;
  p: TPoint;
begin
  Circle := TCircle.Create(_squareSide div 2, _squareSide div 2, _squareSide div 2);
  Data := TMonteCarloPiData.Create(Circle);

  Randomize;

  for i := 0 to _count - 1 do
  begin
    if (i mod _outputInterval = 0) then
      Writeln(FloatToStr(Data.EstimatePI));

    p := TPoint.Create(Random(_squareSide), Random(_squareSide));
    Data.AddPoint(p);
  end;

end;

procedure TMonteCarloExperiment.SetOutputInterval(interval: integer);
begin
  if (interval <= 0) then
    raise Exception.Create('interval must be larger than zero');

  _outputInterval := interval;
end;

end.
