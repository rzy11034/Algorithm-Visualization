unit VisibleDSA.MonteCarloPiData;

interface

uses
  System.SysUtils,
  System.Types,
  System.Generics.Collections,
  VisibleDSA.Circle;

type
  TMonteCarloPiData = class
  private type
    TList_TPoint = TList<TPoint>;

  private
    _circle: TCircle;
    _pointLists: TList_TPoint;
    _inSideCircle: integer;

  public
    constructor Create(Circle: TCircle);
    destructor Destroy; override;

    function GetCircle(): TCircle;
    function GetPoint(i: integer): TPoint;
    function GetPointNumber(): integer;
    function EstimatePI(): double;

    procedure AddPoint(p: TPoint);
  end;

implementation

{ TMonteCarloPiData }

constructor TMonteCarloPiData.Create(Circle: TCircle);
begin
  _inSideCircle := 0;
  _circle := Circle;
  _pointLists := TList_TPoint.Create;
end;

procedure TMonteCarloPiData.AddPoint(p: TPoint);
begin
  _pointLists.Add(p);

  if _circle.Contain(p) then
    Inc(_inSideCircle);
end;

destructor TMonteCarloPiData.Destroy;
begin
  FreeAndNil(_pointLists);
  inherited Destroy;
end;

function TMonteCarloPiData.EstimatePI(): double;
var
  circleArea, squareArea: integer;
begin
  if _pointLists.Count = 0 then
    Exit(0.0);

  circleArea := _inSideCircle;
  squareArea := _pointLists.Count;

  Result := 4 * circleArea / squareArea;
end;

function TMonteCarloPiData.GetCircle(): TCircle;
begin
  Result := _circle;
end;

function TMonteCarloPiData.GetPoint(i: integer): TPoint;
begin
  if (i < 0) or (i >= _pointLists.Count) then
    raise Exception.Create('Out of bound in GetPoint.');

  Result := _pointLists[i];
end;

function TMonteCarloPiData.GetPointNumber(): integer;
begin
  Result := _pointLists.Count;
end;

end.
