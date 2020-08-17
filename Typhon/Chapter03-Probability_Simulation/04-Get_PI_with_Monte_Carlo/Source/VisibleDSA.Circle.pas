unit VisibleDSA.Circle;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TCircle = class(TObject)
  private
    _x: integer;
    _y: integer;
    _r: integer;

  public
    constructor Create(new_x, new_y, new_r: integer);
    destructor Destroy; override;
    function Contain(p: TPoint): boolean;

    property X: integer read _x;
    property Y: integer read _y;
    property R: integer read _r;
  end;

implementation

{ TCircle }

constructor TCircle.Create(new_x, new_y, new_r: integer);
begin
  _x := new_x;
  _y := new_y;
  _r := new_r;
end;

function TCircle.Contain(p: TPoint): boolean;
begin
  // Result := Power((X - p.X), 2) + Power((Y - p.Y), 2) <= R * R;
  Result := TPoint.PointInCircle(p, TPoint.Create(X, Y), R);
end;

destructor TCircle.Destroy;
begin
  inherited Destroy;
end;

end.
