unit VisibleDSA.Circle;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TCircle = class(TObject)
  private
    procedure __checkCollision(min_x, min_y, max_x, max_y: integer);

  public
    IsFilled: boolean;
    R: integer;
    X: integer;
    Y: integer;
    VX: integer;
    VY: integer;

    constructor Create(new_x, new_y, new_r, new_vx, new_vy: integer);
    destructor Destroy; override;
    procedure Move(min_x, min_y, max_x, max_y: integer);
    function Contain(AX, AY: single): boolean;
  end;

implementation

{ TCircle }

constructor TCircle.Create(new_x, new_y, new_r, new_vx, new_vy: integer);
begin
  X := new_x;
  Y := new_y;
  Vx := new_vx;
  Vy := new_vy;
  R := new_r;
  IsFilled := False;
end;

function TCircle.Contain(Ax, Ay: single): boolean;
begin
  Result := (X - Ax) * (X - Ax) + (Y - Ay) * (Y - Ay) <= R * R;
end;

destructor TCircle.Destroy;
begin
  inherited Destroy;
end;

procedure TCircle.Move(min_x, min_y, max_x, max_y: integer);
begin
  X += VX;
  Y += VY;
  __checkCollision(min_x, min_y, max_x, max_y);
end;

procedure TCircle.__checkCollision(min_x, min_y, max_x, max_y: integer);
begin
  if X - R < min_x then
  begin
    X := R;
    VX := -VX;
  end;

  if X + R >= max_x then
  begin
    X := max_x - R;
    VX := -VX;
  end;

  if Y - R < min_y then
  begin
    R := R;
    VY := -VY;
  end;

  if Y + R >= max_y then
  begin
    Y := max_y - R;
    VY := -VY;
  end;
end;

end.
