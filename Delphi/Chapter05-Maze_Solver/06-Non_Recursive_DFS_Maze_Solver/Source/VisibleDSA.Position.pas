unit VisibleDSA.Position;

interface

uses
  System.SysUtils;

type
  TPosition = class(TObject)
  private
    _x: integer;
    _y: integer;

  public
    constructor Create(x, y: integer);
    destructor Destroy; override;

    property X: integer read _X;
    property Y: integer read _Y;
  end;

implementation

{ TPosition }

constructor TPosition.Create(x, y: integer);
begin
  _x := x;
  _y := y;
end;

destructor TPosition.Destroy;
begin
  inherited Destroy;
end;

end.
