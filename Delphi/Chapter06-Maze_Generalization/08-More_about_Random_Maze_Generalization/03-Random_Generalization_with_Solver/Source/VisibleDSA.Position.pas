unit VisibleDSA.Position;

interface

uses
  System.SysUtils;

type
  TPosition = class(TObject)
  private
    _prev: TPosition;
    _x: integer;
    _y: integer;

  public
    constructor Create(x, y: integer; prev: TPosition = nil);
    destructor Destroy; override;

    property X: integer read _X;
    property Y: integer read _Y;
    property Prev: TPosition read _Prev;
  end;

implementation

{ TPosition }

constructor TPosition.Create(x, y: integer; prev: TPosition = nil);
begin
  _x := x;
  _y := y;
  _prev := prev;
end;

destructor TPosition.Destroy;
begin
  inherited Destroy;
end;

end.
