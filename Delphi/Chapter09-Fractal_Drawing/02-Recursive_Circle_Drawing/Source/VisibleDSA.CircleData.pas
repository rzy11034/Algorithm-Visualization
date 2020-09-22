unit VisibleDSA.CircleData;

interface

uses
  System.SysUtils;

type
  TCircleData = class(TObject)
  private
    _depth: integer;
    _startR: integer;
    _startX: integer;
    _startY: integer;
    _step: integer;

  public
    constructor Create(x, y, r, depth, step: integer);
    destructor Destroy; override;

    property StartX: integer read _startX;
    property StartY: integer read _startY;
    property StartR: integer read _startR;
    property Depth: integer read _depth;
    property Step: integer read _step;
  end;

implementation

{ TCircleData }

constructor TCircleData.Create(x, y, r, depth, step: integer);
begin
  _startX := x;
  _startY := y;
  _startR := r;
  _depth := depth;
  _step := step;
end;

destructor TCircleData.Destroy;
begin
  inherited Destroy;
end;

end.
