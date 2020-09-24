unit VisibleDSA.FractalData;

interface

uses
  System.SysUtils;

type
  TFractalData = class(TObject)
  private
    _depth: integer;
    _splitAngle: Double;

  public
    constructor Create(depth: integer; SplitAngle:Double);
    destructor Destroy; override;

    property Depth: integer read _depth write _depth;
    property SplitAngle:Double read _splitAngle write _splitAngle;
  end;

implementation

{ TFractalData }

constructor TFractalData.Create(depth: integer; SplitAngle: Double);
begin
  _depth := depth;
  _splitAngle := SplitAngle;
end;

destructor TFractalData.Destroy;
begin
  inherited Destroy;
end;

end.
