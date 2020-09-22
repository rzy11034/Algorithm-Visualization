unit VisibleDSA.FractalData;

interface

uses
  System.SysUtils;

type
  TFractalData = class(TObject)
  private
    _depth: integer;

  public
    constructor Create(depth: integer);
    destructor Destroy; override;

    property Depth: integer read _depth write _depth;
  end;

implementation

{ TFractalData }

constructor TFractalData.Create(depth: integer);
begin
  _depth := depth;
end;

destructor TFractalData.Destroy;
begin
  inherited Destroy;
end;

end.
