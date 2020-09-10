unit VisibleDSA.Board;

interface

uses
  System.Classes,
  System.SysUtils,
  DeepStar.Utils.UString;

type
  TArray_str = TArray<UString>;
  TArr2D_chr = TArray<TArray<UChar>>;

  TBoard = class(TObject)
  private
    _n: integer;
    _m: integer;

    _data: TArr2D_chr;

  public
    constructor Create(strs: TArray_str);
    destructor Destroy; override;
    function InArea(x, y: integer): boolean;
    procedure Print;

    property N: integer read _n;
    property M: integer read _m;
  end;

implementation

uses
  VisibleDSA.AlgoForm;

{ TBoard }

constructor TBoard.Create(strs: TArray_str);
var
  i: integer;
begin
  if strs = nil then
    raise Exception.Create('lines cannot be null in Board constructor.');

  _n := Length(strs);
  if _n = 0 then
    raise Exception.Create('lines cannot be empty in Board constructor.');

  _m := Length(strs[0]);
  SetLength(_data, _n, _m);

  for i := 0 to _n - 1 do
  begin
    if Length(_data[i]) <> _m then
      raise Exception.Create('All line''s length must be same in Board constructor.');

    _data[i] := strs[i].ToCharArray;
  end;
end;

destructor TBoard.Destroy;
begin
  inherited Destroy;
end;

function TBoard.InArea(x, y: integer): boolean;
begin
  Result := (x >= 0) and (x < N) and (y >= 0) and (y < M);
end;

procedure TBoard.Print;
var
  i, j: integer;
  s: UString;
begin
  for i := 0 to High(_data) do
  begin
    s := '';
    for j := 0 to High(_data[i]) do
      s :=s + _data[i, j];

    writeLn(s);
  end;
end;

end.
