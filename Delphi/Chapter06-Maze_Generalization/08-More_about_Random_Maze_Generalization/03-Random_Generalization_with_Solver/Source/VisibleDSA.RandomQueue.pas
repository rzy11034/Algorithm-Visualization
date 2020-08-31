unit VisibleDSA.RandomQueue;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type

  TRandomQueue<T> = class(TObject)
  private type
    TList_T = TList<T>;

  private
    _list: TList_T;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Enqueue(e: T);
    function Dequeue: T;
    function Count: integer;
  end;

implementation

{ TRandomQueue }

constructor TRandomQueue<T>.Create;
begin
  _list := TList_T.Create;
end;

function TRandomQueue<T>.Count: integer;
begin
  Result := _list.Count;
end;

function TRandomQueue<T>.Dequeue: T;
var
  res: T;
begin
  if _list.Count = 0 then
    raise Exception.Create('There''s no element to remove in Random Queue');

  Randomize;

  if Random < 0.5 then
  begin
    res := _list.Last;
    _list.Delete(_list.Count - 1);
  end
  else
  begin
    res := _list.First;
    _list.Delete(0);
  end;

  Result := res;
end;

destructor TRandomQueue<T>.Destroy;
begin
  FreeAndNil(_list);
  inherited Destroy;
end;

procedure TRandomQueue<T>.Enqueue(e: T);
begin
  Randomize;

  if Random > 0.5 then
  begin
    _list.Add(e);
  end
  else
  begin
    _list.Insert(0, e);
  end;
end;

end.
