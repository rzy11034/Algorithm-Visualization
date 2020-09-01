unit VisibleDSA.RandomQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  VisibleDSA.Position,
  Generics.Collections;

type
  TList_double = specialize TList<double>;

  TRandomQueue = class(TObject)
  private type
    T = TPosition;
    TList_T = specialize TList<T>;
    TList_double = specialize TList<double>;

  private
    _list: TList_T;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Enqueue(e: T);
    function Dequeue: T;
    function Count: integer;
  end;

var
  list: TList_double;

implementation

{ TRandomQueue }

constructor TRandomQueue.Create;
begin
  _list := TList_T.Create;
  Randomize;
end;

function TRandomQueue.Count: integer;
begin
  Result := _list.Count;
end;

function TRandomQueue.Dequeue: T;
var
  res: T;
  r: double;
begin
  if _list.Count = 0 then
    raise Exception.Create('There''s no element to remove in Random Queue');

  //Randomize;
  r := Random;

  if r < 0.5 then
  begin
    res := _list.Last;
    _list.Delete(_list.Count - 1);
  end
  else
  begin
    res := _list.First;
    _list.Delete(0);
  end;

  list.Add(r);
  Result := res;
end;

destructor TRandomQueue.Destroy;
begin
  FreeAndNil(_list);
  inherited Destroy;
end;

procedure TRandomQueue.Enqueue(e: T);
begin
  //Randomize;

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
