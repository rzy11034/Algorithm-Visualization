unit VisibleDSA.RandomQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type

  generic TRandomQueue<T> = class(TObject)
  private type
    TList_T = specialize TList<T>;

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

constructor TRandomQueue.Create;
begin
  _list := TList_T.Create;
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

  Randomize;
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

  Result := res;
end;

destructor TRandomQueue.Destroy;
begin
  FreeAndNil(_list);
  inherited Destroy;
end;

procedure TRandomQueue.Enqueue(e: T);
var
  index: integer;
  temp: T;
begin
  if _list.Count = 0 then
  begin
    _list.Add(e);
    Exit;
  end;

  Randomize;

  index := Random(_list.Count);

  temp := _list[index];
  _list[index] := e;
  _list.Add(temp);
end;

end.
