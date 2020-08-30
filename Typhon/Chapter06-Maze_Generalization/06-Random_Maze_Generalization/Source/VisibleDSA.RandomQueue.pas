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
  randIndex: integer;
  e: T;
begin
  if _list.Count = 0 then
    raise Exception.Create('There''s no element to remove in Random Queue');

  Randomize;
  randIndex := Random(_list.Count);

  e := _list[randIndex];
  _list[randIndex] := _list.Last;
  _list.Delete(_list.Count - 1);

  Result := e;
end;

destructor TRandomQueue.Destroy;
begin
  FreeAndNil(_list);
  inherited Destroy;
end;

procedure TRandomQueue.Enqueue(e: T);
begin
  _list.Add(e);
end;

end.
