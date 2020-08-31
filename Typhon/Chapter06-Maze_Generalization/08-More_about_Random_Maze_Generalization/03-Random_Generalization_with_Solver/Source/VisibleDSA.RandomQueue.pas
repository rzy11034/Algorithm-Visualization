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
  function __solution1__: T;
  var
    res: T;
  begin
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

begin
  if _list.Count = 0 then
    raise Exception.Create('There''s no element to remove in Random Queue');

  Result := __solution1__;
end;

destructor TRandomQueue.Destroy;
begin
  FreeAndNil(_list);
  inherited Destroy;
end;

procedure TRandomQueue.Enqueue(e: T);
  procedure __solution1__(e: T);
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

  procedure __solution2__(e: T);
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

begin
  //__solution1__(e);
  __solution2__(e);
end;

end.
