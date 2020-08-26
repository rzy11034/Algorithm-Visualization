program Merge_Sort;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  VisibleDSA.MergeSort in 'Source\VisibleDSA.MergeSort.pas',
  VisibleDSA.SortTestHelper in 'Source\VisibleDSA.SortTestHelper.pas';

procedure Run();
var
  arr: TArray_int;
  i: integer;
begin
  SetLength(arr, 8);

  for i := 0 to 7 do
  begin
    arr[i] := 8 - i;
  end;

  TMergeSort.Sort(arr);
  TSortTestHelper.PrintArray(arr);
end;

begin
  try
    Run;
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
