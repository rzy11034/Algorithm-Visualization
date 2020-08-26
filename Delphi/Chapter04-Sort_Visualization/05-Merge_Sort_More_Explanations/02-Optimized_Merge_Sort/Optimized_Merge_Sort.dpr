program Optimized_Merge_Sort;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  VisibleDSA.InsertionSort in 'Source\VisibleDSA.InsertionSort.pas',
  VisibleDSA.MergeSort in 'Source\VisibleDSA.MergeSort.pas',
  VisibleDSA.SortTestHelper in 'Source\VisibleDSA.SortTestHelper.pas';

procedure Run();
var
  n: integer;
  arr: TArray_int;
begin
  n := 1000000;
  arr := TSortTestHelper.GenerateRandomArray(n, 0, 100000);

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
