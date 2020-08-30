unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.SysUtils,
  System.Types,
  System.Math,
  FMX.Graphics,
  FMX.Forms,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.MazeData;

type
  TAlgoVisualizer = class(TObject)
  private
    _width: integer;
    _height: integer;
    _data: TMazeData;
    _form: TForm;

    procedure __setData;

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TCanvas);
    procedure Run;
  end;

implementation

uses
  VisibleDSA.AlgoForm;

type
  TArray_int = TArray<integer>;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  blockSide: integer;
begin
  blockSide := 8;
  _data := TMazeData.Create(TMazeData.FILE_NAME);

  _form := form;
  _form.ClientWidth := blockSide * _data.M;
  _form.ClientHeight := blockSide * _data.N;

  _width := _form.ClientWidth;
  _height := _form.ClientHeight;

  _form.Caption := 'Maze solver visualization' +
    Format('W: %d, H: %d', [_Width, _Height]);;
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TCanvas);
var
  w, h: Single;
  i, j: integer;
begin
  w := _width / _data.N;
  h := _height / _data.M;

  for i := 0 to _data.N - 1 do
  begin
    for j := 0 to _data.M - 1 do
    begin
      if _data.GetMaze(i, j) = TMazeData.WALL then
        TAlgoVisHelper.SetFill(CL_LIGHTBLUE)
      else
        TAlgoVisHelper.SetFill(CL_WHITE);

      TAlgoVisHelper.FillRectangle(canvas, j * w, i * h, w, h);
    end;
  end;

//  TAlgoVisHelper.SetFill(CL_LIGHTBLUE);
//  TAlgoVisHelper.FillRectangle(canvas, 0, 0, 50, 50) ;
//  //TAlgoVisHelper.SetFill(CL_RED);
//  TAlgoVisHelper.FillRectangle(canvas, 0, 50, 50, 50) ;
//  //TAlgoVisHelper.SetFill(CL_YELLOW);
//  TAlgoVisHelper.FillRectangle(canvas, 50, 0, 50, 50) ;
//  //TAlgoVisHelper.SetFill(CL_INDIGO);
//  TAlgoVisHelper.FillRectangle(canvas, 50, 50, 50, 50) ;
end;

procedure TAlgoVisualizer.Run;
begin

end;

procedure TAlgoVisualizer.__setData;
begin
  TAlgoVisHelper.Pause(10);
  AlgoForm.PaintBox.Repaint;
end;

end.
