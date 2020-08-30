unit VisibleDSA.AlgoVisualizer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  Forms,
  BGRACanvas2D,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.MazeData;

type
  TAlgoVisualizer = class(TObject)
  private
    _width: integer;
    _height: integer;
    _data: TMazeData;
    _form: TForm;

    procedure __setData();

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
    procedure Run;
  end;

implementation

uses
  VisibleDSA.AlgoForm;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  blockSide: integer;
begin
  blockSide := 8;
  _data := TMazeData.Create(TMazeData.FILE_NAME);

  _form := form;
  _form.Width := blockSide * _data.M;
  _form.Height := blockSide * _data.N;

  _width := _form.Width;
  _height := _form.Height;
  _form.Caption := 'Maze solver visualization --- ' +
    Format('W: %d, H: %d', [_width, _height]);
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TBGRACanvas2D);
var
  w, h: integer;
  i, j: integer;
begin
  w := _width div _data.N;
  h := _height div _data.M;

  for i := 0 to _data.N - 1 do
  begin
    for j := 0 to _data.M - 1 do
    begin
      if _data.GetMaze(i, j) = TMazeData.WALL then
        TAlgoVisHelper.SetFill(CL_LIGHTBLUE)
      else
        TAlgoVisHelper.SetFill(CL_WHITE);

      TAlgoVisHelper.FillRectangle(canvas, j * w, i * h, h, w);
    end;
  end;
end;

procedure TAlgoVisualizer.Run;
begin

end;

procedure TAlgoVisualizer.__setData();
begin

  TAlgoVisHelper.Pause(0);
  AlgoForm.BGRAVirtualScreen.RedrawBitmap;
end;

end.
