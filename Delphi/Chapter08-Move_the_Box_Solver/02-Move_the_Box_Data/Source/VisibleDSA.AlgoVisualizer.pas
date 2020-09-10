unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Math,
  Winapi.Windows,
  FMX.Graphics,
  FMX.Forms,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.GameData;

type
  TAlgoVisualizer = class(TObject)
  public const
    D: TArr2D_int = [[-1, 0], [0, 1], [1, 0], [0, -1]];

  private
    _width: integer;
    _height: integer;
    _data: TGameData;

    procedure __setData;
    procedure __desktopCenter(form: TForm);

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TCanvas);
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
  blockSide := 80;

  _data := TGameData.Create;

  _width := blockSide * _data.M;
  _height := blockSide * _data.N;

  form.ClientWidth := _width;
  form.ClientHeight := _height;

  form.Caption := 'Move the Box Solver --- ' + Format('W: %d, H: %d', [_Width, _Height]);
  __desktopCenter(form);
  AllocConsole;
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  FreeConsole;
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TCanvas);
//var
//  w, h: integer;
//  i, j: integer;
//  str: UString;
//  stm: TResourceStream;
begin
//  w := _width div _data.M;
//  h := _height div _data.N;
//
//  for i := 0 to _data.N - 1 do
//  begin
//    for j := 0 to _data.M - 1 do
//    begin
//      if _data.Mines[i, j] then
//        str := TGameData.PNG_MINE
//      else
//        str := TGameData.PNG_BLOCK;
//
//      stm := TResourceStream.Create(HINSTANCE, str, RT_RCDATA);
//      TAlgoVisHelper.DrawImageFormResourceStream(canvas, stm, j * w, i * h, w, h);
//    end;
//  end;
end;

procedure TAlgoVisualizer.Run;
begin
  _data.PrintStarterBoard;
end;

procedure TAlgoVisualizer.__setData;
begin
//  if finished or (_runningStatus >= 5) then
//  begin
//    TAlgoVisHelper.Pause(0);
//    AlgoForm.PaintBox.Repaint;
//    _runningStatus := 0;
//  end
//  else
//  begin
//    _runningStatus := _runningStatus + 1;
//  end;
end;

procedure TAlgoVisualizer.__desktopCenter(form: TForm);
var
  top, left: Double;
begin
  top := (Screen.Height div 2) - (form.ClientHeight div 2);
  left := (Screen.Width div 2) - (form.ClientWidth div 2);

  form.top := Trunc(top);
  form.left := Trunc(left);
end;

end.
