unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Math,
  System.Generics.Collections,
  Winapi.Windows,
  FMX.Graphics,
  FMX.Forms,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.GameData,
  DeepStar.Utils.UString;

type
  TAlgoVisualizer = class(TObject)
  private type
    THashMap_UString_TColor = TDictionary<UChar, TColor>;
    TList_TColor = TList<TColor>;

  private
    _width: integer;
    _height: integer;
    _data: TGameData;
    _colorlist: TList_TColor;
    _colorMap: THashMap_UString_TColor;

    procedure __setData;
    procedure __desktopCenter(form: TForm);
    procedure __initColorList;

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TCanvas);
    procedure Run;
  end;

implementation

uses
  VisibleDSA.AlgoForm,
  VisibleDSA.Board;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  blockSide: integer;
begin
  blockSide := 80;

  _data := TGameData.Create;
  __initColorList;
  _colorMap := THashMap_UString_TColor.Create;

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
  FreeAndNil(_colorlist);
  FreeAndNil(_colorMap);

  //FreeConsole;
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TCanvas);
var
  w, h: integer;
  i, j: integer;
  showBoard: TBoard;
  c: UChar;
  color: TColor;
  sz: longint;
begin
  w := _width div _data.M;
  h := _height div _data.N;

  showBoard := _data.ShowBoard;

  for i := 0 to showBoard.N - 1 do
  begin
    for j := 0 to showBoard.M - 1 do
    begin
      c := showBoard[i, j];

      if c <> TBoard.EMPTY then
      begin
        if not _colorMap.ContainsKey(c) then
        begin
          sz := _colorMap.Count;
          _colorMap.Add(c, _colorlist[sz]);
        end;

        color := _colorMap[c];
        TAlgoVisHelper.SetFill(color);
        TAlgoVisHelper.FillRectangle(canvas, j * w, i * h, w - 2, h - 2);
      end;
    end;
  end;
end;

procedure TAlgoVisualizer.Run;
begin
  _data.Print;
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

procedure TAlgoVisualizer.__initColorList;
begin
  _colorlist := TList_TColor.Create;

  _colorList.Add(CL_RED);
  _colorList.Add(CL_PURPLE);
  _colorList.Add(CL_BLUE);
  _colorList.Add(CL_TEAL);
  _colorList.Add(CL_LIGHTGREEN);
  _colorList.Add(CL_LIME);
  _colorList.Add(CL_AMBER);
  _colorList.Add(CL_DEEPORANGE);
  _colorList.Add(CL_BROWN);
  _colorList.Add(CL_BLUEGREY);
end;

end.
