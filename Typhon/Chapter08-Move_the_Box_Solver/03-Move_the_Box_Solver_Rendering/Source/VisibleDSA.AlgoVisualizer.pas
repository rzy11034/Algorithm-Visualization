unit VisibleDSA.AlgoVisualizer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  Forms,
  LCLType,
  Generics.Collections,
  BGRACanvas2D,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.GameData,
  DeepStar.Utils.UString;

type
  TAlgoVisualizer = class(TObject)
  private type
    THashMap_UString_TColor = specialize THashMap<UChar, TColor>;
    TList_TColor = specialize TList<TColor>;

  private
    _width: integer;
    _height: integer;
    _data: TGameData;
    _Colorlist: TList_TColor;
    _colorMap: THashMap_UString_TColor;

    procedure __initColorList;
    procedure __setData(finished: boolean);

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
  blockSide := 80;
  __initColorList;
  _colorMap := THashMap_UString_TColor.Create;

  _data := TGameData.Create;

  _width := blockSide * _data.M;
  _height := blockSide * _data.N;

  form.ClientWidth := _width;
  form.ClientHeight := _height;

  form.Caption := 'Move the Box Solver --- ' + Format('W: %d, H: %d', [_width, _height]);

end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  FreeAndNil(_Colorlist);
  FreeAndNil(_colorMap);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TBGRACanvas2D);
var
  w, h: integer;
  i, j: integer;
begin
  w := _width div _data.M;
  h := _height div _data.N;

  //for i := 0 to _data.N - 1 do
  //begin
  //  for j := 0 to _data.M - 1 do
  //  begin
  //    if _data.Mines[i, j] then
  //      str := TMineSweeperData.PNG_MINE
  //    else
  //      str := TMineSweeperData.PNG_BLOCK;
  //
  //    stm := TResourceStream.Create(HINSTANCE, string(str), RT_RCDATA);
  //    TAlgoVisHelper.DrawImageFormResourceStream(canvas, stm, j*w, i*h, w, h);
  //  end;
  //end;
end;

procedure TAlgoVisualizer.Run;
begin
  _data.PrintStarterBoard;
end;

procedure TAlgoVisualizer.__initColorList;
begin
  _Colorlist := TList_TColor.Create;

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

procedure TAlgoVisualizer.__setData(finished: boolean);
begin

end;

end.
