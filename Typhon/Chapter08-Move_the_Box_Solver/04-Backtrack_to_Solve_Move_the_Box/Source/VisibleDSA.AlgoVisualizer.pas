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
    _colorlist: TList_TColor;
    _colorMap: THashMap_UString_TColor;

    procedure __initColorList;
    procedure __setData;

  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
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
  FreeAndNil(_colorlist);
  FreeAndNil(_colorMap);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TBGRACanvas2D);
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
  __setData;
  if _data.Solve then
    WriteLn('The game has a solution!')
  else
    WriteLN('The game does NOT have a solution.');
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

procedure TAlgoVisualizer.__setData;
begin
  _data.Print;
end;

end.
