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
  VisibleDSA.FractalData;

type
  TAlgoVisualizer = class(TObject)
  private
    _data: TFractalData;
    _times: integer;
    _isForward: boolean;

    procedure __setData;
    procedure __formShow(Sender: TObject);


  public
    constructor Create(form: TForm);
    destructor Destroy; override;

    procedure Paint(canvas: TBGRACanvas2D);
    procedure Run;

  end;

implementation

uses
  Math,
  VisibleDSA.AlgoForm;

var
  Static_Vis: TAlgoVisualizer;

{ TAlgoVisualizer }

constructor TAlgoVisualizer.Create(form: TForm);
var
  depth, w, h: integer;
begin
  Static_Vis := self;
  _isForward := true;
  depth := 6;
  _data := TFractalData.Create(0);
  _times := 1;
  w := 3 ** depth;
  h := 3 ** depth;

  form.ClientWidth := w;
  form.ClientHeight := h;
  form.Caption := 'Fractal Visualizer --- ' +
    Format('W: %d, H: %d', [form.ClientWidth, form.ClientHeight]);

  form.OnShow := @__formShow;
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TBGRACanvas2D);

  procedure __drawFractal__(x, y, w, h, depth: integer);
  var
    h_3, w_3, i, j: integer;
  begin
    w_3 := w div 3;
    h_3 := h div 3;

    if depth = _data.Depth then
    begin
      TAlgoVisHelper.SetFill(CL_INDIGO);
      TAlgoVisHelper.FillRectangle(canvas, x, y, w_3, h_3);
      Exit;
    end;

    if (w <= 1) or (h <= 1) then
      Exit;

    for i := 0 to 2 do
    begin
      for j := 0 to 2 do
      begin
        if (i = 1) and (j = 1) then
        begin
          TAlgoVisHelper.SetFill(CL_INDIGO);
          TAlgoVisHelper.FillRectangle(canvas, x + w_3, y + h_3, w_3, h_3);
        end
        else
        begin
          __drawFractal__(x + i * w_3, y + j * h_3, w_3, h_3, depth + 1);
        end;
      end;
    end;
  end;

begin
  __drawFractal__(0, 0, canvas.Width, canvas.Height, 0);
end;

procedure TAlgoVisualizer.Run;
begin
  while true do
  begin
    _data.Depth := _times;
    __setData;

    if _isForward then
    begin
      _times += 1;

      if _times >= 7 then
        _isForward := not _isForward;
    end
    else
    begin
      _times -= 1;

      if _times < 1 then
        _isForward := not _isForward;
    end;
  end;
end;

procedure TAlgoVisualizer.__formShow(Sender: TObject);
  procedure __threadExecute__;
  begin
    Static_Vis.Run;
  end;

var
  thread: TThread;
begin
  thread := TThread.CreateAnonymousThread(TProcedure(@__threadExecute__));
  thread.FreeOnTerminate := true;
  thread.Start;
end;

procedure TAlgoVisualizer.__setData;
begin
  TAlgoVisHelper.Pause(500);
  AlgoForm.BGRAVirtualScreen.DiscardBitmap;
end;

end.
