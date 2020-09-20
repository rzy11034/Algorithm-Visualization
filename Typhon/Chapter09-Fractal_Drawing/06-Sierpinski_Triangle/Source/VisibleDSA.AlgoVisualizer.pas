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
  depth := 9;
  _data := TFractalData.Create(0);
  _times := 1;
  w := 2 ** depth;
  h := 2 ** depth;

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

  procedure __drawFractal__(Ax, Ay, side, depth: integer);
  begin
    if side <= 1 then
    begin
      TAlgoVisHelper.SetFill(CL_INDIGO);
      TAlgoVisHelper.FillRectangle(canvas, Ax, Ay, 1, 1);
      Exit;
    end;

    if depth = _data.Depth then
    begin
      TAlgoVisHelper.SetFill(CL_INDIGO);
    end;
  end;

begin
  //__drawFractal__(0, canvas.Height, canvas.Width, 9);
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
