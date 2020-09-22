unit VisibleDSA.AlgoVisualizer;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Math,
  FMX.Graphics,
  FMX.Forms,
  VisibleDSA.AlgoVisHelper,
  VisibleDSA.FractalData;

type
  TAlgoVisualizer = class(TObject)
  private
    _data: TFractalData;
    _times: integer;
    _isForward: boolean;

    procedure __setData;
    procedure __desktopCenter(form: TForm);
    procedure __formShow(Sender: TObject);

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
  depth, w, h: integer;
begin
  _isForward := true;
  depth := 6;
  _times := 1;

  w := Trunc(Power(3, depth));
  h := Trunc(Power(3, depth));

  form.ClientWidth := w;
  form.ClientHeight := h;
  form.Caption := 'Fractal Visualizer --- ' + Format('W: %d, H: %d', [w, h]);
  form.OnShow := __formShow;
  __desktopCenter(form);

  _data := TFractalData.Create(depth);
end;

destructor TAlgoVisualizer.Destroy;
begin
  FreeAndNil(_data);
  inherited Destroy;
end;

procedure TAlgoVisualizer.Paint(canvas: TCanvas);
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
      _times := _times + 1;

      if _times >= 7 then
        _isForward := not _isForward;
    end
    else
    begin
      _times := _times - 1;

      if _times < 1 then
        _isForward := not _isForward;
    end;
  end;
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

procedure TAlgoVisualizer.__formShow(Sender: TObject);
var
  thread: TThread;
begin
  thread := TThread.CreateAnonymousThread(Run);
  thread.FreeOnTerminate := true;
  thread.Start;
end;

procedure TAlgoVisualizer.__setData;
begin
  TAlgoVisHelper.Pause(400);
  AlgoForm.PaintBox.Repaint;
end;

end.
