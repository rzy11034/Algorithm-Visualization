unit VisibleDSA.AlgoForm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  BGRABitmap,
  BGRAVirtualScreen,
  VisibleDSA.AlgoVisualizer;

type
  TAlgoForm = class(TForm)
    BGRAVirtualScreen: TBGRAVirtualScreen;
    procedure BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    _av: TAlgoVisualizer;
    _thread: TThread;

  public

  end;

var
  AlgoForm: TAlgoForm;

implementation

{$R *.frm}

{ TAlgoForm }

procedure TAlgoForm.BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  _av.Paint(Bitmap.Canvas2D);
end;

procedure TAlgoForm.FormActivate(Sender: TObject);

  procedure __run__;
  begin
    AlgoForm._av.Run;
  end;

begin
  _thread := TThread.CreateAnonymousThread(TProcedure(@__run__));
  _thread.FreeOnTerminate := true;
  _thread.Start;
end;

procedure TAlgoForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if _thread.Finished <> True then
  begin
    _thread.Suspended := True;
  end;
end;

procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  ClientWidth := 600;
  ClientHeight := 600;
  Position := TPosition.poDesktopCenter;
  BorderStyle := TFormBorderStyle.bsSingle;
  DoubleBuffered := true;
  Caption := 'AlgoForm';

  BGRAVirtualScreen.Color := clForm;

  _av := TAlgoVisualizer.Create(self, ClientWidth, ClientHeight, 100);
end;

end.
