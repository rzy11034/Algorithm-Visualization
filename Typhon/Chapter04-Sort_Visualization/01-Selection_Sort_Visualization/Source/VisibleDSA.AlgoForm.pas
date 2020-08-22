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
  TMyThread = class(TThread)
    procedure Execute; override;
    procedure Run;
  end;

  TAlgoForm = class(TForm)
    BGRAVirtualScreen: TBGRAVirtualScreen;
    procedure BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

  private
    _av: TAlgoVisualizer;
    _stop: boolean;
    _thread: TThread;

  public
    property Stop: boolean read _stop;

  end;

var
  AlgoForm: TAlgoForm;

implementation

{$R *.frm}

procedure Run;
begin
  AlgoForm._av.Run;
end;

{ TMyThread }

procedure TMyThread.Execute;
begin
  if not Self.Suspended then
    Synchronize(@Self.Run);
end;

procedure TMyThread.Run;
begin
  if not Self.Terminated then
    AlgoForm._av.Run;
end;

{ TAlgoForm }

procedure TAlgoForm.BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  _av.Paint(Bitmap.Canvas2D);
end;

procedure TAlgoForm.FormActivate(Sender: TObject);
begin
  _thread := TThread.CreateAnonymousThread(@Run);
  _thread.FreeOnTerminate := true;
  _thread.Resume;

  //_av.Run;
end;

procedure TAlgoForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  _av.Suspended := true;
  _av.Terminate;
end;

procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  ClientWidth := 600;
  ClientHeight := 600;
  Position := TPosition.poDesktopCenter;
  BorderStyle := TFormBorderStyle.bsSingle;
  DoubleBuffered := true;
  Caption := 'AlgoForm';
  _stop := false;

  BGRAVirtualScreen.Color := clForm;

  _av := TAlgoVisualizer.Create(self, ClientWidth, ClientHeight, 100);
end;

end.
