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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
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
  _thread.FreeOnTerminate := True;
  _thread.Start;

  //_av.Run;
end;

procedure TAlgoForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  _thread.Suspended := True;
  _thread.Terminate;
end;

procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  ClientWidth := 800;
  ClientHeight := 600;
  Position := TPosition.poDesktopCenter;
  BorderStyle := TFormBorderStyle.bsSingle;
  DoubleBuffered := True;
  Caption := 'AlgoForm';

  BGRAVirtualScreen.Color := clForm;

  _av := TAlgoVisualizer.Create(self, ClientWidth, ClientHeight, 100);
end;

end.
