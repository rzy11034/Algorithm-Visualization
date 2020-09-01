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
  StdCtrls,
  BGRABitmap,
  BGRAVirtualScreen,
  VisibleDSA.AlgoVisualizer;

type
  TAlgoForm = class(TForm)
    BGRAVirtualScreen: TBGRAVirtualScreen;
    Button1: TButton;
    procedure BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure Button1Click(Sender: TObject);
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

uses
  VisibleDSA.InspectForm,
  VisibleDSA.RandomQueue;

{$R *.frm}

{ TAlgoForm }

procedure TAlgoForm.BGRAVirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  _av.Paint(Bitmap.Canvas2D);
end;

procedure TAlgoForm.Button1Click(Sender: TObject);
var
  f: TInspectForm;
  i: Integer;
begin
  f := TInspectForm.Create(self);
  f.Memo1.Clear;

  for i := 0 to list.Count - 1 do
  begin
    f.Memo1.Lines.Add(FloatToStr(list.Items[i]));
  end;

  f.ShowModal;
end;

procedure TAlgoForm.FormActivate(Sender: TObject);

  procedure __run__;
  begin
    AlgoForm._av.Run;
  end;

begin
  //_thread := TThread.CreateAnonymousThread(TProcedure(@__run__));
  //_thread.FreeOnTerminate := True;
  //_thread.Start;

  __run__;
  //Application.ProcessMessages;
end;

procedure TAlgoForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //if _thread.Finished <> True then
  //begin
  //  _thread.Suspended := True;
  //end;
end;

procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  ClientWidth := 600;
  ClientHeight := 600;
  Position := TPosition.poDesktopCenter;
  BorderStyle := TFormBorderStyle.bsSingle;
  //DoubleBuffered := True;
  Caption := 'AlgoForm';

  BGRAVirtualScreen.Color := clForm;

  _av := TAlgoVisualizer.Create(self);
  list := TList_double.Create;
end;

end.
