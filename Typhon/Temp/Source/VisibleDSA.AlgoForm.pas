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
  VisibleDSA.AlgoVisualizer;

type
  TAlgoForm = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
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

procedure TAlgoForm.FormActivate(Sender: TObject);
  procedure __run__;
  begin
    AlgoForm._av.Run;
  end;

begin
  //_thread := TThread.CreateAnonymousThread(TProcedure(@__run__));
  //_thread.FreeOnTerminate := true;
  //_thread.Start;

  _av.Run;
end;

procedure TAlgoForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //if _thread.Finished <> true then
  //begin
  //  _thread.Suspended := true;
  //end;
end;

procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  _av := TAlgoVisualizer.Create(self);
end;

procedure TAlgoForm.FormPaint(Sender: TObject);
begin
  _av.Paint(Self.Canvas);

end;

end.
