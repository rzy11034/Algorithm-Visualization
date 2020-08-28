unit VisibleDSA.AlgoForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  VisibleDSA.AlgoVisualizer;

type
  TAlgoForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    _av: TAlgoVisualizer;
    _thread: TThread;
  public
    { Public declarations }
  end;

var
  AlgoForm: TAlgoForm;

implementation

{$R *.dfm}


procedure TAlgoForm.FormActivate(Sender: TObject);
begin
  _thread := TThread.CreateAnonymousThread(_av.Run);
  _thread.FreeOnTerminate := true;
  _thread.Start;

//  _av.Run;
end;

procedure TAlgoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if _thread.Finished <> true then
  begin
    _thread.Suspended := true;
  end;
end;

procedure TAlgoForm.FormCreate(Sender: TObject);
begin
  _av := TAlgoVisualizer.Create(self);
  DoubleBuffered := True;
end;

procedure TAlgoForm.FormPaint(Sender: TObject);
begin
  _av.Paint(Self.Canvas);
end;

end.
