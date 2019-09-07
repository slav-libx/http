unit Form.DosMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
   Lib.TCPSocket;

type
  TForm12 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FClient: TTCPClient;
  public
    { Public declarations }
  end;

var
  Form12: TForm12;

implementation

{$R *.dfm}

procedure TForm12.Button1Click(Sender: TObject);
var I: Integer;
begin
  for I:=0 to 1000 do
  begin
    FClient.ConnectTo('localhost',5555);
    FClient.WriteString('1111');
  end;
end;

procedure TForm12.FormCreate(Sender: TObject);
begin
  FClient:=TTCPClient.Create;
end;

procedure TForm12.FormDestroy(Sender: TObject);
begin
  FClient.Free;
end;

end.
