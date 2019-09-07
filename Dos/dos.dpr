program dos;

uses
  Vcl.Forms,
  Form.DosMain in 'Form.DosMain.pas' {Form12},
  Lib.CleanSocket in '..\lib\Lib.CleanSocket.pas',
  Lib.SSL.Api in '..\lib\Lib.SSL.Api.pas',
  Lib.SSL in '..\lib\Lib.SSL.pas',
  Lib.TCPSocket in '..\lib\Lib.TCPSocket.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
