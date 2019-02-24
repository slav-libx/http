program httpserver;

uses
  Vcl.Forms,
  Lib.HTTPServer.WebApi in 'Units\Lib.HTTPServer.WebApi.pas',
  Form.ServerMain in 'Forms\Form.ServerMain.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
