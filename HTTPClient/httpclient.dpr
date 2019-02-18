program httpclient;

uses
  Vcl.Forms,
  Form.ClientMain in 'Forms\Form.ClientMain.pas' {Form2},
  Lib.JSON.Store in '..\lib\Lib.JSON.Store.pas',
  Form.Request in 'Forms\Form.Request.pas' {RequestForm},
  Lib.JSON.Format in '..\lib\Lib.JSON.Format.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
