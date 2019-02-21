program httpclient;

uses
  Vcl.Forms,
  Lib.JSON.Store in '..\lib\Lib.JSON.Store.pas',
  Lib.JSON.Format in '..\lib\Lib.JSON.Format.pas',
  Lib.HeaderValues in 'Units\Lib.HeaderValues.pas',
  Form.ClientMain in 'Forms\Form.ClientMain.pas' {Form2},
  Form.Request in 'Forms\Form.Request.pas' {RequestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
