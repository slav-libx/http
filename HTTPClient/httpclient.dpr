program httpclient;

uses
  Vcl.Forms,
  Lib.HeaderValues in 'Units\Lib.HeaderValues.pas',
  Frame.Communication in '..\Frames\Frame.Communication.pas',
  Form.ClientMain in 'Forms\Form.ClientMain.pas',
  Form.Request in 'Forms\Form.Request.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
