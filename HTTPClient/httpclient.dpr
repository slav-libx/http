program httpclient;

uses
  Vcl.Forms,
  Lib.HeaderValues in 'Units\Lib.HeaderValues.pas',
  Frame.Communication in '..\Frames\Frame.Communication.pas' {CommunicationFrame: TFrame},
  Form.ClientMain in 'Forms\Form.ClientMain.pas' {Form2},
  Form.Request in 'Forms\Form.Request.pas' {RequestForm},
  Lib.HTTPHeaders in '..\lib\Lib.HTTPHeaders.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
