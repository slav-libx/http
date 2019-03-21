program httpclient;

uses
  Vcl.Forms,
  Frame.Communication in '..\Frames\Frame.Communication.pas' {CommunicationFrame: TFrame},
  Lib.HeaderValues in 'Units\Lib.HeaderValues.pas',
  Form.ClientMain in 'Forms\Form.ClientMain.pas' {ClientMainForm},
  Form.Request in 'Forms\Form.Request.pas' {RequestForm},
  Lib.HTTPTypes in '..\lib\Lib.HTTPTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TClientMainForm, ClientMainForm);
  Application.Run;
end.
