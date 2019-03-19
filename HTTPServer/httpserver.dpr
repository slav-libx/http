program httpserver;

uses
  Vcl.Forms,
  Frame.Communication in '..\Frames\Frame.Communication.pas' {CommunicationFrame: TFrame},
  Lib.HTTPServer.WebApi in 'Units\Lib.HTTPServer.WebApi.pas',
  Form.ServerMain in 'Forms\Form.ServerMain.pas' {ServerMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TServerMainForm, ServerMainForm);
  Application.Run;
end.
