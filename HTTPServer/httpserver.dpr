program httpserver;

uses
  Vcl.Forms,
  Form.ServerMain in 'Forms\Form.ServerMain.pas' {Form3},
  Lib.HTTPConsts in '..\lib\Lib.HTTPConsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
