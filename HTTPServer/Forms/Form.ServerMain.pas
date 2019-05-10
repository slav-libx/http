unit Form.ServerMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.StrUtils,
  System.JSON,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Lib.HTTPTypes,
  Lib.HTTPConsts,
  Lib.HTTPContent,
  Lib.HTTPServer,
  Lib.HTTPServer.StaticFiles,
  Lib.HTTPServer.WebApi,
  Lib.JSONFormat,
  Lib.JSONStore,
  Frame.Communication;

type
  TServerMainForm = class(TForm,IHTTPMonitor)
    StartServerButton: TButton;
    PortEdit: TEdit;
    CloseConnectionsButton: TButton;
    HostLabel: TLabel;
    HostEdit: TEdit;
    HomeLabel: TLabel;
    HomeEdit: TEdit;
    AliasesLabel: TLabel;
    AliasesMemo: TMemo;
    KeepAliveTimeoutEdit: TEdit;
    ResultLabel: TLabel;
    KeepAliveCheckBox: TCheckBox;
    CommunicationFrame: TCommunicationFrame;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StartServerButtonClick(Sender: TObject);
    procedure CloseConnectionsButtonClick(Sender: TObject);
  private
    FStore: TJSONStore;
    FCount: Integer;
    FServer: THTTPServer;
    FConnections: TList;
    FWebApi: IMiddleware;
    procedure DoTrace(const Text: string); // IHTTPMonitor.DoTrace()
    procedure OnAcceptClient(Sender: TObject);
    procedure OnDestroyClient(Sender: TObject);
    procedure OnRequest(Sender: TObject);
    procedure OnResponse(Sender: TObject);
    procedure SetServerControls;
    procedure SetMiddlewares(Client: THTTPServerClient);
    procedure StartStopServer;
    procedure CloseClients;
  public
  end;

var
  ServerMainForm: TServerMainForm;

implementation

{$R *.dfm}

procedure TServerMainForm.FormCreate(Sender: TObject);
begin

  FServer:=nil;
  FConnections:=TList.Create;

  Caption:='HTTP server ['+GetEnvironmentVariable('ComputerName')+']';

  AliasesMemo.Clear;

  FStore:=TJSONStore.Create(ExtractFilePath(ParamStr(0))+'serv-store.json');

  BoundsRect:=FStore.ReadRect('form.bounds',BoundsRect);
  HostEdit.Text:=FStore.ReadString('host');
  PortEdit.Text:=FStore.ReadInteger('port',80).ToString;
  HomeEdit.Text:=FStore.ReadString('home',ExtractFilePath(ParamStr(0))+'Home');
  KeepAliveTimeoutEdit.Text:=FStore.ReadInteger('keep-alive.timeout',10).ToString;
  KeepAliveCheckBox.Checked:=FStore.ReadBool('keep-alive.enabled',False);
  FStore.ReadStrings('aliases',AliasesMemo.Lines);

  CommunicationFrame.Reset;
  CommunicationFrame.AutoShowResponseContent:=False;

  SetServerControls;

  StartStopServer;

end;

procedure TServerMainForm.FormDestroy(Sender: TObject);
begin

  if WindowState=TWindowState.wsNormal then
    FStore.WriteRect('form.bounds',BoundsRect);
  FStore.WriteString('host',HostEdit.Text);
  FStore.WriteInteger('port',StrToIntDef(PortEdit.Text,80));
  FStore.WriteString('home',HomeEdit.Text);
  FStore.WriteInteger('keep-alive.timeout',StrToIntDef(KeepAliveTimeoutEdit.Text,10));
  FStore.WriteBool('keep-alive.enabled',KeepAliveCheckBox.Checked);
  FStore.WriteStrings('aliases',AliasesMemo.Lines);

  FStore.Free;
  FServer.Free;

  CloseClients;

  FConnections.Free;

end;

procedure TServerMainForm.StartStopServer;
begin

  if Assigned(FServer) then
  begin

    FreeAndNil(FServer);
    DoTrace('Server stoped');
    SetServerControls;

  end else begin

    if not Assigned(FWebApi) then
    begin
      FWebApi:=TWebApi.Create;
    end;

    FServer:=THTTPServer.Create;
    try
      FServer.OnAccept:=OnAcceptClient;
      FServer.Start(HostEdit.Text,StrToInt(PortEdit.Text));
      DoTrace('Server started');
      SetServerControls;
    except
      FreeAndNil(FServer);
      raise;
    end;

  end;

end;

procedure TServerMainForm.CloseClients;
begin
  while FConnections.Count>0 do TObject(FConnections.Last).Free;
end;

procedure TServerMainForm.SetServerControls;
var
  ServerStarted: Boolean;
  ClientsCount: Integer;
begin

  ClientsCount:=FConnections.Count;

  ServerStarted:=Assigned(FServer);

  StartServerButton.Caption:=IfThen(ServerStarted,'Stop','Start');
  CloseConnectionsButton.Enabled:=ClientsCount>0;
  HostEdit.Enabled:=not ServerStarted;
  PortEdit.Enabled:=not ServerStarted;
  ResultLabel.Caption:=' '+ClientsCount.ToString+' ';

end;

procedure TServerMainForm.DoTrace(const Text: string);
begin
  CommunicationFrame.ToLog(Text+CRLF);
end;

procedure TServerMainForm.OnRequest(Sender: TObject);
begin
  CommunicationFrame.SetRequest(THTTPServerClient(Sender).Request);
end;

procedure TServerMainForm.OnResponse(Sender: TObject);
begin
  CommunicationFrame.SetResponse(THTTPServerClient(Sender).Response);
end;

procedure TServerMainForm.SetMiddlewares(Client: THTTPServerClient);
begin
  Client.Use(FWebApi);
  Client.Use(TStaticFiles.Create(HomeEdit.Text,AliasesMemo.Lines));
end;

procedure TServerMainForm.OnAcceptClient(Sender: TObject);
var C: THTTPServerClient;
begin

  C:=THTTPServerClient.Create;

  C.Monitor:=Self;
  C.KeepAliveTimeout:=StrToIntDef(KeepAliveTimeoutEdit.Text,10);
  C.KeepAlive:=KeepAliveCheckBox.Checked;
  C.OnRequest:=OnRequest;
  C.OnResponse:=OnResponse;
  C.OnDestroy:=OnDestroyClient;

  SetMiddlewares(C);

  C.AcceptOn(FServer.AcceptClient);

  CommunicationFrame.ToLog('Remote Host: '+FServer.AcceptRemoteHost+':'+
    FServer.AcceptRemotePort.ToString+CRLF);

  FConnections.Add(C);
  SetServerControls;

end;

procedure TServerMainForm.OnDestroyClient(Sender: TObject);
begin
  FConnections.Remove(Sender);
  SetServerControls;
end;

procedure TServerMainForm.StartServerButtonClick(Sender: TObject);
begin
  StartStopServer;
end;

procedure TServerMainForm.CloseConnectionsButtonClick(Sender: TObject);
begin
  CloseClients;
end;

end.
