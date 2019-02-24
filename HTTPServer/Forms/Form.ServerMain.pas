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
  Lib.HTTPConsts,
  Lib.HTTPContent,
  Lib.HTTPServer,
  Lib.HTTPServer.StaticFiles,
  Lib.HTTPServer.WebApi,
  Lib.JSON.Format,
  Lib.JSON.Store;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Edit2: TEdit;
    RequestsMemo: TMemo;
    Button2: TButton;
    Label3: TLabel;
    Edit1: TEdit;
    Label4: TLabel;
    Edit3: TEdit;
    Label5: TLabel;
    Memo2: TMemo;
    Edit4: TEdit;
    ContentMemo: TMemo;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Button4: TButton;
    Label2: TLabel;
    SpeedButton3: TSpeedButton;
    ResponseMemo: TMemo;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    FStore: TJSONStore;
    FCount: Integer;
    FServer: THTTPServer;
    FConnections: TList;
    FWebApi: IMiddleware;
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
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin

  FServer:=nil;
  FConnections:=TList.Create;

  Caption:='HTTP server ['+GetEnvironmentVariable('ComputerName')+']';

  Memo2.Clear;

  FStore:=TJSONStore.Create(ExtractFilePath(ParamStr(0))+'serv-store.json');

  BoundsRect:=FStore.ReadRect('form.bounds',BoundsRect);
  Edit1.Text:=FStore.ReadString('host');
  Edit2.Text:=FStore.ReadInteger('port',80).ToString;
  Edit3.Text:=FStore.ReadString('home',ExtractFilePath(ParamStr(0))+'Home');
  Edit4.Text:=FStore.ReadInteger('keep-alive.timeout',10).ToString;
  CheckBox1.Checked:=FStore.ReadBool('keep-alive.enabled',False);
  FStore.ReadStrings('aliases',Memo2.Lines);

  SpeedButton2.Down:=True;
  SpeedButton2.OnClick(nil);

  SetServerControls;

  Button1.Click;

end;

procedure TForm3.FormDestroy(Sender: TObject);
begin

  if WindowState=TWindowState.wsNormal then
    FStore.WriteRect('form.bounds',BoundsRect);
  FStore.WriteString('host',Edit1.Text);
  FStore.WriteInteger('port',StrToIntDef(Edit2.Text,80));
  FStore.WriteString('home',Edit3.Text);
  FStore.WriteInteger('keep-alive.timeout',StrToIntDef(Edit4.Text,10));
  FStore.WriteBool('keep-alive.enabled',CheckBox1.Checked);
  FStore.WriteStrings('aliases',Memo2.Lines);

  FStore.Free;
  FServer.Free;

  CloseClients;

  FConnections.Free;

end;

procedure TForm3.StartStopServer;
begin

  if Assigned(FServer) then
  begin

    FreeAndNil(FServer);
    RequestsMemo.Lines.Add('Server stoped'#13#10);
    SetServerControls;

  end else begin

    if not Assigned(FWebApi) then
    begin
      FWebApi:=TWebApi.Create;
    end;

    FServer:=THTTPServer.Create;
    try
      FServer.OnAccept:=OnAcceptClient;
      FServer.Start(Edit1.Text,StrToInt(Edit2.Text));
      RequestsMemo.Lines.Add('Server started'#13#10);
      SetServerControls;
    except
      FreeAndNil(FServer);
      raise;
    end;

  end;

end;

procedure TForm3.CloseClients;
begin
  while FConnections.Count>0 do TObject(FConnections.Last).Free;
end;

procedure TForm3.SetServerControls;
var
  ServerStarted: Boolean;
  ClientsCount: Integer;
begin

  ClientsCount:=FConnections.Count;

  ServerStarted:=Assigned(FServer);

  Button1.Caption:=IfThen(ServerStarted,'Stop','Start');
  Button2.Enabled:=ClientsCount>0;
  Edit1.Enabled:=not ServerStarted;
  Edit2.Enabled:=not ServerStarted;
  Label2.Caption:=' '+ClientsCount.ToString+' ';

end;

procedure TForm3.OnRequest(Sender: TObject);
var C: THTTPServerClient;
begin
  C:=THTTPServerClient(Sender);
  RequestsMemo.Lines.Add(C.Request.Method+' '+C.Request.Resource);
  RequestsMemo.Lines.AddStrings(C.Request.Headers);
  RequestsMemo.Lines.Add('');
  C.Request.ShowTextContentTo(ContentMemo.Lines);
end;

procedure TForm3.OnResponse(Sender: TObject);
var C: THTTPServerClient;
begin
  C:=THTTPServerClient(Sender);
  RequestsMemo.Lines.Add(C.Response.ResultCode.ToString+' '+C.Response.ResultText);
  RequestsMemo.Lines.AddStrings(C.Response.Headers);
  RequestsMemo.Lines.Add('');
  C.Response.ShowTextContentTo(ResponseMemo.Lines);
end;

procedure TForm3.SetMiddlewares(Client: THTTPServerClient);
begin
  Client.Use(FWebApi);
  Client.Use(TStaticFiles.Create(Edit3.Text,Memo2.Lines));
end;

procedure TForm3.OnAcceptClient(Sender: TObject);
var C: THTTPServerClient;
begin

  C:=THTTPServerClient.CreateOn(FServer.AcceptClient);

  C.KeepAliveTimeout:=StrToIntDef(Edit4.Text,10);
  C.KeepAlive:=CheckBox1.Checked;
  C.OnRequest:=OnRequest;
  C.OnResponse:=OnResponse;
  C.OnDestroy:=OnDestroyClient;

  SetMiddlewares(C);

  FConnections.Add(C);
  SetServerControls;

end;

procedure TForm3.OnDestroyClient(Sender: TObject);
begin
  FConnections.Remove(Sender);
  SetServerControls;
end;

procedure TForm3.SpeedButton1Click(Sender: TObject);
begin
  ContentMemo.BringToFront;
end;

procedure TForm3.SpeedButton2Click(Sender: TObject);
begin
  RequestsMemo.BringToFront;
end;

procedure TForm3.SpeedButton3Click(Sender: TObject);
begin
  ResponseMemo.BringToFront;
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  StartStopServer;
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  CloseClients;
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  RequestsMemo.Clear;
end;

end.
