unit Form.ServerMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.JSON,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Lib.HTTPContent,
  Lib.HTTPServer,
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
    Label6: TLabel;
    Edit4: TEdit;
    ContentMemo: TMemo;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Button4: TButton;
    Label2: TLabel;
    SpeedButton3: TSpeedButton;
    ResponseMemo: TMemo;
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
    FConnections: THTTPConnections;
    procedure OnClientsChange(Sender: TObject);
    procedure OnRequest(Sender: TObject);
    procedure OnResponse(Sender: TObject);
    procedure SetServerControls;
  public
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin

  FServer:=nil;
  FConnections:=nil;

  Caption:='HTTP server ['+GetEnvironmentVariable('ComputerName')+']';

  Memo2.Clear;

  FStore:=TJSONStore.Create(ExtractFilePath(ParamStr(0))+'serv-store.json');

  BoundsRect:=FStore.ReadRect('form.rect',BoundsRect);
  Edit1.Text:=FStore.ReadString('host');
  Edit2.Text:=FStore.ReadInteger('port',80).ToString;
  Edit3.Text:=FStore.ReadString('home',ExtractFilePath(ParamStr(0))+'Home');
  Edit4.Text:=FStore.ReadInteger('keep-alive.timeout',10).ToString;
//  CheckBox1.Checked:=FStore.ReadBool('keep-alive.enabled',False);
  FStore.ReadStrings('aliases',Memo2.Lines);

  SpeedButton2.Down:=True;
  SpeedButton2.OnClick(nil);

  SetServerControls;

  Button1.Click;

end;

procedure TForm3.FormDestroy(Sender: TObject);
begin

  if WindowState=TWindowState.wsNormal then
    FStore.WriteRect('form.rect',BoundsRect);
  FStore.WriteString('host',Edit1.Text);
  FStore.WriteInteger('port',StrToIntDef(Edit2.Text,80));
  FStore.WriteString('home',Edit3.Text);
  FStore.WriteInteger('keep-alive.timeout',StrToIntDef(Edit4.Text,10));
  //FStore.WriteBool('keep-alive.enabled',CheckBox1.Checked);
  FStore.WriteStrings('aliases',Memo2.Lines);

  FStore.Free;
  FConnections.Free;
  FServer.Free;

end;

procedure TForm3.SetServerControls;
var
  A,B: Boolean;
  C: Integer;
begin

  C:=0;

  A:=Assigned(FServer);
  if Assigned(FConnections) then C:=FConnections.ClientsCount;
  B:=C>0;

  if A then Button1.Caption:='Stop' else Button1.Caption:='Start';
  Button2.Enabled:=B;
  Label2.Caption:=' '+C.ToString+' ';

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
  if Assigned(FServer) then
  begin
    FreeAndNil(FServer);
    RequestsMemo.Lines.Add('Server stoped'#13#10);
    SetServerControls;
  end else begin
    if not Assigned(FConnections) then
    begin
      FConnections:=THTTPConnections.Create;
      FConnections.OnClientsChange:=OnClientsChange;
      FConnections.OnRequest:=OnRequest;
      FConnections.OnResponse:=OnResponse;
    end;
    FServer:=THTTPServer.Create;
    try
      FServer.Connections:=FConnections;
      FServer.Home:=Edit3.Text;
      FServer.Aliases.Assign(Memo2.Lines);
      FServer.KeepAliveTimeout:=StrToInt64Def(Edit4.Text,10);
      FServer.Start(Edit1.Text,StrToInt(Edit2.Text));
      RequestsMemo.Lines.Add('Server started'#13#10);
      SetServerControls;
    except
      FreeAndNil(FServer);
      raise;
    end;
  end;
end;

procedure TForm3.OnClientsChange(Sender: TObject);
begin
  SetServerControls;
end;

procedure TForm3.OnRequest(Sender: TObject);
var C: THTTPServerClient;
begin
  C:=THTTPServerClient(Sender);
  RequestsMemo.Lines.Add(C.Request.Method+' '+C.Request.Resource);
  RequestsMemo.Lines.AddStrings(C.Request.Headers);
  RequestsMemo.Lines.Add('');
  C.Request.ShowContentTo(ContentMemo.Lines);
end;

procedure TForm3.OnResponse(Sender: TObject);
var C: THTTPServerClient;
begin
  C:=THTTPServerClient(Sender);
  RequestsMemo.Lines.Add(C.Response.ResultCode.ToString+' '+C.Response.ResultText);
  RequestsMemo.Lines.AddStrings(C.Response.Headers);
  RequestsMemo.Lines.Add('');
  C.Response.ShowContentTo(ResponseMemo.Lines);
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  if Assigned(FConnections) then FConnections.DropClients;
  SetServerControls;
  RequestsMemo.Lines.Add('Drop clients connections'#13#10);
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  RequestsMemo.Clear;
end;

end.
