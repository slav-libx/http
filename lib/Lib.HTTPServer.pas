unit Lib.HTTPServer;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  Winapi.Winsock2,
  Lib.TCPSocket,
  Lib.HTTPConsts,
  Lib.HTTPUtils,
  Lib.HTTPSocket;

type

  THTTPServerClient = class(THTTPSocket)
  private
    FHome: string;
    FAliases: TStrings;
    FOnRequest: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    FKeepAliveTimeout: Cardinal;
    FKeepAlive: Boolean;
    procedure SetKeepAliveTimeout(Value: Cardinal);
  protected
    procedure DoTimeout(Code: Integer); override;
    procedure DoRead; override;
    procedure DoResponse;
    procedure DoReadComplete; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property OnRequest: TNotifyEvent read FOnRequest write FOnRequest;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
    property KeepAliveTimeout: Cardinal read FKeepAliveTimeout write SetKeepAliveTimeout;
  end;

  THTTPConnections = class
  private
    FClients: TList<TObject>;
    FOnClientsChange: TNotifyEvent;
    FOnRequest: TNotifyEvent;
    FOnResponse: TNotifyEvent;
  protected
    procedure DoClientsChange;
    function GetClientsCount: Integer;
  public
    procedure DoResponse(Client: TObject);
    procedure DoRequest(Client: TObject);
    procedure DoClose(Client: TObject);
    procedure DoDestroy(Client: TObject);
    procedure AddClient(Client: THTTPServerClient);
    procedure RemoveClient(Client: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DropClients;
    property ClientsCount: Integer read GetClientsCount;
    property OnClientsChange: TNotifyEvent read FOnClientsChange write FOnClientsChange;
    property OnRequest: TNotifyEvent read FOnRequest write FOnRequest;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
  end;

  THTTPServer = class(TTCPServer)
  private
    FConnections: THTTPConnections;
    FHome: string;
    FAliases: TStrings;
    FKeepAliveTimeout: Cardinal;
  protected
    procedure DoEvent(EventCode: Word); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Connections: THTTPConnections write FConnections;
    property Home: string read FHome write FHome;
    property Aliases: TStrings read FAliases;
    property KeepAliveTimeout: Cardinal read FKeepAliveTimeout write FKeepAliveTimeout;
  end;

implementation

const
  TIMEOUT_KEEPALIVE=1;

{ THTTPServerClient }

constructor THTTPServerClient.Create;
begin
  inherited;
  FKeepAlive:=True;
  FAliases:=TStringList.Create;
  SetTimeout(KeepAliveTimeout*1000,TIMEOUT_KEEPALIVE);
end;

destructor THTTPServerClient.Destroy;
begin
  FAliases.Free;
  inherited;
end;

procedure THTTPServerClient.SetKeepAliveTimeout(Value: Cardinal);
begin
  if FKeepAliveTimeout<>Value then
  begin
    FKeepAliveTimeout:=Value;
    SetTimeout(KeepAliveTimeout*1000,TIMEOUT_KEEPALIVE);
  end;
end;

procedure THTTPServerClient.DoRead;
begin

  SetTimeout(0,TIMEOUT_KEEPALIVE);

  while Request.DoRead(Read(20000))>0 do;

  if not FKeepAlive then Free;

end;

procedure THTTPServerClient.DoReadComplete;
begin

  if Assigned(FOnRequest) then FOnRequest(Self);

  FKeepAlive:=(KeepAliveTimeout>0) and Request.ConnectionKeepAlive;

  DoResponse;

  if FKeepAlive then
    SetTimeout(KeepAliveTimeout*1000,TIMEOUT_KEEPALIVE);

end;

procedure THTTPServerClient.DoTimeout(Code: Integer);
begin
  Free;
end;

procedure THTTPServerClient.DoResponse;
var FileName: string;
begin

  Response.Reset;
  Response.Protocol:=PROTOCOL_HTTP11;
  Response.AddHeaderKeepAlive(FKeepAlive,KeepAliveTimeout);

  if Request.Protocol=PROTOCOL_HTTP11 then
  begin

    if Request.Method=METHOD_GET then
    begin

      FileName:=HTTPResourceToLocalFileName(Request.Resource,FHome,FAliases);

      if FileExists(FileName) then
      begin

        Response.SetResult(HTTPCODE_SUCCESS,'OK');

        Response.AddContentFile(FileName);

      end else

      if FileName='' then
      begin

        Response.SetResult(HTTPCODE_BAD_REQUEST,'Bad Request');

      end else
      begin

        Response.SetResult(HTTPCODE_NOT_FOUND,'Not Found');

        Response.AddContentText(content_404,HTTPGetMIMEType('.html'));

      end
    end else
    begin

      Response.SetResult(HTTPCODE_METHOD_NOT_ALLOWED,'Method Not Allowed');

    end;

  end else
  begin

    Response.SetResult(HTTPCODE_NOT_SUPPORTED,'HTTP Version Not Supported');

  end;

  WriteString(Response.SendHeaders);

  Write(Response.Content);

  if Assigned(FOnResponse) then FOnResponse(Self);

end;

{ THTTPConnections }

constructor THTTPConnections.Create;
begin
  FClients:=TList<TObject>.Create;
end;

destructor THTTPConnections.Destroy;
begin
  DropClients;
  FClients.Free;
  inherited;
end;

procedure THTTPConnections.DoClientsChange;
begin
  if Assigned(FOnClientsChange) then FOnClientsChange(Self);
end;

procedure THTTPConnections.AddClient(Client: THTTPServerClient);
begin
  FClients.Add(Client);
  Client.OnRequest:=DoRequest;
  Client.OnResponse:=DoResponse;
  Client.OnClose:=DoClose;
  Client.OnDestroy:=DoDestroy;
  DoClientsChange;
end;

procedure THTTPConnections.RemoveClient(Client: TObject);
begin
  FClients.Remove(Client);
  DoClientsChange;
end;

function THTTPConnections.GetClientsCount: Integer;
begin
  Result:=FClients.Count;
end;

procedure THTTPConnections.DropClients;
begin
  while FClients.Count>0 do FClients[0].Free;
end;

procedure THTTPConnections.DoResponse(Client: TObject);
begin
  if Assigned(FOnResponse) then FOnResponse(Client);
end;

procedure THTTPConnections.DoRequest(Client: TObject);
begin
  if Assigned(FOnRequest) then FOnRequest(Client);
end;

procedure THTTPConnections.DoClose(Client: TObject);
begin
  Client.Free;
end;

procedure THTTPConnections.DoDestroy(Client: TObject);
begin
  RemoveClient(Client);
end;

{ THTTPServer }

procedure THTTPServer.DoEvent(EventCode: Word);
var C: THTTPServerClient;
begin
  inherited;
  if EventCode=FD_ACCEPT then
  begin
    C:=THTTPServerClient.CreateOn(AcceptClient);
    C.FHome:=Home;
    C.FAliases.Assign(FAliases);
    C.KeepAliveTimeout:=KeepAliveTimeout;
    if Assigned(FConnections) then FConnections.AddClient(C);
  end;
end;

constructor THTTPServer.Create;
begin
  inherited;
  FKeepAliveTimeout:=10;
  FAliases:=TStringList.Create;
end;

destructor THTTPServer.Destroy;
begin
  FAliases.Free;
  inherited;
end;

end.
