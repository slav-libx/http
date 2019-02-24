unit Lib.HTTPServer;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Lib.HTTPConsts,
  Lib.HTTPUtils,
  Lib.HTTPSocket,
  Lib.TCPSocket,
  Lib.HTTPContent;

type

  IMiddleware = interface
    function Use(Request: TRequest; Response: TResponse): Boolean;
  end;

  THTTPServer = class(TTCPServer);

  THTTPServerClient = class(THTTPSocket)
  private
    FMiddleware: array of IMiddleware;
    FOnRequest: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    FReadTimeout: Cardinal;
    FKeepAliveTimeout: Integer;
    FKeepAlive: Boolean;
    procedure SetReadTimeout(Value: Cardinal);
    procedure SetKeepAliveTimeout(Value: Integer);
  protected
    procedure DoClose; override;
    procedure DoTimeout(Code: Integer); override;
    procedure DoRead; override;
    procedure DoResponse;
    procedure DoReadComplete; override;
    function DoUseMiddlewares: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Use(Middleware: IMiddleware);
    property OnRequest: TNotifyEvent read FOnRequest write FOnRequest;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
    property KeepAliveTimeout: Integer read FKeepAliveTimeout write FKeepAliveTimeout;
  end;

  THTTPConnections = class
  private
    FClients: TList<TObject>;
    FOnChange: TNotifyEvent;
  protected
    procedure DoChange;
    function GetClientsCount: Integer;
    procedure DoDestroy(Client: TObject);
  public
    procedure AddClient(Client: THTTPServerClient);
    procedure RemoveClient(Client: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DropClients;
    property ClientsCount: Integer read GetClientsCount;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

const
  TIMEOUT_KEEPALIVE=1;
  TIMEOUT_READ=2;

{ THTTPServerClient }

constructor THTTPServerClient.Create;
begin
  inherited;
  FKeepAlive:=True;
  FReadTimeout:=10000;
  SetReadTimeout(FReadTimeout);
end;

destructor THTTPServerClient.Destroy;
begin
  inherited;
end;

procedure THTTPServerClient.Use(Middleware: IMiddleware);
begin
  Insert(Middleware,FMiddleware,Length(FMiddleware));
end;

procedure THTTPServerClient.SetReadTimeout(Value: Cardinal);
begin
  SetTimeout(Value,TIMEOUT_READ);
end;

procedure THTTPServerClient.SetKeepAliveTimeout(Value: Integer);
begin
  if FKeepAlive then
    SetTimeout(Value*1000,TIMEOUT_KEEPALIVE)
  else
    SetTimeout(0,TIMEOUT_KEEPALIVE);
end;

procedure THTTPServerClient.DoClose;
begin
  inherited;
  Free;
end;

procedure THTTPServerClient.DoRead;
begin

  SetReadTimeout(FReadTimeout);

  while Request.DoRead(Read(20000))>0 do; // call DoReadComplete

  if not FKeepAlive then Free;

end;

procedure THTTPServerClient.DoReadComplete;
begin

  SetReadTimeout(0);

  if Assigned(FOnRequest) then FOnRequest(Self);

  FKeepAlive:=(KeepAliveTimeout>0) and Request.ConnectionKeepAlive;

  DoResponse;

  SetKeepAliveTimeout(KeepAliveTimeout);

end;

procedure THTTPServerClient.DoTimeout(Code: Integer);
begin
  Free;
end;

function THTTPServerClient.DoUseMiddlewares: Boolean;
var Middleware: IMiddleware;
begin
  Result:=False;
  for Middleware in FMiddleware do
  if Middleware.Use(Request,Response) then Exit(True);
end;

procedure THTTPServerClient.DoResponse;
begin

  Response.Reset;
  Response.Protocol:=PROTOCOL_HTTP11;
  Response.AddHeaderKeepAlive(FKeepAlive,KeepAliveTimeout);

  if Request.Protocol<>PROTOCOL_HTTP11 then
  begin

    Response.SetResult(HTTPCODE_NOT_SUPPORTED,'HTTP Version Not Supported')

  end else

  if not DoUseMiddlewares then
  begin

    if Request.Method=METHOD_GET then
    begin

      Response.SetResult(HTTPCODE_NOT_FOUND,'Not Found');

      Response.AddContentText(content_404,HTTPGetMIMEType('.html'));

    end else

      Response.SetResult(HTTPCODE_METHOD_NOT_ALLOWED,'Method Not Allowed');

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

procedure THTTPConnections.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure THTTPConnections.AddClient(Client: THTTPServerClient);
begin
  FClients.Add(Client);
  Client.OnDestroy:=DoDestroy;
  DoChange;
end;

procedure THTTPConnections.RemoveClient(Client: TObject);
begin
  FClients.Remove(Client);
  DoChange;
end;

function THTTPConnections.GetClientsCount: Integer;
begin
  Result:=FClients.Count;
end;

procedure THTTPConnections.DropClients;
begin
  while FClients.Count>0 do FClients[0].Free;
end;

procedure THTTPConnections.DoDestroy(Client: TObject);
begin
  RemoveClient(Client);
end;

end.
