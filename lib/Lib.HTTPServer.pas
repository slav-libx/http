unit Lib.HTTPServer;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Lib.TCPSocket,
  Lib.HTTPConsts,
  Lib.HTTPUtils,
  Lib.HTTPSocket,
  Lib.HTTPServer.Middleware;

type

  THTTPServer = class(TTCPServer);

  THTTPServerClient = class(THTTPSocket)
  private
    FMiddleware: array of TMiddleware;
    FOnRequest: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    FReadTimeout: Cardinal;
    FKeepAliveTimeout: Integer;
    FKeepAlive: Boolean;
    procedure SetReadTimeout(Value: Cardinal);
    procedure SetKeepAliveTimeout(Value: Integer);
  protected
    procedure DoTimeout(Code: Integer); override;
    procedure DoRead; override;
    procedure DoResponse;
    procedure DoReadComplete; override;
    function DoMethodMiddleware: Boolean;
    function DoUseMiddleware: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure UseMiddleware(const Method,Route: string; Middleware: IMiddleware);
    property OnRequest: TNotifyEvent read FOnRequest write FOnRequest;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
    property KeepAliveTimeout: Integer read FKeepAliveTimeout write FKeepAliveTimeout;
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

procedure THTTPServerClient.UseMiddleware(const Method,Route: string; Middleware: IMiddleware);
begin
  Insert(TMiddleware.Create(Method,Route,Middleware),FMiddleware,Length(FMiddleware));
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

function THTTPServerClient.DoMethodMiddleware: Boolean;
var M: TMiddleware;
begin
  Result:=False;
  for M in FMiddleware do
  if Request.Method=M.Method then Exit(True);
end;

function THTTPServerClient.DoUseMiddleware: Boolean;
var M: TMiddleware;
begin
  Result:=False;
  for M in FMiddleware do
  if Request.Method=M.Method then
  if Request.Resource.StartsWith(M.Route) then
  if M.Middleware.Use(Request,Response) then Exit(True);
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

  if not DoMethodMiddleware then
  begin

    Response.SetResult(HTTPCODE_METHOD_NOT_ALLOWED,'Method Not Allowed');

  end else

  if not DoUseMiddleware then
  begin

    Response.SetResult(HTTPCODE_NOT_FOUND,'Not Found');

    Response.AddContentText(content_404,HTTPGetMIMEType('.html'));

  end;

//      FileName:=HTTPResourceToLocalFileName(Request.Resource,FHome,FAliases);
//
//      if FileExists(FileName) then
//      begin
//
//        Response.SetResult(HTTPCODE_SUCCESS,'OK');
//
//        Response.AddContentFile(FileName);
//
//      end else
//
//      if FileName='' then
//      begin
//
//        Response.SetResult(HTTPCODE_BAD_REQUEST,'Bad Request');
//
//      end else
//      begin
//
//        Response.SetResult(HTTPCODE_NOT_FOUND,'Not Found');
//
//        Response.AddContentText(content_404,HTTPGetMIMEType('.html'));
//
//      end
//    end else
//    begin
//
//      Response.SetResult(HTTPCODE_METHOD_NOT_ALLOWED,'Method Not Allowed');
//
//    end;
//
//  end else
//  begin
//
//
//  end;

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

end.
