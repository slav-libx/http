unit Lib.HTTPServer;

interface

uses
  System.SysUtils,
  System.Classes,
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
    FMiddlewares: array of IMiddleware;
    FOnRequest: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    FKeepConnection: Boolean;
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
    property OnDestroy;
  end;

implementation

{ THTTPServerClient }

constructor THTTPServerClient.Create;
begin
  inherited;
  FKeepConnection:=True;
  SetReadTimeout(ReadTimeout);
end;

destructor THTTPServerClient.Destroy;
begin
  inherited;
end;

procedure THTTPServerClient.Use(Middleware: IMiddleware);
begin
  FMiddlewares:=FMiddlewares+[Middleware];
end;

procedure THTTPServerClient.DoClose;
begin
  inherited;
  Free;
end;

procedure THTTPServerClient.DoRead;
begin

  SetReadTimeout(ReadTimeout);

  while Request.DoRead(Read(20000))>0 do; // call DoReadComplete

  if not FKeepConnection then Free;

end;

procedure THTTPServerClient.DoReadComplete;
begin

  SetReadTimeout(0);

  Request.Merge;

  if Assigned(FOnRequest) then FOnRequest(Self);

  FKeepConnection:=KeepAlive and (KeepAliveTimeout>0) and Request.ConnectionKeepAlive;

  DoResponse;

  if FKeepConnection then
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
  for Middleware in FMiddlewares do
  if Middleware.Use(Request,Response) then Exit(True);
end;

procedure THTTPServerClient.DoResponse;
begin

  Response.Reset;
  Response.Protocol:=PROTOCOL_HTTP11;
  Response.AddHeaderKeepAlive(FKeepConnection,KeepAliveTimeout);

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

  WriteString(Response.Compose);

  Write(Response.Content);

  if Assigned(FOnResponse) then FOnResponse(Self);

end;

end.
