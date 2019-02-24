unit Lib.HTTPClient;

interface

uses
  System.SysUtils,
  System.Classes,
  Lib.TCPSocket,
  Lib.HTTPConsts,
  Lib.HTTPUtils,
  Lib.HTTPSocket;

type
  THTTPClient = class(THTTPSocket)
  private
    FURLs: TRingBuffer<string>;
    FHost: string;
    FHostName: string;
    FPort: Integer;
    FActive: Boolean;
    FOnResource: TNotifyEvent;
    FOnRequest: TNotifyEvent;
    FOnResponseHeader: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    FOnIdle: TNotifyEvent;
    FOnMessage: TNotifyEvent;
    FMessage: string;
 protected
    procedure DoExcept(Code: Integer); override;
    procedure DoTimeout(Code: Integer); override;
    procedure DoOpen; override;
    procedure DoClose; override;
    procedure DoRead; override;
    procedure DoReadHeader; override;
    procedure DoReadComplete; override;
    procedure DoMessage(const TextMessage: string);
    procedure DoConnectionClose;
    procedure DoRequest;
    procedure DoNextRequestGet;
  public
    procedure Get(const URL: string);
    procedure SendRequest;
    constructor Create; override;
    destructor Destroy; override;
    property OnRequest: TNotifyEvent read FOnRequest write FOnRequest;
    property OnResource: TNotifyEvent read FOnResource write FOnResource;
    property OnResponseHeader: TNotifyEvent read FOnResponseHeader write FOnResponseHeader;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
    property OnMessage: TNotifyEvent read FOnMessage write FOnMessage;
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
    property OnDestroy;
    property Message: string read FMessage;
  end;

implementation

constructor THTTPClient.Create;
begin
  inherited;
  FActive:=False;
  FURLs.Init;
end;

destructor THTTPClient.Destroy;
begin
  DoMessage(ToString+' destroy');
  inherited;
end;

procedure THTTPClient.DoExcept(Code: Integer);
begin
  FActive:=False;
  FHost:='';
  inherited DoExcept(Code);
end;

procedure THTTPClient.DoTimeout(Code: Integer);
begin
  case Code of
  TIMEOUT_KEEPALIVE: DoMessage(ToString+' keepalive-timeout');
  TIMEOUT_READ: DoMessage(ToString+' read-timeout');
  else DoMessage(ToString+' timeout');
  end;
  DoClose;
end;

procedure THTTPClient.DoOpen;
begin
  DoMessage(ToString+' open');
  inherited;
end;

procedure THTTPClient.DoConnectionClose;
begin
  DoMessage(ToString+' close');
  FHost:='';
  Close;
end;

procedure THTTPClient.DoClose;
begin
  inherited;
  SetReadTimeout(0);
  SetKeepAliveTimeout(0);
  DoConnectionClose;
  if FActive then
  begin
    FActive:=False;
    DoNextRequestGet;
  end;
end;

procedure THTTPClient.DoRead;
begin
  while Response.DoRead(Read(20000))>0 do;
end;

procedure THTTPClient.DoReadHeader;
begin
  if Assigned(FOnResponseHeader) then FOnResponseHeader(Self);
end;

procedure THTTPClient.DoReadComplete;
var
  Timeout: Integer;
begin

  FActive:=False;

  if not KeepAlive or Response.ConnectionClose then
  begin

    DoConnectionClose;
    Timeout:=0;

  end else begin

    Timeout:=Response.KeepAliveTimeout;
    if Timeout=0 then Timeout:=KeepAliveTimeout;

  end;

  SetReadTimeout(0);
  SetKeepAliveTimeout(Timeout);

  Response.SetResource(Request.Resource);

  if Assigned(FOnResponse) then FOnResponse(Self);

  DoNextRequestGet;

end;

procedure THTTPClient.DoMessage(const TextMessage: string);
begin
  FMessage:=TextMessage;
  if Assigned(FOnMessage) then FOnMessage(Self);
end;

procedure THTTPClient.DoRequest;
begin

  WriteString(Request.SendHeaders);
  Write(Request.Content);

  if Assigned(FOnRequest) then FOnRequest(Self);

  SetReadTimeout(ReadTimeout);
  SetKeepAliveTimeout(0);

end;

procedure THTTPClient.DoNextRequestGet;
begin

  while not FActive and not FURLs.EOF do
  begin

    Request.Reset;
    Request.Protocol:=PROTOCOL_HTTP11;
    Request.Method:=METHOD_GET;
    Request.ParseURL(FURLs.Read);
    Request.AddHeaderValue('Host',Request.Host);
    Request.AddHeaderKeepAlive(KeepAlive,KeepAliveTimeout);

    SendRequest;

  end;

  if not FActive then
  if Assigned(FOnIdle) then FOnIdle(Self);

end;

procedure THTTPClient.SendRequest;
var
  Port: Integer;
  HostName,HostPort: string;
  ProtocolUseSSL: Boolean;
begin

  if FActive then
  raise Exception.Create('HTTPClient is active');

  HTTPSplitHost(Request.Host,HostName,HostPort);

  ProtocolUseSSL:=SameText(Request.Transport,TRANSPORT_HTTPS);

  if ProtocolUseSSL then
    Port:=StrToIntDef(HostPort,HTTPS_PORT)
  else
    Port:=StrToIntDef(HostPort,HTTP_PORT);

  if Assigned(FOnResource) then FOnResource(Self);

  if (FHost='') or (FHostName<>HostName) or (FPort<>Port) then
  begin
    if FSocket>0 then DoConnectionClose;
    UseSSL:=ProtocolUseSSL;
    if not ConnectTo(HostName,Port) then Exit;
  end;

  FActive:=True;

  FHost:=Request.Host;
  FHostName:=HostName;
  FPort:=Port;

  DoRequest;

end;

procedure THTTPClient.Get(const URL: string);
begin
  FURLs.Write(URL);
  DoNextRequestGet;
end;

end.
