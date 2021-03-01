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
    FOnResponse: TNotifyEvent;
    FOnIdle: TNotifyEvent;
 protected
    procedure DoExcept(Code: Integer); override;
    procedure DoTimeout(Code: Integer); override;
    procedure DoClose; override;
    procedure DoRead; override;
    procedure DoReadComplete; override;
    procedure DoConnectionClose; override;
    procedure DoRequest;
    procedure DoNextRequestGet;
  public
    procedure Get(const URL: string);
    procedure SendRequest;
    constructor Create; override;
    destructor Destroy; override;
    property OnRequest: TNotifyEvent read FOnRequest write FOnRequest;
    property OnResource: TNotifyEvent read FOnResource write FOnResource;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
    property OnDestroy;
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
  inherited;
end;

procedure THTTPClient.DoExcept(Code: Integer);
begin
  inherited;
  FActive:=False;
  FHost:='';
end;

procedure THTTPClient.DoTimeout(Code: Integer);
begin
  inherited;
  DoClose;
end;

procedure THTTPClient.DoConnectionClose;
begin
  inherited;
  FHost:='';
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

procedure THTTPClient.DoReadComplete;
var
  Timeout: Integer;
begin

  FActive:=False;

  if not KeepAlive or Response.Headers.ConnectionClose then
  begin

    DoConnectionClose;
    Timeout:=0;

  end else begin

    Timeout:=Response.Headers.KeepAliveTimeout;
    if Timeout=0 then Timeout:=KeepAliveTimeout;

  end;

  SetReadTimeout(0);
  SetKeepAliveTimeout(Timeout);

  Response.Merge(Request);

  if Assigned(FOnResponse) then FOnResponse(Self);

  DoNextRequestGet;

end;

procedure THTTPClient.DoRequest;
begin

  WriteString(Request.Compose);
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
    Request.DecomposeURL(FURLs.Read);
    Request.Headers.SetValue('Host',Request.Host);
    Request.Headers.SetConnection(KeepAlive,KeepAliveTimeout);

    SendRequest;

  end;

  if not FActive then
  if Assigned(FOnIdle) then FOnIdle(Self);

end;

procedure THTTPClient.SendRequest;
var
  Port: Integer;
  HostName,HostPort: string;
  SSLScheme: Boolean;
begin

  if FActive then
  raise Exception.Create('HTTPClient is active');

  HTTPSplitHost(Request.Host,HostName,HostPort);

  SSLScheme:=SameText(Request.Scheme,SCHEME_HTTPS);

  if SSLScheme then
    Port:=StrToIntDef(HostPort,HTTPS_PORT)
  else
    Port:=StrToIntDef(HostPort,HTTP_PORT);

  if Assigned(FOnResource) then FOnResource(Self);

  if (FHost='') or (FHostName<>HostName) or (FPort<>Port) then
  begin
    DoConnectionClose;
    UseSSL:=SSLScheme;
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
