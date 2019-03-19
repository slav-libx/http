unit Lib.HTTPSocket;

interface

uses
  System.SysUtils,
  Lib.TCPSocket,
  Lib.HTTPTypes,
  Lib.HTTPContent;

type

  THTTPSocket = class(TTCPClient)
  private const
    TIMEOUT_KEEPALIVE=1;
    TIMEOUT_READ=2;
  private
    FRequest: TRequest;
    FResponse: TResponse;
    FReadTimeout: Cardinal;
    FKeepAliveTimeout: Integer;
    FKeepAlive: Boolean;
    FMonitor: IHTTPMonitor;
    procedure OnReadHeader(Sender: TObject);
    procedure OnReadContent(Sender: TObject);
    procedure OnReadComplete(Sender: TObject);
  protected
    procedure SetReadTimeout(Value: Cardinal);
    procedure SetKeepAliveTimeout(Value: Integer);
    procedure DoTimeout(Code: Integer); override;
    procedure DoOpen; override;
    procedure DoConnectionClose; virtual;
    procedure DoReadHeader; virtual;
    procedure DoReadContent; virtual;
    procedure DoReadComplete; virtual;
    procedure DoMessage(const Text: string); virtual;
    procedure DoExcept(Code: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Request: TRequest read FRequest;
    property Response: TResponse read FResponse;
    property KeepAliveTimeout: Integer read FKeepAliveTimeout write FKeepAliveTimeout;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    property ReadTimeout: Cardinal read FReadTimeout write FReadTimeout;
    property Monitor: IHTTPMonitor read FMonitor write FMonitor;
  end;

implementation

constructor THTTPSocket.Create;
begin
  inherited;

  FKeepAlive:=True;
  FKeepAliveTimeout:=10;
  FReadTimeout:=30000;

  FRequest:=TRequest.Create;

  FRequest.OnReadHeader:=OnReadHeader;
  FRequest.OnReadContent:=OnReadContent;
  FRequest.OnReadComplete:=OnReadComplete;

  FResponse:=TResponse.Create;

  FResponse.OnReadHeader:=OnReadHeader;
  FResponse.OnReadContent:=OnReadContent;
  FResponse.OnReadComplete:=OnReadComplete;

end;

destructor THTTPSocket.Destroy;
begin
  DoMessage(ToString+' destroy');
  FRequest.Free;
  FResponse.Free;
  inherited;
end;

procedure THTTPSocket.SetReadTimeout(Value: Cardinal);
begin
  SetTimeout(Value,TIMEOUT_READ);
end;

procedure THTTPSocket.SetKeepAliveTimeout(Value: Integer);
begin
  if KeepAlive then
    SetTimeout(Value*1000,TIMEOUT_KEEPALIVE)
  else
    SetTimeout(0,TIMEOUT_KEEPALIVE);
end;

procedure THTTPSocket.DoOpen;
begin
  DoMessage(ToString+' open');
  inherited;
end;

procedure THTTPSocket.DoConnectionClose;
begin
  if FSocket>0 then DoMessage(ToString+' close');
  Close;
end;

procedure THTTPSocket.DoTimeout(Code: Integer);
begin
  case Code of
  TIMEOUT_KEEPALIVE: DoMessage(ToString+' keepalive-timeout');
  TIMEOUT_READ: DoMessage(ToString+' read-timeout');
  else DoMessage(ToString+' timeout');
  end;
end;

procedure THTTPSocket.OnReadHeader(Sender: TObject);
begin
  DoReadHeader;
end;

procedure THTTPSocket.OnReadContent(Sender: TObject);
begin
  DoReadContent;
end;

procedure THTTPSocket.OnReadComplete(Sender: TObject);
begin
  DoReadComplete;
end;

procedure THTTPSocket.DoReadHeader;
begin
end;

procedure THTTPSocket.DoReadContent;
begin
end;

procedure THTTPSocket.DoReadComplete;
begin
end;

procedure THTTPSocket.DoMessage(const Text: string);
begin
  if Assigned(FMonitor) then FMonitor.DoMessage(Text);
end;

procedure THTTPSocket.DoExcept(Code: Integer);
begin
  inherited;
  DoMessage(ExceptionCode.ToString+' '+ExceptionMessage);
end;


end.
