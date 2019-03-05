unit Lib.HTTPSocket;

interface

uses
  Lib.TCPSocket,
  Lib.HTTPContent;

type

  THTTPSocket = class(TTCPClient)
  private
    FRequest: TRequest;
    FResponse: TResponse;
    FReadTimeout: Cardinal;
    FKeepAliveTimeout: Integer;
    FKeepAlive: Boolean;
    procedure OnReadHeader(Sender: TObject);
    procedure OnReadContent(Sender: TObject);
    procedure OnReadComplete(Sender: TObject);
  protected const
    TIMEOUT_KEEPALIVE=1;
    TIMEOUT_READ=2;
  protected
    procedure SetReadTimeout(Value: Cardinal);
    procedure SetKeepAliveTimeout(Value: Integer);
    procedure DoReadHeader; virtual;
    procedure DoReadContent; virtual;
    procedure DoReadComplete; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Request: TRequest read FRequest;
    property Response: TResponse read FResponse;
    property KeepAliveTimeout: Integer read FKeepAliveTimeout write FKeepAliveTimeout;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    property ReadTimeout: Cardinal read FReadTimeout write FReadTimeout;
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

end.
