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
    procedure OnReadHeader(Sender: TObject);
    procedure OnReadContent(Sender: TObject);
    procedure OnReadComplete(Sender: TObject);
  protected
    procedure DoReadHeader; virtual;
    procedure DoReadContent; virtual;
    procedure DoReadComplete; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Request: TRequest read FRequest;
    property Response: TResponse read FResponse;
  end;

implementation

constructor THTTPSocket.Create;
begin
  inherited;

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
