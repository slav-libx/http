unit WebApi;

interface

uses
  System.SysUtils,
  Lib.HTTPConsts,
  Lib.HTTPContent,
  Lib.HTTPServer.Middleware;

type
  TMiddlewareWebApi = class(TInterfacedObject,IMiddleware)
  public
    constructor Create;
    destructor Destroy; override;
    function Use(Request: TRequest; Response: TResponse): Boolean;
  end;

implementation

constructor TMiddlewareWebApi.Create;
begin
end;

destructor TMiddlewareWebApi.Destroy;
begin
  inherited;
end;

function TMiddlewareWebApi.Use(Request: TRequest; Response: TResponse): Boolean;
begin

  Result:=True;

  if Request.Resource='/api/v' then
  begin

    Response.SetResult(HTTPCODE_SUCCESS,'OK');

    Response.AddContentText('{"version":"1.1.2.34"}','application/json');

  end else

    Response.SetResult(HTTPCODE_BAD_REQUEST,'Bad Request');

end;

end.
