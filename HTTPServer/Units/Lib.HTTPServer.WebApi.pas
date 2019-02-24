unit Lib.HTTPServer.WebApi;

interface

uses
  System.SysUtils,
  Lib.HTTPConsts,
  Lib.HTTPContent,
  Lib.HTTPServer;

type
  TWebApi = class(TInterfacedObject,IMiddleware)
  public
    constructor Create;
    destructor Destroy; override;
    function Use(Request: TRequest; Response: TResponse): Boolean;
  end;

implementation

constructor TWebApi.Create;
begin
end;

destructor TWebApi.Destroy;
begin
  inherited;
end;

function TWebApi.Use(Request: TRequest; Response: TResponse): Boolean;
begin

  Result:=False;

  if Request.Resource.StartsWith('/api/') then
  begin

    Result:=True;

    if Request.Method=METHOD_GET then
    begin

      if Request.Resource='/api/v' then
      begin

        Response.SetResult(HTTPCODE_SUCCESS,'OK');

        Response.AddContentText('{"version":"1.1.2.34"}','application/json');

      end else begin

        Response.SetResult(HTTPCODE_BAD_REQUEST,'Bad Request');

      end;

    end else

      Response.SetResult(HTTPCODE_METHOD_NOT_ALLOWED,'Method Not Allowed');

  end;

end;

end.
