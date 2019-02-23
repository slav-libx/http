unit Lib.HTTPServer.Middleware;

interface

uses
  Lib.HTTPContent;

type
  IMiddleware = interface
    function Use(Request: TRequest; Response: TResponse): Boolean;
  end;

  TMiddleware = record
    Method: string;
    Route: string;
    Middleware: IMiddleware;
    constructor Create(const Method,Route: string; Middleware: IMiddleware);
  end;

implementation

constructor TMiddleware.Create(const Method,Route: string; Middleware: IMiddleware);
begin
  Self.Method:=Method;
  Self.Route:=Route;
  Self.Middleware:=Middleware;
end;

end.
