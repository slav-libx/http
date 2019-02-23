unit Lib.HTTPServer.MiddlewareStatic;

interface

uses
  System.SysUtils,
  System.Classes,
  Lib.HTTPConsts,
  Lib.HTTPContent,
  Lib.HTTPUtils,
  Lib.HTTPServer.Middleware;

type
  TMiddlewareStatic = class(TInterfacedObject,IMiddleware)
  private
    FHome: string;
    FAliases: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    function Use(Request: TRequest; Response: TResponse): Boolean;
    property Home: string read FHome write FHome;
    property Aliases: TStrings read FAliases;
  end;

implementation

constructor TMiddlewareStatic.Create;
begin
  FHome:='';
  FAliases:=TStringList.Create;
end;

destructor TMiddlewareStatic.Destroy;
begin
  FAliases.Free;
  inherited;
end;

function TMiddlewareStatic.Use(Request: TRequest; Response: TResponse): Boolean;
var FileName: string;
begin

  Result:=False;

  FileName:=HTTPResourceToLocalFileName(Request.Resource,FHome,FAliases);

  if FileExists(FileName) then
  begin

    Response.SetResult(HTTPCODE_SUCCESS,'OK');

    Response.AddContentFile(FileName);

    Result:=True;

  end;

end;

end.
