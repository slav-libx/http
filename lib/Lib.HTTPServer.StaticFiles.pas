unit Lib.HTTPServer.StaticFiles;

interface

uses
  System.SysUtils,
  System.Classes,
  Lib.HTTPConsts,
  Lib.HTTPContent,
  Lib.HTTPUtils,
  Lib.HTTPServer;

type
  TStaticFiles = class(TInterfacedObject,IMiddleware)
  private
    FHome: string;
    FAliases: TStrings;
  public
    constructor Create(const Home: string; Aliases: TStrings);
    destructor Destroy; override;
    function Use(Request: TRequest; Response: TResponse): Boolean;
  end;

implementation

constructor TStaticFiles.Create(const Home: string; Aliases: TStrings);
begin
  FHome:=Home;
  FAliases:=TStringList.Create;
  FAliases.Assign(Aliases);
end;

destructor TStaticFiles.Destroy;
begin
  FAliases.Free;
  inherited;
end;

function TStaticFiles.Use(Request: TRequest; Response: TResponse): Boolean;
var FileName: string;
begin

  Result:=False;

  if Request.Method=METHOD_GET then
  begin

    FileName:=HTTPResourceToLocalFileName(Request.Resource,FHome,FAliases);

    if FileExists(FileName) then
    begin

      Response.SetResult(HTTPCODE_SUCCESS,'OK');

      Response.AddContentFile(FileName);

      Result:=True;

    end;

  end;

end;

end.
