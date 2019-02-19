unit Lib.HTTPContent;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  Lib.JSON.Format,
  Lib.HTTPConsts,
  Lib.HTTPUtils;

type
  TContent = class
  protected type
    TState = (stNone,stHeader,stContentLength,stChunked,stUnknownLength);
  private
    FState: TState;
    FHeaderLength: Integer;
    FContentLength: Integer;
    FContentReaded: Integer;
    FHeaders: TStrings;
    FOnHeader: TNotifyEvent;
    FOnContent: TNotifyEvent;
    FOnComplete: TNotifyEvent;
    procedure DoBeginRead;
  protected
    procedure DoHeader; virtual;
    procedure DoContent; virtual;
    procedure DoComplete; virtual;
  public
    Protocol: string;
    Content: TBytes;
    ResourceName: string;
    LocalResource: string;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddHeaderValue(const Name,Value: string);
    procedure AddHeaderKeepAlive(KeepAlive: Boolean; Timeout: Integer);
    procedure AddContentText(const Text: string); overload;
    procedure AddContentText(const Text,ContentType: string); overload;
    procedure AddContentFile(const FileName: string); overload;
    procedure AddContentFile(const FileName,ContentType: string); overload;
    function GetHeaderValue(const Name: string): string;
    property Headers: TStrings read FHeaders;
  public
    procedure Reset; virtual;
    procedure Assign(Source: TContent); virtual;
    function DoRead(const B: TBytes): Integer;
    function SendHeaders: string; virtual;
    procedure ShowContentTo(Strings: TStrings);
  public
    property ContentLength: Integer read FContentLength;
    property ContentReaded: Integer read FContentReaded;
    property OnReadHeader: TNotifyEvent write FOnHeader;
    property OnReadContent: TNotifyEvent write FOnContent;
    property OnReadComplete: TNotifyEvent write FOnComplete;
  end;

  TRequest = class(TContent)
  protected
    procedure DoHeader; override;
  public
    Host: string;
    Transport: string;
    Method: string;
    Resource: string;
    procedure Reset; override;
    procedure Assign(Source: TContent); override;
    procedure ParseURL(const URL: string);
    function SendHeaders: string; override;
  end;

  TResponse = class(TContent)
  protected
    procedure DoHeader; override;
  public
    ResultCode: Integer;
    ResultText: string;
    procedure Reset; override;
    procedure Assign(Source: TContent); override;
    function SendHeaders: string; override;
    procedure SetResult(Code: Integer; const Text: string);
    function ConnectionClose: Boolean;
    procedure SetResource(const Resource: string);
  end;

implementation

constructor TContent.Create;
begin
  FHeaders:=TStringList.Create;
  Reset;
end;

destructor TContent.Destroy;
begin
  FHeaders.Free;
  Content:=nil;
  inherited;
end;

procedure TContent.Reset;
begin
  FContentReaded:=0;
  FContentLength:=0;
  FHeaders.Clear;
  Content:=nil;
  Protocol:='';
  ResourceName:='';
  LocalResource:='';
end;

procedure TContent.Assign(Source: TContent);
begin
  Headers.Assign(Source.Headers);
  Content:=Source.Content;
  Protocol:=Source.Protocol;
  ResourceName:=Source.ResourceName;
  LocalResource:=Source.LocalResource;
end;

procedure TContent.AddHeaderValue(const Name,Value: string);
begin
  HTTPSetHeaderValue(Headers,Name,Value);
end;

procedure TContent.AddHeaderKeepAlive(KeepAlive: Boolean; Timeout: Integer);
begin
  if KeepAlive then
  begin
    AddHeaderValue('Connection','keep-alive');
    if Timeout>0 then
      AddHeaderValue('Keep-Alive','timeout='+Timeout.ToString);
  end else
    AddHeaderValue('Connection','close');
end;

procedure TContent.AddContentText(const Text: string);
begin
  AddContentText(Text,HTTPGetMIMEType('.txt'));
end;

procedure TContent.AddContentText(const Text,ContentType: string);
begin
  Content:=TEncoding.Default.GetBytes(Text);
  AddHeaderValue('Content-Type',ContentType);
end;

procedure TContent.AddContentFile(const FileName: string);
begin
  AddContentFile(FileName,HTTPGetMIMEType(ExtractFileExt(FileName)));
end;

procedure TContent.AddContentFile(const FileName,ContentType: string);
begin
  Content:=TFile.ReadAllBytes(FileName);
  AddHeaderValue('Content-Type',ContentType);
end;

function TContent.GetHeaderValue(const Name: string): string;
begin
  Result:=HTTPGetHeaderValue(Headers,Name);
end;

procedure TContent.DoBeginRead;
begin
  Reset;
  FState:=stHeader;
end;

function TContent.DoRead(const B: TBytes): Integer;
var L: Integer;
begin

  Result:=Length(B);

  if FState<>stUnknownLength then
  begin

    if Result=0 then Exit;

    if FState=stNone then DoBeginRead; // начало чтения запроса (ответа)

  end;

  L:=Result;

  SetLength(Content,FContentReaded+L);
  Move(B[0],Content[FContentReaded],L);
  Inc(FContentReaded,L);

  if FState=stHeader then
  begin

    FHeaderLength:=HTTPGetHeaderLength(Content);

    if FHeaderLength>0 then
    begin

      Headers.Text:=TEncoding.Default.GetString(Content,0,FHeaderLength);

      DoHeader;

      L:=FContentReaded-FHeaderLength-4;

      Move(Content[FContentReaded-L],Content[0],L);
      SetLength(Content,L);
      FContentReaded:=L;

      if TryStrToInt(GetHeaderValue('Content-Length'),FContentLength) then
        FState:=stContentLength
      else
      if GetHeaderValue('Transfer-Encoding')='chunked' then
        FState:=stChunked
      else
        FState:=stUnknownLength;

    end;

  end;

  if FState=stContentLength then
  begin

    if FContentReaded>=FContentLength then
    begin
      DoContent;
      FState:=stNone;
    end;

  end;

  if FState=stChunked then
  begin

    if HTTPEndedChunked(Content) then
    begin
      Content:=HTTPBytesFromChunked(Content);
      DoContent;
      FState:=stNone;
    end;

  end;

  if FState=stUnknownLength then
  begin

    if L=0 then
    begin
      DoContent;
      FState:=stNone;
    end;

  end;

  if FState=stNone then DoComplete; //загрузка ресурса завершена

end;

procedure TContent.DoHeader;
begin
  if Assigned(FOnHeader) then FOnHeader(Self);
end;

procedure TContent.DoContent;
begin
  if Assigned(FOnContent) then FOnContent(Self);
end;

procedure TContent.DoComplete;
begin
  if Assigned(FOnComplete) then FOnComplete(Self);
end;

function TContent.SendHeaders: string;
var S: string;
begin

  if Length(Content)>0 then
    AddHeaderValue('Content-Length',Length(Content).ToString);

  Result:='';

  for S in Headers do Result:=Result+S+CRLF;

  Result:=Result+CRLF;

end;

procedure TContent.ShowContentTo(Strings: TStrings);
var C: string;
begin

  Strings.Clear;

  C:=GetHeaderValue('Content-Type');

  try

    if (C='') or
       (C.StartsWith('text/')) or
       (C='application/x-www-form-urlencoded') or
       (C='application/javascript') then
      Strings.Text:=TEncoding.ANSI.GetString(Content)
    else

    if C.StartsWith('application/json') then
      Strings.Text:=ToJSON(TJSONObject.ParseJSONValue(
        TEncoding.UTF8.GetString(Content)));

  except
  end;

end;

{ TRequest }

procedure TRequest.Reset;
begin
  inherited;
  Host:='';
  Transport:='';
  Method:='';
  Resource:='';
end;

procedure TRequest.Assign(Source: TContent);
var S: TRequest;
begin
  inherited;
  if Source is TRequest then
  begin
    S:=TRequest(Source);
    Host:=S.Host;
    Transport:=S.Transport;
    Method:=S.Method;
    Resource:=S.Resource;
  end;
end;

procedure TRequest.ParseURL(const URL: string);
begin
  HTTPSplitURL(URL,Transport,Host,Resource);
  ResourceName:=HTTPDecodeResourceName(HTTPExtractResourceName(Resource));
  LocalResource:=HTTPResourceToLocal(Resource);
end;

function TRequest.SendHeaders: string;
begin
  Result:=inherited;
  Result:=Method+' '+HTTPEncodeResourceName(Resource)+' '+Protocol+CRLF+Result;
end;

procedure TRequest.DoHeader;
begin

  if Headers.Count>0 then
  if HTTPTrySplitRequest(Headers[0],Method,Resource,Protocol) then
  begin
    Headers.Delete(0);
  end;

  inherited;

end;

{ TResponse }

procedure TResponse.Reset;
begin
  inherited;
  Protocol:='';
  ResultCode:=0;
  ResultText:='';
end;

procedure TResponse.Assign(Source: TContent);
var S: TResponse;
begin
  inherited;
  if Source is TResponse then
  begin
    S:=TResponse(Source);
    Protocol:=S.Protocol;
    ResultCode:=S.ResultCode;
    ResultText:=S.ResultText;
  end;
end;

procedure TResponse.SetResult(Code: Integer; const Text: string);
begin
  ResultCode:=Code;
  ResultText:=Text;
end;

function TResponse.SendHeaders: string;
begin
  Result:=inherited;
  Result:=Protocol+' '+ResultCode.ToString+' '+ResultText+CRLF+Result;
end;

procedure TResponse.DoHeader;
begin

  if HTTPTrySplitResponseResult(Headers,Protocol,ResultCode,ResultText) then
  begin

    Headers.Delete(0);

  end;

  inherited;
end;

function TResponse.ConnectionClose: Boolean;
begin
  Result:=GetHeaderValue('Connection')='close';
end;

procedure TResponse.SetResource(const Resource: string);
var Ext: string;
begin

  ResourceName:=HTTPDecodeResourceName(HTTPExtractResourceName(Resource));
  LocalResource:=HTTPResourceToLocal(Resource);

  Ext:=HTTPGetContentExt(GetHeaderValue('Content-Type'));

  if ResultCode<>HTTP_SUCCESS then
    ResourceName:='res'
  else
  if ResourceName='' then
    ResourceName:='file';

  if GetHeaderValue('Content-Type')<>HTTPGetMIMEType(ExtractFileExt(ResourceName)) then
    ResourceName:=ChangeFileExt(ResourceName,Ext);

end;

end.
