unit Lib.HTTPUtils;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.NetEncoding,
  Lib.HTTPConsts;

type
  THTTPException = class(Exception);

  TRingBuffer<T> = record
    DataSize: Integer;
    Data: array of T;
    StartIndex,EndIndex: Integer;
    procedure Init(ADataSize: Integer=1024);
    function EOF: Boolean;
    procedure Write(const S: T);
    function Read: T;
  end;

function HTTPGetContentExt(const ContentType: string): string;
function HTTPGetMIMEType(const FileExt: string): string;
procedure HTTPGetContentTypes(Strings: TStrings);
function HTTPExtractResourceName(const Resource: string): string;
function HTTPDecodeResourceName(const ResourceName: string): string;
function HTTPEncodeResourceName(const ResourceName: string): string;
procedure HTTPSplitURL(const URL: string; out Protocol,Host,Resource: string);
procedure HTTPSplitHost(const Host: string; out HostName,Port: string);
function HTTPTrySplitResponseResult(Header: TStrings; out Protocol: string; out Code: Integer; out Text: string): Boolean;
function HTTPTrySplitRequest(const Request: string; out AMethod,AResource,AProtocol: string): Boolean;
function HTTPGetHeaderValue(Header: TStrings; const Name: string): string;
procedure HTTPSetHeaderValue(Header: TStrings; const Name,Value: string);
function HTTPGetTag(const Value,Tag: string): string;
function HTTPGetTagValue(const Tag: string): string;
function HTTPEndedChunked(const B: TBytes): Boolean;
function HTTPBytesFromChunked(const B: TBytes): TBytes;
function HTTPGetHeaderLength(const B: TBytes): Integer;
function HTTPResourceToLocalFileName(const Resource: string; const HomePath: string; Aliases: TStrings): string;
function HTTPResourceToLocal(const Resource: string): string;
function HTTPExtractFileName(const Resource: string): string;

function BytesEndsWith(const B,E: TBytes): Boolean;

implementation

function SameMap(const V: array of string; const D: string;
  O1,O2: Integer; CompareProc: TFunc<string,Boolean>): string;
var I: Integer;
begin
  Result:=D;
  for I:=0 to High(V) div 2 do
  if CompareProc(V[I*2+O1]) then Exit(V[I*2+O2]);
end;

function HTTPGetContentExt(const ContentType: string): string;
begin
  Result:=
    SameMap(MIME_Types,'',1,0,
    function(MIMEType: string): Boolean
    begin
      Result:=ContentType.StartsWith(MIMEType);
    end);
end;

function HTTPGetMIMEType(const FileExt: string): string;
begin
  Result:=
    SameMap(MIME_Types,'application/octet-stream',0,1,
    function(MIMEType: string): Boolean
    begin
      Result:=FileExt.ToLower=MIMEType;
    end);
end;

procedure HTTPGetContentTypes(Strings: TStrings);
var I: Integer;
begin
  Strings.BeginUpdate;
  Strings.Clear;
  for I:=0 to High(MIME_Types) div 2 do
    Strings.Add(MIME_Types[I*2+1]);
  Strings.EndUpdate;
end;

function HTTPExtractResourceName(const Resource: string): string;
begin
  Result:=HTTPExtractFileName(Resource);
  if Result.Contains('?') then
    Result:=Result.Substring(0,Result.IndexOf('?'));
end;

function HTTPDecodeResourceName(const ResourceName: string): string;
begin
  try
    Result:=TNetEncoding.URL.Decode(ResourceName);
  except
  on E: EConvertError do Exit(ResourceName);
  else raise;
  end;
end;

function HTTPEncodeResourceName(const ResourceName: string): string;
begin
  try
    Result:=TNetEncoding.URL.Encode(ResourceName);
    Result:=Result.Replace('%2F','/',[rfReplaceAll]);
    Result:=Result.Replace('+','%20',[rfReplaceAll]);
  except
  on E: EConvertError do Exit(ResourceName);
  else raise;
  end;
end;

procedure HTTPSplitURL(const URL: string; out Protocol,Host,Resource: string);
var Index: Integer; S: string;
begin

  S:=URL;

  Index:=S.IndexOf('://');
  if Index<>-1 then
  begin
    Protocol:=S.Substring(0,Index);
    S:=S.Substring(Index+3);
  end else
    Protocol:='';

  Index:=S.IndexOf('/');
  if Index<>-1 then
  begin
    Host:=S.Substring(0,Index);
    Resource:=S.Substring(Index);
  end else begin
    Host:=S;
    Resource:='/';
  end;

end;

procedure HTTPSplitHost(const Host: string; out HostName,Port: string);
var Index: Integer;
begin
  HostName:=Host;
  Index:=HostName.IndexOf(':');
  if Index<>-1 then
  begin
    Port:=HostName.Substring(Index+1);
    HostName:=HostName.Substring(0,Index);
  end else
    Port:='';
end;

function HTTPTrySplitResponseResult(Header: TStrings; out Protocol: string; out Code: Integer; out Text: string): Boolean;
var Index1,Index2: Integer;
begin
  if Header.Count=0 then Exit(False);
  Index1:=Header[0].IndexOf(' ',0)+1;
  Index2:=Header[0].IndexOf(' ',Index1);
  Result:=TryStrToInt(Header[0].Substring(Index1,Index2-Index1),Code);
  Protocol:=Header[0].Substring(0,Index1-1);
  Text:=Header[0].Substring(Index1);
end;

function HTTPTrySplitRequest(const Request: string; out AMethod,AResource,AProtocol: string): Boolean;
var
  RequestMethod: string;
  Index1,Index2,Index3: Integer;
begin

  Index3:=Length(Request);

  RequestMethod:=Request.Substring(0,Index3);

  Index1:=RequestMethod.IndexOf(' ');
  Index2:=RequestMethod.LastIndexOf(' ');

  Result:=(Index1<>-1) and (Index2>Index1);

  if Result then
  begin
    AMethod:=RequestMethod.Substring(0,Index1);
    AResource:=RequestMethod.Substring(Index1+1,Index2-Index1-1);
    AProtocol:=RequestMethod.Substring(Index2+1);
  end;

end;

function CompareBytesWith(const B,E: TBytes; StartIndex: Integer=0): Boolean;
var I: Integer;
begin
  Result:=True;
  if not InRange(StartIndex,0,High(B)-High(E)) then Exit(False);
  for I:=0 to High(E) do
    if B[StartIndex+I]<>E[I] then Exit(False);
end;

function BytesEndsWith(const B,E: TBytes): Boolean;
begin
  Result:=CompareBytesWith(B,E,High(B)-High(E));
end;

function BytesIndexOf(const B,E: TBytes; StartIndex: Integer=0): Integer;
begin
  for Result:=StartIndex to High(B)-High(E) do
    if CompareBytesWith(B,E,Result) then Exit;
  Result:=-1;
end;

function HTTPGetHeaderLength(const B: TBytes): Integer;
var Index: Integer;
begin
  Result:=BytesIndexOf(B,[13,10,13,10]);
end;

function HTTPGetHeaderValue(Header: TStrings; const Name: string): string;
var S: string;
begin
  Result:='';
  for S in Header do if S.StartsWith(Name+': ') then
    Exit(S.Substring(Name.Length+2));
end;

procedure HTTPSetHeaderValue(Header: TStrings; const Name,Value: string);
var I: Integer;
begin
  for I:=0 to Header.Count-1 do
  if Header[I].StartsWith(Name+': ') then
  begin
    Header[I]:=Name+': '+Value;
    Exit;
  end;
  Header.Add(Name+': '+Value);
end;

function HTTPGetTag(const Value,Tag: string): string;
var S: string;
begin
  for S in Value.Split([';']) do
  if S.Trim.StartsWith(Tag+'=') then Exit(S);
  Result:='';
end;

function HTTPGetTagValue(const Tag: string): string;
var P: Integer;
begin
  Result:='';
  P:=Tag.IndexOf('=');
  if P<>-1 then Result:=Tag.Substring(P+1);
end;

function HTTPEndedChunked(const B: TBytes): Boolean;
begin
  Result:=BytesEndsWith(B,[48,13,10,13,10]);
end;

function HTTPBytesFromChunked(const B: TBytes): TBytes;
var Index,ChunkIndex,ResultIndex,ChunkSize: Integer;
begin
  SetLength(Result,Length(B));
  Index:=0;
  ResultIndex:=0;
  while True do
  begin
    ChunkIndex:=BytesIndexOf(B,[13,10],Index);
    if not InRange(ChunkIndex,1,High(B)-2) then Break;
    ChunkSize:=StrToIntDef('$'+TEncoding.ANSI.GetString(B,Index,ChunkIndex-Index),0);
    if not InRange(ChunkSize,1,High(B)-ChunkIndex-6) then Break;
    Move(B[ChunkIndex+2],Result[ResultIndex],ChunkSize);
    Inc(ResultIndex,ChunkSize);
    Index:=ChunkIndex+4+ChunkSize;
  end;
  SetLength(Result,ResultIndex);
end;

function HTTPResourceToLocalFileName(const Resource: string; const HomePath: string; Aliases: TStrings): string;
var
  P,N: string;
  I: Integer;
begin

  Result:=HTTPDecodeResourceName(Resource);

  for I:=0 to Aliases.Count-1 do
  begin
    N:=Aliases.Names[I];
    if N='' then Continue;
    N:='/'+N;
    if Result.StartsWith(N) then
    begin
      P:=Aliases.ValueFromIndex[I];
      Result:=P+Result.Substring(Length(N));
      if IsRelativePath(P) then Result:='/'+Result;
    end;
  end;

  if IsRelativePath(Result) then Result:=HomePath+Result;

end;

procedure TRingBuffer<T>.Init(ADataSize: Integer=1024);
begin
  DataSize:=ADataSize;
  SetLength(Data,DataSize);
  StartIndex:=0;
  EndIndex:=0;
end;

function TRingBuffer<T>.EOF: Boolean;
begin
  Result:=StartIndex=EndIndex;
end;

procedure TRingBuffer<T>.Write(const S: T);
begin
  Data[EndIndex]:=S;
  EndIndex:=(EndIndex+1) mod DataSize;
  if EOF then raise Exception.Create('buffer_overflow');
end;

function TRingBuffer<T>.Read: T;
begin
  Result:=Data[StartIndex];
  StartIndex:=(StartIndex+1) mod DataSize;
end;

function HTTPResourceToLocal(const Resource: string): string;
begin
  Result:=Resource.Replace('/','\',[rfReplaceAll]).TrimLeft(['\']);
end;

function HTTPExtractFileName(const Resource: string): string;
var I: Integer;
begin
  I:=Resource.LastDelimiter('/\');
  Result:=Resource.SubString(I+1);
end;

end.
