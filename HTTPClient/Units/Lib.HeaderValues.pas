unit Lib.HeaderValues;

interface

uses
  System.Classes,
  Lib.HTTPContent,
  Lib.HTTPUtils;

procedure GetHeaderRequestValues(const URL,HeaderName: string; Values: TStrings);

implementation

procedure GetHeaderRequestValues(const URL,HeaderName: string; Values: TStrings);
var Protocol,Host,Resource: string;
begin

  Values.Clear;

  if HeaderName='Host' then
  begin
    HTTPSplitURL(URL,Protocol,Host,Resource);
    Values.Add(Host);
  end else

  if HeaderName='Connection' then
  begin
    Values.Add('close');
    Values.Add('keep-alive');
    Values.Add('keep-alive, Upgrade');
    Values.Add('Upgrade');
  end else

  if HeaderName='Keep-Alive' then
  begin
    Values.Add('timeout=10');
  end else

  if HeaderName='Cookie' then
  begin
    Values.Add('Name=Value');
  end else

  if HeaderName='User-Agent' then
  begin
    Values.Add('Mozilla/5.0 (Windows NT 6.1; rv:61.0)');
  end else

  if HeaderName='Accept' then
  begin
    Values.Add('*/*');
    Values.Add('text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8');
  end else

  if HeaderName='Accept-Language' then
  begin
    Values.Add('ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3');
  end else

  if HeaderName='Accept-Encoding' then
  begin
    Values.Add('gzip, deflate, br');
  end else

  if HeaderName='Cache-Control' then
  begin
    Values.Add('max-age=0');
    Values.Add('no-cache');
  end else

  if HeaderName='Pragma' then
  begin
    Values.Add('no-cache');
  end else

  if HeaderName='Upgrade' then
  begin
    Values.Add('websocket');
  end else

  if HeaderName='If-Modified-Since' then
  begin
    Values.Add('Fri, 15 Feb 2019 21:10:37 GMT');
  end else

  if HeaderName='Content-Length' then
  begin
    Values.Add('0');
  end else

  if HeaderName='Upgrade-Insecure-Requests' then
  begin
    Values.Add('1');
  end else

  if HeaderName='Authorization' then
  begin
    Values.Add('Basic STRING_BASE64');
  end else

  if HeaderName='Content-Type' then
  begin
    HTTPGetContentTypes(Values);
  end;

end;

end.
