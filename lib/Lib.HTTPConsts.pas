unit Lib.HTTPConsts;

interface

const
  HTTP_PORT = 80;
  HTTPS_PORT = 443;
  HTTP_SUCCESS = 200;
  CRLF = #13#10;
  CRLF2 = CRLF+CRLF;
  PROTOCOL_HTTP11 = 'HTTP/1.1';
  METHOD_GET = 'GET';
  TRANSPORT_HTTP = 'http';
  TRANSPORT_HTTPS = 'https';

  content_404 =

    '<html>'+CRLF+
    '<head><title>404 Not Found</title></head>'+CRLF+
    '<body bgcolor="white">'+CRLF+
    '<center><h1>404 Not Found</h1></center>'+CRLF+
    '<hr><center>server</center>'+CRLF+
    '</body>'+CRLF+
    '</html>';

  MIME_Types: array of string = [

    '.txt' ,'text/plain',
    '.html','text/html',
    '.css' ,'text/css',
    '.csv' ,'text/csv',
    '.xml' ,'text/xml',
    '.jpeg','image/jpeg',
    '.jpg' ,'image/jpeg',
    '.gif' ,'image/gif',
    '.svg' ,'image/svg+xml',
    '.png' ,'image/png',
    '.ico' ,'image/vnd.microsoft.icon',
    '.json','application/json',
    '.pdf' ,'application/pdf',
    '.zip' ,'application/zip',
    '.js'  ,'application/javascript',
    ''     ,'application/octet-stream',
    ''     ,'application/x-www-form-urlencoded',
    '.mpeg','video/mpeg',
    '.mp4' ,'video/mp4',
    '.wmv' ,'video/x-ms-wmv',
    '.flv' ,'video/x-flv',
    '.avi' ,'video/x-msvideo'];

implementation

end.
