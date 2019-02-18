unit Lib.HTTPConsts;

interface

const
  HTTP_PORT = 80;
  HTTPS_PORT = 443;
  HTTP_SUCCESS = 200;
  CRLF = #13#10;
  CRLF2 = CRLF+CRLF;

  content_404 =
    '<html>'+CRLF+
    '<head><title>404 Not Found</title></head>'+CRLF+
    '<body bgcolor="white">'+CRLF+
    '<center><h1>404 Not Found</h1></center>'+CRLF+
    '<hr><center>server</center>'+CRLF+
    '</body>'+CRLF+
    '</html>'+CRLF;

implementation

end.
