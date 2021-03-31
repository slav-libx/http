unit Lib.SSL;

interface

uses
  System.SysUtils,
  Winapi.Winsock2,
  Lib.SSL.Api;

type
  TSSL = class
  protected
    FErrorText: string;
    FErrorCode: Integer;
    FPSSL: PSSL;
    FPSSL_CTX: PSSL_CTX;
    FConnected: Boolean;
  public
    constructor create;
    destructor destroy; override;
    function connect(Socket: TSocket): Boolean;
    procedure shutdown;
    function recv(var b; w: Integer): Integer;
    function send(var b; w: Integer): Integer;
    procedure seterror(m: string);
    function cipher: string;
    function init: Boolean;
    function deinit: Boolean;
    function prepare: Boolean;
    function accept(Socket: TSocket): Boolean;
    function SetKeys: Boolean;
  public
    CertCAFile: string;
    CertificateFile: string;
    PrivateKeyFile: string;
    KeyPassword: string;
    Verify: Boolean;
    property ErrorCode: Integer read FErrorCode;
    property ErrorText: string read FErrorText;
  end;

implementation

function PasswordCallback(buf:PAnsiChar; size:Integer; rwflag:Integer; userdata: Pointer):Integer; cdecl;
var
  Password: AnsiString;
begin
  Password := '';
  if TSSL(userdata) is TSSL then
    Password := TSSL(userdata).KeyPassword;
  if Length(Password) > (Size - 1) then
    SetLength(Password, Size - 1);
  Result := Length(Password);
  StrLCopy(buf, PAnsiChar(Password + #0), Result + 1);
end;

constructor TSSL.create;
            begin
            FErrorText:='';
            FErrorCode:=0;
            FPSSL:=nil;
            FPSSL_CTX:=nil;
            FConnected:=False;
            Verify:=False;
            end;

destructor  TSSL.destroy;
            begin
            shutdown;
            inherited;
            end;

function    TSSL.SetKeys: Boolean;
            begin

            Result:=True;

            if CertificateFile <> '' then
              Result:=SslCtxUseCertificateChainFile(FPSSL_CTX,CertificateFile)=1;

            if Result then
            if PrivateKeyFile <> '' then
              Result:=SslCtxUsePrivateKeyFile(FPSSL_CTX, PrivateKeyFile, SSL_FILETYPE_PEM)=1;

            if Result then
            if CertCAFile <> '' then
              Result:=SslCtxLoadVerifyLocations(FPSSL_CTX, CertCAFile, '')=1;

            end;

function    TSSL.init:Boolean;
            var s:ansistring;
            begin

            result:=False;

            FErrorText:='';
            FErrorCode:=0;

          //FPSSL_CTX:=sslctxnew(sslmethodv2);
            FPSSL_CTX:=sslctxnew(sslmethodv23);
          //FPSSL_CTX:=sslctxnew(sslmethodv3);

            if FPSSL_CTX=nil
            then seterror('')
            else begin

                 s:='DEFAULT';

                 sslctxsetcipherlist(FPSSL_CTX,s);

                 if Verify then
                 begin
                   sslctxsetverify(FPSSL_CTX,SSL_VERIFY_PEER,nil);
                   SslCtxSetDefaultPasswdCb(FPSSL_CTX, @PasswordCallback);
                   SslCtxSetDefaultPasswdCbUserdata(FPSSL_CTX, self);
                   if not SetKeys then Exit;
                 end else
                   sslctxsetverify(FPSSL_CTX,SSL_VERIFY_NONE,nil);

                 FPSSL:=sslnew(FPSSL_CTX);

                 if FPSSL=nil
                 then seterror('')
                 else result:=True;

                 end;

            end;

function    TSSL.deinit:Boolean;
            begin

            result:=True;

            if assigned(FPSSL)
            then begin
                 sslfree(FPSSL);
                 FPSSL:=nil;
                 end;

            if assigned(FPSSL_CTX)
            then begin
                 sslctxfree(FPSSL_CTX);
                 FPSSL_CTX:=nil;
                 errremovestate(0);
                 end;

            FConnected:=False;

            end;

function    TSSL.prepare:Boolean;
            begin
            result:=False;
            deinit;
            if init
            then result:=True
            else deinit;
            end;

function    TSSL.connect(Socket: TSocket):Boolean;
            begin

            Result:=False;

            if Socket<>-1 then

            if prepare and (sslsetfd(FPSSL,Socket)>=1) and (sslconnect(FPSSL)>=1)
            then begin
                 FConnected:=True;
                 Result:=True;
                 end
            else seterror('');

            end;

procedure   TSSL.shutdown;
            begin
            if assigned(FPSSL)
            then sslshutdown(FPSSL);
            deinit;
            end;

function    TSSL.accept(Socket: TSocket): Boolean;
            begin
            Result:=False;
            if prepare and (sslsetfd(FPSSL,Socket)>=1)
            then while not Result do
                 case SslGetError(FPSSL,sslaccept(FPSSL)) of
                 SSL_ERROR_NONE: Result:=True;
                 SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE:;
                 else Break;
                 end
            else seterror('');
            end;

function    TSSL.cipher: string;
            begin
            result:=sslcipherdescription(sslgetcurrentcipher(FPSSL));
            end;

procedure   TSSL.seterror(m:string);
            var s:ansistring;
            begin

            if m='' then m:='Error connecting with SSL.';

            FErrorText:='';
            FErrorCode:=errgeterror;

            errclearerror;

            if FErrorCode<>0
            then begin
                 s:=stringofchar(#0,256);
                 errerrorstring(FErrorCode,s,length(s));
                 FErrorText:=s;
                 end
            else FErrorCode:=1;

            if FErrorText='' then FErrorText:=m;

            end;

function    TSSL.recv(var b;w:Integer):Integer;
            begin
            result:=sslread(FPSSL,pointer(b),w);
            end;

function    TSSL.send(var b;w:Integer):Integer;
            begin
            Result:=sslwrite(FPSSL,pointer(b),w);
            end;

end.
