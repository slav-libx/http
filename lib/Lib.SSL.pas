unit Lib.SSL;

interface

uses
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
    property ErrorCode: Integer read FErrorCode;
    property ErrorText: string read FErrorText;
  end;

implementation

constructor TSSL.create;
            begin
            FErrorText:='';
            FErrorCode:=0;
            FPSSL:=nil;
            FPSSL_CTX:=nil;
            FConnected:=False;
            end;

destructor  TSSL.destroy;
            begin
            shutdown;
            inherited;
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
