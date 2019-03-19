unit Lib.TCPSocket;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.WinSock2,
  Lib.CleanSocket,
  Lib.SSL;

const
  CM_SOCKETMESSAGE = WM_USER + $0001;
  CM_DEFERFREE = WM_USER + $0002;

type
  TTCPSocket = class
  private type
    TSocketMessage = record
      Msg: Cardinal;
      Socket: TSocket;
      SelectEvent: Word;
      SelectError: Word;
      Result: Longint;
    end;
  private
    FForceClose: Boolean;
    FEventHandle: HWND;
    FOnDestroy: TNotifyEvent;
    FOnException: TNotifyEvent;
    procedure WndMethod(var Message: TMessage);
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure CMSocketMessage(var Message: TSocketMessage); message CM_SOCKETMESSAGE;
    procedure CMDeferFree(var Message); message CM_DEFERFREE;
  protected
    FSocket: TSocket;
    FAdIn: TSockAddrIn;
    FExceptionCode: Integer;
    FExceptionMessage: string;
    procedure DoExceptCode(Code: Integer); virtual;
    procedure DoExcept(Code: Integer); virtual;
    procedure DoEvent(EventCode: Word); virtual; abstract;
    procedure DoTimeout(Code: Integer); virtual;
    procedure StartEvents(EventCodes: Integer);
    procedure SetTimeout(Elapsed: Cardinal; Code: Integer);
    procedure Check(R: Integer);
    procedure Close; virtual;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ToString: string; override;
    procedure DefaultHandler(var Message); override;
    procedure DeferFree;
    property ExceptionCode: Integer read FExceptionCode;
    property ExceptionMessage: string read FExceptionMessage;
    property OnException: TNotifyEvent read FOnException write FOnException;
  end;

  TTCPClient = class(TTCPSocket)
  private
    FSSL: TSSL;
    FOnOpen: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnRead: TNotifyEvent;
    function GetUseSSL: Boolean;
    procedure SetUseSSL(Value: Boolean);
  protected
    procedure DoExceptCode(Code: Integer); override;
    procedure DoEvent(EventCode: Word); override;
    procedure DoOpen; virtual;
    procedure DoRead; virtual;
    procedure DoClose; virtual;
    procedure CheckConnect;
    procedure Close; override;
  public
//    function WaitForRead(WaitTime: Integer): Boolean;
    function ConnectTo(const Host: string; Port: Integer): Boolean;
    function ReadString(Encoding: TEncoding = nil): string;
    function ReadBuf(var Buffer; Count: Integer): Integer;
    function Read(MaxBuffSize: Integer=10240): TBytes;
    procedure WriteString(const S: string; Encoding: TEncoding = nil);
    procedure WriteBuf(var Buffer; Count: Integer);
    procedure Write(B: TBytes);
    constructor Create; override;
    procedure AcceptOn(S: TSocket);
  public
    property UseSSL: Boolean read GetUseSSL write SetUseSSL;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnRead: TNotifyEvent read FOnRead write FOnRead;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  TTCPServer = class(TTCPSocket)
  private
    FOnAccept: TNotifyevent;
  protected
    procedure DoEvent(EventCode: Word); override;
    procedure DoAccept; virtual;
  public
    procedure Start(const Host: string; Port: Integer);
    procedure Stop;
    function AcceptClient: TSocket;
  public
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
  end;

implementation

var
  WSAData: TWSAData;

function host_isip(const Host: string): Boolean;
begin
  case inet_addr(PAnsiChar(AnsiString(Host))) of
  INADDR_NONE,0: Result:=False;
  else
    Result:=Length(Host.Split(['.']))=4;
  end;
end;

function host_ipfromname(const Name: string): string;
var
  H: PHostEnt;
begin
  H:=gethostbyname(PAnsiChar(AnsiString(Name)));
  if H=nil then
    Result:='0.0.0.1'
  else
    Result:=inet_ntoa(PInAddr(H^.h_addr_list^)^);
end;

procedure sock_addr_iptoinaddr(const IP: string; var InAddr: TInAddr);
var Octets: TArray<string>;
begin
  Octets:=IP.Split(['.']);
  if Length(Octets)<>4 then Octets:='0.0.0.1'.Split(['.']);
  InAddr.s_un_b.s_b1:=StrToInt(Octets[0]);
  InAddr.s_un_b.s_b2:=StrToInt(Octets[1]);
  InAddr.s_un_b.s_b3:=StrToInt(Octets[2]);
  InAddr.s_un_b.s_b4:=StrToInt(Octets[3]);
end;

{ TTCPSocket }

constructor TTCPSocket.Create;
begin
  FSocket:=0;
  FEventHandle:=AllocateHWnd(WndMethod);
end;

destructor TTCPSocket.Destroy;
begin
  Close;
  if FEventHandle<>0 then DeallocateHWnd(FEventHandle);
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  inherited;
end;

function TTCPSocket.ToString: string;
begin
  Result:=inherited+'('+FSocket.ToString+')';
end;

procedure TTCPSocket.Close;
begin
  if FSocket<1 then Exit;
  shutdown(FSocket,1);
  if FForceClose then
    closesocket(FSocket)
  else
    AddCleanSocket(FSocket);
  FSocket:=0;
end;

procedure TTCPSocket.DoExceptCode(Code: Integer);
begin
  FExceptionCode:=Code;
  FExceptionMessage:=SysErrorMessage(Code);
end;

procedure TTCPSocket.DoExcept(Code: Integer);
begin
  DoExceptCode(Code);
  FForceClose:=True;
  Close;
  if Assigned(FOnException) then FOnException(Self);
end;

procedure TTCPSocket.DoTimeout(Code: Integer);
begin
end;

procedure TTCPSocket.Check(R: Integer);
begin
  if R=-1 then DoExcept(WSAGetLastError);
end;

procedure TTCPSocket.WndMethod(var Message: TMessage);
begin
  try
    Dispatch(Message);
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
  end;
end;

procedure TTCPSocket.WMTimer(var Message: TWMTimer);
begin
  SetTimeout(0,Message.TimerID);
  if FSocket>0 then DoTimeout(Message.TimerID);
end;

procedure TTCPSocket.CMSocketMessage(var Message: TSocketMessage);
begin
  if Message.SelectError=0 then
  if Message.Socket>0 then
  if Message.Socket=FSocket then
  begin
    if Message.SelectEvent=FD_CLOSE then
      FForceClose:=True;
    DoEvent(Message.SelectEvent)
  end else
  if Message.SelectEvent=FD_CLOSE then
    CloseCleanSocket(Message.Socket);
end;

procedure TTCPSocket.CMDeferFree(var Message);
begin
  Free;
end;

procedure TTCPSocket.DeferFree;
begin
  if FEventHandle<>0 then PostMessage(FEventHandle,CM_DEFERFREE,0,0);
end;

procedure TTCPSocket.DefaultHandler(var Message);
begin
  if FEventHandle<>0 then with TMessage(Message) do
  Result:=CallWindowProc(@DefWindowProc,FEventHandle,Msg,wParam,lParam);
end;

procedure TTCPSocket.StartEvents(EventCodes: Integer);
begin
  Check(WSAAsyncSelect(FSocket,FEventHandle,CM_SOCKETMESSAGE,EventCodes));
end;

procedure TTCPSocket.SetTimeout(Elapsed: Cardinal; Code: Integer);
begin
  if Elapsed=0 then
    KillTimer(FEventHandle,Code)
  else
    SetTimer(FEventHandle,Code,Elapsed,nil);
end;

{ TTCPClient }

procedure TTCPClient.Close;
begin
  UseSSL:=False;
  inherited;
end;

function TTCPClient.GetUseSSL: Boolean;
begin
  Result:=Assigned(FSSL);
end;

procedure TTCPClient.SetUseSSL(Value: Boolean);
begin
  if Value<>UseSSL then
  if Value then
    FSSL:=TSSL.Create
  else
    FreeAndNil(FSSL);
end;

procedure TTCPClient.DoExceptCode(Code: Integer);
begin
  if UseSSL and (FSSL.ErrorCode=Code) then
  begin
    FExceptionCode:=Code;
    FExceptionMessage:=FSSL.ErrorText;
  end else
    inherited;
end;

constructor TTCPClient.Create;
begin
  inherited;
  FSSL:=nil;
end;

procedure TTCPClient.AcceptOn(S: TSocket);
begin
  FSocket:=S;
  FForceClose:=False;
  DoOpen;
  StartEvents(FD_READ or FD_CLOSE);
end;

function TTCPClient.ConnectTo(const Host: string; Port: Integer): Boolean;
var R: Integer; IP: string;
begin

  Result:=False;

  FForceClose:=True;

  FSocket:=socket(2,1,0);
  IP:=Host;
  if not host_isip(IP) then
    IP:=host_ipfromname(Host);
  sock_addr_iptoinaddr(IP,FAdIn.sin_addr);
  FAdIn.sin_family:=2;
  FAdIn.sin_port:=htons(Port);

  R:=connect(FSocket,TSockAddr(FAdIn),SizeOf(TSockAddr));

  if R<>-1 then
    Result:=True
  else
    if WSAGetLastError<>10035 then Check(R);

  if Result and UseSSL then
  if not fssl.connect(FSocket) then
  begin
    DoExcept(fssl.ErrorCode);
    Exit(False);
  end;

  if Result then AcceptOn(FSocket);

end;

procedure TTCPClient.DoOpen;
begin
  if Assigned(FOnOpen) then FOnOpen(Self);
end;

procedure TTCPClient.DoRead;
begin
  if Assigned(FOnRead) then FOnRead(Self);
end;

procedure TTCPClient.DoClose;
begin
  if Assigned(FOnClose) then FOnClose(Self);
end;

procedure TTCPClient.DoEvent(EventCode: Word);
begin
  if EventCode=FD_READ then DoRead;
  if EventCode=FD_CLOSE then DoClose;
end;

//function TTCPClient.WaitForRead(WaitTime: Integer): Boolean;
//var
//  FDSet: TFDSet;
//  TimeVal: TTimeVal;
//begin
//  FD_ZERO(FDSet);
//  FDSet.fd_array[0]:=FSocket;
//  FDSet.fd_count:=1;
//  TimeVal.tv_sec:=WaitTime div 1000;
//  TimeVal.tv_usec:=WaitTime mod 1000;
//  Result:=select(0,@FDSet,nil,nil,@TimeVal)>0;
//end;

procedure TTCPClient.CheckConnect;
begin
  if FSocket=0 then DoExcept(10057);
end;

function TTCPClient.ReadBuf(var Buffer; Count: Integer): Integer;
begin
  //CheckConnect;
  if UseSSL then
    Result:=fSSL.recv(Buffer,Count)
  else
    Result:=recv(FSocket,Pointer(Buffer)^,Count,0);
  if Result=-1 then Result:=0;
end;

function TTCPClient.Read(MaxBuffSize: Integer=10240): TBytes;
begin
  SetLength(Result,MaxBuffSize);
  SetLength(Result,ReadBuf(Result,Length(Result)));
end;

function TTCPClient.ReadString(Encoding: TEncoding = nil): string;
begin
  if Encoding=nil then
    Result:=TEncoding.Default.GetString(Read)
  else
    Result:=Encoding.GetString(Read);
end;

procedure TTCPClient.WriteBuf(var Buffer; Count: Integer);
var
  W: Boolean;
  I: Integer;
begin
  CheckConnect;
  W:=False;
  I:=0;
  while not W do
  begin
    if UseSSL then
      I:=fSSL.send(Buffer,Count)
    else
      I:=send(FSocket,Pointer(Buffer)^,Count,0);
    W:=(I<>-1) or (WSAGetLastError<>10035);
  end;
  Check(I);
end;

procedure TTCPClient.Write(B: TBytes);
begin
  WriteBuf(B,Length(B));
end;

procedure TTCPClient.WriteString(const S: string; Encoding: TEncoding = nil);
begin
  if Encoding=nil then
    Write(TEncoding.Default.GetBytes(S))
  else
    Write(Encoding.GetBytes(S));
end;

{ TTCPServer }

procedure TTCPServer.DoAccept;
begin
  if Assigned(FOnAccept) then FOnAccept(Self);
end;

procedure TTCPServer.DoEvent(EventCode: Word);
begin
  if EventCode=FD_ACCEPT then DoAccept;
end;

procedure TTCPServer.Start(const Host: string; Port: Integer);
var IP: string;
begin
  FForceClose:=True;
  FSocket:=socket(2,1,0);
  FillChar(FAdIn,SizeOf(FAdIn),0);
  if Host<>'' then begin
    IP:=Host;
    if not host_isip(IP) then
      IP:=host_ipfromname(IP);
    sock_addr_iptoinaddr(IP,FAdIn.sin_addr);
  end;
  FAdIn.sin_family:=2;
  FAdIn.sin_port:=htons(Port);
  Check(bind(FSocket,TSockAddr(FAdIn),SizeOf(TSockAddr)));
  Check(listen(FSocket,1));
  StartEvents(FD_ACCEPT);
end;

procedure TTCPServer.Stop;
begin
  Close;
end;

function TTCPServer.AcceptClient: TSocket;
begin
  Result:=accept(FSocket,nil,nil);
end;

initialization
  WSAStartup($202,WSAData);

end.
