unit Lib.TCPSocket;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.WinSock2,
  Lib.SSL;

type
  TTCPSocket = class
  private
    FForceClose: Boolean;
    FOnDestroy: TNotifyEvent;
    FOnException: TNotifyEvent;
    FLocalAddress: string;
    FLocalPort: Integer;
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
    function Check(R: Integer): Boolean;
    procedure Close; virtual;
    procedure UpdateLocal;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ToString: string; override;
    property LocalAddress: string read FLocalAddress write FLocalAddress;
    property LocalPort: Integer read FLocalPort write FLocalPort;
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
  public
//    function WaitForRead(WaitTime: Integer): Boolean;
    function ConnectTo(const Host: string; Port: Integer): Boolean;
    procedure Close; override;
    function ReadString(Encoding: TEncoding = nil): string;
    function ReadBuf(var Buffer; Count: Integer): Integer;
    function Read(MaxBuffSize: Integer=10240): TBytes;
    procedure WriteString(const S: string; Encoding: TEncoding = nil);
    procedure WriteBuf(var Buffer; Count: Integer);
    procedure Write(B: TBytes);
    constructor Create; override;
    procedure AcceptOn(S: TSocket);
    function SetKeepAlive(KeepAliveTime: Cardinal=1000; KeepAliveInterval: Cardinal=1000): Boolean;
  public
    property UseSSL: Boolean read GetUseSSL write SetUseSSL;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnRead: TNotifyEvent read FOnRead write FOnRead;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  TTCPServer = class(TTCPSocket)
  private
    FAcceptRemoteHost: string;
    FAcceptRemotePort: Integer;
    FOnAccept: TNotifyevent;
  protected
    procedure DoEvent(EventCode: Word); override;
    procedure DoAccept; virtual;
  public
    procedure Start(const Host: string; Port: Integer);
    procedure Stop;
    function AcceptClient: TSocket;
  public
    property AcceptRemoteHost: string read FAcceptRemoteHost;
    property AcceptRemotePort: Integer read FAcceptRemotePort;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
  end;

implementation

{ Support socket messages }

const
  CM_SOCKETMESSAGE = WM_USER + $0001;

type
  TSocketManager = class
  strict private type
    TSocketMessage = record
      Msg: Cardinal;
      Socket: TSocket;
      SelectEvent: Word;
      SelectError: Word;
      Result: Longint;
    end;
  strict private
    FEventHandle: HWND;
    Sockets: TArray<TTCPSocket>;
    procedure WndMethod(var Message: TMessage);
    procedure CMSocketMessage(var Message: TSocketMessage); message CM_SOCKETMESSAGE;
    function IndexOf(TCPSocket: TTCPSocket): Integer; overload;
    function IndexOf(Socket: TSocket): Integer; overload;
    function Get(Socket: TSocket): TTCPSocket;
    function TryGet(Socket: TSocket; out TCPSocket: TTCPSocket): Boolean;
  strict private type
    TCleanData = record
      Socket: TSocket;
      CloseTime: Longint;
    end;
  strict private
    FCleanTimer: THandle;
    CleanSockets: TArray<TCleanData>;
    function GetCleanSocketIndex(Socket: TSocket): Integer;
    function TryGetCleanSocketIndex(Socket: TSocket; out I: Integer): Boolean;
    procedure RemoveCleanSocketByIndex(I: Integer);
    function GetFreeCleanIndex: Integer;
  private
    procedure AddCleanSocket(Socket: TSocket);
    procedure CloseCleanSocket(Socket: TSocket);
    procedure CloseCleanSockets(CloseTime: Longint);
  strict private type
    TTimerEvent = record
      TimerID: UIntPtr;
      EventCode: Integer;
      Socket: TTCPSocket;
    end;
  strict private
    TimerEvents: TArray<TTimerEvent>;
    procedure RemoveTimerEvent(I: Integer); overload;
    procedure RemoveTimerEvent(TCPSocket: TTCPSocket; Code: Integer); overload;
    procedure RemoveTimerEvents(TCPSocket: TTCPSocket);
    function GetEventIndexOf(TCPSocket: TTCPSocket; Code: Integer): Integer; overload;
    function GetEventIndexOf(TimerID: UIntPtr): Integer; overload;
  private
    procedure SetTimerEvent(TCPSocket: TTCPSocket; Code: Integer; Elapsed: Cardinal);
    procedure DoEvent(TimerID: UIntPtr);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;
    procedure Add(TCPSocket: TTCPSocket; EventCodes: Integer);
    procedure Remove(TCPSocket: TTCPSocket; Closed: Boolean);
  end;

var
  SocketManager: TSocketManager;
  WSAData: TWSAData;

constructor TSocketManager.Create;
begin
  Sockets:=nil;
  CleanSockets:=nil;
  FCleanTimer:=0;
  FEventHandle:=AllocateHWnd(WndMethod);
end;

destructor TSocketManager.Destroy;
begin
  DeallocateHWnd(FEventHandle);
  CloseCleanSockets(LongInt.MaxValue);
end;

procedure TSocketManager.DefaultHandler(var Message);
begin
  with TMessage(Message) do
  Result:=CallWindowProc(@DefWindowProc,FEventHandle,Msg,wParam,lParam);
end;

procedure TSocketManager.WndMethod(var Message: TMessage);
begin
  try
    Dispatch(Message);
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
  end;
end;

procedure TSocketManager.CMSocketMessage(var Message: TSocketMessage);
var TCPSocket: TTCPSocket;
begin
  if Message.Socket>0 then
  //if Message.SelectError=0 then
  if TryGet(Message.Socket,TCPSocket) then
  begin
    if Message.SelectEvent=FD_CLOSE then
      TCPSocket.FForceClose:=True;
    TCPSocket.DoEvent(Message.SelectEvent);
  end else
  if Message.SelectEvent=FD_CLOSE then
    CloseCleanSocket(Message.Socket);
end;

function TSocketManager.IndexOf(TCPSocket: TTCPSocket): Integer;
begin
  for Result:=0 to High(Sockets) do
  if Sockets[Result]=TCPSocket then Exit;
  Result:=-1;
end;

function TSocketManager.IndexOf(Socket: TSocket): Integer;
begin
  for Result:=0 to High(Sockets) do
  if Sockets[Result].FSocket=Socket then Exit;
  Result:=-1;
end;

function TSocketManager.Get(Socket: TSocket): TTCPSocket;
var I: Integer;
begin
  I:=IndexOf(Socket);
  if I=-1 then
    Result:=nil
  else
    Result:=Sockets[I];
end;

function TSocketManager.TryGet(Socket: TSocket; out TCPSocket: TTCPSocket): Boolean;
begin
  TCPSocket:=Get(Socket);
  Result:=Assigned(TCPSocket);
end;

procedure TSocketManager.Add(TCPSocket: TTCPSocket; EventCodes: Integer);
begin
  if TCPSocket.Check(WSAAsyncSelect(TCPSocket.FSocket,FEventHandle,CM_SOCKETMESSAGE,EventCodes)) then
  Sockets:=Sockets+[TCPSocket];
end;

procedure TSocketManager.Remove(TCPSocket: TTCPSocket; Closed: Boolean);
begin
  Delete(Sockets,IndexOf(TCPSocket),1);
  if not Closed then
  if WSAAsyncSelect(TCPSocket.FSocket,FEventHandle,0,0)<>-1 then
    AddCleanSocket(TCPSocket.FSocket);
  RemoveTimerEvents(TCPSocket);
end;

function TSocketManager.GetCleanSocketIndex(Socket: TSocket): Integer;
begin
  if Socket>0 then
  for Result:=0 to High(CleanSockets) do
  if CleanSockets[Result].Socket=Socket then Exit;
  Result:=-1;
end;

function TSocketManager.TryGetCleanSocketIndex(Socket: TSocket; out I: Integer): Boolean;
begin
  I:=GetCleanSocketIndex(Socket);
  Result:=I<>-1;
end;

procedure TSocketManager.RemoveCleanSocketByIndex(I: Integer);
var C: TSocket;
begin
  C:=CleanSockets[I].Socket;
  CleanSockets[I].Socket:=0;
  if C>0 then
  if closesocket(C)=-1 then
    raise Exception.Create(SysErrorMessage(WSAGetLastError));
end;

procedure TSocketManager.CloseCleanSockets(CloseTime: Longint);
var I: Integer; Clean: Boolean;
begin
  Clean:=False;
  for I:=0 to High(CleanSockets) do
  if CleanSockets[I].Socket>0 then
  if CleanSockets[I].CloseTime<CloseTime then
    RemoveCleanSocketByIndex(I)
  else
    Clean:=True;
  if not Clean then
  if FCleanTimer>0 then
  begin
    KillTimer(0,FCleanTimer);
    FCleanTimer:=0;
  end;
end;

procedure TSocketManager.CloseCleanSocket(Socket: TSocket);
var I: Integer;
begin
  if TryGetCleanSocketIndex(Socket,I) then RemoveCleanSocketByIndex(I);
end;

function TSocketManager.GetFreeCleanIndex: Integer;
begin
  for Result:=0 to High(CleanSockets) do
  if CleanSockets[Result].Socket=0 then Exit;
  Result:=Length(CleanSockets);
  SetLength(CleanSockets,Result+1);
end;

procedure CleanSocketTimerProc(WND: hwnd; Msg: Longint; idEvent: UINT; dwTime: Longint); stdcall;
begin
  SocketManager.CloseCleanSockets(dwTime);
end;

procedure TSocketManager.AddCleanSocket(Socket: TSocket);
var I: Integer;
begin
  if Socket>0 then
  begin
    I:=GetFreeCleanIndex;
    CleanSockets[I].Socket:=Socket;
    CleanSockets[I].CloseTime:=TThread.GetTickCount+2000;
    if FCleanTimer=0 then
      FCleanTimer:=SetTimer(0,0,2000,@CleanSocketTimerProc);
  end;
end;

procedure TSocketManager.DoEvent(TimerID: UIntPtr);
var I: Integer;
begin
  I:=GetEventIndexOf(TimerID);
  if I<>-1 then
  try
    TimerEvents[I].Socket.DoTimeout(TimerEvents[I].EventCode);
  finally
    I:=GetEventIndexOf(TimerID);
    if I<>-1 then RemoveTimerEvent(I);
  end;
end;

function TSocketManager.GetEventIndexOf(TCPSocket: TTCPSocket; Code: Integer): Integer;
begin
  for Result:=0 to High(TimerEvents) do
  if (TimerEvents[Result].Socket=TCPSocket) and (TimerEvents[Result].EventCode=Code) then Exit;
  Result:=-1;
end;

function TSocketManager.GetEventIndexOf(TimerID: UIntPtr): Integer;
begin
  for Result:=0 to High(TimerEvents) do
  if TimerEvents[Result].TimerID=TimerID then Exit;
  Result:=-1;
end;

procedure EventsTimerProc(WND: hwnd; Msg: Longint; idEvent: UINT; dwTime: Longint); stdcall;
begin
  SocketManager.DoEvent(idEvent);
end;

procedure TSocketManager.SetTimerEvent(TCPSocket: TTCPSocket; Code: Integer; Elapsed: Cardinal);
var Event: TTimerEvent;
begin

  RemoveTimerEvent(TCPSocket,Code);

  if Elapsed>0 then
  begin
    Event.EventCode:=Code;
    Event.Socket:=TCPSocket;
    Event.TimerID:=SetTimer(0,0,Elapsed,@EventsTimerProc);
    TimerEvents:=TimerEvents+[Event];
  end;

end;

procedure TSocketManager.RemoveTimerEvent(I: Integer);
begin
  KillTimer(0,TimerEvents[I].TimerID);
  Delete(TimerEvents,I,1);
end;

procedure TSocketManager.RemoveTimerEvent(TCPSocket: TTCPSocket; Code: Integer);
var I: Integer;
begin
  I:=GetEventIndexOf(TCPSocket,Code);
  if I<>-1 then RemoveTimerEvent(I);
end;

procedure TSocketManager.RemoveTimerEvents(TCPSocket: TTCPSocket);
var I: Integer;
begin
  I:=0;
  while I<Length(TimerEvents) do
  if TimerEvents[I].Socket=TCPSocket then
    RemoveTimerEvent(I)
  else
    Inc(I);
end;

{ Utilities }

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
end;

destructor TTCPSocket.Destroy;
begin
  Close;
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
  if FForceClose then closesocket(FSocket);
  SocketManager.Remove(Self,FForceClose);
  FSocket:=0;
end;

procedure TTCPSocket.UpdateLocal;
var
  SockAddrIn: TSockAddrIn;
  Size: Integer;
begin
  Size:=SizeOf(SockAddrIn);
  if getsockname(FSocket,TSockAddr(SockAddrIn),Size)=0 then
  begin
    FLocalAddress:=string(inet_ntoa(SockAddrIn.sin_addr));
    FLocalPort:=ntohs(SockAddrIn.sin_port);
  end;
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

function TTCPSocket.Check(R: Integer): Boolean;
begin
  if R=-1 then DoExcept(WSAGetLastError);
  Result:=R<>-1;
end;

procedure TTCPSocket.StartEvents(EventCodes: Integer);
begin
  SocketManager.Add(Self,EventCodes);
end;

procedure TTCPSocket.SetTimeout(Elapsed: Cardinal; Code: Integer);
begin
  SocketManager.SetTimerEvent(Self,Code,Elapsed);
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
  UpdateLocal;
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
//  Close;
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
  I,LE,C: Integer;
begin
  CheckConnect;
  W:=False;
  I:=0;
  C:=0;
  while not W do
  begin
    if UseSSL then
      I:=fSSL.send(Buffer,Count)
    else
      I:=send(FSocket,Pointer(Buffer)^,Count,0);
    W:=True;
    LE:=0;
    if I=-1 then
    begin
      LE:=WSAGetLastError;
      if LE=10035 then
      begin
        Inc(C);
        W:=C>10;// False;
        Sleep(400);
      end;
    end;

  end;
  if I=-1 then DoExcept(LE);
  //Check(I);
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

const
  SIO_KEEPALIVE_VALS=(IOC_IN or IOC_VENDOR or 4);

type
  tcp_keepalive = record
    onoff: u_long;
    keepalivetime: u_long;
    keepaliveinterval: u_long;
  end;

function TTCPClient.SetKeepAlive(KeepAliveTime: Cardinal; KeepAliveInterval: Cardinal): Boolean;
var
  ka: tcp_keepalive;
  Bytes: Cardinal;
begin
  ka.onoff:=1;
  ka.keepalivetime:=KeepAliveTime;
  ka.keepaliveinterval:=KeepAliveInterval;
  Result:=WSAIoctl(FSocket,SIO_KEEPALIVE_VALS,@ka,SizeOf(ka),nil,0,Bytes,nil,nil)=0;
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
  P: PHostEnt;
  Buf: array [0..127] of Char;
  s: string;
begin

  FLocalAddress:='';

  if gethostname(@Buf,128)=0 then
  begin
    P:=gethostbyname(@Buf);
    if P<>nil then FLocalAddress:=inet_ntoa(PInAddr(p^.h_addr_list^)^);
  end;

  FForceClose:=True;
  FSocket:=socket(2,1,0);
  FAdIn:=Default(TSockAddrIn);
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
var
  SockAddr: TSockAddrIn;
  SockAddrLenght: Integer;
begin
  SockAddrLenght:=SizeOf(SockAddr);
  SockAddr:=Default(TSockAddrIn);
  Result:=accept(FSocket,@SockAddr,@SockAddrLenght);
  FAcceptRemoteHost:=inet_ntoa(SockAddr.sin_addr);
  FAcceptRemotePort:=ntohs(SockAddr.sin_port);
end;

initialization

  SocketManager:=TSocketManager.Create;

  WSAStartup($202,WSAData);

finalization

  SocketManager.Free;

end.
