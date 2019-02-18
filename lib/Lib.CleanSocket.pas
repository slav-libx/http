unit Lib.CleanSocket;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.WinSock2;

procedure AddCleanSocket(Socket: TSocket);
procedure CloseCleanSocket(Socket: TSocket);

implementation

type
  TCleanData = record
    Socket: TSocket;
    CloseTime: Longint;
  end;

var
  FCleanTimer: THandle=0;
  CleanSockets: array of TCleanData;

function GetCleanSocketIndex(Socket: TSocket): Integer;
begin
  if Socket>0 then
  for Result:=0 to High(CleanSockets) do
  if CleanSockets[Result].Socket=Socket then Exit;
  Result:=-1;
end;

function TryGetCleanSocketIndex(Socket: TSocket; out I: Integer): Boolean;
begin
  I:=GetCleanSocketIndex(Socket);
  Result:=I<>-1;
end;

procedure RemoveCleanSocketByIndex(I: Integer);
var C: TSocket;
begin
  C:=CleanSockets[I].Socket;
  CleanSockets[I].Socket:=0;
  if C>0 then
  if closesocket(C)=-1 then
    raise Exception.Create(SysErrorMessage(WSAGetLastError));
end;

procedure CloseCleanSockets(CloseTime: Longint);
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

procedure CloseCleanSocket(Socket: TSocket);
var I: Integer;
begin
  if TryGetCleanSocketIndex(Socket,I) then RemoveCleanSocketByIndex(I);
end;

procedure CleanSocketTimerProc(WND: hwnd; Msg: Longint; idEvent: UINT; dwTime: Longint); stdcall;
begin
  CloseCleanSockets(dwTime);
end;

function GetFreeCleanIndex: Integer;
begin
  for Result:=0 to High(CleanSockets) do
  if CleanSockets[Result].Socket=0 then Exit;
  Result:=Length(CleanSockets);
  SetLength(CleanSockets,Result+1);
end;

procedure AddCleanSocket(Socket: TSocket);
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

initialization

finalization
  CloseCleanSockets(LongInt.MaxValue);

end.
