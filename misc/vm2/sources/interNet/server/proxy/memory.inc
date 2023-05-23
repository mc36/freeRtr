Const
  relayListMax=128;
Type
  OneConnectionRecord=record
    pipe1:LongInt;              {pipeline id of accepted connection}
    pipe2:LongInt;              {pipeline id of initiated connection}
    pipe3:LongInt;              {pipeline id of control connection}
    time:LongInt;               {time of last action}
    cadr:OneTCPaddressRecord;   {client address}
    cprt:LongInt;               {client port}
    tadr:OneTCPaddressRecord;   {target address}
    tprt:LongInt;               {target port}
    domain:String;              {domain name}
    proto:LongInt;              {protocol: 0-tcp, 1-ssh, 2-tls, 3-ftp, 4-gopher}
    method:LongInt;             {method: 0-connect, 1-get, 2-post, 3-put}
    userpass:String;            {username#13password}
    uri:String;                 {url after domain}
    line:String;                {readed line}
    stat:LongInt;               {status: 0-disconn, 1-init, 2-req, 3-resing}
                                {4-resed, 5-hdr, 6-start, 7-conning, 8-securing}
                                {9-ready, 10-user, 11-pass, 12-pasv, 13-conning}
                                {14-type, 15-get, 16-start}
                                {98-ftp, 99-gopher, 100-conned}
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  relayListDat:array[1..relayListMax] of record
    adr:OneTCPaddressRecord;
    msk:OneTCPaddressRecord;
    end;
  relayListNum:LongInt;
  serverPort:LongInt;
  serverAddr:OneTCPaddressRecord;
  lastSent:LongInt;


Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;

Function TestAddrMask(a1,a2,a3:OneTCPaddressRecord):Boolean;
Var i:LongInt;
Begin;
TestAddrMask:=False;
for i:=1 to sizeof(a1) do if (a1[i] and a3[i]<>a2[i] and a3[i]) then exit;
TestAddrMask:=True;
End;

Function getWord(var a:String;s:String):String;
Var i:LongInt;
Begin;
i:=pos(s,a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+length(s),255);
End;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function lastPos(c:char;a:String):LongInt;
Var
  b:byte absolute c;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
o:=ab0+1;
for i:=1 to ab0 do if (ab[i]=b) then o:=i;
lastPos:=o;
End;
