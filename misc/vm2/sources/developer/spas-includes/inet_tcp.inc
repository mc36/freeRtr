Type
  OneTCPaddressRecord=array[1..16] of byte;
  OneTCPcommandHeader=record
    cmd:longint;
    adr:OneTCPaddressRecord;
    prt:LongInt;
    dat:LongInt;
    end;
Var
  TCPprocessId:LongInt;

Function TCPcompareAddress(var adr1,adr2):Boolean;
Var
  a1:array[1..1] of longint absolute adr1;
  a2:array[1..1] of longint absolute adr2;
Begin;
TCPcompareAddress:=False;
if (a1[4]<>a2[4]) then exit;
if (a1[1]<>a2[1]) then exit;
if (a1[2]<>a2[2]) then exit;
if (a1[3]<>a2[3]) then exit;
TCPcompareAddress:=True;
End;


Function TCPfindProcess:Boolean;
Begin;
TCPprocessId:=BugOS_findProcNam('tcp.code');
TCPfindProcess:=(TCPprocessId=0);
End;

Function SSHfindProcess:Boolean;
Begin;
TCPprocessId:=BugOS_findProcNam('ssh.code');
SSHfindProcess:=(TCPprocessId=0);
End;

Function TLSfindProcess:Boolean;
Begin;
TCPprocessId:=BugOS_findProcNam('tls.code');
TLSfindProcess:=(TCPprocessId=0);
End;

Function RTPfindProcess:Boolean;
Begin;
TCPprocessId:=BugOS_findProcNam('rtp.code');
RTPfindProcess:=(TCPprocessId=0);
End;

Function UDPfindProcess:Boolean;
Begin;
TCPprocessId:=BugOS_findProcNam('udp.code');
UDPfindProcess:=(TCPprocessId=0);
End;

Function DTLSfindProcess:Boolean;
Begin;
TCPprocessId:=BugOS_findProcNam('dtls.code');
DTLSfindProcess:=(TCPprocessId=0);
End;

Function DCCPfindProcess:Boolean;
Begin;
TCPprocessId:=BugOS_findProcNam('dccp.code');
DCCPfindProcess:=(TCPprocessId=0);
End;

Function TCPbeginConnect(var pipe:LongInt;size:LongInt;var addr;prt:LongInt):Boolean;
Var hdr:OneTCPcommandHeader;
Begin;
TCPbeginConnect:=True;
hdr.cmd:=1;
move(addr,hdr.adr,16);
hdr.prt:=prt and $ffff;
if (pipeLineCreate(pipe,TCPprocessId,size,false)<>0) then exit;
if (pipeLineSend(pipe,hdr,sizeof(hdr))<>0) then exit;
TCPbeginConnect:=False;
End;

Function TCPlookConnected(var pipe:LongInt;var addr;var prt,dat:LongInt):Boolean;
Label f1;
Var
  hdr:OneTCPcommandHeader;
  i,o,p:LongInt;
Begin;
TCPlookConnected:=True;
pipeLineStats(pipe,i,o,p);
if (o<sizeof(hdr)) then begin;
  if (i<>0) then exit;
  f1:
  pipeLineClose(pipe);
  pipe:=0;
  exit;
  end;
i:=sizeof(hdr);
if (pipeLineRecv(pipe,hdr,i)<>0) then exit;
if (hdr.cmd<>3) then goto f1;
move(hdr.adr,addr,16);
prt:=hdr.prt;
dat:=hdr.dat;
TCPlookConnected:=False;
End;

Function TCPlistenOnPort(var pipe:LongInt;siz:LongInt;var adr;var prt:LongInt):Boolean;
Label f1;
Var hdr:OneTCPcommandHeader;
Begin;
TCPlistenOnPort:=True;
hdr.cmd:=2;
move(siz,hdr.adr,4);
hdr.prt:=prt and $ffff;
if (pipeLineCreate(pipe,TCPprocessId,64,false)<>0) then exit;
if (pipeLineSend(pipe,hdr,sizeof(hdr))<>0) then exit;
f1:
relequish;
if (pipe=0) then exit;
if TCPlookConnected(pipe,adr,prt,siz) then goto f1;
TCPlistenOnPort:=False;
End;



Function UDPlistenOnPort(var pipe:LongInt;siz:LongInt;var adr;var prt:LongInt):Boolean;
Label f1;
Var hdr:OneTCPcommandHeader;
Begin;
UDPlistenOnPort:=True;
hdr.cmd:=4;
move(siz,hdr.adr,4);
hdr.prt:=prt and $ffff;
if (pipeLineCreate(pipe,TCPprocessId,siz,true)<>0) then exit;
if (pipeLineSend(pipe,hdr,sizeof(hdr))<>0) then exit;
f1:
relequish;
if (pipe=0) then exit;
if TCPlookConnected(pipe,adr,prt,siz) then goto f1;
UDPlistenOnPort:=False;
End;

Function UDPsendPacket(var pipe:LongInt;var adr;prt:LongInt;var buf;siz:LongInt):Boolean;
Const max=2048;
Var
  pck:record
    hdr:OneTCPcommandHeader;
    dat:array[1..max] of byte;
    end;
  hdr:OneTCPcommandHeader absolute pck;
Begin;
UDPsendPacket:=True;
if (siz>max) then exit;
hdr.cmd:=5;
move(adr,hdr.adr,16);
hdr.prt:=prt and $ffff;
move(buf,pck.dat,siz);
if (pipeLineSend(pipe,pck,siz+sizeof(hdr))<>0) then exit;
UDPsendPacket:=False;
End;

Function UDPreceivePacket(var pipe:LongInt;var adr;var prt:LongInt;var buf;var siz:LongInt):Boolean;
Label f1;
Var
  hdr:OneTCPcommandHeader absolute buf;
  buff:array[1..1] of byte absolute buf;
  i,o,p:LongInt;
Begin;
UDPreceivePacket:=True;
pipeLineStats(pipe,i,o,p);
if (o<sizeof(hdr)) then begin;
  if (i<>0) then exit;
  f1:
  pipeLineClose(pipe);
  pipe:=0;
  exit;
  end;
if (pipeLineRecv(pipe,buf,siz)<>0) then exit;
if (hdr.cmd<>5) then goto f1;
move(hdr.adr,adr,16);
prt:=hdr.prt;
dec(siz,sizeof(hdr));
move(buff[sizeof(hdr)+1],buff,siz);
UDPreceivePacket:=False;
End;



Function RTPopenConnection(var pipe:LongInt;siz:LongInt;var adr;var prt:LongInt):Boolean;
Label f1;
Var hdr:OneTCPcommandHeader;
Begin;
RTPopenConnection:=True;
hdr.cmd:=6;
move(adr,hdr.adr,sizeof(hdr.adr));
hdr.prt:=prt and $ffff;
if (pipeLineCreate(pipe,TCPprocessId,siz,true)<>0) then exit;
if (pipeLineSend(pipe,hdr,sizeof(hdr))<>0) then exit;
f1:
relequish;
if (pipe=0) then exit;
if TCPlookConnected(pipe,adr,prt,siz) then goto f1;
RTPopenConnection:=False;
End;

Function RTPbeginConnect(var pipe:LongInt;siz:LongInt;var adr;prt:LongInt):Boolean;
Var hdr:OneTCPcommandHeader;
Begin;
RTPbeginConnect:=True;
hdr.cmd:=6;
move(adr,hdr.adr,sizeof(hdr.adr));
hdr.prt:=prt and $ffff;
if (pipeLineCreate(pipe,TCPprocessId,siz,true)<>0) then exit;
if (pipeLineSend(pipe,hdr,sizeof(hdr))<>0) then exit;
RTPbeginConnect:=False;
End;



Function ProtoListenOnPort(var pipe:LongInt;siz:LongInt;var adr;prt:LongInt):Boolean;
Label f1;
Var hdr:OneTCPcommandHeader;
Begin;
ProtoListenOnPort:=True;
hdr.cmd:=7;
move(adr,hdr.adr,sizeof(hdr.adr));
hdr.prt:=prt and $ff;
if (pipeLineCreate(pipe,TCPprocessId,siz,true)<>0) then exit;
if (pipeLineSend(pipe,hdr,sizeof(hdr))<>0) then exit;
f1:
relequish;
if (pipe=0) then exit;
if TCPlookConnected(pipe,adr,prt,siz) then goto f1;
ProtoListenOnPort:=False;
End;
