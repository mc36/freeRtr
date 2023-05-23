Var
  prAdr,myAdr:OneTCPaddressRecord;
  prPrt,myPrt:LongInt;
  tcpPipe,uprPipe:LongInt;
  prGrup:LongInt;
  recLen:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;



Procedure WaitForUpperLayer;
Var
  i,o,p:LongInt;
  a:String;
Begin;
Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(uprPipe)<>0) do relequish;
pipeLineEndListen;
a:='stun with '+ipAddr2string(prAdr)+' '+BStr(prPrt);
BugOS_MyProcessInfo(i,o,p);
i:=i xor p xor o;
p:=((i shr 24) xor (i shr 16) xor (i shr 8) xor i) and $ff;
a:='12341234'#0#0#0#0#0#0#0#0+chr(p)+#255+a+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1500;move(i,a[5],sizeof(i));
pipeLineSend(uprPipe,a[1],length(a));
WriteLn(' done!');
End;





Procedure releq2upper;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
o:=sizeof(buf);
if (pipeLineRecv(uprPipe,buf,o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(uprPipe,o,i,i);
  if (o=0) then immErr('upper layer exited!');
  exit;
  end;
sendDataPack(tcpPipe,prGrup,o,buf);
End;



Procedure releq2conn;
Var
  buf:array[1..4*1024] of byte;
  i,o,p:LongInt;
Begin;
if (recLen<1) then begin;
  recLen:=receiveHeader(tcpPipe,i);
  if (recLen>0) then exit;
  case i of
    -1:exit;
    -2:immErr('remote closed connection!');
    -3:immErr('protocol error happened!');
    else immErr('unknown error!');
    end;
  exit;
  end;
if (pipeLineStats(tcpPipe,p,i,o)<>0) then p:=0;
if (p=0) then immErr('remote closed connection!');
if (i<recLen) then exit;
i:=recLen;
if (pipeLineRecv(tcpPipe,buf,i)<>0) then i:=0;
if (i<>recLen) then immErr('error reading tcp pipe!');
pipeLineSend(uprPipe,buf,recLen);
recLen:=0;
End;



Procedure doOneConnection;
Label f1;
Begin;
recLen:=0;
f1:
relequish;
releq2conn;
releq2upper;
goto f1;
End;
