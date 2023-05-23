Var
  prAdr,myAdr:OneTCPaddressRecord;
  prPrt,myPrt:LongInt;
  tcpPipe,uprCmd,uprDat:LongInt;
  prGrup:LongInt;
  recLen:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;



Procedure WaitForUpperLayer;
Label f1;
Var
  i,o,p:LongInt;
  a:String;
Begin;
Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
f1:
while (pipeLineGetIncoming(uprCmd)<>0) do relequish;
i:=1;
pipeLineSend(uprCmd,i,sizeof(i));
for i:=1 to 16 do relequish;
if (pipeLineGetIncoming(uprDat)<>0) then begin;
  pipeLineClose(uprCmd);
  pipeLineClose(uprDat);
  goto f1;
  end;
pipeLineEndListen;
for i:=1 to 16 do relequish;
i:=sizeof(a);
pipeLineRecv(uprCmd,a,i);
for i:=1 to 16 do relequish;
i:=sizeof(a);
pipeLineRecv(uprCmd,a,i);
WriteLn(' done!');
End;





Procedure releq2upper;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
o:=sizeof(buf);
if (pipeLineRecv(uprDat,buf,o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(uprDat,o,i,i);
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
pipeLineSend(uprDat,buf,recLen);
recLen:=0;
End;



Procedure doOneConnection;
Label f1;
Begin;
recLen:=0;
f1:
relequish;
releq2conn;
processCommandPipe(uprCmd);
releq2upper;
goto f1;
End;
