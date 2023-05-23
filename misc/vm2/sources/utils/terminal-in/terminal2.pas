Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Label f1;
Var
  buf:array[1..512] of byte;
  p1,p2,s,u:LongInt;
  i,o,p:LongInt;
  w:Word;
BEGIN;
WriteLn('terminal (input) filter v1.0, done by Mc at '#%date' '#%time'.');
BugOS_MyProcessInfo(i,o,i);

i:=pipeLineCreate(p1,o,65536,false);
inc(i,pipeLineCreate(p2,o,65536,false));
if (i<>0) then immErr('error creating pipeline!');
pipeLineStats(p2,i,i,s);

while (1=1) do begin;
  relequish;
  pipeLineStats(p1,i,o,p);
  if (i=0) then immErr('remote closed pipeline!');
  if (o>=sizeof(o)*2) then break;
  end;

i:=sizeof(screenMaxX);
pipeLineRecv(p1,screenMaxX,i);
i:=sizeof(screenMaxY);
pipeLineRecv(p1,screenMaxY,i);
u:=1;
startEmulator;

f1:
relequish;
pipeLineStats(p2,i,o,p);
if (i=0) then immErr('remote closed pipeline!');
if (p>=s) and (u>0) then begin;
  pipeLineSend(p2,screenPosX,sizeof(screenPosX));
  pipeLineSend(p2,screenPosY,sizeof(screenPosY));
  pipeLineSend(p2,screenData,screenMaxX*screenMaxY*2);
  u:=0;
  end;
while (1=1) do begin;
  i:=sizeof(w);
  pipeLineRecv(p2,w,i);
  if (i<>sizeof(w)) then break;
  gotLocalKey(w);
  inc(u);
  end;
pipeLineStats(p1,i,o,p);
if (i=0) then immErr('remote closed pipeline!');
if (outputSize>0) then begin;
  if (pipeLineSend(p1,outputData,outputSize)=0) then outputSize:=0;
  end;
while (1=1) do begin;
  o:=sizeof(buf);
  pipeLineRecv(p1,buf,o);
  if (o<1) then break;
  for i:=1 to o do gotRemoteChar(buf[i]);
  inc(u);
  end;
goto f1;
END.