{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc memory.inc}
{$sysinc hex.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Type
  OneDataRecord=record
    n:String;                   {name}
    s:LongInt;                  {size}
    d:array[1..1024] of byte;   {data}
    end;
Var
  entryDat:^array[1..1] of OneDataRecord;
  entryNum:LongInt;
  dataPort:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneDataRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
entryNum:=n;
entryDat:=p^;
ResizeMem:=False;
End;

Procedure ReadUpConfig(a:String);
Label f1,f2,f3,vege;
Const sep='---';
Var
  t:xtText;
  d:OneDataRecord;
  i:LongInt;
Begin;
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
a:=xtReadLn(t,255);
i:=pos(' ',a);
if (i<1) then i:=666;
dataPort:=BVal(copy(a,1,i-1));
entryNum:=0;
f1:
if xtEOF(t) then goto vege;
a:=xtReadLn(t,255);
if (a<>sep) then goto f1;
fillchar(d,sizeof(d),0);
d.n:=kicsi(xtReadLn(t,255));
f2:
if xtEOF(t) then goto f3;
a:=xtReadLn(t,255);
if (a<>sep) then begin;
  a:=a+#13#10;
  move(a[1],d.d[d.s+1],length(a));
  inc(d.s,length(a));
  goto f2;
  end;
f3:
if ResizeMem(entryNum+1) then immErr('out of memory!');
entryDat^[entryNum]:=d;
goto f1;
vege:
xtClose(t);
End;

Procedure sendString(a:string);
Begin;
a:=a+#13#10;
pipeLineSend(dataPort,a[1],length(a));
End;

Procedure sendBuffer(var d:OneDataRecord);
Begin;
pipeLineSend(dataPort,d.d,d.s);
End;


Function askUser(var a:String):Boolean;
Label f1,f2;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,t:LongInt;
Begin;
askUser:=True;
ab0:=0;
timer2start;
t:=CurrentTime;
f1:
relequish;
timer2start;
if (GetTimePast(t)>5) then exit;
i:=255-ab0;
if (pipeLineRecv(dataPort,ab[ab0+1],i)<>0) then i:=0;
inc(ab0,i);
for i:=1 to ab0 do if (ab[i]=13) then begin;
  ab0:=i-1;
  goto f2;
  end;
if (i>128) then goto f2;
goto f1;
f2:
askUser:=False;
End;



Label f1,f2;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('finger server v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

a:=GetAllParameters;
if (a='') then immErr('using: finger.code <config>');
ReadUpConfig(a);
if (dataPort=0) then dataPort:=79;

if pipeLineBegListen then immErr('failed to start listening!');

i:=dataPort;
if TCPlistenOnPort(i,4096,a,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+' port...');

BugOS_SignDaemoning;
f1:
relequish;
if (pipeLineGetIncoming(dataPort)<>0) then goto f1;
while TCPlookConnected(dataPort,a,o,i) do begin;
  relequish;
  if (dataPort=0) then begin;
    WriteLn('got buggy connection!');
    goto f1;
    end;
  end;
WriteLn('got request from '+ipAddr2string(a)+' '+BStr(o));
if askUser(a) then begin;
  sendString('timeout occured!');
  goto f2;
  end;
a:=kicsi(a);
for i:=1 to entryNum do if (entryDat^[i].n=a) then begin;
  sendBuffer(entryDat^[i]);
  goto f2;
  end;
sendString('invalid name requested!');
f2:
pipeLineClose(dataPort);
goto f1;
END.