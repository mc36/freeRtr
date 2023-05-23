{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc filesys.inc}
{$sysinc hex.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Function conv2hex(i:LongInt):String;
Begin;
conv2hex:=byte2hextype(i shr 24)+byte2hextype(i shr 16)+byte2hextype(i shr 8)+byte2hextype(i);
End;

Label f1;
Var
  addrSiz:LongInt;
  packSiz:LongInt;
  a,b:String;
  buf:array[1..1024*8] of byte;
  pip:LongInt;
  i,o,p:LongInt;
  logFile:xFile;

Procedure add2log;
Begin;
buf[1]:=0;
move(p,buf[2],sizeof(p));
move(currentTime,buf[6],sizeof(currentTime));
xBlockWrite(logFile,buf,p+9);
End;

Function getNextAddr:String;
Var
  i:LongInt;
  a:String;
Begin;
a:='';
for i:=1 to addrSiz do begin;
  a:=a+'-'+byte2hextype(buf[o]);
  inc(o);
  end;
getNextAddr:=copy(a,2,255);
End;

BEGIN;
a:=ParamStr(1);
if (paramStr(2)='') then immErr('using: etherLogger.code <process> <logfile>');
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(pip,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(pip));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(pip,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],addrSiz,sizeof(addrSiz));
move(buf[5],packSiz,sizeof(packSiz));
WriteLn('address size: '+BStr(addrSiz));
WriteLn('packet size: '+BStr(packSiz));
move(buf[9],i,sizeof(i));
WriteLn('io base: '+conv2hex(i));
move(buf[13],i,sizeof(i));
WriteLn('mem base: '+conv2hex(i));
o:=17;
b:=getNextAddr;
WriteLn('station address: '+b);
WriteLn('broadcast address: '+getNextAddr);
a:='';
while (buf[o]<>0) do begin;
  a:=a+chr(buf[o]);
  inc(o);
  end;
writeln('device name: "'+a+'"');
b:=a+' ('+b+')';

a:=paramStr(2);
if (xCreate(a)<>0) then a:='';
if (xOpen(logFile,a,xGenFilMod_rw)<>0) then immErr('error opening log file!');
writeLn('logging to '+a+'...');

timer2start;
i:=1;
xBlockWrite(logFile,i,sizeof(i));
p:=length(b);
move(b[1],buf[10],p);
add2log;

f1:
relequish;
timer2start;
p:=sizeof(buf);
if (pipeLineRecv(pip,buf[9],p)<>0) then goto f1;
if (p<1) then goto f1;
add2log;
goto f1;
END.