{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}

Var
  ethAddr:array[1..6] of byte;
  devAdrS:LongInt;
  devPipe:LongInt;
  uppPipe:LongInt;
  myName:String;



Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;


Procedure dumpPacket(var buffer;siz:LongInt;act:String);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
Write(act+' '+BStr(siz)+':');
for i:=1 to siz do write(' '+byte2hextype(buf[i]));
WriteLn('');
End;


Procedure openDevice(a:String);
Var
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
Begin;
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(devPipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(devPipe));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(devPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],p,sizeof(p));
devAdrS:=p;
WriteLn('address size: '+BStr(p));
move(buf[5],i,sizeof(i));
WriteLn('packet size: '+BStr(i));
o:=17;
Write('station address: ');
for i:=1 to p do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
Write('broadcast address: ');
for i:=1 to p do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
a:='';
while (buf[o]<>0) do begin;
  a:=a+chr(buf[o]);
  inc(o);
  end;
writeln('device name: "'+a+'"');
myName:='etherBridge on '+a;
End;


Procedure parseMyMac(b:String);
Var
  i,o:LongInt;
  a:String;
Begin;
kicserel('-','',b);
for o:=1 to sizeof(ethAddr) do begin;
  a:=copy(b,1,2);
  b:=copy(b,3,666);
  ethAddr[o]:=BVal('$'+a);
  end;
if (b<>'') then immErr('failed to parse mac address!');
Write('my mac address: ');
for i:=1 to sizeof(ethAddr) do write(byte2hextype(ethAddr[i])+'-');
WriteLn(#8' ');
End;




Function doLower:Boolean;
Var
  buf:array[1..4*1024] of byte;
  i,o,p:LongInt;
Begin;
doLower:=False;
p:=sizeof(buf);
if (pipeLineRecv(devPipe,buf,p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(devPipe,p,i,o);
  if (p=0) then immErr('lower level closed connection!');
  exit;
  end;
doLower:=True;
i:=ReadWordMSB(buf[devAdrS+1]);
if (i<>$6558) then begin;
  WriteLn('got invalid type ($'+byte2hextype(i shr 8)+byte2hextype(i)+') packet!');
  exit;
  end;
dec(p,devAdrS+8);
if (p<6) then begin;
  WriteLn('got too short packet!');
  exit;
  end;
pipeLineSend(uppPipe,buf[devAdrS+9],p);
End;



Function doUpper:Boolean;
Var
  buf:array[1..4*1024] of byte;
  i,o,p:LongInt;
  a:String;
Begin;
doUpper:=False;
if (uppPipe=0) then begin;
  if (pipeLineGetIncoming(p)<>0) then exit;
  uppPipe:=p;
  a:='12341234'#0#0#0#0#0#0#0#0+#0#0#0#0#0#0+#255#255#255#255#255#255+myName+#0;
  i:=sizeof(ethAddr);move(i,a[1],sizeof(i));
  i:=1500;move(i,a[5],sizeof(i));
  move(ethAddr,a[17],sizeof(ethAddr));
  pipeLineSend(uppPipe,a[1],length(a));
  WriteLn('upper level opened connection!');
  end;
p:=sizeof(buf);
if (pipeLineRecv(uppPipe,buf[devAdrS+9],p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(uppPipe,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper level closed connection!');
  pipeLineClose(uppPipe);
  uppPipe:=0;
  exit;
  end;
fillchar(buf,devAdrS,$ff);
WriteWordMSB(buf[devAdrS+1],$6558);
move(buf[devAdrS+9],buf[devAdrS+3],sizeof(ethAddr));
move(ethAddr,buf[devAdrS+9],sizeof(ethAddr));
pipeLineSend(devPipe,buf,p+devAdrS+8);
doUpper:=True;
End;







Label f1;
BEGIN;
WriteLn('ethernet bridger v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>2) then immErr('using: bridge.code <process> <mymac>');

uppPipe:=0;
openDevice(ParamStr(1));
parseMyMac(ParamStr(2));
BugOS_SignDaemoning;
pipeLineBegListen;

f1:
relequish;
while doLower do;
while doUpper do;
goto f1;
END.