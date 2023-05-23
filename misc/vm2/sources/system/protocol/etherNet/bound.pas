{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}

Var
  buf:array[1..1024*4] of byte;
  addrSiz:LongInt;
  packSiz:LongInt;
  addrDat:String;
  pipeD:array[1..16] of LongInt;
  pipeN:LongInt;
  pipeC:LongInt;
  pipeU:LongInt;

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Function conv2hex(i:LongInt):String;
Begin;
conv2hex:=byte2hextype(i shr 24)+byte2hextype(i shr 16)+byte2hextype(i shr 8)+byte2hextype(i);
End;

Function openIface(a:String):LongInt;
Var i,o,p:LongInt;
Begin;
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(p,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(p));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(p,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
openIface:=p;
move(buf[1],addrSiz,sizeof(addrSiz));
move(buf[5],packSiz,sizeof(packSiz));
WriteLn('address size: '+BStr(addrSiz));
WriteLn('packet size: '+BStr(packSiz));
move(buf[9],i,sizeof(i));
WriteLn('io base: '+conv2hex(i));
move(buf[13],i,sizeof(i));
WriteLn('mem base: '+conv2hex(i));
o:=17;
addrDat[0]:=chr(addrSiz*2);
move(buf[o],addrDat[1],length(addrDat));
Write('station address: ');
for i:=1 to addrSiz do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
Write('broadcast address: ');
for i:=1 to addrSiz do begin;
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
End;


Label f1;
Var
  a,b:String;
  i,o,p:LongInt;
BEGIN;
WriteLn('interface bounding v1.0, done by Mc at '#%date' '#%time'.');
pipeN:=paramCount;
if (pipeN<2) then immErr('using: bound.code <process> <process> [process...]');
b:='bound of';
for i:=1 to pipeN do begin;
  a:=paramStr(i);
  b:=b+' '+a;
  WriteLn('interface #'+BStr(i)+' - '+a);
  pipeD[i]:=openIface(a);
  end;
pipeC:=0;

Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(pipeU)<>0) do relequish;
pipeLineEndListen;
a:='12341234'#0#0#0#0#0#0#0#0+addrDat+b+#0;
i:=addrSiz;move(i,a[1],sizeof(i));
i:=packSiz;move(i,a[5],sizeof(i));
pipeLineSend(pipeU,a[1],length(a));
WriteLn(' done!');

f1:
i:=sizeof(buf);
if (pipeLineRecv(pipeU,buf,i)<>0) then i:=0;
if (i>0) then begin;
  pipeC:=(pipeC mod pipeN)+1;
  pipeLineSend(pipeD[pipeC],buf,i);
  goto f1;
  end;
for p:=1 to pipeN do begin;
  i:=sizeof(buf);
  if (pipeLineRecv(pipeD[p],buf,i)<>0) then i:=0;
  if (i>0) then pipeLineSend(pipeU,buf,i);
  end;

relequish;
goto f1;
END.