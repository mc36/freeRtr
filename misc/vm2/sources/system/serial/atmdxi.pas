{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}

Const maxVciNum=128;
Var
  devPipe:LongInt;
  atmConnD:array[1..maxVciNum] of LongInt;
  atmConnU:array[1..maxVciNum] of LongInt;
  atmConnN:LongInt;
  myName:String;


Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Procedure processParams;
Var
  a:String;
  i,o,p:LongInt;
Begin;
atmConnN:=0;
p:=2;
while (p<paramCount) do begin;
  i:=BVal(paramStr(p));
  inc(p);
  o:=BVal(paramStr(p));
  inc(p);
  if (i<0) or (i>15) or (o<0) or (o>63) then continue;
  {format: ccpppp00 cccc0001}
  o:=((i and $f) shl 10) or ((o and $30) shl 10) or ((o and $f) shl 4) or 1;
  inc(atmConnN);
  atmConnD[atmConnN]:=o;
  atmConnU[atmConnN]:=0;
  end;
if (atmConnN<1) then immErr('no vci specified!');
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
myName:=a;
writeln('device name: "'+a+'"');
End;




Function doLower:Boolean;
Label f1,f2;
Var
  buf:array[1..4*1024] of byte;
  i,o,p,q:LongInt;
  a:String;

Function get:LongInt;
Begin;
get:=-1;
inc(q);
if (q>p) then exit;
get:=buf[q];
End;

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
q:=ReadWordMSB(buf);
for o:=1 to atmConnN do if (atmConnD[o]=q) then begin;
  pipeLineSend(atmConnU[o],buf,p);
  exit;
  end;
WriteLn('got for unknown vci: '+BStr(q));
End;



Function doUpper:Boolean;
Var
  buf:array[1..4*1024] of byte;
  i,o,p,q:LongInt;
  a:String;
Begin;
doUpper:=False;
while (pipeLineGetIncoming(q)=0) do begin;
  p:=0;
  for i:=atmConnN downto 1 do if (atmConnU[i]=0) then p:=i;
  if (p<1) then begin; pipeLineClose(q);continue; end;
  atmConnU[p]:=q;
  i:=atmConnD[p];
  WriteLn('vci '+BStr(i)+' logged in!');
  a:='vci '+BStr(i)+' on '+myName;
  a:='12341234'#0#0#0#0#0#0#0#0+chr(i shr 8)+chr(i)+#255#255+a+#0;
  i:=2;move(i,a[1],sizeof(i));
  i:=1500;move(i,a[5],sizeof(i));
  pipeLineSend(q,a[1],length(a));
  end;
for q:=1 to atmConnN do if (atmConnU[q]<>0) then begin;
  p:=sizeof(buf);
  if (pipeLineRecv(atmConnU[q],buf,p)<>0) then p:=0;
  if (p>0) then begin;
    WriteWordMSB(buf,atmConnD[q]);
    pipeLineSend(devPipe,buf,p);
    doUpper:=True;
    continue;
    end;
  pipeLineStats(atmConnU[q],p,i,o);
  if (p<>0) then continue;
  WriteLn('vci '+BStr(atmConnD[q])+' logged out!');
  pipeLineClose(atmConnU[q]);
  atmConnU[q]:=0;
  end;
End;






Label f1;
BEGIN;
WriteLn('atmdxi v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<3) then immErr('using: atmdxi.code <process> <vpi> <vci> [<vpi> <vci>]');

processParams;
openDevice(ParamStr(1));

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
while doLower do;
while doUpper do;
goto f1;
END.