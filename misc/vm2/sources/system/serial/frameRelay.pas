{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const maxDlciNum=128;
Var
  devPipe:LongInt;
  lastSent:LongInt;
  lastRecv:LongInt;
  seqSent:LongInt;
  seqRecv:LongInt;
  fullReq:Boolean;
  frSwtch:Boolean;
  frProto:LongInt;  {1=ccitt, 2=ansi, 3=cisco}
  frConnD:array[1..maxDlciNum] of LongInt;
  frConnU:array[1..maxDlciNum] of LongInt;
  frConnN:LongInt;
  myName:String;


Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Procedure processParams;
Var
  a:String;
  i,o:LongInt;
Begin;
a:=kicsi(paramStr(2));
i:=0;
if (a='dte') then i:=1;
if (a='dce') then i:=2;
if (i<1) then immErr('invalid mode!');
frSwtch:=(i=2);
a:=kicsi(paramStr(3));
i:=0;
if (a='ccitt') then i:=1;
if (a='ansi') then i:=2;
if (a='cisco') then i:=3;
if (i<1) then immErr('invalid lmi type!');
frProto:=i;
frConnN:=0;
for i:=4 to paramCount do begin;
  o:=BVal(paramStr(i));
  if (o<1) or (o>=1023) then continue;
  inc(frConnN);
  frConnD[frConnN]:=o;
  frConnU[frConnN]:=0;
  end;
if (frConnN<1) then immErr('no dlci specified!');
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


Procedure placeDlciHeader(var buffer;dlci:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
buf[1]:=(dlci shr 4) shl 2;
buf[2]:=(dlci shl 4) or 1;
End;


Procedure sendStatus(fullStat:Boolean);
Var
  buf:array[1..2*1024] of byte;
  i,o,p:LongInt;

Procedure put(i:LongInt);
Begin;
inc(p);
buf[p]:=i;
End;

Begin;
case frProto of
  1:placeDlciHeader(buf,0);             {ccitt}
  2:placeDlciHeader(buf,0);             {ansi}
  3:placeDlciHeader(buf,1023);          {cisco}
  else exit;
  end;
buf[3]:=$03;                            {control}
if (frProto=3) then i:=9 else i:=8;
buf[4]:=i;                              {nlpid}
p:=4;
seqSent:=(seqSent+1) and $ff;
if (seqSent=0) then seqSent:=1;
WriteLn('sending status; mySeq='+BStr(seqSent)+' sawSeq='+BStr(seqRecv));
put($00);                               {call reference}
if frSwtch then i:=$7d else i:=$75;     {status/enquiry}
put(i);
if (frProto=2) then put($95);           {ansi lockshift}
if (frProto=1) then i:=$51 else i:=1;   {report}
put(i);
put($01);                               {report length}
if fullStat then i:=0 else i:=1;        {full/integrity}
put(i);
if (frProto=1) then i:=$53 else i:=3;   {alive}
put(i);
put($02);                               {alive length}
put(seqSent);
put(seqRecv);
if frSwtch and fullStat then for o:=1 to frConnN do begin;
  if (frProto=1) then i:=$57 else i:=7;         {pvcstat}
  put(i);
  if (frProto<>3) then begin;
    put($03);                                   {pvcstat length}
    i:=frConnD[o];
    put((i shr 4) and $3f);
    put((i shl 3) or $80);
    put($82);
    end else begin;
    put($06);                                   {pvcstat length}
    i:=frConnD[o];
    put(i shr 8);
    put(i);
    put($02);
    put($00);
    put($00);
    put($00);
    end;
  end;
pipeLineSend(devPipe,buf,p);
lastSent:=currentTime;
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
q:=((buf[1] shr 2) shl 4) or (buf[2] shr 4);
if (buf[2] and 1<>1) then begin;
  WriteLn('got invalid q922 address!');
  exit;
  end;
if (q<>0) and (q<>1023) then begin;
  for o:=1 to frConnN do if (frConnD[o]=q) then begin;
    pipeLineSend(frConnU[o],buf,p);
    exit;
    end;
  WriteLn('got for unknown dlci: '+BStr(q));
  exit;
  end;
if (buf[3]<>3) then begin;
  WriteLn('got invalid control field!');
  exit;
  end;
i:=-1;
case frProto of
  1:if (q=0) then i:=8;         {ccitt}
  2:if (q=0) then i:=8;         {ansi}
  3:if (q=1023) then i:=9;      {cisco}
  else exit;
  end;
if (i<0) then begin;
  WriteLn('got for reserved dlci!');
  exit;
  end;
if (buf[4]<>i) then begin;
  WriteLn('got invalid nlpid value!');
  exit;
  end;
q:=4;
if (get<>0) then begin;
  WriteLn('got invalid call reference value!');
  exit;
  end;
if frSwtch then i:=$75 else i:=$7d;     {status/enquiry}
if (get<>i) then begin;
  WriteLn('got invalid packet type!');
  exit;
  end;
if (frProto=2) then if (get<>$95) then begin;
  WriteLn('got invalid lockshift!');
  exit;
  end;
a:='; dlci:';
f1:
i:=get;
if (i<0) then goto f2;
o:=get;
if (o<0) or (q+o>p) then begin;
  a:='; truncated'+a;
  goto f2;
  end;
case i of
  $01,$51:begin; {report}
    case buf[q+1] of
      0:begin; a:='; full'+a;fullReq:=True; end;
      1:begin; a:='; int'+a;fullReq:=False; end;
      end;
    end;
  $03,$53:begin; {alive}
    seqRecv:=buf[q+1];
    i:=buf[q+2];
    a:='; peerSeq='+BStr(seqRecv)+' sawSeq='+BStr(i)+a;
    end;
  $07,$57:begin; {pvcstat}
    if (frProto<>3) then begin;
      i:=((buf[q+1] and $3f) shl 4) or ((buf[q+2] and $78) shr 3);
      end else begin;
      i:=ReadWordMSB(buf[q+1]);
      end;
    a:=a+' '+BStr(i);
    if (buf[q+3] and 2=0) then a:=a+'-';
    end;
  end;
inc(q,o);
goto f1;
f2:
WriteLn('got status'+a);
lastRecv:=currentTime;
if frSwtch then sendStatus(fullReq);
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
  for i:=frConnN downto 1 do if (frConnU[i]=0) then p:=i;
  if (p<1) then begin; pipeLineClose(q);continue; end;
  frConnU[p]:=q;
  i:=frConnD[p];
  WriteLn('dlci '+BStr(i)+' logged in!');
  a:='dlci '+BStr(i)+' on '+myName;
  a:='12341234'#0#0#0#0#0#0#0#0+chr(i shr 8)+chr(i)+#255#255+a+#0;
  i:=2;move(i,a[1],sizeof(i));
  i:=1500;move(i,a[5],sizeof(i));
  pipeLineSend(q,a[1],length(a));
  end;
for q:=1 to frConnN do if (frConnU[q]<>0) then begin;
  p:=sizeof(buf);
  if (pipeLineRecv(frConnU[q],buf,p)<>0) then p:=0;
  if (p>0) then begin;
    placeDlciHeader(buf,frConnD[q]);
    pipeLineSend(devPipe,buf,p);
    doUpper:=True;
    continue;
    end;
  pipeLineStats(frConnU[q],p,i,o);
  if (p<>0) then continue;
  WriteLn('dlci '+BStr(frConnD[q])+' logged out!');
  pipeLineClose(frConnU[q]);
  frConnU[q]:=0;
  end;
End;



Procedure doKeepalive;
Begin;
if (getTimePast(lastRecv)>16*60) then immErr('remote possibly dead!');
if frSwtch then exit;
if (getTimePast(lastSent)>10) then sendStatus(seqSent and 3=0);
End;



Label f1;
BEGIN;
WriteLn('frameRelay v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<4) then begin;
  WriteLn('using: fr.code <process> <mode> <lmitype> <dlci> [dlci]');
  WriteLn('modes: dte, dce');
  WriteLn('types: ansi, ccitt, cisco');
  Halt(1);
  end;

processParams;
seqSent:=0;
seqRecv:=0;
timer2start;
lastSent:=0;
lastRecv:=currentTime;
openDevice(ParamStr(1));
fullReq:=True;

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
doKeepalive;
while doLower do;
while doUpper do;
goto f1;
END.