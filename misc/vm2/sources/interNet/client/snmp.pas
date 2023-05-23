{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc bignum.inc}
{$sysinc random.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}

{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\kernel\tls\asn1hdr.inc}
{$include \sources\internet\kernel\tls\asn1num.inc}

{$include snmp.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Function GetNextWord(Var a:String):String;
Var i:Word;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
GetNextWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Function ReadLine:String;
Label f0,f1;
Var
  a:String;
  w:Word;
Begin;
f0:
a:='';
Write(#13'snmp>');
f1:
w:=ReadKey;
if (w and $fe00=0) then begin;{simple key}
  w:=w and $ff;
  if (w in [0,255,13,10,8,9]) then w:=ord(' ');
  if (length(a)>250) then goto f1;
  a:=a+chr(w);
  write(chr(w));
  goto f1;
  end;
case w of
  $8001:begin;{redraw}
    clrscr;
    goto f0;
    end;
  $8003:begin;{backspace}
    if (a='') then goto f1;
    Write(#8' '#8);
    a:=copy(a,1,length(a)-1);
    goto f1;
    end;
  $8004:begin;{enter}
    WriteLn('');
    ReadLine:=a;
    exit;
    end;
  $8005:begin;{escape}
    WriteLn('');
    goto f0;
    end;
  end;
goto f1;
End;

Var
  addr:OneTCPaddressRecord;
  pipe,port:LongInt;
  ver,typ:LongInt;
  pwd,oid:String;


Function doOneReq(var buffer):LongInt;
Label f1,f2,vege;
Var
  buf1:array[1..1] of byte absolute buffer;
  buf2:array[1..4096] of byte;
  retry,siz,myid,vr,tp,id,st,idx,tim:LongInt;
  i,o:LongInt;
  a,b:String;

Procedure addStr(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
move(ab[1],buf1[siz+1],ab0);
inc(siz,ab0);
End;

Begin;
siz:=0;
addStr('peer: '+ipAddr2string(addr)+' '+BStr(port)+#13);
addStr('req.ver: '+BStr(ver)+#13);
addStr('req.typ: '+BStr(typ)+#13);
addStr('req.pwd: '+pwd+#13);
addStr('req.oid: '+convertOid2str(oid)+#13);
doOneReq:=0;
retry:=16;
f1:
dec(retry);
if (retry<0) then begin;
  addStr('status: timeout!'#13);
  WriteLn('timeout!');
  goto vege;
  end;
myid:=random($fffffff);
WriteLn('sending to '+ipAddr2string(addr)+' '+BStr(port));
o:=createSNMPrequest(buf2,ver,typ,myid,pwd,oid);
UDPsendPacket(pipe,addr,port,buf2,o);
timer2start;
tim:=currentTime;
f2:
relequish;
timer2start;
o:=sizeof(buf2);
if UDPreceivePacket(pipe,a,i,buf2,o) then o:=0;
if (o<1) then begin;
  if (getTimePast(tim)>5) then goto f1;
  goto f2;
  end;
o:=parseSNMPresponse(buf2,o,vr,tp,id,st,idx,a,b);
if (o<0) then begin;
  WriteLn('got bad packet!');
  goto f2;
  end;
if (id<>myid) then begin;
  WriteLn('got bad id!');
  goto f2;
  end;
oid:=b;
addStr('status: done!'#13);
addStr('rep.ver: '+BStr(vr)+#13);
addStr('rep.typ: '+BStr(tp)+#13);
addStr('rep.pwd: '+a+#13);
addStr('rep.oid: '+convertOid2str(b)+#13);
addStr('rep.err: '+BStr(st)+#13);
addStr('rep.idx: '+BStr(idx)+#13);
vr:=0;
readASN1header(buf2,vr,tp,st,idx);
addStr('hdr.siz: '+BStr(vr)+#13);
addStr('hdr.typ: '+BStr(tp)+#13);
addStr('hdr.tag: '+BStr(st)+#13);
addStr('hdr.bin:');
for i:=1 to vr do addStr(' '+byte2hextype(buf2[i]));
addStr(#13);
addStr('dat.siz: '+BStr(o-vr)+#13);
addStr('dat.bin:');
for i:=vr+1 to o do addStr(' '+byte2hextype(buf2[i]));
addStr(#13);
vege:
doOneReq:=siz;
End;




Procedure doCommand(b:String);
Var
  bufB:array[1..4096] of byte;
  bufC:array[1..1] of char absolute bufB;
  i,o,p:LongInt;
  a:String;
  t:xtText;
Begin;
a:=kicsi(GetNextWord(b));
if (a='quit') then halt(0);
if (a='') then exit;
if (a='disconnect') then begin;
  pipeLineClose(pipe);
  pipe:=0;
  exit;
  end;
if (a='connect') then begin;
  pipeLineClose(pipe);
  pipe:=0;
  a:=GetNextWord(b);
  port:=BVal(GetNextWord(b));
  if (port=0) then port:=161;
  Write('resolving '+a+'...');
  DNSresolvePut(1,a);
  while (1=1) do begin;
    i:=DNSresolveGet(a,b);
    if (i=0) then begin; relequish;continue; end;
    if (i and $80=0) then break;
    WriteLn(' failed!');
    exit;
    end;
  WriteLn(' ok!');
  move(b,addr,sizeof(addr));
  i:=port;
  if UDPlistenOnPort(pipe,4096,b,i) then begin;
    WriteLn('failed to listen on port!');
    pipeLineClose(pipe);
    pipe:=0;
    exit;
    end;
  WriteLn('listening on '+ipAddr2string(b)+' '+BStr(i)+'...');
  WriteLn('will send to '+ipAddr2string(addr)+' '+BStr(port)+'...');
  exit;
  end;
if (a='help') or (a='?') then begin;
  WriteLn(' commands');
  WriteLn('~~~~~~~~~~');
  WriteLn('connect <name> [port]');
  WriteLn('disconnect');
  WriteLn('quit');
  WriteLn('version <number>');
  WriteLn('action <get/next>');
  WriteLn('pass <string>');
  WriteLn('oid <objid>');
  WriteLn('query [file]');
  exit;
  end;
if (a='version') then begin;
  ver:=BVal(b);
  if (ver<1) then ver:=1;
  WriteLn('will try version '+BStr(ver)+'...');
  exit;
  end;
if (a='action') then begin;
  a:=kicsi(b);
  if (a='get') then typ:=0 else typ:=1;
  if (typ=0) then a:='get' else a:='get-next';
  WriteLn('will do '+a+'...');
  exit;
  end;
if (a='pass') then begin;
  pwd:=b;
  WriteLn('will use '+pwd+' as password...');
  exit;
  end;
if (a='oid') then begin;
  a:=convertStr2oid(b);
  if (a<>'') then oid:=a;
  WriteLn('will ask '+convertOid2str(oid)+' object...');
  exit;
  end;
if (a='query') then begin;
  p:=doOneReq(bufB);
  xCreate(b);
  if (xtOpen(t,b,false)=0) then begin;
    xtWriteLn(t,'---');
    for i:=1 to p do xtWrite(t,bufC[i]);
    xtClose(t);
    end;
  for i:=1 to p do if (bufB[i]=13) then WriteLn('') else Write(bufC[i]);
  exit;
  end;

WriteLn('unknown command: '+a+' '+b);
end;




Label f1;
Var
  a:String;
  t:xtText;
BEGIN;
WriteLn('snmp client v1.0, done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');
if DNSstartResolver then immErr('failed to find dns process!');
if BigNumStartActions then immErr('failed to find bignum process!');

pipe:=0;
ver:=1;
typ:=1;
pwd:='public';
oid:='';

a:=GetAllParameters;
if (a='') then goto f1;
if (xtOpen(t,a,true)<>0) then goto f1;
while not xtEOF(t) do begin;
  a:=xtReadLn(t,255);
  doCommand(a);
  end;
xtClose(t);

f1:
WriteLn('');
a:=ReadLine;
doCommand(a);
goto f1;
END.