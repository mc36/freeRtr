{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc hex.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const portMax=16;
Type
  oneDescriptorRecord=record
    pipe:LongInt;
    proc:LongInt;
    end;
Var
  portDat:array[1..portMax] of oneDescriptorRecord;
  addrSiz:LongInt;
  packMax:LongInt;
  packMin:LongInt;


Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Function conv2hex(i:LongInt):String;
Begin;
conv2hex:=byte2hextype(i shr 24)+byte2hextype(i shr 16)+byte2hextype(i shr 8)+byte2hextype(i);
End;

Function convAddr(var data):String;
Var
  d:array[1..1] of byte absolute data;
  i:LongInt;
  a:String;
Begin;
a:='';
for i:=1 to addrSiz do a:=a+'-'+byte2hextype(d[i]);
convAddr:=copy(a,2,666);
End;



Label f1;
Var
  a:String;
  i,o,p,q:LongInt;
  buf:array[1..1024*8] of byte;
BEGIN;
WriteLn('etherEmu v1.0, done by Mc at '#%date' '#%time'.');

addrSiz:=BVal(paramStr(1));
packMin:=BVal(paramStr(2));
packMax:=BVal(paramStr(3));
if (addrSiz<1) then addrSiz:=6;
if (packMax<1) then packMax:=1502;
if (packMin<1) then packMin:=48;
fillchar(portDat,sizeOf(portDat),0);

WriteLn('address size: '+BStr(addrSiz));
WriteLn('packet size: '+BStr(packMin)+'..'+BStr(packMax));

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
while (pipeLineGetIncoming(p)=0) do begin;
  pipeLineStats(p,q,i,i);
  BugOS_ProcessName(q,buf,i,i,o);
  if (o and $40=0) then begin; pipeLineClose(p);break; end;
  o:=0;
  for i:=1 to portMax do if (portDat[i].pipe=0) then begin; o:=i;break; end;
  if (o=0) then begin; pipeLineClose(p);break; end;
  portDat[o].pipe:=p;
  portDat[o].proc:=q;
  move(addrSiz,buf[1],sizeof(addrSiz));
  move(packMax,buf[5],sizeof(packMax));
  move(o,buf[9],sizeof(o));
  move(o,buf[13],sizeof(o));
  i:=17;
  fillchar(buf[i],addrSiz,0);buf[i+addrSiz-1]:=o;inc(i,addrSiz);
  fillchar(buf[i],addrSiz,$ff);inc(i,addrSiz);
  a:='etherEmu port #'+BStr(o);
  move(a[1],buf[i],sizeof(a));
  inc(i,length(a));
  buf[i]:=0;
  pipeLineSend(portDat[o].pipe,buf,i);
  WriteLn('port '+BStr(o)+' logged in!');
  end;
for q:=1 to portMax do begin;
  if (portDat[q].pipe=0) then continue;
  o:=sizeof(buf);
  if (pipeLineRecv(portDat[q].pipe,buf,o)<>0) then o:=0;
  if (o<1) then begin;
    pipeLineStats(portDat[q].pipe,o,i,i);
    if (o<>0) then continue;
    pipeLineClose(portDat[q].pipe);
    portDat[q].pipe:=0;
    WriteLn('port '+BStr(q)+' logged out!');
    continue;
    end;
  if (o<=addrSiz) then continue;
  p:=0;
  for i:=1 to addrSiz-1 do inc(p,buf[i]);
  if (p=0) then p:=buf[addrSiz] else p:=$ff;
  fillchar(buf,addrSiz,0);
  buf[addrSiz]:=q;
  i:=packMin+addrSiz-o;
  if (i>0) then begin;
    fillchar(buf[o+1],i,0);
    inc(o,i);
    end;
  if (p>=1) and (p<=portMax) then begin;
    pipeLineSend(portDat[p].pipe,buf,o);
    continue;
    end;
  for p:=1 to portMax do begin;
    if (p=q) then continue;
    if (portDat[p].pipe=0) then continue;
    pipeLineSend(portDat[p].pipe,buf,o);
    end;
  end;
relequish;
goto f1;
END.