{$heap 7k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc crt.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include gpsparser1.inc}
{$include gpsparser_nmea.inc}

Label f1;
Var
  pipe:LongInt;
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Procedure addLin(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
move(ab[1],buf[p+1],ab0);
inc(p,ab0);
inc(p);buf[p]:=13;
inc(p);buf[p]:=10;
End;

Procedure genRep;

function st(v,s:longint):string;
var a:string;
begin;
a:=bstr(v);
while (length(a)<s) do a:='0'+a;
st:=a;
end;

function coord2str(i:longint):string;
begin;
coord2str:=st(i div 10000000,3)+'.'+st(i mod 10000000,7);
end;

Begin;
p:=0;
if gpsCoordFix then a:='valid' else a:='invalid';
if (getTimePast(gpsChanged)>5) then a:='not communicating';
addLin('status: '+a);
addLin('satellites: '+BStr(gpsSatellites));
addLin('longitude: '+coord2str(gpsCoordX));
addLin('latitude: '+coord2str(gpsCoordY));
addLin('altitude: '+BStr(gpsCoordZ)+' m');
addLin('precision: '+BStr(gpsPrecise)+' m');
a:=bstr(gpsTime);
while (length(a)<6) do a:='0'+a;
i:=length(a);
a:=copy(a,1,i-4)+':'+copy(a,i-3,2)+':'+copy(a,i-1,2);
addLin('utc time: '+a);
a:=bstr(gpsDate);
while (length(a)<6) do a:='0'+a;
i:=length(a);
a:=copy(a,1,i-4)+'-'+copy(a,i-3,2)+'-'+copy(a,i-1,2);
addLin('utc date: '+a);
addLin('speed: '+BStr(gpsSpeed)+' km/h');
addLin('course: '+BStr(gpsCourse)+' ø');
End;

BEGIN;
WriteLn('gps parser v1.0, done by Mc at '#%date' '#%time'.');
o:=BVal(paramStr(2));
if (o<1) then begin;
  BugOS_MyProcessInfo(p,i,i);
  BugOS_ProcessName(p,buf,i,i,i);
  move(buf,a,sizeof(a));
  o:=0;
  for i:=1 to ab0 do if (ab[i]=92) then o:=i;
  a:=copy(a,o+1,666);
  i:=BugOS_findProcNam(a);
  if (i=p) then immErr('using: gps.code <process> <port>');
  WriteLn('querying '+BStr(i)+'...');
  pipeLineCreate(pipe,i,65536,false);
  for i:=1 to 16 do relequish;
  p:=sizeof(buf);
  if (pipeLineRecv(pipe,buf,p)<>0) then p:=0;
  ab0:=0;
  for i:=1 to p do begin;
    inc(ab0);
    ab[ab0]:=buf[i];
    if (ab0<250) then continue;
    Write(a);
    ab0:=0;
    end;
  Write(a);
  exit;
  end;
a:=paramStr(1);
p:=BVal(a);
if (p=0) then p:=BugOS_findProcNam(a);
if (p=0) then immErr('process not found!');
pipeLineCreate(i,p,4096,true);
pipeLineCreate(pipe,p,4096,false);
dec(o);
pipeLineSend(i,o,sizeof(o));
pipeLineSend(i,pipe,sizeof(pipe));

pipeLineBegListen;
BugOS_SignDaemoning;

gpsReset;
ab0:=0;
f1:
relequish;
timer2start;
p:=sizeof(buf);
if (pipeLineRecv(pipe,buf,p)<>0) then p:=0;
for i:=1 to p do gpsParser(buf[i]);
p:=0;
while (pipeLineGetIncoming(o)=0) do begin;
  if (p=0) then genRep;
  pipeLineSend(o,buf,p);
  pipeLineClose(o);
  end;
goto f1;
END.