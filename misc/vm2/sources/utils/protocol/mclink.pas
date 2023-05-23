{$heap 15k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc param.inc}
{$sysinc memory.inc}
{$sysinc controlchars.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Var
  pipe:LongInt;
  path:String;
  mode:LongInt; {1-overwrite, 2-skip, 3-continue}

{$include chksum.inc}
{$include crc16.inc}
{$include mclink.inc}





Procedure doSend;
Label f1;
Var
  buf:array[1..1024] of byte;
  p,q,r,s:LongInt;
  i,o:LongInt;
  b:Boolean;
  a:String;
  f:xFile;
Begin;
WriteLn('sending '+path+'...');
if (xOpen(f,path,xGenFilMod_r)<>0) then begin;
  WriteLn('error opening file!');
  exit;
  end;
s:=xFileSize(f);
WriteLn('going to send '+BStr(s)+' bytes...');
a:=initiateStr;
pipeLineSend(pipe,a[1],length(a));
o:=0;
for i:=1 to length(path) do if (path[i]='\') then o:=i;
path:=copy(path,o+1,255)+#0+BStr(s);

p:=-1;
r:=16;
f1:
Write(BStr(p)+#13);
a:=recvRequest(i,o);
if (i=command_req) and (o>0) then begin;
  r:=16;
  q:=(o-1)*sizeof(buf);
  if (q<>p) then WriteLn(BStr(p)+': seeking to '+BStr(q)+'...');
  p:=q+sizeof(buf);
  xSeek(f,q);
  fillchar(buf,sizeof(buf),0);
  i:=s-q;
  if (i>sizeof(buf)) then i:=sizeof(buf);
  xBlockRead(f,buf,i);
  sendPacket(command_dat,o,buf);
  goto f1;
  end;
if (i<0) then begin;
  Write(BStr(p)+a+'!');
  if (i<-1) then begin;
    Write(#8', flushing!');
    flushLine(2);
    end;
  WriteLn('');
  dec(r);
  goto f1;
  end;
if (i=command_str) and (o=0) then begin;
  r:=16;
  WriteLn(BStr(p)+': got start...');
  sendReqest(command_str,0);
  p:=0;
  goto f1;
  end;
if (i=command_fin) and (o=0) then begin;
  r:=16;
  WriteLn(BStr(p)+': got finish...');
  sendReqest(command_fin,0);
  exit;
  end;
if (i=command_req) and (o=-1) then begin;
  r:=16;
  fillchar(buf,sizeof(buf),0);
  move(path[1],buf,length(path));
  sendPacket(command_dat,-1,buf);
  goto f1;
  end;

WriteLn(BStr(q)+': got invalid packet type!');
dec(r);
goto f1;
End;





Procedure doReceive;
Label f1,f2,f3,f4,f5,f6;
Var
  buf:array[1..1024] of byte;
  i,o,p,q,r,s:LongInt;
  a:String;
  f:xFile;

function getNextPart:String;
var
  a:string;
  c:longint;
begin;
a:='';
while (i<sizeof(buf)) do begin;
  inc(i);
  c:=buf[i];
  if (c in [0,32,255]) then break;
  a:=a+chr(c);
  end;
getNextPart:=a;
end;

Function doFlush(ps:Boolean):Boolean;
Begin;
if (i>=0) then begin;
  doFlush:=false;
  exit;
  end;
if ps then Write(BStr(p)+': ');
doFlush:=True;
Write(a+'!');
if (i<-1) then begin;
  Write(#8', flushing!');
  flushLine(2);
  end;
WriteLn('');
dec(r);
End;

Begin;
WriteLn('receiving header...');
r:=10;
f1:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
sendReqest(command_req,-1);
a:=recvPacket(i,o,buf);
if doFlush(false) then goto f1;
if (i<>command_dat) or (o<>-1) then begin;
  WriteLn('got invalid packet type!');
  goto f1;
  end;
i:=0;
a:=getNextPart;
a:=path+a;
WriteLn('receiving '+a+'...');
s:=BVal(getNextPart);
WriteLn('going to receive '+BStr(s)+' bytes...');

i:=xCreate(a);
if (mode=2) and (i<>0) then begin;
  WriteLn('file already exists, skipping...');
  exit;
  end;
if (xOpen(f,a,xGenFilMod_rw)<>0) then begin;
  WriteLn('error opening file!');
  exit;
  end;
if (mode=1) then begin;
  xTruncate(f);
  p:=0;
  end else begin;
  p:=xFileSize(f) and $fffffc00;
  WriteLn('going to continue at '+BStr(p)+'...');
  end;
xSeek(f,p);

r:=16;
f2:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
sendReqest(command_str,0);
a:=recvRequest(i,o);
if doFlush(false) then goto f2;
if (i<>command_str) or (o<>0) then begin;
  WriteLn('got invalid packet type!');
  goto f2;
  end;

r:=16;
f3:
if (p>=s) then goto f5;
Write(BStr(p)+#13);
q:=(p shr 10)+1;
sendReqest(command_req,q);
f4:
a:=recvPacket(i,o,buf);
if doFlush(true) then goto f3;
if (i<>command_dat) or (o<>q) then begin;
  WriteLn('got invalid packet type!');
  dec(r);
  goto f4;
  end;
i:=s-p;
if (i>sizeof(buf)) then i:=sizeof(buf);
xBlockWrite(f,buf,i);
inc(p,i);
r:=16;
goto f3;

f5:
WriteLn(BStr(s)+' bytes received, closing session...');
xSeek(f,s);
xTruncate(f);
xClose(f);
r:=8;
f6:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
sendReqest(command_fin,0);
a:=recvRequest(i,o);
if doFlush(false) then goto f6;
if (i<>command_fin) or (o<>0) then begin;
  WriteLn('got invalid packet type!');
  goto f6;
  end;

WriteLn('successfully finished!');
End;





Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('mclink file transfer v1.0, done by Mc at '#%date' '#%time'.');

BugOS_MyProcessInfo(i,o,i);
if (pipeLineCreate(pipe,o,65536,false)<>0) then begin;
  WriteLn('error creating pipeline!');
  exit;
  end;

a:=GetAllParameters;
o:=0;
case lowCase(a[1]) of
  'r':o:=1;
  't':o:=2;
  end;
case lowCase(a[2]) of
  'o':mode:=1;
  's':mode:=2;
  'c':mode:=3;
  end;
path:=copy(a,3,255);
if (o=0) or (mode=0) then begin;
  WriteLn('using: protocol.code <direction><existing><pathname>');
  WriteLn(' mode: t=transmit, r=receive');
  WriteLn('exist: c=continue, o=overwrite, s=skip');
  exit;
  end;

if (o=1) then doReceive else doSend;
END.