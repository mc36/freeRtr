{$define kilo}
{$define crc}

{$heap 31k}
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

{$ifdef crc}
{$include crc16.inc}
{$else}
{$include chksum.inc}
{$endif}

{$include xmodem.inc}



Procedure doSend;
Label f1,f2;
Var
  buf:array[1..1024] of byte;
  p,q,r,s:LongInt;
  i,o:LongInt;
  a:String;
  f:xFile;

procedure red;
begin;
o:=s-p;
fillchar(buf,OnePacketSize,controlCharacterSUB);
if (o>OnePacketSize) then o:=OnePacketSize;
xBlockRead(f,buf,o);
r:=16;
end;

procedure snd;
begin;
if (OnePacketSize<512) then i:=controlCharacterSOH else i:=controlCharacterSTX;
xmod_sendPck(i,q,o,buf);
end;

Begin;
WriteLn('sending '+path+'...');
if (xOpen(f,path,xGenFilMod_r)<>0) then begin;
  WriteLn('error opening file!');
  exit;
  end;
s:=xFileSize(f);
WriteLn('going to send '+BStr(s)+' bytes...');

r:=16;
f1:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
Write('header'#13);
xmod_wait(1);
i:=sizeof(buf);
if (pipeLineRecv(pipe,buf,i)<>0) then i:=0;
p:=0;o:=0;
for i:=1 to i do case buf[i] of
  controlCharacterACK:inc(p);
  controlCharacterNAK:inc(o);
  controlCharacterCAN:begin;
    WriteLn('header: remote aborted!');
    exit;
    end;
  $43:inc(o);
  end;
if (p<1) then begin;
  dec(r);
  if (o<1) then begin;
    WriteLn('header: timeout...');
    goto f1;
    end;
  WriteLn('header: sending...');
  fillchar(buf,sizeof(buf),0);
  o:=0;
  for i:=1 to length(path) do if (path[i]='\') then o:=i;
  a:=copy(path,o+1,255)+#0+BStr(s)+' 0';
  move(a[1],buf,length(a));
  o:=128;
  q:=0;
  snd;
  goto f1;
  end;
WriteLn('header: acked, sending data...');

p:=0;
q:=1;
red;
if (o>0) then begin
  o:=OnePacketSize;
  snd;
  end;
f2:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
Write(BStr(p)+#13);
i:=xmod_recvReq;
if (i<0) then begin;
  WriteLn(BStr(p)+': timeout...');
  dec(r);
  goto f2;
  end;
if (i=controlCharacterNAK) or (i=$43) then begin;
  o:=OnePacketSize;
  snd;
  WriteLn(BStr(p)+': nak received, resending...');
  dec(r);
  goto f2;
  end;
if (i=controlCharacterCAN) then begin;
  WriteLn(BStr(p)+': remote aborted!');
  exit;
  end;
if (i<>controlCharacterACK) then begin;
  WriteLn(BStr(p)+': invalid char received, flushing...');
  xmod_flush(3);
  dec(r);
  goto f2;
  end;
inc(q);
inc(p,o);
red;
if (o>0) then begin;
  o:=OnePacketSize;
  snd;
  goto f2;
  end;
xClose(f);
xmod_sendReq(controlCharacterEOT);
WriteLn('file sent successfully, closing session...');

xmod_flush(1);
fillchar(buf,sizeof(buf),0);
o:=128;
q:=0;
snd;
i:=xmod_recvReq;
if (i<>controlCharacterACK) then WriteLn('final ack not received!');

WriteLn('successfully finished!');
End;



Procedure doReceive;
Label f1,f2,f3,f4;
Var
  buf:array[1..1024] of byte;
  reack:Boolean;
  i,o,p,q,r,s,m:LongInt;
  a,b:String;
  f:xFile;

procedure wrt;
begin;
xBlockWrite(f,buf,o);
reack:=false;
r:=16;
end;

Begin;

f1:
WriteLn('--- receiving header...');
r:=16;
f2:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
xmod_sendReq($43);
Write('header'#13);
a:=xmod_recvPck(i,o,buf);
if (o<0) then begin;
  WriteLn('header: '+a+', flushing...');
  xmod_flush(3);
  dec(r);
  goto f2;
  end;
if (i<>0) then begin;
  WriteLn('header: got invalid sequence number, flushing...');
  xmod_flush(3);
  dec(r);
  goto f2;
  end;
xmod_sendReq(controlCharacterACK);
p:=0;
for i:=1 to o do if (buf[i]<>0) then p:=i;
if (p<1) then begin;
  WriteLn('session successfully finished!');
  exit;
  end;
if (o>255) then o:=255;
b[0]:=chr(o);
move(buf,b[1],o);
i:=pos(#0,b);
a:=copy(b,1,i-1);
b:=copy(b,i+1,255);
i:=pos(#32,b);
m:=BVal(copy(b,1,i-1));
b:=copy(b,i+1,255);
o:=0;
for i:=1 to length(a) do if (a[i]='/') then o:=i;
a:=copy(a,o+1,255);
a:=path+a;
WriteLn('--- receiving '+a+'...');
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
  end else begin;
  WriteLn('continuing not available, skipping...');
  exit;
  end;
WriteLn('going to receive '+BStr(m)+' bytes...');
s:=0;
xmod_sendReq($43);
o:=0;
q:=1;
wrt;
f3:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
Write(BStr(s)+#13);
a:=xmod_recvPck(i,o,buf);
if (o<0) then begin;
  f4:
  WriteLn(BStr(s)+': '+a+', flushing...');
  xmod_flush(3);
  xmod_sendReq(controlCharacterNAK);
  dec(r);
  goto f3;
  end;
if (o=0) then begin;
  if (s<m) then begin; a:='got eof packet';goto f4; end;
  xmod_sendReq(controlCharacterACK);
  if (m>0) and (m<s) then s:=m;
  xSeek(f,s);
  xTruncate(f);
  xClose(f);
  WriteLn(BStr(s)+' bytes received successfully!');
  goto f1;
  end;
if (i=q and $ff) then begin;
  xmod_sendReq(controlCharacterACK);
  wrt;
  inc(q);
  inc(s,o);
  goto f3;
  end;
if (i<>(q-1) and $ff) then begin;
  WriteLn(BStr(s)+': got packet #'+BStr(i)+', excepting '+BStr(q and $ff)+'...');
  xmod_flush(3);
  dec(r);
  goto f3;
  end;
WriteLn(BStr(s)+': got last packet again...');
xmod_flush(3);
if reack then begin;
  WriteLn('trying to re-ack...');
  xmod_sendReq(controlCharacterACK);
  end;
dec(r);
reack:=true;
goto f3;
End;




Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('ymodem file transfer v1.0, done by Mc at '#%date' '#%time'.');

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