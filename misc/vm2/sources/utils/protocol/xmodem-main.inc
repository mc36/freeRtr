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
xmod_sendPck(i,q,OnePacketSize,buf);
end;

Begin;
WriteLn('sending '+path+'...');
if (xOpen(f,path,xGenFilMod_r)<>0) then begin;
  WriteLn('error opening file!');
  exit;
  end;
s:=xFileSize(f);
WriteLn('going to send '+BStr(s)+' bytes...');
p:=0;
q:=1;
red;
f1:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
Write(BStr(p)+#13);
i:=xmod_recvReq;
if (i<0) then begin;
  WriteLn(BStr(p)+': timeout...');
  dec(r);
  goto f1;
  end;
if (i=controlCharacterNAK) or (i=$43) then begin;
  snd;
  WriteLn(BStr(p)+': nak received, resending...');
  dec(r);
  goto f1;
  end;
if (i=controlCharacterCAN) then begin;
  WriteLn(BStr(p)+': can received, transfer aborted!');
  exit;
  end;
if (i<>controlCharacterACK) then begin;
  WriteLn(BStr(p)+': invalid char received, flushing...');
  xmod_flush(3);
  dec(r);
  goto f1;
  end;
inc(q);
inc(p,o);
red;
if (o>0) then begin;
  snd;
  goto f1;
  end;
xmod_sendReq(controlCharacterEOT);
i:=xmod_recvReq;
if (i<>controlCharacterACK) then WriteLn('final ack not received!');
xClose(f);
WriteLn('successfully finished!');
End;




Procedure doReceive;
Label f1,f2;
Var
  buf:array[1..1024] of byte;
  reack:Boolean;
  i,o,p,q,r,s:LongInt;
  a:String;
  f:xFile;

procedure wrt;
begin;
xBlockWrite(f,buf,o);
reack:=false;
r:=16;
end;

Begin;
WriteLn('receiving '+path+'...');
i:=xCreate(path);
if (mode=2) and (i<>0) then begin;
  WriteLn('file already exists, skipping...');
  exit;
  end;
if (xOpen(f,path,xGenFilMod_rw)<>0) then begin;
  WriteLn('error opening file!');
  exit;
  end;
if (mode=1) then begin;
  xTruncate(f);
  end else begin;
  WriteLn('continuing not available, skipping...');
  exit;
  end;
s:=0;
{$ifdef crc}
xmod_sendReq($43);
{$else}
xmod_sendReq(controlCharacterNAK);
{$endif}
o:=0;
q:=1;
wrt;
f1:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
Write(BStr(s)+#13);
a:=xmod_recvPck(i,o,buf);
if (o<0) then begin;
  WriteLn(BStr(s)+': '+a+', flushing...');
  xmod_flush(3);
  xmod_sendReq(controlCharacterNAK);
  dec(r);
  goto f1;
  end;
if (o=0) then goto f2;
if (i=q and $ff) then begin;
  xmod_sendReq(controlCharacterACK);
  wrt;
  inc(q);
  inc(s,o);
  goto f1;
  end;
if (i<>(q-1) and $ff) then begin;
  WriteLn(BStr(s)+': got packet #'+BStr(i)+', excepting '+BStr(q and $ff)+'...');
  xmod_flush(3);
  dec(r);
  goto f1;
  end;
WriteLn(BStr(s)+': got last packet again...');
xmod_flush(3);
if reack then begin;
  WriteLn('trying to re-ack...');
  xmod_sendReq(controlCharacterACK);
  end;
dec(r);
reack:=true;
goto f1;

f2:
xmod_sendReq(controlCharacterACK);
xClose(f);
WriteLn(BStr(s)+' bytes received.');
WriteLn('successfully finished!');
End;




Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('xmodem file transfer v1.0, done by Mc at '#%date' '#%time'.');

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