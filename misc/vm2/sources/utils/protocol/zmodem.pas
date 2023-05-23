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

{$include crc16.inc}
{$include crc32.inc}
{$include zmodem.inc}


Procedure printCapa(dat,msk:LongInt;cap:String);
Begin;
cap:=' '+cap;
if (dat and msk=0) then cap:='''t'+cap;
WriteLn('- can'+cap);
End;

Function handleGenPack(var buffer):Boolean;
Var buf:array[1..1] of byte absolute buffer;
Begin;
handleGenPack:=false;
case buf[1] of
  14:begin; {zchallenge}
    buf[1]:=3;
    sendOneHeader(buf,false,true);
    handleGenPack:=true;
    end;
  17:begin; {zfreecnt}
    buf[1]:=3;
    buf[2]:=0;
    buf[3]:=0;
    buf[4]:=0;
    buf[5]:=0;
    sendOneHeader(buf,false,true);
    handleGenPack:=true;
    end;
  16:begin; {zcan}
    buf[1]:=8;
    end;
  7:begin; {zabort}
    buf[1]:=8;
    buf[2]:=0;
    buf[3]:=0;
    buf[4]:=0;
    buf[5]:=0;
    sendOneHeader(buf,false,true);
    end;
  12:begin; {zferr}
    buf[1]:=8;
    buf[2]:=0;
    buf[3]:=0;
    buf[4]:=0;
    buf[5]:=0;
    sendOneHeader(buf,false,true);
    end;
  18:begin; {zcommand}
    buf[1]:=15;
    buf[2]:=0;
    buf[3]:=0;
    buf[4]:=0;
    buf[5]:=0;
    sendOneHeader(buf,false,true);
    handleGenPack:=true;
    end;
  end;
End;





Procedure doSend;
Label f1,f2,f3,f4,f5,f6,f7,f8,f9;
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
output_putBin($72);
output_putBin($7a);
output_putBin(13);

r:=8;
f1:
fillchar(buf,sizeof(buf),0);
WriteLn('sending '+dumpOneHeader(buf)+'...');
sendOneHeader(buf,false,true);
f2:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
if sawAbort then begin;
  WriteLn('remote aborted session, transfer failed!');
  exit;
  end;
i:=recvOneHeader(buf,b,5);
if (i<>0) then begin;
  dec(r);
  goto f1;
  end;
WriteLn('got '+dumpOneHeader(buf)+'...');
if handleGenPack(buf) then goto f2;
case buf[1] of
  8:begin; {zfin}
    WriteLn('remote cancelled, transfer failed!');
    exit;
    end;
  1:; {zrinit}
  else goto f2;
  end;
WriteLn('remote capabilities:');
i:=buf[5];
hasCrc32:=(i and 32<>0);
hasFulDp:=(i and 1<>0);
hasOvrIo:=(i and 2<>0);
if (i and 64<>0) then begin;
  for o:=0 to 31 do maskOutChar(o);
  maskOutChar(127);
  end;
if (i and 128<>0) then move(quoteMask[0],quoteMask[$80],32);
printCapa(i,1,'communicate in full duplex');
printCapa(i,2,'receive during disk io');
printCapa(i,4,'send break signal');
printCapa(i,8,'decrypt');
printCapa(i,16,'uncompress');
printCapa(i,32,'use 32 bit crc');
i:=not i;
printCapa(i,64,'receive control characters');
printCapa(i,128,'receive 8 bit characters');
i:=buf[4];
printCapa(i,1,'accept variable length headers');
printCapa(i,8,'specify window size');
printCapa(i,16,'specify characters to quote');

r:=16;
f3:
fillchar(buf,sizeof(buf),0);
buf[1]:=4;
buf[4]:=7;
buf[5]:=3;
WriteLn('sending '+dumpOneHeader(buf)+'...');
sendOneHeader(buf,hasCrc32,false);
o:=0;
for i:=1 to length(path) do if (path[i]='\') then o:=i;
a:=copy(path,o+1,255);
a:=a+#0+BStr(s)+' 0 0 0 1 '+BStr(s)+' 0'#0;
sendOneFrame(a[1],length(a),107,hasCrc32);
f4:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
if sawAbort then begin;
  WriteLn('remote aborted session, transfer failed!');
  exit;
  end;
i:=recvOneHeader(buf,b,5);
if (i<>0) then begin;
  dec(r);
  goto f4;
  end;
WriteLn('got '+dumpOneHeader(buf)+'...');
if handleGenPack(buf) then goto f4;
case buf[1] of
  8:begin; {zfin}
    WriteLn('remote cancelled, transfer failed!');
    exit;
    end;
  1:goto f3; {zrinit}
  9:; {zrpos}
  5:goto f8; {zskip}
  else goto f4;
  end;
p:=ReadLongLSB(buf[2]);
r:=16;

f5:
buf[1]:=10;
WriteLongLSB(buf[2],p);
sendOneHeader(buf,hasCrc32,false);
WriteLn(BStr(p)+': sending '+dumpOneHeader(buf)+'...');
f6:
Write(BStr(p)+#13);
xSeek(f,p);
o:=s-p;
if (o<1) then begin;
  sendOneFrame(buf,0,104,hasCrc32);
  goto f8;
  end;
if (o>sizeof(buf)) then o:=sizeof(buf);
xBlockRead(f,buf,o);
inc(p,o);
if hasOvrIo then i:=105 else i:=107;
if (o<sizeof(buf)) then i:=107;
sendOneFrame(buf,o,i,hasCrc32);
if not hasOvrIo then goto f7;
if (o<sizeof(buf)) then goto f7;
if not inbuf_check then goto f6;
i:=inbuf_getBin(0);
if (i<0) then goto f6;
inbuf_ungetOne;
sendOneFrame(buf,0,107,hasCrc32);
f7:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
if sawAbort then begin;
  WriteLn('remote aborted session, transfer failed!');
  exit;
  end;
i:=recvOneHeader(buf,b,10);
if (i<>0) then begin;
  dec(r);
  goto f7;
  end;
WriteLn(BStr(p)+': got '+dumpOneHeader(buf)+'...');
if handleGenPack(buf) then goto f7;
case buf[1] of
  8:begin; {zfin}
    WriteLn('remote cancelled, transfer failed!');
    exit;
    end;
  1,5:goto f8; {zrinit,zskip}
  9:begin; {zrpos}
    p:=ReadLongLSB(buf[2]);
    r:=16;
    goto f5;
    end;
  3:begin; {zack}
    i:=ReadLongLSB(buf[2]);
    if (i<>p) then goto f7;
    r:=16;
    goto f5;
    end;
  end;
dec(r);
goto f7;

f8:
r:=4;
buf[1]:=11;
WriteLongLSB(buf[2],s);
sendOneHeader(buf,hasCrc32,false);
WriteLn('sending '+dumpOneHeader(buf)+'...');
f9:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
if sawAbort then begin;
  WriteLn('remote aborted session, transfer failed!');
  exit;
  end;
i:=recvOneHeader(buf,b,10);
if (i<>0) then begin;
  dec(r);
  goto f9;
  end;
WriteLn('got '+dumpOneHeader(buf)+'...');
if handleGenPack(buf) then goto f9;
case buf[1] of
  8:begin; {zfin}
    WriteLn('remote cancelled transfer!');
    output_putBin($4f);
    output_putBin($4f);
    outbuf_flush;
    exit;
    end;
  1,5:begin; {zrinit,zskip}
    fillchar(buf,sizeof(buf),0);
    buf[1]:=8;
    sendOneHeader(buf,hasCrc32,false);
    WriteLn('sending '+dumpOneHeader(buf)+'...');
    goto f9;
    end;
  9:begin; {zrpos}
    p:=ReadLongLSB(buf[2]);
    r:=16;
    goto f5;
    end;
  3:begin; {zack}
    i:=ReadLongLSB(buf[2]);
    if (i<>p) then goto f7;
    r:=16;
    goto f5;
    end;
  end;
dec(r);
goto f9;
End;





Procedure doReceive;
Label f1,f2,f3,f4,f5,f6,f7;
Var
  buf:array[1..8192] of byte;
  i,o,p,q,r,s:LongInt;
  a:String;
  f:xFile;

function getNextPart:String;
var
  a:string;
  c:longint;
begin;
a:='';
while (i<o) do begin;
  inc(i);
  c:=buf[i];
  if (c in [0,32,255]) then break;
  a:=a+chr(c);
  end;
getNextPart:=a;
end;

procedure writeBuffer;
begin;
if (o<1) then exit;
xBlockWrite(f,buf,o);
inc(p,o);
end;

procedure sendZack;
begin;
buf[1]:=3;
WriteLongLSB(buf[2],p);
sendOneHeader(buf,hasCrc32,true);
WriteLn(BStr(p)+': sending '+dumpOneHeader(buf)+'...');
end;

Begin;
goto f1;
f4:
fillchar(buf,sizeof(buf),0);
buf[1]:=5;
sendOneHeader(buf,hasCrc32,true);
WriteLn('sending '+dumpOneHeader(buf)+'...');

f1:
WriteLn('--- receiving header...');
r:=16;
f2:
buf[1]:=1;
i:=$23;
WriteLongMSB(buf[2],i);
sendOneHeader(buf,hasCrc32,true);
WriteLn('sending '+dumpOneHeader(buf)+'...');
f3:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
if sawAbort then begin;
  WriteLn('remote aborted session, transfer failed!');
  exit;
  end;
i:=recvOneHeader(buf,hasCrc32,5);
if (i<>0) then begin;
  dec(r);
  goto f2;
  end;
WriteLn('got '+dumpOneHeader(buf)+'...');
if handleGenPack(buf) then goto f2;
case buf[1] of
  8:begin; {zfin}
    fillchar(buf,sizeof(buf),0);
    buf[1]:=8;
    sendOneHeader(buf,hasCrc32,true);
    WriteLn('sending '+dumpOneHeader(buf)+'...');
    exit;
    end;
  0:goto f2; {zrqinit}
  4:; {zfile}
  else begin;
    dec(r);
    goto f3;
    end;
  end;
o:=sizeof(buf);
i:=recvOneFrame(buf,o,5,hasCrc32);
if not (i in [104..107]) then begin;
  WriteLn('invalid frame received!');
  goto f2;
  end;
i:=0;
a:=getNextPart;
s:=0;
for p:=1 to length(a) do if (a[p]='/') then s:=p;
a:=copy(a,s+1,255);
a:=path+a;
WriteLn('--- receiving '+a+'...');
s:=BVal(getNextPart);
WriteLn('going to receive '+BStr(s)+' bytes...');
WriteLn('modified (seconds since 1970jan1): '+getNextPart);
WriteLn('file permissions (unix): '+getNextPart);
WriteLn('serial number: '+getNextPart);
WriteLn('files remaining: '+getNextPart);
WriteLn('bytes remaining: '+getNextPart);
WriteLn('file type: '+getNextPart);

i:=xCreate(a);
if (mode=2) and (i<>0) then begin;
  WriteLn('file already exists, skipping...');
  goto f4;
  end;
if (xOpen(f,a,xGenFilMod_rw)<>0) then begin;
  WriteLn('error opening file!');
  goto f4;
  end;
if (mode=1) then begin;
  xTruncate(f);
  p:=0;
  end else begin;
  p:=xFileSize(f);
  WriteLn('going to continue at '+BStr(p)+'...');
  end;

xSeek(f,p);
r:=16;
f5:
buf[1]:=9;
WriteLongLSB(buf[2],p);
sendOneHeader(buf,hasCrc32,true);
WriteLn(BStr(p)+': sending '+dumpOneHeader(buf)+'...');
f6:
Write(BStr(p)+#13);
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
if sawAbort then begin;
  WriteLn('remote aborted session, transfer failed!');
  exit;
  end;
i:=recvOneHeader(buf,hasCrc32,10);
if (i<>0) then begin;
  dec(r);
  goto f5;
  end;
WriteLn(BStr(p)+': got '+dumpOneHeader(buf)+'...');
if handleGenPack(buf) then goto f2;
case buf[1] of
  8:begin; {zfin}
    WriteLn('remote cancelled, transfer failed!');
    exit;
    end;
  4:begin; {zfile}
    o:=sizeof(buf);
    i:=recvOneFrame(buf,o,5,hasCrc32);
    goto f6;
    end;
  11:begin; {zeof}
    i:=ReadLongLSB(buf[2]);
    if (p<i) then goto f5;
    xClose(f);
    WriteLn('successfully finished!');
    goto f1;
    end;
  10:; {zdata}
  else begin;
    dec(r);
    goto f6;
    end;
  end;
i:=ReadLongLSB(buf[2]);
if (i<>p) then goto f5;
r:=16;
f7:
Write(BStr(p)+#13);
o:=sizeof(buf);
i:=recvOneFrame(buf,o,5,hasCrc32);
case i of
  104:begin; {crce - header}
    writeBuffer;
    goto f6;
    end;
  105:begin; {crcg - continue}
    writeBuffer;
    goto f7;
    end;
  106:begin; {crcq - zack, continue}
    writeBuffer;
    sendZack;
    goto f7;
    end;
  107:begin; {crcw - zack, header}
    writeBuffer;
    sendZack;
    goto f6;
    end;
  end;
WriteLn(BStr(p)+': invalid frame received!');
inbuf_flush2;
goto f5;
End;





Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('zmodem file transfer v1.0, done by Mc at '#%date' '#%time'.');

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

fillchar(quoteMask,sizeof(quoteMask),0);
output_bufSiz:=0;
input_bufSiz:=0;
input_bufPos:=0;
canCount:=0;
sawAbort:=false;
hasCrc32:=false;
hasFulDp:=false;
hasOvrIo:=false;
crc32build;
maskOutChar(controlCharacterCAN);
maskOutChar(controlCharacterDLE);
maskOutChar(controlCharacterDC1);
maskOutChar(controlCharacterDC3);

if (o=1) then doReceive else doSend;
END.