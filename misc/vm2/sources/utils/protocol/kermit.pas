{$heap 63k}
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
{$include ccitt16.inc}
{$include kermit.inc}






Procedure doSend;
Label f1,f2,f3,f4,f5,f6,f7;
Var
  buf1:array[1..2048] of byte;
  buf2:array[1..8192] of byte;
  i,o,p,q,r,s,t,u:LongInt;
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
r:=16;
q:=0;

f1:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
buf1[1]:=126;                   {packet max length}
buf1[2]:=62;                    {timeout}
buf1[3]:=32;                    {number of pads}
buf1[4]:=64;                    {pad character}
buf1[5]:=currEoPc+32;           {packet terminator}
buf1[6]:=currCtPf;              {control prefix}
buf1[7]:=89;                    {binary prefix}
buf1[8]:=$33;                   {check type}
buf1[9]:=126;                   {repeat prefix}
buf1[10]:=78;                   {capabilities}
buf1[11]:=32;                   {window size}
buf1[12]:=42;                   {window size high}
buf1[13]:=106;                  {window size low}
WriteLn('sending send-init...');
sendOnePacket(buf1,13,q,83);    {send-init}
if recvOnePacket(5,buf1,p,o,i) then begin;
  WriteLn('timeout!');
  dec(r);
  goto f1;
  end;
if (i<>89) then begin;
  WriteLn('got invalid packet type!');
  dec(r);
  goto f1;
  end;
if (o<>q and $3f) then begin;
  WriteLn('got invalid sequence number!');
  dec(r);
  goto f1;
  end;
WriteLn('got ack...');
inc(q);
if (p>=1) then begin;
  currPack:=buf1[1]-32;
  WriteLn('packet size: '+BStr(currPack));
  end;
if (p>=2) then begin;
  i:=buf1[2]-32;
  WriteLn('timeout: '+BStr(i)+' seconds');
  end;
if (p>=3) then begin;
  currPadN:=buf1[3]-32;
  WriteLn('number of pads: '+BStr(currPadN));
  end;
if (p>=4) then begin;
  currPadC:=buf1[4] xor $40;
  WriteLn('pad character: #'+BStr(currPadC));
  end;
if (p>=5) then begin;
  currEoPc:=buf1[5]-32;
  WriteLn('packet terminator: #'+BStr(currEoPc));
  end;
if (p>=6) then begin;
  currCtPf:=buf1[6];
  WriteLn('control prefix: #'+BStr(currCtPf));
  end;
if (p>=7) then begin;
  i:=buf1[7];
  if (i in [33..62,96..126]) then currBnPf:=i else currBnPf:=-1;
  WriteLn('binary prefix: #'+BStr(currBnPf));
  end;
if (p>=8) then begin;
  currChck:=buf1[8]-$30;
  if (currChck<1) then currChck:=1;
  if (currChck>3) then currChck:=3;
  case currChck of
    1:a:='checksum8 truncated to 6 bit';
    2:a:='checksum8 truncated to 12 bit';
    3:a:='ccitt fcs16';
    else a:='?';
    end;
  WriteLn('block check type: '+BStr(currChck)+'='a);
  end;
if (p>=9) then begin;
  i:=buf1[9];
  if (i in [33..62,96..126]) then currRpPf:=i else currRpPf:=-1;
  WriteLn('repeat prefix: #'+BStr(currRpPf));
  end;
o:=10;r:=0;
if (p>=10) then begin;
  r:=buf1[o]-32;
  while (buf1[o] and 1<>0) and (o<=p) do inc(p);
  end;
if (r and 32<>0) then begin;
  WriteLn('locking shift: enabled');
  currShIn:=15;
  currShOt:=14;
  end;
if (r and 16<>0) then WriteLn('extra long packet: remote supports');
if (r and 8<>0) then WriteLn('file attributes: enabled');
if (r and 4<>0) then begin;
  inc(o);
  currWind:=(buf1[o]-32) and $1f;
  WriteLn('window size: '+BStr(currWind));
  end;
if (r and 2<>0) then begin;
  i:=buf1[o+1]*95+buf1[o+2]-3072;
  inc(o,2);
  WriteLn('extended packet size: '+BStr(i));
  if (i>currPack) then currPack:=i;
  end;
dec(currPack,4);
if (currPack<16) then currPack:=16;
if (currPack>1024) then currPack:=1024;

r:=16;
f2:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
o:=0;
for i:=1 to length(path) do if (path[i]='\') then o:=i;
a:=copy(path,o+1,255);
i:=length(a);
move(a[1],buf1,i);
enQuoteOnePacket(buf1,buf2,i,i);
WriteLn('sending file-header...');
sendOnePacket(buf2,i,q,70);     {file-header}
if recvOnePacket(5,buf1,p,o,i) then begin;
  WriteLn('timeout!');
  dec(r);
  goto f2;
  end;
if (i<>89) then begin;
  WriteLn('got invalid packet type!');
  dec(r);
  goto f2;
  end;
if (o<>q and $3f) then begin;
  WriteLn('got invalid sequence number!');
  dec(r);
  goto f2;
  end;
WriteLn('got ack...');
inc(q);

p:=0;
r:=16;
f3:
Write(BStr(p)+#13);
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
t:=s-p;
if (currBnPf<0) then i:=currPack shr 1 else i:=currPack shr 2;
if (t>i) then t:=i;
if (t<1) then begin;
  r:=16;
  goto f6;
  end;
xSeek(f,p);
xBlockRead(f,buf1,t);
enQuoteOnePacket(buf1,buf2,t,i);
sendOnePacket(buf2,i,q,68);     {data}
f4:
if recvOnePacket(10,buf1,u,o,i) then begin;
  WriteLn(BStr(p)+': timeout!');
  dec(r);
  if (r and 1=0) then goto f3;
  goto f4;
  end;
if (i=78) then begin;
  dec(r);
  if (o=(q+1) and $3f) then begin;
    WriteLn(BStr(p)+': got new nak!');
    goto f5;
    end;
  if (o<>q and $3f) then begin;
    WriteLn(BStr(p)+': got invalid sequence number!');
    goto f4;
    end;
  WriteLn(BStr(p)+': got old nak!');
  if (r and 1=0) then goto f3;
  goto f4;
  end;
if (i<>89) then begin;
  WriteLn(BStr(p)+': got invalid packet type!');
  dec(r);
  goto f4;
  end;
if (o=(q-1) and $3f) then begin;
  WriteLn(BStr(p)+': got old ack!');
  if (r and 1=0) then goto f3;
  goto f4;
  end;
if (o<>q and $3f) then begin;
  WriteLn(BStr(p)+': got invalid sequence number!');
  dec(r);
  goto f4;
  end;
f5:
inc(q);
inc(p,t);
r:=16;
goto f3;

f6:
WriteLn('sending end of file...');
sendOnePacket(buf1,0,q,90);     {end of file}
if recvOnePacket(5,buf1,p,o,i) then begin;
  WriteLn('timeout!');
  dec(r);
  goto f6;
  end;
if (i<>89) then begin;
  WriteLn('got invalid packet type!');
  dec(r);
  goto f6;
  end;
if (o<>q and $3f) then begin;
  WriteLn('got invalid sequence number!');
  dec(r);
  goto f6;
  end;
WriteLn('got ack...');
inc(q);
r:=4;

f7:
WriteLn('sending break transfer...');
sendOnePacket(buf1,0,q,66);     {break transfer}
if recvOnePacket(5,buf1,p,o,i) then begin;
  WriteLn('timeout!');
  dec(r);
  goto f7;
  end;
if (i<>89) then begin;
  WriteLn('got invalid packet type!');
  dec(r);
  goto f7;
  end;
if (o<>q and $3f) then begin;
  WriteLn('got invalid sequence number!');
  dec(r);
  goto f7;
  end;
WriteLn('got ack...');
inc(q);

WriteLn('successfully finished!');
End;





Procedure doReceive;
Label f1,f2,f3,f4,f5;
Var
  buf1:array[1..2048] of byte;
  buf2:array[1..8192] of byte;
  i,o,p,q,r,s:LongInt;
  a:String;
  f:xFile;
Begin;
r:=16;
s:=0;
WriteLn('--- receiving header...');
f1:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
sendOnePacket(buf1,0,s,78);     {nak}
if recvOnePacket(5,buf1,p,o,i) then begin;
  WriteLn('timeout!');
  dec(r);
  goto f1;
  end;
if (i<>83) then begin;
  WriteLn('got invalid packet type!');
  dec(r);
  goto f1;
  end;
if (o<>s and $3f) then begin;
  WriteLn('got invalid sequence number!');
  dec(r);
  goto f1;
  end;
WriteLn('got send-init...');
inc(s);
if (p>=1) then begin;
  currPack:=buf1[1]-32;
  WriteLn('packet size: '+BStr(currPack));
  end;
if (p>=2) then begin;
  i:=buf1[2]-32;
  WriteLn('timeout: '+BStr(i)+' seconds');
  end;
if (p>=3) then begin;
  currPadN:=buf1[3]-32;
  WriteLn('number of pads: '+BStr(currPadN));
  end;
if (p>=4) then begin;
  currPadC:=buf1[4] xor $40;
  WriteLn('pad character: #'+BStr(currPadC));
  end;
if (p>=5) then begin;
  currEoPc:=buf1[5]-32;
  WriteLn('packet terminator: #'+BStr(currEoPc));
  end;
if (p>=6) then begin;
  currCtPf:=buf1[6];
  WriteLn('control prefix: #'+BStr(currCtPf));
  end;
if (p>=7) then begin;
  i:=buf1[7];
  if (i in [33..62,96..126]) then currBnPf:=i else currBnPf:=-1;
  WriteLn('binary prefix: #'+BStr(currBnPf));
  end;
if (p>=8) then begin;
  currChck:=buf1[8]-$30;
  if (currChck<1) then currChck:=1;
  if (currChck>3) then currChck:=3;
  case currChck of
    1:a:='checksum8 truncated to 6 bit';
    2:a:='checksum8 truncated to 12 bit';
    3:a:='ccitt fcs16';
    else a:='?';
    end;
  WriteLn('block check type: '+BStr(currChck)+'='a);
  end;
if (p>=9) then begin;
  i:=buf1[9];
  if (i in [33..62,96..126]) then currRpPf:=i else currRpPf:=-1;
  WriteLn('repeat prefix: #'+BStr(currRpPf));
  end;
q:=10;r:=0;
if (p>=10) then begin;
  r:=buf1[q]-32;
  while (buf1[q] and 1<>0) and (q<=p) do inc(p);
  end;
if (r and 32<>0) then begin;
  WriteLn('locking shift: enabled');
  currShIn:=15;
  currShOt:=14;
  end;
if (r and 16<>0) then WriteLn('extra long packet: remote supports');
if (r and 8<>0) then WriteLn('file attributes: enabled');
if (r and 4<>0) then begin;
  inc(q);
  currWind:=(buf1[q]-32) and $1f;
  WriteLn('window size: '+BStr(currWind));
  end;
if (r and 2<>0) then begin;
  i:=buf1[q+1]*95+buf1[q+2]-3072;
  inc(q,2);
  WriteLn('extended packet size: '+BStr(i));
  if (i>currPack) then currPack:=i;
  end;
if (currPack<16) then currPack:=16;
if (currPack>1024) then currPack:=1024;

r:=16;
f2:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
WriteLn('sending ack...');
buf1[1]:=126;                   {packet max length}
buf1[2]:=62;                    {timeout}
buf1[3]:=32;                    {number of pads}
buf1[4]:=64;                    {pad character}
buf1[5]:=currEoPc+32;           {packet terminator}
buf1[6]:=currCtPf;              {control prefix}
if (currBnPf<0) then i:=32 else i:=currBnPf;
buf1[7]:=i;                     {binary prefix}
buf1[8]:=currChck+$30;          {check type}
if (currRpPf<0) then i:=32 else i:=currRpPf;
buf1[9]:=i;                     {repeat prefix}
if (currShIn<0) or (currShOt<0) then i:=0 else i:=32;
i:=i or 14;
buf1[10]:=i+32;                 {capabilities}
buf1[11]:=currWind+32;          {window size}
buf1[12]:=(currPack div 95)+32; {window size high}
buf1[13]:=(currPack mod 95)+32; {window size low}
q:=currChck;
currChck:=1;
sendOnePacket(buf1,13,s-1,89);  {ack}
currChck:=q;
if recvOnePacket(5,buf1,p,o,i) then begin;
  WriteLn('timeout!');
  dec(r);
  goto f2;
  end;
if (i<>70) then begin;
  WriteLn('got invalid packet type!');
  dec(r);
  goto f2;
  end;
if (o<>s and $3f) then begin;
  WriteLn('got invalid sequence number!');
  dec(r);
  goto f2;
  end;
f3:
currShft:=0;
WriteLn('got file-header...');
inc(s);
deQuoteOnePacket(buf1,buf2,p,p);
a[0]:=chr(p);
move(buf2,a[1],length(a));
a:=path+a;
WriteLn('--- receiving '+a+'...');

i:=xCreate(a);
if (mode=2) and (i<>0) then begin;
  WriteLn('file already exists!');
  exit;
  end;
if (xOpen(f,a,xGenFilMod_rw)<>0) then begin;
  WriteLn('error opening file!');
  exit;
  end;
if (mode=3) and (xfileSize(f)<>0) then begin;
  WriteLn('file already exists!');
  exit;
  end;
xSeek(f,0);
xTruncate(f);
q:=0;
r:=16;
sendOnePacket(buf1,0,s-1,89);   {ack}

f4:
Write(BStr(q)+#13);
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
if recvOnePacket(10,buf1,p,o,i) then begin;
  WriteLn(BStr(q)+': timeout, sending nak!');
  sendOnePacket(buf1,0,s,78);   {nak}
  dec(r);
  goto f4;
  end;
if (o<>s and $3f) then begin;
  WriteLn(BStr(q)+': got invalid sequence number!');
  dec(r);
  goto f4;
  end;
if (i=65) then begin;
  WriteLn(BStr(q)+': got file attributes!');
  sendOnePacket(buf1,0,s,89);     {ack}
  inc(s);
  goto f4;
  end;
if (i=90) then begin;
  WriteLn(BStr(q)+': end of file received!');
  r:=16;
  inc(s);
  goto f5;
  end;
if (i<>68) then begin;
  WriteLn(BStr(q)+': got invalid packet type!');
  dec(r);
  goto f4;
  end;
sendOnePacket(buf1,0,s,89);     {ack}
deQuoteOnePacket(buf1,buf2,p,p);
xBlockWrite(f,buf2,p);
inc(q,p);
inc(s);
r:=16;
goto f4;

f5:
if (r<1) then begin;
  WriteLn('retry counter expired, transfer failed!');
  exit;
  end;
sendOnePacket(buf1,0,s-1,89);   {ack}
if recvOnePacket(5,buf1,p,o,i) then begin;
  WriteLn('timeout, sending ack!');
  dec(r);
  goto f5;
  end;
if (o<>s and $3f) then begin;
  WriteLn('got invalid sequence number!');
  dec(r);
  goto f5;
  end;
if (i=70) then goto f3;
if (i<>66) then begin;
  WriteLn('got invalid packet type!');
  dec(r);
  goto f5;
  end;
WriteLn('got break transfer...');
inc(s);
sendOnePacket(buf1,0,s-1,89);   {ack}
End;





Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('kermit file transfer v1.0, done by Mc at '#%date' '#%time'.');

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

output_bufSiz:=0;
input_bufSiz:=0;
input_bufPos:=0;
currChck:=1;
currMark:=1;
currPadC:=32;
currPadN:=0;
currEoPc:=13;
currCtPf:=35;
currBnPf:=-1; {38}
currRpPf:=-1; {126}
currWind:=8;
currPack:=90;
currShIn:=-1; {15}
currShOt:=-1; {14}
currShft:=0;
ccitt16build;

if (o=1) then doReceive else doSend;
END.