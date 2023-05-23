{$undef debug}
{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc datetime.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$include authenticator.inc}
{$include \sources\internet\kernel\utils\unixtime.inc}

Type oneOctetType=array[1..8] of byte;
Var
  privateKeyPath:String;
  timeRange,timeZone:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure ReadLocalhostFile;
Var
  t:xtText;
  a:String;
Begin;
timeZone:=0;
if (xtOpen(t,'c:\system\localHost.text',true)<>0) then exit;
a:=xtReadLn(t,255);
a:=xtReadLn(t,255);
a:=xtReadLn(t,255);
a:=xtReadLn(t,255);
timeZone:=(BVal(copy(a,1,3))*3600)+(BVal(copy(a,1,1)+copy(a,4,2))*60);
xtClose(t);
End;


Procedure rol(var data;siz,bit:LongInt);
Var
  new:array[0..255] of byte absolute data;
  old:array[0..255] of byte;
  mov:LongInt;
  i:LongInt;
Begin;
siz:=siz and $fff8;
bit:=bit mod siz;
while (bit<0) do inc(bit,siz);
siz:=siz shr 3;
mov:=bit shr 3;
bit:=bit and 7;
move(new,old,siz);
for i:=0 to siz-1 do
 new[i]:=(old[(siz+siz+i-mov) mod siz] shl bit) or (old[(siz+siz+i-mov-1) mod siz] shr (8-bit));
End;

Procedure bswap(var data;siz:LongInt);
Var
  dat:array[0..255] of byte absolute data;
  i,o:LongInt;
Begin;
if (siz<=2) then begin;
  i:=dat[0];
  dat[0]:=dat[1];
  dat[1]:=i;
  exit;
  end;
siz:=siz shr 1;
bswap(dat,siz);
bswap(dat[siz],siz);
for o:=0 to siz-1 do begin;
  i:=dat[o];
  dat[o]:=dat[siz+o];
  dat[siz+o]:=i;
  end;
End;




Function readOneKey(a:String;var key:oneOctetType):Boolean;
Label f1;
Var
  d:array[1..512] of byte;
  i,o,p:LongInt;
  t:xtText;
Begin;
readOneKey:=True;
fillchar(key,sizeof(key),0);
if (xtOpen(t,a,true)<>0) then begin;
  f1:
  xtClose(t);
  exit;
  end;
a:=xtReadLn(t,666);
if (copy(a,1,1)<>'#') then goto f1;
a:=copy(a,2,72);
a:=a+copy(xtReadLn(t,666),1,72);
if (xtReadLn(t,666)<>'0000:') then goto f1;
xtClose(t);
p:=0;
while (a<>'') do begin;
  i:=bval('$'+copy(a,1,2));
  a:=copy(a,3,666);
  inc(p);
  d[p]:=i;
  end;
p:=0;
while (p<72) do begin;
  inc(p);
  rol(d[p],32,16);
  d[p]:=d[p] xor $bf;
  inc(p);
  d[p]:=d[p] xor $88;
  inc(p);
  d[p]:=d[p] xor $bf;
  inc(p);
  d[p]:=d[p] xor $88;
  end;
move(d[37],key,sizeof(key));
readOneKey:=False;
End;




Procedure generateTime(delta:LongInt;var tim:oneOctetType);
Var
  a,b,c,d,e,f:Word;
  i:LongInt;
Begin;
xGetDate(a,b,c);
xGetTime(d,e,f);
i:=unixTime_convertTo(a,b,c,d,e,f)-timeZone;
i:=((i div 60)-$806880)*2;
i:=(i and $fffffffc)+(delta and $fffffffc);
tim[1]:=i shr 16;
tim[2]:=i shr 8;
tim[3]:=i;
tim[4]:=i;
move(tim,tim[5],4);
End;





Function hashData(key,dat:oneOctetType):String;
Var
  bitDat:array[0..255] of byte;
  hexKey:array[0..255] of byte;
  m:LongInt;
  a:String;

Procedure perm1rnd(bit,hkw:LongInt);
Var i,j,b,k:LongInt;
Begin;
b:=28;
for k:=0 to 7 do begin;
  for j:=hexKey[hkw+k] downto 1 do begin;
    bitDat[(bit+b+m+4) and $3f]:=bitDat[m];
    m:=(m+1) and $3f;
    end;
  for i:=0 to 3 do
   bitDat[64+bit+b+i]:=bitDat[64+bit+b+i] or bitDat[(bit+b+m+i) and $3f];
  dec(b,4);
  end;
End;

Procedure permute;
Var i,o,p:LongInt;
Begin;
fillchar(hexkey,sizeof(hexkey),0);
for i:=0 to 7 do begin;
  hexKey[i shl 1]:=key[i+1] shr 4;
  hexKey[(i shl 1)+1]:=key[i+1] and $f;
  end;
fillchar(bitdat,sizeof(bitdat),0);
p:=0;
for o:=0 to 7 do for i:=7 downto 0 do begin;
  bitDat[p]:=(dat[o+1] shr i) and 1;
  inc(p);
  end;
m:=0;
perm1rnd(32,0);
perm1rnd(0,8);
fillchar(dat,sizeof(dat),0);
p:=64;
for i:=0 to 7 do for o:=7 downto 0 do begin;
  inc(dat[i+1],(bitDat[p] and 1) shl o);
  inc(p);
  end;
End;

Procedure do4rnds;
Var rnd,i,j,t:LongInt;
Begin;
for rnd:=1 to 4 do begin;
  for i:=0 to 7 do for j:=0 to 7 do begin;
    if (((key[i+1] shr (j xor 7)) xor (dat[1] shr 7)) and 1<>0) then begin;
      t:=dat[5];
      dat[5]:=100-dat[1];
      dat[1]:=t;
      end else begin;
      rol(dat,8,7);
      dec(dat[1]);
      rol(dat,8,7);
      dec(dat[1]);
      dat[1]:=dat[1] xor dat[5];
      end;
    bswap(dat,8);
    rol(dat,64,1);
    bswap(dat,8);
    end;
  for i:=1 to 8 do key[i]:=key[i] xor dat[i];
  end;
End;

Procedure conv2dec;
Var i,c,h,l:LongInt;
Begin;
c:=(key[8] and $f) mod 5;
for i:=1 to 8 do begin;
  h:=dat[i] shr 4;
  l:=dat[i] and $f;
  c:=(c+(key[i] shr 4)) mod 5;
  if (h>9) then h:=(h-(c+1)*2) mod 10;
  c:=(c+(key[i] and $f)) mod 5;
  if (l>9) then l:=(l-(c+1)*2) mod 10;
  dat[i]:=(h shl 4) or l;
  end;
End;

Begin;
permute;
do4rnds;
permute;
conv2dec;
a:='';
for m:=1 to 8 do a:=a+byte2hextype(dat[m]);
hashData:=a;
End;





Function testOneAuthen(usr,pwd:String;var uid:LongInt):LongInt;
Label f1;
Var
  key,tim:oneOctetType;
  i,o,p,q:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  t:xtText;
  f:xFile;

Function rnl:String;
Var
  i:LongInt;
  a:String;
Begin;
a:=xtReadLn(t,666);
i:=pos(' ',a);
if (i<1) then i:=666;
rnl:=copy(a,1,i-1);
End;

Begin;
testOneAuthen:=3;
uid:=-1;
kicserel('\','',usr);
usr:=privateKeyPath+usr;
if readOneKey(usr+'.asc',key) then exit;
usr:=usr+'.pwd';
if (xtOpen(t,usr,true)<>0) then exit;
a:=rnl;
o:=BVal(a);
if (o=0) and (a<>'0') then o:=-1;
a:=rnl;
p:=xtGetPos(t);
xtClose(t);
testOneAuthen:=2;
i:=length(a);
if (copy(pwd,1,i)<>a) then exit;
pwd:=copy(pwd,i+1,666);
q:=length(pwd);
if (q<6) then exit;
for i:=-timeRange to timeRange do begin;
  generateTime(i*4,tim);
  a:=hashData(key,tim);
  if (copy(a,1,q)=pwd) then goto f1;
  if (copy(a,q+1,q)=pwd) then goto f1;
  end;
exit;
f1:
if (xOpen(f,usr,xGenFilMod_rw)<>0) then exit;
ab0:=xFileSize(f)-p;
xSeek(f,p);
xBlockRead(f,ab[1],ab0);
pwd:=a;
a:='';
for i:=1 to sizeof(tim) do a:=a+byte2hextype(tim[i]);
q:=length(a);
a:=a+'                ;last login'#13#10;
xSeek(f,p);
xBlockWrite(f,ab[1],ab0);
xTruncate(f);
xClose(f);
if (copy(a,1,q)=copy(pwd,1,q)) then exit;
uid:=o;
testOneAuthen:=0;
End;



Procedure ReadUpConfig(a:String);
Var t:xtText;

function gnl:string;
var
  i:longint;
  a:String;
begin;
a:=xtReadLn(t,255);
i:=pos(';',a);
if (i>0) then a:=copy(a,1,i-1);
a:=' '+a+' ';
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
gnl:=a;
end;

Var
  b:String;
  i:LongInt;
Begin;
WriteLn('reading '+a+'...');
if (xtOpen(t,a,true)<>0) then immErr('error opening!');
privateKeyPath:=gnl;
timeRange:=BVal(gnl);
xtClose(t);
if (copy(privateKeyPath,length(privateKeyPath),666)<>'\') then privateKeyPath:=privateKeyPath+'\';
if (timeRange<1) then timeRange:=60;
timeRange:=timeRange shr 1;
End;





Label f1,f2,f3;
Var
  req:authenticateRequestRecord;
  rep:authenticateResponseRecord;
  pip:LongInt;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('authen_securid v1.0, done by Mc at '#%date' '#%time'.');
unixTime_generateTable;
ReadLocalhostFile;
ReadUpConfig(GetAllParameters);
pipeLineBegListen;
BugOS_SignDaemoning;

f1:
if (pipeLineGetIncoming(pip)<>0) then begin;
  relequish;
  goto f1;
  end;
p:=16;
f2:
dec(p);
if (p<0) then begin;
  f3:
  pipeLineClose(pip);
  goto f1;
  end;
o:=sizeof(req);
if (pipeLineRecv(pip,req,o)<>0) then o:=0;
if (o=0) then begin;
  relequish;
  goto f2;
  end;
if (o<>sizeof(req)) then goto f3;
{$ifdef debug}write('user='+req.user+' pass='+req.pass+' info='+req.info+' service='+BStr(req.service));{$endif}
rep.stat:=testOneAuthen(req.user,req.pass,rep.uid);
{$ifdef debug}writeln(' result='+BStr(rep.stat)+' uid='+BStr(rep.uid));{$endif}

pipeLineSend(pip,rep,sizeof(rep));
pipeLineClose(pip);
goto f1;
END.