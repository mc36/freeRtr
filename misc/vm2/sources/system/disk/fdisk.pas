{$heap 255k}
{$stack 7k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}

Var
  DriveSize:LongInt;
  DriveBegin:LongInt;
  DriveReadOnly:Boolean;
  geomCyl:LongInt;
  geomHed:LongInt;
  geomSec:LongInt;
  ScrSizX,ScrSizY:Word;
  RefreshScr:byte; {32-cursor, 64-listing, 128-screen}

{$include \sources\filesystem\disk2.inc}

Const
  partitionsMax=32;
  partiNamesMax=512;
Type
  OnePartitionDataRecord=record
    d:array[1..partitionsMax] of record
      typ:Byte;
      bot:Boolean;
      beg:LongInt;
      siz:LongInt;
      end;
    n:Byte;
    end;
  OnePartitionEntry=record
    flags:Byte;                 {80h-bootable}
    begHed:Byte;                {beginning head}
    begSec:Byte;                {beginning sector}
    begCyl:Byte;                {beginning cylinder}
    sysId:Byte;                 {system indicator}
    endHed:Byte;                {ending head}
    endSec:Byte;                {ending sector}
    endCyl:Byte;                {ending cylinder}
    pos:LongWord;               {position inside the disk}
    siz:LongWord;               {size of partition}
    end;
  OnePartitionTable=record
    code:array[1..446] of byte; {boot loader code}
    tab:array[1..4] of OnePartitionEntry;  {the data}
    sig1:Byte;                  {should be $55}
    sig2:Byte;                  {should be $aa}
    end;
Var
  PartiNamesNum:Word;
  PartiNamesDat:array[1..partiNamesMax] of record
    t:byte;
    n:string;
    end;





Procedure readOnePartitionName(var f:xfile;var p,s:LongInt);
Var
  i:Byte;
  o:LongInt;
  a:String;
Begin;
xSeek(f,p);
xBlockRead(f,i,sizeof(i));
o:=s-p-1;
if (o>255) then o:=255;
a[0]:=chr(o);
xBlockRead(f,a[1],o);
o:=pos(#0,a);
a[0]:=chr(o-1);
inc(p,o+1);
if (partiNamesNum>=partiNamesMax) then exit;
inc(partiNamesNum);
partiNamesDat[partiNamesNum].t:=i;
partiNamesDat[partiNamesNum].n:=a;
End;

Function FindOnePartitionName(t:Byte):String;
Var
  i:Word;
  a:String;
Begin;
a:='unknown';
for i:=1 to partiNamesNum do if (partiNamesDat[i].t=t) then a:=partiNamesDat[i].n;
FindOnePartitionName:='$'+byte2hexType(t)+' - '+a;
End;

Procedure rereadPartitionTable(var d:OnePartitionTable);
Begin;
if (DriveRead(0,d)<>0) then begin;
  WriteLn('error reading partition table!');
  Halt(1);
  end;
End;

Procedure writePartitionTable(var d:OnePartitionTable);
Begin;
if (DriveWrite(0,d)<>0) then begin;
  WriteLn('error writing partition table!');
  Halt(1);
  end;
End;

Procedure alignCheck(var cur:LongInt;max:LongInt);
Begin;
if (cur>max) then cur:=max;
if (cur<1) then cur:=1;
End;


Procedure detectDiskGeometry;

procedure calc;
begin;
geomCyl:=DriveSize div ((geomHed+1)*geomSec);
end;

function size:longint;
begin;
size:=((geomHed+1)*geomCyl*geomSec) shr 1;
end;

Label f1;
Const heds:array[1..8] of byte=(2,4,8,16,32,64,128,255);
Var i:LongInt;
Begin;
WriteLn('model: "'+DriveInternalIdentifier.model+'"');
WriteLn('serial: "'+DriveInternalIdentifier.serial+'"');
WriteLn('firm: "'+DriveInternalIdentifier.firm+'"');
WriteLn('capacity: '+alakit(DriveSize shr 1)+' kb');
geomCyl:=DriveInternalIdentifier.cyl;
geomHed:=DriveInternalIdentifier.hed-1;
geomSec:=DriveInternalIdentifier.sec;
WriteLn('original size: '+alakit(size)+' kb');
WriteLn('original cylinder: 0..'+BStr(geomCyl));
WriteLn('original head: 0..'+BStr(geomHed));
WriteLn('original sector: 1..'+BStr(geomSec));
calc;
if (geomCyl<1024) then goto f1;
geomSec:=63;
for i:=1 to sizeof(heds) do begin;
  geomHed:=heds[i]-1;
  calc;
  if (geomCyl<1024) then goto f1;
  end;
f1:
calc;
WriteLn('translated size: '+alakit(size)+' kb');
WriteLn('translated cylinder: 0..'+BStr(geomCyl));
WriteLn('translated head: 0..'+BStr(geomHed));
WriteLn('translated sector: 1..'+BStr(geomSec));
End;

Procedure decodePartiTable(var d1:OnePartitionTable;var d2:OnePartitionDataRecord);
Var i:LongInt;
Begin;
if (d1.sig1<>$55) or (d1.sig2<>$aa) then begin;
  fillchar(d1,sizeof(d1),0);
  d1.sig1:=$55;
  d1.sig2:=$aa;
  end;
d2.n:=0;
for i:=1 to 4 do begin;
  if (d1.tab[i].sysId=0) then continue;
  inc(d2.n);
  d2.d[d2.n].beg:=d1.tab[i].pos;
  d2.d[d2.n].siz:=d1.tab[i].siz;
  d2.d[d2.n].typ:=d1.tab[i].sysId;
  d2.d[d2.n].bot:=(d1.tab[i].flags and $80<>0);
  end;
End;

Procedure unplugPartitions(var d:OnePartitionDataRecord);
Var i,o,p:LongInt;
Begin;
o:=0;
for i:=1 to d.n do if (d.d[i].typ<>0) then begin;
  inc(o);
  d.d[o]:=d.d[i];
  end;
d.n:=o;
End;

Procedure sortPartitions(var d:OnePartitionDataRecord);
Var
  dd:OnePartitionDataRecord;
  i,o,p,q:LongInt;
Begin;
for q:=1 to d.n do begin;
  p:=q;
  o:=d.d[p].beg;
  for i:=q to d.n do if (d.d[i].beg<o) then begin;
    p:=i;
    o:=d.d[p].beg;
    end;
  dd:=d;
  d.d[q]:=dd.d[p];
  d.d[p]:=dd.d[q];
  end;
End;

Procedure fillUpPartitions(var d:OnePartitionDataRecord);

procedure add(typ,beg,siz:LongInt;bot:boolean);
begin;
if (d.n>=partitionsMax) then exit;
if (siz<0) then siz:=0;
inc(d.n);
d.d[d.n].typ:=typ;
d.d[d.n].beg:=beg;
d.d[d.n].siz:=siz;
d.d[d.n].bot:=bot;
end;

Var
  dd:OnePartitionDataRecord;
  i,o,p:LongInt;
Begin;
unplugPartitions(d);
sortPartitions(d);
dd:=d;
fillchar(d,sizeof(d),0);
p:=geomSec;
for i:=1 to dd.n do begin;
  add(0,p,dd.d[i].beg-p,false);
  add(dd.d[i].typ,dd.d[i].beg,dd.d[i].siz,dd.d[i].bot);
  p:=dd.d[i].beg+dd.d[i].siz;
  end;
add(0,p,DriveSize-p,false);
End;

Function encodePartiTable(var d1:OnePartitionTable;var d2:OnePartitionDataRecord):Boolean;
Var i:LongInt;

procedure enc(var c,h,s:byte;v:longint);
begin;
s:=(v mod geomSec)+1;
v:=v div geomSec;
h:=v mod (geomHed+1);
v:=v div (geomHed+1);
c:=v and $ff;
s:=((v and $300) shr 2) or s;
end;

Begin;
if (d2.n>4) then begin;
  encodePartiTable:=True;
  exit;
  end;
fillchar(d1.tab,sizeof(d1.tab),0);
for i:=1 to d2.n do begin;
  if d2.d[i].bot then d1.tab[i].flags:=$80;
  d1.tab[i].sysId:=d2.d[i].typ;
  d1.tab[i].pos:=d2.d[i].beg;
  d1.tab[i].siz:=d2.d[i].siz;
  enc(d1.tab[i].begCyl,d1.tab[i].begHed,d1.tab[i].begSec,d2.d[i].beg);
  enc(d1.tab[i].endCyl,d1.tab[i].endHed,d1.tab[i].endSec,d2.d[i].beg+d2.d[i].siz-1);
  end;
encodePartiTable:=False;
End;




Procedure editPartitionData(var data:OnePartitionDataRecord;num:LongInt);
Label f1,f2,f3,f4;

procedure printOne(a:string;p:LongInt);
Var i,o:LongInt;
begin;
textColor($0f);
while (length(a)<10) do a:=' '+a;
write(a);
textColor($07);
a:=alakit(p);
while (length(a)<13) do a:=a+' ';
write('  '+a);
a:=alakit(p div 2);
while (length(a)<13) do a:=a+' ';
write('  '+a);
i:=(p mod geomSec)+1;
p:=p div geomSec;
o:=p mod (geomHed+1);
p:=p div (geomHed+1);
a:=alakit(p);
while (length(a)<8) do a:=a+' ';
write('  '+a);
a:=alakit(o);
while (length(a)<4) do a:=a+' ';
write('  '+a);
a:=alakit(i);
while (length(a)<6) do a:=a+' ';
write('  '+a);
WriteLn('');
end;

Var
  d:OnePartitionDataRecord;
  linesCur:LongInt;
  a:String;
  i,o,p,s:LongInt;
  w:Word;
Begin;
d:=data;
if (num>d.n) then exit;
RefreshScr:=$ff;
linesCur:=1;

f1:
if (RefreshScr and $80<>0) then begin;
  ConsoleSize(ScrSizX,ScrSizY);
  textColor($07);clrscr;
  textColor($2f);
  a:='keys:   enter=edit   f10=update   esc=exit   +/-/pgup/pgdn=inc/dec';
  while (length(a)<ScrSizX) do a:=a+' ';
  a:=copy(a,1,ScrSizX-1);
  GotoXY(1,ScrSizY);Write(a);
  textColor($1f);
  a:='partition '+BStr(num)+' of '+BStr(d.n);
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,1);Write(a);
  RefreshScr:=$ff;
  end;
if (RefreshScr and $40<>0) then begin;
  GotoXY(1,3);
  textColor($07);
  if d.d[num].bot then a:='yes' else a:='no ';
  WriteLn('bootable: '+a);
  a:='id: '+FindOnePartitionName(d.d[num].typ);
  while (length(a)<ScrSizX) do a:=a+' ';
  WriteLn(copy(a,1,ScrSizX-1));
  WriteLn('');
  textColor($0f);
  WriteLn('            sector         kilobytes      cylinder  head  sector');
  printOne('beginning',d.d[num].beg);
  printOne('ending',d.d[num].beg+d.d[num].siz-1);
  printOne('size',d.d[num].siz);
  RefreshScr:=RefreshScr or $20;
  end;
if (RefreshScr and $20<>0) then if (linesCur<3) then begin;
  gotoXY(11,linesCur+2);
  end else begin;
  case linesCur mod 5 of
    0:i:=43;
    1:i:=53;
    2:i:=59;
    3:i:=13;
    4:i:=28;
    end;
  gotoXY(i,((linesCur-3) div 5)+7);
  end;
RefreshScr:=0;

f2:
w:=ReadKey;
o:=linesCur;
case w of
  $8001:begin; RefreshScr:=$ff;goto f1; end;
  $801d:begin; data:=d;exit; end;{f10}
  $0478:exit;{alt+x}
  $8005:exit;{escape}
  $002d:begin;i:=-1;goto f3; end;{-}
  $002b:begin;i:=+1;goto f3; end;{+}
  $800a:begin;i:=-16;goto f3; end;{pgup}
  $800b:begin;i:=+16;goto f3; end;{pgdn}
  $8008:begin;i:=-64;goto f3; end;{home}
  $8009:begin;i:=+64;goto f3; end;{end}
  $8006:begin;i:=-256;goto f3; end;{insert}
  $8007:begin;i:=+256;goto f3; end;{delete}
  $810a:begin;i:=-1024;goto f3; end;{shift+pgup}
  $810b:begin;i:=+1024;goto f3; end;{shift+pgdn}
  $8108:begin;i:=-4096;goto f3; end;{shift+home}
  $8109:begin;i:=+4096;goto f3; end;{shift+end}
  $8106:begin;i:=-16384;goto f3; end;{shift+insert}
  $8107:begin;i:=+16384;goto f3; end;{shift+delete}
  $820a:begin;i:=-65536;goto f3; end;{ctrl+pgup}
  $820b:begin;i:=+65536;goto f3; end;{ctrl+pgdn}
  $8208:begin;i:=-262144;goto f3; end;{ctrl+home}
  $8209:begin;i:=+262144;goto f3; end;{ctrl+end}
  $8206:begin;i:=-1048576;goto f3; end;{ctrl+insert}
  $8207:begin;i:=+1048576;goto f3; end;{ctrl+delete}
  $840a:begin;i:=-4194304;goto f3; end;{ctrl+pgup}
  $840b:begin;i:=+4194304;goto f3; end;{ctrl+pgdn}
  $8408:begin;i:=-16777216;goto f3; end;{ctrl+home}
  $8409:begin;i:=+16777216;goto f3; end;{ctrl+end}
  $8406:begin;i:=-67108864;goto f3; end;{ctrl+insert}
  $8407:begin;i:=+67108864;goto f3; end;{ctrl+delete}
  $800c:if (linesCur<3) then dec(linesCur) else dec(linesCur,5);{up}
  $800d:if (linesCur<3) then inc(linesCur) else inc(linesCur,5);{down}
  $800e:dec(linesCur);{left}
  $800f:inc(linesCur);{left}
  end;
if (linesCur<1) then linesCur:=17;
if (linesCur>17) then linesCur:=1;
if (o<>linesCur) then begin;
  RefreshScr:=$20;
  goto f1;
  end;
goto f2;
f3:
case linesCur of
  1:d.d[num].bot:=not d.d[num].bot;
  2:inc(d.d[num].typ,i);
  else goto f4;
  end;
RefreshScr:=$40;
goto f1;
f4:
case linesCur mod 5 of
  3:i:=i;
  4:i:=i shl 1;
  0:i:=i*(geomHed+1)*geomSec;
  1:i:=i*geomSec;
  2:i:=i;
  end;
case (linesCur-3) div 5 of
  0:begin; inc(d.d[num].beg,i);dec(d.d[num].siz,i); end;
  1:inc(d.d[num].siz,i);
  2:inc(d.d[num].siz,i);
  end;
RefreshScr:=$40;
goto f1;
End;






Label f1,f2,f3;
Var
  f:xFile;
  partTab:OnePartitionTable;
  data:OnePartitionDataRecord;
  partiCur:LongInt;
  a:String;
  i,o,p,s:LongInt;
  w:Word;
BEGIN;
WriteLn('fixed disk partitioner v1.0, done by Mc at '#%date' '#%time'.');

PartiNamesNum:=0;
a:=paramStr(0);
a:=xFileName(a,1)+xFileName(a,2)+'.data';
if (xOpen(f,a,xGenFilMod_r)<>0) then begin;
  writeln('error reading '+a+'!');
  halt(1);
  end;
s:=xFileSize(f);
p:=0;
while (p<s) do readOnePartitionName(f,p,s);
xclose(f);

if (paramCount<>2) then begin;
  WriteLn('using: fdisk.code <process> <drive>');
  Halt(1);
  end;
a:=ParamStr(2);
i:=BVal(a);
a:=ParamStr(1);
WriteLn('opening '+a+' '+BStr(i)+'...');
if (DriveOpen(a,i)<>0) then begin;
  WriteLn('error opening drive!');
  Halt(1);
  end;
DriveReadOnly:=False;
detectDiskGeometry;
rereadPartitionTable(partTab);
decodePartiTable(partTab,data);
unplugPartitions(data);
sortPartitions(data);
fillUpPartitions(data);

RefreshScr:=$ff;
partiCur:=1;

f1:
alignCheck(partiCur,data.n);
if (RefreshScr and $80<>0) then begin;
  ConsoleSize(ScrSizX,ScrSizY);
  textColor($07);clrscr;
  textColor($2f);
  a:='keys:   alt+r=reread   alt+w=write   alt+e=edit   alt+d=delete   alt+x=exit';
  while (length(a)<ScrSizX) do a:=a+' ';
  a:=copy(a,1,ScrSizX-1);
  GotoXY(1,ScrSizY);Write(a);
  textColor($1f);
  a:='begin         size          type';
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,1);Write(a);
  RefreshScr:=$ff;
  end;
if (RefreshScr and $40<>0) then begin;
  for o:=1 to ScrSizY-2 do begin;
    GotoXY(1,o+1);
    if (o=partiCur) then textColor($2f) else textColor($07);
    if (o<1) or (o>data.n) then a:='' else begin;
      a:=copy(alakit(data.d[o].beg div 2)+'              ',1,14)+
         copy(alakit(data.d[o].siz div 2)+'              ',1,14)+
         FindOnePartitionName(data.d[o].typ);
      end;
    while (length(a)<ScrSizX) do a:=a+' ';
    a:=copy(a,1,ScrSizX);
    Write(a);
    end;
  RefreshScr:=RefreshScr or $20;
  end;
if (RefreshScr and $20<>0) then gotoXY(29,1+partiCur);
RefreshScr:=0;

f2:
w:=ReadKey;
o:=partiCur;
case w of
  $8001:begin; RefreshScr:=$ff;goto f1; end;
  $801d:goto f3;{f10}
  $0478:goto f3;{alt+x}
  $0472:begin;{alt+r}
    rereadPartitionTable(partTab);
    decodePartiTable(partTab,data);
    unplugPartitions(data);
    sortPartitions(data);
    fillUpPartitions(data);
    RefreshScr:=$40;
    goto f1;
    end;
  $0477:begin;{alt+w}
    unplugPartitions(data);
    sortPartitions(data);
    if encodePartiTable(partTab,data) then begin;
      gotoxy(1,1);
      textColor($1c);
      WriteLn('partition table is buggy! press any key to continue!');
      ReadKey;
      end else begin;
      writePartitionTable(partTab);
      end;
    fillUpPartitions(data);
    RefreshScr:=$ff;
    goto f1;
    end;
  $0465:begin;{alt+e}
    editPartitionData(data,partiCur);
    unplugPartitions(data);
    sortPartitions(data);
    fillUpPartitions(data);
    RefreshScr:=$ff;
    goto f1;
    end;
  $0464:begin;{alt+d}
    data.d[partiCur].typ:=0;
    unplugPartitions(data);
    sortPartitions(data);
    fillUpPartitions(data);
    RefreshScr:=$40;
    goto f1;
    end;
  $800c:dec(partiCur);{up}
  $800d:inc(partiCur);{down}
  $8008:partiCur:=1;{home}
  $8009:partiCur:=data.n;{end}
  $800a:dec(partiCur,ScrSizY shr 1);{pgup}
  $800b:inc(partiCur,ScrSizY shr 1);{pgdn}
  end;
alignCheck(partiCur,data.n);
if (o<>partiCur) then begin;
  RefreshScr:=$40;
  goto f1;
  end;
goto f2;
f3:
GotoXY(1,ScrSizY);
textColor($07);
WriteLn('');
END.