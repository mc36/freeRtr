{$heap 83k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
Var
  DriveSize:LongInt;
  DriveBegin:LongInt;
  DriveReadOnly:Boolean;
{$include \sources\filesystem\disk2.inc}

Type
  OneDriveCommunicationRecord=record
    cmd:longint;
    uid:longint;
    rgt:longint;
    siz:longint;
    dir:string;
    fn1:string;
    fn2:string;
    hdr:array[1..512] of byte;
    dat:array[1..65536] of byte;
    end;
Var
  DriveComm:OneDriveCommunicationRecord;
  DriveLetter:Char;


Function GetInfoBlock(var buffer):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  siz:LongInt;

function conv(i:longint):string;
begin;
conv:=BStr(i)+' sectors;  '+BStr(i shr 1)+' KB;  '+BStr(i shr 11)+' MB';
end;

procedure add(a:string);
begin;
a:=a+#13#10;
move(a[1],buf[siz+1],length(a));
inc(siz,length(a));
end;

Begin;
siz:=0;
add('    model: "'+DriveInternalIdentifier.model+'"');
add('   serial: "'+DriveInternalIdentifier.serial+'"');
add(' firmware: "'+DriveInternalIdentifier.firm+'"');
add('cylinders: '+BStr(DriveInternalIdentifier.cyl));
add('    heads: '+BStr(DriveInternalIdentifier.hed));
add('  sectors: '+BStr(DriveInternalIdentifier.sec));
add('beginning: '+conv(DriveInternalIdentifier.beg));
add('     size: '+conv(DriveInternalIdentifier.siz));
GetInfoBlock:=siz;
End;


Procedure justName(var a:String;full:Boolean);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
o:=0;
for i:=1 to ab0 do if (ab[i]=$5c) then o:=i;
a:=copy(a,o+1,255);
if full then a:=DriveLetter+':\'+a;
End;

Procedure updateDateEntry(var d:xDirEntryDateTimeRec);
Begin;
fillchar(d,sizeof(d),0);
d.year:=2000;
d.month:=1;
d.day:=1;
End;


Label f1,f2;
Var
  f:xFile;
  a:String;
  buf:array[1..512] of byte;
  i,o,p,q,r:LongInt;
  DiskSize:LongInt;
  ntry:xDirEntryRec;
BEGIN;
WriteLn('disk viewer v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>3) then begin;
  WriteLn('using: diskViewer.code <process> <drive> <letter>');
  Halt(1);
  end;
a:=ParamStr(3);
DriveLetter:=LowCase(a[1]);
a:=ParamStr(2);
i:=BVal(a);
a:=ParamStr(1);
WriteLn('opening '+a+' '+BStr(i)+'...');
if (DriveOpen(a,i)<>0) then begin;
  WriteLn('error opening drive!');
  Halt(1);
  end;
DriveReadOnly:=False;
WriteLn('this disk has '+BStr(DriveSize*512)+' bytes total capacity.');
WriteLn('going to log in as '+DriveLetter+':\...');
FillChar(DriveComm,sizeof(DriveComm),0);
if (BugOS_driveLogin(DriveLetter,DriveComm)<>0) then begin;
  WriteLn('error logging in!');
  Halt(1);
  end;
BugOS_SignDaemoning;
i:=DriveSize;
if (i>$3fffff) then i:=$3fffff;
DiskSize:=i*512;

f1:
if (DriveComm.cmd shr 16<>$8000) then begin;
  Relequish;
  goto f1;
  end;
i:=DriveComm.cmd and $ffffff;
case i of
  $0e:begin;{read file}
    DriveComm.cmd:=1;
    move(DriveComm.hdr[5],i,sizeof(i));
    move(DriveComm.hdr,p,sizeof(p));
    if (i=1) then begin;
      i:=GetInfoBlock(DriveComm.dat)-p;
      if (i<DriveComm.siz) then goto f2;
      move(DriveComm.dat[p+1],DriveComm.dat,i);
      inc(p,i);
      move(p,DriveComm.hdr,sizeof(p));
      DriveComm.cmd:=0;
      goto f2;
      end;
    q:=DriveComm.siz;
    if (q+p>DiskSize) then goto f2;
    o:=1;
    while (q>0) do begin;
      if (DriveRead(p shr 9,buf)<>0) then goto f2;
      i:=p and $1ff;
      r:=sizeof(buf)-i;
      if (r>q) then r:=q;
      move(buf[i+1],DriveComm.dat[o],r);
      inc(o,r);
      inc(p,r);
      dec(q,r);
      end;
    move(p,DriveComm.hdr,sizeof(p));
    DriveComm.cmd:=0;
    end;
  $0f:begin;{write file}
    DriveComm.cmd:=1;
    move(DriveComm.hdr[5],i,sizeof(i));
    move(DriveComm.hdr,p,sizeof(p));
    if (i=1) then goto f2;
    q:=DriveComm.siz;
    if (q+p>DiskSize) then goto f2;
    o:=1;
    while (q>0) do begin;
      i:=p and $1ff;
      r:=sizeof(buf)-i;
      if (r>q) then r:=q;
      if (r<>sizeof(buf)) then if (DriveRead(p shr 9,buf)<>0) then goto f2;
      move(DriveComm.dat[o],buf[i+1],r);
      if (DriveWrite(p shr 9,buf)<>0) then goto f2;
      inc(o,r);
      inc(p,r);
      dec(q,r);
      end;
    move(p,DriveComm.hdr,sizeof(p));
    DriveComm.cmd:=0;
    end;
  $10:begin;{seek file}
    move(DriveComm.dat,i,sizeof(i));
    move(i,DriveComm.hdr,sizeof(i));
    DriveComm.cmd:=0;
    end;
  $11:begin;{get file size}
    move(DriveComm.hdr[5],i,sizeof(i));
    if (i=1) then i:=GetInfoBlock(DriveComm.dat) else i:=DiskSize;
    move(i,DriveComm.dat,sizeof(i));
    DriveComm.cmd:=0;
    end;
  $12:begin;{get file pos}
    move(DriveComm.hdr,i,sizeof(i));
    DriveComm.cmd:=0;
    move(i,DriveComm.dat,sizeof(i));
    end;
  $13:begin;{truncate file}
    DriveComm.cmd:=1;
    end;
  $0d:begin;{open file}
    if (DriveComm.uid<>0) then begin; DriveComm.cmd:=4;goto f2; end;
    DriveComm.cmd:=1;
    justName(DriveComm.fn1,false);
    a:=kicsi(DriveComm.fn1);
    o:=0;
    if (a='info') then o:=1;
    if (a='data') then o:=2;
    if (o<1) then goto f2;
    justName(DriveComm.fn1,true);
    i:=0;
    move(i,DriveComm.hdr,sizeof(i));
    move(o,DriveComm.hdr[5],sizeof(o));
    i:=$03;
    move(o,DriveComm.dat[1],sizeof(o));
    move(i,DriveComm.dat[5],sizeof(i));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    DriveComm.cmd:=0;
    end;
  $01:begin;{change directory}
    DriveComm.cmd:=0;
    DriveComm.fn1:=DriveLetter+':\';
    end;
  $0c:begin;{read directory}
    move(DriveComm.hdr,o,sizeof(o));
    inc(o);
    fillChar(ntry,sizeof(ntry),0);
    ntry.rights:=$03;
    updateDateEntry(ntry.created);
    updateDateEntry(ntry.modified);
    case o of
      1:begin;
        ntry.name:='info';
        ntry.size:=GetInfoBlock(DriveComm.dat);
        end;
      2:begin;
        ntry.name:='data';
        ntry.size:=DiskSize;
        end;
      else fillChar(ntry,sizeof(ntry),0);
      end;
    DriveComm.cmd:=0;
    move(o,DriveComm.hdr,sizeof(o));
    move(ntry,DriveComm.dat[1],sizeof(ntry));
    end;
  $0b:begin;{open directory}
    DriveComm.fn1:=DriveLetter+':\';
    DriveComm.cmd:=0;
    i:=0;o:=$03;
    move(i,DriveComm.hdr,sizeof(i));
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(DriveComm.fn1,DriveComm.dat[9],sizeof(DriveComm.fn1));
    end;
  $03:begin;{create directory}
    DriveComm.cmd:=4;
    end;
  $04:begin;{erase directory}
    DriveComm.cmd:=4;
    end;
  $05:begin;{create file}
    DriveComm.cmd:=4;
    end;
  $06:begin;{erase file}
    DriveComm.cmd:=4;
    end;
  $07:begin;{rename}
    DriveComm.cmd:=4;
    end;
  $08:begin;{make link}
    DriveComm.cmd:=4;
    end;
  $09:begin;{set rights}
    DriveComm.cmd:=4;
    end;
  $0a:begin;{set date}
    DriveComm.cmd:=4;
    end;
  $02:begin;{drive statistics}
    i:=0;
    o:=DiskSize;
    p:=0;
    move(i,DriveComm.dat[1],sizeof(i));
    move(o,DriveComm.dat[5],sizeof(o));
    move(p,DriveComm.dat[9],sizeof(p));
    i:=1;
    move(i,DriveComm.dat[13],sizeof(i));
    DriveComm.cmd:=0;
    end;
  else DriveComm.cmd:=1;
  end;
f2:
BugOS_driveDone;
goto f1;
END.