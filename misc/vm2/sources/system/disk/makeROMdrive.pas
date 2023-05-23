{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
Const maxFiles=32;
Type
  AllFilesRecord=array[1..maxFiles] of record
    p:String;
    n:String;
    i:xDirEntryRec;
    end;

Function FindOneFile(a:String;var n:xDirEntryRec;var m:String):Boolean;
Label f1;
Var f:xFile;
Begin;
FindOneFile:=True;
if (xDirOpen(f,xFileName(a,1))<>0) then exit;
a:=kicsi(xFileName(a,2)+xFileName(a,3));
while (xDirRead(f,n)=0) do begin;
  if (n.name='') then exit;
  if (kicsi(n.name)<>a) then continue;
  FindOneFile:=False;
  if (m='') then m:=n.name;
  goto f1;
  end;
f1:
xDirClose(f);
End;

Procedure convDate(var t:xtText;d:xDirEntryDateTimeRec);
Begin;
xtWriteLn(t,'dw '+BStr(d.year));
xtWriteLn(t,'db '+BStr(d.month)+','+BStr(d.day)+','+BStr(d.hour)+','+BStr(d.minute)+','+BStr(d.second));
End;

Var
  dat:AllFilesRecord;
  num:Byte;
  t1,t2:xtText;
  a,b,fn:String;
  i:LongInt;
BEGIN;
WriteLn('romDrive Creator v1.0, done by Mc at '#%date' '#%time'.');
fn:=ParamStr(1);
if (fn='') then begin;
  WriteLn('using: mkromdrv.code <filelist>');
  Halt(2);
  end;
WriteLn('using '+fn+'...');
if (xtOpen(t1,fn,true)<>0) then begin;
  WriteLn('error opening filelist!');
  Halt(1);
  end;
num:=0;
while not xtEOF(t1) do begin;
  a:=xtReadLn(t1,255);
  i:=pos(' ',a);
  if (i<1) then i:=666;
  b:=copy(a,i+1,255);
  a:=copy(a,1,i-1);
  inc(num);
  dat[num].p:=a;
  dat[num].n:=b;
  end;
xtClose(t1);
for i:=1 to num do if FindOneFile(dat[i].p,dat[i].i,dat[i].n) then begin;
  WriteLn('error opening '+dat[i].p+'!');
  Halt(1);
  end;
b:=xFileName(fn,1)+xFileName(fn,2)+'.asm';
WriteLn('generating '+b+'...');
xErase(b);
xCreate(b);
if (xtOpen(t2,b,false)<>0) then begin;
  WriteLn('error opening output!');
  Halt(1);
  end;
a:=GetMyFullFileName;
a:=xFileName(a,1)+xFileName(a,2)+'.lod';
if (xtOpen(t1,a,true)<>0) then begin;
  WriteLn('error opening loader!');
  Halt(1);
  end;
while not xtEOF(t1) do begin;
  a:=xtReadLn(t1,255);
  if (a<>';X;') then begin;
    xtWriteLn(t2,a);
    continue;
    end;
  xtWriteLn(t2,'numberOfFiles equ '+BStr(num));
  xtWriteLn(t2,'dataBegin:');
  for i:=1 to num do begin;
    a:=BStr(i);
    xtWriteLn(t2,'dd offset file'+a+'_hdr,offset file'+a+'_bin');
    end;
  for i:=1 to num do begin;
    xtWriteLn(t2,'');
    xtWriteLn(t2,'align 4');
    xtWriteLn(t2,'file'+BStr(i)+'_hdr:');
    xtWriteLn(t2,'dd '+BStr(dat[i].i.size));
    xtWriteLn(t2,'dd '+BStr(dat[i].i.rights));
    xtWriteLn(t2,'dd '+BStr(dat[i].i.owner));
    convDate(t2,dat[i].i.created);
    convDate(t2,dat[i].i.modified);
    a:=dat[i].n;
    xtWriteLn(t2,'db '+BStr(length(a))+','''+a+''',0');
    end;
  for i:=1 to num do begin;
    xtWriteLn(t2,'');
    xtWriteLn(t2,'file'+BStr(i)+'_bin:');
    xtWriteLn(t2,'incbin '+dat[i].p);
    end;
  end;
xtClose(t2);
WriteLn('successfully finished!');
END.