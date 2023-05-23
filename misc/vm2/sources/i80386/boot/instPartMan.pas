{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc bugos.inc}
Var DriveSize,DriveBegin:LongInt;DriveReadOnly:Boolean;
{$include \sources\filesystem\disk2.inc}
Type OneSectorRecord=record
  s:word;
  d:array[1..8*1024] of byte;
  end;

Procedure LoadFile(var d:OneSectorRecord;a:String);
Var
  f:xFile;
  i:LongInt;
Begin;
a:=xFileName(ParamStr(0),1)+a;
WriteLn('reading '+a+'...');
if (xOpen(f,a,xGenFilMod_r)<>0) then begin;
  WriteLn('error opening file!');
  Halt(1);
  end;
i:=xFileSize(f);
if (i>sizeof(d.d)) then i:=sizeof(d.d);
d.s:=i;
if (xBlockRead(f,d.d,i)<>0) then begin;
  WriteLn('error reading file!');
  Halt(1);
  end;
xClose(f);
for i:=d.s+1 to sizeof(d.d) do d.d[i]:=0;
End;


Var
  orig,lod,cod:OneSectorRecord;
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('partition mananger writer v1.0, done by Mc at '#%date' '#%time'.');
if (ParamCount<>2) then begin;
  WriteLn('using: instPart.code <process> <drive>');
  Halt(1);
  end;
a:=paramStr(1);
i:=BVal(ParamStr(2));
WriteLn('opening '+a+' '+BStr(i)+'...');
if (DriveOpen(a,i)<>0) then begin;
  WriteLn('error opening drive!');
  Halt(1);
  end;
DriveReadOnly:=False;
WriteLn('this disk has '+BStr(DriveSize*512)+' bytes total capacity.');
LoadFile(lod,'partload.code');
LoadFile(cod,'partman.code');
if (DriveRead(0,orig.d)<>0) then begin;
  WriteLn('error reading disk!');
  halt(1);
  end;
Write('do you really want to install partition manager?');
i:=ReadKey;
if not ((i=ord('y')) or (i=ord('Y'))) then begin;
  WriteLn(' NO!');
  Halt(1);
  end;
WriteLn(' yes!');
WriteLn('generating data...');
for i:=1 to lod.s do orig.d[i]:=lod.d[i];
for i:=lod.s+1 to 512-2-4*16-1 do orig.d[i]:=0;
orig.d[$180+1]:=20;
o:=(cod.s+511) div 512;
orig.d[lod.s+1]:=o;
WriteLn('writing partition manager...');
for i:=1 to o do if (DriveWrite(i,cod.d[i*512-511])<>0) then begin;
  WriteLn('error writing disk!');
  halt(1);
  end;
WriteLn('writing partition table...');
if (DriveWrite(0,orig.d)<>0) then begin;
  WriteLn('error writing disk!');
  halt(1);
  end;
DriveClose;
WriteLn('successful!');
END.