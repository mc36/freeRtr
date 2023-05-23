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
Type OneSectorRecord=array[1..512] of byte;

Var
  orig,new:OneSectorRecord;
  f:xFile;
  a:String;
  i:LongInt;
BEGIN;
WriteLn('bootSector writer v1.0, done by Mc at '#%date' '#%time'.');
if (ParamCount<2) then begin;
  WriteLn('using: instBoot.code <process> <drive> [bootsec]');
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
a:=paramStr(3);
if (a='') then a:='bootSec-disk.code';
a:=xFileName(ParamStr(0),1)+a;
WriteLn('reading '+a+'...');
if (xOpen(f,a,xGenFilMod_r)<>0) then begin;
  WriteLn('error opening file!');
  Halt(1);
  end;
if (xBlockRead(f,new,sizeof(new))<>0) then begin;
  WriteLn('error reading file!');
  Halt(1);
  end;
xClose(f);
if (DriveRead(0,orig)<>0) then begin;
  WriteLn('error reading disk!');
  halt(1);
  end;
Write('do you really want to install new boot sector?');
i:=ReadKey;
if not ((i=ord('y')) or (i=ord('Y'))) then begin;
  WriteLn(' NO!');
  Halt(1);
  end;
WriteLn(' yes!');
WriteLn('generating data...');
for i:=1 to 36 do new[16+i]:=orig[16+i];
WriteLn('writing data...');
if (DriveWrite(0,new)<>0) then begin;
  WriteLn('error writing disk!');
  halt(1);
  end;
DriveClose;
WriteLn('successful!');
END.