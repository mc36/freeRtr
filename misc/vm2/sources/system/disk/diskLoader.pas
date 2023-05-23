{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
Var
  DriveSize:LongInt;
  DriveBegin:LongInt;
  DriveReadOnly:Boolean;
{$include \sources\filesystem\disk2.inc}

Var
  f:xFile;
  buf:array[1..512] of byte;
  a:String;
  i,p,s:LongInt;
BEGIN;
WriteLn('disk loader v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>3) then begin;
  WriteLn('using: diskLoader.code <filename> <process> <drive>');
  Halt(1);
  end;
a:=ParamStr(1);
if (xOpen(f,a,xGenFilMod_r)<>0) then begin;
  WriteLn('error opening file!');
  Halt(1);
  end;
a:=ParamStr(3);
i:=BVal(a);
a:=ParamStr(2);
WriteLn('opening '+a+' '+BStr(i)+'...');
DriveReadOnly:=False;
if (DriveOpen(a,i)<>0) then begin;
  WriteLn('error opening drive!');
  Halt(1);
  end;
i:=DriveSize*512;
p:=xFileSize(f);
WriteLn('this disk has '+BStr(i)+' bytes total capacity.');
if (i<>p) then begin;
  WriteLn('size mismatch; filesize='+BStr(p));
  halt(1);
  end;
s:=DriveSize;
p:=0;
while (p<s) do begin;
  if (xBlockRead(f,buf,sizeof(buf))<>0) then begin;
    WriteLn('error reading file!');
    Halt(1);
    end;
  if (DriveWrite(p,buf)<>0) then begin;
    WriteLn('error writing drive!');
    Halt(1);
    end;
  inc(p);
  Write(BStr(p*512)+#13);
  end;
WriteLn('successful!            ');
END.