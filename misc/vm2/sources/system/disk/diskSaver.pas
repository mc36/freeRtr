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
WriteLn('disk saver v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>3) then begin;
  WriteLn('using: diskSaver.code <process> <drive> <filename>');
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
WriteLn('this disk has '+BStr(DriveSize*512)+' bytes total capacity.');
a:=ParamStr(3);
if (xCreate(a)<>0) then begin;
  WriteLn('error creating file!');
  Halt(1);
  end;
if (xOpen(f,a,xGenFilMod_rw)<>0) then begin;
  WriteLn('error opening file!');
  Halt(1);
  end;
s:=DriveSize;
p:=0;
while (p<s) do begin;
  if (DriveRead(p,buf)<>0) then begin;
    WriteLn('error reading drive!');
    Halt(1);
    end;
  if (xBlockWrite(f,buf,sizeof(buf))<>0) then begin;
    WriteLn('error writing file!');
    Halt(1);
    end;
  inc(p);
  Write(BStr(p*512)+#13);
  end;
WriteLn('successful!            ');
END.