{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
Const maxRW=16*1024;
Var
  bufferBuf:^array[0..0] of char;
  bufferSiz:LongInt;

Procedure resize(n:LongInt);
Var
  i:LongInt;
  p:Pointer;
Begin;
i:=ExtendedMemoryResize(p,n);
if (i<n) then begin;
  writeln('error allocating memory!');
  halt(1);
  end;
bufferBuf:=p^;
End;

Procedure ReadUpFile(a:String);
Var
  f:xFile;
  i,p,s:LongInt;
Begin;
WriteLn('reading '+a+'...');
if (xOpen(f,a,xGenFilMod_r)<>0) then begin;
  WriteLn('error opening file!');
  Halt(1);
  end;
s:=xFileSize(f);
p:=0;
while (p<s) do begin;
  i:=s-p;
  if (i>maxRW) then i:=maxRW;
  resize(bufferSiz+i);
  if (xBlockRead(f,bufferBuf^[bufferSiz],i)<>0) then begin;
    WriteLn('error reading file!');
    Halt(1);
    end;
  inc(p,i);
  inc(bufferSiz,i);
  end;
xClose(f);
End;

Var
  f:xFile;
  a:String;
  p,i:LongInt;
BEGIN;
WriteLn('kernel writer v1.0, done by Mc at '#%date' '#%time'.');
bufferSiz:=0;
if (paramCount<>3) then begin;
  WriteLn('using: instKern.code <bootImage> <kernel> <romDrive>');
  halt(1);
  end;
ReadUpFile(ParamStr(2));
ReadUpFile(ParamStr(3));
Write('do you really want to install kernel?');
i:=ReadKey;
if not ((i=ord('y')) or (i=ord('Y'))) then begin;
  WriteLn(' NO!');
  Halt(1);
  end;
WriteLn(' yes!');
a:=ParamStr(1);
WriteLn('writing '+a+'...');
xSetRight(a,0,xRights_OwnRead);
if (xOpen(f,a,xGenFilMod_rw)<>0) then begin;
  WriteLn('error opening file!');
  Halt(1);
  end;
i:=xFileSize(f);
if (i<bufferSiz) then begin;
  WriteLn('output too small ('+BStr(i)+'), at least '+BStr(bufferSiz)+' bytes needed.');
  halt(1);
  end;
p:=0;
while (p<bufferSiz) do begin;
  i:=bufferSiz-p;
  if (i>maxRW) then i:=maxRW;
  if (xBlockWrite(f,bufferBuf^[p],i)<>0) then begin;
    WriteLn('error writing file!');
    Halt(1);
    end;
  inc(p,i);
  end;
xClose(f);
WriteLn('successful!');
END.