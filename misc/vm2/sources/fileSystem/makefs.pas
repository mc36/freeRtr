{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc param.inc}
{$sysinc datetime.inc}
{$sysinc bugos.inc}
{$include fs_all.inc}

Function askUserToAccept(a:String):Boolean;
Begin;
askUserToAccept:=False;
End;

Procedure displayStatusToUser(ps,mx:LongInt);
Begin;
End;

{$include fs_util.inc}

Var
  a:String;
  i,o:numberType;
  w:Word;
BEGIN;
WriteLn('makeFS v1.0 for '+FilesysName+', done by Mc at '#%date' '#%time'.');
if (ParamCount<>3) then begin;
  WriteLn('using: makefs.code <process> <drive> <bootKB>');
  Halt(1);
  end;
a:=ParamStr(1);
i:=BVal(ParamStr(2));
o:=BVal(ParamStr(3))*1024;
o:=o div BytesPerSector;
WriteLn('going to format '+a+' '+BStr(i)+'...');
if (DriveOpen(a,i)<>0) then begin;
  WriteLn('error opening drive!');
  Halt(1);
  end;
CacheInit;
WriteLn('this disk has '+BStr(DriveSize*BytesPerSector)+' bytes total capacity.');
if (o<0) or (o>DriveSize) then begin;
  WriteLn('boot size too big!');
  Halt(1);
  end;
WriteLn('boot file size will be '+BStr(o*BytesPerSector)+' bytes.');
Write('checking existing format...');
w:=FilesysOpen(true);
if (w=0) then WriteLn(' WARNiNG: filesystem still exists!') else WriteLn(' filesystem not found!');
Write('do you really want to continue (y/N)?');
w:=ReadKey;
if not ((w=ord('y')) or (w=ord('Y'))) then begin;
  WriteLn(' NO!');
  Halt(1);
  end;
WriteLn(' yes!');
WriteLn('creating filesystem...');
w:=FileSysFormat(o);
i:=CacheFlush;
if (w=0) then w:=i;
if (w=0) then WriteLn('successful!') else WriteLn('failed!');
DriveClose;
Halt(w);
END.