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
if (ps and $7f<>0) then exit;
ps:=ps shr 7;
mx:=mx shr 7;
Write(BStr((ps*100) div mx)+'%'#13);
End;

{$include fs_util.inc}

Var
  a:String;
  i,o:numberType;
  w:Word;
BEGIN;
WriteLn('clearFS v1.0 for '+FilesysName+', done by Mc at '#%date' '#%time'.');
if (ParamCount<>2) then begin;
  WriteLn('using: clearfs.code <process> <drive>');
  Halt(1);
  end;
a:=ParamStr(1);
i:=BVal(ParamStr(2));
WriteLn('going to clear '+a+' '+BStr(i)+'...');
if (DriveOpen(a,i)<>0) then begin;
  WriteLn('error opening drive!');
  Halt(1);
  end;
CacheInit;
WriteLn('this disk has '+BStr(DriveSize*BytesPerSector)+' bytes total capacity.');
if (FilesysOpen(false)<>0) then begin;
  WriteLn('error opening filesystem!');
  Halt(1);
  end;
Write('do you really want to continue (y/N)?');
w:=ReadKey;
if not ((w=ord('y')) or (w=ord('Y'))) then begin;
  WriteLn(' NO!');
  Halt(1);
  end;
WriteLn(' yes!');
WriteLn('clearing free space...');
w:=FileSysClear;
i:=CacheFlush;
if (w=0) then w:=i;
if (w=0) then WriteLn('successful!') else WriteLn('failed!');
DriveClose;
Halt(w);
END.