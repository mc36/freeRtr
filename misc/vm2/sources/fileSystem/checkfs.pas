{$heap 255k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc param.inc}
{$sysinc datetime.inc}
{$sysinc bugos.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include fs_all.inc}

Var autoMode:numberType;        {0=manual, 1=autoYes, 2=autoNo}

Function askUserToAccept(a:String):Boolean;
Label f1,f2;
Var i,o,p:numberType;
Begin;
askUserToAccept:=false;
case autoMode of
  1:begin; askUserToAccept:=true;exit; end;
  2:exit;
  end;
Write(a);
timer2start;
p:=CurrentTime;
o:=-1;
f1:
relequish;
timer2start;
i:=60-GetTimePast(p);
if (i<1) then begin; i:=0;goto f2; end;
if (i<>o) then begin;
  o:=i;
  a:=BStr(o);
  while (length(a)<2) do a:='0'+a;
  write(' ('+a+')'#8#8#8#8#8);
  end;
if not keypressed then goto f1;
i:=ReadKey;
if (i and $8000<>0) then goto f1;
f2:
a:=' NO! ';
if (i and $feff=ord('y')) then begin; askUserToAccept:=true;a:=' YES!'; end;
WriteLn(a);
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
WriteLn('checkFS v1.0 for '+FilesysName+', done by Mc at '#%date' '#%time'.');
if (ParamCount<2) then begin;
  WriteLn('using: checkfs.code <process> <drive> [auto/check/manual]');
  Halt(1);
  end;
a:=ParamStr(1);
i:=BVal(ParamStr(2));
WriteLn('going to check '+a+' '+BStr(i)+'...');
if (DriveOpen(a,i)<>0) then begin;
  WriteLn('error opening drive!');
  Halt(1);
  end;
CacheInit;
WriteLn('this disk has '+BStr(DriveSize*BytesPerSector)+' bytes total capacity.');
a:=paramStr(3);
autoMode:=0;
if (a='auto') then autoMode:=1;
if (a='check') then autoMode:=2;
if (a='manual') then autoMode:=0;
DriveLetter:='!';
CurrentUser:=UIDofAdmin;
if (FilesysOpen(false)<>0) then begin;
  WriteLn('error opening filesystem!');
  Halt(1);
  end;
i:=autoMode;
if (autoMode=2) then autoMode:=1;
if not askUserToAccept('do you really want to check filesystem?') then Halt(1);
autoMode:=i;
WriteLn('checking filesystem...');
w:=FileSysCheck;
i:=FilesysClose(true);
if (w=0) then w:=i;
i:=CacheFlush;
if (w=0) then w:=i;
if (w=0) then WriteLn('successful!') else WriteLn('failed!');
DriveClose;
Halt(w);
END.