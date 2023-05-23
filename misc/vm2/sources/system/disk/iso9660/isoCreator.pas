{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc datetime.inc}
{$sysinc param.inc}
{$sysinc memory.inc}

Const ProggyName='iso9660 creator v1.0';


Procedure immErr(a:string);
Begin;
WriteLn(a);
Halt(1);
End;


Function FindOneFile(a:String;var n:xDirEntryRec):Boolean;
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
  goto f1;
  end;
f1:
xDirClose(f);
End;

Procedure GetCurrentDate(var d:xDirEntryDateTimeRec);
Var w1,w2,w3:Word;
Begin;
xGetDate(w1,w2,w3);
d.year:=w1;
d.month:=w2;
d.day:=w3;
xGetTime(w1,w2,w3);
d.hour:=w1;
d.minute:=w2;
d.second:=w3;
End;



{$include isoCreator.inc}



Var
  a:String;
BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');

if (paramCount<>2) then immErr('using: isocreator.code <textfile> <imagefile>');

a:=paramStr(1);
if (xtOpen(filesInp,a,true)<>0) then immErr('error opening textfile');

a:=paramStr(2);
if (xCreate(a)<>0) then immErr('error creating isofile!');
if (xOpen(filesOut,a,xGenFilMod_rw)<>0) then immErr('error opening isofile!');

GenerateHeader;
GenerateFileList;
GenerateDirectories;
GeneratePathTable;
GenerateFooters;

xClose(filesOut);
WriteLn('successful!');
END.