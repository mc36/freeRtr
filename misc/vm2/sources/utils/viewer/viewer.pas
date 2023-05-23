{$heap 127k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc bin.inc}
{$sysinc crt.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}

{$include utils.inc}
{$include fileio.inc}
{$include screen.inc}
{$include viewer.inc}


Label f1,f2;
Var i:Word;
BEGIN;
WriteLn('viewer v1.0, done by Mc at '#%date' '#%time'.');
if (ParamCount<>1) then begin;
  WriteLn('using: viewer.code <filename>');
  Halt(1);
  end;
filNam:=ParamStr(1);
WriteLn('opening '+filNam+'...');
if (xOpen(filHdr,filNam,xGenFilMod_rw)<>0) then immErr('error opening file!');
filSiz:=xFileSize(filHdr);
bufSiz:=0;
filScr:=0;
filPos:=0;
filBin:=True;
selBeg:=0;
selEnd:=-1;

RefreshScr:=$ff;
FreshScreen;
f1:
i:=ReadKey;
if ProcessKey(i) then goto f2;
if keypressed then goto f1;
FreshScreen;
goto f1;
f2:
xClose(filHdr);
GotoXY(1,ScrSizY);
textColor($07);
WriteLn('');
END.