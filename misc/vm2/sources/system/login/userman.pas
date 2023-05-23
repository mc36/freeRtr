{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}

{$include login.inc}
{$include utils.inc}
{$include fileio.inc}
{$include screen.inc}
{$include userman.inc}

Label f1,f2;
Var i:LongInt;
BEGIN;
WriteLn('user manager v1.0, done by Mc at '#%date' '#%time'.');
Randomize;

entryOpen(LoginDatabaseFilename);
scrPos:=1;
scrBeg:=0;

RefreshScr:=$ff;
FreshScreen;
f1:
i:=ReadKey;
if ProcessKey(i) then goto f2;
if keypressed then goto f1;
FreshScreen;
goto f1;
f2:
entryClose;
GotoXY(1,ScrSizY);
textColor($07);
WriteLn('');
END.