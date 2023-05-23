{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc crt.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}

{$include memory.inc}
{$include parser.inc}
{$include screen.inc}
{$include query.inc}
{$include main.inc}

Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
BEGIN;
WriteLn('mibber v1.0, done by Mc at '#%date' '#%time'.');

targetServer:=ParamStr(1);
a:=paramStr(2);
o:=0;
for i:=1 to ab0 do if (ab[i]=$5c) then o:=i;
mibPath:=copy(a,1,o);
if (o<1) then mibPath:='.\';
a:=copy(a,o+1,666);
if (ab0<1) then immErr('using: mibber.code <host> <file>');

mibCacheInit;
a:=ParseUpOneFile(a);
mibCacheTrunc(a);
if (mibEntryNum<1) then immErr('no oid parsed!');

BugOS_MyProcessInfo(o,i,i);
tempBegin:='snmp-'+BStr(o)+'.';
currentObject:='1';
descrSiz:=0;
descrBeg:=0;
oidCur:=1;
oidBeg:=0;

RefreshScr:=$ff;
drawScreen;
while handleOneKey do;

gotoXY(1,scrSizY);TextColor(7);WriteLn('');
xErase(tempBegin+extensionCmd);
xErase(tempBegin+extensionRes);
END.