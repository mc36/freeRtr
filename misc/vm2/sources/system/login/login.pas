{$heap 7k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$include utils.inc}
{$include login.inc}
{$include authenticator.inc}



Label f1;
Const maxTries=3;
Var
  dev:LongInt;
  i:Word;
  o:Longint;
  a,b:String;
BEGIN;
if (paramCount<3) then begin;
  WriteLn('user login v1.0, done by Mc at '#%date' '#%time'.');
  WriteLn('using: login.code <device> <startupDir> <file2start> [parameters]');
  WriteLn('devices: console, serial, modem, telnet, ssh, ftp, ppp, radius, tacacs, ether');
  Halt(1);
  end;

a:=kicsi(ParamStr(1));
dev:=BVal(a);
if (a='console') then dev:=LoginFlags_accessCon;
if (a='serial') then dev:=LoginFlags_accessSer;
if (a='modem') then dev:=LoginFlags_accessMdm;
if (a='telnet') then dev:=LoginFlags_accessTel;
if (a='ssh') then dev:=LoginFlags_accessSsh;
if (a='ftp') then dev:=LoginFlags_accessFtp;
if (a='ppp') then dev:=LoginFlags_accessPpp;
if (a='radius') then dev:=LoginFlags_accessRad;
if (a='tacacs') then dev:=LoginFlags_accessTac;
if (a='ether') then dev:=LoginFlags_accessEth;

displayFile('c:\system\loginScreen.text');

i:=0;
f1:
WriteLn('');
Write('username: ');
InputOneText(a,0);
Write('password: ');
InputOneText(b,42);

if (authenticateOneUser(a,b,'login at '+paramStr(1),dev,o)<>0) then begin;
  inc(i);
  if (i>maxTries) then delay(10);
  delay(3);
  WriteLn('access denied!');
  if (i>maxTries) then Halt(1);
  goto f1;
  end;
WriteLn('login successful!');
xChDir(ParamStr(2));
BugOS_SetOwnerInfo(o);
BugOS_dropPrivileges;
a:='';
for i:=4 to ParamCount do a:=a+' '+ParamStr(i);
a:=copy(a,2,255);
xExec(ParamStr(3),a,i);
END.