{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc param.inc}

{$include utils.inc}
{$include login.inc}

Label f1;
Var
  f:xFile;
  d:OneLoginUserDataRecord;
  i,o,p:LongInt;
  a:String;
  w:Word;
BEGIN;
WriteLn('password asker v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<1) then immErr('using: askpass.code <file2start> [parameters]');
BugOS_MyOwnerInfo(p,o);
BugOS_SetOwnerInfo(0);
if (xOpen(f,LoginDatabaseFilename,xGenFilMod_r)<>0) then immErr('error opening file!');
o:=xFileSize(f) div sizeof(d);
for i:=1 to o do begin;
  if (xBlockRead(f,d,sizeof(d))<>0) then immErr('error reading file!');
  if (d.userID=p) then goto f1;
  end;
immErr('your user id ('+conv2hex(p)+') not found!');
f1:
xClose(f);
BugOS_SetOwnerInfo(p);
BugOS_dropPrivileges;
WriteLn('dear '+d.realName+'!');
WriteLn('please enter your password!');
Write('password: ');
InputOneText(a,42);
a:=kicsi(a);
if (a<>kicsi(d.password)) then begin;
  delay(5);
  immErr('wrong!');
  end;
WriteLn('login successful!');
a:='';
for i:=2 to ParamCount do a:=a+' '+ParamStr(i);
a:=copy(a,2,255);
xExec(ParamStr(1),a,w);
END.