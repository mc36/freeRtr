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

{$include authenticator.inc}
{$include login.inc}
{$include utils.inc}


Var
  a,b:String;
  i,o:LongInt;
  w:Word;
BEGIN;
WriteLn('relogin v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<1) then immErr('using: relogin.code <file2start> [parameters]');

WriteLn('');
Write('username: ');
InputOneText(a,0);
Write('password: ');
InputOneText(b,42);

i:=authenticateOneUser(a,b,'relogin',LoginFlags_accessCon or LoginFlags_accessSer or LoginFlags_accessMdm or LoginFlags_accessTel or LoginFlags_accessSsh,o);
if (i<>0) then begin;
  delay(3);
  immErr('access denied!');
  end;
WriteLn('login successful!');
a:='';
for i:=2 to ParamCount do a:=a+' '+ParamStr(i);
a:=copy(a,2,255);
BugOS_SetOwnerInfo(o);
BugOS_dropPrivileges;
xExec(ParamStr(1),a,w);
WriteLn('your original user rights has been restored!');
END.