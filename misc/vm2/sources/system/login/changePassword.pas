{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}

{$include utils.inc}
{$include login.inc}
Var
  uid:LongInt;
  dat:OneLoginUserDataRecord;
  fil:xFile;

Procedure findUser(mode:LongInt);
Var i,o:LongInt;
Begin;
if (xOpen(fil,LoginDatabaseFilename,mode)<>0) then immErr('error opening file!');
o:=xFileSize(fil) div sizeof(dat);
for i:=1 to o do begin;
  if (xBlockRead(fil,dat,sizeof(dat))<>0) then immErr('error reading file!');
  if (dat.userID<>uid) then continue;
  if (xSeek(fil,xFilePos(fil)-sizeof(dat))<>0) then immErr('error seeking in file!');
  exit;
  end;
immErr('your user id ('+conv2hex(uid)+') not found!');
End;

Label f1;
Var
  a,b:String;
  i:LongInt;
BEGIN;
WriteLn('password changer v1.0, done by Mc at '#%date' '#%time'.');

BugOS_MyOwnerInfo(uid,i);
if (uid and GuestUserIDmasking=GuestUserIDmasking) then immErr('you are a guest, you cannot change your password!');
BugOS_SetOwnerInfo(0);
findUser(xGenFilMod_r);
xClose(fil);

WriteLn('dear '+dat.realName+'!');
WriteLn('first, you have to enter your current password!');
WriteLn('after it, you will be asked to enter the new one!');
WriteLn('after that, you have to enter again, to be sure');
WriteLn('that nothing was misspelled!');
Write('old password: ');
InputOneText(a,42);
a:=kicsi(a);
if (a<>kicsi(dat.password)) then begin;
  delay(5);
  immErr('wrong!');
  end;

Write('new password: ');
InputOneText(b,42);
Write('again: ');
InputOneText(a,42);
if (kicsi(a)<>kicsi(b)) then immErr('mismatch!');
findUser(xGenFilMod_rw);
dat.password:=b;
if (xBlockWrite(fil,dat,sizeof(dat))<>0) then immErr('error writing file!');
xClose(fil);
WriteLn('successful!');
END.