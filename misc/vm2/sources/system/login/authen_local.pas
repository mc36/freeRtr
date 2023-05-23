{$undef debug}
{$heap 7k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$include login.inc}
{$include authenticator.inc}


Label f1,f2,f3,f4,f5;
Var
  req:authenticateRequestRecord;
  rep:authenticateResponseRecord;
  usr:OneLoginUserDataRecord;
  nocase:Boolean;
  pip:LongInt;
  i,o,p:LongInt;
  a,b:String;
  f:xFile;
BEGIN;
WriteLn('authen_local v1.0, done by Mc at '#%date' '#%time'.');
pipeLineBegListen;
BugOS_SignDaemoning;
nocase:=(GetAllParameters='');

f1:
if (pipeLineGetIncoming(pip)<>0) then begin;
  relequish;
  goto f1;
  end;
p:=16;
f2:
dec(p);
if (p<0) then begin;
  f3:
  pipeLineClose(pip);
  goto f1;
  end;
o:=sizeof(req);
if (pipeLineRecv(pip,req,o)<>0) then o:=0;
if (o=0) then begin;
  relequish;
  goto f2;
  end;
if (o<>sizeof(req)) then goto f3;
{$ifdef debug}write('user='+req.user+' pass='+req.pass+' info='+req.info+' service='+BStr(req.service)+' result=');{$endif}

rep.stat:=4;
if (xOpen(f,LoginDatabaseFilename,xGenFilMod_r)<>0) then begin;
  f4:
  {$ifdef debug}writeln('code#'+bstr(rep.stat));{$endif}
  rep.uid:=-1;
  pipeLineSend(pip,rep,sizeof(rep));
  pipeLineClose(pip);
  goto f1;
  end;
b:=req.user;
if nocase then b:=kicsi(b);
for i:=1 to xFileSize(f) div sizeof(usr) do begin;
  if (xBlockRead(f,usr,sizeof(usr))<>0) then goto f4;
  if nocase then begin;
    if (kicsi(usr.userName)=b) then goto f5;
    end else begin;
    if (usr.userName=b) then goto f5;
    end;
  end;
xClose(f);
rep.stat:=3;
goto f4;
f5:
xClose(f);
rep.stat:=2;
if (usr.userID and GuestUserIDmasking=GuestUserIDmasking) then req.pass:=usr.password;
if nocase then begin;
  if (kicsi(usr.password)<>kicsi(req.pass)) then goto f4;
  end else begin;
  if (usr.password<>req.pass) then goto f4;
  end;
rep.stat:=1;
if (usr.flags and req.service=0) then goto f4;
rep.stat:=0;
rep.uid:=usr.userID;
pipeLineSend(pip,rep,sizeof(rep));
pipeLineClose(pip);
{$ifdef debug}writeln('pass');{$endif}
goto f1;
END.