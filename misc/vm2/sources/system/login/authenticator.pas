{$undef debug}
{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc datetime.inc}

{$include login.inc}
{$include authenticator.inc}

Var
  logFileName:String;
  logFileDate:LongInt;
  logFileHndr:xtText;
  logFileLock:Boolean;
  authListCode:String;
  authListName:String;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function getCurrentDateBin:LongInt;
Var a,b,c:Word;
Begin;
xGetDate(a,b,c);
getCurrentDateBin:=(c shl 16) or (b shl 8) or c;
End;

Function padUpWithZeros(i:LongInt):String;
Var a:String;
Begin;
a:=BStr(i);
while (length(a)<2) do a:='0'+a;
padUpWithZeros:=a;
End;

Function getCurrentDateStr:String;
Var a,b,c:Word;
Begin;
xGetDate(a,b,c);
getCurrentDateStr:=padUpWithZeros(a)+'-'+padUpWithZeros(b)+'-'+padUpWithZeros(c);
End;

Function getCurrentTimeStr:String;
Var a,b,c:Word;
Begin;
xGetTime(a,b,c);
getCurrentTimeStr:=padUpWithZeros(a)+':'+padUpWithZeros(b)+':'+padUpWithZeros(c);
End;


Procedure append2log(act,usr,frm,dat:String);
Var
  i:LongInt;
  a:String;
Begin;
WriteLn(act+' '+usr+' '+dat+' '+frm);
if (logFileName='') then exit;
i:=getCurrentDateBin;
if (i<>logFileDate) then begin;
  logFileDate:=i;
  xtClose(logFileHndr);
  a:=logFileName;
  i:=pos('%',a);
  if (i>0) then a:=copy(a,1,i-1)+getCurrentDateStr+copy(a,i+1,666);
  xCreate(a);
  if (xtOpen(logFileHndr,a,false)<>0) then immErr('error opening '+a+'!');
  end;
xtWrite(logFileHndr,getCurrentTimeStr+' '+act);
xtWrite(logFileHndr,' '+usr);
xtWrite(logFileHndr,' '+dat);
xtWriteLn(logFileHndr,' '+frm);
if logFileLock then exit;
xtClose(logFileHndr);
dec(logFileDate);
End;


Procedure ReadUpConfig(a:String);
Var t:xtText;

function gnl:string;
var
  i:longint;
  a:String;
begin;
a:=xtReadLn(t,255);
i:=pos(';',a);
if (i>0) then a:=copy(a,1,i-1);
a:=' '+a+' ';
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
gnl:=a;
end;

Var
  b:String;
  i:LongInt;
Begin;
WriteLn('reading '+a+'...');
if (xtOpen(t,a,true)<>0) then immErr('error opening!');
logFileName:=gnl;
logFileLock:=(BVal(gnl)<>0);
authListCode:=gnl;
authListName:='';
while not xtEOF(t) do begin;
  a:=gnl;
  if (a='') then continue;
  authListName:=authListName+#13+a;
  end;
xtClose(t);
kicserel(',',' ',authListCode);
kicserel(' ',#13,authListCode);
authListCode:=#13+authListCode+#13;
authListName:=copy(authListName,2,666);
End;






Label f1,f2,f3,f4,f5;
Var
  req:authenticateRequestRecord;
  rep:authenticateResponseRecord;
  pip:LongInt;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('authenticator v1.0, done by Mc at '#%date' '#%time'.');
ReadUpConfig(GetAllParameters);
pipeLineBegListen;
BugOS_SignDaemoning;


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
{$ifdef debug}writeln('user='+req.user+' pass='+req.pass+' info='+req.info+' service='+getAuthenticationServiceName(req.service));{$endif}
rep.stat:=5;
rep.uid:=-1;
b:=authListName;
authenticateCurrentProcess:='';
f5:
i:=pos(#13,b);
if (i<1) then i:=666;
authenticateCurrentProcess:=copy(b,1,i-1);
b:=copy(b,i+1,666);
if (authenticateCurrentProcess='') then goto f4;
{$ifdef debug}write('auther='+authenticateCurrentProcess);{$endif}
rep.stat:=authenticateOneUser(req.user,req.pass,req.info,req.service,rep.uid);
{$ifdef debug}writeln(' code='+BStr(rep.stat)+' uid='+BStr(rep.uid));{$endif}
if (rep.stat=0) then goto f4;
if (pos(#13+BStr(rep.stat)+#13,authListCode)<>0) then goto f5;
f4:
pipeLineSend(pip,rep,sizeof(rep));
pipeLineClose(pip);
if (rep.stat=0) then a:='uid='+BStr(rep.uid) else a:='fail='+BStr(rep.stat);
append2log(a+' '+getAuthenticationServiceName(req.service),req.user,req.info,authenticateCurrentProcess);
goto f1;
END.