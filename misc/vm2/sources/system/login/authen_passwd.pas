{$undef debug}
{$heap 7k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$include authenticator.inc}


Label f1,f2,f3;
Var
  req:authenticateRequestRecord;
  rep:authenticateResponseRecord;
  uid:LongInt;
  pwd:String;
  pip:LongInt;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn('authen_passwd v1.0, done by Mc at '#%date' '#%time'.');
pipeLineBegListen;
BugOS_SignDaemoning;
a:=paramStr(1);
uid:=BVal(a);
if (uid=0) and (a<>'0') then uid:=-1;
pwd:='';
for i:=2 to paramCount do pwd:=pwd+' '+paramStr(i);
pwd:=copy(pwd,2,666);

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
if (req.pass=pwd) then begin;
  rep.stat:=0;
  rep.uid:=uid;
  end else begin;
  rep.stat:=2;
  rep.uid:=-1;
  end;
{$ifdef debug}writeln('user='+req.user+' pass='+req.pass+' info='+req.info+' service='+BStr(req.service)+' result='+BStr(rep.stat)+' uid='+BStr(rep.uid));{$endif}

pipeLineSend(pip,rep,sizeof(rep));
pipeLineClose(pip);
goto f1;
END.