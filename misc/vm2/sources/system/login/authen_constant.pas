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
  cod,uid:LongInt;
  pip:LongInt;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn('authen_constant v1.0, done by Mc at '#%date' '#%time'.');
pipeLineBegListen;
BugOS_SignDaemoning;
a:=paramStr(1);
cod:=BVal(a);
if (cod=0) and (a<>'0') then cod:=4;
a:=paramStr(2);
uid:=BVal(a);
if (uid=0) and (a<>'0') then uid:=-1;


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
{$ifdef debug}writeln('user='+req.user+' pass='+req.pass+' info='+req.info+' service='+BStr(req.service)+' result='+BStr(cod));{$endif}

rep.stat:=cod;
rep.uid:=uid;
pipeLineSend(pip,rep,sizeof(rep));
pipeLineClose(pip);
goto f1;
END.