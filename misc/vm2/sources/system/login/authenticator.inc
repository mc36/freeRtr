Type
  authenticateRequestRecord=record
    service:LongInt;
    user:String;
    pass:String;
    info:String;
    end;
  authenticateResponseRecord=record
    stat:LongInt;
    uid:LongInt;
    end;
Const authenticateCurrentProcess:String='authenticator.code';


Function authenticateOneUser(user,pass,info:String;service:LongInt;var uid:LongInt):LongInt;
Label f1,f2;
Var
  i,o:LongInt;
  pipe,try:LongInt;
  req:authenticateRequestRecord;
  rep:authenticateResponseRecord absolute req;
Begin;
authenticateOneUser:=3;
uid:=-1;
i:=BugOS_findProcNam(authenticateCurrentProcess);
if (i=0) then exit;
relequish;
if (pipeLineCreate(pipe,i,4096,true)<>0) then exit;
req.service:=service;
req.user:=user;
req.pass:=pass;
req.info:=info;
i:=pipeLineSend(pipe,req,sizeof(req));
fillchar(user,sizeof(user),0);
fillchar(pass,sizeof(pass),0);
fillchar(req,sizeof(req),0);
if (i<>0) then goto f2;
try:=0;
f1:
relequish;
o:=sizeof(req);
if (pipeLineRecv(pipe,rep,o)<>0) then o:=0;
if (o=sizeof(rep)) then begin;
  authenticateOneUser:=rep.stat;
  uid:=rep.uid;
  f2:
  pipeLineClose(pipe);
  exit;
  end;
if (o<>0) then goto f2;
pipeLineStats(pipe,o,i,i);
if (o<>0) then goto f1;
inc(try);
if (try>8) then goto f2;
goto f1;
End;
