Var
  soundPipe:LongInt;
  soundSize:LongInt;

Function soundXchg(Var buf;siz:LongInt):LongInt;
Label f1;
Var i,o:LongInt;
Begin;
if (siz>0) then pipeLineSend(soundPipe,buf,siz);
f1:
relequish;
siz:=1024*32;
if (pipeLineRecv(soundPipe,buf,siz)<>0) then siz:=0;
if (siz<1) then begin;
  pipeLineStats(soundPipe,o,i,i);
  if (o=0) then immErr('sound device closed connection!');
  goto f1;
  end;
soundXchg:=siz;
End;

Procedure soundOpen(a:String);
Var buf:array[1..512] of byte;
Begin;
pipeLineClose(soundPipe);
soundPipe:=BVal(a);
if (soundPipe=0) then soundPipe:=BugOS_findProcNam(a);
if (soundPipe=0) then immErr('process not found!');
if (pipeLineCreate(soundPipe,soundPipe,65536,true)<>0) then immErr('error opening pipeline!');
buf[1]:=5;
soundXchg(buf,1);
move(buf,soundSize,sizeof(soundSize));
End;

Function soundDial(a:String):Boolean;
Var buf:array[1..512] of byte;
Begin;
move(a,buf,sizeof(a));
buf[1]:=9;
soundXchg(buf,length(a)+1);
soundDial:=(buf[1]=1);
End;

Function soundHangup:Boolean;
Var buf:array[1..512] of byte;
Begin;
buf[1]:=10;
soundXchg(buf,1);
soundHangup:=(buf[1]=1);
End;

Procedure soundFedup;
Var buf:array[1..32*1024] of byte;
Begin;
fillchar(buf,sizeof(buf),0);
buf[1]:=6;
pipeLineSend(soundPipe,buf,soundSize+1);
pipeLineSend(soundPipe,buf,soundSize+1);
End;

Procedure soundFlush;
Var
  buf:array[1..32*1024] of byte;
  i,o:LongInt;
Begin;
fillchar(buf,sizeof(buf),0);
buf[1]:=7;
for i:=1 to 176400 div soundSize do pipeLineSend(soundPipe,buf,1);
buf[1]:=5;
pipeLineSend(soundPipe,buf,1);
repeat until (soundXchg(buf,0)<soundSize);
End;
