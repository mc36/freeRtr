{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}

Type
  OneConnectionRecord=record
    pipe:LongInt;
    user:String;
    end;
Var
  ConnectionDat:^array[1..1] of OneConnectionRecord;
  ConnectionNum:LongInt;
  MyName:String;
  currMode:Byte; {1=cmd, 2=txt}
  sysMsgs:Boolean;

Function getWord(var b:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',b);
if (i<1) then i:=$666;
getWord:=kicsi(copy(b,1,i-1));
b:=copy(b,i+1,255);
End;

Function ResizeMem(n:LongInt):Boolean;
Var
  p:Pointer;
  i:LongInt;
Begin;
ResizeMem:=True;
i:=n*sizeof(OneConnectionRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnectionNum:=n;
ConnectionDat:=p^;
ResizeMem:=False;
End;

Function Receiver:String;
Label f1;
Var
  i,o,p:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
Begin;
Receiver:='';
if (pipeLineGetIncoming(p)=0) then begin;
  if ResizeMem(ConnectionNum+1) then begin;
    Receiver:='failed to allocate memory!';
    pipeLineClose(p);
    exit;
    end;
  fillchar(ConnectionDat^[ConnectionNum],sizeof(ConnectionDat^[ConnectionNum]),0);
  ConnectionDat^[ConnectionNum].pipe:=p;
  a:=#2+MyName;
  pipeLineSend(p,a[1],length(a));
  if sysMsgs then Receiver:='new user arrived!';
  exit;
  end;
for p:=ConnectionNum downto 1 do begin;
  i:=sizeof(a)-1;
  if (pipeLineRecv(ConnectionDat^[p].pipe,a[1],i)<>0) then i:=0;
  if (i>0) then goto f1;
  pipeLineStats(ConnectionDat^[p].pipe,o,i,i);
  if (o<>0) then continue;
  if sysMsgs then Receiver:=ConnectionDat^[p].user+' logged out';
  pipeLineClose(ConnectionDat^[p].pipe);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[p],sizeof(OneConnectionRecord));
  ResizeMem(ConnectionNum-1);
  exit;
  end;
exit;
f1:
ab[0]:=i;
case ab[1] of
  1:begin; {text}
    Receiver:=ConnectionDat^[p].user+'> '+copy(a,2,255);
    end;
  2:begin; {nick}
    a:=copy(a,2,255);
    if sysMsgs then Receiver:=ConnectionDat^[p].user+' changed nick to '+a;
    ConnectionDat^[p].user:=a;
    end;
  end;
End;

Procedure Broadcast(a:string);
Var i,o:LongInt;
Begin;
o:=length(a);
for i:=1 to ConnectionNum do pipeLineSend(ConnectionDat^[i].pipe,a[1],o);
End;


Function ReadLine:String;
Label f1;
Var
  a,b:String;
  w:Word;

procedure clr;
begin;
write(#13'    ');
for w:=1 to length(a) do write(' ');
write(#13);
end;

procedure prmt;
begin;
if (currMode=1) then write(#13'cmd>') else write(#13'txt>');
write(a);
end;

Begin;
a:='';
prmt;
f1:
while not keypressed do begin;
  relequish;
  b:=Receiver;
  if (b='') then continue;
  clr;
  WriteLn(b);
  while (1=1) do begin;
    b:=Receiver;
    if (b='') then break;
    WriteLn(b);
    end;
  prmt;
  end;
w:=ReadKey;
if (w and $fe00=0) then begin;{simple key}
  w:=w and $ff;
  if (w in [0,255,13,10,8,9]) then w:=ord(' ');
  if (length(a)>200) then goto f1;
  a:=a+chr(w);
  write(chr(w));
  goto f1;
  end;
case w of
  $8001:begin;{redraw}
    clr;
    prmt;
    goto f1;
    end;
  $8003:begin;{backspace}
    if (a='') then goto f1;
    Write(#8' '#8);
    a:=copy(a,1,length(a)-1);
    goto f1;
    end;
  $8004:begin;{enter}
    clr;
    ReadLine:=a;
    exit;
    end;
  $8005:begin;{escape}
    clr;
    a:='';
    prmt;
    goto f1;
    end;
  $8002:begin;{tab}
    currMode:=3-currMode;
    clr;
    prmt;
    goto f1;
    end;
  end;
goto f1;
End;

Procedure DetectOthers;
Var
  buf:record
    n:string;
    p:string;
    end;
  a:String;
  i,o,p,m:LongInt;
Begin;
BugOS_MyProcessInfo(m,i,o);
BugOS_ProcessName(m,buf,i,o,p);
a:=buf.n;
BugOS_totalSysInfo(p,i,i);
for o:=1 to p do begin;
  i:=BugOS_findProcNum(o);
  BugOS_ProcessName(i,buf,p,p,p);
  if (buf.n<>a) then continue;
  if (i=m) then continue;
  if ResizeMem(ConnectionNum+1) then continue;
  fillchar(ConnectionDat^[ConnectionNum],sizeof(ConnectionDat^[ConnectionNum]),0);
  pipeLineCreate(ConnectionDat^[ConnectionNum].pipe,i,1024,true);
  end;
End;


Label f1;
Var
  a,b:string;
  i,o,p:LongInt;
BEGIN;
WriteLn('talker v1.0, done by Mc at '#%date' '#%time'.');
currMode:=2;
ConnectionNum:=0;
MyName:='';
sysMsgs:=false;
WriteLn('please enter your nick name:');
MyName:=ReadLine;
WriteLn('press tab to toggle command mode!');

pipeLineBegListen;
DetectOthers;
Broadcast(#2+MyName);

f1:
b:=ReadLine;
if (b='') then goto f1;
if (currMode=2) then begin;
  Broadcast(#1+b);
  WriteLn(MyName+'> '+b);
  goto f1;
  end;
a:=getWord(b);
if (a='?') or (a='help') then begin;
  WriteLn('commands:');
  WriteLn('help             - this screen');
  WriteLn('exit             - leave this code');
  WriteLn('nosys            - hide system messages');
  WriteLn('dosys            - show system messages');
  WriteLn('users            - list of users');
  WriteLn('nick <name>      - change nick name');
  WriteLn('priv <usr> <txt> - send private message to user');
  goto f1;
  end;
if (a='exit') then exit;
if (a='nosys') then begin;
  sysMsgs:=false;
  WriteLn('system messages disabled!');
  goto f1;
  end;
if (a='dosys') then begin;
  sysMsgs:=true;
  WriteLn('system messages enabled!');
  goto f1;
  end;
if (a='nick') then begin;
  MyName:=b;
  Broadcast(#2+MyName);
  WriteLn('nick changed!');
  goto f1;
  end;
if (a='users') then begin;
  WriteLn('list of users: ('+BStr(ConnectionNum+1)+')');
  for i:=1 to ConnectionNum do WriteLn(ConnectionDat^[i].user);
  WriteLn(MyName);
  goto f1;
  end;
if (a='priv') then begin;
  a:=getWord(b);
  for i:=1 to ConnectionNum do if (kicsi(ConnectionDat^[i].user)=a) then begin;
    a:=#1+b;
    pipeLineSend(ConnectionDat^[i].pipe,a[1],length(a));
    WriteLn('private message sent!');
    goto f1;
    end;
  WriteLn('user not found!');
  goto f1;
  end;

WriteLn('bad command: '+a+' '+b);
goto f1;
END.