{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}

Const maxChanNum=128;
Var
  chanNum:LongInt;
  chanPip:array[1..maxChanNum] of LongInt;
  upper:LongInt;

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;


Label f1,f2;
Var
  buf:array[1..4*1024] of byte;
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn('channel merger v1.0, done by Mc at '#%date' '#%time'.');
chanNum:=paramCount;
if (chanNum<2) then immErr('usage: cm.code <proc1> <proc2> [procN]');
WriteLn('opening processes...');
for i:=1 to chanNum do begin;
  Write(#13'ch#'+BStr(i)+'... ');
  a:=paramStr(i);
  o:=BVal(a);
  if (o=0) then o:=BugOS_findProcNam(a);
  if (o=0) then immErr('process not found!');
  if (pipeLineCreate(chanPip[i],o,65536,true)<>0) then immErr('unabled to create pipeline!');
  end;
Write(#13'                  '#13);
for i:=1 to 16 do relequish;
for p:=1 to chanNum do begin;
  Write('ch#'+BStr(p)+': ');
  o:=sizeof(buf);
  if (pipeLineRecv(chanPip[p],buf,o)<>0) then o:=0;
  if (o<1) then immErr('initial packet not received!');
  move(buf[1],i,sizeof(i));
  o:=i+i+17;
  a:='';
  while (buf[o]<>0) do begin;
    a:=a+chr(buf[o]);
    inc(o);
    end;
  WriteLn(a);
  end;
WriteLn(BStr(chanNum)+' channels opened!');

pipeLineBegListen;
BugOS_SignDaemoning;
upper:=0;

f1:
relequish;
for p:=1 to chanNum do while (1=1) do begin;
  o:=sizeof(buf);
  if (pipeLineRecv(chanPip[p],buf[2],o)<>0) then o:=0;
  if (o<1) then begin;
    pipeLineStats(chanPip[p],o,i,i);
    if (o=0) then immErr('channel '+BStr(p)+' exited!');
    break;
    end;
  buf[1]:=p;
  pipeLineSend(upper,buf,o+1);
  end;
if (upper=0) then begin;
  if (pipeLineGetIncoming(upper)<>0) then goto f1;
  a:=BStr(chanNum)+' channels merged';
  a:='12341234'#0#0#0#0#0#0#0#0+#0#255+a+#0;
  i:=1;move(i,a[1],sizeof(i));
  i:=1500;move(i,a[5],sizeof(i));
  WriteLn('upper logged in!');
  pipeLineSend(upper,a[1],length(a));
  goto f1;
  end;
f2:
o:=sizeof(buf);
if (pipeLineRecv(upper,buf,o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(upper,o,i,i);
  if (o<>0) then goto f1;
  WriteLn('upper logged out!');
  pipeLineClose(upper);
  upper:=0;
  goto f1;
  end;
i:=buf[1];
if (i<1) or (i>chanNum) then begin;
  WriteLn('upper wanted to send not existing channel!');
  goto f2;
  end;
pipeLineSend(chanPip[i],buf[2],o-1);
goto f2;
END.