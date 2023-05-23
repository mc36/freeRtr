{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc crt.inc}

Const maxBuf=32*1024;

Type
  onePeerRecord=record
    pipe:LongInt;
    size:LongInt;
    saw:LongInt;
    used:LongInt;
    data:array[1..maxBuf] of byte;
    end;
Var
  p1,p2:onePeerRecord;
  underrun:LongInt;
  buf:array[1..maxBuf] of byte;
  i,o:LongInt;
  a:String;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function exchange(pipe:LongInt;Var buf;siz:LongInt):LongInt;
Label f1;
Var i,o:LongInt;
Begin;
pipeLineSend(pipe,buf,siz);
f1:
relequish;
siz:=1024*32;
if (pipeLineRecv(pipe,buf,siz)<>0) then siz:=0;
if (siz<1) then begin;
  pipeLineStats(pipe,o,i,i);
  if (o=0) then immErr('sound device closed connection!');
  goto f1;
  end;
exchange:=siz;
End;

Procedure deleteFromBuffer(var buffer;var siz:LongInt;len:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
if (siz<0) then siz:=0;
if (len>siz) then len:=siz;
if (len<1) then exit;
dec(siz,len);
move(buf[len+1],buf,siz);
End;

Procedure releq2peer(var loc,rem:onePeerRecord);
Begin;
o:=sizeof(buf);
pipeLineRecv(loc.pipe,buf,o);
if (o<1) then begin;
  pipeLineStats(loc.pipe,o,i,i);
  if (o=0) then immErr('sound device closed connection!');
  exit;
  end;
deleteFromBuffer(rem.data,rem.used,loc.size-maxBuf+rem.used);
move(buf,rem.data[rem.used+1],loc.size);
inc(rem.used,loc.size);
inc(loc.saw,loc.size);
buf[1]:=8;
move(loc.data,buf[2],loc.size);
if (loc.used<loc.size) then begin;
  fillchar(buf[loc.used+2],loc.size-loc.used,0);
  inc(underrun);
  end;
pipeLineSend(loc.pipe,buf,loc.size+1);
deleteFromBuffer(loc.data,loc.used,loc.size);
End;

Label f1,f2;
BEGIN;
WriteLn('connecter v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<2) then immErr('using: conn.code <process1> <process2> [number1] [number2]');

fillchar(p1,sizeof(p1),0);
fillchar(p2,sizeof(p2),0);

a:=paramStr(1);
Write('opening process...');
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then immErr('process not found!');
if (pipeLineCreate(p1.pipe,i,65536,true)<>0) then immErr('error opening pipeline!');
buf[1]:=5;
exchange(p1.pipe,buf,1);
move(buf,p1.size,sizeof(i));
WriteLn(' size='+BStr(p1.size));

a:=paramStr(2);
Write('opening process...');
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then immErr('process not found!');
if (pipeLineCreate(p2.pipe,i,65536,true)<>0) then immErr('error opening pipeline!');
buf[1]:=5;
exchange(p2.pipe,buf,1);
move(buf,p2.size,sizeof(i));
WriteLn(' size='+BStr(p2.size));

a:=paramStr(3);
if (a<>'') then begin;
  WriteLn('dialing '+a+'...');
  move(a,buf,sizeof(a));
  buf[1]:=9;
  exchange(p1.pipe,buf,length(a)+1);
  if (buf[1]=1) then a:='successful!' else a:='failed!';
  WriteLn(a);
  end;

a:=paramStr(4);
if (a<>'') then begin;
  WriteLn('dialing '+a+'...');
  move(a,buf,sizeof(a));
  buf[1]:=9;
  exchange(p2.pipe,buf,length(a)+1);
  if (buf[1]=1) then a:='successful!' else a:='failed!';
  WriteLn(a);
  end;

fillchar(buf,sizeof(buf),0);
buf[1]:=8;
pipeLineSend(p1.pipe,buf,p1.size+1);
pipeLineSend(p1.pipe,buf,p1.size+1);
pipeLineSend(p2.pipe,buf,p2.size+1);
pipeLineSend(p2.pipe,buf,p2.size+1);
underrun:=0;

f1:
relequish;
Write(BStr(p1.saw)+'  '+BStr(p2.saw)+'  (under='+BStr(underrun)+')'#13);
if keypressed then goto f2;
releq2peer(p1,p2);
releq2peer(p2,p1);
goto f1;
f2:
WriteLn('');
Halt(0);
END.