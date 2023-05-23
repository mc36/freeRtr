Const registerMax=128;
Type
  OneRegisterListRecord=record
    n:LongInt;
    d:array[1..registerMax] of record
      nam:String[63];
      pos:LongInt;
      siz:LongInt;
      val:LongInt;
      end;
    end;
  OneDebuggerBuffer=array[1..2*1024] of byte;
Var
  debuggerPipe:LongInt;
  debuggerProc:LongInt;
  registerDat:OneRegisterListRecord;
  registerOld:OneRegisterListRecord;
  flagsRegDat:OneRegisterListRecord;
  stackRegIdx:LongInt;
  instrRegIdx:LongInt;
  flagsRegIdx:longInt;
  myProcessID:LongInt;



Procedure debug_exchangePack(var d:OneDebuggerBuffer;var s:LongInt);
Var i,o:LongInt;
Begin;
if (s>sizeof(d)) then s:=sizeof(d);
pipeLineSend(debuggerPipe,d,s);
repeat
  Relequish;
  s:=sizeof(d);
  if (pipeLineRecv(debuggerPipe,d,s)<>0) then s:=0;
  if (s<1) then begin;
    if (pipeLineStats(debuggerPipe,i,o,o)<>0) then i:=0;
    if (i<>debuggerProc) then immErr('emulator process died!');
    continue;
    end;
  until (s<>0);
End;



Procedure debug_decodeList(var buffer;p,s:LongInt;var d:OneRegisterListRecord);
Var
  buf:array[1..1] of byte absolute buffer;
  i,o:LongInt;
  a:String;

function get:LongInt;
begin;
inc(p);
get:=buf[p];
if (p>s) then get:=0;
end;

Begin;
d.n:=0;
while (p<s) do begin;
  inc(d.n);
  d.d[d.n].siz:=get;
  d.d[d.n].pos:=get;
  a:='';
  repeat
    i:=get;
    a:=a+chr(i);
    until (i=0);
  d.d[d.n].nam:=copy(a,1,length(a)-1);
  end;
End;



Function putNextVal(var buffer;var p:LongInt;v,s:LongInt):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  w:Word;
Begin;
case s of
  1:begin;
    buf[p+1]:=v;
    inc(p);
    end;
  2:begin;
    w:=v;
    move(w,buf[p+1],sizeof(w));
    inc(p,sizeof(w));
    end;
  4:begin;
    move(v,buf[p+1],sizeof(v));
    inc(p,sizeof(v));
    end;
  else for v:=1 to s do begin;
    inc(p);
    buf[p]:=0;
    end;
  end;
End;


Function getNextVal(var buffer;var p:LongInt;s:LongInt):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  l:LongInt;
  w:Word;
Begin;
case s of
  1:begin;
    getNextVal:=buf[p+1];
    end;
  2:begin;
    move(buf[p+1],w,sizeof(w));
    getNextVal:=w;
    end;
  4:begin;
    move(buf[p+1],l,sizeof(l));
    getNextVal:=l;
    end;
  else getNextVal:=-1;
  end;
inc(p,s);
End;



Procedure debug_readRegNames(var dat:OneRegisterListRecord);
Var
  d:OneDebuggerBuffer;
  i,o:LongInt;
Begin;
fillchar(dat,sizeof(dat),0);
i:=1;
d[1]:=00;
debug_exchangePack(d,i);
debug_decodeList(d,1,i,dat);
stackRegIdx:=registerMax;
instrRegIdx:=registerMax;
flagsRegIdx:=registerMax;
dat.d[registerMax].siz:=1;
for i:=1 to dat.n do begin;
  case dat.d[i].pos and 7 of
    1:stackRegIdx:=i;
    2:instrRegIdx:=i;
    3:flagsRegIdx:=i;
    end;
  end;
End;



Procedure debug_readBitNames(i:LongInt;var dat:OneRegisterListRecord);
Var
  d:OneDebuggerBuffer;
  o:LongInt;
Begin;
d[1]:=02;
move(i,d[2],sizeof(i));
i:=5;
debug_exchangePack(d,i);
debug_decodeList(d,1,i,dat);
for i:=1 to dat.n do begin;
  o:=dat.d[i].siz;
  dat.d[i].siz:=dat.d[i].pos;
  dat.d[i].pos:=o;
  end;
End;



Procedure debug_readRegsVal(var dat:OneRegisterListRecord);
Var
  d:OneDebuggerBuffer;
  i,o:LongInt;
Begin;
d[1]:=04;
i:=1;
debug_exchangePack(d,i);
o:=1;
for i:=1 to dat.n do dat.d[i].val:=getNextVal(d,o,dat.d[i].siz);
End;



Procedure debug_writeRegsVal(var dat:OneRegisterListRecord);
Var
  d:OneDebuggerBuffer;
  i,o:LongInt;
Begin;
d[1]:=12;
o:=1;
for i:=1 to dat.n do putNextVal(d,o,dat.d[i].val,dat.d[i].siz);
debug_exchangePack(d,o);
End;



Function debug_execute:LongInt;
Var
  d:OneDebuggerBuffer;
  i,o:LongInt;
Begin;
d[1]:=06;
i:=1;
debug_exchangePack(d,i);
move(d[2],i,sizeof(i));
debug_execute:=i;
End;



Procedure debug_readMem(ofs,siz:LongInt;var buffer);
Var
  d:OneDebuggerBuffer;
  i,o:LongInt;
Begin;
d[1]:=08;
move(ofs,d[2],sizeof(ofs));
move(siz,d[6],sizeof(siz));
i:=9;
debug_exchangePack(d,i);
move(d[2],buffer,i-1);
End;



Procedure debug_writeMem(ofs,siz:LongInt;var buffer);
Var
  d:OneDebuggerBuffer;
  i,o:LongInt;
Begin;
d[1]:=10;
move(ofs,d[2],sizeof(ofs));
move(buffer,d[6],siz);
i:=siz+5;
debug_exchangePack(d,i);
End;
