{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc memory.inc}
{$sysinc bignum.inc}

{$define debug}
{$undef debug}

{$include rolror.inc}
{$include bignum.inc}
{$include emulator-mips.inc}


Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Function ResizeMem(i:LongInt):Boolean;
Var
  p:Pointer;
Begin;
ResizeMem:=True;
if (i<1) then exit;
if (ExtendedMemoryResize(p,i)<i) then exit;
memorySize:=i;
memoryData:=p^;
ResizeMem:=False;
End;

Procedure putByte(var buffer;var siz:LongInt;b:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
inc(siz);
buf[siz]:=b;
End;

Procedure putStr(var buffer;var siz:LongInt;b:String);
Var buf:array[1..1] of byte absolute buffer;
Begin;
move(b[1],buf[siz+1],length(b));
inc(siz,length(b));
inc(siz);
buf[siz]:=0;
End;

Procedure putReg(var buf;var siz:LongInt;s,t:LongInt;n:String);
Begin;
putByte(buf,siz,s);
putByte(buf,siz,t);
putStr(buf,siz,n);
End;

Label f1;
Const maxPacketSize=2*1024;
Var
  pipe:Longint;
  buf:array[1..maxPacketSize+32] of byte;
  i,o,p:LongInt;
  w:Word;
BEGIN;
WriteLn('emulator-mips v1.0, done by Mc at '#%date' '#%time'.');
if BigNumStartActions then immErr('failed to find bignum process!');

memorySize:=BVal(ParamStr(1));
i:=BVal(ParamStr(2));
if (i=0) then i:=BugOS_findProcNam(ParamStr(2));
if (i=0) then immErr('using: emulator.code <memsize> <processid>');

if (pipeLineCreate(pipe,i,8192,true)<>0) then immErr('error creating pipeline!');

if ResizeMem(memorySize) then immErr('error allocating memory!');
fillchar(memoryData^,memorySize,0);
emulateRestart;

f1:
p:=sizeof(buf);
if (pipeLineRecv(pipe,buf,p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(pipe,p,o,i);
  if (p=0) then immErr('debugger closed connection!');
  Relequish;
  goto f1;
  end;
case buf[1] of
  06:begin; {execute instruction}
    i:=emulateOneOpcode;
    buf[1]:=07;
    move(i,buf[2],sizeof(i));
    p:=5;
    end;
  04:begin; {read register values}
    buf[1]:=05;
    move(regs,buf[2],128);
    move(reg_hi,buf[130],sizeof(reg_hi));
    move(reg_lo,buf[134],sizeof(reg_lo));
    move(reg_pc,buf[138],sizeof(reg_pc));
    move(reg_npc,buf[142],sizeof(reg_npc));
    p:=145;
    end;
  08:begin; {memory read}
    buf[1]:=09;
    move(buf[2],i,sizeof(i));
    move(buf[6],o,sizeof(o));
    if (i>=memorySize) then i:=memorySize-1;
    if (i<0) then i:=0;
    p:=memorySize-i;
    if (o>p) then o:=p;
    if (o<0) then o:=0;
    if (o>maxPacketSize) then o:=maxPacketSize;
    move(memoryData^[i],buf[2],o);
    p:=o+1;
    end;
  12:begin; {write register values}
    buf[1]:=13;p:=1;
    move(buf[2],regs,128);
    move(buf[130],reg_hi,sizeof(reg_hi));
    move(buf[134],reg_lo,sizeof(reg_lo));
    move(buf[138],reg_pc,sizeof(reg_pc));
    move(buf[142],reg_npc,sizeof(reg_npc));
    end;
  10:begin; {memory write}
    buf[1]:=11;
    move(buf[2],i,sizeof(i));
    o:=p-5;
    if (i>=memorySize) then i:=memorySize-1;
    if (i<0) then i:=0;
    p:=memorySize-i;
    if (o>p) then o:=p;
    if (o<0) then o:=0;
    move(buf[6],memoryData^[i],o);
    p:=1;
    end;
  00:begin; {list of registers}
    buf[1]:=01;p:=1;
    putReg(buf,p,4,$00,'r0');
    putReg(buf,p,4,$00,'at');
    for i:=0 to 1 do putReg(buf,p,4,$00,'v'+BStr(i));
    for i:=0 to 3 do putReg(buf,p,4,$00,'a'+BStr(i));
    for i:=0 to 7 do putReg(buf,p,4,$00,'t'+BStr(i));
    for i:=0 to 7 do putReg(buf,p,4,$00,'s'+BStr(i));
    for i:=8 to 9 do putReg(buf,p,4,$00,'t'+BStr(i));
    for i:=0 to 1 do putReg(buf,p,4,$00,'k'+BStr(i));
    putReg(buf,p,4,$00,'gp');
    putReg(buf,p,4,$03,'sp');
    putReg(buf,p,4,$00,'s8');
    putReg(buf,p,4,$00,'ra');
    putReg(buf,p,4,$00,'hi');
    putReg(buf,p,4,$00,'lo');
    putReg(buf,p,4,$02,'pc');
    putReg(buf,p,4,$00,'npc');
    end;
  02:begin; {register bitmap}
    buf[1]:=03;p:=1;
    move(buf[2],i,sizeof(i));
    putReg(buf,p,0,32,'reg');
    end;
  else immErr('debugger is incompatible!');
  end;
pipeLineSend(pipe,buf,p);
goto f1;
END.