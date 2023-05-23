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
{$include emulator-arm.inc}


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
WriteLn('emulator-arm v1.0, done by Mc at '#%date' '#%time'.');
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
    move(regs,buf[2],64);
    move(reg_cpsr,buf[66],sizeof(reg_cpsr));
    p:=69;
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
    move(buf[2],regs,64);
    move(buf[66],reg_cpsr,sizeof(reg_cpsr));
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
    for i:=0 to 12 do putReg(buf,p,4,$00,'r'+BStr(i));
    putReg(buf,p,4,$01,'sp');
    putReg(buf,p,4,$00,'lr');
    putReg(buf,p,4,$02,'pc');
    putReg(buf,p,4,$83,'psr');
    end;
  02:begin; {register bitmap}
    buf[1]:=03;p:=1;
    move(buf[2],i,sizeof(i));
    if (i<>17) then putReg(buf,p,0,32,'reg') else begin;
      putReg(buf,p,31,1,'n');
      putReg(buf,p,30,1,'z');
      putReg(buf,p,29,1,'c');
      putReg(buf,p,28,1,'v');
      putReg(buf,p,27,1,'q');
      putReg(buf,p,24,1,'j');
      putReg(buf,p,19,1,'g3');
      putReg(buf,p,18,1,'g2');
      putReg(buf,p,17,1,'g1');
      putReg(buf,p,16,1,'g0');
      putReg(buf,p,9,1,'e');
      putReg(buf,p,8,1,'a');
      putReg(buf,p,7,1,'i');
      putReg(buf,p,6,1,'f');
      putReg(buf,p,5,1,'t');
      putReg(buf,p,4,1,'m4');
      putReg(buf,p,3,1,'m3');
      putReg(buf,p,2,1,'m2');
      putReg(buf,p,1,1,'m1');
      putReg(buf,p,0,1,'m0');
      end;
    end;
  else immErr('debugger is incompatible!');
  end;
pipeLineSend(pipe,buf,p);
goto f1;
END.