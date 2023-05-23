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
{$include emulator-powerpc.inc}


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
WriteLn('emulator-powerpc v1.0, done by Mc at '#%date' '#%time'.');
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
    move(reg_msr,buf[130],sizeof(reg_msr));
    move(reg_cr,buf[134],sizeof(reg_cr));
    move(reg_xer,buf[138],sizeof(reg_xer));
    move(reg_lr,buf[142],sizeof(reg_lr));
    move(reg_ctr,buf[146],sizeof(reg_ctr));
    move(reg_iar,buf[150],sizeof(reg_iar));
    move(reg_res,buf[154],sizeof(reg_res));
    p:=157;
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
    move(buf[130],reg_msr,sizeof(reg_msr));
    move(buf[134],reg_cr,sizeof(reg_cr));
    move(buf[138],reg_xer,sizeof(reg_xer));
    move(buf[142],reg_lr,sizeof(reg_lr));
    move(buf[146],reg_ctr,sizeof(reg_ctr));
    move(buf[150],reg_iar,sizeof(reg_iar));
    move(buf[154],reg_res,sizeof(reg_res));
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
    for i:=0 to 31 do putReg(buf,p,4,$00,'r'+BStr(i));
    putReg(buf,p,4,$00,'msr');
    putReg(buf,p,4,$83,'cr');
    putReg(buf,p,4,$80,'xer');
    putReg(buf,p,4,$00,'lr');
    putReg(buf,p,4,$00,'ctr');
    putReg(buf,p,4,$02,'iar');
    putReg(buf,p,4,$00,'res');
    end;
  02:begin; {register bitmap}
    buf[1]:=03;p:=1;
    move(buf[2],i,sizeof(i));
    case i of
      1..32:putReg(buf,p,0,32,'r'+BStr(i-1));
      33:begin;
        putReg(buf,p,0,1,'le');
        putReg(buf,p,1,1,'ri');
        putReg(buf,p,6,1,'ip');
        putReg(buf,p,8,1,'fe1');
        putReg(buf,p,9,1,'be');
        putReg(buf,p,10,1,'se');
        putReg(buf,p,11,1,'fe0');
        putReg(buf,p,12,1,'me');
        putReg(buf,p,13,1,'fp');
        putReg(buf,p,14,1,'pr');
        putReg(buf,p,15,1,'ee');
        putReg(buf,p,16,1,'le');
        end;
      34:begin;
        for i:=0 to 7 do begin;
          o:=i*(-4)+28;
          putReg(buf,p,o+0,1,'so'+BStr(i));
          putReg(buf,p,o+1,1,'eq'+BStr(i));
          putReg(buf,p,o+2,1,'gt'+BStr(i));
          putReg(buf,p,o+3,1,'lt'+BStr(i));
          end;
        end;
      35:begin;
        putReg(buf,p,0,7,'bytes');
        putReg(buf,p,29,1,'ca');
        putReg(buf,p,30,1,'ov');
        putReg(buf,p,31,1,'so');
        end;
      36:putReg(buf,p,0,32,'lr');
      37:putReg(buf,p,0,32,'ctr');
      38:putReg(buf,p,0,32,'iar');
      39:putReg(buf,p,0,32,'res_addr');
      end;
    end;
  else immErr('debugger is incompatible!');
  end;
pipeLineSend(pipe,buf,p);
goto f1;
END.