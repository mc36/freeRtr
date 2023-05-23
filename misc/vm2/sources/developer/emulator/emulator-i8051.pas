{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc memory.inc}

{$define debug}
{$undef debug}



{$include emulator-i8051.inc}

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
WriteLn('emulator-i8051 v1.0, done by Mc at '#%date' '#%time'.');

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
    buf[2]:=regs_a;
    move(regs,buf[3],sizeof(regs));
    move(regs_pc,buf[11],sizeof(regs_pc));
    move(regs_sp,buf[13],sizeof(regs_sp));
    i:=0;
    if flg_c then inc(i,1);
    if flg_v then inc(i,2);
    if flg_p then inc(i,4);
    buf[15]:=i;
    p:=15;
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
    regs_a:=buf[2];
    move(buf[3],regs,sizeof(regs));
    move(buf[11],w,sizeof(w));regs_pc:=w;
    move(buf[13],w,sizeof(w));regs_sp:=w;
    i:=buf[14];
    flg_c:=(i and 1<>0);
    flg_v:=(i and 2<>0);
    flg_p:=(i and 4<>0);
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
    putReg(buf,p,1,$00,'a');
    putReg(buf,p,1,$00,'r0');
    putReg(buf,p,1,$00,'r1');
    putReg(buf,p,1,$00,'r2');
    putReg(buf,p,1,$00,'r3');
    putReg(buf,p,1,$00,'r4');
    putReg(buf,p,1,$00,'r5');
    putReg(buf,p,1,$00,'r6');
    putReg(buf,p,1,$00,'r7');
    putReg(buf,p,2,$02,'pc');
    putReg(buf,p,2,$01,'sp');
    putReg(buf,p,1,$83,'flg');
    end;
  02:begin; {register bitmap}
    buf[1]:=03;p:=1;
    move(buf[2],i,sizeof(i));
    case i of
      1:begin; putReg(buf,p,0,8,'a'); end;
      2:begin; putReg(buf,p,0,8,'r0'); end;
      3:begin; putReg(buf,p,0,8,'r1'); end;
      4:begin; putReg(buf,p,0,8,'r2'); end;
      5:begin; putReg(buf,p,0,8,'r3'); end;
      6:begin; putReg(buf,p,0,8,'r4'); end;
      7:begin; putReg(buf,p,0,8,'r5'); end;
      8:begin; putReg(buf,p,0,8,'r6'); end;
      9:begin; putReg(buf,p,0,8,'r7'); end;
     10:begin; putReg(buf,p,0,16,'pc'); end;
     11:begin; putReg(buf,p,0,16,'sp'); end;
     12:begin;
        putReg(buf,p,2,1,'p');
        putReg(buf,p,1,1,'v');
        putReg(buf,p,0,1,'c');
        end;
      end;
    end;
  else immErr('debugger is incompatible!');
  end;
pipeLineSend(pipe,buf,p);
goto f1;
END.