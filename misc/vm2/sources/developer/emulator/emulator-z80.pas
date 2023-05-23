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



{$include io.inc}
{$include rolror.inc}
{$include emulator-z80.inc}

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
WriteLn('emulator-z80 v1.0, done by Mc at '#%date' '#%time'.');

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
    ioaccess:=0;
    i:=emulateOneOpcode;
    if (ioaccess<>0) then i:=3;
    buf[1]:=07;
    move(i,buf[2],sizeof(i));
    p:=5;
    end;
  04:begin; {read register values}
    buf[1]:=05;
    buf[2]:=regs[reg_a];
    w:=ReadReg16(reg_bc);move(w,buf[3],sizeof(w));
    w:=ReadReg16(reg_de);move(w,buf[5],sizeof(w));
    w:=ReadReg16(reg_hl);move(w,buf[7],sizeof(w));
    buf[9]:=getFlags;
    move(regs_ix,buf[10],sizeof(regs_ix));
    move(regs_iy,buf[12],sizeof(regs_iy));
    move(regs_pc,buf[14],sizeof(regs_pc));
    move(regs_sp,buf[16],sizeof(regs_sp));
    p:=17;
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
    regs[reg_a]:=buf[2];
    move(buf[3],w,sizeof(w));WriteReg16(reg_bc,w);
    move(buf[5],w,sizeof(w));WriteReg16(reg_de,w);
    move(buf[7],w,sizeof(w));WriteReg16(reg_hl,w);
    putFlags(buf[9]);
    move(buf[10],regs_ix,sizeof(regs_ix));
    move(buf[12],regs_iy,sizeof(regs_iy));
    move(buf[14],regs_pc,sizeof(regs_pc));
    move(buf[16],regs_sp,sizeof(regs_sp));
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
    putReg(buf,p,2,$80,'bc');
    putReg(buf,p,2,$80,'de');
    putReg(buf,p,2,$80,'hl');
    putReg(buf,p,1,$83,'f');
    putReg(buf,p,2,$00,'ix');
    putReg(buf,p,2,$00,'iy');
    putReg(buf,p,2,$02,'pc');
    putReg(buf,p,2,$01,'sp');
    end;
  02:begin; {register bitmap}
    buf[1]:=03;p:=1;
    move(buf[2],i,sizeof(i));
    case i of
      1:begin; putReg(buf,p,0,8,'a'); end;
      2:begin; putReg(buf,p,8,8,'b');putReg(buf,p,0,8,'c'); end;
      3:begin; putReg(buf,p,8,8,'d');putReg(buf,p,0,8,'e'); end;
      4:begin; putReg(buf,p,8,8,'h');putReg(buf,p,0,8,'l'); end;
      5:begin;
        putReg(buf,p,7,1,'s');
        putReg(buf,p,6,1,'z');
        putReg(buf,p,4,1,'h');
        putReg(buf,p,2,1,'p');
        putReg(buf,p,1,1,'n');
        putReg(buf,p,0,1,'c');
        end;
      6:begin; putReg(buf,p,8,8,'ixh');putReg(buf,p,0,8,'ixl'); end;
      7:begin; putReg(buf,p,8,8,'iyh');putReg(buf,p,0,8,'iyl'); end;
      8:putReg(buf,p,0,16,'pc');
      9:putReg(buf,p,0,16,'sp');
      end;
    end;
  else immErr('debugger is incompatible!');
  end;
pipeLineSend(pipe,buf,p);
goto f1;
END.