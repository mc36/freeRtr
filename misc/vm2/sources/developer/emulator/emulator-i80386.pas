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



{$include io.inc}
{$include rolror.inc}
{$include bignum.inc}
{$include emulator-i80386.inc}

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
  a:String;
  w:Word;
BEGIN;
WriteLn('emulator-i80386 v1.0, done by Mc at '#%date' '#%time'.');
if BigNumStartActions then immErr('failed to find bignum process!');

memorySize:=BVal(ParamStr(1));
i:=BVal(ParamStr(2));
if (i=0) then i:=BugOS_findProcNam(ParamStr(2));
if (i=0) then immErr('using: emulator.code <memsize> <processid>');

if (pipeLineCreate(pipe,i,8192,true)<>0) then immErr('error creating pipeline!');

if ResizeMem(memorySize) then immErr('error allocating memory!');
fillchar(memoryData^,memorySize,0);
emulateRestart;
for i:=3 to paramCount do begin;
  a:=kicsi(paramStr(i));
  if (a='use16') then emulateSetupUse16;
  if (a='use32') then emulateSetupUse32;
  end;

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
    move(regs_d[reg_eax],buf[2],sizeof(reg_flg));
    move(regs_d[reg_ebx],buf[6],sizeof(reg_flg));
    move(regs_d[reg_ecx],buf[10],sizeof(reg_flg));
    move(regs_d[reg_edx],buf[14],sizeof(reg_flg));
    move(regs_d[reg_esi],buf[18],sizeof(reg_flg));
    move(regs_d[reg_edi],buf[22],sizeof(reg_flg));
    move(regs_d[reg_ebp],buf[26],sizeof(reg_flg));
    move(regs_d[reg_esp],buf[30],sizeof(reg_flg));
    move(reg_eip,buf[34],sizeof(reg_eip));
    move(reg_flg,buf[38],sizeof(reg_flg));
    move(regs_s[reg_ds],buf[42],sizeof(reg_flg));
    move(regs_s[reg_es],buf[46],sizeof(reg_flg));
    move(regs_s[reg_fs],buf[50],sizeof(reg_flg));
    move(regs_s[reg_gs],buf[54],sizeof(reg_flg));
    move(regs_s[reg_ss],buf[58],sizeof(reg_flg));
    move(regs_s[reg_cs],buf[62],sizeof(reg_flg));
    move(reg_cr0,buf[66],sizeof(reg_cr0));
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
    move(buf[2],regs_d[reg_eax],sizeof(reg_flg));
    move(buf[6],regs_d[reg_ebx],sizeof(reg_flg));
    move(buf[10],regs_d[reg_ecx],sizeof(reg_flg));
    move(buf[14],regs_d[reg_edx],sizeof(reg_flg));
    move(buf[18],regs_d[reg_esi],sizeof(reg_flg));
    move(buf[22],regs_d[reg_edi],sizeof(reg_flg));
    move(buf[26],regs_d[reg_ebp],sizeof(reg_flg));
    move(buf[30],regs_d[reg_esp],sizeof(reg_flg));
    move(buf[34],reg_eip,sizeof(reg_eip));
    move(buf[38],reg_flg,sizeof(reg_flg));
    move(buf[42],regs_s[reg_ds],sizeof(reg_flg));
    move(buf[46],regs_s[reg_es],sizeof(reg_flg));
    move(buf[50],regs_s[reg_fs],sizeof(reg_flg));
    move(buf[54],regs_s[reg_gs],sizeof(reg_flg));
    move(buf[58],regs_s[reg_ss],sizeof(reg_flg));
    move(buf[62],regs_s[reg_cs],sizeof(reg_flg));
    move(buf[66],reg_cr0,sizeof(reg_cr0));
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
    putReg(buf,p,4,$00,'eax');
    putReg(buf,p,4,$00,'ebx');
    putReg(buf,p,4,$00,'ecx');
    putReg(buf,p,4,$00,'edx');
    putReg(buf,p,4,$00,'esi');
    putReg(buf,p,4,$00,'edi');
    putReg(buf,p,4,$00,'ebp');
    putReg(buf,p,4,$01,'esp');
    putReg(buf,p,4,$02,'eip');
    putReg(buf,p,4,$83,'flg');
    putReg(buf,p,4,$00,'ds');
    putReg(buf,p,4,$00,'es');
    putReg(buf,p,4,$00,'fs');
    putReg(buf,p,4,$00,'gs');
    putReg(buf,p,4,$00,'ss');
    putReg(buf,p,4,$00,'cs');
    putReg(buf,p,4,$80,'cr0');
    end;
  02:begin; {register bitmap}
    buf[1]:=03;p:=1;
    move(buf[2],i,sizeof(i));
    case i of
      1..4:begin;
        a:=chr(i+$60);
        putReg(buf,p,0,8,a+'l');
        putReg(buf,p,8,8,a+'h');
        putReg(buf,p,0,16,a+'x');
        putReg(buf,p,16,16,'upper');
        putReg(buf,p,0,32,'e'+a+'x');
        end;
      5:begin; putReg(buf,p,0,16,'si');putReg(buf,p,0,32,'esi'); end;
      6:begin; putReg(buf,p,0,16,'di');putReg(buf,p,0,32,'edi'); end;
      7:begin; putReg(buf,p,0,16,'bp');putReg(buf,p,0,32,'ebp'); end;
      8:begin; putReg(buf,p,0,16,'sp');putReg(buf,p,0,32,'esp'); end;
      9:begin; putReg(buf,p,0,16,'ip');putReg(buf,p,0,32,'eip'); end;
      10:begin;
        putReg(buf,p,0,1,'cf');
        putReg(buf,p,6,1,'zf');
        putReg(buf,p,7,1,'sf');
        putReg(buf,p,11,1,'of');
        putReg(buf,p,2,1,'pf');
        putReg(buf,p,4,1,'af');
        putReg(buf,p,9,1,'if');
        putReg(buf,p,10,1,'df');
        putReg(buf,p,8,1,'tf');
        putReg(buf,p,12,2,'iopl');
        putReg(buf,p,14,1,'nt');
        putReg(buf,p,16,1,'rf');
        putReg(buf,p,17,1,'vm');
        putReg(buf,p,18,1,'ac');
        putReg(buf,p,19,1,'vif');
        putReg(buf,p,20,1,'vip');
        putReg(buf,p,21,1,'id');
        end;
      11:putReg(buf,p,0,16,'ds');
      12:putReg(buf,p,0,16,'es');
      13:putReg(buf,p,0,16,'fs');
      14:putReg(buf,p,0,16,'gs');
      15:putReg(buf,p,0,16,'ss');
      16:putReg(buf,p,0,16,'cs');
      17:begin;
        putReg(buf,p,0,1,'pe');
        putReg(buf,p,1,1,'mp');
        putReg(buf,p,2,1,'em');
        putReg(buf,p,3,1,'ts');
        putReg(buf,p,4,1,'et');
        putReg(buf,p,5,1,'ne');
        putReg(buf,p,16,1,'wp');
        putReg(buf,p,18,1,'am');
        putReg(buf,p,29,1,'nw');
        putReg(buf,p,30,1,'cd');
        putReg(buf,p,31,1,'pg');
        end;
      end;
    end;
  else immErr('debugger is incompatible!');
  end;
pipeLineSend(pipe,buf,p);
goto f1;
END.