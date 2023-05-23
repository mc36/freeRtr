{$I tp70emu.inc}
Const
  MaxProggyMemory=16*1024*1024;
  TerminationFlag=$80000000;
  MaxHandlers=10;
  reg_a=1;
  reg_b=2;
  reg_c=3;
  reg_d=4;
  reg_src=5;
  reg_trg=6;
  reg_cip=7;
Type
  int32=longint;
  OneArrayRecord=array[1..1024] of byte;
  OneProcessorRecord=record
    regs:array[1..7] of int32;
    flags:int32;  {1=above, 2=below, 4=equal}
    nam:String;
    par:String;
    stackD:^OneArrayRecord;
    stackS:int32;
    stackP:int32;
    codeD:^OneArrayRecord;
    codeS:int32;
    dataD:^OneArrayRecord;
    dataS:int32;
    dataP:int32;
    procD:^OneArrayRecord;
    procS:int32;
    files:array[1..MaxHandlers] of record
      h:int32;
      d:xFile;
      r:Word;
      u:Boolean;
      end;
    dirs:array[1..MaxHandlers] of record
      h:int32;
      d:xFile;
      u:Boolean;
      end;
    handlers:int32;
    end;

Function conv2hex(i:int32):String;
Const HexDigits:String[16]='0123456789ABCDEF';
function c(b:int32):string;begin; b:=b and $ff;c:=hexdigits[b shr 4+1]+hexdigits[b and 15+1]; end;
Begin;
conv2hex:='0x'+c(i shr 24)+c(i shr 16)+c(i shr 8)+c(i shr 0);
End;

Function getConstant(var d:OneProcessorRecord;n:int32):int32;
Var i:int32;
Begin;
i:=0;
move(d.codeD^[d.regs[reg_cip]+1],i,n);
inc(d.regs[reg_cip],n);
getConstant:=i;
End;

Procedure emuClear(var d:OneProcessorRecord);
Begin;
fillchar(d,sizeof(d),0);
End;

Procedure emuFinish(var d:OneProcessorRecord);
Begin;
if (d.dataD<>nil) then freeMem(d.dataD,d.dataS);
if (d.stackD<>nil) then freeMem(d.stackD,d.stackS);
if (d.procD<>nil) then freeMem(d.procD,d.procS*4);
if (d.codeD<>nil) then freeMem(d.codeD,d.codeS);
emuClear(d);
End;

Procedure emuDump(var d:OneProcessorRecord);
Begin;
WriteLn('nam="'+d.nam+'"  par="'+d.par+'"');
WriteLn('  a='+conv2hex(d.regs[reg_a])+'    b='+conv2hex(d.regs[reg_b])+
  '    c='+conv2hex(d.regs[reg_c])+'    d='+conv2hex(d.regs[reg_d]));
WriteLn('src='+conv2hex(d.regs[reg_src])+'  trg='+conv2hex(d.regs[reg_trg])+
  '  cip='+conv2hex(d.regs[reg_cip])+'  flg='+conv2hex(d.flags));
End;

Function emuGetResult(var d:OneProcessorRecord;i:int32):String;
Var a:String;
Begin;
case i of
  0:a:='running';
  1:a:='invalid syscall code';
  2:a:='invalid instruction code';
  3:a:='invalid stack reference';
  4:a:='division by zero';
  5:a:='invalid memory reference';
  6:a:='invalid subroutine reference';
  7:a:='invalid code reference';
  8:a:='invalid memory reference';
  else a:='unknown error happened';
  end;
if (i and TerminationFlag<>0) then a:='successful, exitcode='+BStr(i and $ffff);
emuGetResult:=BStr(i and $ffff)+'-'+a;
End;

Function emuStart(var d:OneProcessorRecord;n,p:string):int32;
Label err;
Const max=16*1024;
Var
  f:xFile;
  i,o:int32;
Begin;
emuClear(d);
emuStart:=1;
d.nam:=n;
d.par:=p;
if (xOpen(f,n,xGenFilMod_rw)<>0) then goto err;
d.codeS:=xFileSize(f);
xSeek(f,0);
getMem(d.codeD,d.codeS);
if (d.codeD=nil) then goto err;
o:=0;
while (o<d.codeS) do begin;
  i:=d.codeS-o;
  if (i>max) then i:=max;
  if (xBlockRead(f,d.codeD^[o+1],i)<>0) then goto err;
  inc(o,i);
  end;
xClose(f);
emuStart:=2;
if (getConstant(d,4)<>$30314d56) then goto err;
i:=getConstant(d,4);
emuStart:=3;
d.stackS:=getConstant(d,4);
getmem(d.stackD,d.stackS);
if (d.stackD=nil) then goto err;
d.dataS:=getConstant(d,4);
if (d.dataS>MaxProggyMemory) then goto err;
getmem(d.dataD,MaxProggyMemory);
if (d.dataD=nil) then goto err;
d.procS:=getConstant(d,4);
i:=d.procS*4;
getmem(d.procD,i);
if (d.procD=nil) then goto err;
fillchar(d.procD^,i,$7f);
d.regs[reg_cip]:=getConstant(d,4);
emuStart:=0;
exit;
err:
emuClear(d);
End;

Function emuExec(var d:OneProcessorRecord):int32;
Var registers:array[1..6] of int32 absolute d;

Function stackPush(val,siz:int32):Boolean;
Begin;
stackPush:=True;
if (d.stackP+siz>d.stackS) then exit;
move(val,d.stackD^[d.stackP+1],siz);
inc(d.stackP,siz);
stackPush:=False;
End;

Function stackPop(var val:int32;siz:int32):Boolean;
Begin;
stackPop:=True;
val:=0;
if (d.stackP-siz<0) then exit;
dec(d.stackP,siz);
move(d.stackD^[d.stackP+1],val,siz);
stackPop:=False;
End;

Function convType(d,sig,siz:int32;after:boolean):int32;
Begin;
sig:=sig and 1;
siz:=siz and 15;
case siz of
  1:d:=d and $ff;
  2:d:=d and $ffff;
  3:d:=d and $ffffffff;
  end;
if (sig<>0) then case siz of
  1:d:=shortint(d);
  2:d:=integer(d);
  3:d:=longint(d);
  end;
if after then case siz of
  1:d:=d and $ff;
  2:d:=d and $ffff;
  3:d:=d and $ffffffff;
  end;
convType:=d;
End;

Function getMemory:int32;
Var i,o,p:int32;
Begin;
i:=getConstant(d,1);
p:=i and $80;
i:=i and 15;
o:=getConstant(d,4);
i:=registers[i];
if (p=0) then inc(o,i) else dec(o,i);
getMemory:=o;
End;

Function convForm(d,frm,siz:int32):int32;
Begin;
frm:=frm and 15;
siz:=siz and 15;
if (frm=2) then case siz of
  1:d:=d and $ff;
  2:d:=((d shr 8) and $ff)+((d and $ff) shl 8);
  3:d:=((d shr 24) and $ff)+(((d shr 16) and $ff) shl 8)+(((d shr 8) and $ff) shl 16)+((d and $ff) shl 24);
  end;
convForm:=d;
End;

Function GetMovementSize(siz:int32):int32;
Begin;
case siz and 15 of
  1:siz:=1;
  2:siz:=2;
  3:siz:=4;
  else siz:=0;
  end;
GetMovementSize:=siz;
End;


Label syscall,err,oke;
Const
  AddrMagic=$fababe;
  AddrSize=3;
Var
  time1,time2:xDirEntryDateTimeRec;
  ntry:xDirEntryRec;
  ocip:int32;
  siz1,siz2:int32;
  reg1,reg2:int32;
  form:int32;
  val1,val2:int32;
  a,b:String;
  w1,w2,w3:word;

Function findFileHandler:Boolean;
Var i:int32;
Begin;
for i:=1 to MaxHandlers do if (d.files[i].h=d.regs[reg_a]) then begin;
  findFileHandler:=False;
  val2:=i;
  exit;
  end;
val2:=-1;
d.regs[reg_b]:=1;
findFileHandler:=True;
End;

Function findDirHandler:Boolean;
Var i:int32;
Begin;
for i:=1 to MaxHandlers do if (d.dirs[i].h=d.regs[reg_a]) then begin;
  findDirHandler:=False;
  val2:=i;
  exit;
  end;
val2:=-1;
d.regs[reg_b]:=1;
findDirHandler:=True;
End;

Begin;
emuExec:=2;
ocip:=d.regs[reg_cip];
if (ocip<0) or (ocip>=d.codeS) then begin; emuExec:=7;goto err; end;
siz1:=getConstant(d,1);
case siz1 of
  1:begin; {add}
    siz1:=getConstant(d,1);
    reg1:=getConstant(d,1);
    if (siz1 and $80<>0) then val1:=getConstant(d,4) else val1:=registers[getConstant(d,1)];
    inc(registers[reg1],val1);
    end;
  2:begin; {sub}
    siz1:=getConstant(d,1);
    reg1:=getConstant(d,1);
    if (siz1 and $80<>0) then val1:=getConstant(d,4) else val1:=registers[getConstant(d,1)];
    dec(registers[reg1],val1);
    end;
  3:begin; {mul}
    siz1:=getConstant(d,1);
    siz2:=siz1 shr 6;
    reg1:=getConstant(d,1);
    if (siz1 and $80<>0) then val1:=getConstant(d,4) else val1:=registers[getConstant(d,1)];
    val1:=convType(val1,siz2,siz1,false);
    val2:=convType(registers[reg1],siz2,siz1,false);
    val1:=val2*val1;
    registers[reg1]:=val1;
    end;
  4:begin; {div}
    siz1:=getConstant(d,1);
    siz2:=siz1 shr 6;
    reg1:=getConstant(d,1);
    if (siz1 and $80<>0) then val1:=getConstant(d,4) else val1:=registers[getConstant(d,1)];
    val1:=convType(val1,siz2,siz1,false);
    val2:=convType(registers[reg1],siz2,siz1,false);
    emuExec:=4;
    if (val1=0) then goto err;
    val1:=val2 div val1;
    registers[reg1]:=val1;
    end;
  5:begin; {mod}
    siz1:=getConstant(d,1);
    siz2:=siz1 shr 6;
    reg1:=getConstant(d,1);
    if (siz1 and $80<>0) then val1:=getConstant(d,4) else val1:=registers[getConstant(d,1)];
    val1:=convType(val1,siz2,siz1,false);
    val2:=convType(registers[reg1],siz2,siz1,false);
    emuExec:=4;
    if (val1=0) then goto err;
    val1:=val2 mod val1;
    registers[reg1]:=val1;
    end;
  6:begin; {or}
    siz1:=getConstant(d,1);
    reg1:=getConstant(d,1);
    if (siz1 and $80<>0) then val1:=getConstant(d,4) else val1:=registers[getConstant(d,1)];
    registers[reg1]:=registers[reg1] or val1;
    end;
  7:begin; {xor}
    siz1:=getConstant(d,1);
    reg1:=getConstant(d,1);
    if (siz1 and $80<>0) then val1:=getConstant(d,4) else val1:=registers[getConstant(d,1)];
    registers[reg1]:=registers[reg1] xor val1;
    end;
  8:begin; {and}
    siz1:=getConstant(d,1);
    reg1:=getConstant(d,1);
    if (siz1 and $80<>0) then val1:=getConstant(d,4) else val1:=registers[getConstant(d,1)];
    registers[reg1]:=registers[reg1] and val1;
    end;
  9:begin; {not}
    siz1:=getConstant(d,1);
    reg1:=getConstant(d,1);
    registers[reg1]:=not registers[reg1];
    end;
  10:begin; {neg}
    siz1:=getConstant(d,1);
    reg1:=getConstant(d,1);
    registers[reg1]:=-registers[reg1];
    end;
  11:begin; {shl}
    siz1:=getConstant(d,1);
    reg1:=getConstant(d,1);
    if (siz1 and $80<>0) then val1:=getConstant(d,4) else val1:=registers[getConstant(d,1)];
    registers[reg1]:=registers[reg1] shl val1;
    end;
  12:begin; {shr}
    siz1:=getConstant(d,1);
    reg1:=getConstant(d,1);
    if (siz1 and $80<>0) then val1:=getConstant(d,4) else val1:=registers[getConstant(d,1)];
    registers[reg1]:=registers[reg1] shr val1;
    end;
  13:begin; {push}
    siz1:=getConstant(d,1);
    reg1:=getConstant(d,1);
    siz1:=GetMovementSize(siz1);
    val1:=registers[reg1];
    if (reg1 in [5..6]) then begin; val1:=val1 xor AddrMagic;siz1:=AddrSize; end;
    emuExec:=3;
    if stackPush(val1,siz1) then goto err;
    end;
  14:begin; {pop}
    siz1:=getConstant(d,1);
    reg1:=getConstant(d,1);
    siz1:=GetMovementSize(siz1);
    if (reg1 in [5..6]) then siz1:=AddrSize;
    emuExec:=3;
    if stackPop(val1,siz1) then goto err;
    if (reg1 in [5..6]) then val1:=val1 xor AddrMagic;
    registers[reg1]:=val1;
    end;
  15:begin; {comp}
    siz1:=getConstant(d,1);
    siz2:=siz1 shr 6;
    reg1:=getConstant(d,1);
    if (siz1 and $80<>0) then val1:=getConstant(d,4) else val1:=registers[getConstant(d,1)];
    val1:=convType(val1,siz2,siz1,false);
    val2:=convType(registers[reg1],siz2,siz1,false);
    reg1:=0;
    if (val1<val2) then reg1:=reg1 or 1;
    if (val1>val2) then reg1:=reg1 or 2;
    if (val1=val2) then reg1:=reg1 or 4;
    d.flags:=(d.flags and $fffffff8) or reg1;
    end;
  16:begin; {move}
    siz1:=getConstant(d,1);
    siz2:=getConstant(d,1);
    reg1:=getConstant(d,1);
    if (siz2 and $80<>0) then val1:=getConstant(d,4) else val1:=registers[getConstant(d,1)];
    val1:=convType(val1,siz2 shr 6,siz2,false);
    val1:=convType(val1,siz1 shr 6,siz1,true);
    registers[reg1]:=val1;
    end;
  17:begin; {movr}
    form:=getConstant(d,1);
    siz1:=getConstant(d,1);
    siz2:=getConstant(d,1);
    val2:=getMemory;
    emuExec:=5;
    if (val2<0) or (val2>=MaxProggyMemory) then goto err;
    reg1:=getConstant(d,1);
    move(d.dataD^[val2+1],val1,4);
    val1:=convForm(val1,form,siz2);
    val1:=convType(val1,siz2 shr 6,siz2,false);
    val1:=convType(val1,siz1 shr 6,siz1,true);
    registers[reg1]:=val1;
    end;
  18:begin; {movw}
    form:=getConstant(d,1);
    siz1:=getConstant(d,1);
    siz2:=getConstant(d,1);
    val2:=getMemory;
    emuExec:=5;
    if (val2<0) or (val2>=MaxProggyMemory) then goto err;
    reg1:=getConstant(d,1);
    val1:=registers[reg1];
    val1:=convType(val1,siz2 shr 6,siz2,false);
    val1:=convType(val1,siz1 shr 6,siz1,true);
    val1:=convForm(val1,form,siz1);
    siz2:=GetMovementSize(siz1);
    move(val1,d.dataD^[val2+1],siz2);
    end;
  19:begin; {call}
    val1:=getConstant(d,4);
    emuExec:=3;
    if stackPush(d.regs[reg_cip] xor AddrMagic,AddrSize) then goto err;
    d.regs[reg_cip]:=val1;
    end;
  20:begin; {ret}
    emuExec:=3;
    if stackPop(val1,AddrSize) then goto err;
    d.regs[reg_cip]:=val1 xor AddrMagic;
    end;
  21:begin; {jump}
    val1:=getConstant(d,4);
    d.regs[reg_cip]:=val1;
    end;
  22:begin; {jmpc}
    reg1:=getConstant(d,1);
    val1:=getConstant(d,4);
    if (d.flags and reg1<>0) then d.regs[reg_cip]:=val1;
    end;
  23:begin; {addrLod}
    val2:=getMemory;
    reg1:=getConstant(d,1);
    emuExec:=5;
    if (val2<0) or (val2>=MaxProggyMemory) then goto err;
    val1:=0;
    move(d.dataD^[val2+1],val1,AddrSize);
    registers[reg1]:=val1 xor AddrMagic;
    end;
  24:begin; {addrSav}
    val2:=getMemory;
    reg1:=getConstant(d,1);
    emuExec:=5;
    if (val2<0) or (val2>=MaxProggyMemory) then goto err;
    val1:=registers[reg1] xor AddrMagic;
    move(val1,d.dataD^[val2+1],AddrSize);
    end;
  25:begin; {procAddr}
    reg1:=getConstant(d,1);
    val1:=getConstant(d,4);
    emuExec:=6;
    if (val1=-1) then begin;
      emuExec:=3;
      if stackPop(val2,AddrSize) then goto err;
      if stackPush(val2,AddrSize) then goto err;
      val2:=val2 xor AddrMagic;
      end else begin;
      if (val1<0) or (val1>=d.procS) then goto err;
      val1:=(val1*sizeof(val2))+1;
      move(d.procD^[val1],val2,sizeof(val2));
      end;
    registers[reg1]:=val2;
    end;
  26:begin; {procAllocBeg}
    val1:=getConstant(d,4);
    val1:=(val1*sizeof(val2))+1;
    move(d.procD^[val1],val2,sizeof(val2));
    emuExec:=3;
    if stackPush(val2 xor AddrMagic,AddrSize) then goto err;
    val2:=d.DataP;
    val1:=getConstant(d,4);
    inc(d.dataP,val1);
    emuExec:=8;
    if (d.dataP>=d.dataS) then goto err;
    if stackPush(val2 xor AddrMagic,AddrSize) then goto err;
    end;
  31:begin; {procAllocEnd}
    emuExec:=3;
    if stackPop(val2,AddrSize) then goto err;
    val2:=val2 xor AddrMagic;
    val1:=getConstant(d,4);
    val1:=(val1*sizeof(val2))+1;
    move(val2,d.procD^[val1],sizeof(val2));
    getConstant(d,4);
    end;
  27:begin; {procFree}
    val1:=getConstant(d,4);
    val1:=(val1*sizeof(val2))+1;
    emuExec:=3;
    if stackPop(val2,AddrSize) then goto err;
    val2:=val2 xor AddrMagic;
    move(val2,d.procD^[val1],sizeof(val2));
    val2:=getConstant(d,4);
    dec(d.dataP,val2);
    emuExec:=8;
    if (d.dataP<0) then goto err;
    end;
  28:begin; {codeOfs}
    reg1:=getConstant(d,1);
    val1:=getConstant(d,4);
    registers[reg1]:=val1;
    end;
  29:begin; {xchg}
    siz1:=getConstant(d,1);
    val2:=getMemory;
    reg1:=getConstant(d,1);
    emuExec:=5;
    if (val2<0) or (val2>=MaxProggyMemory) then goto err;
    siz2:=GetMovementSize(siz1);
    val1:=registers[reg1];
    move(d.dataD^[val2+1],registers[reg1],siz2);
    move(val1,d.dataD^[val2+1],siz2);
    end;
  30:begin; {setc}
    reg2:=getConstant(d,1);
    siz1:=getConstant(d,1);
    reg1:=getConstant(d,1);
    val1:=0;
    if (d.flags and reg2<>0) then val1:=1;
    registers[reg1]:=val1;
    end;
  32:begin; {sysCall}
    siz1:=getConstant(d,1);
    goto syscall;
    end;
  33:begin; {cllr}
    emuExec:=3;
    reg1:=getConstant(d,1);
    if stackPush(d.regs[reg_cip] xor AddrMagic,AddrSize) then goto err;
    d.regs[reg_cip]:=registers[reg1];
    end;
  34:begin; {jmpr}
    reg1:=getConstant(d,1);
    d.regs[reg_cip]:=registers[reg1];
    end;
  else begin;
    err:
    d.regs[reg_cip]:=ocip;
    exit;
    end;
  end;
emuExec:=0;
exit;

syscall:
emuExec:=1;
case siz1 of
  1:begin; {sleep}
    end;
  2:begin; {memCopy}
    move(d.dataD^[d.regs[reg_src]+1],d.dataD^[d.regs[reg_trg]+1],d.regs[reg_c] and $ffff);
    end;
  3:begin; {codeCopy}
    move(d.codeD^[d.regs[reg_src]+1],d.dataD^[d.regs[reg_trg]+1],d.regs[reg_c] and $ffff);
    end;
  4:begin; {terminate}
    emuExec:=TerminationFlag or (d.regs[reg_a] and $ffff);
    goto err;
    end;
  5:begin; {console.write}
    val1:=d.regs[reg_src];
    for val2:=1 to d.regs[reg_c] do begin;
      inc(val1);
      write(chr(d.dataD^[val1]));
      end;
    end;
  6:begin; {console.read}
    a:='';
    while keypressed and (length(a)+2<=d.regs[reg_c]) do begin;
      siz1:=readkey;
      a:=a+chr(siz1 and $ff)+chr(siz1 shr 8);
      end;
    d.regs[reg_c]:=length(a);
    move(a[1],d.dataD^[d.regs[reg_trg]+1],length(a));
    end;
  7:begin; {file.maxName}
    d.regs[reg_a]:=12;
    end;
  8:begin; {file.myName}
    a:=d.nam;
    d.regs[reg_c]:=length(a);
    move(a[1],d.dataD^[d.regs[reg_trg]+1],d.regs[reg_c]);
    end;
  9:begin; {file.myParam}
    a:=d.par;
    d.regs[reg_c]:=length(a);
    move(a[1],d.dataD^[d.regs[reg_trg]+1],d.regs[reg_c]);
    end;
  10:begin; {file.open}
    d.regs[reg_b]:=1;
    val2:=-1;
    for val1:=1 to MaxHandlers do if not d.files[val1].u then val2:=val1;
    if (val2<0) then goto oke;
    move(d.dataD^[d.regs[reg_src]+1],a,sizeof(a));
    siz1:=d.regs[reg_a];
    if (siz1 and 2<>0) then siz1:=xGenFilMod_rw else siz1:=xGenFilMod_r;
    siz1:=xOpen(d.files[val2].d,a,siz1);
    d.regs[reg_b]:=siz1;
    if (siz1<>0) then goto oke;
    inc(d.handlers);
    d.files[val2].r:=d.regs[reg_a];
    d.files[val2].u:=true;
    d.files[val2].h:=d.handlers;
    d.regs[reg_a]:=d.handlers;
    d.regs[reg_b]:=0;
    end;
  11:begin; {file.read}
    if findFileHandler then goto oke;
    d.regs[reg_b]:=xBlockRead(d.files[val2].d,d.dataD^[d.regs[reg_trg]+1],word(d.regs[reg_c]));
    end;
  12:begin; {file.write}
    if findFileHandler then goto oke;
    d.regs[reg_b]:=xBlockWrite(d.files[val2].d,d.dataD^[d.regs[reg_src]+1],word(d.regs[reg_c]));
    end;
  13:begin; {file.seek}
    if findFileHandler then goto oke;
    d.regs[reg_b]:=xSeek(d.files[val2].d,d.regs[reg_c]);
    end;
  14:begin; {file.getSize}
    if findFileHandler then goto oke;
    d.regs[reg_c]:=xFileSize(d.files[val2].d);
    d.regs[reg_b]:=0;
    end;
  15:begin; {file.getPos}
    if findFileHandler then goto oke;
    d.regs[reg_c]:=xFilePos(d.files[val2].d);
    d.regs[reg_b]:=0;
    end;
  16:begin; {file.truncate}
    if findFileHandler then goto oke;
    d.regs[reg_b]:=xTruncate(d.files[val2].d);
    end;
  17:begin; {file.close}
    if findFileHandler then goto oke;
    xClose(d.files[val2].d);
    fillchar(d.files[val2],sizeof(d.files[val2]),0);
    d.regs[reg_b]:=0;
    end;
  18:begin; {file.create}
    move(d.dataD^[d.regs[reg_src]+1],a,sizeof(a));
    d.regs[reg_b]:=xCreate(a);
    end;
  19:begin; {file.erase}
    move(d.dataD^[d.regs[reg_src]+1],a,sizeof(a));
    d.regs[reg_b]:=xErase(a);
    end;
  20:begin; {dir.current}
    a:=xGetDir;
    move(a,d.dataD^[d.regs[reg_trg]+1],length(a)+1);
    d.regs[reg_b]:=0;
    end;
  21:begin; {dir.change}
    move(d.dataD^[d.regs[reg_src]+1],a,sizeof(a));
    d.regs[reg_b]:=xChDir(a);
    end;
  22:begin; {dir.setRights}
    move(d.dataD^[d.regs[reg_src]+1],a,sizeof(a));
    d.regs[reg_b]:=xSetRight(a,d.regs[reg_b],d.regs[reg_a]);
    end;
  23:begin; {dir.rename}
    move(d.dataD^[d.regs[reg_src]+1],a,sizeof(a));
    move(d.dataD^[d.regs[reg_trg]+1],b,sizeof(b));
    d.regs[reg_b]:=xRename(a,b);
    end;
  24:begin; {dir.makeLink}
    move(d.dataD^[d.regs[reg_src]+1],a,sizeof(a));
    move(d.dataD^[d.regs[reg_trg]+1],b,sizeof(b));
    d.regs[reg_b]:=xMkLink(a,b);
    end;
  25:begin; {dir.open}
    d.regs[reg_b]:=1;
    val2:=-1;
    for val1:=1 to MaxHandlers do if not d.dirs[val1].u then val2:=val1;
    if (val2<0) then goto oke;
    move(d.dataD^[d.regs[reg_src]+1],a,sizeof(a));
    siz1:=xDirOpen(d.dirs[val2].d,a);
    d.regs[reg_b]:=siz1;
    if (siz1<>0) then goto oke;
    inc(d.handlers);
    d.dirs[val2].u:=true;
    d.dirs[val2].h:=d.handlers;
    d.regs[reg_a]:=d.handlers;
    d.regs[reg_b]:=0;
    end;
  26:begin; {dir.read}
    if findDirHandler then goto oke;
    d.regs[reg_b]:=xDirRead(d.dirs[val2].d,ntry);
    move(ntry,d.dataD^[d.regs[reg_trg]+1],sizeof(ntry));
    end;
  27:begin; {dir.close}
    if findDirHandler then goto oke;
    d.regs[reg_b]:=xDirClose(d.dirs[val2].d);
    fillchar(d.dirs[val2],sizeof(d.dirs[val2]),0);
    end;
  28:begin; {dir.create}
    move(d.dataD^[d.regs[reg_src]+1],a,sizeof(a));
    d.regs[reg_b]:=xMkDir(a);
    end;
  29:begin; {dir.erase}
    move(d.dataD^[d.regs[reg_src]+1],a,sizeof(a));
    d.regs[reg_b]:=xRmDir(a);
    end;
  30:begin; {dir.statistic}
    xDiskInfo(d.regs[reg_a],d.regs[reg_b],d.regs[reg_c],d.regs[reg_d]);
    end;
  31:begin; {dir.setDate}
    move(d.dataD^[d.regs[reg_src]+1],a,sizeof(a));
    siz1:=d.regs[reg_trg];
    move(d.dataD^[siz1+1],time1,sizeof(time1));
    move(d.dataD^[siz1+sizeof(time1)+1],time2,sizeof(time2));
    d.regs[reg_b]:=xSetDate(a,time1,time2);
    end;
  32:begin; {memresize}
    d.regs[reg_trg]:=d.dataS;
    d.regs[reg_c]:=MaxProggyMemory-d.dataS;
    end;
  33:begin; {getmeminfo}
    d.regs[reg_trg]:=d.dataS;
    d.regs[reg_c]:=MaxProggyMemory-d.dataS;
    end;
  34:begin; {console.iskey}
    if keypressed then siz1:=1 else siz1:=0;
    d.regs[reg_a]:=siz1;
    end;
  35:begin; {console.size}
    d.regs[reg_a]:=78;
    d.regs[reg_b]:=24;
    end;
  36:begin; {console.gotoxy}
    gotoXY(d.regs[reg_a],d.regs[reg_b]);
    end;
  37:begin; {console.setcolor}
    textAttr:=d.regs[reg_a] and $ff;
    end;
  38:begin; {console.clear}
    clrscr;
    end;
  39:begin; {console.execWait}
    move(d.dataD^[d.regs[reg_src]+1],a,sizeof(a));
    move(d.dataD^[d.regs[reg_trg]+1],b,sizeof(b));
    d.regs[reg_b]:=xExec(a,b,w1);
    d.regs[reg_a]:=w1;
    end;
  40:begin; {console.execBckgnd}
    d.regs[reg_b]:=1;
    end;
  41:begin; {console.execInme}
    d.regs[reg_b]:=1;
    end;
  42:begin; {console.getDate}
    xGetDate(w1,w2,w3);
    d.regs[reg_a]:=w1;
    d.regs[reg_b]:=w2;
    d.regs[reg_c]:=w3;
    end;
  43:begin; {console.getTime}
    xGetTime(w1,w2,w3);
    d.regs[reg_a]:=w1;
    d.regs[reg_b]:=w2;
    d.regs[reg_c]:=w3;
    end;
  44:begin; {memFillByte}
    fillchar(d.dataD^[d.regs[reg_trg]+1],d.regs[reg_c] and $ffff,d.regs[reg_a]);
    end;
  else goto err;
  end;
oke:
emuExec:=0;
exit;

End;

Label f1;
Var
  d:OneProcessorRecord;
  a,b:String;
  i:int32;
BEGIN;
WriteLn('Virtual Machine Emulator v1.0, done by Mc.');
a:=paramStr(1);
if (a='') then begin;
  WriteLn('parameters: <binary-file> [parameters]');
  Halt(1);
  end;
b:='';
for i:=2 to paramCount do b:=b+' '+paramStr(i);
b:=copy(b,2,255);
a:=fexpand(a);
WriteLn('executable: '+a);
WriteLn('parameters: '+b);
i:=emuStart(d,a,b);
if (i<>0) then begin;
  WriteLn('error reading executable!');
  Halt(2);
  end;
WriteLn('running...');
f1:
i:=emuExec(d);
if (i=0) then goto f1;
if (whereX<>1) then WriteLn('');
a:=emuGetResult(d,i);
WriteLn('finished: '+a);
if (i and TerminationFlag=0) then begin;
  emuDump(d);
  emuFinish(d);
  Halt(3);
  end;
emuFinish(d);
END.