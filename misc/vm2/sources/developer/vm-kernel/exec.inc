Function processExecute(var proc):Boolean;
Label fetch,fetched,next,done,later,ok,err,res;
Label opc00,opc08,opc16,opc24,opc32,opcErr;
Label sys000,sys100,sys200,sysErr;
Const maxWrt=200;
Var
  d:oneProcessRecord absolute proc;
  proc2:oneProcessRecord;
  prefBuf:array[1..512] of byte;
  prefSiz:LongInt;
  prefPos:LongInt;
  prefOfs:LongInt;
  buf:array[1..1024] of Byte;
  rounds:LongInt;
  siz1,siz2,reg1,reg2:LongInt;
  form,val1,val2:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  w1,w2,w3,w4:word;

function push(val:LongInt):Boolean;
begin;
push:=true;
if processCopyTo(d,val,sizeof(val),d.regSP) then exit;
inc(d.regSP,sizeof(val));
push:=false;
end;

function pop(var val:LongInt):Boolean;
begin;
pop:=true;
dec(d.regSP,sizeof(val));
if processCopyFrom(d,val,sizeof(val),d.regSP) then exit;
pop:=false;
end;

function getByte:LongInt;
begin;
inc(prefPos);
getByte:=prefBuf[prefPos];
end;

function getLong:LongInt;
begin;
getLong:=ReadLongLSB(prefBuf[prefPos+1]);
inc(prefPos,sizeof(LongInt));
end;

function getMem:LongInt;
var i,o,p:LongInt;
begin;
i:=getByte;
p:=i and $80;
i:=i and 7;
o:=getLong;
i:=d.regs[i];
if (p=0) then inc(o,i) else dec(o,i);
getMem:=o;
end;

procedure getSrv1;
begin;
siz1:=getByte;
reg1:=getByte and 7;
if (siz1 and $80<>0) then val1:=getLong else val1:=d.regs[getByte and 7];
end;

Function convType(val,siz:LongInt):LongInt;
Var sig:LongInt;
Begin;
sig:=siz and $40;
siz:=siz and 7;
case siz of
  1:val:=val and $ff;
  2:val:=val and $ffff;
  end;
if (sig<>0) then case siz of
  1:if (val and $80<>0) then val:=val or $ffffff00;
  2:if (val and $8000<>0) then val:=val or $ffff0000;
  end;
convType:=val;
End;

function typeSize(siz:LongInt):LongInt;
begin;
case siz and 7 of
  1:siz:=1;
  2:siz:=2;
  3:siz:=4;
  else siz:=0;
  end;
typeSize:=siz;
end;

function heapCopy(src,trg,siz:LongInt):Boolean;
var i,o:LongInt;
begin;
heapCopy:=true;
if (src>trg) then o:=1 else begin;
  inc(src,siz);
  inc(trg,siz);
  o:=-1;
  end;
while (siz>0) do begin;
  if (siz>sizeof(buf)) then i:=sizeof(buf) else i:=siz;
  if (o<0) then begin; dec(src,i);dec(trg,i); end;
  if processCopyFrom(d,buf,i,src) then exit;
  if processCopyTo(d,buf,i,trg) then exit;
  if (o>0) then begin; inc(src,i);inc(trg,i); end;
  dec(siz,i);
  end;
heapCopy:=false;
end;

procedure updateIP;
begin;
d.regIP:=prefOfs+prefPos;
end;

procedure add2buf(a:String);
var i:LongInt;
begin;
i:=length(a);
move(a[1],buf[siz1+1],i);
inc(siz1,i);
end;

function put2con(a:String):Boolean;
var
  ab:array[0..1] of byte absolute a;
  i:LongInt;
begin;
put2con:=false;
{$ifdef debug}write(a);{$endif}
i:=listingOfs(listingPipe,d.consPipe);
if (i=0) then exit;
put2con:=pipelineSend(totalMemoryByte^[i],ab[1],ab[0],d.consProc);
end;

Begin;
processExecute:=False;
rounds:=0;
prefOfs:=0;
prefPos:=d.regIP;
inc(d.timesRun);

fetch:
if (rounds>10000) then begin;
  d.relequish:=0;
  inc(d.workSince);
  goto done;
  end;
inc(prefOfs,prefPos);
if (prefOfs<0) then begin; siz1:=1;goto err; end;
prefSiz:=d.codeSiz-prefOfs;
if (prefSiz<1) then begin; siz1:=1;goto err; end;
if (prefSiz>sizeof(prefBuf)) then prefSiz:=sizeof(prefBuf);
if processCopyFrom(d,prefBuf,prefSiz,prefOfs) then begin; siz1:=1;goto err; end;
dec(prefSiz,16);
prefPos:=0;
goto fetched;

next:
inc(rounds);
if (prefPos>=prefSiz) then goto fetch;
if (prefPos<0) then goto fetch;
fetched:

siz1:=getByte;
if (siz1<8) then goto opc00;
if (siz1<16) then goto opc08;
if (siz1<24) then goto opc16;
if (siz1<32) then goto opc24;
if (siz1<40) then goto opc32;
goto opcErr;

res:
d.regs[reg_b]:=siz1;
goto next;

err:
updateIP;
case siz1 of
  1:a:='invalid address at exec';
  2:a:='division by zero';
  3:a:='stack overflow';
  4:a:='stack underflow';
  5:a:='invalid address at read';
  6:a:='invalid address at write';
  7:a:='invalid opcode';
  8:a:='invalid procedure address';
  9:a:='heap overflow';
  10:a:='invalid address at xchg';
  11:a:='heap underflow';
  12:a:='terminated';
  13:a:='invalid address at syscall';
  14:a:='invalid syscall';
  else a:='unknown error';
  end;
if (siz1=12) then siz1:=0 else begin;
  put2con(#13#10);
  put2con(#13#10'error='+a);
  put2con(#13#10'program='+d.filNam);
  put2con(#13#10'parameter='+d.params);
  put2con(#13#10'pid='+conv2hex(d.pid,8)+' ppid='+conv2hex(d.ppid,8)+' uid='+conv2hex(d.cuid,8)+' right='+conv2hex(d.right,8));
  put2con(#13#10'a='+conv2hex(d.regs[reg_a],8)+' b='+conv2hex(d.regs[reg_b],8)+' c='+conv2hex(d.regs[reg_c],8)+' d='+conv2hex(d.regs[reg_d],8)+' src='+conv2hex(d.regs[reg_src],8)+' trg='+conv2hex(d.regs[reg_trg],8));
  put2con(#13#10'flg='+conv2hex(d.flags,8)+' ip='+conv2hex(d.regIP,8)+' sp='+conv2hex(d.regSP,8));
  put2con(#13#10);
  siz1:=18;
  siz2:=0;
  end;
processKillPID(d.pid,siz1,siz2);
exit;

ok:
processExecute:=True;
exit;

done:
updateIP;
goto ok;

later:
updateIP;
dec(d.regIP,2);
goto ok;

opcErr:
siz1:=7;
goto err;

sysErr:
siz1:=14;
goto err;

opc00:
case siz1 of
  1:begin; {add}
    getSrv1;
    inc(d.regs[reg1],val1);
    end;
  2:begin; {sub}
    getSrv1;
    dec(d.regs[reg1],val1);
    end;
  3:begin; {mul}
    getSrv1;
    d.regs[reg1]:=d.regs[reg1]*val1;
    end;
  4:begin; {div}
    getSrv1;
    if (val1=0) then begin; siz1:=2;goto err; end;
    d.regs[reg1]:=d.regs[reg1] div val1;
    end;
  5:begin; {mod}
    getSrv1;
    if (val1=0) then begin; siz1:=2;goto err; end;
    d.regs[reg1]:=d.regs[reg1] mod val1;
    end;
  6:begin; {or}
    getSrv1;
    d.regs[reg1]:=d.regs[reg1] or val1;
    end;
  7:begin; {xor}
    getSrv1;
    d.regs[reg1]:=d.regs[reg1] xor val1;
    end;
  else goto opcErr;
  end;
goto next;

opc08:
case siz1 of
  8:begin; {and}
    getSrv1;
    d.regs[reg1]:=d.regs[reg1] and val1;
    end;
  9:begin; {not}
    siz1:=getByte;
    reg1:=getByte and 7;
    d.regs[reg1]:=not d.regs[reg1];
    end;
  10:begin; {neg}
    siz1:=getByte;
    reg1:=getByte and 7;
    d.regs[reg1]:=-d.regs[reg1];
    end;
  11:begin; {shl}
    getSrv1;
    d.regs[reg1]:=d.regs[reg1] shl val1;
    end;
  12:begin; {shr}
    getSrv1;
    d.regs[reg1]:=d.regs[reg1] shr val1;
    end;
  13:begin; {push}
    siz1:=getByte;
    reg1:=getByte and 7;
    if push(d.regs[reg1]) then begin; siz1:=3;goto err; end;
    end;
  14:begin; {pop}
    siz1:=getByte;
    reg1:=getByte and 7;
    if pop(d.regs[reg1]) then begin; siz1:=4;goto err; end;
    end;
  15:begin; {comp}
    getSrv1;
    val2:=convType(d.regs[reg1],siz1);
    val1:=convType(val1,siz1);
    siz2:=0;
    if (val1<val2) then inc(siz2,1);
    if (val1>val2) then inc(siz2,2);
    if (val1=val2) then inc(siz2,4);
    d.flags:=siz2;
    end;
  else goto opcErr;
  end;
goto next;

opc16:
case siz1 of
  16:begin; {move}
    siz2:=getByte;
    getSrv1;
    d.regs[reg1]:=convType(val1,siz1);
    end;
  17:begin; {movr}
    form:=getByte;
    siz1:=getByte;
    siz2:=getByte;
    val2:=getMem;
    reg1:=getByte and 7;
    if (val2<d.heapBeg) then begin; siz1:=5;goto err; end;
    if processCopyFrom(d,buf,sizeof(LongInt),val2) then begin; siz1:=5;goto err; end;
    case form of
      1:case siz1 and 7 of
        1:val1:=buf[1];
        2:val1:=ReadWordDEF(buf);
        3:val1:=ReadLongDEF(buf);
        else goto opcErr;
        end;
      2:case siz1 and 7 of
        1:val1:=buf[1];
        2:val1:=ReadWordMSB(buf);
        3:val1:=ReadLongMSB(buf);
        else goto opcErr;
        end;
      3:case siz1 and 7 of
        1:val1:=buf[1];
        2:val1:=ReadWordLSB(buf);
        3:val1:=ReadLongLSB(buf);
        else goto opcErr;
        end;
      else goto opcErr;
      end;
    d.regs[reg1]:=convType(val1,siz2);
    end;
  18:begin; {movw}
    form:=getByte;
    siz1:=getByte;
    siz2:=getByte;
    val2:=getMem;
    reg1:=getByte and 7;
    val1:=convType(d.regs[reg1],siz2);
    if (val2<d.heapBeg) then begin; siz1:=6;goto err; end;
    case form of
      1:case siz1 and 7 of
        1:buf[1]:=val1;
        2:WriteWordDEF(buf,val1);
        3:WriteLongDEF(buf,val1);
        else goto opcErr;
        end;
      2:case siz1 and 7 of
        1:buf[1]:=val1;
        2:WriteWordMSB(buf,val1);
        3:WriteLongMSB(buf,val1);
        else goto opcErr;
        end;
      3:case siz1 and 7 of
        1:buf[1]:=val1;
        2:WriteWordLSB(buf,val1);
        3:WriteLongLSB(buf,val1);
        else goto opcErr;
        end;
      else goto opcErr;
      end;
    if processCopyTo(d,buf,typeSize(siz1),val2) then begin; siz1:=6;goto err; end;
    end;
  19:begin; {call}
    val1:=getLong;
    if push(prefOfs+prefPos) then begin; siz1:=3;goto err; end;
    prefPos:=val1-prefOfs;
    end;
  20:begin; {ret}
    if pop(val1) then begin; siz1:=4;goto err; end;
    prefPos:=val1-prefOfs;
    end;
  21:begin; {jump}
    val1:=getLong;
    prefPos:=val1-prefOfs;
    end;
  22:begin; {jmpc}
    reg1:=getByte;
    val1:=getLong;
    if (d.flags and reg1<>0) then prefPos:=val1-prefOfs;
    end;
  23:begin; {addrLod}
    val2:=getMem;
    reg1:=getByte and 7;
    if (val2<d.heapBeg) then begin; siz1:=5;goto err; end;
    if processCopyFrom(d,d.regs[reg1],sizeof(LongInt),val2) then begin; siz1:=5;goto err; end;
    end;
  else goto opcErr;
  end;
goto next;

opc24:
case siz1 of
  24:begin; {addrSav}
    val2:=getMem;
    reg1:=getByte and 7;
    if (val2<d.heapBeg) then begin; siz1:=5;goto err; end;
    if processCopyTo(d,d.regs[reg1],sizeof(LongInt),val2) then begin; siz1:=6;goto err; end;
    end;
  25:begin; {procAddr}
    reg1:=getByte and 7;
    val1:=getLong;
    if (val1=-1) then begin;
      val2:=d.heapCurr;
      end else begin;
      if (val1<0) or (val1>=d.procNum) then begin; siz1:=8;goto err; end;
      if processCopyFrom(d,val2,sizeof(val1),(val1*sizeof(val2))+d.procTab) then begin; siz1:=8;goto err; end;
      end;
    d.regs[reg1]:=val2;
    end;
  26:begin; {procAllocBeg}
    val1:=getLong;
    siz1:=getLong;
    if (siz1<0) then goto opcErr;
    if (val1<0) or (val1>=d.procNum) then begin; siz1:=8;goto err; end;
    if processCopyFrom(d,val2,sizeof(val1),(val1*sizeof(val2))+d.procTab) then begin; siz1:=8;goto err; end;
    if push(val2) then begin; siz1:=3;goto err; end;
    if push(d.heapCurr) then begin; siz1:=3;goto err; end;
    d.heapCurr:=d.heapFree;
    inc(d.heapFree,siz1);
    if (d.heapFree>d.heapEnd) then begin; siz1:=9;goto err; end;
    end;
  27:begin; {procFree}
    val1:=getLong;
    siz1:=getLong;
    if (siz1<0) then goto opcErr;
    if (val1<0) or (val1>=d.procNum) then begin; siz1:=8;goto err; end;
    if pop(val2) then begin; siz1:=4;goto err; end;
    if processCopyTo(d,val2,sizeof(val1),(val1*sizeof(val2))+d.procTab) then begin; siz1:=8;goto err; end;
    dec(d.heapFree,siz1);
    if (d.heapFree<d.heapBeg) then begin; siz1:=11;goto err; end;
    end;
  28:begin; {codeOfs}
    reg1:=getByte and 7;
    val1:=getLong;
    d.regs[reg1]:=val1;
    end;
  29:begin; {xchg}
    siz1:=getByte;
    val2:=getMem;
    reg1:=getByte and 7;
    siz2:=typeSize(siz1);
    if (siz2=0) then goto opcErr;
    val1:=d.regs[reg1];
    if processCopyFrom(d,buf,siz2,val2) then begin; siz1:=10;goto err; end;
    case siz1 and 7 of
      1:begin;
        reg2:=buf[1];
        buf[1]:=val1;
        end;
      2:begin;
        reg2:=ReadWordDEF(buf);
        WriteWordDEF(buf,val1);
        end;
      3:begin;
        reg2:=ReadLongDEF(buf);
        WriteLongDEF(buf,val1);
        end;
      else goto opcErr;
      end;
    if processCopyTo(d,buf,siz2,val2) then begin; siz1:=10;goto err; end;
    d.regs[reg1]:=reg2;
    end;
  30:begin; {setc}
    reg2:=getByte;
    siz1:=getByte;
    reg1:=getByte and 7;
    if (d.flags and reg2<>0) then val1:=1 else val1:=0;
    d.regs[reg1]:=val1;
    end;
  31:begin; {procAllocEnd}
    val1:=getLong;
    siz1:=getLong;
    if (val1<0) or (val1>=d.procNum) then begin; siz1:=8;goto err; end;
    if processCopyTo(d,d.heapCurr,sizeof(val1),(val1*sizeof(val2))+d.procTab) then begin; siz1:=8;goto err; end;
    if pop(d.heapCurr) then begin; siz1:=4;goto err; end;
    end;
  else goto opcErr;
  end;
goto next;

opc32:
case siz1 of
  32:begin; {syscll}
    siz1:=getByte;
    inc(rounds,10);
    if (siz1<100) then goto sys000;
    if (siz1<200) then goto sys100;
    goto sys200;
    end;
  33:begin; {cllr}
    reg1:=getByte and 7;
    if push(prefOfs+prefPos) then begin; siz1:=3;goto err; end;
    prefPos:=d.regs[reg1]-prefOfs;
    end;
  34:begin; {jmpr}
    reg1:=getByte and 7;
    prefPos:=d.regs[reg1]-prefOfs;
    end;
  else goto opcErr;
  end;
goto next;

sys000:
case siz1 of
  1:begin; {sleep}
    d.workSince:=0;
    inc(d.relequish);
    goto done;
    end;
  2:begin; {memCopy}
    if heapCopy(d.regs[reg_src],d.regs[reg_trg],d.regs[reg_c]) then begin; siz1:=13;goto err; end;
    end;
  3:begin; {codeCopy}
    if heapCopy(d.regs[reg_src],d.regs[reg_trg],d.regs[reg_c]) then begin; siz1:=13;goto err; end;
    end;
  4:begin; {terminate}
    siz1:=12;
    siz2:=d.regs[reg_a] and $ffff;
    goto err;
    end;
  5:begin; {console.write}
    val1:=d.regs[reg_src];
    siz1:=d.regs[reg_c];
    while (siz1>0) do begin;
      if (siz1>maxWrt) then siz2:=maxWrt else siz2:=siz1;
      if processCopyFrom(d,a[1],siz2,val1) then begin; siz1:=13;goto err; end;
      ab[0]:=siz2;
      if put2con(a) then goto later;
      dec(siz1,siz2);
      inc(val1,siz2);
      end;
    end;
  6:begin; {console.read}
    siz1:=d.regs[reg_c];
    d.regs[reg_c]:=0;
    val1:=listingOfs(listingPipe,d.consPipe);
    if (val1=0) then goto next;
    if pipelineRecv(totalMemoryByte^[val1],buf,siz1,d.consProc) then siz1:=0;
    if processCopyTo(d,buf,siz1,d.regs[reg_trg]) then begin; siz1:=13;goto err; end;
    d.regs[reg_c]:=siz1;
    if (siz1<>2) then goto next;
    if (readWordDEF(buf)<>$8001) then goto next;
    siz2:=sizeof(word)*2;
    siz1:=siz2;
    if pipelineRecv(totalMemoryByte^[val1],buf,siz1,d.consProc) then goto next;
    if (siz1<>siz2) then goto next;
    d.consSizX:=ReadWordDEF(buf[1]);
    d.consSizY:=ReadWordDEF(buf[3]);
    end;
  7:begin; {file.maxName}
    d.regs[reg_a]:=250;
    end;
  8:begin; {file.myName}
    a:=d.filNam;
    siz1:=length(a);
    d.regs[reg_c]:=siz1;
    if processCopyTo(d,a[1],siz1,d.regs[reg_trg]) then begin; siz1:=13;goto err; end;
    end;
  9:begin; {file.myParam}
    a:=d.params;
    siz1:=length(a);
    d.regs[reg_c]:=siz1;
    if processCopyTo(d,a[1],siz1,d.regs[reg_trg]) then begin; siz1:=13;goto err; end;
    end;
  10:begin; {file.open}
    updateIP;
    if driveStartCmd(d,13) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  11:begin; {file.read}
    updateIP;
    if driveStartCmd(d,14) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  12:begin; {file.write}
    updateIP;
    if driveStartCmd(d,15) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  13:begin; {file.seek}
    updateIP;
    if driveStartCmd(d,16) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  14:begin; {file.getSize}
    updateIP;
    if driveStartCmd(d,17) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  15:begin; {file.getPos}
    updateIP;
    if driveStartCmd(d,18) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  16:begin; {file.truncate}
    updateIP;
    if driveStartCmd(d,19) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  17:begin; {file.close}
    updateIP;
    if driveStartCmd(d,101) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  18:begin; {file.create}
    updateIP;
    if driveStartCmd(d,5) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  19:begin; {file.erase}
    updateIP;
    if driveStartCmd(d,6) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  20:begin; {dir.current}
    a:=d.curDir;
    if processCopyTo(d,a,sizeof(a),d.regs[reg_trg]) then begin; siz1:=13;goto err; end;
    d.regs[reg_b]:=0;
    end;
  21:begin; {dir.change}
    updateIP;
    if driveStartCmd(d,1) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  22:begin; {dir.setRights}
    updateIP;
    if driveStartCmd(d,9) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  23:begin; {dir.rename}
    updateIP;
    if driveStartCmd(d,7) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  24:begin; {dir.makeLink}
    updateIP;
    if driveStartCmd(d,8) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  25:begin; {dir.open}
    updateIP;
    if driveStartCmd(d,11) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  26:begin; {dir.read}
    updateIP;
    if driveStartCmd(d,12) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  27:begin; {dir.close}
    updateIP;
    if driveStartCmd(d,102) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  28:begin; {dir.create}
    updateIP;
    if driveStartCmd(d,3) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  29:begin; {dir.erase}
    updateIP;
    if driveStartCmd(d,4) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  30:begin; {dir.statistic}
    updateIP;
    if driveStartCmd(d,2) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  31:begin; {dir.setDate}
    updateIP;
    if driveStartCmd(d,10) then begin; siz1:=13;goto err; end;
    goto ok;
    end;
  32:begin; {memresize}
    siz1:=d.regs[reg_c];
    if (siz1<0) then siz1:=0;
    processResize(d,d.extBeg+siz1);
    d.regs[reg_trg]:=d.extBeg;
    d.regs[reg_c]:=(d.blockFwd*memoryPageByte)-d.extBeg;
    end;
  33:begin; {getmeminfo}
    d.regs[reg_trg]:=d.extBeg;
    d.regs[reg_c]:=(d.blockFwd*memoryPageByte)-d.extBeg;
    end;
  34:begin; {console.iskey}
    d.regs[reg_a]:=0;
    val1:=listingOfs(listingPipe,d.consPipe);
    if (val1=0) then goto next;
    if pipelineInfo(totalMemoryByte^[val1],d.consProc,val1,val2,siz1) then goto next;
    if (val2>0) then d.regs[reg_a]:=1;
    end;
  35:begin; {console.size}
    d.regs[reg_a]:=d.consSizX;
    d.regs[reg_b]:=d.consSizY;
    end;
  36:begin; {console.gotoxy}
    a:=#0#4'1234';
    WriteWordDEF(ab[3],d.regs[reg_a]);
    WriteWordDEF(ab[5],d.regs[reg_b]);
    if put2con(a) then goto later;
    end;
  37:begin; {console.setcolor}
    a:=#0#3'1';
    ab[3]:=d.regs[reg_a] and $ff;
    if put2con(a) then goto later;
    end;
  38:begin; {console.clear}
    if put2con(#0#2) then goto later;
    end;
  39:begin; {console.execWait}
    if processCopyFrom(d,a,sizeof(a),d.regs[reg_src]) then goto sysErr;
    if processCopyFrom(d,b,sizeof(b),d.regs[reg_trg]) then goto sysErr;
    siz1:=processCreate(a,b,d.curDir,d.pid,d.cuid,d.consProc,d.consPipe,d.consSizX,d.consSizY);
    if (siz1=0) then begin; siz1:=2;goto res; end;
    d.execWait:=siz1;
    d.oldStat:=d.status;
    d.status:=4;
    goto done;
    end;
  40:begin; {console.execBckgnd}
    if (d.right and extraRightsMask=0) then begin; siz1:=4;goto res; end;
    if processCopyFrom(d,a,sizeof(a),d.regs[reg_src]) then goto sysErr;
    if processCopyFrom(d,b,sizeof(b),d.regs[reg_trg]) then goto sysErr;
    siz1:=processCreate(a,b,d.curDir,d.pid,d.cuid,0,0,d.consSizX,d.consSizY);
    if (siz1=0) then begin; siz1:=2;goto res; end;
    d.regs[reg_a]:=siz1;
    d.regs[reg_b]:=0;
    goto done;
    end;
  41:begin; {console.execInme}
    if (d.right and extraRightsMask=0) then begin; siz1:=4;goto res; end;
    if processCopyFrom(d,a,sizeof(a),d.regs[reg_src]) then goto sysErr;
    if processCopyFrom(d,b,sizeof(b),d.regs[reg_trg]) then goto sysErr;
    siz1:=processCreate(a,b,d.curDir,d.pid,d.cuid,0,0,0,0);
    if (siz1=0) then begin; siz1:=2;goto res; end;
    siz2:=pipelineCreate(d.pid,siz1,4000,0);
    if (siz2=0) then begin; processKillPID(siz1,0,0);siz1:=2;goto res; end;
    d.regs[reg_a]:=siz1;
    d.regs[reg_c]:=siz2;
    d.regs[reg_b]:=0;
    val1:=listingOfs(listingProc,siz1);
    if (val1=0) then goto done;
    processUpdate(totalMemoryByte^[val1],siz1,siz2,d.consSizX,d.consSizY);
    goto done;
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
    fillchar(buf,sizeof(buf),d.regs[reg_a]);
    siz1:=d.regs[reg_c];
    val1:=d.regs[reg_trg];
    while (siz1>0) do begin;
      if (siz1>sizeof(buf)) then siz2:=sizeof(buf) else siz2:=siz1;
      if processCopyTo(d,buf,siz2,val1) then begin; siz1:=13;goto err; end;
      dec(siz1,siz2);
      inc(val1,siz2);
      end;
    end;
  else goto sysErr;
  end;
goto next;

sys100:
case siz1 of
  100:begin; {pipeline.startListen}
    if (d.lstnPipe<>0) then begin; siz1:=8;goto res; end;
    siz1:=pipelineCreate(d.pid,0,4000,0);
    if (siz1=0) then begin; siz1:=2;goto res; end;
    d.lstnPipe:=siz1;
    siz1:=0;
    goto res;
    end;
  101:begin; {pipeline.stopListen}
    if (d.lstnPipe=0) then begin; siz1:=8;goto res; end;
    pipelineClose(d.lstnPipe,d.pid);
    siz1:=0;
    goto res;
    end;
  102:begin; {pipeline.getIncoming}
    d.regs[reg_a]:=0;
    if (d.lstnPipe=0) then begin; siz1:=8;goto res; end;
    siz2:=listingOfs(listingPipe,d.lstnPipe);
    siz1:=sizeof(val1);
    if pipelineRecv(totalMemoryByte^[siz2],val1,siz1,d.pid) then siz1:=0;
    if (siz1<>sizeof(val1)) then begin; siz1:=8;goto res; end;
    d.regs[reg_a]:=val1;
    siz1:=0;
    goto res;
    end;
  103:begin; {pipeline.create}
    val1:=d.regs[reg_a];
    if (val1=kernelFakeNum) then begin;
      val2:=pipelineCreate(d.pid,kernelProcNum,d.regs[reg_c],d.regs[reg_b] and $ff);
      if (val2=0) then begin; siz1:=2;goto res; end;
      releq2incoming(val2);
      d.regs[reg_a]:=val2;
      d.regs[reg_b]:=0;
      goto done;
      end;
    d.regs[reg_a]:=0;
    siz1:=listingOfs(listingProc,val1);
    if (siz1=0) then begin; siz1:=8;goto res; end;
    move(totalMemoryByte^[siz1],proc2,sizeof(proc2));
    siz2:=proc2.lstnPipe;
    if (siz2=0) then begin; siz1:=8;goto res; end;
    siz2:=listingOfs(listingPipe,siz2);
    if (siz2=0) then begin; siz1:=8;goto res; end;
    val2:=pipelineCreate(d.pid,val1,d.regs[reg_c],d.regs[reg_b] and $ff);
    if (val2=0) then begin; siz1:=2;goto res; end;
    if pipelineSend(totalMemoryByte^[siz2],val2,sizeof(val2),0) then begin; siz1:=2;goto res; end;
    d.regs[reg_a]:=val2;
    siz1:=0;
    goto res;
    end;
  104:begin; {pipeline.close}
    if pipelineClose(d.regs[reg_a],d.pid) then begin; siz1:=8;goto res; end;
    siz1:=0;
    goto res;
    end;
  105:begin; {pipeline.info}
    siz1:=d.regs[reg_a];
    d.regs[reg_a]:=0;
    d.regs[reg_c]:=1024;
    d.regs[reg_d]:=0;
    siz1:=listingOfs(listingPipe,siz1);
    if (siz1=0) then begin; siz1:=8;goto res; end;
    if pipelineInfo(totalMemoryByte^[siz1],d.pid,val1,val2,siz2) then begin; siz1:=8;goto res; end;
    d.regs[reg_a]:=val1;
    d.regs[reg_d]:=val2;
    d.regs[reg_c]:=siz2;
    siz1:=0;
    goto res;
    end;
  106:begin; {pipeline.receive}
    siz2:=d.regs[reg_c] and $ffff;
    d.regs[reg_c]:=0;
    val1:=listingOfs(listingPipe,d.regs[reg_a]);
    if (val1=0) then begin; siz1:=8;goto res; end;
    if pipelineRecv(totalMemoryByte^[val1],totalMemoryByte^[kernelBigBuf],siz2,d.pid) then begin; siz1:=2;goto res; end;
    if processCopyTo(d,totalMemoryByte^[kernelBigBuf],siz2,d.regs[reg_trg]) then begin; siz1:=13;goto err; end;
    d.regs[reg_c]:=siz2;
    siz1:=0;
    goto res;
    end;
  107:begin; {pipeline.send}
    siz1:=d.regs[reg_c] and $ffff;
    val1:=listingOfs(listingPipe,d.regs[reg_a]);
    if (val1=0) then begin; siz1:=8;goto res; end;
    if processCopyFrom(d,totalMemoryByte^[kernelBigBuf],siz1,d.regs[reg_src]) then begin; siz1:=13;goto err; end;
    if pipelineSend(totalMemoryByte^[val1],totalMemoryByte^[kernelBigBuf],siz1,d.pid) then begin; siz1:=2;goto res; end;
    siz1:=0;
    goto res;
    end;
  108:begin; {system.getPID}
    d.regs[reg_a]:=d.pid;
    d.regs[reg_b]:=d.ppid;
    d.regs[reg_c]:=d.right;
    end;
  109:begin; {system.getUID}
    d.regs[reg_a]:=d.cuid;
    d.regs[reg_b]:=d.ouid;
    end;
  110:begin; {system.sysInfoNum}
    d.regs[reg_a]:=listingUsed(listingProc);
    d.regs[reg_c]:=listingUsed(listingPipe);
    d.regs[reg_d]:=listingUsed(listingFile);
    end;
  111:begin; {system.sysInfoMem}
    d.regs[reg_a]:=totalMemorySize;
    d.regs[reg_c]:=memoryCountPID(kernelProcNum);
    d.regs[reg_d]:=memoryCountPID(0);
    end;
  112:begin; {system.sysInfoProc}
    d.regs[reg_a]:=timesRelequished;
    d.regs[reg_b]:=timesFullRounds+timesHalfRounds;
    d.regs[reg_c]:=timesFullRounds;
    d.regs[reg_d]:=timesActiveStarts;
    end;
  113:begin; {system.procInfoNam}
    val1:=listingOfs(listingProc,d.regs[reg_a]);
    if (val1=0) then begin;
      fillchar(buf,sizeof(buf),0);
      d.regs[reg_a]:=0;
      d.regs[reg_c]:=0;
      d.regs[reg_d]:=0;
      end else begin;
      move(totalMemoryByte^[val1],proc2,sizeof(proc2));
      d.regs[reg_a]:=proc2.cuid;
      d.regs[reg_c]:=proc2.ppid;
      d.regs[reg_d]:=proc2.right;
      move(proc2.filNam,buf[1],sizeof(String));
      move(proc2.params,buf[sizeof(String)+1],sizeof(String));
      end;
    if processCopyTo(d,buf,sizeof(String)*2,d.regs[reg_trg]) then begin; siz1:=13;goto err; end;
    end;
  114:begin; {system.procInfoNum}
    val1:=d.regs[reg_a];
    d.regs[reg_a]:=memoryCountPID(val1);
    d.regs[reg_c]:=pipelineCountPID(val1);
    d.regs[reg_d]:=handleCountPID(val1);
    end;
  115:begin; {system.procInfoRun}
    val1:=listingOfs(listingProc,d.regs[reg_a]);
    if (val1=0) then begin;
      d.regs[reg_a]:=0;
      d.regs[reg_b]:=0;
      d.regs[reg_c]:=0;
      goto next;
      end;
    move(totalMemoryByte^[val1],proc2,sizeof(proc2));
    d.regs[reg_a]:=proc2.workSince;
    d.regs[reg_b]:=proc2.timesBusy;
    d.regs[reg_c]:=proc2.activeRun;
    end;
  116:begin; {system.findProcNum}
    val1:=listingSeqNum(listingProc,d.regs[reg_a]+1);
    val1:=listingDataOfs(listingProc,val1);
    if (val1<>0) then begin;
      move(totalMemoryByte^[val1],proc2,sizeof(proc2));
      val1:=proc2.pid;
      end;
    d.regs[reg_a]:=val1;
    end;
  117:begin; {system.findProcNam}
    if processCopyFrom(d,a,sizeof(a),d.regs[reg_src]) then begin; siz1:=13;goto err; end;
    d.regs[reg_a]:=processFindName(a);
    end;
  118:begin; {system.cpuInfo}
    d.regs[reg_a]:=1;
    d.regs[reg_c]:=10;
    a:='1234'#0;
    WriteLongLSB(a[1],executableMagic);
    if processCopyTo(d,a[1],sizeof(a),d.regs[reg_trg]) then begin; siz1:=13;goto err; end;
    end;
  119:begin; {system.kernelInfo}
    siz1:=0;
    add2buf(kernelInf1);
    add2buf(kernelInf2);
    add2buf(#0);
    if processCopyTo(d,buf,siz1,d.regs[reg_trg]) then begin; siz1:=13;goto err; end;
    end;
  120:begin; {system.kernelLogo}
    siz1:=0;
    add2buf(kernelLog1);
    add2buf(kernelLog2);
    add2buf(#0);
    if processCopyTo(d,buf,siz1,d.regs[reg_trg]) then begin; siz1:=13;goto err; end;
    end;
  121:begin; {system.procLive}
    val1:=listingOfs(listingProc,d.regs[reg_a]);
    if (val1<>0) then val1:=1;
    d.regs[reg_b]:=val1;
    end;
  122:begin; {system.uptimeInfo}
    BugOS_KernelUptime(d.regs[reg_a],d.regs[reg_c],d.regs[reg_d]);
    end;
  123:begin; {system.killProcess}
    val2:=d.regs[reg_a];
    if (val2=d.pid) then begin; siz1:=5;goto res; end;
    val1:=listingOfs(listingProc,val2);
    if (val1=0) then begin; siz1:=7;goto res; end;
    if (d.cuid<>0) then begin;
      move(totalMemoryByte^[val1],proc2,sizeof(proc2));
      if (proc2.cuid<>d.cuid) then begin; siz1:=4;goto res; end;
      end;
    processKillPID(val2,18,0);
    siz1:=0;
    goto res;
    end;
  else goto sysErr;
  end;
goto next;

sys200:
case siz1 of
  243:begin; {system.setUID}
    if (d.right and extraRightsMask=0) then begin; siz1:=4;goto res; end;
    d.cuid:=d.regs[reg_a];
    end;
  251:begin; {system.driveLogin}
    if (handleFindDrivePID(d.pid)>=0) then begin; siz1:=5;goto res; end;
    if (d.right and extraRightsMask=0) then begin; siz1:=4;goto res; end;
    siz1:=lowCaseByte(d.regs[reg_a]);
    if (listingDrvr[siz1].drivePid<>0) then begin; siz1:=8;goto res; end;
    fillchar(listingDrvr[siz1],sizeof(listingDrvr[siz1]),0);
    listingDrvr[siz1].drivePid:=d.pid;
    listingDrvr[siz1].driveBuf:=d.regs[reg_src];
    siz1:=0;
    goto res;
    end;
  252:begin; {system.driveLogout}
    if (d.right and extraRightsMask=0) then begin; siz1:=4;goto res; end;
    siz1:=lowCaseByte(d.regs[reg_a]);
    if (listingDrvr[siz1].drivePid<>d.pid) then begin; siz1:=8;goto res; end;
    fillchar(listingDrvr[siz1],sizeof(listingDrvr[siz1]),0);
    siz1:=0;
    goto res;
    end;
  253:begin; {system.driveFinished}
    if (d.right and extraRightsMask=0) then begin; siz1:=4;goto res; end;
    if driveDoneCmd(d) then begin; siz1:=13;goto err; end;
    end;
  254:begin; {system.dropPrivi}
    if (d.right and extraRightsMask=0) then begin; siz1:=4;goto res; end;
    d.right:=d.right and (not extraRightsMask);
    end;
  else goto sysErr;
  end;
goto next;
End;
