Procedure CompileOneCpuLine;
Const
  sizeTable1:array[0..4] of byte=($ff,0,1,$ff,2);
  sizeTable2:array[0..4] of byte=($ff,1,2,$ff,3);
  sizeTable3:array[0..4] of byte=($ff,1,3,$ff,2);
Var
  Cmd:String;
  p1:OneParameterRec;
  p2:OneParameterRec;
  p3:OneParameterRec;
  p4:OneParameterRec;
  p5:OneParameterRec;
  p6:OneParameterRec;


Procedure nextParam(var a,b:String);
Var i,o,p:LongInt;
Begin;
b:=xLevesz(b);
i:=Pos(',',b+',');
o:=Pos('(',b+'(');
p:=Pos('<',b+'<');
if (i<=o) and (i<=p) then begin;
  a:=copy(b,1,i-1);
  b:=copy(b,i+1,255);
  exit;
  end;
o:=Pos(')',b+')');
p:=Pos('>',b+'>');
if (p<o) then o:=p;
a:=copy(b,1,o);
b:=copy(b,o+1,255);
i:=Pos(',',b+',');
a:=a+copy(b,1,i-1);
b:=copy(b,i+1,255);
End;




Const
  Str_abcd='|abcd|abcd.b|addx|addx.b|addx.w|addx.l|sbcd|sbcd.b|'+
           'subx|subx.b|subx.w|subx.l|';
Procedure Inst_abcd;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Typ<>p2.Typ) then exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='abcd') then begin; i:=$c100;p:=1; end;
if (Cmd='addx') then i:=$d100;
if (Cmd='sbcd') then begin; i:=$8100;p:=1; end;
if (Cmd='subx') then i:=$9100;
if (i=0) then exit;
i:=i or (sizeTable1[p] shl 6);
if (p1.Typ=1) then begin;
  if (p1.Reg.Num>8) then exit;
  if (p2.Reg.Num>8) then exit;
  i:=i or (p1.Reg.Num-1);
  i:=i or ((p2.Reg.Num-1) shl 9);
  OutStr:=b2h(i shr 8)+SepCh+b2h(i);
  OutOk:=True;
  exit;
  end;
if (ConvertAllMemoryDatas(p1,p,OutStr) and $f8<>$20) then exit;
if (ConvertAllMemoryDatas(p2,p,OutStr) and $f8<>$20) then exit;
i:=i or (p1.Mem.Reg3.Num-9);
i:=i or ((p2.Mem.Reg3.Num-9) shl 9);
i:=i or $08;
OutStr:=b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_add='|add|add.b|add.w|add.l|and|and.b|and.w|and.l|or|or.b|or.w|or.l|'+
          'sub|sub.b|sub.w|sub.l|';
Procedure Inst_add;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='add') then i:=$d000;
if (Cmd='and') then i:=$c000;
if (Cmd='or') then i:=$8000;
if (Cmd='sub') then i:=$9000;
if (i=0) then exit;
if (p2.Typ=1) then begin;
  if not (p2.Reg.Num in [1..8]) then exit;
  o:=ConvertAllMemoryDatas(p1,p,OutStr);
  if (o<0) then exit;
  OutStr:=b2h((i shr 8) or ((p2.Reg.Num-1) shl 1))+SepCh+b2h((sizeTable1[p] shl 6) or o)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
if (p1.Typ=1) then begin;
  if not (p1.Reg.Num in [1..8]) then exit;
  o:=ConvertAllMemoryDatas(p2,p,OutStr);
  if (o<0) then exit;
  OutStr:=b2h((i shr 8) or ((p1.Reg.Num-1) shl 1) or 1)+SepCh+b2h((sizeTable1[p] shl 6) or o)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
End;


Const
  Str_adda='|adda|adda.w|adda.l|cmpa|cmpa.w|cmpa.l|suba|suba.w|suba.l|';
Procedure Inst_adda;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='adda') then i:=$d0c0;
if (Cmd='cmpa') then i:=$b0c0;
if (Cmd='suba') then i:=$90c0;
if (i=0) then exit;
if (p>2) then i:=i or $100;
if not (p2.Reg.Num in [9..16]) then exit;
o:=ConvertAllMemoryDatas(p1,p,OutStr);
if (o<0) then exit;
OutStr:=b2h((i shr 8) or ((p2.Reg.Num-9) shl 1))+SepCh+b2h(i or o)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_addi='|addi|addi.b|addi.w|addi.l|andi|andi.b|andi.w|andi.l|callm|'+
           'cmpi|cmpi.b|cmpi.w|cmpi.l|eori|eori.b|eori.w|eori.l|'+
           'ori|ori.b|ori.w|ori.l|subi|subi.b|subi.w|subi.l|';
Procedure Inst_addi;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=-1;
if (Cmd='addi') then i:=$0600;
if (Cmd='andi') then i:=$0200;
if (Cmd='callm') then i:=$06c0;
if (Cmd='cmpi') then i:=$0c00;
if (Cmd='eori') then i:=$0a00;
if (Cmd='ori') then i:=$0000;
if (Cmd='subi') then i:=$0400;
if (i<0) then exit;
o:=ConvertAllMemoryDatas(p2,p,OutStr);
if (o<0) then exit;
a:=ConvertNumOrLabDatas(p1,p,2,'');
if (a='') then exit;
i:=i or (sizeTable1[p] shl 6);
OutStr:=b2h(i shr 8)+SepCh+b2h(i or o)+SepCh+a+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_addq='|addq|addq.b|addq.w|addq.l|subq|subq.b|subq.w|subq.l|';
Procedure Inst_addq;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='addq') then i:=$5000;
if (Cmd='subq') then i:=$5100;
if (i=0) then exit;
o:=BVal('$'+p1.Num);
if (o<1) or (o>8) then exit;
i:=i or ((o and 7) shl 9);
o:=ConvertAllMemoryDatas(p2,p,OutStr);
if (o<0) then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h((sizeTable1[p] shl 6) or o)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_andiccr='|andi|andi.b|eori|eori.b|ori|ori.b|';
Procedure Inst_andiccr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>4) then Exit;
if (p3.Typ<>0) then Exit;
if (p2.Str<>'ccr') then exit;
p:=AnalizeSizePostfix1(Cmd,1);
i:=0;
if (Cmd='andi') then i:=$023c;
if (Cmd='eori') then i:=$0a3c;
if (Cmd='ori') then i:=$003c;
if (i=0) then exit;
OutStr:=ConvertNumOrLabDatas(p1,2,2,'');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_asl='|asl|asl.b|asl.w|asl.l|asr|asr.b|asr.w|asr.l|'+
          'lsl|lsl.b|lsl.w|lsl.l|lsr|lsr.b|lsr.w|lsr.l|'+
          'rol|rol.b|rol.w|rol.l|ror|ror.b|ror.w|ror.l|'+
          'roxl|roxl.b|roxl.w|roxl.l|roxr|roxr.b|roxr.w|roxr.l|';
Procedure Inst_asl;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='asl') then i:=$e100;
if (Cmd='asr') then i:=$e000;
if (Cmd='lsl') then i:=$e108;
if (Cmd='lsr') then i:=$e008;
if (Cmd='rol') then i:=$e118;
if (Cmd='ror') then i:=$e018;
if (Cmd='roxl') then i:=$e110;
if (Cmd='roxr') then i:=$e010;
if (i=0) then exit;
i:=i or (sizeTable1[p] shl 6);
if (p1.Typ=1) and (p2.Typ=1) then begin;
  if (p1.Reg.Num<1) or (p1.Reg.Num>8) then exit;
  if (p2.Reg.Num<1) or (p2.Reg.Num>8) then exit;
  i:=i or (p2.Reg.Num-1);
  i:=i or ((p1.Reg.Num-1) shl 9);
  i:=i or $20;
  OutStr:=b2h(i shr 8)+SepCh+b2h(i);
  OutOk:=True;
  exit;
  end;
if (p1.Typ=2) and (p2.Typ=1) then begin;
  if (p2.Reg.Num<1) or (p2.Reg.Num>8) then exit;
  o:=BVal('$'+p1.Num);
  if (o<1) or (o>8) then exit;
  i:=i or ((o and 7) shl 9);
  i:=i or (p2.Reg.Num-1);
  OutStr:=b2h(i shr 8)+SepCh+b2h(i);
  OutOk:=True;
  exit;
  end;
if (p2.Typ<>0) then exit;
i:=i or $c0;
i:=i or ((i and $18) shl 6);
i:=i and $fff7;
o:=ConvertAllMemoryDatas(p1,p,OutStr);
if (o<0) then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i or o)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_bchg='|bchg|bclr|bset|btst|';
Procedure Inst_bchg;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=-1;
if (Cmd='bchg') then i:=$0040;
if (Cmd='bclr') then i:=$0080;
if (Cmd='bset') then i:=$00c0;
if (Cmd='btst') then i:=$0000;
if (i<0) then exit;
if (p1.Typ=1) then begin;
  if (p1.Reg.Num<1) or (p1.Reg.Num>8) then exit;
  i:=i or ((p1.Reg.Num-1) shl 9);
  i:=i or $100;
  o:=ConvertAllMemoryDatas(p2,p,OutStr);
  if (o<0) then exit;
  OutStr:=b2h(i shr 8)+SepCh+b2h(i or o)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
if not (p1.Typ in [2,4]) then exit;
i:=i or $800;
a:=ConvertNumOrLabDatas(p1,2,2,'');
if (a='') then exit;
o:=ConvertAllMemoryDatas(p2,p,OutStr);
if (o<0) then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i or o)+SepCh+a+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_bfchg='|bfchg|bfclr|bfset|bftst|';
Procedure Inst_bfchg;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if not (p2.Typ in [1,2]) then Exit;
if not (p3.Typ in [1,2]) then Exit;
if (p4.Typ<>0) then exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='bfchg') then i:=$eac0;
if (Cmd='bfclr') then i:=$ecc0;
if (Cmd='bfset') then i:=$eec0;
if (Cmd='bftst') then i:=$e8c0;
if (i=0) then exit;
o:=ConvertAllMemoryDatas(p1,p,OutStr);
if (o<0) then exit;
i:=i or o;
if (p2.Typ=2) then begin;
  o:=BVal('$'+p2.Num);
  if (o<0) or (o>31) then exit;
  o:=o shl 6;
  end else begin;
  o:=p2.Reg.Num-1;
  if (o<0) or (o>7) then exit;
  o:=(o shl 6) or $800;
  end;
if (p3.Typ=2) then begin;
  p:=BVal('$'+p3.Num);
  if (p<0) or (p>31) then exit;
  o:=o or p;
  end else begin;
  p:=p3.Reg.Num-1;
  if (p<0) or (p>7) then exit;
  o:=o or p or $20;
  end;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+b2h(o shr 8)+SepCh+b2h(o)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_bfexts='|bfexts|bfextu|bfffo|';
Procedure Inst_bfexts;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if not (p2.Typ in [1,2]) then Exit;
if not (p3.Typ in [1,2]) then Exit;
if (p4.Typ<>1) then exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='bfexts') then i:=$ebc0;
if (Cmd='bfextu') then i:=$e9c0;
if (Cmd='bfffo') then i:=$edc0;
if (i=0) then exit;
o:=ConvertAllMemoryDatas(p1,p,OutStr);
if (o<0) then exit;
i:=i or o;
if (p2.Typ=2) then begin;
  o:=BVal('$'+p2.Num);
  if (o<0) or (o>31) then exit;
  o:=o shl 6;
  end else begin;
  o:=p2.Reg.Num-1;
  if (o<0) or (o>7) then exit;
  o:=(o shl 6) or $800;
  end;
if (p3.Typ=2) then begin;
  p:=BVal('$'+p3.Num);
  if (p<0) or (p>31) then exit;
  o:=o or p;
  end else begin;
  p:=p3.Reg.Num-1;
  if (p<0) or (p>7) then exit;
  o:=o or p or $20;
  end;
p:=p4.Reg.Num-1;
if (p<0) or (p>7) then exit;
o:=o or (p shl 12);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+b2h(o shr 8)+SepCh+b2h(o)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_bfins='|bfins|';
Procedure Inst_bfins;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then exit;
if (p2.Typ=0) then Exit;
if not (p3.Typ in [1,2]) then Exit;
if not (p4.Typ in [1,2]) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='bfins') then i:=$efc0;
if (i=0) then exit;
o:=ConvertAllMemoryDatas(p2,p,OutStr);
if (o<0) then exit;
i:=i or o;
if (p3.Typ=2) then begin;
  o:=BVal('$'+p3.Num);
  if (o<0) or (o>31) then exit;
  o:=o shl 6;
  end else begin;
  o:=p3.Reg.Num-1;
  if (o<0) or (o>7) then exit;
  o:=(o shl 6) or $800;
  end;
if (p4.Typ=2) then begin;
  p:=BVal('$'+p4.Num);
  if (p<0) or (p>31) then exit;
  o:=o or p;
  end else begin;
  p:=p4.Reg.Num-1;
  if (p<0) or (p>7) then exit;
  o:=o or p or $20;
  end;
p:=p1.Reg.Num-1;
if (p<0) or (p>7) then exit;
o:=o or (p shl 12);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+b2h(o shr 8)+SepCh+b2h(o)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_bkpt='|bkpt|trap|';
Procedure Inst_bkpt;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='bkpt') then begin; p:=7;i:=$4848; end;
if (Cmd='trap') then begin; p:=15;i:=$4e40; end;
if (i=0) then exit;
o:=BVal('$'+p1.Num);
if (o<0) or (o>p) then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i or o);
OutOk:=True;
End;


Const
  Str_cas='|cas|cas.b|cas.w|cas.l|';
Procedure Inst_cas;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ=0) then Exit;
if (p4.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='cas') then i:=$08c0;
if (i=0) then exit;
o:=ConvertAllMemoryDatas(p3,p,OutStr);
if (o<0) then exit;
i:=i or (sizeTable2[p] shl 9);
i:=i or o;
p:=p1.Reg.Num-1;
if (p<0) or (p>7) then exit;
o:=p2.Reg.Num-1;
if (o<0) or (o>7) then exit;
p:=p or (o shl 6);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+b2h(p shr 8)+SepCh+b2h(p)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_cas2='|cas2|cas2.b|cas2.w|cas2.l|';
Procedure Inst_cas2;
Var i,o,p,q:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>1) then Exit;
if (p5.Typ<>1) then Exit;
if (p6.Typ<>1) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='cas2') then i:=$08fc;
if (i=0) then exit;
i:=i or (sizeTable2[p] shl 9);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh;
o:=p1.Reg.Num-1;
if (p<0) or (p>7) then exit;
p:=p2.Reg.Num-1;
if (p<0) or (p>7) then exit;
i:=p3.Reg.Num-1;
if (i<0) or (i>7) then exit;
o:=o or (i shl 6);
i:=p4.Reg.Num-1;
if (i<0) or (i>7) then exit;
p:=p or (i shl 6);
i:=p5.Reg.Num-1;
if (i<0) or (i>15) then exit;
o:=o or (i shl 12);
i:=p6.Reg.Num-1;
if (i<0) or (i>15) then exit;
p:=p or (i shl 12);
OutStr:=OutStr+b2h(o shr 8)+SepCh+b2h(o)+SepCh+b2h(p shr 8)+SepCh+b2h(p);
OutOk:=True;
End;


Const
  Str_chk='|chk|chk.w|chk.l|';
Procedure Inst_chk;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='chk') then i:=$4000;
if (i=0) then exit;
if not (p2.Reg.Num in [1..8]) then exit;
o:=ConvertAllMemoryDatas(p1,p,OutStr);
if (o<0) then exit;
i:=i or (sizeTable3[p] shl 7);
OutStr:=b2h((i shr 8) or ((p2.Reg.Num-1) shl 1))+SepCh+b2h(i or o)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_chk2='|chk2|chk2.b|chk2.w|chk2.l|cmp2|cmp2.b|cmp2.w|cmp2.l|';
Procedure Inst_chk2;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='chk2') then i:=$080000c0;
if (Cmd='cmp2') then i:=$000000c0;
if (i=0) then exit;
if not (p2.Reg.Num in [1..16]) then exit;
o:=ConvertAllMemoryDatas(p1,p,OutStr);
if (o<0) then exit;
i:=i or (sizeTable1[p] shl 9);
OutStr:=b2h(i shr 8)+SepCh+b2h(i or o)+SepCh+b2h(((p2.Reg.Num-1) shl 4) or (i shr 24))+SepCh+b2h(i shr 16)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_clr='|clr|clr.b|clr.w|clr.l|jmp|jsr|nbcd|nbcd.b|neg|neg.b|neg.w|neg.l|'+
          'negx|negx.b|negx.w|negx.l|not|not.b|not.w|not.l|pea|pea.l|'+
          'tas|tas.b|tst|tst.b|tst.w|tst.l|st|sf|shi|sls|scc|scs|sne|seq|'+
          'svc|svs|spl|smi|sge|slt|sgt|sle|';
Procedure Inst_clr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='clr') then i:=$4200;
if (Cmd='jmp') then begin; p:=2;i:=$4ec0; end;
if (Cmd='jsr') then begin; p:=1;i:=$4e80; end;
if (Cmd='nbcd') then begin; p:=1;i:=$4800; end;
if (Cmd='neg') then i:=$4400;
if (Cmd='negx') then i:=$4000;
if (Cmd='not') then i:=$4600;
if (Cmd='pea') then begin; p:=1;i:=$4840; end;
if (Cmd='tst') then i:=$4a00;
if (Cmd='tas') then begin; p:=1;i:=$4ac0; end;
if (Cmd[1]='s') then begin;
  i:=AnalizeCpuCond(copy(Cmd,2,255));
  if (i<0) then exit;
  i:=(i shl 8) or $50c0;
  p:=1;
  end;
if (i=0) then exit;
o:=ConvertAllMemoryDatas(p1,p,OutStr);
if (o<0) then exit;
i:=i or (sizeTable1[p] shl 6);
OutStr:=b2h(i shr 8)+SepCh+b2h(i or o)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_cmp='|cmp|cmp.b|cmp.w|cmp.l|';
Procedure Inst_cmp;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='cmp') then i:=$b000;
if (i=0) then exit;
if not (p2.Reg.Num in [1..8]) then exit;
o:=ConvertAllMemoryDatas(p1,p,OutStr);
if (o<0) then exit;
i:=i or (sizeTable1[p] shl 6);
OutStr:=b2h((i shr 8) or ((p2.Reg.Num-1) shl 1))+SepCh+b2h(i or o)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_cmpm='|cmpm|cmpm.b|cmpm.w|cmpm.l|';
Procedure Inst_cmpm;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='cmpm') then i:=$b108;
if (i=0) then exit;
i:=i or (sizeTable1[p] shl 6);
if (ConvertAllMemoryDatas(p1,p,OutStr) and $f8<>$18) then exit;
if (ConvertAllMemoryDatas(p2,p,OutStr) and $f8<>$18) then exit;
i:=i or (p1.Mem.Reg3.Num-9);
i:=i or ((p2.Mem.Reg3.Num-9) shl 9);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_divs='|divs|divs.w|divs.l|divsl.l|divu|divu.w|divu.l|divul.l|'+
           'muls|muls.w|muls.l|mulu|mulu.w|mulu.l|';
Procedure Inst_divs;
Var i,o,p,q:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
q:=0;
if (Cmd='divs') then i:=$080081c0;
if (Cmd='divsl') then i:=$0c0081c0;
if (Cmd='divu') then i:=$000080c0;
if (Cmd='divul') then i:=$040080c0;
if (Cmd='muls') then begin; i:=$0800c1c0;q:=$04000040; end;
if (Cmd='mulu') then begin; i:=$0000c0c0;q:=$04000040; end;
if (i=0) then exit;
o:=ConvertAllMemoryDatas(p1,p,OutStr);
if (o<0) then exit;
i:=i or o;
if (p=2) then begin;
  if (p3.Typ<>0) then Exit;
  o:=p2.Reg.Num-1;
  if (o<0) or (o>7) then exit;
  i:=i or (o shl 9);
  OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
if (p<>4) then exit;
i:=(i and $ffff003f) or $4c40 xor q;
if (p3.Typ=0) then begin;
  p3:=p2;
  end else i:=i or (q and $ffff0000);
o:=p2.Reg.Num-1;
if (o<0) or (o>7) then exit;
i:=i or (o shl 16);
o:=p3.Reg.Num-1;
if (o<0) or (o>7) then exit;
i:=i or (o shl 28);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_eor='|eor|eor.b|eor.w|eor.l|';
Procedure Inst_eor;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='eor') then i:=$b100;
if (i=0) then exit;
if not (p1.Reg.Num in [1..8]) then exit;
o:=ConvertAllMemoryDatas(p2,p,OutStr);
if (o<0) then exit;
i:=i or (sizeTable1[p] shl 6);
OutStr:=b2h((i shr 8) or ((p1.Reg.Num-1) shl 1))+SepCh+b2h(i or o)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_exg='|exg|';
Procedure Inst_exg;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='exg') then i:=$c140;
if (i=0) then exit;
o:=p1.Reg.Num-1;
p:=p2.Reg.Num-1;
if (o<0) or (o>15) then exit;
if (p<0) or (p>15) then exit;
if (p and 8<>o and 8) then exit;
i:=i or ((p and 7) shl 9);
i:=i or o;
OutStr:=b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_ext='|ext.w|ext.l|extb.l|rtm|swap|unlk|';
Procedure Inst_ext;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
if (Cmd='ext.w') then begin; p:=7;i:=$4880; end;
if (Cmd='ext.l') then begin; p:=7;i:=$48c0; end;
if (Cmd='extb.l') then begin; p:=7;i:=$49c0; end;
if (Cmd='rtm') then begin; p:=15;i:=$06c0; end;
if (Cmd='swap') then begin; p:=7;i:=$4840; end;
if (Cmd='unlk') then begin; p:=7;i:=$4e58; dec(p1.Reg.Num,8); end;
if (i=0) then exit;
o:=p1.Reg.Num-1;
if (o<0) or (o>p) then exit;
i:=i or o;
OutStr:=b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_illegal='|illegal|';
Procedure Inst_illegal;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
if (Cmd='illegal') then i:=$4afc;
if (i=0) then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_lea='|lea|';
Procedure Inst_lea;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='lea') then i:=$41c0;
if (i=0) then exit;
if not (p2.Reg.Num in [9..16]) then exit;
o:=ConvertAllMemoryDatas(p1,p,OutStr);
if (o<0) then exit;
i:=i or (sizeTable1[p] shl 6);
OutStr:=b2h((i shr 8) or ((p2.Reg.Num-9) shl 1))+SepCh+b2h(i or o)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_link='|link|link.w|link.l|';
Procedure Inst_link;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [2,4]) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='link') then i:=$4800;
if (i=0) then exit;
if not (p1.Reg.Num in [9..16]) then exit;
if (p>2) then i:=i or 8 else i:=i or $650;
i:=i or (p1.Reg.Num-9);
OutStr:=ConvertNumOrLabDatas(p2,p,2,'');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_move='|move|move.b|move.w|move.l|';
Procedure Inst_move;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,1);
i:=-1;
if (Cmd='move') then i:=$0000;
if (i<0) then exit;
i:=i or (sizeTable3[p] shl 12);
o:=ConvertAllMemoryDatas(p1,p,OutStr);
if (o<0) then exit;
i:=i or o;
o:=ConvertAllMemoryDatas(p2,p,a);
if (o<0) then exit;
o:=((o and 7) shl 3) or ((o and $38) shr 3);
i:=i or (o shl 6);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr+SepCh+a;
OutOk:=True;
End;


Const
  Str_movea='|movea|movea.w|movea.l|';
Procedure Inst_movea;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='movea') then i:=$0040;
if (i=0) then exit;
o:=p2.Reg.Num-9;
if (o<0) or (o>7) then exit;
i:=i or (o shl 9);
o:=ConvertAllMemoryDatas(p1,p,OutStr);
if (o<0) then exit;
i:=i or o;
i:=i or (sizeTable3[p] shl 12);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_moveccr='|move|move.b|';
Procedure Inst_moveccr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,1);
i:=0;
if (p1.Str='ccr') then i:=$42c0;
if (p2.Str='ccr') then begin; i:=$44c0;p2:=p1; end;
if (p1.Str='sr') then i:=$40c0;
if (i=0) then exit;
o:=ConvertAllMemoryDatas(p2,p,OutStr);
if (o<0) then exit;
i:=i or o;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_move16='|move16|';
Procedure Inst_move16;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
i:=ConvertAllMemoryDatas(p1,4,OutStr);
o:=ConvertAllMemoryDatas(p2,4,OutStr);
if (i and $f8=$18) and (o and $f8=$18) then begin;
  OutStr:=b2h($f6)+SepCh+b2h((i and 7) or $20)+SepCh+b2h(((o and 7) shl 4) or $80)+SepCh+b2h($00);
  OutOk:=True;
  exit;
  end;
p:=-1;
if (i and $f8=$38) and (o and $f8=$10) then begin;
  {num,(adr)}
  OutStr:=p1.Str;
  p:=(o and 7) or $18;
  end;
if (i and $f8=$38) and (o and $f8=$18) then begin;
  {num,(adr)+}
  OutStr:=p1.Str;
  p:=(o and 7) or $08;
  end;
if (i and $f8=$10) and (o and $f8=$38) then begin;
  {(adr),num}
  OutStr:=p2.Str;
  p:=(i and 7) or $10;
  end;
if (i and $f8=$18) and (o and $f8=$38) then begin;
  {(adr),num}
  OutStr:=p2.Str;
  p:=(i and 7);
  end;
if (p<0) then exit;
outStr:=copy(outStr,2,length(outStr)-2);
if (outStr='') then exit;
AnalizeParameter(outStr,1,p6);
if not (p6.Typ in [2,4]) then exit;
OutStr:=ConvertNumOrLabDatas(p6,4,4,'');
if (OutStr='') then exit;
p:=p or $f600;
OutStr:=b2h(p shr 8)+SepCh+b2h(p)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_movem='|movem|movem.w|movem.l|';
Procedure Inst_movem;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=$4880;
if (p>2) then i:=i or $40;
if (p1.Typ=0) then begin;
  o:=AnalizeCpuRegList(p1.Str);
  if (o<0) then exit;
  p:=ConvertAllMemoryDatas(p2,2,OutStr);
  if (p<0) then exit;
  i:=i or p;
  OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+b2h(o shr 8)+SepCh+b2h(o)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
if (p2.Typ=0) then begin;
  o:=AnalizeCpuRegList(p2.Str);
  if (o<0) then exit;
  p:=ConvertAllMemoryDatas(p1,2,OutStr);
  if (p<0) then exit;
  i:=i or p;
  i:=i or $400;
  OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+b2h(o shr 8)+SepCh+b2h(o)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
End;


Const
  Str_movep='|movep|movep.w|movep.l|';
Procedure Inst_movep;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=$0108;
if (p>2) then i:=i or $40;
if (p2.Typ=1) then begin;
  p6:=p1;
  p1:=p2;
  p2:=p6;
  end else i:=i or $80;
if (p1.Typ<>1) then exit;
if not (p1.Typ in [1..8]) then exit;
i:=i or ((p1.Reg.Num-1) shl 9);
outStr:=p2.Str;
if (copy(outStr,1,1)<>'(') then exit;
if (copy(outStr,length(outStr),1)<>')') then exit;
outStr:=copy(outStr,2,length(outStr)-2);
o:=pos(',',outStr);
AnalizeParameter(copy(outStr,1,o-1),1,p5);
AnalizeParameter(copy(outStr,o+1,255),1,p6);
if (p6.Typ<>1) then exit;
if not (p6.Reg.Num in [9..16]) then exit;
i:=i or (p6.Reg.Num-9);
OutStr:=ConvertNumOrLabDatas(p5,2,2,'');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_moveq='|moveq|moveq.l|';
Procedure Inst_moveq;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
i:=p2.Reg.Num-1;
if (i<0) or (i>7) then exit;
i:=$7000 or (i shl 9);
OutStr:=ConvertNumOrLabDatas(p1,1,1,'');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_nop='|nop|rtr|rts|trapv|';
Procedure Inst_nop;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
i:=0;
if (Cmd='nop') then i:=$4e71;
if (Cmd='rtr') then i:=$4e77;
if (Cmd='rts') then i:=$4e75;
if (Cmd='trapv') then i:=$4e76;
if (i=0) then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_pack='|pack|unpk|';
Procedure Inst_pack;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if not (p3.Typ in [2,4]) then Exit;
if (p4.Typ<>0) then Exit;
if (p1.Typ<>p2.Typ) then exit;
p:=AnalizeSizePostfix1(Cmd,1);
i:=0;
if (Cmd='pack') then i:=$8140;
if (Cmd='unpk') then i:=$8180;
if (i=0) then exit;
a:=ConvertNumOrLabDatas(p3,2,2,'');
if (a='') then exit;
i:=i or (sizeTable1[p] shl 6);
if (p1.Typ=1) then begin;
  if (p1.Reg.Num>8) then exit;
  if (p2.Reg.Num>8) then exit;
  i:=i or (p1.Reg.Num-1);
  i:=i or ((p2.Reg.Num-1) shl 9);
  OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+a;
  OutOk:=True;
  exit;
  end;
if (ConvertAllMemoryDatas(p1,p,OutStr) and $f8<>$20) then exit;
if (ConvertAllMemoryDatas(p2,p,OutStr) and $f8<>$20) then exit;
i:=i or (p1.Mem.Reg3.Num-9);
i:=i or ((p2.Mem.Reg3.Num-9) shl 9);
i:=i or $08;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+a;
OutOk:=True;
End;


Const
  Str_rtd='|rtd|';
Procedure Inst_rtd;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='rtd') then i:=$4e74;
if (i=0) then exit;
OutStr:=ConvertNumOrLabDatas(p1,2,2,'');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_trapcc1='|trapt|trapf|traphi|trapls|trapcc|trapcs|trapne|trapeq|'+
              'trapvc|trapvs|trappl|trapmi|trapge|traplt|trapgt|traple|';
Procedure Inst_trapcc1;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
i:=AnalizeCpuCond(copy(Cmd,5,255));
if (i<0) then exit;
i:=(i shl 8) or $50fc;
OutStr:=b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_trapcc2='|trapt.w|trapf.w|traphi.w|trapls.w|trapcc.w|trapcs.w|trapne.w|'+
              'trapeq.w|trapvc.w|trapvs.w|trappl.w|trapmi.w|trapge.w|'+
              'traplt.w|trapgt.w|traple.w|';
Procedure Inst_trapcc2;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=AnalizeCpuCond(copy(Cmd,5,255));
if (i<0) then exit;
i:=(i shl 8) or $50fa;
OutStr:=ConvertNumOrLabDatas(p1,2,2,'');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_trapcc3='|trapt.l|trapf.l|traphi.l|trapls.l|trapcc.l|trapcs.l|trapne.l|'+
              'trapeq.l|trapvc.l|trapvs.l|trappl.l|trapmi.l|trapge.l|'+
              'traplt.l|trapgt.l|traple.l|';
Procedure Inst_trapcc3;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=AnalizeCpuCond(copy(Cmd,5,255));
if (i<0) then exit;
i:=(i shl 8) or $50fb;
OutStr:=ConvertNumOrLabDatas(p1,4,4,'');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_dbcc='|dbt|dbf|dbhi|dbls|dbcc|dbcs|dbne|dbeq|'+
           'dbvc|dbvs|dbpl|dbmi|dbge|dblt|dbgt|dble|';
Procedure Inst_dbcc;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [2,4]) then Exit;
if (p3.Typ<>0) then Exit;
i:=AnalizeCpuCond(copy(Cmd,3,255));
if (i<0) then exit;
i:=(i shl 8) or $50c8;
o:=p1.Reg.Num-1;
if (o<0) or (o>7) then exit;
i:=i or o;
p:=2;
case p2.Typ of
  2:OutStr:='numofs'+SpxCh+p2.Num;
  4:OutStr:='ofs?'+SpxCh+p2.Lab;
  else exit;
  end;
OutStr:='rela2beg'+SpxCh+'add'+SpxCh+AnalizeNumbers('-2')+SpxCh+DataSizeNames[p]+SpxCh+'msb'+SpxCh+OutStr;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_bra='|bra|bra.b|bra.w|bra.l|bsr|bsr.b|bsr.w|bsr.l|';
Procedure Inst_bra;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,2);
i:=0;
if (Cmd='bra') then i:=$6000;
if (Cmd='bsr') then i:=$6100;
if (i=0) then exit;
case p1.Typ of
  2:OutStr:='numofs'+SpxCh+p1.Num;
  4:OutStr:='ofs?'+SpxCh+p1.Lab;
  else exit;
  end;
OutStr:='rela2beg'+SpxCh+'add'+SpxCh+AnalizeNumbers('-2')+SpxCh+DataSizeNames[p]+SpxCh+'msb'+SpxCh+OutStr;
if (p<4) then OutStr:='signed'+SpxCh+OutStr;
if (p=1) then begin;
  OutStr:=b2h(i shr 8)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
if (p=4) then i:=i or $ff;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_bcc1='|bhi|bls|bcc|bcs|bne|beq|bvc|bvs|bpl|bmi|bge|blt|bgt|ble|'+
           'bhi.b|bls.b|bcc.b|bcs.b|bne.b|beq.b|bvc.b|bvs.b|bpl.b|bmi.b|'+
           'bge.b|blt.b|bgt.b|ble.b|';
Procedure Inst_bcc1;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,1);
i:=AnalizeCpuCond(copy(Cmd,2,255));
if (i<0) then exit;
case p1.Typ of
  2:OutStr:='numofs'+SpxCh+p1.Num;
  4:OutStr:='ofs?'+SpxCh+p1.Lab;
  else exit;
  end;
OutStr:='rela2beg'+SpxCh+'add'+SpxCh+AnalizeNumbers('-2')+SpxCh+DataSizeNames[p]+SpxCh+'msb'+SpxCh+OutStr;
OutStr:=b2h(i or $60)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_bcc2='|bhi.w|bls.w|bcc.w|bcs.w|bne.w|beq.w|'+
           'bvc.w|bvs.w|bpl.w|bmi.w|bge.w|blt.w|bgt.w|ble.w|bhi.l|bls.l|'+
           'bcc.l|bcs.l|bne.l|beq.l|bvc.l|bvs.l|bpl.l|bmi.l|bge.l|blt.l|'+
           'bgt.l|ble.l|';
Procedure Inst_bcc2;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>0) then Exit;
p:=AnalizeSizePostfix1(Cmd,1);
i:=AnalizeCpuCond(copy(Cmd,2,255));
if (i<0) then exit;
i:=(i shl 8) or $6000;
case p1.Typ of
  2:OutStr:='numofs'+SpxCh+p1.Num;
  4:OutStr:='ofs?'+SpxCh+p1.Lab;
  else exit;
  end;
OutStr:='rela2beg'+SpxCh+'add'+SpxCh+AnalizeNumbers('-2')+SpxCh+DataSizeNames[p]+SpxCh+'msb'+SpxCh+OutStr;
if (p=4) then i:=i or $ff;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;





Var
  a,b:String;
  i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
OutStr:='';
b:=InStr;
b:=xLevesz(b);
if (Copy(b,Length(b),1)=',') then Exit;
if (b='') then Exit;
i:=Pos(' ',b+' ');
Cmd:=Copy(b,1,i-1);
Delete(b,1,i);b:=xLevesz(b);
Kicserel('{',',',b);
Kicserel(':',',',b);
Kicserel('}','',b);
nextParam(a,b);
AnalizeParameter(a,1,p1);
nextParam(a,b);
AnalizeParameter(a,1,p2);
nextParam(a,b);
AnalizeParameter(a,1,p3);
nextParam(a,b);
AnalizeParameter(a,1,p4);
nextParam(a,b);
AnalizeParameter(a,1,p5);
AnalizeParameter(b,1,p6);
OutStr:='er';
if (Cmd='') then Exit;

if BenneVan(Str_andiccr,Cmd) then Inst_andiccr;
if BenneVan(Str_moveccr,Cmd) then Inst_moveccr;
if BenneVan(Str_abcd,Cmd) then Inst_abcd;
if BenneVan(Str_add,Cmd) then Inst_add;
if BenneVan(Str_adda,Cmd) then Inst_adda;
if BenneVan(Str_addi,Cmd) then Inst_addi;
if BenneVan(Str_addq,Cmd) then Inst_addq;
if BenneVan(Str_asl,Cmd) then Inst_asl;
if BenneVan(Str_bchg,Cmd) then Inst_bchg;
if BenneVan(Str_bfchg,Cmd) then Inst_bfchg;
if BenneVan(Str_bfexts,Cmd) then Inst_bfexts;
if BenneVan(Str_bfins,Cmd) then Inst_bfins;
if BenneVan(Str_bkpt,Cmd) then Inst_bkpt;
if BenneVan(Str_cas,Cmd) then Inst_cas;
if BenneVan(Str_cas2,Cmd) then Inst_cas2;
if BenneVan(Str_chk,Cmd) then Inst_chk;
if BenneVan(Str_chk2,Cmd) then Inst_chk2;
if BenneVan(Str_clr,Cmd) then Inst_clr;
if BenneVan(Str_cmp,Cmd) then Inst_cmp;
if BenneVan(Str_cmpm,Cmd) then Inst_cmpm;
if BenneVan(Str_divs,Cmd) then Inst_divs;
if BenneVan(Str_eor,Cmd) then Inst_eor;
if BenneVan(Str_exg,Cmd) then Inst_exg;
if BenneVan(Str_ext,Cmd) then Inst_ext;
if BenneVan(Str_illegal,Cmd) then Inst_illegal;
if BenneVan(Str_lea,Cmd) then Inst_lea;
if BenneVan(Str_link,Cmd) then Inst_link;
if BenneVan(Str_move,Cmd) then Inst_move;
if BenneVan(Str_movea,Cmd) then Inst_movea;
if BenneVan(Str_move16,Cmd) then Inst_move16;
if BenneVan(Str_movem,Cmd) then Inst_movem;
if BenneVan(Str_movep,Cmd) then Inst_movep;
if BenneVan(Str_moveq,Cmd) then Inst_moveq;
if BenneVan(Str_nop,Cmd) then Inst_nop;
if BenneVan(Str_pack,Cmd) then Inst_pack;
if BenneVan(Str_rtd,Cmd) then Inst_rtd;
if BenneVan(Str_trapcc1,Cmd) then Inst_trapcc1;
if BenneVan(Str_trapcc2,Cmd) then Inst_trapcc2;
if BenneVan(Str_trapcc3,Cmd) then Inst_trapcc3;
if BenneVan(Str_dbcc,Cmd) then Inst_dbcc;
if BenneVan(Str_bra,Cmd) then Inst_bra;
if BenneVan(Str_bcc1,Cmd) then Inst_bcc1;
if BenneVan(Str_bcc2,Cmd) then Inst_bcc2;

if (OutOk=False) then OutStr:='';
End;
