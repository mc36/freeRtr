Procedure CompileOneCpuLine;
Const
  reg8list1:array[1..14] of byte=(7,8,1,2,3,0,4,5,$44,$45,$84,$85,0,0);
  reg16list1:array[1..9] of byte=(0,8,1,2,$42,$82,3,0,0);
  reg16list2:array[1..9] of word=(0,$34b,$35b,$2a,$12a,$22a,$37b,0,0);
  reg16list3:array[1..9] of word=($f5,$c5,$d5,$e5,$1e5,$2e5,0,0,0);
Var
  Cmd:String;
  p1:OneParameterRec;
  p2:OneParameterRec;
  p3:OneParameterRec;


Procedure ConvFirstParamForCond;
Begin;
if (p1.Typ=1) then begin;
  if (p1.Reg.Len<>1) then exit;
  if (p1.Reg.Num<>3) then exit;
  p1.Typ:=4;
  p1.Lab:='c';
  end;
End;


Const
  Str_ldreg8reg='|ld|';
Procedure Inst_ldreg8reg;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>1) then exit;
if (p2.Reg.Len<>1) then exit;
o:=reg8list1[p1.Reg.Num];
p:=reg8list1[p2.Reg.Num];
if (o=0) then exit;
if (p=0) then exit;
i:=$40+(o and 7)*8+(p and 7);
OutStr:=b2h(i and $ff);
case (o or p) shr 4 of
  0:;
  4:OutStr:=b2h($dd)+SepCh+OutStr;
  8:OutStr:=b2h($fd)+SepCh+OutStr;
  else exit;
  end;
OutOk:=True;
End;


Const
  Str_ldreg8num='|ld|';
Procedure Inst_ldreg8num;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [2,4]) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>1) then exit;
o:=reg8list1[p1.Reg.Num];
if (o=0) then exit;
i:=$06+(o and 7)*8;
OutStr:=b2h(i and $ff)+SepCh+ConvertNumOrLabDatas(p2,1,'');
case o shr 4 of
  0:;
  4:OutStr:=b2h($dd)+SepCh+OutStr;
  8:OutStr:=b2h($fd)+SepCh+OutStr;
  else exit;
  end;
OutOk:=True;
End;


Const
  Str_ldreg8mem='|ld|';
Procedure Inst_ldreg8mem;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>3) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>1) then exit;
o:=reg8list1[p1.Reg.Num];
if (o=0) then exit;
if (o shr 4<>0) then exit;
i:=$46+(o and 7)*8;
OutStr:=b2h(i and $ff);
case p2.Mem.Reg of
  4:if (p2.Mem.Typ<>1) then exit;
  5:OutStr:=b2h($dd)+SepCh+OutStr+SepCh+ConvertAllMemoryDatas(p2,1);
  6:OutStr:=b2h($fd)+SepCh+OutStr+SepCh+ConvertAllMemoryDatas(p2,1);
  else exit;
  end;
OutOk:=True;
End;


Const
  Str_ldmem8reg='|ld|';
Procedure Inst_ldmem8reg;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
if (p2.Reg.Len<>1) then exit;
o:=reg8list1[p2.Reg.Num];
if (o=0) then exit;
if (o shr 4<>0) then exit;
i:=$70+(o and 7);
OutStr:=b2h(i and $ff);
case p1.Mem.Reg of
  4:if (p1.Mem.Typ<>1) then exit;
  5:OutStr:=b2h($dd)+SepCh+OutStr+SepCh+ConvertAllMemoryDatas(p1,1);
  6:OutStr:=b2h($fd)+SepCh+OutStr+SepCh+ConvertAllMemoryDatas(p1,1);
  else exit;
  end;
OutOk:=True;
End;


Const
  Str_ldmem8num='|ld|';
Procedure Inst_ldmem8num;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>2) then Exit;
if (p3.Typ<>0) then Exit;
OutStr:=b2h($36);
case p1.Mem.Reg of
  4:if (p1.Mem.Typ<>1) then exit;
  5:OutStr:=b2h($dd)+SepCh+OutStr+SepCh+ConvertAllMemoryDatas(p1,1);
  6:OutStr:=b2h($fd)+SepCh+OutStr+SepCh+ConvertAllMemoryDatas(p1,1);
  else exit;
  end;
OutStr:=OutStr+SepCh+ConvertNumOrLabDatas(p2,1,'');
OutOk:=True;
End;


Const
  Str_lda8mem='|ld|';
Procedure Inst_lda8mem;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>3) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>1) then exit;
if (p1.Reg.Num<>1) then exit;
OutStr:='';
i:=0;
if (p2.Mem.Typ=1) then begin;
  case p2.Mem.Reg of
    2:i:=$0a;
    3:i:=$1a;
    else exit;
    end;
  end else begin;
  if (p2.Mem.Reg<>0) then exit;
  OutStr:=SepCh+ConvertAllMemoryDatas(p2,2);
  i:=$3a;
  end;
OutStr:=b2h(i)+OutStr;
OutOk:=True;
End;


Const
  Str_ldmem8a='|ld|';
Procedure Inst_ldmem8a;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
if (p2.Reg.Len<>1) then exit;
if (p2.Reg.Num<>1) then exit;
OutStr:='';
i:=0;
if (p1.Mem.Typ=1) then begin;
  case p1.Mem.Reg of
    2:i:=$02;
    3:i:=$12;
    else exit;
    end;
  end else begin;
  if (p1.Mem.Reg<>0) then exit;
  OutStr:=SepCh+ConvertAllMemoryDatas(p1,2);
  i:=$32;
  end;
OutStr:=b2h(i)+OutStr;
OutOk:=True;
End;


Const
  Str_ldaira='|ld|';
Procedure Inst_ldaira;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>1) then exit;
if (p2.Reg.Len<>1) then exit;
if (p1.Reg.Num=1) then case p2.Reg.Num of
  13:i:=$57;
  14:i:=$5f;
  end;
if (p2.Reg.Num=1) then case p1.Reg.Num of
  13:i:=$47;
  14:i:=$4f;
  end;
if (i=0) then exit;
OutStr:=b2h($ed)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_ldreg16num='|ld|';
Procedure Inst_ldreg16num;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [2,4]) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>2) then exit;
o:=reg16list1[p1.Reg.Num];
if (o=0) then exit;
i:=$01+(o and 7)*16;
OutStr:=b2h(i and $ff)+SepCh+ConvertNumOrLabDatas(p2,2,'');
case o shr 4 of
  0:;
  4:OutStr:=b2h($dd)+SepCh+OutStr;
  8:OutStr:=b2h($fd)+SepCh+OutStr;
  else exit;
  end;
OutOk:=True;
End;


Const
  Str_ldreg16mem='|ld|';
Procedure Inst_ldreg16mem;
Var i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>3) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>2) then exit;
i:=reg16list2[p1.Reg.Num];
if (i=0) then exit;
OutStr:=b2h(i and $ff)+SepCh+ConvertAllMemoryDatas(p2,2);
case i shr 8 of
  0:;
  1:OutStr:=b2h($dd)+SepCh+OutStr;
  2:OutStr:=b2h($fd)+SepCh+OutStr;
  3:OutStr:=b2h($ed)+SepCh+OutStr;
  else exit;
  end;
OutOk:=True;
End;


Const
  Str_ldmem16reg='|ld|';
Procedure Inst_ldmem16reg;
Var i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
if (p2.Reg.Len<>2) then exit;
i:=reg16list2[p2.Reg.Num];
if (i=0) then exit;
OutStr:=b2h((i and $ff)-8)+SepCh+ConvertAllMemoryDatas(p1,2);
case i shr 8 of
  0:;
  1:OutStr:=b2h($dd)+SepCh+OutStr;
  2:OutStr:=b2h($fd)+SepCh+OutStr;
  3:OutStr:=b2h($ed)+SepCh+OutStr;
  else exit;
  end;
OutOk:=True;
End;


Const
  Str_ldsphlixiy='|ld|';
Procedure Inst_ldsphlixiy;
Var i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>2) then exit;
if (p2.Reg.Len<>2) then exit;
if (p1.Reg.Num<>7) then exit;
case p2.Reg.Num of
  4:i:=0;
  5:i:=$dd;
  6:i:=$fd;
  else exit;
  end;
OutStr:=b2h($f9);
if (i<>0) then OutStr:=b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_push='|push|';
Procedure Inst_push;
Var i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>2) then exit;
i:=reg16list3[p1.Reg.Num];
if (i=0) then exit;
OutStr:=b2h(i and $ff);
case i shr 8 of
  0:;
  1:OutStr:=b2h($dd)+SepCh+OutStr;
  2:OutStr:=b2h($fd)+SepCh+OutStr;
  else exit;
  end;
OutOk:=True;
End;


Const
  Str_pop='|pop|';
Procedure Inst_pop;
Var i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>2) then exit;
i:=reg16list3[p1.Reg.Num];
if (i=0) then exit;
OutStr:=b2h((i and $ff)-4);
case i shr 8 of
  0:;
  1:OutStr:=b2h($dd)+SepCh+OutStr;
  2:OutStr:=b2h($fd)+SepCh+OutStr;
  else exit;
  end;
OutOk:=True;
End;


Const
  Str_ex='|ex|';
Procedure Inst_ex;
Var i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
if (p2.Reg.Len<>2) then exit;
OutStr:='';
case p1.Typ of
  1:begin;
    if (p1.Reg.Len<>2) then exit;
    if (p1.Reg.Num=3) and (p2.Reg.Num=4) then OutStr:=b2h($eb);
    if (p1.Reg.Num=1) and (p2.Reg.Num=1) then OutStr:=b2h($08);
    end;
  3:begin;
    if (p1.Mem.Typ<>1) then exit;
    if (p1.Mem.Reg<>7) then exit;
    case p2.Reg.Num of
      4:i:=0;
      5:i:=$dd;
      6:i:=$fd;
      else exit;
      end;
    OutStr:=b2h($e3);
    if (i<>0) then OutStr:=b2h(i)+SepCh+OutStr;
    end;
  end;
if (OutStr='') then exit;
OutOk:=True;
End;


Const
  Str_nop='|exx|ldi|ldd|cpi|cpd|ldir|lddr|cpir|cpdr|daa|cpl|neg|'+
          'rlca|rla|rrca|rra|rld|rrd|ccf|scf|nop|halt|di|ei|'+
          'ret|reti|retn|ini|ind|inir|indr|outi|outd|otir|otdr|';
Procedure Inst_nop;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
OutStr:='';
if (Cmd='exx') then OutStr:=b2h($d9);
if (Cmd='ldi') then OutStr:=b2h($ed)+SepCh+b2h($a0);
if (Cmd='ldd') then OutStr:=b2h($ed)+SepCh+b2h($a8);
if (Cmd='cpi') then OutStr:=b2h($ed)+SepCh+b2h($a1);
if (Cmd='cpd') then OutStr:=b2h($ed)+SepCh+b2h($a9);
if (Cmd='ldir') then OutStr:=b2h($ed)+SepCh+b2h($b0);
if (Cmd='lddr') then OutStr:=b2h($ed)+SepCh+b2h($b8);
if (Cmd='cpir') then OutStr:=b2h($ed)+SepCh+b2h($b1);
if (Cmd='cpdr') then OutStr:=b2h($ed)+SepCh+b2h($b9);
if (Cmd='daa') then OutStr:=b2h($27);
if (Cmd='cpl') then OutStr:=b2h($2f);
if (Cmd='neg') then OutStr:=b2h($ed)+SepCh+b2h($44);
if (Cmd='rlca') then OutStr:=b2h($07);
if (Cmd='rla') then OutStr:=b2h($17);
if (Cmd='rrca') then OutStr:=b2h($0f);
if (Cmd='rra') then OutStr:=b2h($1f);
if (Cmd='rld') then OutStr:=b2h($ed)+SepCh+b2h($6f);
if (Cmd='rrd') then OutStr:=b2h($ed)+SepCh+b2h($67);
if (Cmd='ccf') then OutStr:=b2h($3f);
if (Cmd='scf') then OutStr:=b2h($37);
if (Cmd='nop') then OutStr:=b2h($00);
if (Cmd='halt') then OutStr:=b2h($76);
if (Cmd='di') then OutStr:=b2h($f3);
if (Cmd='ei') then OutStr:=b2h($fb);
if (Cmd='ret') then OutStr:=b2h($c9);
if (Cmd='reti') then OutStr:=b2h($ed)+SepCh+b2h($4d);
if (Cmd='retn') then OutStr:=b2h($ed)+SepCh+b2h($45);
if (Cmd='ini') then OutStr:=b2h($ed)+SepCh+b2h($a2);
if (Cmd='ind') then OutStr:=b2h($ed)+SepCh+b2h($aa);
if (Cmd='inir') then OutStr:=b2h($ed)+SepCh+b2h($b2);
if (Cmd='indr') then OutStr:=b2h($ed)+SepCh+b2h($ba);
if (Cmd='outi') then OutStr:=b2h($ed)+SepCh+b2h($a3);
if (Cmd='outd') then OutStr:=b2h($ed)+SepCh+b2h($ab);
if (Cmd='otir') then OutStr:=b2h($ed)+SepCh+b2h($b3);
if (Cmd='otdr') then OutStr:=b2h($ed)+SepCh+b2h($bb);

if (OutStr='') then exit;
OutOk:=True;
End;


Const
  Str_addsub8='|add|sub|adc|sbc|and|or|xor|cp|';
Procedure Inst_addsub8;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>1) then exit;
if (p1.Reg.Num<>1) then Exit;
i:=0;
if (Cmd='add') then i:=$80;
if (Cmd='adc') then i:=$88;
if (Cmd='sub') then i:=$90;
if (Cmd='sbc') then i:=$98;
if (Cmd='and') then i:=$a0;
if (Cmd='xor') then i:=$a8;
if (Cmd='or') then i:=$b0;
if (Cmd='cp') then i:=$b8;
if (i=0) then exit;
if (p2.Typ=1) then begin;
  if (p2.Reg.Len<>1) then exit;
  o:=reg8list1[p2.Reg.Num];
  if (o=0) then exit;
  OutStr:=b2h(i or (o and 7));
  case o shr 4 of
    0:;
    4:OutStr:=b2h($dd)+SepCh+OutStr;
    8:OutStr:=b2h($fd)+SepCh+OutStr;
    else exit;
    end;
  OutOk:=True;
  exit;
  end;
if (p2.Typ in [2,4]) then begin;
  OutStr:=b2h(i+$46)+SepCh+ConvertNumOrLabDatas(p2,1,'');
  OutOk:=True;
  exit;
  end;
if (p2.Typ=3) then begin;
  inc(i,6);
  case p2.Mem.Reg of
    4:begin;
      if (p2.Mem.Typ<>1) then exit;
      OutStr:=b2h(i);
      end;
    5:OutStr:=b2h($dd)+SepCh+b2h(i)+SepCh+ConvertAllMemoryDatas(p2,1);
    6:OutStr:=b2h($fd)+SepCh+b2h(i)+SepCh+ConvertAllMemoryDatas(p2,1);
    else exit;
    end;
  OutOk:=True;
  exit;
  end;
End;

Const
  Str_incdec8='|inc|dec|';
Procedure Inst_incdec8;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='inc') then i:=$04;
if (Cmd='dec') then i:=$05;
if (i=0) then exit;
if (p1.Typ=1) then begin;
  if (p1.Reg.Len<>1) then exit;
  o:=reg8list1[p1.Reg.Num];
  if (o=0) then exit;
  OutStr:=b2h(((o and 7)*8) or i);
  case o shr 4 of
    0:;
    4:OutStr:=b2h($dd)+SepCh+OutStr;
    8:OutStr:=b2h($fd)+SepCh+OutStr;
    else exit;
    end;
  OutOk:=True;
  exit;
  end;
if (p1.Typ=3) then begin;
  inc(i,$30);
  case p1.Mem.Reg of
    4:begin;
      if (p1.Mem.Typ<>1) then exit;
      OutStr:=b2h(i);
      end;
    5:OutStr:=b2h($dd)+SepCh+b2h(i)+SepCh+ConvertAllMemoryDatas(p1,1);
    6:OutStr:=b2h($fd)+SepCh+b2h(i)+SepCh+ConvertAllMemoryDatas(p1,1);
    else exit;
    end;
  OutOk:=True;
  exit;
  end;
End;


Const
  Str_addreg16reg='|add|';
Procedure Inst_addreg16reg;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>2) then exit;
if (p2.Reg.Len<>2) then exit;
o:=reg16list1[p1.Reg.Num];
p:=reg16list1[p2.Reg.Num];
if (o=0) then exit;
if (p=0) then exit;
if (p and 7=2) then if (o<>p) then exit;
OutStr:=b2h((p and 7)*$10+9);
case o shr 4 of
  0:;
  4:OutStr:=b2h($dd)+SepCh+OutStr;
  8:OutStr:=b2h($fd)+SepCh+OutStr;
  else exit;
  end;
OutOk:=True;
End;


Const
  Str_adcsbc16='|adc|sbc|';
Procedure Inst_adcsbc16;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>2) then exit;
if (p2.Reg.Len<>2) then exit;
if (p1.Reg.Num<>4) then exit;
i:=0;
if (Cmd='adc') then i:=$4a;
if (Cmd='sbc') then i:=$42;
if (i=0) then exit;
p:=reg16list1[p2.Reg.Num];
if (p=0) then exit;
if (p and $f0<>0) then exit;
OutStr:=b2h($ed)+SepCh+b2h((p and 7)*$10+i);
OutOk:=True;
End;


Const
  Str_incdec16='|inc|dec|';
Procedure Inst_incdec16;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>2) then exit;
i:=0;
if (Cmd='inc') then i:=$03;
if (Cmd='dec') then i:=$0b;
if (i=0) then exit;
o:=reg16list1[p1.Reg.Num];
if (o=0) then exit;
OutStr:=b2h((o and 7)*$10+i);
case o shr 4 of
  0:;
  4:OutStr:=b2h($dd)+SepCh+OutStr;
  8:OutStr:=b2h($fd)+SepCh+OutStr;
  else exit;
  end;
OutOk:=True;
End;


Const
  Str_rsrlcla='|rlc|rl|rrc|rr|sla|sll|sra|srl|';
Procedure Inst_rsrlcla;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='rlc') then i:=$00;
if (Cmd='rl') then i:=$10;
if (Cmd='rrc') then i:=$08;
if (Cmd='rr') then i:=$18;
if (Cmd='sla') then i:=$20;
if (Cmd='sll') then i:=$30;
if (Cmd='sra') then i:=$28;
if (Cmd='srl') then i:=$38;
if (i<0) then exit;
if (p1.Typ=1) then begin;
  if (p1.Reg.Len<>1) then exit;
  o:=reg8list1[p1.Reg.Num];
  if (o=0) then exit;
  if (o shr 4<>0) then exit;
  OutStr:=b2h($cb)+SepCh+b2h((o and 7)+i);
  OutOk:=True;
  exit;
  end;
if (p1.Typ=3) then begin;
  inc(i,6);
  case p1.Mem.Reg of
    4:begin;
      if (p1.Mem.Typ<>1) then exit;
      OutStr:=b2h($cb)+SepCh+b2h(i);
      end;
    5:OutStr:=b2h($dd)+SepCh+b2h($cb)+SepCh+ConvertAllMemoryDatas(p1,1)+SepCh+b2h(i);
    6:OutStr:=b2h($fd)+SepCh+b2h($cb)+SepCh+ConvertAllMemoryDatas(p1,1)+SepCh+b2h(i);
    else exit;
    end;
  OutOk:=True;
  exit;
  end;
End;


Const
  Str_bitsetres='|bit|set|res|';
Procedure Inst_bitsetres;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='bit') then i:=$40;
if (Cmd='set') then i:=$c0;
if (Cmd='res') then i:=$80;
if (i=0) then exit;
o:=BVal('$'+p1.Num);
if (o<0) or (o>7) then exit;
inc(i,o*8);
if (p2.Typ=1) then begin;
  if (p2.Reg.Len<>1) then exit;
  o:=reg8list1[p2.Reg.Num];
  if (o=0) then exit;
  if (o shr 4<>0) then exit;
  OutStr:=b2h($cb)+SepCh+b2h((o and 7)+i);
  OutOk:=True;
  exit;
  end;
if (p2.Typ=3) then begin;
  inc(i,6);
  case p2.Mem.Reg of
    4:begin;
      if (p2.Mem.Typ<>1) then exit;
      OutStr:=b2h($cb)+SepCh+b2h(i);
      end;
    5:OutStr:=b2h($dd)+SepCh+b2h($cb)+SepCh+ConvertAllMemoryDatas(p2,1)+SepCh+b2h(i);
    6:OutStr:=b2h($fd)+SepCh+b2h($cb)+SepCh+ConvertAllMemoryDatas(p2,1)+SepCh+b2h(i);
    else exit;
    end;
  OutOk:=True;
  exit;
  end;
End;


Const
  Str_imrst='|im|rst|';
Procedure Inst_imrst;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
o:=BVal('$'+p1.Num);
if (o<0) then exit;
if (Cmd='im') then begin;
  case o of
    0:i:=$46;
    1:i:=$56;
    2:i:=$5e;
    end;
  if (o>2) then exit;
  OutStr:=b2h($ed)+sepch+b2h(i);
  OutOk:=True;
  Exit;
  end;
if (Cmd='rst') then begin;
  if (o>7) then exit;
  OutStr:=b2h($c7+o*8);
  OutOk:=True;
  Exit;
  end;
End;


Const
  Str_jpnum='|jp|call|';
Procedure Inst_jpnum;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='jp') then i:=$c3;
if (Cmd='call') then i:=$cd;
if (i=0) then exit;
OutStr:=b2h(i)+SepCh+ConvertNumOrLabDatas(p1,2,'ofs');
OutOk:=True;
End;


Const
  Str_jpreg='|jp|';
Procedure Inst_jpreg;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Len<>2) then exit;
case p1.Reg.Num of
  4:OutStr:='';
  5:OutStr:=b2h($dd)+SepCh;
  6:OutStr:=b2h($fd)+SepCh;
  else exit;
  end;
OutStr:=OutStr+b2h($e9);
OutOk:=True;
End;


Const
  Str_jpccnum='|jp|call|';
Procedure Inst_jpccnum;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
ConvFirstParamForCond;
if (p1.Typ<>4) then Exit;
if not (p2.Typ in [2,4]) then Exit;
if (p3.Typ<>0) then Exit;
o:=AnalizeCondition(p1.Lab);
if (o<1) then exit;
i:=0;
if (Cmd='jp') then i:=$ba;
if (Cmd='call') then i:=$bc;
if (i=0) then exit;
OutStr:=b2h(o*8+i)+SepCh+ConvertNumOrLabDatas(p2,2,'ofs');
OutOk:=True;
End;


Const
  Str_jrnum='|jr|djnz|';
Procedure Inst_jrnum;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='jr') then i:=$18;
if (Cmd='djnz') then i:=$10;
if (i=0) then exit;
OutStr:='';
case p1.Typ of
  2:OutStr:='numofs'+SpxCh+p1.Num;
  4:OutStr:='ofs?'+SpxCh+p1.Lab;
  end;
if (OutStr='') then exit;
OutStr:=b2h(i)+SepCh+'byte'+SpxCh+'rela2end'+SpxCh+'signed'+SpxCh+OutStr;
OutOk:=True;
End;


Const
  Str_jrccnum='|jr|';
Procedure Inst_jrccnum;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
ConvFirstParamForCond;
if (p1.Typ<>4) then Exit;
if (p3.Typ<>0) then Exit;
i:=AnalizeCondition(p1.Lab);
if (i<1) or (i>4) then exit;
OutStr:='';
case p2.Typ of
  2:OutStr:='numofs'+SpxCh+p2.Num;
  4:OutStr:='ofs?'+SpxCh+p2.Lab;
  end;
if (OutStr='') then exit;
OutStr:=b2h($18+i*8)+SepCh+'byte'+SpxCh+'rela2end'+SpxCh+'signed'+SpxCh+OutStr;
OutOk:=True;
End;


Const
  Str_retcc='|ret|';
Procedure Inst_retcc;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
ConvFirstParamForCond;
if (p1.Typ<>4) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=AnalizeCondition(p1.Lab);
if (i<1) then exit;
OutStr:=b2h($b8+i*8);
OutOk:=True;
End;


Const
  Str_inout='|in|out|';
Procedure Inst_inout;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p3.Typ<>0) then Exit;
p:=-1;
if (Cmd='in') then p:=0;
if (Cmd='out') then begin; p:=1;p3:=p1;p1:=p2;p2:=p3; end;
if (p<0) then exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>3) then Exit;
if (p1.Reg.Len<>1) then exit;
if (p2.Mem.Typ in [2,4]) then begin;
  if (p1.Reg.Num<>1) then exit;
  if (p2.Mem.Reg<>0) then exit;
  OutStr:=b2h($db+8*-p)+SepCh+ConvertAllMemoryDatas(p2,1);
  OutOk:=True;
  exit;
  end;
if (p2.Mem.Typ<>1) then exit;
o:=reg8list1[p1.Reg.Num];
if (o=0) then exit;
i:=$40+(o and 7)*8+p;
OutStr:=b2h($ed)+SepCh+b2h(i and $ff);
case o shr 4 of
  0:;
  4:OutStr:=b2h($dd)+SepCh+OutStr;
  8:OutStr:=b2h($fd)+SepCh+OutStr;
  else exit;
  end;
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
i:=Pos(' ',b+' ');
b:=xLevesz(b);
i:=Pos(',',b+',');
a:=Copy(b,1,i-1);
b:=Copy(b,i+1,255);
AnalizeParameter(a,1,p1);
b:=xLevesz(b);
i:=Pos(',',b+',');
a:=Copy(b,1,i-1);
b:=Copy(b,i+1,255);
AnalizeParameter(a,1,p2);
AnalizeParameter(b,1,p3);
OutStr:='er';
if (Cmd='') then Exit;

if BenneVan(Str_ldreg8reg,Cmd) then Inst_ldreg8reg;
if BenneVan(Str_ldreg8num,Cmd) then Inst_ldreg8num;
if BenneVan(Str_ldreg8mem,Cmd) then Inst_ldreg8mem;
if BenneVan(Str_ldmem8reg,Cmd) then Inst_ldmem8reg;
if BenneVan(Str_ldmem8num,Cmd) then Inst_ldmem8num;
if BenneVan(Str_lda8mem,Cmd) then Inst_lda8mem;
if BenneVan(Str_ldmem8a,Cmd) then Inst_ldmem8a;
if BenneVan(Str_ldaira,Cmd) then Inst_ldaira;
if BenneVan(Str_ldreg16num,Cmd) then Inst_ldreg16num;
if BenneVan(Str_ldreg16mem,Cmd) then Inst_ldreg16mem;
if BenneVan(Str_ldmem16reg,Cmd) then Inst_ldmem16reg;
if BenneVan(Str_ldsphlixiy,Cmd) then Inst_ldsphlixiy;
if BenneVan(Str_push,Cmd) then Inst_push;
if BenneVan(Str_pop,Cmd) then Inst_pop;
if BenneVan(Str_ex,Cmd) then Inst_ex;
if BenneVan(Str_nop,Cmd) then Inst_nop;
if BenneVan(Str_addsub8,Cmd) then Inst_addsub8;
if BenneVan(Str_incdec8,Cmd) then Inst_incdec8;
if BenneVan(Str_addreg16reg,Cmd) then Inst_addreg16reg;
if BenneVan(Str_adcsbc16,Cmd) then Inst_adcsbc16;
if BenneVan(Str_incdec16,Cmd) then Inst_incdec16;
if BenneVan(Str_rsrlcla,Cmd) then Inst_rsrlcla;
if BenneVan(Str_bitsetres,Cmd) then Inst_bitsetres;
if BenneVan(Str_imrst,Cmd) then Inst_imrst;
if BenneVan(Str_jpnum,Cmd) then Inst_jpnum;
if BenneVan(Str_jpreg,Cmd) then Inst_jpreg;
if BenneVan(Str_jpccnum,Cmd) then Inst_jpccnum;
if BenneVan(Str_jrnum,Cmd) then Inst_jrnum;
if BenneVan(Str_jrccnum,Cmd) then Inst_jrccnum;
if BenneVan(Str_retcc,Cmd) then Inst_retcc;
if BenneVan(Str_inout,Cmd) then Inst_inout;

if (OutOk=False) then OutStr:='';
End;
