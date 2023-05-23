Procedure CompileOneCpuLine;
Var
  Cmd:String;
  p1:OneParameterRec;
  p2:OneParameterRec;
  p3:OneParameterRec;
  p4:OneParameterRec;


Procedure nextParam(var a,b:String);
Var i,o,p:LongInt;
Begin;
b:=xLevesz(b);
i:=Pos(',',b);
if (i<1) then i:=666;
a:=copy(b,1,i-1);
b:=copy(b,i+1,255);
End;



Const
  Str_addrddr='|add|addc|subb|anl|orl|xrl|xch|';
Procedure Inst_addrddr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Reg.Num<>9) then exit;
i:=-1;
if (Cmd='add') then i:=$24;
if (Cmd='addc') then i:=$34;
if (Cmd='subb') then i:=$94;
if (Cmd='anl') then i:=$54;
if (Cmd='orl') then i:=$44;
if (Cmd='xrl') then i:=$64;
if (Cmd='xch') then i:=$c4;
if (i<0) then exit;
if (p2.Typ=1) then begin; {r0..r7}
  o:=p2.Reg.Num;
  if (o<1) or (o>8) then exit;
  OutStr:=b2h(i+o+3);
  OutOk:=True;
  exit;
  end;
if (p2.Typ in [3,4]) then begin;
  OutStr:=b2h(i)+SepCh+ConvertNumOrLabDatas(p2,1);
  OutOk:=True;
  exit;
  end;
if (p2.Typ<>2) then exit;
if (p2.Mem.Typ=1) then begin; {@r0..@r1}
  o:=p2.Mem.Reg;
  if (o<1) or (o>2) then exit;
  OutStr:=b2h(i+o+1);
  OutOk:=True;
  exit;
  end;
if (p2.Mem.Typ=2) then begin; {mem}
  OutStr:=b2h(i+1)+SepCh+ConvertMemoryDatas(p2.Mem,1);
  OutOk:=True;
  exit;
  end;
End;



Const
  Str_anldad='|anl|orl|xrl|';
Procedure Inst_anldad;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p3.Typ<>0) then Exit;
if not (p1.Mem.Typ in [2,3]) then exit;
i:=-1;
if (Cmd='anl') then i:=$54;
if (Cmd='orl') then i:=$44;
if (Cmd='xrl') then i:=$64;
if (i<0) then exit;
OutStr:=SepCh+ConvertMemoryDatas(p1.Mem,1);
if (p2.Typ=1) and (p2.Reg.Num=9) then begin; {r0..r7}
  OutStr:=b2h(i-2)+OutStr;
  OutOk:=True;
  exit;
  end;
if (p2.Typ in [3,4]) then begin;
  OutStr:=b2h(i-1)+OutStr+SepCh+ConvertNumOrLabDatas(p2,1);
  OutOk:=True;
  exit;
  end;
End;




Const
  Str_incardd='|inc|dec||';
Procedure Inst_incardd;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='inc') then i:=$04;
if (Cmd='dec') then i:=$14;
if (i<0) then exit;
if (p1.Typ=1) then begin;
  o:=p1.Reg.Num;
  if (o<1) or (o>9) then exit;
  if (o=9) then o:=-3;
  OutStr:=b2h(i+o+3);
  OutOk:=True;
  exit;
  end;
if (p1.Typ<>2) then exit;
if (p1.Mem.Typ=1) then begin; {@r0..@r1}
  o:=p1.Mem.Reg;
  if (o<1) or (o>2) then exit;
  OutStr:=b2h(i+o+1);
  OutOk:=True;
  exit;
  end;
if (p1.Mem.Typ in [2,3]) then begin; {mem}
  OutStr:=b2h(i+1)+SepCh+ConvertMemoryDatas(p1.Mem,1);
  OutOk:=True;
  exit;
  end;
End;




Const
  Str_incdptr='|inc|mul|div|da|clr|cpl|rl|rlc|rr|rrc|swap|movc|movx|xchd|'+
              'ret|reti|jmp|nop|';
Procedure Inst_incdptr;
Var
  i,o,p:LongInt;
  a:String;

Procedure adder(var p:OneParameterRec;s:String);
Begin;
if (p.Str='') then exit;
a:=a+s+p.Str;
End;

Begin;
if (OutOk=True) then Exit;
a:=Cmd;
adder(p1,' ');
adder(p2,',');
adder(p3,',');
i:=-1;
if (a='inc dptr') then i:=$a3;
if (a='mul ab') then i:=$a4;
if (a='div ab') then i:=$84;
if (a='da a') then i:=$d4;
if (a='clr a') then i:=$e4;
if (a='cpl a') then i:=$f4;
if (a='rl a') then i:=$23;
if (a='rlc a') then i:=$33;
if (a='rr a') then i:=$03;
if (a='rrc a') then i:=$13;
if (a='swap a') then i:=$c4;
if (a='movc a,@a+dptr') then i:=$93;
if (a='movc a,@a+pc') then i:=$83;
if (a='movx a,@r0') then i:=$e2;
if (a='movx a,@r1') then i:=$e3;
if (a='movx a,@dptr') then i:=$e0;
if (a='movx @r0,a') then i:=$f2;
if (a='movx @r1,a') then i:=$f3;
if (a='movx @dptr,a') then i:=$f0;
if (a='xchd a,@r0') then i:=$d6;
if (a='xchd a,@r1') then i:=$d7;
if (a='ret') then i:=$22;
if (a='reti') then i:=$32;
if (a='jmp @a+dptr') then i:=$73;
if (a='nop') then i:=$00;
if (i<0) then exit;
OutStr:=b2h(i);
OutOk:=True;
End;



Const
  Str_push='|push|pop|';
Procedure Inst_push;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if not (p1.Mem.Typ in [2,3]) then exit;
i:=-1;
if (Cmd='push') then i:=$c0;
if (Cmd='pop') then i:=$d0;
if (i<0) then exit;
OutStr:=b2h(i)+SepCh+ConvertMemoryDatas(p1.Mem,1);
OutOk:=True;
End;




Const
  Str_mov='|mov|';
Procedure Inst_mov;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p3.Typ<>0) then Exit;

if (p1.Typ=1) and (p1.Reg.Num=9) then begin; {A,XXX}
  if (p2.Typ=1) then begin; {r0..r7}
    o:=p2.Reg.Num;
    if (o<1) or (o>8) then exit;
    OutStr:=b2h($e7+o);
    OutOk:=True;
    exit;
    end;
  if (p2.Typ in [3,4]) then begin;
    OutStr:=b2h($74)+SepCh+ConvertNumOrLabDatas(p2,1);
    OutOk:=True;
    exit;
    end;
  if (p2.Typ<>2) then exit;
  if (p2.Mem.Typ=1) then begin; {@r0..@r1}
    o:=p2.Mem.Reg;
    if (o<1) or (o>2) then exit;
    OutStr:=b2h($e5+o);
    OutOk:=True;
    exit;
    end;
  if (p2.Mem.Typ in [2,3]) then begin; {mem}
    OutStr:=b2h($e5)+SepCh+ConvertMemoryDatas(p2.Mem,1);
    OutOk:=True;
    exit;
    end;
  exit;
  end;

if (p1.Typ=1) and (p1.Reg.Num in [1..8]) then begin; {Rn,XXX}
  o:=p1.Reg.Num-1;
  if (p2.Typ=1) then begin; {a}
    if (p2.Reg.Num<>9) then exit;
    OutStr:=b2h($f8+o);
    OutOk:=True;
    exit;
    end;
  if (p2.Typ in [3,4]) then begin;
    OutStr:=b2h($78+o)+SepCh+ConvertNumOrLabDatas(p2,1);
    OutOk:=True;
    exit;
    end;
  if (p2.Typ=2) and (p2.Mem.Typ in [2,3]) then begin; {mem}
    OutStr:=b2h($a8+o)+SepCh+ConvertMemoryDatas(p2.Mem,1);
    OutOk:=True;
    exit;
    end;
  exit;
  end;

if (p1.Typ=2) and (p1.Mem.Typ in [2,3]) then begin; {ADDR,XXX}
  OutStr:=SepCh+ConvertMemoryDatas(p1.Mem,1);
  o:=p2.Reg.Num-1;
  if (p2.Typ=1) and (o=8) then begin; {A}
    OutStr:=b2h($f5)+OutStr;
    OutOk:=True;
    exit;
    end;
  if (p2.Typ=1) and (o in [0..7]) then begin; {Rn}
    OutStr:=b2h($88+o)+OutStr;
    OutOk:=True;
    exit;
    end;
  if (p2.Typ=2) and (p2.Mem.Typ in [2,3]) then begin; {ADDR}
    OutStr:=b2h($85)+SepCh+ConvertMemoryDatas(p2.Mem,1)+OutStr;
    OutOk:=True;
    exit;
    end;
  if (p2.Typ=2) and (p2.Mem.Typ=1) then begin; {@Rn}
    i:=p2.Mem.Reg;
    if (i<1) or (i>2) then exit;
    OutStr:=b2h($85+i)+OutStr;
    OutOk:=True;
    exit;
    end;
  if (p2.Typ in [3,4]) then begin; {num}
    OutStr:=b2h($75)+OutStr+SepCh+ConvertNumOrLabDatas(p2,1);
    OutOk:=True;
    exit;
    end;
  exit;
  end;

if (p1.Typ=2) and (p1.Mem.Typ=1) then begin; {@Rn,XXX}
  o:=p1.Mem.Reg-1;
  if (o<0) or (o>1) then exit;
  if (p2.Typ=1) then begin; {A}
    if (p2.Reg.Num<>9) then exit;
    OutStr:=b2h($f6+o);
    OutOk:=True;
    exit;
    end;
  if (p2.Typ=2) and (p2.Mem.Typ in [2,3]) then begin; {ADDR}
    OutStr:=b2h($a6+o)+SepCh+ConvertMemoryDatas(p2.Mem,1);
    OutOk:=True;
    exit;
    end;
  if (p2.Typ in [3,4]) then begin; {num}
    OutStr:=b2h($76+o)+SepCh+ConvertNumOrLabDatas(p2,1);
    OutOk:=True;
    exit;
    end;
  exit;
  end;

if (p1.Typ=4) and (p1.Str='dptr') then begin;
  if not (p2.Typ in [3,4]) then exit;
  OutStr:=b2h($90)+SepCh+ConvertNumOrLabDatas(p2,2);
  OutOk:=True;
  exit;
  end;

if (p1.Str='c') then begin;
  if not (p2.Typ in [3,4]) then exit;
  OutStr:=b2h($a2)+SepCh+ConvertNumOrLabDatas(p2,1);
  OutOk:=True;
  exit;
  end;

if (p2.Str='c') then begin;
  if not (p1.Typ in [3,4]) then exit;
  OutStr:=b2h($92)+SepCh+ConvertNumOrLabDatas(p1,1);
  OutOk:=True;
  exit;
  end;

End;



Const
  Str_clrcn='|clr|setb|cpl|';
Procedure Inst_clrcn;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='clr') then i:=$c3;
if (Cmd='setb') then i:=$d3;
if (Cmd='cpl') then i:=$b3;
if (i<0) then exit;
if (p1.Str='c') then begin;
  OutStr:=b2h(i);
  OutOk:=True;
  Exit;
  end;
if not (p1.Typ in [3,4]) then exit;
OutStr:=b2h(i-1)+SepCh+ConvertNumOrLabDatas(p1,1);
OutOk:=True;
End;



Const
  Str_anlcn='|anl|orl|';
Procedure Inst_anlcn;
Var
  p:OneParameterRec;
  i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Str<>'c') then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='anl') then i:=$82;
if (Cmd='orl') then i:=$72;
if (i<0) then exit;
p:=p2;
if (copy(p.Str,1,1)='/') then begin;
  AnalizeParameter(copy(p.Str,2,666),1,p);
  inc(i,$2e);
  end;
if not (p.Typ in [3,4]) then Exit;
OutStr:=b2h(i)+SepCh+ConvertNumOrLabDatas(p,1);
OutOk:=True;
End;



Const
  Str_jc='|jc|jnc|sjmp|jz|jnz|';
Procedure Inst_jc;
Var
  i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='jc') then i:=$40;
if (Cmd='jnc') then i:=$50;
if (Cmd='sjmp') then i:=$80;
if (Cmd='jz') then i:=$60;
if (Cmd='jnz') then i:=$70;
if (i<0) then exit;
OutStr:=convertRelAddr(p1,1);
if (OutStr='') then Exit;
OutStr:=b2h(i)+OutStr;
OutOk:=True;
End;



Const
  Str_jb='|jb|jnb|jbc|';
Procedure Inst_jb;
Var
  i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [3,4]) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='jb') then i:=$20;
if (Cmd='jnb') then i:=$30;
if (Cmd='jbc') then i:=$10;
if (i<0) then exit;
OutStr:=convertRelAddr(p2,1);
if (OutStr='') then Exit;
OutStr:=b2h(i)+SepCh+ConvertNumOrLabDatas(p1,1)+OutStr;
OutOk:=True;
End;



Const
  Str_acall='|acall|ajmp|';
Procedure Inst_acall;
Var
  i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='acall') then i:=$11;
if (Cmd='ajmp') then i:=$01;
if (i<0) then exit;
case p1.Typ of
  3:OutStr:='numofs'+SpxCh+p1.Num;
  4:OutStr:='ofs?'+SpxCh+p1.Lab;
  else exit;
  end;
OutStr:=DataSizeNames[1]+SpxCh+OutStr;
OutStr:='or'+SpxCh+AnalizeNumbers(BStr(i))+SpxCh+'div'+SpxCh+AnalizeNumbers('8')+SpxCh+'and'+SpxCh+AnalizeNumbers('0fffffe0h')+SpxCh+OutStr+SepCh+'and'+SpxCh+AnalizeNumbers('0ffh')+SpxCh+OutStr;
OutOk:=True;
End;



Const
  Str_lcall='|lcall|ljmp|';
Procedure Inst_lcall;
Var
  i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [3,4]) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='lcall') then i:=$12;
if (Cmd='ljmp') then i:=$02;
if (i<0) then exit;
OutStr:=b2h(i)+SepCh+ConvertNumOrLabDatas(p1,2);
OutOk:=True;
End;



Const
  Str_cjne='|cjne|';
Procedure Inst_cjne;
Var
  i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p3.Typ in [3,4]) then Exit;
OutStr:=convertRelAddr(p3,1);
if (OutStr='') then exit;
if (p1.Str='a') and (p2.Typ=2) then begin;
  if not (p2.Mem.Typ in [2,3]) then Exit;
  OutStr:=b2h($b5)+SepCh+ConvertMemoryDatas(p2.Mem,1)+OutStr;
  OutOk:=True;
  Exit;
  end;
if not (p2.Typ in [3,4]) then exit;
OutStr:=SepCh+ConvertNumOrLabDatas(p2,1)+OutStr;
if (p1.Str='a') then begin;
  OutStr:=b2h($b4)+OutStr;
  OutOk:=True;
  Exit;
  end;
if (p1.Typ=1) then begin;
  i:=p1.Reg.Num-1;
  if (i<0) or (i>7) then exit;
  OutStr:=b2h($b8+i)+OutStr;
  OutOk:=True;
  exit;
  end;
if (p1.Typ<>2) then Exit;
if (p1.Mem.Typ<>1) then Exit;
i:=p1.Mem.Reg-1;
if (i<0) or (i>1) then Exit;
OutStr:=b2h($b6+i)+OutStr;
OutOk:=True;
End;



Const
  Str_djnz='|djnz|';
Procedure Inst_djnz;
Var
  i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p2.Typ in [3,4]) then Exit;
if (p3.Typ<>0) then Exit;
OutStr:=convertRelAddr(p2,1);
if (OutStr='') then exit;
if (p1.Typ=1) then begin;
  i:=p1.Reg.Num-1;
  if (i<0) or (i>7) then exit;
  OutStr:=b2h($d8+i)+OutStr;
  OutOk:=True;
  Exit;
  end;
if (p1.Typ<>2) then Exit;
if not (p1.Mem.Typ in [2,3]) then Exit;
OutStr:=b2h($d5)+SepCh+ConvertMemoryDatas(p1.Mem,1)+OutStr;
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
AnalizeParameter(b,1,p4);
OutStr:='er';
if (Cmd='') then Exit;

if BenneVan(Str_addrddr,Cmd) then Inst_addrddr;
if BenneVan(Str_anldad,Cmd) then Inst_anldad;
if BenneVan(Str_incardd,Cmd) then Inst_incardd;
if BenneVan(Str_mov,Cmd) then Inst_mov;
if BenneVan(Str_acall,Cmd) then Inst_acall;
if BenneVan(Str_lcall,Cmd) then Inst_lcall;
if BenneVan(Str_jc,Cmd) then Inst_jc;
if BenneVan(Str_jb,Cmd) then Inst_jb;
if BenneVan(Str_push,Cmd) then Inst_push;
if BenneVan(Str_cjne,Cmd) then Inst_cjne;
if BenneVan(Str_djnz,Cmd) then Inst_djnz;
if BenneVan(Str_clrcn,Cmd) then Inst_clrcn;
if BenneVan(Str_anlcn,Cmd) then Inst_anlcn;
if BenneVan(Str_incdptr,Cmd) then Inst_incdptr;

if (OutOk=False) then OutStr:='';
End;
