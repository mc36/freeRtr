Procedure CompileOneCpuLine;

Function AnalizeBytesToTransfer(s:String):Byte;
Var
  i:Byte;
Begin;
i:=DecodeDataSizeName(s);
if not (i in [1,2,4]) then i:=0;
AnalizeBytesToTransfer:=i;
End;

Var
  Cmd:String;
  p1:OneParameterRec;
  p2:OneParameterRec;
  p3:OneParameterRec;
  Trns:Byte;


Const
  Str_retiret='|ret|retw|retd|retn|retnw|retnd|'+
  'retf|retfw|retfd|iret|iretw|iretd|';
Procedure Inst_retiret;
Label Vege;
Var
  i,o:LongInt;
  a,s:String;
Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
s:=Cmd;
i:=p1.Typ;
if BenneVan('|iret|iretw|iretd|',s) then begin;
  if (i<>0) then Exit;
  end else begin;
  if not (i in [0,2,4]) then Exit;
  end;
i:=-1;o:=1;
if (s='retn')  then s:='ret';
if (s='retnw') then s:='ret';
if (s='retnd') then s:='retd';
if (s='retfw') then s:='retf';
if (s='retw')  then s:='ret';
if (s='iretw') then s:='iret';
if (s='retf')  then begin; i:=$cb;o:=2; end;
if (s='retfd') then begin; i:=$cb;o:=4; end;
if (s='ret')   then begin; i:=$c3;o:=2; end;
if (s='retd')  then begin; i:=$c3;o:=4; end;
if (s='iret')  then begin; i:=$cf;o:=2; end;
if (s='iretd') then begin; i:=$cf;o:=4; end;
if (i=-1) then Exit;
a:='';
if (p1.Typ=0) then Goto Vege;
dec(i);
a:=SepCh+ConvertNumOrLabDatas(p1,2,'');
if (a='') then Exit;
Vege:
OutStr:=b2h(i)+a;
AddNativeOpCode(o,0);
OutOk:=True;
End;



Const
  Str_nop='|nop|';
Procedure Inst_nop;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
OutStr:=b2h($90);
OutOk:=True;
End;



Const
  Str_reprepnz='|rep|repe|repz|repne|repnz|';
Procedure Inst_reprepnz;
Var
  i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='repe') then Cmd:='rep';
if (Cmd='repz') then Cmd:='rep';
if (Cmd='repne') then Cmd:='repnz';
if (Cmd='rep') then i:=$f3;
if (Cmd='repnz') then i:=$f2;
if (i=-1) then Exit;
OutStr:=b2h(i);
OutOk:=True;
End;



Const
  Str_pushpop='|push|pop|';
Procedure Inst_pushpop;
Label i_Reg,i_Mem;
Var
  a:String;
  i,l:LongInt;
  r:OneRegType;
  m:OneMemPointRec;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
l:=-1;
if (Cmd='push') then l:=0;
if (Cmd='pop') then l:=1;
if (l=-1) then Exit;

if (p1.Typ=1) then Goto i_Reg;
if (p1.Typ=3) then Goto i_Mem;
Exit;

i_Reg:
r:=p1.Reg;
if (r.Typ=1) and (r.Val=3) then begin;
  {DataReg 16}
  i:=$50+l*8;
  OutStr:=b2h(i+r.Num-1);
  AddNativeOpCode(r.Len,0);
  OutOk:=True;
  Exit;
  end;
if (r.Typ=2) then begin;
  {PointReg 16,32}
  i:=$54+l*8;
  OutStr:=b2h(i+r.Num-1);
  AddNativeOpCode(r.Len,0);
  OutOk:=True;
  Exit;
  end;
if (r.Typ=3) and (r.Pro=0) then begin;
  {SegReg .286}
  i:=$06+l;
  if (Cmd='pop') and (r.Num=2) then Exit;
  OutStr:=b2h(i+(r.Num-1)*8);
  OutOk:=True;
  Exit;
  end;
if (r.Typ=3) and (r.Pro=1) then begin;
  {SegReg .386}
  i:=$a0+l;
  OutStr:=b2h($0f)+SepCh+b2h(i+(r.Num-1)*8);
  OutOk:=True;
  Exit;
  end;
Exit;

i_Mem:
if (Trns=1) then Exit;
m:=p1.Mem;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
l:=1-l;
OutStr:=a+SepCh;
i:=$8f+($70*l);
OutStr:=OutStr+b2h(i)+SepCh;
i:=l*$30;
a:=ConvertAllMemoryDatas(m,i);
if (a='') then Exit;
OutStr:=OutStr+a;
AddNativeOpCode(Trns,0);
OutOk:=True;
Exit;

End;



Const
  Str_lodsbstosb='|lodsb|lodsw|lodsd|stosb|stosw|stosd|movsb|movsw|movsd|'+
  'scasb|scasw|scasd|cmpsb|cmpsw|cmpsd|insb|insw|insd|outsb|outsw|outsd|'+
  'xlat|xlatb|';
Procedure Inst_lodsbstosb;
Var
  i:LongInt;
  a,b,s:String;
  r,rr:OneRegType;
  m:OneMemPointRec;
  CurSeg,CurPtr:Byte;
Begin;
if (OutOk=True) then Exit;
b:='';
if (p1.Typ<>0) then b:=b+p1.Str+',';
if (p2.Typ<>0) then b:=b+p2.Str+',';
if (p3.Typ<>0) then b:=b+p3.Str+',';
s:=Cmd;
i:=-1;
a:='lods';if (Copy(s,1,Length(a))=a) then begin; i:=$ac;Delete(s,1,Length(a)); end;
a:='stos';if (Copy(s,1,Length(a))=a) then begin; i:=$aa;Delete(s,1,Length(a)); end;
a:='movs';if (Copy(s,1,Length(a))=a) then begin; i:=$a4;Delete(s,1,Length(a)); end;
a:='scas';if (Copy(s,1,Length(a))=a) then begin; i:=$ae;Delete(s,1,Length(a)); end;
a:='cmps';if (Copy(s,1,Length(a))=a) then begin; i:=$a6;Delete(s,1,Length(a)); end;
a:='outs';if (Copy(s,1,Length(a))=a) then begin; i:=$6e;Delete(s,1,Length(a)); end;
a:='ins';if (Copy(s,1,Length(a))=a) then begin; i:=$6c;Delete(s,1,Length(a)); end;
if (s='xlat') or (s='xlatb') then begin; i:=$d7;s:='b'; end;
if (i=-1) then Exit;
if (Length(s)<>1) then Exit;
Case s[1] of
  'b':i:=i+0;
  'w','d':i:=i+1;
  else Exit;
  end;
OutStr:=b2h(i);
CurPtr:=0;CurSeg:=0;
rr.Typ:=0;
While (b<>'') do begin;
  i:=Pos(',',b);
  a:=Copy(b,1,i-1);
  b:=Copy(b,i+1,255);
  i:=0;
  if (a='ptr16') then i:=1;
  if (a='ptr32') then i:=2;
  if (i<>0) and (CurPtr=0) then begin; a:='';CurPtr:=i; end;
  AnalizeAnyRegs(a,r);
  if (r.Typ=3) and (CurSeg=0) then begin; a:='';CurSeg:=1;rr:=r; end;
  if (a='def') and (CurSeg=0) then begin; a:='';CurSeg:=1; end;
  if (a<>'') then Exit;
  end;
if (CurPtr=0) then CurPtr:=1;
m.Typ:=1;
m.Mode:=CurPtr;
m.SegR:=rr;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh+OutStr;
i:=1;
if (s='d') then i:=4;
if (s='w') then i:=2;
AddNativeOpCode(i,0);
OutOk:=True;
Exit;
End;



Const
  Str_incdec='|inc|dec|';
Procedure Inst_incdec;
Label i_Reg,i_Mem;
Var
  c,a,d:String;
  i,o,l:LongInt;
  r:OneRegType;
  m:OneMemPointRec;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;

l:=-1;
if (Cmd='inc') then l:=$00;
if (Cmd='dec') then l:=$08;
if (l=-1) then Exit;

if (p1.Typ=1) then Goto i_Reg;
if (p1.Typ=3) then Goto i_Mem;
Exit;

i_Reg:
r:=p1.Reg;
o:=l;
if (r.Typ=1) then begin;
  {DataReg}
  if (r.Val in [1,2]) then begin;
    OutStr:=b2h($fe)+SepCh+b2h($c0+o+r.Num-1+(r.Val-1)*4);
    end;
  if (r.Val in [3]) then begin;
    OutStr:=b2h($40+o+r.Num-1);
    end;
  AddNativeOpCode(r.Len,0);
  OutOk:=True;
  Exit;
  end;
if (r.Typ=2) then begin;
  {PointReg}
  OutStr:=b2h($44+o+r.Num-1);
  AddNativeOpCode(r.Len,0);
  OutOk:=True;
  Exit;
  end;
Exit;

i_Mem:
m:=p1.Mem;
r:=m.SegR;
c:=ConvertPointerSegRegs(m,0);
if (c='') then Exit;
Case Trns of
  1:c:=c+SepCh+b2h($fe);
  2,4:c:=c+SepCh+b2h($ff);
  else Exit;
  end;
OutStr:=c+SepCh;
AddNativeOpCode(Trns,0);
a:=ConvertAllMemoryDatas(m,l);
if (a='') then Exit;
OutStr:=OutStr+a;
OutOk:=True;
Exit;

End;



Const
  Str_addsubmov='|add|sub|mov|or|xor|and|cmp|xchg|test|sbb|adc|';
Procedure Inst_addsubmov;
Label i_RegReg,i_RegNum,i_RegMem,i_MemReg,i_MemNum;
Var
  r1,r2:OneRegType;
  r:OneRegType;
  m:OneMemPointRec;
  ppp:OneParameterRec;
  MemRegSwp:ShortInt;
  a,b,n:String;
  i,o,l:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
MemRegSwp:=0;

if (p1.Typ=1) and (p2.Typ=1) then Goto i_RegReg;
if (p1.Typ=1) and (p2.Typ in [2,4]) then Goto i_RegNum;
if (p1.Typ=1) and (p2.Typ=3) then Goto i_RegMem;
if (p1.Typ=3) and (p2.Typ=1) then Goto i_MemReg;
if (p1.Typ=3) and (p2.Typ in [2,4]) then Goto i_MemNum;
Exit;

i_RegReg:
r1:=p1.Reg;
r2:=p2.Reg;
if (r1.Typ=3) or (r2.Typ=3) then Exit;
{Public}
o:=-1;
if (Cmd='add') then o:=$02;
if (Cmd='sub') then o:=$2a;
if (Cmd='mov') then o:=$8a;
if (Cmd='or') then o:=$0a;
if (Cmd='xor') then o:=$32;
if (Cmd='and') then o:=$22;
if (Cmd='cmp') then o:=$3a;
if (Cmd='xchg') then o:=$86;
if (Cmd='test') then o:=$84;
if (Cmd='sbb') then o:=$1a;
if (Cmd='adc') then o:=$12;
if (o=-1) then Exit;
if (Cmd='test') then begin;
  r:=r1;
  r1:=r2;
  r2:=r;
  end;
if (r1.Len=1) then begin;
  {8bit data-data}
  i:=(b8_RegsToQueue(r1)-1)*8+b8_RegsToQueue(r2)-1;
  OutStr:=b2h($c0+i);
  OutStr:=b2h(o)+SepCh+OutStr;
  OutOk:=True;
  Exit;
  end;
if  (r1.Len in [2,4]) and (r1.Typ<>3) then begin;
  {data or point-data or point}
  i:=(b16_RegsToQueue(r1)-1)*8+b16_RegsToQueue(r2)-1;
  OutStr:=b2h($c0+i);
  OutStr:=b2h(o+1)+SepCh+OutStr;
  AddNativeOpCode(r1.Len,0);
  OutOk:=True;
  Exit;
  end;
Exit;

i_RegNum:
r1:=p1.Reg;
if (r1.Typ=3) then Exit;
{Public}
o:=-1;
if (Cmd='add') then o:=$c0;
if (Cmd='sub') then o:=$e8;
if (Cmd='or') then o:=$c8;
if (Cmd='xor') then o:=$f0;
if (Cmd='and') then o:=$e0;
if (Cmd='cmp') then o:=$f8;
if (Cmd='test') then o:=$c0;
if (Cmd='sbb') then o:=$d8;
if (Cmd='adc') then o:=$d0;
if (o=-1) then Exit;
n:=ConvertNumOrLabDatas(p2,r1.Len,'');
if (n='') then Exit;
l:=0;
if (Cmd='test') then l:=$76;
if (r1.Len=1) then begin;
  {8bit data}
  i:=b8_RegsToQueue(r1)-1;
  OutStr:=b2h($80+l)+SepCh+b2h(o+i);
  OutStr:=OutStr+SepCh+n;
  OutOk:=True;
  Exit;
  end;
if (r1.Len in [2,4]) then begin;
  {16,32bit data}
  i:=b16_RegsToQueue(r1)-1;
  OutStr:=b2h($81+l)+SepCh+b2h(o+i);
  OutStr:=OutStr+SepCh+n;
  AddNativeOpCode(r1.Len,0);
  OutOk:=True;
  Exit;
  end;
Exit;

i_RegMem:
r1:=p1.Reg;
m:=p2.Mem;
if (r1.Typ=3) then Exit;
{Public}
o:=-1;
if (Cmd='add') then o:=$02;
if (Cmd='sub') then o:=$2a;
if (Cmd='mov') then o:=$8a;
if (Cmd='or') then o:=$0a;
if (Cmd='xor') then o:=$32;
if (Cmd='and') then o:=$22;
if (Cmd='cmp') then o:=$3a;
if (Cmd='xchg') then o:=$86;
if (Cmd='test') then o:=$84;
if (Cmd='sbb') then o:=$1a;
if (Cmd='adc') then o:=$12;
if (o=-1) then Exit;
if (Cmd='xchg') then MemRegSwp:=0;
if (Cmd='test') then MemRegSwp:=0;
inc(o,MemRegSwp);
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh;
if (r1.Len=1) then i:=0 else i:=1;
OutStr:=OutStr+b2h(o+i)+SepCh;
if (r1.Len=1) then begin;
  i:=b8_RegsToQueue(r1);
  end;
if (r1.Len in [2,4]) then begin;
  i:=b16_RegsToQueue(r1);
  end;
i:=(i-1)*8;
a:=ConvertAllMemoryDatas(m,i);
if (a='') then Exit;
OutStr:=OutStr+a;
AddNativeOpCode(r1.Len,0);
OutOk:=True;
Exit;

i_MemReg:
r2:=p2.Reg;
if (r2.Typ=3) then Exit;
MemRegSwp:=-2;
ppp:=p1;
p1:=p2;
p2:=ppp;
Goto i_RegMem;
Exit;

i_MemNum:
m:=p1.Mem;
{Public}
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh;
i:=0;
if (Cmd='test') then i:=$76;
o:=$80+i;
if (Trns in [2,4]) then inc(o);
OutStr:=OutStr+b2h(o)+SepCh;
AddNativeOpCode(Trns,0);
o:=-1;
if (Cmd='add') then o:=$00;
if (Cmd='sub') then o:=$28;
if (Cmd='or') then o:=$08;
if (Cmd='xor') then o:=$30;
if (Cmd='and') then o:=$20;
if (Cmd='cmp') then o:=$38;
if (Cmd='test') then o:=$00;
if (Cmd='sbb') then o:=$18;
if (Cmd='adc') then o:=$10;
if (o=-1) then Exit;
a:=ConvertAllMemoryDatas(m,o);
if (a='') then Exit;
OutStr:=OutStr+a;
n:=ConvertNumOrLabDatas(p2,Trns,'');
if (n='') then Exit;
OutStr:=OutStr+SepCh+n;
OutOk:=True;
Exit;

End;



Const
  Str_mov='|mov|';
Procedure Inst_mov;
Label i_RegReg,i_RegNum,i_MemNum,i_MemReg,i_RegMem;
Var
  r1,r2:OneRegType;
  ppp:OneParameterRec;
  RegRegSwp:ShortInt;
  r:OneRegType;
  m:OneMemPointRec;
  i,o:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;

RegRegSwp:=0;
if (p1.Typ=1) and (p2.Typ=1) then Goto i_RegReg;
if (p1.Typ=1) and (p2.Typ in [2,4]) then Goto i_RegNum;
if (p1.Typ=3) and (p2.Typ in [2,4]) then Goto i_MemNum;
if (p1.Typ=3) and (p2.Typ=1) then Goto i_MemReg;
if (p1.Typ=1) and (p2.Typ=3) then Goto i_RegMem;
Exit;

i_RegReg:
r1:=p1.Reg;
r2:=p2.Reg;
if (r2.Typ=3) then begin;
  r:=r1;
  r1:=r2;
  r2:=r;
  RegRegSwp:=-2;
  end;
if (r1.Typ<>3) or (r2.Typ=3) then Exit;
if (r2.Len<2) then Exit;
OutStr:=b2h($8e+RegRegSwp)+SepCh;
i:=(b16_RegsToQueue(r1)-8-1)*8+b16_RegsToQueue(r2)-1;
if (r1.Pro=1) then i:=i+$20;
OutStr:=OutStr+b2h($c0+i);
AddNativeOpCode(r2.Len,0);
OutOk:=True;
Exit;

i_RegNum:
r1:=p1.Reg;
if (r1.Typ=3) then Exit;
if (r1.Len=1) then begin;
  {8bit}
  i:=b8_RegsToQueue(r1);
  OutStr:=b2h($b0+i-1);
  a:=ConvertNumOrLabDatas(p2,r1.Len,'');
  if (a='') then Exit;
  OutStr:=OutStr+SepCh+a;
  OutOk:=True;
  Exit;
  end;
if (r1.Len in [2,4]) then begin;
  {16bit}
  i:=b16_RegsToQueue(r1);
  OutStr:=b2h($b8+i-1);
  a:=ConvertNumOrLabDatas(p2,r1.Len,'');
  if (a='') then Exit;
  OutStr:=OutStr+SepCh+a;
  AddNativeOpCode(r1.Len,0);
  OutOk:=True;
  exit;
  end;
Exit;

i_MemNum:
m:=p1.Mem;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh;
if (Trns=0) then Exit;
if (Trns=1) then begin;
  o:=$c6;
  end else begin;
  o:=$c7;
  end;
OutStr:=OutStr+b2h(o)+SepCh;
AddNativeOpCode(Trns,0);
a:=ConvertAllMemoryDatas(m,0);
if (a='') then Exit;
OutStr:=OutStr+a+SepCh;
a:=ConvertNumOrLabDatas(p2,Trns,'');
if (a='') then Exit;
OutStr:=OutStr+a;
OutOk:=True;
Exit;

i_MemReg:
m:=p1.Mem;
r:=p2.Reg;
if (r.Typ<>3) then Exit;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh;
OutStr:=OutStr+b2h($8c+RegRegSwp)+SepCh;
i:=(b16_RegsToQueue(r)-8-1)*8;
if (r.Pro=1) then i:=i+$20;
a:=ConvertAllMemoryDatas(m,i);
if (a='') then Exit;
OutStr:=OutStr+a;
OutOk:=True;
Exit;

i_RegMem:
RegRegSwp:=2;
ppp:=p1;
p1:=p2;
p2:=ppp;
Goto i_MemReg;
Exit;

End;



Const
  Str_mov2='|mov|';
Procedure Inst_mov2;
Var
  r1,r2:OneRegType;
  RegRegSwp:ShortInt;
  r:OneRegType;
  i,o:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Typ<>1) and (p2.Typ<>1) then Exit;
if (p1.Typ<>4) and (p2.Typ<>4) then Exit;
if (p1.Typ=p2.Typ) then Exit;

RegRegSwp:=0;
if (p1.Typ<>1) then begin;
  p3:=p1;
  p1:=p2;
  p2:=p3;
  RegRegSwp:=2;
  end;
r:=p1.Reg;
if (r.Typ<>1) and (r.Typ<>2) then Exit;
if (r.Len<>4) then Exit;

AnalizeCTDregisters(p2.Str,i,o);
if (i=0) then Exit;
dec(i);

if (i=2) then i:=4;
OutStr:=b2h($0f)+SepCh+b2h($20+i+RegRegSwp);

i:=b16_RegsToQueue(r);
dec(i);
OutStr:=OutStr+SepCh+b2h($c0+o*$08+i);
OutOk:=True;
Exit;

End;



Const
  Str_int='|int|';
Procedure Inst_int;
Var
  a:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns<>0) then Exit;
a:=ConvertNumOrLabDatas(p1,1,'');
if (a='') then Exit;
OutStr:=b2h($cd)+SepCh+a;
OutOk:=True;
End;



Const
  Str_shlshr='|shl|shr|rol|ror|rcl|rcr|sal|sar|';
Procedure Inst_shlshr;
Label i_RegAny,i_MemAny;
Var
  r:OneRegType;
  m:OneMemPointRec;
  i,o,l:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
if not (p2.Typ in [1,2,4]) then Exit;
if (p2.Typ=1) and (All_RegsToQueue(p2.Reg)<>$010200) then Exit;

l:=-1;
if (Cmd='rcl') then l:=$10;
if (Cmd='rcr') then l:=$18;
if (Cmd='sal') then Cmd:='shl';
if (Cmd='sar') then l:=$38;
if (Cmd='shl') then l:=$20;
if (Cmd='shr') then l:=$28;
if (Cmd='rol') then l:=$00;
if (Cmd='ror') then l:=$08;
if (l=-1) then Exit;

if (p1.Typ=1) then Goto i_RegAny;
if (p1.Typ=3) then Goto i_MemAny;
Exit;

i_RegAny:
r:=p1.Reg;
if (r.Typ=3) then Exit;
l:=l+$c0;
if (r.Len=1) then begin;
  o:=$c0;
  i:=b8_RegsToQueue(r);
  end;
if (r.Len in [2,4]) then begin;
  o:=$c1;
  i:=b16_RegsToQueue(r);
  end;
OutStr:=b2h(o)+SepCh;
OutStr:=OutStr+b2h(l+i-1);
a:='';
Case p2.Typ of
  1:begin;
    i:=h2b(Copy(OutStr,1,2));
    i:=i+$12;
    OutStr:=b2h(i)+Copy(OutStr,3,255);
    end;
  2,4:begin;
    a:=ConvertNumOrLabDatas(p2,1,'');
    if (a='') then Exit;
    end;
  else Exit;
  end;
if (a<>'') then a:=SepCh+a;
OutStr:=OutStr+a;
AddNativeOpCode(r.Len,0);
OutOK:=True;
Exit;

i_MemAny:
if (Trns=0) then Exit;
m:=p1.Mem;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh;
Case Trns of
  1:o:=$c0;
  2,4:o:=$c1;
  else Exit;
  end;
if (p2.Typ=1) then o:=o+$12;
OutStr:=OutStr+b2h(o)+SepCh;
a:='';
a:=ConvertAllMemoryDatas(m,l);
if (a='') then Exit;
OutStr:=OutStr+a;
a:='';
Case p2.Typ of
  1:begin;
    end;
  2,4:begin;
    a:=ConvertNumOrLabDatas(p2,1,'');
    if (a='') then Exit;
    end;
  else Exit;
  end;
if (a<>'') then a:=SepCh+a;
OutStr:=OutStr+a;
AddNativeOpCode(Trns,0);
OutOk:=True;
Exit;

End;



Const
  Str_pushpopadf='|pushf|pushfw|pushfd|popf|popfw|popfd|'+
    'pusha|pushaw|pushad|popa|popaw|popad|';
Procedure Inst_pushpopadf;
Var
  i,o:LongInt;
  s,a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
s:=Cmd;
o:=-1;
a:='push';if (Copy(s,1,Length(a))=a) then begin;
  o:=0;
  delete(s,1,Length(a));
  end;
a:='pop';if (Copy(s,1,Length(a))=a) then begin;
  o:=1;
  delete(s,1,Length(a));
  end;
if (o=-1) then Exit;
i:=-1;
if (s='f') or (s='fw') or (s='fd') then i:=$9c;
if (s='a') or (s='aw') or (s='ad') then i:=$60;
if (i=-1) then Exit;
OutStr:=b2h(o+i);
i:=2;if (s='ad') or (s='fd') then i:=4;
AddNativeOpCode(i,0);
OutOk:=True;
End;



Const
  Str_notneg='|not|neg|mul|imul|div|idiv|';
Procedure Inst_notneg;
Label i_Reg,i_Mem;
Var
  r:OneRegType;
  m:OneMemPointRec;
  i,o,l:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;

l:=-1;
if (Cmd='not') then l:=$00;
if (Cmd='neg') then l:=$08;
if (Cmd='mul') then l:=$10;
if (Cmd='imul') then l:=$18;
if (Cmd='div') then l:=$20;
if (Cmd='idiv') then l:=$28;
if (l=-1) then Exit;

if (p1.Typ=1) then Goto i_Reg;
if (p1.Typ=3) then Goto i_Mem;

Exit;

i_Reg:
r:=p1.Reg;
if (r.Typ=3) then Exit;
if (r.Len=1) then begin;
  i:=$f6;
  o:=b8_RegsToQueue(r);
  end else begin;
  i:=$f7;
  o:=b16_RegsToQueue(r);
  end;
OutStr:=b2h(i)+SepCh+b2h($d0+l+o-1);
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;

i_Mem:
m:=p1.Mem;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh;
if (Trns=1) then i:=$f6;
if (Trns in [2,4]) then i:=$f7;
OutStr:=OutStr+b2h(i)+SepCh;
a:=ConvertAllMemoryDatas(m,$10+l);
if (a='') then Exit;
OutStr:=OutStr+a;
AddNativeOpCode(Trns,0);
OutOk:=True;
Exit;

End;



Const
  Str_jmpcall='|jmp|call|';
Procedure Inst_jmpcall;
Label i_Reg,i_Mem,i_NumLab,i_Far;
Var
  r:OneRegType;
  m:OneMemPointRec;
  i,o,l:LongInt;
  a,b,c:String;

Procedure CompileFarParas(Var a:String);
Begin;
FillChar(p3,Sizeof(p3),0);
AnalizeParameter(a,0,p3);
a:=ConvertNumOrLabDatas(p3,2,'');
if (a='') then Exit;
a:=SepCh+a;
End;

Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;

l:=-1;
if (Cmd='call') then l:=0;
if (Cmd='jmp') then l:=1;
if (l=-1) then Exit;

if (p1.Typ=0) then Goto i_Far;
if (p1.Typ=1) then Goto i_Reg;
if (p1.Typ=3) then Goto i_Mem;
if (p1.Typ in [2,4]) then Goto i_NumLab;
Exit;

i_Reg:
r:=p1.Reg;
if (r.Typ=3) then Exit;
if (r.Len=1) then Exit;
if (Trns<>0) then Exit;
OutStr:=b2h($ff)+SepCh;

i:=b16_RegsToQueue(r)-1;
i:=$d0+i+l*$10;
OutStr:=OutStr+b2h(i);

AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;

i_Mem:
if (Trns=0) then Exit;
m:=p1.Mem;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh;
OutStr:=OutStr+b2h($ff)+SepCh;
o:=-1;
if (Trns=2) then o:=0;
if (Trns=4) then o:=$08;
if (o=-1) then Exit;

i:=(l+1)*$10;
a:=ConvertAllMemoryDatas(m,i+o);
if (a='') then Exit;
OutStr:=OutStr+a;
AddNativeOpCode(2,0);
OutOk:=True;
Exit;

i_NumLab:
if (Trns=0) then Trns:=2;
if (Trns=1) and (l=0) then Exit;
i:=$e8;
if (Trns=1) then i:=$ea;
OutStr:=b2h(i+l)+SepCh;
AddNativeOpCode(Trns,0);
a:='';
if (p1.Typ=2) then begin;
  a:=p1.Num;
  a:='numofs'+SpxCh+a;
  end;
if (p1.Typ=4) then begin;
  a:=p1.Lab;
  a:='ofs?'+SpxCh+a;
  end;
if (a='') then Exit;
if (Trns=1) then c:='signed'+SpxCh else c:='';
OutStr:=OutStr+DataSizeNames[Trns]+SpxCh;
OutStr:=OutStr+'rela2end'+SpxCh+c+a;
OutOk:=True;
Exit;

i_Far:
a:=p1.Str;
i:=Pos(':',a);
b:=Copy(a,1,i-1);
c:=Copy(a,i+1,255);
CompileFarParas(b);
CompileFarParas(c);
if (b='') or (c='') then Exit;

OutStr:=b2h($9a+l*$50)+c+b;
AddNativeOpCode(2,0);
OutOk:=True;
Exit;

End;



Const
  Str_push='|push|';
Procedure Inst_push;
Var
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns=1) then Exit;
a:=ConvertNumOrLabDatas(p1,Trns,'');
if (a='') then Exit;
OutStr:=b2h($68)+SepCh+a;
AddNativeOpCode(Trns,0);
OutOk:=True;
Exit;
End;



Const
  Str_inout='|in|out|';
Procedure Inst_inout;
Label i_Reg,i_Num,i_Vege;
Var
  a:String;
  i,o:LongInt;
  ppp:OneParameterRec;
  r:OneRegType;
Begin;
if (OutOk=True) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns<>0) then Exit;
if (Cmd='in') then begin;
  ppp:=p1;
  p1:=p2;
  p2:=ppp;
  end;
if (p2.Typ<>1) then Exit;
r:=p2.Reg;
r.Pro:=0;
i:=All_RegsToQueue(r);
o:=-1;
if (i=$010100) then o:=0;
if (i=$030100) then o:=1;
if (o=-1) then Exit;
i:=-1;
if (Cmd='in') then i:=0;
if (Cmd='out') then i:=1;
if (i=-1) then Exit;
o:=i*2+o;

if (p1.Typ=1) then Goto i_Reg;
if (p1.Typ in [2,4]) then Goto i_Num;

Exit;

i_Reg:
if (All_RegsToQueue(p1.Reg)<>$030300) then Exit;
OutStr:=b2h($ec+o);
Goto i_Vege;

i_Num:
a:=ConvertNumOrLabDatas(p1,1,'');
if (a='') then exit;
OutStr:=b2h($e4+o)+SepCh+a;
Goto i_Vege;

i_Vege:
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;
End;



Const
  Str_lea='|lea|';
Procedure Inst_lea;
Var
  a:String;
  i,o:LongInt;
  r:OneRegType;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [2,4]) then Exit;
if (p3.Typ<>0) then Exit;
r:=p1.Reg;
if (r.Typ=3) then Exit;
if (r.Len=1) then Exit;
OutStr:=b2h($8d)+SepCh;
i:=b16_RegsToQueue(r)-1;
OutStr:=OutStr+b2h($6+i*$8);
a:=ConvertNumOrLabDatas(p2,2,'ofs?');
if (a='') then Exit;
OutStr:=OutStr+SepCh+a;
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;
End;



Const
  Str_leslds='|lea|les|lds|lfs|lgs|lss|';
Procedure Inst_leslds;
Var
  r:OneRegType;
  m:OneMemPointRec;
  a:String;
  i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>3) then Exit;
if (p3.Typ<>0) then Exit;
r:=p1.Reg;
m:=p2.Mem;
if (r.Typ=3) then Exit;
if (r.Len=1) then Exit;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh;
i:=-1;o:=-1;
if (Cmd='lea') then i:=$8d;
if (Cmd='les') then i:=$c4;
if (Cmd='lds') then i:=$c5;
if (Cmd='lss') then begin; i:=$0f;o:=$b2; end;
if (Cmd='lfs') then begin; i:=$0f;o:=$b4; end;
if (Cmd='lgs') then begin; i:=$0f;o:=$b5; end;
if (i=-1) then Exit;
OutStr:=OutStr+b2h(i)+SepCh;
if (o<>-1) then OutStr:=OutStr+b2h(o)+SepCh;
i:=(b16_RegsToQueue(r)-1)*8;
a:=ConvertAllMemoryDatas(m,i);
if (a='') then Exit;
OutStr:=OutStr+a;
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;
End;




Const
  Str_jajbjc='|ja|jb|jnb|jbe|je|jg|jnl|jl|jle|jnc|jno|jns|jnp|jo|jp|js|'+
  'jnbe|jae|jnae|jna|jc|jz|jnle|jge|jnge|jng|jne|jnz|jpo|jpe|';
Procedure Inst_jajbjc;
Var
  r:OneRegType;
  m:OneMemPointRec;
  a,b:String;
  i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns=0) then Trns:=2;

a:='';if (Trns<>1) then a:=b2h($0f)+SepCh;
OutStr:=a;
AddNativeOpCode(Trns,0);
a:=Cmd;
b:='j';
if (Copy(a,1,length(b))=b) then Delete(a,1,length(b)) else Exit;
i:=AnalizeCpuConditions(a);
if (i=-1) then Exit;
o:=$80;if (Trns=1) then o:=$70;
OutStr:=OutStr+b2h(o+i)+SepCh;
a:='';
if (p1.Typ=2) then begin;
  a:=p1.Num;
  a:='numofs'+SpxCh+a;
  end;
if (p1.Typ=4) then begin;
  a:=p1.Lab;
  a:='ofs?'+SpxCh+a;
  end;
if (a='') then Exit;
if (Trns=1) then b:='signed'+SpxCh else b:='';
OutStr:=OutStr+DataSizeNames[Trns]+SpxCh;
OutStr:=OutStr+'rela2end'+SpxCh+b+a;
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;
End;



Const
  Str_loop='|loop|loope|loopz|loopnz|loopne|'+
  'loopw|loopwe|loopwz|loopwnz|loopwne|'+
  'loopd|loopde|loopdz|loopdnz|loopdne|'+
  'jcxz|jecxz|';
Procedure Inst_loop;
Var
  r:OneRegType;
  m:OneMemPointRec;
  a,b:String;
  i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
o:=-1;i:=2;
if (Copy(Cmd,1,4)='loop') then begin;
  a:=Cmd;
  delete(a,1,4);
  if (Copy(a,1,1)='w') then begin; i:=2;delete(a,1,1); end;
  if (Copy(a,1,1)='d') then begin; i:=4;delete(a,1,1); end;
  if (a='e') then a:='z';
  if (a='ne') then a:='nz';
  if (a='')   then o:=$e2;
  if (a='z')  then o:=$e1;
  if (a='nz') then o:=$e0;
  end;
if (Cmd='jcxz')  then begin; i:=2;o:=$e3; end;
if (Cmd='jecxz') then begin; i:=4;o:=$e3; end;
if (o=-1) then Exit;
OutStr:=b2h(o)+SepCh;
AddNativeOpCode(i,1);
a:='';
if (p1.Typ=2) then begin;
  a:=p1.Num;
  b:=Copy(a,5,4);
  a:=Copy(a,1,Length(a)-Length(b));
  if (Copy(ZeroFill,1,Length(a))<>a) then Exit;
  a:='numofs'+SpxCh+b;
  end;
if (p1.Typ=4) then begin;
  a:=p1.Lab;
  a:='ofs?'+SpxCh+a;
  end;
if (a='') then Exit;
a:='signed'+SpxCh+a;
OutStr:=OutStr+DataSizeNames[1]+SpxCh;
OutStr:=OutStr+'rela2end'+SpxCh+a;
OutOk:=True;
Exit;
End;



Const
  Str_bsfbsr='|bsf|bsr|';
Procedure Inst_bsfbsr;
Label i_RegReg,i_RegMem;
Var
  r1,r2:OneRegType;
  m:OneMemPointRec;
  a:String;
  i,o,l:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [1,3]) then Exit;
if (p3.Typ<>0) then Exit;
r1:=p1.Reg;
if (r1.Typ=3) then Exit;
if (r1.Len=1) then Exit;

l:=-1;
if (Cmd='bsf') then l:=0;
if (Cmd='bsr') then l:=1;
if (l=-1) then Exit;

if (p1.Typ=1) and (p2.Typ=1) then Goto i_RegReg;
if (p1.Typ=1) and (p2.Typ=3) then Goto i_RegMem;
Exit;

i_RegReg:
r2:=p2.Reg;
if (r2.Typ=3) then Exit;
OutStr:=b2h($0f)+SepCh;
OutStr:=OutStr+b2h($bc+l)+SepCh;
i:=$c0+(b16_RegsToQueue(r1)-1)*8+b16_RegsToQueue(r2)-1;
OutStr:=OutStr+b2h(i);
AddNativeOpCode(r1.Len,0);
OutOk:=True;
Exit;

i_RegMem:
m:=p2.Mem;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh;
OutStr:=OutStr+b2h($0f)+SepCh;
OutStr:=OutStr+b2h($bc+l)+SepCh;
i:=(b16_RegsToQueue(r1)-1)*8;
a:=ConvertAllMemoryDatas(m,i);
if (a='') then Exit;
OutStr:=OutStr+a;
AddNativeOpCode(r1.Len,0);
OutOk:=True;
Exit;

End;


Const
  Str_movsxzx='|movsx|movzx|';
Procedure Inst_movsxzx;
Label i_RegReg,i_RegMem,i_Vege;
Var
  r1,r2:OneRegType;
  m:OneMemPointRec;
  a:String;
  i,o,l:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [1,3]) then Exit;
if (p3.Typ<>0) then Exit;
r1:=p1.Reg;
if (r1.Typ=3) then Exit;
if (r1.Len=1) then Exit;
if (p2.Typ=1) then begin;
  r2:=p2.Reg;
  if not (r2.Len in [1,2]) then Exit;
  Trns:=r2.Len;
  end else begin;
  m:=p2.Mem;
  if not (Trns in [1,2]) then Exit;
  end;

l:=-1;
if (Cmd='movzx') then l:=-5;
if (Cmd='movsx') then l:=3;
if (l=-1) then Exit;

if (p1.Typ=1) and (p2.Typ=1) then Goto i_RegReg;
if (p1.Typ=1) and (p2.Typ=3) then Goto i_RegMem;
Exit;

i_RegReg:
if (r2.Typ=3) then Exit;
OutStr:=b2h($0f)+SepCh;
OutStr:=OutStr+b2h($ba+l+Trns)+SepCh;
if (r2.Len=1) then begin;
  i:=b8_RegsToQueue(r2);
  end;
if (r2.Len in [2,4]) then begin;
  i:=b16_RegsToQueue(r2);
  end;
i:=$c0+(b16_RegsToQueue(r1)-1)*8+i-1;
OutStr:=OutStr+b2h(i);
Goto i_Vege;

i_RegMem:
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh;
OutStr:=OutStr+b2h($0f)+SepCh;
OutStr:=OutStr+b2h($ba+l+Trns)+SepCh;
i:=(b16_RegsToQueue(r1)-1)*8;
a:=ConvertAllMemoryDatas(m,i);
if (a='') then Exit;
OutStr:=OutStr+a;
Goto i_Vege;

i_Vege:
AddNativeOpCode(r1.Len,0);
OutOk:=True;
Exit;
End;



Const
  Str_setasetb='|seta|setb|setnb|setbe|sete|setg|setnl|setl|setle|setnc|'+
  'setno|setns|setnp|seto|setp|sets|'+
  'setnbe|setae|setnae|setna|setc|setz|setnle|setge|setnge|setng|setne|'+
  'setnz|setpo|setpe|';
Procedure Inst_setasetb;
Label i_Reg,i_Mem;
Var
  r:OneRegType;
  m:OneMemPointRec;
  a,b:String;
  i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [1,3]) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
{Public}
OutStr:=b2h($0f)+SepCh;
a:=Cmd;
b:='set';
if (Copy(a,1,length(b))=b) then Delete(a,1,length(b)) else Exit;
i:=AnalizeCpuConditions(a);
if (i=-1) then Exit;
OutStr:=OutStr+b2h($90+i)+SepCh;

if (p1.Typ=1) then Goto i_Reg;
if (p1.Typ=3) then Goto i_Mem;
Exit;

i_Reg:
r:=p1.Reg;
if (r.Len<>1) then Exit;
i:=b8_RegsToQueue(r);
OutStr:=OutStr+b2h($c0+i-1);
OutOk:=True;
Exit;

i_Mem:
if (Trns<>1) then Exit;
m:=p1.Mem;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh+OutStr;
a:=ConvertAllMemoryDatas(m,0);
if (a='') then Exit;
OutStr:=OutStr+a;
OutOk:=True;
Exit;

End;



Const
  Str_cmovacmovb='|cmova|cmovb|cmovnb|cmovbe|cmove|cmovg|cmovnl|cmovl|'+
  'cmovle|cmovnc|cmovno|cmovns|cmovnp|cmovo|cmovp|cmovs|'+
  'cmovnbe|cmovae|cmovnae|cmovna|cmovc|cmovz|cmovnle|cmovge|cmovnge|'+
  'cmovng|cmovne|cmovnz|cmovpo|cmovpe|';
Procedure Inst_cmovacmovb;
Label i_RegReg,i_RegMem,Vege;
Var
  a,b:String;
  i:LongInt;
  r1,r2:OneRegType;
  m:OneMemPointRec;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [1,3]) then Exit;
if (p3.Typ<>0) then Exit;
OutStr:=b2h($0f)+SepCh;
a:=Cmd;
b:='cmov';
if (Copy(a,1,length(b))=b) then Delete(a,1,length(b)) else Exit;
i:=AnalizeCpuConditions(a);
if (i=-1) then Exit;
OutStr:=OutStr+b2h($40+i)+SepCh;

if (p2.Typ=1) then Goto i_RegReg;
if (p2.Typ=3) then Goto i_RegMem;
Exit;

i_RegReg:
r1:=p1.Reg;
r2:=p2.Reg;
if (r1.Typ=3) or (r2.Typ=3) then Exit;
if (r1.Len=1) then Exit;
i:=(b16_RegsToQueue(r1)-1)*8+b16_RegsToQueue(r2)-1;
OutStr:=OutStr+b2h($c0+i);
AddNativeOpCode(r1.Len,0);
OutOk:=True;
Exit;

i_RegMem:
r1:=p1.Reg;
m:=p2.Mem;
if (r1.Typ=3) then Exit;
if (r1.Len=1) then Exit;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh+OutStr;
i:=b16_RegsToQueue(r1);
i:=(i-1)*8;
a:=ConvertAllMemoryDatas(m,i);
if (a='') then Exit;
OutStr:=OutStr+a;
AddNativeOpCode(r1.Len,0);
OutOk:=True;
Exit;

Vege:
Exit;
End;



Const
  Str_btbtc='|bt|bts|btr|btc|';
Procedure Inst_btbtc;
Label i_RegReg,i_RegNum,i_MemNum,i_MemReg;
Var
  r1,r2:OneRegType;
  m:OneMemPointRec;
  a:String;
  i,o,l:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;

l:=-1;
if (Cmd='bt')  then l:=0;
if (Cmd='bts') then l:=1;
if (Cmd='btr') then l:=2;
if (Cmd='btc') then l:=3;
if (l=-1) then Exit;

if (p1.Typ=1) and (p2.Typ=1) then Goto i_RegReg;
if (p1.Typ=1) and (p2.Typ in [2,4]) then Goto i_RegNum;
if (p1.Typ=3) and (p2.Typ in [2,4]) then Goto i_MemNum;
if (p1.Typ=3) and (p2.Typ=1) then Goto i_MemReg;
Exit;

i_RegReg:
r1:=p1.Reg;
r2:=p2.Reg;
if (r1.Len<2) then Exit;
if (r1.Typ=3) then Exit;
if (r2.Typ=3) then Exit;
OutStr:=b2h($0f)+SepCh;
OutStr:=OutStr+b2h($a3+l*8)+SepCh;
i:=(b16_RegsToQueue(r2)-1)*8+b16_RegsToQueue(r1)-1;
OutStr:=OutStr+b2h($c0+i);
AddNativeOpCode(r1.Len,0);
OutOk:=True;
Exit;

i_RegNum:
r1:=p1.Reg;
if (r1.Len<2) then Exit;
if (r1.Typ=3) then Exit;
OutStr:=b2h($0f)+SepCh;
OutStr:=OutStr+b2h($ba)+SepCh;
i:=(b16_RegsToQueue(r1)-1)+l*8;
OutStr:=OutStr+b2h($e0+i)+SepCh;
a:=ConvertNumOrLabDatas(p2,1,'');
if (a='') then Exit;
OutStr:=OutStr+a;
AddNativeOpCode(r1.Len,0);
OutOk:=True;
Exit;

i_MemNum:
m:=p1.Mem;
if (Trns<2) then Exit;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh;
OutStr:=OutStr+b2h($0f)+SepCh;
OutStr:=OutStr+b2h($ba)+SepCh;
i:=$20+l*8;
a:=ConvertAllMemoryDatas(m,i);
if (a='') then Exit;
OutStr:=OutStr+a+SepCh;
a:=ConvertNumOrLabDatas(p2,1,'');
if (a='') then Exit;
OutStr:=OutStr+a;
AddNativeOpCode(Trns,0);
OutOk:=True;
Exit;

i_MemReg:
m:=p1.Mem;
r1:=p2.Reg;
if (r1.Len<2) then Exit;
if (r1.Typ=3) then Exit;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh;
OutStr:=OutStr+b2h($0f)+SepCh;
OutStr:=OutStr+b2h($a3+l*8)+SepCh;
i:=(b16_RegsToQueue(r1)-1)*8;
a:=ConvertAllMemoryDatas(m,i);
if (a='') then Exit;
OutStr:=OutStr+a;
AddNativeOpCode(r1.Len,0);
OutOk:=True;
Exit;

End;



Const
  Str_aadaam='|aad|aam|';
Procedure Inst_aadaam;
Label Folyt1;
Var
  i,o:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;

i:=-1;
if (Cmd='aad') then i:=$d5;
if (Cmd='aam') then i:=$d4;
if (i=-1) then Exit;

if (p1.Typ=0) then begin;
  a:=b2h(10);
  Goto Folyt1;
  end;
if (p1.Typ in [2,4]) then begin;
  a:=ConvertNumOrLabDatas(p1,1,'');
  Goto Folyt1;
  end;
Exit;


Folyt1:
OutStr:=b2h(i)+SepCh+a;
OutOk:=True;
End;



Const
  Str_clcstc='|clc|cld|cli|stc|std|sti|setalc|aaa|aas|daa|das|';
Procedure Inst_clcstc;
Var
  i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='setalc') then i:=$d6;
if (Cmd='clc') then i:=$f8;
if (Cmd='stc') then i:=$f9;
if (Cmd='cli') then i:=$fa;
if (Cmd='sti') then i:=$fb;
if (Cmd='cld') then i:=$fc;
if (Cmd='std') then i:=$fd;
if (Cmd='aaa') then i:=$37;
if (Cmd='aas') then i:=$3f;
if (Cmd='daa') then i:=$27;
if (Cmd='das') then i:=$2f;
if (i=-1) then Exit;
OutStr:=b2h(i);
OutOk:=True;
End;


Const
  Str_cbwcdq='|cbw|cdq|cmc|cwd|cwde|';
Procedure Inst_cbwcdq;
Var
  i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;o:=-1;
if (Cmd='cbw')  then begin; o:=2;i:=$98; end;
if (Cmd='cdq')  then begin; o:=4;i:=$99; end;
if (Cmd='cmc')  then i:=$f5;
if (Cmd='cwd')  then begin; o:=2;i:=$99; end;
if (Cmd='cwde') then begin; o:=4;i:=$98; end;
if (i=-1) then Exit;
OutStr:=b2h(i);
AddNativeOpCode(o,0);
OutOk:=True;
End;




Const
  Str_intohlt='|into|int3|hlt|sahf|lahf|leave|icebp|';
Procedure Inst_intohlt;
Var
  i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='into') then i:=$ce;
if (Cmd='int3') then i:=$cc;
if (Cmd='hlt')  then i:=$f4;
if (Cmd='sahf') then i:=$9e;
if (Cmd='lahf') then i:=$9f;
if (Cmd='leave') then i:=$c9;
if (Cmd='icebp') then i:=$f1;
if (i=-1) then Exit;
OutStr:=b2h(i);
OutOk:=True;
End;


Const
  Str_enter='|enter|';
Procedure Inst_enter;
Var
  i:LongInt;
  a,b:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if not (p2.Typ in [2,4]) then Exit;
if (p3.Typ<>0) then Exit;
OutStr:=b2h($c8)+SepCh;
a:=ConvertNumOrLabDatas(p1,2,'');
if (a='') then Exit;
OutStr:=OutStr+a+SepCh;
a:=ConvertNumOrLabDatas(p2,1,'');
if (a='') then Exit;
OutStr:=OutStr+a;
OutOk:=True;
Exit;
End;



Const
  Str_bound='|bound|';
Procedure Inst_bound;
Var
  r:OneRegType;
  m:OneMemPointRec;
  i:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>3) then Exit;
if (p3.Typ<>0) then Exit;
r:=p1.Reg;
if (r.Typ=3) then Exit;
if (r.Len<2) then Exit;
m:=p2.Mem;
i:=-1;
if (Cmd='bound') then i:=$62;
if (i=-1) then Exit;
OutStr:=b2h(i);
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh+OutStr;
i:=(b16_RegsToQueue(r)-1)*8;
a:=ConvertAllMemoryDatas(m,i);
if (a='') then Exit;
OutStr:=OutStr+SepCh+a;
AddNativeOpCode(r.Len,0);
OutOk:=True;
End;



Const
  Str_shrdshld='|shld|shrd|';
Procedure Inst_shrdshld;
Var
  r1,r2:OneRegType;
  m:OneMemPointRec;
  i,o,l:LongInt;
  a,b:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [1,3]) then Exit;
if (p2.Typ<>1) then Exit;
if not (p3.Typ in [1,2,4]) then Exit;
l:=-1;
if (Cmd='shld') then l:=$a4;
if (Cmd='shrd') then l:=$ac;
if (l=-1) then Exit;
r1:=p1.Reg;if (p1.Typ<>1) then r1.Num:=0;
m:=p1.Mem;
r2:=p2.Reg;
if (p3.Typ=1) and (All_RegsToQueue(p3.Reg)<>$010200) then Exit;
if (r2.Typ=3) then Exit;
if (r2.Len<2) then Exit;
if (p1.Typ=1) then begin;
  if (r1.Typ=3) then Exit;
  if (r1.Len<2) then Exit;
  end;
OutStr:='';
if (p3.Typ=1) then begin;
  {Reg}
  inc(l);
  end else begin;
  {Num,Lab}
  a:=ConvertNumOrLabDatas(p3,1,'');
  if (a='') then Exit;
  OutStr:=SepCh+a;
  end;
a:='';
i:=(b16_RegsToQueue(r2)-1)*8;
if (p1.Typ=1) then begin;
  i:=i+b16_RegsToQueue(r1)-1;
  a:=b2h($c0+i);
  end;
if (p1.Typ=3) then begin;
  a:=ConvertAllMemoryDatas(m,i);
  end;
if (a='') then Exit;
OutStr:=SepCh+a+OutStr;
a:=b2h($0f)+SepCh+b2h(l);
OutStr:=a+OutStr;
if (p1.Typ=3) then begin;
  a:=ConvertPointerSegRegs(m,0);
  if (a='') then Exit;
  OutStr:=a+SepCh+OutStr;
  end;
AddNativeOpCode(r2.Len,0);
OutOk:=True;
End;



Const
  Str_lockwait='|lock|wait|fwait|';
Procedure Inst_lockwait;
Var
  i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='fwait') then Cmd:='wait';
if (Cmd='lock') then i:=$f0;
if (Cmd='wait') then i:=$9b;
if (i=-1) then Exit;
OutStr:=b2h(i);
OutOk:=True;
End;



Const
  Str_verrverw1='|sldt|str|lldt|ltr|verr|verw|smsw|lmsw|';
Procedure Inst_verrverw1;
Var
  i,o:LongInt;
  r:OneRegType;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
r:=p1.Reg;
if not (r.Len in [2,4]) then Exit;
if not (r.Typ in [1,2]) then Exit;

o:=-1;
if (Cmd='sldt') then o:=$00c0;
if (Cmd='str')  then o:=$00c8;
if (Cmd='lldt') then o:=$00d0;
if (Cmd='ltr')  then o:=$00d8;
if (Cmd='verr') then o:=$00e0;
if (Cmd='verw') then o:=$00e8;
if (Cmd='smsw') then o:=$01e0;
if (Cmd='lmsw') then o:=$01f0;
if (o=-1) then Exit;

i:=b16_RegsToQueue(r);
dec(i);

OutStr:=b2h($0f)+SepCh+b2h(o div 256)+SepCh+b2h(o mod 256+i);
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;

End;



Const
  Str_verrverw2='|sldt|str|lldt|ltr|verr|verw|smsw|lmsw|sgdt|sidt|lgdt|lidt|';
Procedure Inst_verrverw2;
Var
  i,o:LongInt;
  m:OneMemPointRec;
  r:OneRegType;
  a,c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
m:=p1.Mem;
r:=m.SegR;

if (Trns<2) then exit;

o:=-1;
if (Cmd='sldt') then o:=$00c0;
if (Cmd='str')  then o:=$00c8;
if (Cmd='lldt') then o:=$00d0;
if (Cmd='ltr')  then o:=$00d8;
if (Cmd='verr') then o:=$00e0;
if (Cmd='verw') then o:=$00e8;
if (Cmd='sgdt') then o:=$01c0;
if (Cmd='sidt') then o:=$01c8;
if (Cmd='lgdt') then o:=$01d0;
if (Cmd='lidt') then o:=$01d8;
if (Cmd='smsw') then o:=$01e0;
if (Cmd='lmsw') then o:=$01f0;
if (o=-1) then Exit;

a:=ConvertAllMemoryDatas(m,o mod 256-$c0);
if (a='') then Exit;

c:=ConvertPointerSegRegs(m,0);
if (c='') then Exit;

OutStr:=c+SepCh+b2h($0f)+SepCh+b2h(o div 256)+SepCh+a;
AddNativeOpCode(Trns,0);
OutOk:=True;
Exit;

End;



Const
  Str_arpl='|arpl|';
Procedure Inst_arpl;
Label i_RegReg,i_MemReg;
Var
  i,o:LongInt;
  m:OneMemPointRec;
  r:OneRegType;
  rr:OneRegType;
  a,c:String;
Begin;
if (OutOk=True) then Exit;
if (p3.Typ<>0) then Exit;

if (p1.Typ=1) and (p2.Typ=1) then Goto i_RegReg;
if (p1.Typ=3) and (p2.Typ=1) then Goto i_MemReg;
Exit;


i_RegReg:
r:=p1.Reg;
rr:=p2.Reg;
if not (r.Typ in [1,2]) then Exit;
if (r.Len<>2) then Exit;
if not (rr.Typ in [1,2]) then Exit;
if (rr.Len<>2) then Exit;

i:=b16_RegsToQueue(r);dec(i);
o:=b16_RegsToQueue(rr);dec(o);

OutStr:=b2h($63)+SepCh+b2h($c0+o*$8+i);
OutOk:=True;
Exit;


i_MemReg:
m:=p1.Mem;
rr:=m.SegR;
r:=p2.Reg;
if not (r.Typ in [1,2]) then Exit;
if (r.Len<>2) then Exit;

i:=b16_RegsToQueue(r);dec(i);

a:=ConvertAllMemoryDatas(m,i*$8);
if (a='') then Exit;

c:=ConvertPointerSegRegs(m,0);
if (c='') then Exit;

OutStr:=c+SepCh+b2h($63)+SepCh+a;
OutOk:=True;
Exit;

End;


Const
  Str_cpuid='|cpuid|rdmsr|wrmsr|rdpmc|rdtsc|oio|ud|ud2|rsm|clts|'+
            'sysenter|sysexit|';
Procedure Inst_cpuid;
Var
  i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;

i:=-1;
if (Cmd='rdmsr')     then i:=$32;
if (Cmd='wrmsr')     then i:=$30;
if (Cmd='cpuid')     then i:=$a2;
if (Cmd='rdpmc')     then i:=$33;
if (Cmd='rdtsc')     then i:=$31;
if (Cmd='ud')        then i:=$b9;
if (Cmd='ud2')       then i:=$0b;
if (Cmd='oio')       then i:=$ff;
if (Cmd='rsm')       then i:=$aa;
if (Cmd='clts')      then i:=$06;
if (Cmd='sysenter')  then i:=$34;
if (Cmd='sysexit')   then i:=$35;
if (i=-1) then Exit;

OutStr:=b2h($0f)+SepCh+b2h(i);
OutOk:=True;
Exit;

End;



Const
  Str_larlsl='|lar|lsl|';
Procedure Inst_larlsl;
Label i_RegReg,i_RegMem;
Var
  i,o:LongInt;
  m:OneMemPointRec;
  r:OneRegType;
  rr:OneRegType;
  a,c:String;
Begin;
if (OutOk=True) then Exit;
if (p3.Typ<>0) then Exit;

o:=-1;
if (Cmd='lar') then o:=$02;
if (Cmd='lsl') then o:=$03;
if (o=-1) then Exit;
OutStr:=b2h($0f)+SepCh+b2h(o)+SepCh;
r:=p1.Reg;
if not (r.Typ in [1,2]) then Exit;
AddNativeOpCode(r.Len,0);

if (p1.Typ=1) and (p2.Typ=1) then Goto i_RegReg;
if (p1.Typ=1) and (p2.Typ=3) then Goto i_RegMem;
Exit;


i_RegReg:
rr:=p2.Reg;
if not (rr.Typ in [1,2]) then Exit;

i:=b16_RegsToQueue(r);dec(i);
o:=b16_RegsToQueue(rr);dec(o);

OutStr:=OutStr+b2h($c0+i*$8+o);
OutOk:=True;
Exit;


i_RegMem:
m:=p2.Mem;
rr:=m.SegR;

o:=b16_RegsToQueue(r);dec(o);

a:=ConvertAllMemoryDatas(m,o*$8);
if (a='') then Exit;

c:=ConvertPointerSegRegs(m,0);
if (c='') then Exit;

OutStr:=c+SepCh+OutStr+a;
OutOk:=True;
Exit;

End;


Const
  Str_bswap='|bswap|';
Procedure Inst_bswap;
Var
  i,o:LongInt;
  r:OneRegType;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>0) then Exit;

if (p1.Typ<>1) then Exit;
r:=p1.Reg;
if (r.Len<2) then exit;
if (r.Typ in [1,2]) then else exit;
i:=b16_RegsToQueue(r);
if (i=0) then Exit;

OutStr:=b2h($0f)+SepCh+b2h($c7+i);
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;

End;


Const
  Str_wbinvd='|wbinvd|invd|';
Procedure Inst_wbinvd;
Var
  i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;

i:=-1;
if (Cmd='wbinvd') then i:=$9;
if (Cmd='invd') then i:=$8;
if (i=-1) then Exit;

OutStr:=b2h($0f)+SepCh+b2h(i);
OutOk:=True;
Exit;

End;


Const
  Str_invlpg='|invlpg|cmpxchg8b|';
Procedure Inst_invlpg;
Var
  i,o:LongInt;
  m:OneMemPointRec;
  r:OneRegType;
  a,c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns<>0) then Exit;
m:=p1.Mem;
r:=m.SegR;

i:=-1;o:=-1;
if (Cmd='cmpxchg8b') then begin; i:=$c7;o:=$08; end;
if (Cmd='invlpg')    then begin; i:=$01;o:=$38; end;
if (i=-1) or (o=-1) then Exit;

a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh+b2h($0f)+SepCh;

a:=ConvertAllMemoryDatas(m,o);
if (a='') then Exit;
OutStr:=OutStr+b2h(i)+SepCh+a;

OutOk:=True;
Exit;

End;


Const
  Str_cmpxchgxadd='|cmpxchg|xadd|';
Procedure Inst_cmpxchgxadd;
Label i_RegReg,i_MemReg;
Var
  opc:byte;
  i,o:LongInt;
  m:OneMemPointRec;
  r:OneRegType;
  rr:OneRegType;
  a,c:String;
Begin;
if (OutOk=True) then Exit;
if (p3.Typ<>0) then Exit;

opc:=0;
if (Cmd='xadd') then opc:=$c0;
if (Cmd='cmpxchg') then opc:=$b0;
if (opc=0) then Exit;
inc(opc);


if (p1.Typ=1) and (p2.Typ=1) then Goto i_RegReg;
if (p1.Typ=3) and (p2.Typ=1) then Goto i_MemReg;
Exit;

i_RegReg:
r:=p1.Reg;
rr:=p2.Reg;
if not (r.Typ in [1,2]) then Exit;
if not (rr.Typ in [1,2]) then Exit;

i:=int_RegsToQueue(r);dec(i);
o:=int_RegsToQueue(rr);dec(o);

OutStr:=b2h($0f)+SepCh+b2h(opc-r.Len mod 2)+SepCh+b2h($c0+o*$8+i);
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;

i_MemReg:
m:=p1.Mem;
rr:=m.SegR;
r:=p2.Reg;
if not (r.Typ in [1,2]) then Exit;

i:=int_RegsToQueue(r);dec(i);

a:=ConvertAllMemoryDatas(m,i*$8);
if (a='') then Exit;

c:=ConvertPointerSegRegs(m,0);
if (c='') then Exit;

OutStr:=c+SepCh+b2h($0f)+SepCh+b2h(opc-r.Len mod 2)+SepCh+a;
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;

End;


Const
  Str_imul='|imul|';
Procedure Inst_imul;
Label i_RegNum,i_RegReg,i_RegMem,i_RegRegNum,i_RegMemNum;
Var
  i,o:LongInt;
  m:OneMemPointRec;
  r:OneRegType;
  rr:OneRegType;
  a,c,n:String;
Begin;
if (OutOk=True) then Exit;

if (p1.Typ=1) and (p2.Typ in [2,4]) and (p3.Typ=0) then Goto i_RegNum;
if (p1.Typ=1) and (p2.Typ=1) and (p3.Typ=0) then Goto i_RegReg;
if (p1.Typ=1) and (p2.Typ=3) and (p3.Typ=0) then Goto i_RegMem;
if (p1.Typ=1) and (p2.Typ=1) and (p3.Typ in [2,4]) then Goto i_RegRegNum;
if (p1.Typ=1) and (p2.Typ=3) and (p3.Typ in [2,4]) then Goto i_RegMemNum;
Exit;

i_RegNum:
r:=p1.Reg;
if (r.Typ in [1,2]) then else Exit;
if (r.Len<2) then Exit;

i:=b16_RegsToQueue(r);dec(i);

n:=ConvertNumOrLabDatas(p2,r.Len,'');
if (n='') then Exit;

OutStr:=b2h($69)+SepCh+b2h($c0+i*9)+SepCh+n;
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;

i_RegReg:
r:=p1.Reg;
rr:=p2.Reg;
if not (r.Typ in [1,2]) then Exit;
if (r.Len<2) then Exit;
if not (rr.Typ in [1,2]) then Exit;
if (rr.Len<2) then Exit;

i:=b16_RegsToQueue(r);dec(i);
o:=b16_RegsToQueue(rr);dec(o);

OutStr:=b2h($0f)+SepCh+b2h($af)+SepCh+b2h($c0+i*$8+o);
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;

i_RegMem:
m:=p2.Mem;
rr:=m.SegR;
r:=p1.Reg;
if not (r.Typ in [1,2]) then Exit;
if (r.Len<2) then Exit;

i:=b16_RegsToQueue(r);dec(i);

a:=ConvertAllMemoryDatas(m,i*$8);
if (a='') then Exit;

c:=ConvertPointerSegRegs(m,0);
if (c='') then Exit;

OutStr:=c+SepCh+b2h($0f)+SepCh+b2h($af)+SepCh+a;
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;

i_RegRegNum:
r:=p1.Reg;
rr:=p2.Reg;
if not (r.Typ in [1,2]) then Exit;
if (r.Len<2) then Exit;
if not (rr.Typ in [1,2]) then Exit;
if (rr.Len<2) then Exit;

i:=b16_RegsToQueue(r);dec(i);
o:=b16_RegsToQueue(rr);dec(o);

n:=ConvertNumOrLabDatas(p3,r.Len,'');
if (n='') then Exit;

OutStr:=b2h($69)+SepCh+b2h($c0+i*$8+o)+SepCh+n;
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;

i_RegMemNum:
m:=p2.Mem;
rr:=m.SegR;
r:=p1.Reg;
if not (r.Typ in [1,2]) then Exit;
if (r.Len<2) then Exit;

i:=b16_RegsToQueue(r);dec(i);

a:=ConvertAllMemoryDatas(m,i*$8);
if (a='') then Exit;

c:=ConvertPointerSegRegs(m,0);
if (c='') then Exit;

n:=ConvertNumOrLabDatas(p3,r.Len,'');
if (n='') then Exit;

OutStr:=c+SepCh+b2h($69)+SepCh+a+SepCh+n;
AddNativeOpCode(r.Len,0);
OutOk:=True;
Exit;

End;


Const
  Str_ExtEsc='|esc|';
Procedure Inst_ExtEsc;
Label i_NumReg,i_NumMem;
Var
  Code:Byte;
  i,o:LongInt;
  a:String;
  r:OneRegType;
  m:OneMemPointRec;
Begin;
if (OutOk=True) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Typ<>2) then Exit;

a:=p1.Num;i:=6;
if (Copy(a,1,i)<>Copy(ZeroFill,1,6)) then Exit;
delete(a,1,i);i:=h2b(a);if (b2h(i)<>a) then Exit;
if (i<0) or (i>$3f) then Exit;
OutStr:=b2h($d8+i div 8)+SepCh;
Code:=(i mod 8)*8;

if (p2.Typ=1) then Goto i_NumReg;
if (p2.Typ=3) then Goto i_NumMem;
Exit;

i_NumReg:
r:=p2.Reg;i:=0;
Case r.Len of
  1:i:=b8_RegsToQueue(r);
  2,4:i:=b16_RegsToQueue(r);
  end;
if (i<1) or (i>8) then Exit;
OutStr:=OutStr+b2h($c0+Code+i-1);
OutOk:=True;
Exit;

i_NumMem:
m:=p2.Mem;
a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh+OutStr;

a:=ConvertAllMemoryDatas(m,Code);
if (a='') then Exit;
OutStr:=OutStr+a;
OutOk:=True;
Exit;

End;




Var
  a,b:String;
  i,o:LongInt;
  RegTest:Boolean;
  TrnsTest:Boolean;
  ParaTest:Boolean;
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
o:=AnalizeBytesToTransfer(Copy(b,1,i-1));
Trns:=o;
if (o<>0) then begin;
  Delete(b,1,i);
  end;
b:=xLevesz(b);
i:=Pos(',',b+',');
a:=Copy(b,1,i-1);
b:=Copy(b,i+1,255);
AnalizeParameter(a,0,p1);
b:=xLevesz(b);
i:=Pos(',',b+',');
a:=Copy(b,1,i-1);
b:=Copy(b,i+1,255);
AnalizeParameter(a,0,p2);
AnalizeParameter(b,0,p3);
OutStr:='er';
if (Cmd='') then Exit;

ParaTest:=True;
if BenneVan(Str_jmpcall,Cmd) then ParaTest:=False;

if ParaTest then begin;
  if (p1.Typ=0) and (p1.Str<>'') then Exit;
  if (p2.Typ=0) and (p2.Str<>'') then Exit;
  if (p3.Typ=0) and (p3.Str<>'') then Exit;
  end;

RegTest:=True;
TrnsTest:=True;
if BenneVan(Str_jmpcall,Cmd) then TrnsTest:=False;
if BenneVan(Str_int,Cmd) then TrnsTest:=False;
if BenneVan(Str_shlshr,Cmd) then begin;
  RegTest:=False;
  TrnsTest:=False;
  end;
if BenneVan(Str_movsxzx,Cmd) then begin;
  RegTest:=False;
  TrnsTest:=False;
  end;
if BenneVan(Str_inout,Cmd) then RegTest:=False;
if BenneVan(Str_jajbjc,Cmd) then TrnsTest:=False;
if BenneVan(Str_loop,Cmd) then TrnsTest:=False;
if BenneVan(Str_enter,Cmd) then TrnsTest:=False;
if BenneVan(Str_invlpg,Cmd) then TrnsTest:=False;
if BenneVan(Str_mov,Cmd) and (p1.Typ=1) and (p2.Typ=1) and
      (p2.Reg.Typ=3) then RegTest:=False;
if BenneVan(Str_aadaam,Cmd) then TrnsTest:=False;
if BenneVan(Str_retiret,Cmd) then TrnsTest:=False;
if BenneVan(Str_ExtEsc,Cmd) then TrnsTest:=False;
if BenneVan(Str_lodsbstosb,Cmd) then TrnsTest:=False;

if (RegTest=True) then begin;
  if (p1.Typ=1) and (p2.Typ=1) and (p1.Reg.Len<>p2.Reg.Len) then Exit;
  end;
if (TrnsTest=True) then begin;
  if (Trns=0) then begin;
    if (not (p1.Typ in [0,1])) and (p2.Typ<>1) then Exit;
    end else begin;
    if (p1.Typ=1) or (p2.Typ=1) then Exit;
    end;
  end;
OutStr:='ok';
OutOk:=False;

if BenneVan(Str_retiret,Cmd) then Inst_retiret;

if BenneVan(Str_nop,Cmd) then Inst_nop;

if BenneVan(Str_reprepnz,Cmd) then Inst_reprepnz;

if BenneVan(Str_pushpop,Cmd) then Inst_pushpop;

if BenneVan(Str_incdec,Cmd) then Inst_incdec;

if BenneVan(Str_lodsbstosb,Cmd) then Inst_lodsbstosb;

if BenneVan(Str_addsubmov,Cmd) then Inst_addsubmov;
if BenneVan(Str_mov2,Cmd) then Inst_mov2;
if BenneVan(Str_mov,Cmd) then Inst_mov;

if BenneVan(Str_int,Cmd) then Inst_int;

if BenneVan(Str_shlshr,Cmd) then Inst_shlshr;

if BenneVan(Str_pushpopadf,Cmd) then Inst_pushpopadf;

if BenneVan(Str_notneg,Cmd) then Inst_notneg;

if BenneVan(Str_jmpcall,Cmd) then Inst_jmpcall;

if BenneVan(Str_push,Cmd) then Inst_push;

if BenneVan(Str_inout,Cmd) then Inst_inout;

if BenneVan(Str_lea,Cmd) then Inst_lea;
if BenneVan(Str_leslds,Cmd) then Inst_leslds;

if BenneVan(Str_jajbjc,Cmd) then Inst_jajbjc;

if BenneVan(Str_loop,Cmd) then Inst_loop;

if BenneVan(Str_bsfbsr,Cmd) then Inst_bsfbsr;

if BenneVan(Str_movsxzx,Cmd) then Inst_movsxzx;

if BenneVan(Str_setasetb,Cmd) then Inst_setasetb;

if BenneVan(Str_btbtc,Cmd) then Inst_btbtc;

if BenneVan(Str_aadaam,Cmd) then Inst_aadaam;

if BenneVan(Str_clcstc,Cmd) then Inst_clcstc;

if BenneVan(Str_cbwcdq,Cmd) then Inst_cbwcdq;

if BenneVan(Str_intohlt,Cmd) then Inst_intohlt;

if BenneVan(Str_enter,Cmd) then Inst_enter;

if BenneVan(Str_cpuid,Cmd) then Inst_cpuid;

if BenneVan(Str_bound,Cmd) then Inst_bound;

if BenneVan(Str_shrdshld,Cmd) then Inst_shrdshld;

if BenneVan(Str_lockwait,Cmd) then Inst_lockwait;

if BenneVan(Str_verrverw1,Cmd) then Inst_verrverw1;
if BenneVan(Str_verrverw2,Cmd) then Inst_verrverw2;

if BenneVan(Str_arpl,Cmd) then Inst_arpl;

if BenneVan(Str_larlsl,Cmd) then Inst_larlsl;

if BenneVan(Str_bswap,Cmd) then Inst_bswap;

if BenneVan(Str_wbinvd,Cmd) then Inst_wbinvd;

if BenneVan(Str_invlpg,Cmd) then Inst_invlpg;

if BenneVan(Str_cmpxchgxadd,Cmd) then Inst_cmpxchgxadd;

if BenneVan(Str_imul,Cmd) then Inst_imul;

if BenneVan(Str_cmovacmovb,Cmd) then Inst_cmovacmovb;

if BenneVan(Str_ExtEsc,Cmd) then Inst_ExtEsc;


if (OutOk=False) then OutStr:='';
End;
