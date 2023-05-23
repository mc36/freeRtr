Procedure CompileOneMmxLine;

Var
  Cmd:String;
  p1:OneParameterRec;
  p2:OneParameterRec;
  p3:OneParameterRec;
  Trns:Byte;




Procedure ExChangeParams(var p1,p2:OneParameterRec);
Var p:OneParameterRec;
Begin;
p:=p1;
p1:=p2;
p2:=p;
End;

Procedure CompileNormalInstr(op,par,reg1,reg2:Integer);
Begin;
OutStr:=b2h($0f)+SepCh+b2h(op);
if (par<>-1) then begin;
  OutStr:=OutStr+SepCh+b2h(par+reg1 shl 3+reg2);
  end;
End;

Function CompileMemInstr(op:Byte;mem:OneMemPointRec;reg:OneRegType):Byte;
Var
  a:String;
Begin;
CompileMemInstr:=1;

OutStr:=b2h($0f)+SepCh+b2h(op);

a:=ConvertAllMemoryDatas(Mem,(Reg.Num) shl 3);
if (a='') then Exit;
OutStr:=OutStr+SepCh+a;

a:=ConvertPointerSegRegs(Mem,0);
OutStr:=a+SepCh+OutStr;

CompileMemInstr:=0;
End;



Const
  Str_Emms='|emms|';
Procedure Inst_Emms;
Begin;
CompileNormalInstr($77,-1,0,0);
OutOk:=True;
End;

Const
  Str_Movd='|movd|';
Procedure Inst_Movd;
Label i_MmxReg,i_MmxMem;
Var
  op:Byte;
  r:OneRegType;
  i:LongInt;
Begin;
op:=$6e;
if (p3.Typ<>0) then Exit;
if (p1.Typ<>6) then begin;
  ExChangeParams(p1,p2);
  Op:=Op xor $10;
  end;
if (p1.Typ<>6) then Exit;

if (p2.Typ=1) then Goto i_MmxReg;
if (p2.Typ=3) then Goto i_MmxMem;
Exit;

i_MmxReg:
r:=p2.Reg;
if (r.Len<>4) then Exit;
i:=b16_RegsToQueue(r);dec(i);
if (i<0) then Exit;
CompileNormalInstr(Op,$c0,p1.Reg.Num,i);
OutOk:=True;
Exit;

i_MmxMem:
CompileMemInstr(Op,p2.Mem,p1.Reg);
OutOk:=True;
Exit;
End;



Const
  Str_Movq='|movq|';
Procedure Inst_Movq;
Label i_MmxMmx,i_MmxMem;
Var
  op:Byte;
Begin;
op:=$6f;
if (p3.Typ<>0) then Exit;
if (p1.Typ<>6) then begin;
  ExChangeParams(p1,p2);
  Op:=Op xor $10;
  end;
if (p1.Typ<>6) then Exit;

if (p2.Typ=6) then Goto i_MmxMmx;
if (p2.Typ=3) then Goto i_MmxMem;
Exit;

i_MmxMmx:
if (Op and $10<>0) then ExChangeParams(p1,p2);
CompileNormalInstr(Op,$c0,p1.Reg.Num,p2.Reg.Num);
OutOk:=True;
Exit;

i_MmxMem:
CompileMemInstr(Op,p2.Mem,p1.Reg);
OutOk:=True;
Exit;
End;



Const
  Str_mmxmmx_mmxmem2='|packssdw|packsswb|packuswb|paddb|paddw|paddd|'+
   'paddsb|paddsw|paddusb|paddusw|pand|pandn|paddq|pmulhuw|pmuludq|'+
   'pcmpeqb|pcmpeqw|pcmpeqd|pcmpgtb|pcmpgtw|pcmpgtd|psubq|punpcklq|'+
   'pmaddwd|pmulhw|pmullw|por|pxor|punpckhq|';
Procedure Inst_mmxmmx_mmxmem2;
Var
  i:LongInt;
Begin;
if (p1.Typ<>6) then Exit;
if not (p2.Typ in [3,6]) then Exit;
if (p3.Typ<>0) then Exit;

i:=-1;
if (Cmd='packssdw') then i:=$6b;
if (Cmd='packsswb') then i:=$63;
if (Cmd='packuswb') then i:=$67;
if (Cmd='paddb')    then i:=$fc;
if (Cmd='paddw')    then i:=$fd;
if (Cmd='paddd')    then i:=$fe;
if (Cmd='paddq')    then i:=$d4;
if (Cmd='paddsb')   then i:=$ec;
if (Cmd='paddsw')   then i:=$ed;
if (Cmd='paddusb')  then i:=$dc;
if (Cmd='paddusw')  then i:=$dd;
if (Cmd='pand')     then i:=$db;
if (Cmd='pandn')    then i:=$df;
if (Cmd='pcmpeqb')  then i:=$74;
if (Cmd='pcmpeqw')  then i:=$75;
if (Cmd='pcmpeqd')  then i:=$76;
if (Cmd='pcmpgtb')  then i:=$64;
if (Cmd='pcmpgtw')  then i:=$65;
if (Cmd='pcmpgtd')  then i:=$66;
if (Cmd='pmaddwd')  then i:=$f5;
if (Cmd='pmulhuw')  then i:=$e4;
if (Cmd='pmulhw')   then i:=$e5;
if (Cmd='pmullw')   then i:=$d5;
if (Cmd='pmuludq')  then i:=$f4;
if (Cmd='por')      then i:=$eb;
if (Cmd='pxor')     then i:=$ef;
if (Cmd='psubq')    then i:=$fb;
if (Cmd='punpckhq') then i:=$6d;
if (Cmd='punpcklq') then i:=$6c;
if (i=-1) then Exit;

if (p2.Typ=6) then begin;
  CompileNormalInstr(i,$c0,p1.Reg.Num,p2.Reg.Num);
  end else begin;
  CompileMemInstr(i,p2.Mem,p1.Reg);
  end;
OutOk:=True;
End;



Const
  Str_mmxmmx_mmxmem_mmximm='|psllw|pslld|psllq|psraw|psrad|'+
   'psrlw|psrld|psrlq|pslldq|psrldq|';
Procedure Inst_mmxmmx_mmxmem_mmximm;
Label i_MmxMem,i_MmxMmx,i_MmxNum;
Var
  Cm,Gr:Byte;
  i:LongInt;
  a:String;
Begin;
if (p1.Typ<>6) then Exit;
if not (p2.Typ in [2,3,4,6]) then Exit;
if (p3.Typ<>0) then Exit;

i:=-1;a:=Copy(Cmd,1,Length(Cmd)-1);
if (a='pslld') then i:=7;
if (a='psll') then i:=6;
if (a='psra') then i:=4;
if (a='psrl') then i:=2;
if (a='psrld') then i:=3;
if (i=-1) then Exit;
Cm:=i;
i:=-1;a:=Copy(Cmd,Length(Cmd),1);
if (a='b') then i:=0;
if (a='w') then i:=1;
if (a='d') then i:=2;
if (a='q') then i:=3;
if (i=-1) then Exit;
Gr:=i;

if (p2.Typ=6) then Goto i_MmxMmx;
if (p2.Typ=3) then Goto i_MmxMem;
if (p2.Typ in [2,4]) then Goto i_MmxNum;
Exit;

i_MmxMmx:
CompileNormalInstr($c0+Cm shl 3+Gr,$c0,p1.Reg.Num,p2.Reg.Num);
OutOk:=True;
Exit;

i_MmxMem:
CompileMemInstr($c0+Cm shl 3+Gr,p2.Mem,p1.Reg);
OutOk:=True;
Exit;

i_MmxNum:
CompileNormalInstr($70+Gr,$c0,Cm,p1.Reg.Num);
a:=ConvertNumOrLabDatas(p2,1,'');
if (a='') then Exit;
OutStr:=OutStr+SepCh+a;
OutOk:=True;
Exit;

End;



Const
  Str_mmxmmx_mmxmem='|psubb|psubw|psubd|psubsb|psubsw|psubusb|psubusw|'+
   'punpcklb|punpcklw|punpckld|punpckhb|punpckhw|punpckhd|';
Procedure Inst_mmxmmx_mmxmem;
Label i_MmxMem,i_MmxMmx;
Var
  Cm,Gr:Byte;
  i:LongInt;
  a:String;
Begin;
if (p1.Typ<>6) then Exit;
if not (p2.Typ in [3,6]) then Exit;
if (p3.Typ<>0) then Exit;

i:=-1;a:=Copy(Cmd,1,Length(Cmd)-1);
if (a='psub')    then i:=$f8;
if (a='psubs')   then i:=$e8;
if (a='psubus')   then i:=$d8;
if (a='punpckh') then i:=$68;
if (a='punpckl') then i:=$60;
if (i=-1) then Exit;
Cm:=i;
i:=-1;a:=Copy(Cmd,Length(Cmd),1);
if (a='b') then i:=0;
if (a='w') then i:=1;
if (a='d') then i:=2;
if (a='q') then i:=3;
if (i=-1) then Exit;
Gr:=i;

if (p2.Typ=6) then Goto i_MmxMmx;
if (p2.Typ=3) then Goto i_MmxMem;
Exit;

i_MmxMmx:
CompileNormalInstr(Cm+Gr,$c0,p1.Reg.Num,p2.Reg.Num);
OutOk:=True;
Exit;

i_MmxMem:
CompileMemInstr(Cm+Gr,p2.Mem,p1.Reg);
OutOk:=True;
Exit;

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
Trns:=DecodeDataSizeName(Copy(b,1,i-1));
if (Trns<>0) then Delete(b,1,i);
b:=xLevesz(b);
i:=Pos(',',b+',');
a:=Copy(b,1,i-1);
b:=Copy(b,i+1,255);
AnalizeParameter(a,2,p1);
b:=xLevesz(b);
i:=Pos(',',b+',');
a:=Copy(b,1,i-1);
b:=Copy(b,i+1,255);
AnalizeParameter(a,2,p2);
AnalizeParameter(b,2,p3);
OutStr:='er';
if (Cmd='') then Exit;

if BenneVan(Str_Emms,Cmd) then Inst_Emms;
if BenneVan(Str_Movd,Cmd) then Inst_Movd;
if BenneVan(Str_Movq,Cmd) then Inst_Movq;

if BenneVan(Str_mmxmmx_mmxmem_mmximm,Cmd) then Inst_mmxmmx_mmxmem_mmximm;

if BenneVan(Str_mmxmmx_mmxmem,Cmd) then Inst_mmxmmx_mmxmem;

if BenneVan(Str_mmxmmx_mmxmem2,Cmd) then Inst_mmxmmx_mmxmem2;


if (OutOk=False) then OutStr:='';
End;
