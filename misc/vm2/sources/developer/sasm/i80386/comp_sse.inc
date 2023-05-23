Procedure CompileOneSseLine;

Var
  Cmd:String;
  p1:OneParameterRec;
  p2:OneParameterRec;
  p3:OneParameterRec;
  Trns:Byte;



Procedure CompileNormalInstr(op,par,reg1,reg2:Integer);
Begin;
OutStr:=b2h($0f)+SepCh+b2h(op);
OutStr:=OutStr+SepCh+b2h(par+reg1 shl 3+reg2);
End;

Function CompileMemInstr(op:Byte;mem:OneMemPointRec;reg:OneRegType):Byte;
Var a:String;
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
  Str_mem='|fxrstor|fxsave|ldmxcsr|stmxcsr|prefetch0|prefetch1|prefetch2|'+
   'prefetchnta|clflush|';
Procedure Inst_mem;
Var
  i,o:LongInt;
  m:OneMemPointRec;
  r:OneRegType;
  a,c:String;
Begin;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
m:=p1.Mem;
r:=m.SegR;

i:=-1;o:=-1;
if (Cmd='fxsave')      then begin; i:=$ae;o:=$00; end;
if (Cmd='fxrstor')     then begin; i:=$ae;o:=$08; end;
if (Cmd='ldmxcsr')     then begin; i:=$ae;o:=$10; end;
if (Cmd='stmxcsr')     then begin; i:=$ae;o:=$18; end;
if (Cmd='clflush')     then begin; i:=$ae;o:=$38; end;
if (Cmd='prefetch0')   then begin; i:=$18;o:=$08; end;
if (Cmd='prefetch1')   then begin; i:=$18;o:=$10; end;
if (Cmd='prefetch2')   then begin; i:=$18;o:=$18; end;
if (Cmd='prefetchnta') then begin; i:=$18;o:=$00; end;
if (i=-1) or (o=-1) then Exit;

a:=ConvertPointerSegRegs(m,0);
if (a='') then Exit;
OutStr:=a+SepCh+b2h($0f)+SepCh;

a:=ConvertAllMemoryDatas(m,o);
if (a='') then Exit;
OutStr:=OutStr+b2h(i)+SepCh+a;

OutOk:=True;
End;



Const
  Str_none='|sfence|pause|lfence|mfence|mwait|monitor|';
Procedure Inst_none;
Var i:LongInt;
Begin;
if (p1.Typ<>0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;

i:=0;
if (Cmd='sfence')  then i:=$0faef8;
if (Cmd='pause')   then i:=$f390;
if (Cmd='lfence')  then i:=$0faee8;
if (Cmd='mfence')  then i:=$0faef0;
if (Cmd='mwait')   then i:=$0f01c9;
if (Cmd='monitor') then i:=$0f01c8;
if (i=0) then Exit;

OutStr:=b2h(i and $ff);
i:=i shr 8;
while (i<>0) do begin;
  OutStr:=b2h(i and $ff)+SepCh+OutStr;
  i:=i shr 8;
  end;
OutOk:=True;
End;




Const
  Str_mmxmmx_mmxmem1='|addpd|addps|addsd|addss|addsubpd|addsubps|andpd|'+
   'andps|andnpd|andnps|comisd|comiss|divpd|divps|divsd|divss|maxpd|maxps|'+
   'maxsd|maxss|minpd|minps|minsd|minss|movapd|movaps|movddup|movdqa|movdqu|'+
   'movdq2q|movhlps|movhps|movhpd|movlhps|movlps|movlpd|';
  Str_mmxmmx_mmxmem2='|cvtpi2ps|cvtps2pi|cvtsi2ss|cvtss2si|cvttps2pi|'+
   'cvttss2si||cvttpd2pi|cvttsd2si|cvttpd2dq|cvtpd2dq|cvtdq2pd|cvtps2dq|'+
   'cvtdq2ps|cvttps2dq|cvtpd2ps|cvtps2pd|cvtsd2ss|cvtss2sd|cvtpi2pd|'+
   'cvtsi2sd|cvtpd2pi|cvtsd2si|movq2dq|movsd|movss|movupd|';
  Str_mmxmmx_mmxmem3='|mulpd|mulps|mulsd|mulss|orps|orpd|rcpps|rcpss|'+
   'rsqrtps|rsqrtss|sqrtpd|sqrtps|sqrtsd|sqrtss|subpd|subps|subsd|subss|'+
   'ucomiss|ucomisd|unpckhpd|unpckhps|unpcklpd|unpcklps|xorps|xorpd|'+
   'pavgb|pavgw|pmaxsw|pmaxub|pminsw|pminub|pmulhuw|pmulhw|pmullw|';
  Str_mmxmmx_mmxmem4='|psadbw|maskmovq|maskmovdqu|packssdw|packsswb|'+
   'packuswb|punpckhbw|punpckhwd|punpckhdq|punpckhqdq|punpcklbw|punpcklwd|'+
   'punpckldq|punpcklqdq|haddps|haddpd|hsubps|hsubpd|lddqu|movshdup|'+
   'movsldup|movups|pmuludq|';
Procedure Inst_mmxmmx_mmxmem;
Var i:LongInt;
Begin;
if (p1.Typ<>6) then Exit;
if not (p2.Typ in [3,6]) then Exit;
if (p3.Typ<>0) then Exit;

i:=-1;
if (Cmd='addpd')      then i:=$6658;
if (Cmd='addps')      then i:=$0058;
if (Cmd='addsd')      then i:=$f258;
if (Cmd='addss')      then i:=$f358;
if (Cmd='addsubpd')   then i:=$66d0;
if (Cmd='addsubps')   then i:=$f2d0;
if (Cmd='andpd')      then i:=$6654;
if (Cmd='andps')      then i:=$0054;
if (Cmd='andnpd')     then i:=$6655;
if (Cmd='andnps')     then i:=$0055;
if (Cmd='comisd')     then i:=$662f;
if (Cmd='comiss')     then i:=$002f;
if (Cmd='cvttpd2pi')  then i:=$662c;
if (Cmd='cvttps2pi')  then i:=$002c;
if (Cmd='cvttsd2si')  then i:=$f22c;
if (Cmd='cvttss2si')  then i:=$f32c;
if (Cmd='cvttpd2dq')  then i:=$66e6;
if (Cmd='cvtpd2dq')   then i:=$f2e6;
if (Cmd='cvtdq2pd')   then i:=$f3e6;
if (Cmd='cvtps2dq')   then i:=$665b;
if (Cmd='cvtdq2ps')   then i:=$005b;
if (Cmd='cvttps2dq')  then i:=$f35b;
if (Cmd='cvtpd2ps')   then i:=$665a;
if (Cmd='cvtps2pd')   then i:=$005a;
if (Cmd='cvtsd2ss')   then i:=$f25a;
if (Cmd='cvtss2sd')   then i:=$f35a;
if (Cmd='cvtpi2pd')   then i:=$662a;
if (Cmd='cvtpi2ps')   then i:=$002a;
if (Cmd='cvtsi2sd')   then i:=$f22a;
if (Cmd='cvtsi2ss')   then i:=$f32a;
if (Cmd='cvtpd2pi')   then i:=$662d;
if (Cmd='cvtps2pi')   then i:=$002d;
if (Cmd='cvtsd2si')   then i:=$f22d;
if (Cmd='cvtss2si')   then i:=$f32d;
if (Cmd='divpd')      then i:=$665e;
if (Cmd='divps')      then i:=$005e;
if (Cmd='divsd')      then i:=$f25e;
if (Cmd='divss')      then i:=$f35e;
if (Cmd='maxpd')      then i:=$665f;
if (Cmd='maxps')      then i:=$005f;
if (Cmd='maxsd')      then i:=$f25f;
if (Cmd='maxss')      then i:=$f35f;
if (Cmd='minpd')      then i:=$665d;
if (Cmd='minps')      then i:=$005d;
if (Cmd='minsd')      then i:=$f25d;
if (Cmd='minss')      then i:=$f35d;
if (Cmd='movapd')     then i:=$6628;
if (Cmd='movaps')     then i:=$0028;
if (Cmd='movddup')    then i:=$f212;
if (Cmd='movdqa')     then i:=$666f;
if (Cmd='movdqu')     then i:=$f36f;
if (Cmd='movdq2q')    then i:=$f2d6;
if (Cmd='movhlps')    then i:=$0012;
if (Cmd='movhps')     then i:=$0016;
if (Cmd='movhpd')     then i:=$6616;
if (Cmd='movlhps')    then i:=$0016;
if (Cmd='movlps')     then i:=$0012;
if (Cmd='movlpd')     then i:=$6612;
if (Cmd='movshdup')   then i:=$f316;
if (Cmd='movsldup')   then i:=$f312;
if (Cmd='movq2dq')    then i:=$f3d6;
if (Cmd='movsd')      then i:=$f210;
if (Cmd='movss')      then i:=$f310;
if (Cmd='movupd')     then i:=$6610;
if (Cmd='movups')     then i:=$0010;
if (Cmd='mulpd')      then i:=$6659;
if (Cmd='mulps')      then i:=$0059;
if (Cmd='mulsd')      then i:=$f259;
if (Cmd='mulss')      then i:=$f359;
if (Cmd='orpd')       then i:=$6656;
if (Cmd='orps')       then i:=$0056;
if (Cmd='rcpps')      then i:=$0053;
if (Cmd='rcpss')      then i:=$f353;
if (Cmd='rsqrtps')    then i:=$0052;
if (Cmd='rsqrtss')    then i:=$f352;
if (Cmd='sqrtpd')     then i:=$6651;
if (Cmd='sqrtps')     then i:=$0051;
if (Cmd='sqrtsd')     then i:=$f251;
if (Cmd='sqrtss')     then i:=$f351;
if (Cmd='subpd')      then i:=$665c;
if (Cmd='subps')      then i:=$005c;
if (Cmd='subsd')      then i:=$f25c;
if (Cmd='subss')      then i:=$f35c;
if (Cmd='ucomisd')    then i:=$662e;
if (Cmd='ucomiss')    then i:=$002e;
if (Cmd='unpckhpd')   then i:=$6615;
if (Cmd='unpckhps')   then i:=$0015;
if (Cmd='unpcklpd')   then i:=$6614;
if (Cmd='unpcklps')   then i:=$0014;
if (Cmd='xorpd')      then i:=$6657;
if (Cmd='xorps')      then i:=$0057;
if (Cmd='pavgb')      then i:=$00e0;
if (Cmd='pavgw')      then i:=$00e3;
if (Cmd='pmaxsw')     then i:=$00ee;
if (Cmd='pmaxub')     then i:=$00de;
if (Cmd='pminsw')     then i:=$00ea;
if (Cmd='pminub')     then i:=$00da;
if (Cmd='pmulhuw')    then i:=$00e4;
if (Cmd='pmulhw')     then i:=$00e5;
if (Cmd='pmullw')     then i:=$00d5;
if (Cmd='pmuludq')    then i:=$00f4;
if (Cmd='psadbw')     then i:=$00f6;
if (Cmd='maskmovq')   then i:=$00f7;
if (Cmd='maskmovdqu') then i:=$66f7;
if (Cmd='packssdw')   then i:=$006b;
if (Cmd='packsswb')   then i:=$0063;
if (Cmd='packuswb')   then i:=$0067;
if (Cmd='punpckhbw')  then i:=$0068;
if (Cmd='punpckhwd')  then i:=$0069;
if (Cmd='punpckhdq')  then i:=$006a;
if (Cmd='punpckhqdq') then i:=$666d;
if (Cmd='punpcklbw')  then i:=$0060;
if (Cmd='punpcklwd')  then i:=$0061;
if (Cmd='punpckldq')  then i:=$0062;
if (Cmd='punpcklqdq') then i:=$666c;
if (Cmd='haddpd')     then i:=$667c;
if (Cmd='haddps')     then i:=$f27c;
if (Cmd='hsubpd')     then i:=$667d;
if (Cmd='hsubps')     then i:=$f27d;
if (Cmd='lddqu')      then i:=$f2f0;
if (i=-1) then Exit;

if (p2.Typ=6) then begin;
  CompileNormalInstr(i and $ff,$c0,p1.Reg.Num,p2.Reg.Num);
  end else begin;
  CompileMemInstr(i and $ff,p2.Mem,p1.Reg);
  end;
if (i and $ff00<>0) then OutStr:=b2h(i shr 8)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_mmxmmx_memmmx='|movapd|movaps|movdqa|movdqu|movhps|movhpd|movlps|'+
   'movlpd|movntdq|movntpd|movntps|movntq|movsd|movss|movupd|movups|';
Procedure Inst_mmxmmx_memmmx;
Var i:LongInt;
Begin;
if not (p1.Typ in [3,6]) then Exit;
if (p2.Typ<>6) then Exit;
if (p3.Typ<>0) then Exit;

i:=-1;
if (Cmd='movapd')  then i:=$6629;
if (Cmd='movaps')  then i:=$0029;
if (Cmd='movdqa')  then i:=$667f;
if (Cmd='movdqu')  then i:=$f37f;
if (Cmd='movhps')  then i:=$0017;
if (Cmd='movhpd')  then i:=$6617;
if (Cmd='movlps')  then i:=$0013;
if (Cmd='movlpd')  then i:=$6613;
if (Cmd='movntdq') then i:=$66e7;
if (Cmd='movntpd') then i:=$662b;
if (Cmd='movntps') then i:=$002b;
if (Cmd='movntq')  then i:=$00e7;
if (Cmd='movsd')   then i:=$f211;
if (Cmd='movss')   then i:=$f311;
if (Cmd='movupd')  then i:=$6611;
if (Cmd='movups')  then i:=$0011;
if (i=-1) then Exit;

if (p1.Typ=6) then begin;
  CompileNormalInstr(i and $ff,$c0,p2.Reg.Num,p1.Reg.Num);
  end else begin;
  CompileMemInstr(i and $ff,p1.Mem,p2.Reg);
  end;
if (i and $ff00<>0) then OutStr:=b2h(i shr 8)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_regmmx='|movmskpd|movmskps|pmovmskb|';
Procedure Inst_regmmx;
Var i:LongInt;
Begin;
if (p1.Typ<>1) then Exit;
if (p1.Reg.Len<>4) then Exit;
if (p2.Typ<>6) then Exit;
if (p3.Typ<>0) then Exit;

i:=-1;
if (Cmd='movmskpd') then i:=$6650;
if (Cmd='movmskps') then i:=$0050;
if (Cmd='pmovmskb') then i:=$00d7;
if (i=-1) then Exit;

OutStr:=b2h($0f)+SepCh+b2h(i and $ff)+SepCh+b2h($c0 or ((p1.Reg.Num-1) shl 3) or p2.Reg.Num);
if (i and $ff00<>0) then OutStr:=b2h(i shr 8)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_regmmximm='|pextrw|';
Procedure Inst_regmmximm;
Var
  i:LongInt;
  a:String;
Begin;
if (p1.Typ<>1) then Exit;
if (p1.Reg.Len<>4) then Exit;
if (p2.Typ<>6) then Exit;
if not (p3.Typ in [2,4]) then Exit;

i:=-1;
if (Cmd='pextrw') then i:=$00c5;
if (i=-1) then Exit;

OutStr:=b2h($0f)+SepCh+b2h(i and $ff)+SepCh+b2h($c0 or ((p1.Reg.Num-1) shl 3) or p2.Reg.Num);
a:=ConvertNumOrLabDatas(p3,1,'');
if (a='') then Exit;
OutStr:=OutStr+SepCh+a;
if (i and $ff00<>0) then OutStr:=b2h(i shr 8)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_mmxregimm_mmxmemimm='|pinsrw|';
Procedure Inst_mmxregimm_mmxmemimm;
Var
  i:LongInt;
  a:String;
Begin;
if (p1.Typ<>6) then Exit;
if not (p2.Typ in [1,3]) then Exit;
if not (p3.Typ in [2,4]) then Exit;

i:=-1;
if (Cmd='pinsrw') then i:=$00c4;
if (i=-1) then Exit;

if (p2.Typ=1) then begin;
  OutStr:=b2h($0f)+SepCh+b2h(i and $ff)+SepCh+b2h($c0 or (p1.Reg.Num shl 3) or (p2.Reg.Num-1));
  end else begin;
  CompileMemInstr(i and $ff,p2.Mem,p1.Reg);
  end;
a:=ConvertNumOrLabDatas(p3,1,'');
if (a='') then Exit;
OutStr:=OutStr+SepCh+a;
if (i and $ff00<>0) then OutStr:=b2h(i shr 8)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_mmxmmximm_mmxmemimm='|cmppd|cmpps|cmpsd|cmpss|shufpd|shufps|'+
   'pshufd|pshufhw|pshuflw|pshufw|';
Procedure Inst_mmxmmximm_mmxmemimm;
Var
  i:LongInt;
  a:String;
Begin;
if (p1.Typ<>6) then Exit;
if not (p2.Typ in [3,6]) then Exit;
if not (p3.Typ in [2,4]) then Exit;

i:=-1;
if (Cmd='cmppd')   then i:=$66c2;
if (Cmd='cmpps')   then i:=$00c2;
if (Cmd='cmpsd')   then i:=$f2c2;
if (Cmd='cmpss')   then i:=$f3c2;
if (Cmd='shufpd')  then i:=$66c6;
if (Cmd='shufps')  then i:=$00c6;
if (Cmd='pshufd')  then i:=$6670;
if (Cmd='pshufhw') then i:=$f370;
if (Cmd='pshuflw') then i:=$f270;
if (Cmd='pshufw')  then i:=$0070;
if (i=-1) then Exit;

if (p2.Typ=6) then begin;
  CompileNormalInstr(i and $ff,$c0,p1.Reg.Num,p2.Reg.Num);
  end else begin;
  CompileMemInstr(i and $ff,p2.Mem,p1.Reg);
  end;
a:=ConvertNumOrLabDatas(p3,1,'');
if (a='') then Exit;
OutStr:=OutStr+SepCh+a;
if (i and $ff00<>0) then OutStr:=b2h(i shr 8)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_memreg='|movnti|';
Procedure Inst_memreg;
Var
  i:LongInt;
  a:String;
Begin;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>1) then Exit;
if (p2.Reg.Len<>4) then Exit;
if (p3.Typ<>0) then Exit;

i:=-1;
if (Cmd='movnti')  then i:=$00c3;
if (i=-1) then Exit;

dec(p2.Reg.Num);
CompileMemInstr(i and $ff,p1.Mem,p2.Reg);
if (i and $ff00<>0) then OutStr:=b2h(i shr 8)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_m16_m32_m64='|fisttp|';
Procedure Inst_m16_m32_m64;
Var
  i:LongInt;
  a:String;
Begin;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if not (Trns in [2,4,8]) then Exit;

i:=-1;
if (Cmd='fisttp') then i:=1;
if (i=-1) then Exit;

a:=ConvertAllMemoryDatas(p1.Mem,i shl 3);
if (a='') then Exit;
case Trns of
  2:i:=$df;
  4:i:=$db;
  8:i:=$dd;
  else exit;
  end;
OutStr:=b2h(i)+SepCh+a;

a:=ConvertPointerSegRegs(p1.Mem,0);
OutStr:=a+SepCh+OutStr;

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

if BenneVan(Str_none,Cmd) then Inst_none;

if BenneVan(Str_mem,Cmd) then Inst_mem;

if BenneVan(Str_mmxmmx_mmxmem1,Cmd) then Inst_mmxmmx_mmxmem;
if BenneVan(Str_mmxmmx_mmxmem2,Cmd) then Inst_mmxmmx_mmxmem;
if BenneVan(Str_mmxmmx_mmxmem3,Cmd) then Inst_mmxmmx_mmxmem;
if BenneVan(Str_mmxmmx_mmxmem4,Cmd) then Inst_mmxmmx_mmxmem;

if BenneVan(Str_mmxmmx_memmmx,Cmd) then Inst_mmxmmx_memmmx;

if BenneVan(Str_regmmx,Cmd) then Inst_regmmx;

if BenneVan(Str_mmxregimm_mmxmemimm,Cmd) then Inst_mmxregimm_mmxmemimm;

if BenneVan(Str_regmmximm,Cmd) then Inst_regmmximm;

if BenneVan(Str_mmxmmximm_mmxmemimm,Cmd) then Inst_mmxmmximm_mmxmemimm;

if BenneVan(Str_memreg,Cmd) then Inst_memreg;

if BenneVan(Str_m16_m32_m64,Cmd) then Inst_m16_m32_m64;


if (OutOk=False) then OutStr:='';
End;
