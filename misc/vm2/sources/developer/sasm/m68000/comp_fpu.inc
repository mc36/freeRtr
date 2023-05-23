Procedure CompileOneFpuLine;
Var
  Cmd:String;
  p:LongInt;
  p1:OneParameterRec;
  p2:OneParameterRec;
  p3:OneParameterRec;


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
  Str_fabs='|fabs|fsabs|fdabs|facos|fasin|fasin|fatan|fatanh|fcos|fcosh|'+
           'fetox|fetoxm1|fgetexp|fgetman|fint|fintrz|flog10|flog2|flogn|'+
           'flognp1|fneg|fsneg|fdneg|fsin|fsinh|fsqrt|fssqrt|fdsqrt|ftan|'+
           'ftanh|ftentox|ftwotox|';
Procedure Inst_fabs;
Var i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='fabs') then i:=$f0000018;
if (Cmd='fsabs') then i:=$f0000058;
if (Cmd='fdabs') then i:=$f000005c;
if (Cmd='facos') then i:=$f000001c;
if (Cmd='fasin') then i:=$f000000c;
if (Cmd='fatan') then i:=$f000000a;
if (Cmd='fatanh') then i:=$f000000d;
if (Cmd='fcos') then i:=$f000001d;
if (Cmd='fcosh') then i:=$f0000019;
if (Cmd='fetox') then i:=$f0000010;
if (Cmd='fetoxm1') then i:=$f0000008;
if (Cmd='fgetexp') then i:=$f000001e;
if (Cmd='fgetman') then i:=$f000001f;
if (Cmd='fint') then i:=$f0000001;
if (Cmd='fintrz') then i:=$f0000003;
if (Cmd='flog10') then i:=$f0000015;
if (Cmd='flog2') then i:=$f0000016;
if (Cmd='flogn') then i:=$f0000014;
if (Cmd='flognp1') then i:=$f0000006;
if (Cmd='fneg') then i:=$f000001a;
if (Cmd='fsneg') then i:=$f000005a;
if (Cmd='fdneg') then i:=$f000005e;
if (Cmd='fsin') then i:=$f000000e;
if (Cmd='fsinh') then i:=$f0000002;
if (Cmd='fsqrt') then i:=$f0000004;
if (Cmd='fssqrt') then i:=$f0000041;
if (Cmd='fdsqrt') then i:=$f0000045;
if (Cmd='ftan') then i:=$f000000f;
if (Cmd='ftanh') then i:=$f0000009;
if (Cmd='ftentox') then i:=$f0000012;
if (Cmd='ftwotox') then i:=$f0000011;
if (i=0) then exit;
i:=i or CoprocessorId;
if (p1.Typ<>5) then begin;
  if (p2.Typ<>5) then exit;
  o:=ConvertAllMemoryDatas(p1,p,OutStr);
  if (o<0) then exit;
  i:=i or (o shl 16);
  i:=i or $4000;
  i:=i or ((p2.Frg.Num-1) shl 7);
  i:=i or (p shl 10);
  OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
if (p2.Typ=0) then p2:=p1;
if (p1.Typ<>5) then exit;
if (p2.Typ<>5) then exit;
i:=i or ((p1.Frg.Num-1) shl 10);
i:=i or ((p2.Frg.Num-1) shl 7);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_fadd='|fadd|fsadd|fdadd|fcmp|fdiv|fsdiv|fddiv|fmod|fmul|fsmul|fdmul|'+
           'frem|fscale|fsgldiv|fsglmul|fsub|fssub|fdsub|';
Procedure Inst_fadd;
Var i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='fadd') then i:=$f0000022;
if (Cmd='fsadd') then i:=$f0000062;
if (Cmd='fdadd') then i:=$f0000066;
if (Cmd='fcmp') then i:=$f0000038;
if (Cmd='fdiv') then i:=$f0000020;
if (Cmd='fsdiv') then i:=$f0000060;
if (Cmd='fddiv') then i:=$f0000064;
if (Cmd='fmod') then i:=$f0000021;
if (Cmd='fmul') then i:=$f0000023;
if (Cmd='fsmul') then i:=$f0000063;
if (Cmd='fdmul') then i:=$f0000067;
if (Cmd='frem') then i:=$f0000025;
if (Cmd='fscale') then i:=$f0000026;
if (Cmd='fsgldiv') then i:=$f0000024;
if (Cmd='fsglmul') then i:=$f0000027;
if (Cmd='fsub') then i:=$f0000028;
if (Cmd='fssub') then i:=$f0000068;
if (Cmd='fdsub') then i:=$f000006c;
if (i=0) then exit;
i:=i or CoprocessorId;
if (p1.Typ<>5) then begin;
  if (p2.Typ<>5) then exit;
  o:=ConvertAllMemoryDatas(p1,p,OutStr);
  if (o<0) then exit;
  i:=i or (o shl 16);
  i:=i or $4000;
  i:=i or ((p2.Frg.Num-1) shl 7);
  i:=i or (p shl 10);
  OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
if (p1.Typ<>5) then exit;
if (p2.Typ<>5) then exit;
i:=i or ((p1.Frg.Num-1) shl 10);
i:=i or ((p2.Frg.Num-1) shl 7);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_ftst='|ftst|';
Procedure Inst_ftst;
Var i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='ftst') then i:=$f000003a;
if (i=0) then exit;
i:=i or CoprocessorId;
if (p1.Typ<>5) then begin;
  o:=ConvertAllMemoryDatas(p1,p,OutStr);
  if (o<0) then exit;
  i:=i or (o shl 16);
  i:=i or $4000;
  i:=i or (p shl 10);
  OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
if (p1.Typ<>5) then exit;
i:=i or ((p1.Frg.Num-1) shl 10);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_fnop='|fnop|';
Procedure Inst_fnop;
Var i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
i:=0;
if (Cmd='fnop') then i:=$f0800000;
if (i=0) then exit;
i:=i or CoprocessorId;
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_fsincos='|fsincos|';
Procedure Inst_fsincos;
Var i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ=0) then Exit;
i:=0;
if (Cmd='fsincos') then i:=$f0000030;
if (i=0) then exit;
i:=i or CoprocessorId;
if (p1.Typ<>5) then begin;
  if (p2.Typ<>5) then exit;
  o:=ConvertAllMemoryDatas(p1,p,OutStr);
  if (o<0) then exit;
  i:=i or (o shl 16);
  i:=i or $4000;
  i:=i or (p2.Frg.Num-1);
  i:=i or ((p3.Frg.Num-1) shl 7);
  i:=i or (p shl 10);
  OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
if (p1.Typ<>5) then exit;
if (p2.Typ<>5) then exit;
i:=i or ((p1.Frg.Num-1) shl 10);
i:=i or (p2.Frg.Num-1);
i:=i or ((p3.Frg.Num-1) shl 7);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_fmove='|fmove|fsmove|fdmove|';
Procedure Inst_fmove;
Var i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='fmove') then i:=$f0000000;
if (Cmd='fsmove') then i:=$f0000040;
if (Cmd='fdmove') then i:=$f0000044;
if (i=0) then exit;
i:=i or CoprocessorId;
if (p1.Typ<>5) then begin;
  if (p2.Typ<>5) then exit;
  o:=ConvertAllMemoryDatas(p1,p,OutStr);
  if (o<0) then exit;
  i:=i or (o shl 16);
  i:=i or $4000;
  i:=i or ((p2.Frg.Num-1) shl 7);
  i:=i or (p shl 10);
  OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
if (p1.Typ<>5) then exit;
if (p2.Typ=5) then exit;
o:=ConvertAllMemoryDatas(p2,p,OutStr);
if (o<0) then exit;
i:=i or (o shl 16);
i:=i or $6000;
i:=i or ((p1.Frg.Num-1) shl 7);
i:=i or (p shl 10);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_fmove2='|fmove|';
Procedure Inst_fmove2;
Var i,o:LongInt;

Function decod(a:string):LongInt;
Var i:LongInt;
Begin;
i:=0;
if (a='fpcr') then i:=4;
if (a='fpsr') then i:=2;
if (a='fpiar') then i:=1;
decod:=i;
End;

Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
p:=decod(p2.Str);
if (p<>0) then begin;
  o:=ConvertAllMemoryDatas(p1,2,OutStr);
  if (o<0) then exit;
  i:=(o shl 16) or CoprocessorId;
  i:=i or $f0008000;
  i:=i or (p shl 10);
  OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
p:=decod(p1.Str);
o:=ConvertAllMemoryDatas(p2,2,OutStr);
if (o<0) then exit;
i:=(o shl 16) or CoprocessorId;
i:=i or $f000a000;
i:=i or (p shl 10);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_fmovecr='|fmovecr|';
Procedure Inst_fmovecr;
Var i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>5) then Exit;
if (p3.Typ<>0) then Exit;
i:=BVal('$'+p1.Num);
if (i<0) or (i>127) then exit;
i:=((p2.Frg.Num-1) shl 7) or i;
i:=i or CoprocessorId;
i:=i or $f0005c00;
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_fmovem='|fmovem|';
Procedure Inst_fmovem;
Var i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p3.Typ<>0) then Exit;
if (p1.Typ=1) then begin;
  o:=ConvertAllMemoryDatas(p2,2,OutStr);
  if (o<0) then exit;
  i:=(o shl 16) or CoprocessorId;
  i:=i or $f000f800;
  i:=i or ((p1.Reg.Num-1) shl 4);
  OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
if (p2.Typ=1) then begin;
  o:=ConvertAllMemoryDatas(p1,2,OutStr);
  if (o<0) then exit;
  i:=(o shl 16) or CoprocessorId;
  i:=i or $f000d800;
  i:=i or ((p2.Reg.Num-1) shl 4);
  OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
if (p1.Typ=0) then begin;
  i:=AnalizeFpuRegList(p1.Str);
  if (i<0) then exit;
  o:=ConvertAllMemoryDatas(p2,2,OutStr);
  if (o<0) then exit;
  i:=i or (o shl 16);
  i:=i or CoprocessorId;
  i:=i or $f000f000;
  OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
  OutOk:=True;
  exit;
  exit;
  end;
if (p2.Typ=0) then begin;
  i:=AnalizeFpuRegList(p2.Str);
  if (i<0) then exit;
  o:=ConvertAllMemoryDatas(p1,2,OutStr);
  if (o<0) then exit;
  i:=i or (o shl 16);
  i:=i or CoprocessorId;
  i:=i or $f000d000;
  OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
  OutOk:=True;
  exit;
  end;
End;


Const
  Str_fbcc='|fbeq|fbne|fbgt|fbngt|fbge|fbnge|fblt|fbnlt|fble|fbnle|fbgl|'+
           'fbngl|fbgle|fbngle|fbogt|fbule|fboge|fbult|fbolt|fbuge|fbole|'+
           'fbugt|fbogl|fbueq|fbor|fbun|fbf|fbt|fbsf|fbst|fbseq|fbsne|';
Procedure Inst_fbcc;
Var i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>0) then exit;
if (p=0) then p:=4 else p:=2;
i:=AnalizeFpuCond(copy(Cmd,3,255));
if (i<0) then exit;
i:=(i shl 16);
i:=i or CoprocessorId;
i:=i or $f0800000;
if (p>2) then i:=i or $400000;
case p1.Typ of
  2:OutStr:='numofs'+SpxCh+p1.Num;
  4:OutStr:='ofs?'+SpxCh+p1.Lab;
  else exit;
  end;
OutStr:='rela2beg'+SpxCh+'add'+SpxCh+AnalizeNumbers('-2')+SpxCh+DataSizeNames[p]+SpxCh+'msb'+SpxCh+OutStr;
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_fdbcc='|fdbeq|fdbne|fdbgt|fdbngt|fdbge|fdbnge|fdblt|fdbnlt|fdble|'+
           'fdbnle|fdbgl|fdbngl|fdbgle|fdbngle|fdbogt|fdbule|fdboge|fdbult|'+
           'fdbolt|fdbuge|fdbole|fdbugt|fdbogl|fdbueq|fdbor|fdbun|fdbf|fdbt|'+
           'fdbsf|fdbst|fdbseq|fdbsne|';
Procedure Inst_fdbcc;
Var i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [2,4]) then Exit;
if (p3.Typ<>0) then exit;
if (p=0) then p:=4 else p:=2;
i:=AnalizeFpuCond(copy(Cmd,4,255));
if (i<0) then exit;
i:=i or CoprocessorId;
i:=i or $f0480000;
o:=p1.Reg.Num-1;
if (o<0) or (o>7) then exit;
i:=i or (o shl 16);
case p2.Typ of
  2:OutStr:='numofs'+SpxCh+p2.Num;
  4:OutStr:='ofs?'+SpxCh+p2.Lab;
  else exit;
  end;
OutStr:='rela2beg'+SpxCh+'add'+SpxCh+AnalizeNumbers('-2')+SpxCh+DataSizeNames[p]+SpxCh+'msb'+SpxCh+OutStr;
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_fscc='|fseq|fsne|fsgt|fsngt|fsge|fsnge|fslt|fsnlt|fsle|'+
           'fsnle|fsgl|fsngl|fsgle|fsngle|fsogt|fsule|fsoge|fsult|'+
           'fsolt|fsuge|fsole|fsugt|fsogl|fsueq|fsor|fsun|fsf|fst|'+
           'fssf|fsst|fsseq|fssne|';
Procedure Inst_fscc;
Var i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>0) then exit;
i:=AnalizeFpuCond(copy(Cmd,3,255));
if (i<0) then exit;
i:=i or CoprocessorId;
i:=i or $f0500000;
o:=ConvertAllMemoryDatas(p1,2,OutStr);
if (o<0) then exit;
i:=i or (o shl 16);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_ftrapcc1='|ftrapeq|ftrapne|ftrapgt|ftrapngt|ftrapge|ftrapnge|ftraplt|'+
               'ftrapnlt|ftraple|ftrapnle|ftrapgl|ftrapngl|ftrapgle|'+
               'ftrapngle|ftrapogt|ftrapule|ftrapoge|ftrapult|ftrapolt|';
  Str_ftrapcc2='|ftrapuge|ftrapole|ftrapugt|ftrapogl|ftrapueq|ftrapor|'+
               'ftrapun|ftrapf|ftrapt|ftrapsf|ftrapst|ftrapseq|ftrapsne|';
Procedure Inst_ftrapcc;
Var i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p2.Typ<>0) then exit;
i:=AnalizeFpuCond(copy(Cmd,6,255));
if (i<0) then exit;
i:=i or CoprocessorId;
i:=i or $f0700000;
if (p1.Typ=0) then begin;
  i:=i or $c0000;
  OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
  OutOk:=True;
  exit;
  end;
if (p=0) then begin;
  i:=i or $b0000;
  p:=4;
  end else begin;
  i:=i or $a0000;
  p:=2;
  end;
OutStr:=ConvertNumOrLabDatas(p1,p,p,'');
if (OutStr='') then exit;
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
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
nextParam(a,b);
AnalizeParameter(a,1,p1);
nextParam(a,b);
AnalizeParameter(a,1,p2);
AnalizeParameter(b,1,p3);
OutStr:='er';
p:=AnalizeSizePostfix2(Cmd,2);
if (Cmd='') then Exit;

if BenneVan(Str_fabs,Cmd) then Inst_fabs;
if BenneVan(Str_fadd,Cmd) then Inst_fadd;
if BenneVan(Str_ftst,Cmd) then Inst_ftst;
if BenneVan(Str_fnop,Cmd) then Inst_fnop;
if BenneVan(Str_fsincos,Cmd) then Inst_fsincos;
if BenneVan(Str_fmove,Cmd) then Inst_fmove;
if BenneVan(Str_fmove2,Cmd) then Inst_fmove2;
if BenneVan(Str_fmovecr,Cmd) then Inst_fmovecr;
if BenneVan(Str_fmovem,Cmd) then Inst_fmovem;
if BenneVan(Str_fbcc,Cmd) then Inst_fbcc;
if BenneVan(Str_fdbcc,Cmd) then Inst_fdbcc;
if BenneVan(Str_fscc,Cmd) then Inst_fscc;
if BenneVan(Str_ftrapcc1,Cmd) then Inst_ftrapcc;
if BenneVan(Str_ftrapcc2,Cmd) then Inst_ftrapcc;

if (OutOk=False) then OutStr:='';
End;
