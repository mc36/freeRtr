Procedure CompileOneFpuLine;

Var
  Cmd:String;
  p1:OneParameterRec;
  p2:OneParameterRec;
  p3:OneParameterRec;
  Trns:Byte;


Procedure WriteDownShortOperands(i:LongInt);
Begin;
OutStr:=b2h($d8+i shr 8)+SepCh+b2h(i and $ff);
End;

Const
  Str_NoPerand2Siz='|f2xm1|fabs|fchs|fclex|fcos|fdecstp|feni|fdisi|'+
   'finit|fincstp|fld1|fldl2e|fldl2t|fldlg2|fldln2|fldpi|fldz|fnop|'+
   'fpatan|fprem|fprem1|fptan|frndint|fscale|fsetpm|fsin|fsincos|'+
   'fsqrt|ftst|fucompp|fxam|fxtract|fyl2x|fyl2xp1|';
Procedure Inst_NoPerand2Siz;
Var
  i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns<>0) then Exit;
i:=-1;
if (Cmd='f2xm1')   then i:=$1f0;
if (Cmd='fabs')    then i:=$1e1;
if (Cmd='fchs')    then i:=$1e0;
if (Cmd='fclex')   then i:=$3e2;
if (Cmd='fcos')    then i:=$1ff;
if (Cmd='fdecstp') then i:=$1f6;
if (Cmd='feni')    then i:=$3e0;
if (Cmd='fdisi')   then i:=$3e1;
if (Cmd='finit')   then i:=$3e3;
if (Cmd='fincstp') then i:=$1f7;
if (Cmd='fld1')    then i:=$1e8;
if (Cmd='fldl2e')  then i:=$1ea;
if (Cmd='fldl2t')  then i:=$1e9;
if (Cmd='fldlg2')  then i:=$1ec;
if (Cmd='fldln2')  then i:=$1ed;
if (Cmd='fldpi')   then i:=$1eb;
if (Cmd='fldz')    then i:=$1ee;
if (Cmd='fnop')    then i:=$1d0;
if (Cmd='fpatan')  then i:=$1f3;
if (Cmd='fprem')   then i:=$1f8;
if (Cmd='fprem1')  then i:=$1f5;
if (Cmd='fptan')   then i:=$1f2;
if (Cmd='frndint') then i:=$1fc;
if (Cmd='fscale')  then i:=$1fd;
if (Cmd='fsetpm')  then i:=$3e4;
if (Cmd='fsin')    then i:=$1fe;
if (Cmd='fsincos') then i:=$1fb;
if (Cmd='fsqrt')   then i:=$1fa;
if (Cmd='ftst')    then i:=$1e4;
if (Cmd='fucompp') then i:=$2e9;
if (Cmd='fxam')    then i:=$1e5;
if (Cmd='fxtract') then i:=$1f4;
if (Cmd='fyl2x')   then i:=$1f1;
if (Cmd='fyl2xp1') then i:=$1f9;
if (i=-1) then Exit;
WriteDownShortOperands(i);
OutOk:=True;
End;

Const
  Str_m32b_m64b='|fadd|fcom|fcomp|fdiv|fdivr|fmul|fst|fsub|fsubr|';
Procedure Inst_m32b_m64b;
Var
  i:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if not (Trns in [4,8]) then Exit;

i:=-1;
if (Cmd='fadd')  then i:=$00;
if (Cmd='fcom')  then i:=$02;
if (Cmd='fcomp') then i:=$03;
if (Cmd='fdiv')  then i:=$06;
if (Cmd='fdivr') then i:=$07;
if (Cmd='fmul')  then i:=$01;
if (Cmd='fst')   then i:=$12;
if (Cmd='fsub')  then i:=$04;
if (Cmd='fsubr') then i:=$05;
if (i=-1) then Exit;
if (Trns=8) then i:=i xor $40;

a:=ConvertAllMemoryDatas(p1.Mem,(i and 7) shl 3);
if (a='') then Exit;
OutStr:=b2h($d8+i shr 4)+SepCh+a;

a:=ConvertPointerSegRegs(p1.Mem,0);
OutStr:=a+SepCh+OutStr;

OutOk:=True;
End;

Const
  Str_m16b_m32b='|fiadd|ficom|ficomp|fidiv|fidivr|fimul|fist|fisub|fisubr|';
Procedure Inst_m16b_m32b;
Var
  i:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if not (Trns in [2,4]) then Exit;

i:=-1;
if (Cmd='fiadd')  then i:=$20;
if (Cmd='fimul')  then i:=$21;
if (Cmd='ficom')  then i:=$22;
if (Cmd='ficomp') then i:=$23;
if (Cmd='fisub')  then i:=$24;
if (Cmd='fisubr') then i:=$25;
if (Cmd='fidiv')  then i:=$26;
if (Cmd='fidivr') then i:=$27;
if (Cmd='fist')   then i:=$32;
if (i=-1) then Exit;
if (Trns=2) then i:=i xor $40;

a:=ConvertAllMemoryDatas(p1.Mem,(i and 7) shl 3);
if (a='') then Exit;
OutStr:=b2h($d8+i shr 4)+SepCh+a;

a:=ConvertPointerSegRegs(p1.Mem,0);
OutStr:=a+SepCh+OutStr;

OutOk:=True;
End;

Const
  Str_m16b_m32b_m64b='|fild|fistp|';
Procedure Inst_m16b_m32b_m64b;
Var
  i:LongInt;
  mem64b:Byte;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if not (Trns in [2,4,8]) then Exit;

i:=-1;mem64b:=$05;
if (Cmd='fild')  then i:=$70;
if (Cmd='fistp') then begin; i:=$73;mem64b:=$04; end;
if (i=-1) then Exit;
if (Trns=4) then i:=i xor $40;
if (Trns=8) then i:=i xor mem64b;

a:=ConvertAllMemoryDatas(p1.Mem,(i and 7) shl 3);
if (a='') then Exit;
OutStr:=b2h($d8+i shr 4)+SepCh+a;

a:=ConvertPointerSegRegs(p1.Mem,0);
OutStr:=a+SepCh+OutStr;

OutOk:=True;
End;

Const
  Str_m32b_m64b_m80b='|fld|fstp|';
Procedure Inst_m32b_m64b_m80b;
Var
  i:LongInt;
  mem80b:Byte;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if not (Trns in [4,8,10]) then Exit;

i:=-1;mem80b:=$25;
if (Cmd='fld')  then i:=$10;
if (Cmd='fstp') then begin; i:=$13;mem80b:=$24; end;
if (i=-1) then Exit;
if (Trns=8)  then i:=i xor $40;
if (Trns=10) then i:=i xor mem80b;

a:=ConvertAllMemoryDatas(p1.Mem,(i and 7) shl 3);
if (a='') then Exit;
OutStr:=b2h($d8+i shr 4)+SepCh+a;

a:=ConvertPointerSegRegs(p1.Mem,0);
OutStr:=a+SepCh+OutStr;

OutOk:=True;
End;


Const
  Str_m00b='|fbld|fbstp|fldcw|fldenv|frstor|fsave|fstcw|fstenv|fstsw|';
Procedure Inst_m00b;
Var
  i:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns<>0) then Exit;

i:=-1;
if (Cmd='fbld')   then i:=$74;
if (Cmd='fbstp')  then i:=$76;
if (Cmd='fldcw')  then i:=$15;
if (Cmd='fldenv') then i:=$14;
if (Cmd='frstor') then i:=$54;
if (Cmd='fsave')  then i:=$56;
if (Cmd='fstcw')  then i:=$17;
if (Cmd='fstenv') then i:=$16;
if (Cmd='fstsw')  then i:=$57;
if (i=-1) then Exit;

a:=ConvertAllMemoryDatas(p1.Mem,(i and 7) shl 3);
if (a='') then Exit;
OutStr:=b2h($d8+i shr 4)+SepCh+a;

a:=ConvertPointerSegRegs(p1.Mem,0);
OutStr:=a+SepCh+OutStr;

OutOk:=True;
End;

Const
  Str_stXsti_stiXst='|fadd|fdiv|fdivr|fmul|fsub|fsubr|';
Procedure Inst_stXsti_stiXst;
Const
  DirCode=$400;
  RevCode=$8;
Var
  i:LongInt;
  r:OneRegType;
  dir:Word;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>5) then Exit;
if (p2.Typ<>5) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns<>0) then Exit;

i:=-1;dir:=0;
if (Cmd='fadd')  then i:=$c0;
if (Cmd='fdiv')  then i:=$f0;
if (Cmd='fdivr') then i:=$f8;
if (Cmd='fmul')  then i:=$c8;
if (Cmd='fsub')  then i:=$e0;
if (Cmd='fsubr') then i:=$e8;
if (i=-1) then Exit;

if (p1.Reg.Num<>0) then begin;
  dir:=dir xor DirCode;
  r:=p2.Reg;
  p2.Reg:=p1.Reg;
  p1.Reg:=r;
  if (Cmd='fdiv')  then i:=i xor RevCode;
  if (Cmd='fdivr') then i:=i xor RevCode;
  if (Cmd='fsub')  then i:=i xor RevCode;
  if (Cmd='fsubr') then i:=i xor RevCode;
  end;
if (p1.Reg.Num<>0) then Exit;
r:=p2.Reg;

WriteDownShortOperands(i xor dir or r.Num);
OutOk:=True;
End;

Const
  Str_stiXst='|faddp|fcompp|fdivp|fdivrp|fmulp|fsubp|fsubrp|';
Procedure Inst_stiXst;
Var
  i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>5) then Exit;
if (p2.Typ<>5) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns<>0) then Exit;

if (p2.Reg.Num<>0) then Exit;

i:=-1;
if (Cmd='faddp')   then i:=$6c0;
if (Cmd='fcompp')  then i:=$6d8;
if (Cmd='fdivp')   then i:=$6f8;
if (Cmd='fdivrp')  then i:=$6f0;
if (Cmd='fmulp')   then i:=$6c8;
if (Cmd='fsubp')   then i:=$6e8;
if (Cmd='fsubrp')  then i:=$6e0;
if (i=-1) then Exit;

WriteDownShortOperands(i or p1.Reg.Num);
OutOk:=True;
End;

Const
  Str_stXsti='|fcomi|fcomip|fucomi|fucomip|';
Procedure Inst_stXsti;
Var
  i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>5) then Exit;
if (p2.Typ<>5) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns<>0) then Exit;

if (p1.Reg.Num<>0) then Exit;

i:=-1;
if (Cmd='fcomi')   then i:=$3f0;
if (Cmd='fcomip')  then i:=$7f0;
if (Cmd='fucomi')  then i:=$3e8;
if (Cmd='fucomip') then i:=$7e8;
if (i=-1) then Exit;

WriteDownShortOperands(i or p2.Reg.Num);
OutOk:=True;
End;


Const
   Str_sti='|fcom|fcomp|fcompp|ffree|fld|fst|fstp|fucom|fucomp|fxch|';
Procedure Inst_sti;
Label Folyt1;
Var
  i:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>5) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns<>0) then Exit;

i:=-1;
if (Cmd='fcom')   then i:=$0d0;
if (Cmd='fcomp')  then i:=$0d8;
if (Cmd='fcompp') then i:=$6d8;
if (Cmd='ffree')  then i:=$5c0;
if (Cmd='fld')    then i:=$1c0;
if (Cmd='fst')    then i:=$5d0;
if (Cmd='fstp')   then i:=$5d8;
if (Cmd='fucom')  then i:=$5e0;
if (Cmd='fucomp') then i:=$5e8;
if (Cmd='fxch')   then i:=$1c8;
if (i=-1) then Exit;

WriteDownShortOperands(i or p1.Reg.Num);
OutOk:=True;
End;

Const
  Str_fcmovxx='|fcmovc|fcmovnae|fcmovae|fcmovnc|fcmovz|fcmovnz|fcmovp'+
  '|fcmovpe|fcmovnp|fcmovpo|fcmovna|fcmova|fcmovb|fcmove'+
  '|fcmovbe|fcmovu|fcmovnb|fcmovne|fcmovnbe|fcmovnu|';
Procedure Inst_fcmovxx;
Const
  InstrBeg='fcmov';
Var
  i:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>5) then Exit;
if (p2.Typ<>5) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns<>0) then Exit;

if (p1.Reg.Num<>0) then Exit;
a:=Cmd;
if (Copy(a,1,Length(InstrBeg))<>InstrBeg) then Exit;
Delete(a,1,Length(InstrBeg));
i:=AnalizeFpuConditions(a);
if (i=-1) then Exit;
if (i>3) then inc(i,$1c);
i:=$2c0+(i shl 3);

WriteDownShortOperands(i or p2.Reg.Num);
OutOk:=True;
End;

Const
  Str_fstswax='|fstsw|';
Procedure Inst_fstswax;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
if (Trns<>0) then Exit;

if (All_RegsToQueue(p1.Reg)<>$30100) then Exit;

WriteDownShortOperands($7e0);
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
AnalizeParameter(a,1,p1);
b:=xLevesz(b);
i:=Pos(',',b+',');
a:=Copy(b,1,i-1);
b:=Copy(b,i+1,255);
AnalizeParameter(a,1,p2);
AnalizeParameter(b,1,p3);
OutStr:='er';
if (Cmd='') then Exit;

if BenneVan(Str_NoPerand2Siz,Cmd) then Inst_NoPerand2Siz;

if BenneVan(Str_m32b_m64b,Cmd) then Inst_m32b_m64b;
if BenneVan(Str_m16b_m32b,Cmd) then Inst_m16b_m32b;
if BenneVan(Str_m16b_m32b_m64b,Cmd) then Inst_m16b_m32b_m64b;
if BenneVan(Str_m32b_m64b_m80b,Cmd) then Inst_m32b_m64b_m80b;
if BenneVan(Str_m00b,Cmd) then Inst_m00b;

if BenneVan(Str_stXsti_stiXst,Cmd) then Inst_stXsti_stiXst;
if BenneVan(Str_stiXst,Cmd) then Inst_stiXst;
if BenneVan(Str_sti,Cmd) then Inst_sti;
if BenneVan(Str_stXsti,Cmd) then Inst_stXsti;

if BenneVan(Str_fcmovxx,Cmd) then Inst_fcmovxx;
if BenneVan(Str_fstswax,Cmd) then Inst_fstswax;



if (OutOk=False) then OutStr:='';
End;
