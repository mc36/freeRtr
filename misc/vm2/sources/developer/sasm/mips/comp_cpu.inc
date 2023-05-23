Procedure CompileOneCpuLine;
Var
  Cmd:String;
  p1:OneParameterRec;
  p2:OneParameterRec;
  p3:OneParameterRec;
  p4:OneParameterRec;
  p5:OneParameterRec;
  p6:OneParameterRec;

Procedure getParam(var p:OneParameterRec;var b:String);
Var
  a:String;
  i:Longint;
Begin;
b:=xLevesz(b);
i:=Pos(',',b+',');
a:=Copy(b,1,i-1);
b:=Copy(b,i+1,255);
AnalizeParameter(a,p);
End;



Const
  Str_add='|add|addu|and|nor|or|slt|sltu|sub|subu|xor|dadd|daddu|dsub|dsubu|';
Procedure Inst_add;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='add') then i:=$00000020;
if (Cmd='addu') then i:=$00000021;
if (Cmd='and') then i:=$00000024;
if (Cmd='nor') then i:=$00000027;
if (Cmd='or') then i:=$00000025;
if (Cmd='slt') then i:=$0000002a;
if (Cmd='sltu') then i:=$0000002b;
if (Cmd='sub') then i:=$00000022;
if (Cmd='subu') then i:=$00000023;
if (Cmd='xor') then i:=$00000026;
if (Cmd='dadd') then i:=$0000002c;
if (Cmd='daddu') then i:=$0000002d;
if (Cmd='dsub') then i:=$0000002e;
if (Cmd='dsubu') then i:=$0000002f;
if (i=0) then exit;
inc(i,p1.Reg shl 11);
inc(i,p2.Reg shl 21);
inc(i,p3.Reg shl 16);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;



Const
  Str_addi='|addi|addiu|andi|ori|slti|sltiu|xori|daddi|daddiu|';
Procedure Inst_addi;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if not (p3.Typ in [2,3]) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='addi') then i:=$2000;
if (Cmd='addiu') then i:=$2400;
if (Cmd='andi') then i:=$3000;
if (Cmd='ori') then i:=$3400;
if (Cmd='slti') then i:=$2800;
if (Cmd='sltiu') then i:=$2c00;
if (Cmd='xori') then i:=$3800;
if (Cmd='daddi') then i:=$6000;
if (Cmd='daddiu') then i:=$6400;
if (i=0) then exit;
inc(i,p1.Reg);
inc(i,p2.Reg shl 5);
OutStr:=ConvertNumOrLabDatas(p3,2,'');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_break='|break|syscall|';
Procedure Inst_break;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='break') then i:=$000d;
if (Cmd='syscall') then i:=$000c;
if (i=0) then exit;
inc(i,BVal('$'+p1.num) shl 6);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;



Const
  Str_cop='|cop0|cop1|cop2|cop3|';
Procedure Inst_cop;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='cop0') then i:=$40;
if (Cmd='cop1') then i:=$44;
if (Cmd='cop2') then i:=$48;
if (Cmd='cop3') then i:=$4c;
if (i=0) then exit;
i:=(i shl 24)+BVal('$'+p1.num);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;



Const
  Str_beq='|beq|bne|beql|bnel|';
Procedure Inst_beq;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if not (p3.Typ in [2,3]) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='beq') then i:=$1000;
if (Cmd='beql') then i:=$5000;
if (Cmd='bne') then i:=$1400;
if (Cmd='bnel') then i:=$5400;
if (i=0) then exit;
inc(i,p1.Reg shl 5);
inc(i,p2.Reg);
OutStr:=ConvertAllMemoryDatas(p3,2,'rela2end');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_bgez='|bgez|bgezal|bgtz|blez|bltz|bltzal|bgezall|bgezl|bgtzl|blezl|'+
           'bltzall|bltzl|';
Procedure Inst_bgez;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [2,3]) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='bgez') then i:=$0401;
if (Cmd='bgezl') then i:=$0403;
if (Cmd='bgezal') then i:=$0411;
if (Cmd='bgezall') then i:=$0413;
if (Cmd='bgtz') then i:=$1c00;
if (Cmd='bgtzl') then i:=$5c00;
if (Cmd='blez') then i:=$1800;
if (Cmd='blezl') then i:=$5800;
if (Cmd='bltz') then i:=$0400;
if (Cmd='bltzl') then i:=$0402;
if (Cmd='bltzal') then i:=$0410;
if (Cmd='bltzall') then i:=$0412;
if (i=0) then exit;
inc(i,p1.Reg shl 5);
OutStr:=ConvertAllMemoryDatas(p2,2,'rela2end');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_teqi='|teqi|tgei|tgeiu|tlti|tltiu|tnei|';
Procedure Inst_teqi;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [2,3]) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='teqi') then i:=$040c;
if (Cmd='tgei') then i:=$0408;
if (Cmd='tgeiu') then i:=$0409;
if (Cmd='tlti') then i:=$040a;
if (Cmd='tltiu') then i:=$040b;
if (Cmd='tnei') then i:=$040e;
if (i=0) then exit;
inc(i,p1.Reg shl 5);
OutStr:=ConvertNumOrLabDatas(p2,2,'');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_div='|div|divu|mult|multu|teq|tge|tgeu|tlt|tltu|tne|ddiv|ddivu|'+
          'dmult|dmultu|';
Procedure Inst_div;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='div') then i:=$1a;
if (Cmd='divu') then i:=$1b;
if (Cmd='ddiv') then i:=$1e;
if (Cmd='ddivu') then i:=$1f;
if (Cmd='mult') then i:=$18;
if (Cmd='multu') then i:=$19;
if (Cmd='dmult') then i:=$1c;
if (Cmd='dmultu') then i:=$1d;
if (Cmd='teq') then i:=$34;
if (Cmd='tge') then i:=$30;
if (Cmd='tgeu') then i:=$31;
if (Cmd='tlt') then i:=$32;
if (Cmd='tltu') then i:=$33;
if (Cmd='tne') then i:=$36;
if (i=0) then exit;
inc(i,p1.Reg shl 21);
inc(i,p2.Reg shl 16);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;



Const
  Str_jal='|j|jal|';
Procedure Inst_jal;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,3]) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='j') then i:=$08;
if (Cmd='jal') then i:=$0c;
if (i=0) then exit;
OutStr:=ConvertAllMemoryDatas(p1,4,'and'+SpxCh+'03ffffff'+SpxCh+'or'+SpxCh+AnalizeNumbers(BStr(i shl 24)));
if (OutStr='') then exit;
OutOk:=True;
End;



Const
  Str_jalr='|jalr|';
Procedure Inst_jalr;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='jalr') then i:=$09;
if (i=0) then exit;
inc(i,p1.Reg shl 11);
inc(i,p2.Reg shl 21);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;



Const
  Str_jr='|jr|';
Procedure Inst_jr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='jr') then i:=$08;
if (i=0) then exit;
inc(i,p1.Reg shl 21);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;



Const
  Str_lb='|lb|lbu|lh|lhu|lw|lwc0|lwc1|lwc2|lwc3|lwl|lwr|sb|sh|sw|swc0|swc1|'+
         'swc2|swc3|swl|swr|ldc0|ldc1|ldc2|ldc3|ll|sc|sdc0|sdc1|sdc2|sdc3|'+
         'ld|ldl|ldr|lld|lwu|scd|sd|sdl|sdr|';
Procedure Inst_lb;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [2,3]) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='lb') then i:=$8000;
if (Cmd='lbu') then i:=$9000;
if (Cmd='lh') then i:=$8400;
if (Cmd='lhu') then i:=$9400;
if (Cmd='lw') then i:=$8c00;
if (Cmd='ll') then i:=$c000;
if (Cmd='lld') then i:=$d000;
if (Cmd='lwu') then i:=$9c00;
if (Cmd='ld') then i:=$dc00;
if (Cmd='ldl') then i:=$6800;
if (Cmd='ldr') then i:=$6c00;
if (Cmd='sc') then i:=$e000;
if (Cmd='scd') then i:=$f000;
if (Cmd='sd') then i:=$fc00;
if (Cmd='sdl') then i:=$b000;
if (Cmd='sdr') then i:=$b400;
if (Cmd='lwc0') then i:=$c000;
if (Cmd='lwc1') then i:=$c400;
if (Cmd='lwc2') then i:=$c800;
if (Cmd='lwc3') then i:=$cc00;
if (Cmd='ldc0') then i:=$d000;
if (Cmd='ldc1') then i:=$d400;
if (Cmd='ldc2') then i:=$d800;
if (Cmd='ldc3') then i:=$dc00;
if (Cmd='lwl') then i:=$8800;
if (Cmd='lwr') then i:=$9800;
if (Cmd='sb') then i:=$a000;
if (Cmd='sh') then i:=$a400;
if (Cmd='sw') then i:=$ac00;
if (Cmd='swc0') then i:=$e000;
if (Cmd='swc1') then i:=$e400;
if (Cmd='swc2') then i:=$e800;
if (Cmd='swc3') then i:=$ec00;
if (Cmd='sdc0') then i:=$f000;
if (Cmd='sdc1') then i:=$f400;
if (Cmd='sdc2') then i:=$f800;
if (Cmd='sdc3') then i:=$fc00;
if (Cmd='swl') then i:=$a800;
if (Cmd='swr') then i:=$b800;
if (i=0) then exit;
inc(i,p1.Reg);
inc(i,p3.Reg shl 5);
OutStr:=ConvertNumOrLabDatas(p2,2,'');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;


Const
  Str_pref='|pref|cache|';
Procedure Inst_pref;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if not (p2.Typ in [2,3]) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='pref') then i:=$cc00;
if (Cmd='cache') then i:=$bc00;
if (i=0) then exit;
inc(i,BVal('$'+p1.Num) and $1f);
inc(i,p3.Reg shl 5);
OutStr:=ConvertNumOrLabDatas(p2,2,'');
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_lui='|lui|';
Procedure Inst_lui;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if not (p2.Typ in [2,3]) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='lui') then i:=$3c00;
if (i=0) then exit;
inc(i,p1.Reg);
OutStr:=ConvertNumOrLabDatas(p2,2,upperShifter);
if (OutStr='') then exit;
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_mfhi='|mfhi|mflo|';
Procedure Inst_mfhi;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='mfhi') then i:=$0010;
if (Cmd='mflo') then i:=$0012;
if (i=0) then exit;
inc(i,p1.Reg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;



Const
  Str_mthi='|mthi|mtlo|';
Procedure Inst_mthi;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='mthi') then i:=$0011;
if (Cmd='mtlo') then i:=$0013;
if (i=0) then exit;
inc(i,p1.Reg shl 21);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;



Const
  Str_noop='|noop|sync|wait|tlbr|tlbp|tlbwi|tlbwr|eret|deret|';
Procedure Inst_noop;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
i:=-1;
if (Cmd='noop') then i:=$0;
if (Cmd='sync') then i:=$f;
if (Cmd='wait') then i:=$42000020;
if (Cmd='eret') then i:=$42000018;
if (Cmd='deret') then i:=$4200001f;
if (Cmd='tlbr') then i:=$42000001;
if (Cmd='tlbp') then i:=$42000008;
if (Cmd='tlbwi') then i:=$42000002;
if (Cmd='tlbwr') then i:=$42000006;
if (i<0) then exit;
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;



Const
  Str_sll='|sll|sra|srl|dsll|dsll32|dsra|dsra32|dsrl|dsrl32|';
Procedure Inst_sll;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>2) then Exit;
if (p4.Typ<>0) then Exit;
i:=-1;
if (Cmd='sll') then i:=$00;
if (Cmd='sra') then i:=$03;
if (Cmd='srl') then i:=$02;
if (Cmd='dsll') then i:=$38;
if (Cmd='dsll32') then i:=$3c;
if (Cmd='dsra') then i:=$3b;
if (Cmd='dsra32') then i:=$3f;
if (Cmd='dsrl') then i:=$3a;
if (Cmd='dsrl32') then i:=$3e;
if (i<0) then exit;
inc(i,p1.Reg shl 11);
inc(i,p2.Reg shl 16);
inc(i,BVal('$'+p3.num) shl 6);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;



Const
  Str_sllv='|sllv|srav|srlv|dsllv|dsrav|dsrlv|movn|movz|';
Procedure Inst_sllv;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='sllv') then i:=$04;
if (Cmd='srav') then i:=$07;
if (Cmd='srlv') then i:=$06;
if (Cmd='dsllv') then i:=$14;
if (Cmd='dsrav') then i:=$17;
if (Cmd='dsrlv') then i:=$16;
if (Cmd='movn') then i:=$0b;
if (Cmd='movz') then i:=$0a;
if (i=0) then exit;
inc(i,p1.Reg shl 11);
inc(i,p2.Reg shl 16);
inc(i,p3.Reg shl 21);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;



Const
  Str_mfc0='|mfc0|mfc1|mfc2|mfc3|mtc0|mtc1|mtc2|mtc3|dmfc0|dmfc1|dmfc2|'+
           'dmfc3|dmtc0|dmtc1|dmtc2|dmtc3|';
Procedure Inst_mfc0;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>2) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='mfc0') then i:=$40000000;
if (Cmd='mfc1') then i:=$44000000;
if (Cmd='mfc2') then i:=$48000000;
if (Cmd='mfc3') then i:=$4c000000;
if (Cmd='mtc0') then i:=$40800000;
if (Cmd='mtc1') then i:=$44800000;
if (Cmd='mtc2') then i:=$48800000;
if (Cmd='mtc3') then i:=$4c800000;
if (Cmd='dmfc0') then i:=$40200000;
if (Cmd='dmfc1') then i:=$44200000;
if (Cmd='dmfc2') then i:=$48200000;
if (Cmd='dmfc3') then i:=$4c200000;
if (Cmd='dmtc0') then i:=$40a00000;
if (Cmd='dmtc1') then i:=$44a00000;
if (Cmd='dmtc2') then i:=$48a00000;
if (Cmd='dmtc3') then i:=$4ca00000;
if (i=0) then exit;
inc(i,p1.Reg shl 16);
inc(i,(BVal('$'+p2.Num) and $1f) shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
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
getParam(p1,b);
getParam(p2,b);
getParam(p3,b);
getParam(p4,b);
getParam(p5,b);
AnalizeParameter(b,p6);
OutStr:='er';
if (Cmd='') then Exit;

if BenneVan(Str_add,Cmd) then Inst_add;
if BenneVan(Str_addi,Cmd) then Inst_addi;
if BenneVan(Str_break,Cmd) then Inst_break;
if BenneVan(Str_cop,Cmd) then Inst_cop;
if BenneVan(Str_beq,Cmd) then Inst_beq;
if BenneVan(Str_bgez,Cmd) then Inst_bgez;
if BenneVan(Str_div,Cmd) then Inst_div;
if BenneVan(Str_jal,Cmd) then Inst_jal;
if BenneVan(Str_jalr,Cmd) then Inst_jalr;
if BenneVan(Str_jr,Cmd) then Inst_jr;
if BenneVan(Str_lb,Cmd) then Inst_lb;
if BenneVan(Str_lui,Cmd) then Inst_lui;
if BenneVan(Str_mfhi,Cmd) then Inst_mfhi;
if BenneVan(Str_mthi,Cmd) then Inst_mthi;
if BenneVan(Str_noop,Cmd) then Inst_noop;
if BenneVan(Str_sll,Cmd) then Inst_sll;
if BenneVan(Str_sllv,Cmd) then Inst_sllv;
if BenneVan(Str_teqi,Cmd) then Inst_teqi;
if BenneVan(Str_pref,Cmd) then Inst_pref;
if BenneVan(Str_mfc0,Cmd) then Inst_mfc0;

if (OutOk=False) then OutStr:='';
End;
