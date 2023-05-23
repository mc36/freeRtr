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
  Str_simplify='|li|lis|extlwi|extrwi|rotlwi|rotrwi|slwi|srwi|crllwi|crlrwi|'+
  'crset|crnot|crclr|inslwi|insrwi|sub|sub.|subo|subo.|';
Procedure Inst_simplify;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (Cmd='li') or (Cmd='lis') then begin;
  Cmd:='ori'+copy(cmd,3,666);
  p4:=p3;
  p3:=p2;
  p2.Typ:=1;
  p2.Reg:=0;
  exit;
  end;
if (copy(Cmd,1,3)='sub') then begin;
  Cmd:='subf'+copy(Cmd,4,666);
  p6:=p2;
  p2:=p3;
  p3:=p6;
  exit;
  end;
if (Cmd='rotlwi') and (p3.Typ=2) then begin;
  p6:=p4;
  i:=BVal('$'+p3.Num);
  ParameterPutNumber(p3,i);
  ParameterPutNumber(p4,0);
  ParameterPutNumber(p5,31);
  Cmd:='rlwinm';
  exit;
  end;
if (Cmd='rotrwi') and (p3.Typ=2) then begin;
  p6:=p4;
  i:=BVal('$'+p3.Num);
  ParameterPutNumber(p3,32-i);
  ParameterPutNumber(p4,0);
  ParameterPutNumber(p5,31);
  Cmd:='rlwinm';
  exit;
  end;
if (Cmd='slwi') and (p3.Typ=2) then begin;
  p6:=p4;
  i:=BVal('$'+p3.Num);
  ParameterPutNumber(p3,i);
  ParameterPutNumber(p4,0);
  ParameterPutNumber(p5,31-i);
  Cmd:='rlwinm';
  exit;
  end;
if (Cmd='srwi') and (p3.Typ=2) then begin;
  p6:=p4;
  i:=BVal('$'+p3.Num);
  ParameterPutNumber(p3,32-i);
  ParameterPutNumber(p4,i);
  ParameterPutNumber(p5,31);
  Cmd:='rlwinm';
  exit;
  end;
if (Cmd='extlwi') and (p3.Typ=2) and (p4.Typ=2) then begin;
  p6:=p5;
  i:=BVal('$'+p3.Num);
  o:=BVal('$'+p4.Num);
  ParameterPutNumber(p3,o);
  ParameterPutNumber(p4,0);
  ParameterPutNumber(p5,i-1);
  Cmd:='rlwinm';
  exit;
  end;
if (Cmd='extrwi') and (p3.Typ=2) and (p4.Typ=2) then begin;
  p6:=p5;
  i:=BVal('$'+p3.Num);
  o:=BVal('$'+p4.Num);
  ParameterPutNumber(p3,i+o);
  ParameterPutNumber(p4,32-i);
  ParameterPutNumber(p5,31);
  Cmd:='rlwinm';
  exit;
  end;
if (Cmd='inslwi') and (p3.Typ=2) and (p4.Typ=2) then begin;
  p6:=p5;
  i:=BVal('$'+p3.Num);
  o:=BVal('$'+p4.Num);
  ParameterPutNumber(p3,32-o);
  ParameterPutNumber(p4,o);
  ParameterPutNumber(p5,o+i-1);
  Cmd:='rlwimi';
  exit;
  end;
if (Cmd='insrwi') and (p3.Typ=2) and (p4.Typ=2) then begin;
  p6:=p5;
  i:=BVal('$'+p3.Num);
  o:=BVal('$'+p4.Num);
  ParameterPutNumber(p3,32-o-i);
  ParameterPutNumber(p4,o);
  ParameterPutNumber(p5,o+i-1);
  Cmd:='rlwimi';
  exit;
  end;
if (Cmd='crllwi') and (p3.Typ=2) then begin;
  p6:=p4;
  i:=BVal('$'+p3.Num);
  ParameterPutNumber(p3,0);
  ParameterPutNumber(p4,i);
  ParameterPutNumber(p5,31);
  Cmd:='rlwinm';
  exit;
  end;
if (Cmd='crlrwi') and (p3.Typ=2) then begin;
  p6:=p4;
  i:=BVal('$'+p3.Num);
  ParameterPutNumber(p3,0);
  ParameterPutNumber(p4,0);
  ParameterPutNumber(p5,31-i);
  Cmd:='rlwinm';
  exit;
  end;
if (Cmd='crset') then begin;
  p4:=p2;
  p3:=p1;
  p2:=p1;
  Cmd:='creqv';
  exit;
  end;
if (Cmd='crnot') then begin;
  p4:=p2;
  p3:=p1;
  p2:=p1;
  Cmd:='crnor';
  exit;
  end;
if (Cmd='crclr') then begin;
  p4:=p2;
  p3:=p1;
  p2:=p1;
  Cmd:='crxor';
  exit;
  end;
End;


Const
  Str_add='|add|add.|addo|addo.|addc|addc.|addco|addco.|'+
          'adde|adde.|addeo|addeo.|divw|divw.|divwo|divwo.|'+
          'divwu|divwu.|divwuo|divwuo.|lbzux|lbzx|lhaux|lhax|'+
          'lhbrx|lhzux|lhzx|lswx|lwarx|lwbrx|lwzux|lwzx|eciwx|ecowx|';
Procedure Inst_add;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='add') then i:=$7c000214;
if (Cmd='add.') then i:=$7c000215;
if (Cmd='addo') then i:=$7c000614;
if (Cmd='addo.') then i:=$7c000615;
if (Cmd='addc') then i:=$7c000014;
if (Cmd='addc.') then i:=$7c000015;
if (Cmd='addco') then i:=$7c000414;
if (Cmd='addco.') then i:=$7c000415;
if (Cmd='adde') then i:=$7c000114;
if (Cmd='adde.') then i:=$7c000115;
if (Cmd='addeo') then i:=$7c000514;
if (Cmd='addeo.') then i:=$7c000515;
if (Cmd='divw') then i:=$7c0003d6;
if (Cmd='divw.') then i:=$7c0003d7;
if (Cmd='divwo') then i:=$7c0007d6;
if (Cmd='divwo.') then i:=$7c0007d7;
if (Cmd='divwu') then i:=$7c000396;
if (Cmd='divwu.') then i:=$7c000397;
if (Cmd='divwuo') then i:=$7c000796;
if (Cmd='divwuo.') then i:=$7c000797;
if (Cmd='lbzux') then i:=$7c0000ee;
if (Cmd='lbzx') then i:=$7c0000ae;
if (Cmd='lhaux') then i:=$7c0002ee;
if (Cmd='lhax') then i:=$7c0002ae;
if (Cmd='lhbrx') then i:=$7c00062c;
if (Cmd='lhzux') then i:=$7c00026e;
if (Cmd='lhzx') then i:=$7c00022e;
if (Cmd='lswx') then i:=$7c00042a;
if (Cmd='lwarx') then i:=$7c000028;
if (Cmd='lwbrx') then i:=$7c00042c;
if (Cmd='lwzux') then i:=$7c00006e;
if (Cmd='lwzx') then i:=$7c00002e;
if (Cmd='eciwx') then i:=$7c00026c;
if (Cmd='ecowx') then i:=$7c00036c;
if (i=0) then exit;
inc(i,p1.Reg shl 21);
inc(i,p2.Reg shl 16);
inc(i,p3.Reg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mulhw='|mulhw|mulhw.|mulhwu|mulhwu.|mullw|mullw.|mullwo|mullwo.|'+
          'stbux|stbx|sthbrx|sthux|sthx|stswx|stwbrx|stwcx.|stwux|stwx|'+
          'subf|subf.|subfo|subfo.|subfc|subfc.|subfco|subfco.|'+
          'subfe|subfe.|subfeo|subfeo.|';
Procedure Inst_mulhw;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='mulhw') then i:=$7c000096;
if (Cmd='mulhw.') then i:=$7c000097;
if (Cmd='mulhwu') then i:=$7c000016;
if (Cmd='mulhwu.') then i:=$7c000017;
if (Cmd='mullw') then i:=$7c0001d6;
if (Cmd='mullw.') then i:=$7c0001d7;
if (Cmd='mullwo') then i:=$7c0005d6;
if (Cmd='mullwo.') then i:=$7c0005d7;
if (Cmd='stbux') then i:=$7c0001ee;
if (Cmd='stbx') then i:=$7c0001ae;
if (Cmd='sthbrx') then i:=$7c00072c;
if (Cmd='sthux') then i:=$7c00036e;
if (Cmd='sthx') then i:=$7c00032e;
if (Cmd='stswx') then i:=$7c00052a;
if (Cmd='stwbrx') then i:=$7c00052c;
if (Cmd='stwcx.') then i:=$7c00012d;
if (Cmd='stwux') then i:=$7c00016e;
if (Cmd='stwx') then i:=$7c00012e;
if (Cmd='subf') then i:=$7c000050;
if (Cmd='subf.') then i:=$7c000051;
if (Cmd='subfo') then i:=$7c000450;
if (Cmd='subfo.') then i:=$7c000451;
if (Cmd='subfc') then i:=$7c000010;
if (Cmd='subfc.') then i:=$7c000011;
if (Cmd='subfco') then i:=$7c000410;
if (Cmd='subfco.') then i:=$7c000411;
if (Cmd='subfe') then i:=$7c000110;
if (Cmd='subfe.') then i:=$7c000111;
if (Cmd='subfeo') then i:=$7c000510;
if (Cmd='subfeo.') then i:=$7c000511;
if (i=0) then exit;
inc(i,p1.Reg shl 21);
inc(i,p2.Reg shl 16);
inc(i,p3.Reg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_addi='|addi|addic|addic.|addis|mulli|subfic|';
Procedure Inst_addi;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if not (p3.Typ in [2,4]) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
a:='';
if (Cmd='addi') then i:=$13800;
if (Cmd='addic') then i:=$3000;
if (Cmd='addic.') then i:=$3400;
if (Cmd='addis') then begin; i:=$13c00;a:=upperShifter; end;
if (Cmd='mulli') then i:=$1c00;
if (Cmd='subfic') then i:=$2000;
if (i=0) then exit;
if (i and $10000<>0) then if (p2.Reg=0) then exit;
inc(i,p1.Reg shl 5);
inc(i,p2.Reg);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+ConvertNumOrLabDatas(p3,2,a);
OutOk:=True;
End;


Const
  Str_addme='|addme|addme.|addmeo|addmeo.|addze|addze.|addzeo|addzeo.|'+
            'neg|neg.|nego|nego.|subfme|subfme.|subfmeo|subfmeo.|'+
            'subfze|subfze.|subfzeo|subfzeo.|';
Procedure Inst_addme;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='addme') then i:=$7c0001d4;
if (Cmd='addme.') then i:=$7c0001d5;
if (Cmd='addmeo') then i:=$7c0005d4;
if (Cmd='addmeo.') then i:=$7c0005d5;
if (Cmd='addze') then i:=$7c000194;
if (Cmd='addze.') then i:=$7c000195;
if (Cmd='addzeo') then i:=$7c000594;
if (Cmd='addzeo.') then i:=$7c000595;
if (Cmd='neg') then i:=$7c0000d0;
if (Cmd='neg.') then i:=$7c0000d1;
if (Cmd='nego') then i:=$7c0004d0;
if (Cmd='nego.') then i:=$7c0004d1;
if (Cmd='subfme') then i:=$7c0001d0;
if (Cmd='subfme.') then i:=$7c0001d1;
if (Cmd='subfmeo') then i:=$7c0005d0;
if (Cmd='subfmeo.') then i:=$7c0005d1;
if (Cmd='subfze') then i:=$7c000190;
if (Cmd='subfze.') then i:=$7c000191;
if (Cmd='subfzeo') then i:=$7c000590;
if (Cmd='subfzeo.') then i:=$7c000591;
if (i=0) then exit;
inc(i,p1.Reg shl 21);
inc(i,p2.Reg shl 16);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_and='|and|and.|andc|andc.|eqv|eqv.|nand|nand.|nor|nor.|or|or.|'+
          'orc|orc.|slw|slw.|sraw|sraw.|srw|srw.|xor|xor.|';
Procedure Inst_and;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='and') then i:=$7c000038;
if (Cmd='and.') then i:=$7c000039;
if (Cmd='andc') then i:=$7c000078;
if (Cmd='andc.') then i:=$7c000079;
if (Cmd='eqv') then i:=$7c000238;
if (Cmd='eqv.') then i:=$7c000239;
if (Cmd='nand') then i:=$7c0003b8;
if (Cmd='nand.') then i:=$7c0003b9;
if (Cmd='nor') then i:=$7c0000f8;
if (Cmd='nor.') then i:=$7c0000f9;
if (Cmd='or') then i:=$7c000378;
if (Cmd='or.') then i:=$7c000379;
if (Cmd='orc') then i:=$7c000338;
if (Cmd='orc.') then i:=$7c000339;
if (Cmd='slw') then i:=$7c000030;
if (Cmd='slw.') then i:=$7c000031;
if (Cmd='sraw') then i:=$7c000630;
if (Cmd='sraw.') then i:=$7c000631;
if (Cmd='srw') then i:=$7c000430;
if (Cmd='srw.') then i:=$7c000431;
if (Cmd='xor') then i:=$7c000278;
if (Cmd='xor.') then i:=$7c000279;
if (i=0) then exit;
inc(i,p1.Reg shl 16);
inc(i,p2.Reg shl 21);
inc(i,p3.Reg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_andi='|andi.|andis.|ori|oris|xori|xoris|';
Procedure Inst_andi;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if not (p3.Typ in [2,4]) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
a:='';
if (Cmd='andi.') then i:=$7000;
if (Cmd='andis.') then begin; i:=$7400;a:=upperShifter; end;
if (Cmd='ori') then i:=$6000;
if (Cmd='oris') then begin; i:=$6400;a:=upperShifter; end;
if (Cmd='xori') then i:=$6800;
if (Cmd='xoris') then begin; i:=$6c00;a:=upperShifter; end;
if (i=0) then exit;
if (i and $10000<>0) then if (p2.Reg=0) then exit;
inc(i,p1.Reg);
inc(i,p2.Reg shl 5);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+ConvertNumOrLabDatas(p3,2,a);
OutOk:=True;
End;


Const
  Str_cmp='|cmpw|cmpd|cmplw|cmpld|';
Procedure Inst_cmp;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>5) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='cmpw') then i:=$7c000000;
if (Cmd='cmpd') then i:=$7c200000;
if (Cmd='cmplw') then i:=$7c000040;
if (Cmd='cmpld') then i:=$7c200040;
if (i=0) then exit;
inc(i,p1.Cnd shl 23);
inc(i,p2.Reg shl 16);
inc(i,p3.Reg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_cmpi='|cmpwi|cmpdi|cmplwi|cmpldi|';
Procedure Inst_cmpi;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>5) then Exit;
if (p2.Typ<>1) then Exit;
if not (p3.Typ in [2,4]) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='cmpwi') then i:=$2c00;
if (Cmd='cmpdi') then i:=$2c20;
if (Cmd='cmplwi') then i:=$2800;
if (Cmd='cmpldi') then i:=$2820;
if (i=0) then exit;
inc(i,p1.Cnd shl 7);
inc(i,p2.Reg);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+ConvertNumOrLabDatas(p3,2,'');
OutOk:=True;
End;


Const
  Str_cntlzw='|cntlzw|cntlzw.|extsb|extsb.|extsh|extsh.|';
Procedure Inst_cntlzw;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='cntlzw') then i:=$7c000034;
if (Cmd='cntlzw.') then i:=$7c000035;
if (Cmd='extsb') then i:=$7c000774;
if (Cmd='extsb.') then i:=$7c000775;
if (Cmd='extsh') then i:=$7c000734;
if (Cmd='extsh.') then i:=$7c000735;
if (i=0) then exit;
inc(i,p1.Reg shl 16);
inc(i,p2.Reg shl 21);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_crand='|crand|crandc|creqv|crnand|crnor|cror|crorc|crxor|';
Procedure Inst_crand;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>2) then Exit;
if (p3.Typ<>2) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='crand') then i:=$4c000202;
if (Cmd='crandc') then i:=$4c000102;
if (Cmd='creqv') then i:=$4c000242;
if (Cmd='crnand') then i:=$4c0001c2;
if (Cmd='crnor') then i:=$4c000042;
if (Cmd='cror') then i:=$4c000382;
if (Cmd='crorc') then i:=$4c000342;
if (Cmd='crxor') then i:=$4c000182;
if (i=0) then exit;
o:=BVal('$'+p1.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 21);
o:=BVal('$'+p2.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 16);
o:=BVal('$'+p3.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_eieio='|eieio|isync|rfi|sc|sync|tlbia|tlbsync|';
Procedure Inst_eieio;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
i:=0;
if (Cmd='eieio') then i:=$7c0006ac;
if (Cmd='isync') then i:=$4c00012c;
if (Cmd='rfi') then i:=$4c000064;
if (Cmd='sc') then i:=$44000002;
if (Cmd='sync') then i:=$7c0004ac;
if (Cmd='tlbia') then i:=$7c0002e4;
if (Cmd='tlbsync') then i:=$7c00046c;
if (i=0) then exit;
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_icbi='|icbi|dcba|dcbf|dcbi|dcbst|dcbt|dcbtst|dcbz|';
Procedure Inst_icbi;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='icbi') then i:=$7c0007ac;
if (Cmd='dcba') then i:=$7c0005ec;
if (Cmd='dcbf') then i:=$7c0000ac;
if (Cmd='dcbi') then i:=$7c0003ac;
if (Cmd='dcbst') then i:=$7c00006c;
if (Cmd='dcbt') then i:=$7c00022c;
if (Cmd='dcbtst') then i:=$7c0001ec;
if (Cmd='dcbz') then i:=$7c0007ec;
if (i=0) then exit;
inc(i,p1.Reg shl 16);
inc(i,p2.Reg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_lbz='|lbz|lbzu|lha|lhau|lhz|lhzu|lmw|lwz|lwzu|stb|stbu|sth|sthu|'+
          'stmw|stw|stwu|';
Procedure Inst_lbz;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>3) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='lbz') then i:=$8800;
if (Cmd='lbzu') then i:=$8c00;
if (Cmd='lha') then i:=$a800;
if (Cmd='lhau') then i:=$ac00;
if (Cmd='lhz') then i:=$a000;
if (Cmd='lhzu') then i:=$a400;
if (Cmd='lmw') then i:=$b800;
if (Cmd='lwz') then i:=$8000;
if (Cmd='lwzu') then i:=$8400;
if (Cmd='stb') then i:=$9800;
if (Cmd='stbu') then i:=$9c00;
if (Cmd='sth') then i:=$b000;
if (Cmd='sthu') then i:=$b400;
if (Cmd='stmw') then i:=$bc00;
if (Cmd='stw') then i:=$9000;
if (Cmd='stwu') then i:=$9400;
if (i=0) then exit;
inc(i,p1.Reg shl 5);
inc(i,p2.Mem.Reg);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+ConvertAllMemoryDatas(p2,2);
OutOk:=True;
End;


Const
  Str_lswi='|lswi|stswi|';
Procedure Inst_lswi;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>2) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='lswi') then i:=$7c0004aa;
if (Cmd='stswi') then i:=$7c0005aa;
if (i=0) then exit;
inc(i,p1.Reg shl 21);
inc(i,p2.Reg shl 16);
o:=BVal('$'+p3.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mcrf='|mcrf|mcrfs|';
Procedure Inst_mcrf;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>5) then Exit;
if (p2.Typ<>5) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='mcrf') then i:=$4c000000;
if (Cmd='mcrfs') then i:=$fc000080;
if (i=0) then exit;
inc(i,p1.Cnd shl 23);
inc(i,p2.Cnd shl 18);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mcrxr='|mcrxr|';
Procedure Inst_mcrxr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>5) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='mcrxr') then i:=$7c000400;
if (i=0) then exit;
inc(i,p1.Cnd shl 23);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mfcr='|mfcr|mfmsr|mtmsr|';
Procedure Inst_mfcr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='mfcr') then i:=$7c000026;
if (Cmd='mfmsr') then i:=$7c0000a6;
if (Cmd='mtmsr') then i:=$7c000124;
if (i=0) then exit;
inc(i,p1.Reg shl 21);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_tlbie='|tlbie|';
Procedure Inst_tlbie;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='tlbie') then i:=$7c000264;
if (i=0) then exit;
inc(i,p1.Reg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mfspr='|mfspr|mftb|';
Procedure Inst_mfspr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
AnalizeSPRname(p2);
if (p1.Typ<>1) then Exit;
if (p2.Typ<>2) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='mfspr') then i:=$7c0002a6;
if (Cmd='mftb') then i:=$7c0002e6;
if (i=0) then exit;
inc(i,p1.Reg shl 21);
o:=BVal('$'+p2.Num);
if (o<0) or (o>1023) then exit;
inc(i,(o and $1f) shl 16);
inc(i,(o shr 5) shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mfsr='|mfsr|';
Procedure Inst_mfsr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>2) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='mfsr') then i:=$7c0004a6;
if (i=0) then exit;
inc(i,p1.Reg shl 21);
o:=BVal('$'+p2.Num);
if (o<0) or (o>15) then exit;
inc(i,o shl 16);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mtsr='|mtsr|';
Procedure Inst_mtsr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='mtsr') then i:=$7c0001a4;
if (i=0) then exit;
inc(i,p2.Reg shl 21);
o:=BVal('$'+p1.Num);
if (o<0) or (o>15) then exit;
inc(i,o shl 16);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mfsrin='|mfsrin|mtsrin|';
Procedure Inst_mfsrin;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='mfsrin') then i:=$7c000526;
if (Cmd='mtsrin') then i:=$7c0001e4;
if (i=0) then exit;
inc(i,p1.Reg shl 21);
inc(i,p2.Reg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mtcrf='|mtcrf|';
Procedure Inst_mtcrf;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='mtcrf') then i:=$7c000120;
if (i=0) then exit;
inc(i,p2.Reg shl 21);
o:=BVal('$'+p1.Num);
if (o<0) or (o>255) then exit;
inc(i,o shl 12);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mtspr='|mtspr|';
Procedure Inst_mtspr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
AnalizeSPRname(p1);
if (p1.Typ<>2) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='mtspr') then i:=$7c0003a6;
if (i=0) then exit;
inc(i,p2.Reg shl 21);
o:=BVal('$'+p1.Num);
if (o<0) or (o>1023) then exit;
inc(i,(o and $1f) shl 16);
inc(i,(o shr 5) shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_rlwimi='|rlwimi|rlwimi.|rlwinm|rlwinm.|';
Procedure Inst_rlwimi;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>2) then Exit;
if (p4.Typ<>2) then Exit;
if (p5.Typ<>2) then Exit;
if (p6.Typ<>0) then Exit;
i:=0;
if (Cmd='rlwimi') then i:=$50000000;
if (Cmd='rlwimi.') then i:=$50000001;
if (Cmd='rlwinm') then i:=$54000000;
if (Cmd='rlwinm.') then i:=$54000001;
if (i=0) then exit;
inc(i,p1.Reg shl 16);
inc(i,p2.Reg shl 21);
o:=BVal('$'+p3.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 11);
o:=BVal('$'+p4.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 6);
o:=BVal('$'+p5.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 1);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_rlwnm='|rlwnm|rlwnm.|';
Procedure Inst_rlwnm;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>2) then Exit;
if (p5.Typ<>2) then Exit;
if (p6.Typ<>0) then Exit;
i:=0;
if (Cmd='rlwnm') then i:=$5c000000;
if (Cmd='rlwnm.') then i:=$5c000001;
if (i=0) then exit;
inc(i,p1.Reg shl 16);
inc(i,p2.Reg shl 21);
inc(i,p3.Reg shl 11);
o:=BVal('$'+p4.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 6);
o:=BVal('$'+p5.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 1);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_srawi='|srawi|srawi.|stswi|';
Procedure Inst_srawi;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>2) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='srawi') then i:=$7c000670;
if (Cmd='srawi.') then i:=$7c000671;
if (Cmd='stswi') then i:=$7c0005aa;
if (i=0) then exit;
inc(i,p1.Reg shl 16);
inc(i,p2.Reg shl 21);
o:=BVal('$'+p3.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_tw='|tw|';
Procedure Inst_tw;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='tw') then i:=$7c000008;
if (i=0) then exit;
o:=BVal('$'+p1.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 21);
inc(i,p2.Reg shl 16);
inc(i,p3.Reg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_twi='|twi|';
Procedure Inst_twi;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>1) then Exit;
if not (p3.Typ in [2,4]) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='twi') then i:=$0c00;
if (i=0) then exit;
o:=BVal('$'+p1.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 5);
inc(i,p2.Reg);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+ConvertNumOrLabDatas(p3,2,'');
OutOk:=True;
End;


Const
  Str_bclr='|bcctr|bcctrl|bclr|bclrl|';
Procedure Inst_bclr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
AnalizeCondBO(p1);
AnalizeCondBI(p2);
if (p1.Typ<>2) then Exit;
if (p2.Typ<>2) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='bcctr') then i:=$4c000420;
if (Cmd='bcctrl') then i:=$4c000421;
if (Cmd='bclr') then i:=$4c000020;
if (Cmd='bclrl') then i:=$4c000021;
if (i=0) then exit;
o:=BVal('$'+p1.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 21);
o:=BVal('$'+p2.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 16);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_bcla='|bc|bcl|bca|bcla|';
Procedure Inst_bcla;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
AnalizeCondBO(p1);
AnalizeCondBI(p2);
if (p1.Typ<>2) then Exit;
if (p2.Typ<>2) then Exit;
if not (p3.Typ in [2,4]) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='bc') then i:=$40000000;
if (Cmd='bcl') then i:=$40000001;
if (Cmd='bca') then i:=$40000002;
if (Cmd='bcla') then i:=$40000003;
if (i=0) then exit;
o:=BVal('$'+p1.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 21);
o:=BVal('$'+p2.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 16);
OutStr:='';
case p3.Typ of
  2:OutStr:='numofs'+SpxCh+p3.Num;
  4:OutStr:='ofs?'+SpxCh+p3.Lab;
  end;
if (OutStr='') then exit;
if (i and 2=0) then OutStr:='rela2beg'+SpxCh+OutStr;
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+'word'+
        SpxCh+'and'+SpxCh+AnalizeNumbers('0fffch')+
        SpxCh+'or'+SpxCh+AnalizeNumbers(BStr(i and 255))+
        SpxCh+'msb'+SpxCh+OutStr;
OutOk:=True;
End;


Const
  Str_bla='|b|bl|ba|bla|';
Procedure Inst_bla;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='b') then i:=$48000000;
if (Cmd='bl') then i:=$48000001;
if (Cmd='ba') then i:=$48000002;
if (Cmd='bla') then i:=$48000003;
if (i=0) then exit;
OutStr:='';
case p1.Typ of
  2:OutStr:='numofs'+SpxCh+p1.Num;
  4:OutStr:='ofs?'+SpxCh+p1.Lab;
  end;
if (OutStr='') then exit;
if (i and 2=0) then OutStr:='rela2beg'+SpxCh+OutStr;
OutStr:='dword'+SpxCh+'and'+SpxCh+AnalizeNumbers('03fffffch')+
        SpxCh+'or'+SpxCh+AnalizeNumbers(BStr(i))+
        SpxCh+'msb'+SpxCh+OutStr;
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

if BenneVan(Str_simplify,Cmd) then Inst_simplify;
if BenneVan(Str_add,Cmd) then Inst_add;
if BenneVan(Str_mulhw,Cmd) then Inst_mulhw;
if BenneVan(Str_addi,Cmd) then Inst_addi;
if BenneVan(Str_addme,Cmd) then Inst_addme;
if BenneVan(Str_and,Cmd) then Inst_and;
if BenneVan(Str_andi,Cmd) then Inst_andi;
if BenneVan(Str_cmp,Cmd) then Inst_cmp;
if BenneVan(Str_cmpi,Cmd) then Inst_cmpi;
if BenneVan(Str_cntlzw,Cmd) then Inst_cntlzw;
if BenneVan(Str_crand,Cmd) then Inst_crand;
if BenneVan(Str_eieio,Cmd) then Inst_eieio;
if BenneVan(Str_icbi,Cmd) then Inst_icbi;
if BenneVan(Str_lbz,Cmd) then Inst_lbz;
if BenneVan(Str_lswi,Cmd) then Inst_lswi;
if BenneVan(Str_mcrf,Cmd) then Inst_mcrf;
if BenneVan(Str_mcrxr,Cmd) then Inst_mcrxr;
if BenneVan(Str_mfcr,Cmd) then Inst_mfcr;
if BenneVan(Str_tlbie,Cmd) then Inst_tlbie;
if BenneVan(Str_mfspr,Cmd) then Inst_mfspr;
if BenneVan(Str_mfsr,Cmd) then Inst_mfsr;
if BenneVan(Str_mtsr,Cmd) then Inst_mtsr;
if BenneVan(Str_mfsrin,Cmd) then Inst_mfsrin;
if BenneVan(Str_mtcrf,Cmd) then Inst_mtcrf;
if BenneVan(Str_mtspr,Cmd) then Inst_mtspr;
if BenneVan(Str_rlwimi,Cmd) then Inst_rlwimi;
if BenneVan(Str_rlwnm,Cmd) then Inst_rlwnm;
if BenneVan(Str_srawi,Cmd) then Inst_srawi;
if BenneVan(Str_tw,Cmd) then Inst_tw;
if BenneVan(Str_twi,Cmd) then Inst_twi;
if BenneVan(Str_bclr,Cmd) then Inst_bclr;
if BenneVan(Str_bcla,Cmd) then Inst_bcla;
if BenneVan(Str_bla,Cmd) then Inst_bla;

if (OutOk=False) then OutStr:='';
End;
