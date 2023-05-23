Procedure CompileOneCpuLine;
Var
  Cmd:String;
  p1:OneParameterRec;
  p2:OneParameterRec;
  p3:OneParameterRec;
  p4:OneParameterRec;
  p5:OneParameterRec;
  p6:OneParameterRec;
  p7:OneParameterRec;

Procedure getParam(var p:OneParameterRec;var b:String);
Label f1;
Var
  a:String;
  i:Longint;
Begin;
b:=xLevesz(b);
i:=Pos(',',b+',');
a:=Copy(b,1,i-1);
b:=Copy(b,i+1,255);
if (copy(a,1,1)<>'[') then goto f1;
i:=pos(']',b);
if (i<1) then begin; a:=a+b;b:='';goto f1; end;
a:=a+','+Copy(b,1,i);
b:=Copy(b,i+1,255);
i:=Pos(',',b+',');
a:=a+Copy(b,1,i-1);
b:=Copy(b,i+1,255);
f1:
AnalizeParameter(a,p);
End;

Procedure addParam(var a:String;p:OneParameterRec);
Begin;
if (p.str='') then exit;
a:=a+','+p.str;
End;

Procedure xchgParam(var p1,p2:OneParameterRec);
Var p:OneParameterRec;
Begin;
p:=p1;
p1:=p2;
p2:=p;
End;


Const
  Str_adc='|adc|add|and|bic|eor|mov|mvn|orr|rsb|rsc|sbc|sub|';
Procedure Inst_adc;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
{if (p3.Typ=0) then Exit;}
if (p4.Typ<>0) then Exit;
c:=cmd;
o:=AnalizeCondS(c,3);
i:=-1;
if (c='adc') then i:=$00a0;
if (c='add') then i:=$0080;
if (c='and') then i:=$0000;
if (c='bic') then i:=$01c0;
if (c='eor') then i:=$0020;
if (c='mov') then i:=$01a0;
if (c='mvn') then i:=$01e0;
if (c='orr') then i:=$0180;
if (c='rsb') then i:=$0060;
if (c='rsc') then i:=$00e0;
if (c='sbc') then i:=$00c0;
if (c='sub') then i:=$0040;
if (i<0) then exit;
i:=(i+o) shl 16;
inc(i,p1.Reg shl 12);
inc(i,p2.Reg shl 16);
OutStr:=ConvertMemPointData(p3.Str,i);
if (OutStr='') then exit;
OutOk:=True;
End;



Const
  Str_bbl='|b|bl|';
Procedure Inst_bbl;
Var
  i,o,p:LongInt;
  a,c:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,3]) then Exit;
if (p2.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 4;
i:=0;
if (c='b') then i:=$0a;
if (c='bl') then i:=$0b;
if (c='blx') then begin; i:=$fa;o:=0; end;
if (c='blxh') then begin; i:=$fb;o:=0; end;
if (i=0) then exit;
i:=(i+o) shl 24;
case p1.Typ of
  2:a:='numofs'+SpxCh+p1.Num;  {Num}
  3:a:='ofs'+SpxCh+p1.Lab; {Label}
  else exit;
  end;
OutStr:='dword'+SpxCh+'rela2beg'+SpxCh+'shr'+SpxCh+'2'+SpxCh+'and'+SpxCh+'FFFFFF'+SpxCh;
OutStr:=OutStr+'add'+SpxCh+'FFFFFFF8'+SpxCh+'or'+SpxCh+AnalizeNumbers(bstr(i))+SpxCh+a;
OutOk:=True;
End;


Const
  Str_bkpt='|bkpt|';
Procedure Inst_bkpt;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='bkpt') then i:=$e1200070;
if (i=0) then exit;
o:=BVal('$'+p1.Num);
inc(i,o and $f);
inc(i,((o shr 4) and $fff) shl 8);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
OutOk:=True;
End;


Const
  Str_blx='|blx|bx|bxj|';
Procedure Inst_blx;
Var
  i,o,p:LongInt;
  a,c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 28;
i:=0;
if (c='blx') then i:=$012fff30;
if (c='bx') then i:=$012fff10;
if (c='bxj') then i:=$012fff20;
if (i=0) then exit;
inc(i,o);
inc(i,p1.reg);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
OutOk:=True;
End;



Const
  Str_cdp='|cdp|';
Procedure Inst_cdp;
Var
  i,o,p:LongInt;
  a,c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>2) then Exit;
if (p3.Typ<>4) then Exit;
if (p4.Typ<>4) then Exit;
if (p5.Typ<>4) then Exit;
if (p6.Typ<>2) then Exit;
if (p7.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 4;
i:=0;
if (c='cdp') then i:=$0e+o;
if (c='cdp2') then i:=$fe;
if (i=0) then exit;
i:=i shl 24;
inc(i,(BVal('$'+p2.Num) and $f) shl 20);
inc(i,p4.Rg2 shl 16);
inc(i,p3.Rg2 shl 12);
AnalizeFpuNums(p1.Str,o);
if (o<0) then exit;
inc(i,o shl 8);
inc(i,(BVal('$'+p6.Num) and 7) shl 5);
inc(i,p5.Rg2);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
OutOk:=True;
End;



Const
  Str_clz='|clz|cpy|rev|sxt|uxt|';
Procedure Inst_clz;
Var
  i,o,p:LongInt;
  a,c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 28;
i:=0;
if (c='clz') then i:=$016f0f10;
if (c='cpy') then i:=$01a00000;
if (c='rev') then i:=$06bf0f30;
if (c='rev16') then i:=$06bf0fb0;
if (c='revsh') then i:=$06ff0fb0;
if (c='sxtb') then i:=$06af0070;
if (c='sxtb16') then i:=$068f0070;
if (c='sxth') then i:=$06bf0070;
if (c='uxtb') then i:=$06ef0070;
if (c='uxtb16') then i:=$06cf0070;
if (c='uxth') then i:=$06ff0070;
if (i=0) then exit;
inc(i,o);
inc(i,p1.reg shl 12);
inc(i,p2.reg);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
OutOk:=True;
End;



Const
  Str_cmn='|cmn|cmp|';
Procedure Inst_cmn;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
{if (p2.Typ=0) then Exit;}
if (p3.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 28;
i:=0;
if (c='cmn') then i:=$01700000;
if (c='cmp') then i:=$01500000;
if (i=0) then exit;
inc(i,o);
inc(i,p1.Reg shl 16);
OutStr:=ConvertMemPointData(p2.Str,i);
if (OutStr='') then exit;
OutOk:=True;
End;



Const
  Str_cps='|cps|';
Procedure Inst_cps;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='cps') then i:=$f1020000;
if (i=0) then exit;
inc(i,BVal('$'+p1.Num) and $1f);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
OutOk:=True;
End;



Const
  Str_cpsie='|cpsie|cpsid|';
Procedure Inst_cpsie;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then exit;
if (p2.Typ<>2) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='cpsie') then i:=$f10a0000;
if (Cmd='cpsid') then i:=$f10e0000;
if (i=0) then exit;
inc(i,BVal('$'+p2.Num) and $1f);
if (pos('a',p1.Str)<>0) then inc(i,$100);
if (pos('i',p1.Str)<>0) then inc(i,$80);
if (pos('f',p1.Str)<>0) then inc(i,$40);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
OutOk:=True;
End;



Const
  Str_ldc='|ldc|ldc2|stc|stc2|';
Procedure Inst_ldc;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then exit;
if (p2.Typ<>4) then Exit;
if (p3.Typ<>5) then Exit;
if (p4.Typ<>0) then Exit;
o:=0;
c:=cmd;
if (copy(c,length(c),666)='l') then begin;
  c:=copy(c,1,length(c)-1);
  o:=$00400000;
  end;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
inc(o,i shl 28);
i:=0;
if (c='ldc') then i:=$0c100000;
if (c='ldc2') then i:=$fc100000;
if (c='stc') then i:=$0c000000;
if (c='stc2') then i:=$fc000000;
if (i=0) then exit;
inc(o,i);
AnalizeFpuNums(p1.Str,i);
if (i<0) then exit;
inc(o,i shl 8);
inc(o,p2.Rg2 shl 12);
OutStr:=ConvertMemPointCoproc(p3.Mem,o);
if (OutStr='') then exit;
OutOk:=True;
End;



Const
  Str_ldm='|ldm|stm|';
Procedure Inst_ldm;
Var
  i,o,p:LongInt;
  a,c:String;
Begin;
if (OutOk=True) then Exit;
c:=cmd;
o:=AnalizeMultpl(copy(c,length(c)-1,666),false);
if (o<0) then exit;
c:=copy(c,1,length(c)-2);
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
inc(o,i shl 28);
i:=-1;
if (c='ldm') then i:=$08100000;
if (c='stm') then i:=$08000000;
if (i<0) then exit;
o:=o or i;
a:=p1.Str;
if (copy(a,length(a),666)='!') then begin;
  a:=copy(a,1,length(a)-1);
  inc(o,$00200000);
  end;
AnalizeCpuRegs(a,i);
if (i<0) then exit;
inc(o,i shl 16);
a:=p2.Str;
addParam(a,p3);
addParam(a,p4);
addParam(a,p5);
addParam(a,p6);
addParam(a,p7);
if (copy(a,length(a),666)='^') then begin;
  a:=copy(a,1,length(a)-1);
  o:=o or $00400000;
  end;
i:=AnalizeRegsList(a);
if (i<0) then exit;
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o+i)),4);
OutOk:=True;
End;



Const
  Str_ldr1='|ldr|str|';
Procedure Inst_ldr1;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then exit;
if (p2.Typ<>5) then Exit;
if (p3.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,4,2));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,3)+copy(c,6,666);
o:=i shl 28;
i:=0;
if (c='ldr') then i:=$04100000;
if (c='ldrbt') then i:=$04700000;
if (c='ldrb') then i:=$04500000;
if (c='ldrt') then i:=$04300000;
if (c='str') then i:=$04000000;
if (c='strb') then i:=$04400000;
if (c='strbt') then i:=$04600000;
if (c='strt') then i:=$04200000;
if (i=0) then exit;
o:=o or i;
inc(o,p1.Reg shl 12);
OutStr:=ConvertMemPointWord(p2.Mem,o);
if (OutStr='') then exit;
OutOk:=True;
End;



Const
  Str_ldr2='|ldr|str|';
Procedure Inst_ldr2;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then exit;
if (p2.Typ<>5) then Exit;
if (p3.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,4,2));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,3)+copy(c,6,666);
o:=i shl 28;
i:=0;
if (c='ldrd') then i:=$000000d0;
if (c='ldrh') then i:=$001000b0;
if (c='ldrsb') then i:=$001000d0;
if (c='ldrsh') then i:=$001000f0;
if (c='strd') then i:=$000000f0;
if (c='strh') then i:=$000000b0;
if (i=0) then exit;
o:=o or i;
inc(o,p1.Reg shl 12);
OutStr:=ConvertMemPointMisc(p2.Mem,o);
if (OutStr='') then exit;
OutOk:=True;
End;



Const
  Str_ldr3='|ldrex|';
Procedure Inst_ldr3;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then exit;
if (p2.Typ<>5) then Exit;
if (p3.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 28;
i:=0;
if (c='ldrex') then i:=$01900f9f;
if (i=0) then exit;
o:=o or i;
inc(o,p1.Reg shl 12);
if (p2.Mem.Typ<>5) then exit;
inc(o,p2.Mem.Bas shl 16);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o)),4);
OutOk:=True;
End;



Const
  Str_mcr='|mcr|mrc|';
Procedure Inst_mcr;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then exit;
if (p2.Typ<>2) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>4) then Exit;
if (p5.Typ<>4) then Exit;
if (p6.Typ<>2) then Exit;
if (p7.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 28;
i:=0;
if (c='mcr') then i:=$0e000010;
if (c='mcr2') then i:=$fe000010;
if (c='mrc') then i:=$0e100010;
if (c='mrc2') then i:=$fe100010;
if (i=0) then exit;
o:=o or i;
AnalizeFpuNums(p1.Lab,i);
if (i<0) then exit;
inc(o,i shl 8);
inc(o,(BVal('$'+p2.Num) and 7) shl 21);
inc(o,p3.Reg shl 12);
inc(o,p4.Rg2 shl 16);
inc(o,p5.Rg2);
inc(o,(BVal('$'+p6.Num) and 7) shl 5);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o)),4);
OutOk:=True;
End;



Const
  Str_mcrr='|mcrr|mrrc|';
Procedure Inst_mcrr;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then exit;
if (p2.Typ<>2) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>1) then Exit;
if (p5.Typ<>4) then Exit;
if (p6.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 28;
i:=0;
if (c='mcrr') then i:=$0c400000;
if (c='mcrr2') then i:=$fc400000;
if (c='mrrc') then i:=$0c500000;
if (c='mrrc2') then i:=$fc500000;
if (i=0) then exit;
o:=o or i;
AnalizeFpuNums(p1.Lab,i);
if (i<0) then exit;
inc(o,i shl 8);
inc(o,(BVal('$'+p2.Num) and 15) shl 4);
inc(o,p3.Reg shl 12);
inc(o,p4.Reg shl 16);
inc(o,p5.Rg2);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o)),4);
OutOk:=True;
End;


Const
  Str_mla='|mla|smla|smls|smml|usada8|';
Procedure Inst_mla;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>1) then Exit;
if (p5.Typ<>0) then Exit;
c:=cmd;
o:=AnalizeCondS(c,3) shl 16;
i:=0;
if (c='mla') then i:=$00200090;
if (c='smlabb') then i:=$01000080;
if (c='smlatb') then i:=$010000a0;
if (c='smlabt') then i:=$010000c0;
if (c='smlatt') then i:=$010000e0;
if (c='smlad') then i:=$07000010;
if (c='smladx') then i:=$07000030;
if (c='smlawb') then i:=$01200080;
if (c='smlawt') then i:=$012000c0;
if (c='smlsd') then i:=$07000050;
if (c='smlsdx') then i:=$07000070;
if (c='smlsld') then i:=$07400050;
if (c='smlsldx') then i:=$07400070;
if (c='smmla') then i:=$07500010;
if (c='smmlar') then i:=$07500030;
if (c='smmls') then i:=$075000d0;
if (c='smmlsr') then i:=$075000f0;
if (c='usada8') then i:=$07800010;
if (i=0) then exit;
o:=o or i;
inc(o,p1.Reg shl 16);
inc(o,p2.Reg);
inc(o,p3.Reg shl 8);
inc(o,p4.Reg shl 12);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o)),4);
OutOk:=True;
End;


Const
  Str_mov='|mov|mvn|';
Procedure Inst_mov;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
{if (p2.Typ=0) then Exit;}
if (p3.Typ<>0) then Exit;
p3:=p2;
p2.Typ:=1;
p2.Reg:=0;
End;


Const
  Str_mrs='|mrs|';
Procedure Inst_mrs;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>3) then Exit;
if (p3.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 28;
i:=0;
if (c='mrs') then i:=$010f0000;
if (i=0) then exit;
o:=o or i;
c:=kicsi(p2.str);
i:=-1;
if (c='cpsr') then i:=0;
if (c='spsr') then i:=1;
if (i<0) then exit;
inc(o,i shl 22);
inc(o,p1.Reg shl 12);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o)),4);
OutOk:=True;
End;


Const
  Str_msr='|msr|';
Procedure Inst_msr;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 28;
i:=0;
if (c='msr') then i:=$0120f000;
if (i=0) then exit;
o:=o or i;
c:=kicsi(copy(p1.str,1,4));
i:=-1;
if (c='cpsr') then i:=0;
if (c='spsr') then i:=1;
if (i<0) then exit;
inc(o,i shl 22);
inc(o,p2.Reg);
c:=kicsi(copy(c,5,666));
if (c='') then c:='cf';
i:=0;
if (pos('c',c)<>0) then inc(i,$10000);
if (pos('x',c)<>0) then inc(i,$20000);
if (pos('s',c)<>0) then inc(i,$40000);
if (pos('f',c)<>0) then inc(i,$80000);
inc(o,i);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o)),4);
OutOk:=True;
End;


Const
  Str_mul='|mul|smmul|smuad|smul|smus|usad8|';
Procedure Inst_mul;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
c:=cmd;
o:=AnalizeCondS(c,3) shl 16;
i:=-1;
if (c='mul') then i:=$00000090;
if (c='smmul') then i:=$0750f010;
if (c='smmulr') then i:=$0750f030;
if (c='smuad') then i:=$0700f010;
if (c='smuadx') then i:=$0700f030;
if (c='smulbb') then i:=$01600080;
if (c='smulbt') then i:=$016000c0;
if (c='smultb') then i:=$016000a0;
if (c='smultt') then i:=$016000e0;
if (c='smulwb') then i:=$012000a0;
if (c='smulwt') then i:=$012000e0;
if (c='smusd') then i:=$0700f050;
if (c='smusdx') then i:=$0700f070;
if (c='usad8') then i:=$0780f010;
if (i<0) then exit;
inc(i,o);
inc(i,p1.Reg shl 16);
inc(i,p2.Reg);
inc(i,p3.Reg shl 8);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
OutOk:=True;
End;


Const
  Str_pkhbt='|pkhbt|pkhtb|';
Procedure Inst_pkhbt;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if not (p4.Typ in [0,2]) then Exit;
if (p5.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 28;
i:=-1;
if (c='pkhbt') then i:=$06800010;
if (c='pkhtb') then i:=$06800090;
if (i<0) then exit;
inc(i,o);
inc(i,p1.Reg shl 12);
inc(i,p2.Reg shl 16);
inc(i,p3.Reg);
inc(i,(BVal(p4.Str) and $1f) shl 7);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
OutOk:=True;
End;


Const
  Str_pld='|pld|';
Procedure Inst_pld;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>5) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='pld') then i:=$f550f000;
if (i=0) then exit;
OutStr:=ConvertMemPointWord(p1.Mem,i);
if (OutStr='') then exit;
OutOk:=True;
End;


Const
  Str_qadd='|qadd|qdadd|qsub|qdsub|sadd|sel|shadd|shsub|ssub|sxta|uadd|'+
  'uhadd|uhsub|uqadd|uqsub|usub|uxta|';
Procedure Inst_qadd;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 28;
i:=-1;
p:=0;
if (c='qadd') then begin; p:=1;i:=$01000050; end;
if (c='qadd16') then i:=$06200f10;
if (c='qadd8') then i:=$06200f90;
if (c='qaddsubx') then i:=$06200f30;
if (c='qdadd') then begin; p:=1;i:=$01400050; end;
if (c='qdsub') then begin; p:=1;i:=$01600050; end;
if (c='qsub') then begin; p:=1;i:=$01200050; end;
if (c='qsub16') then i:=$06200f70;
if (c='qsub8') then i:=$06200ff0;
if (c='qsubaddx') then i:=$06200f50;
if (c='sadd16') then i:=$06100f10;
if (c='sadd8') then i:=$06100f90;
if (c='saddsubx') then i:=$06100f30;
if (c='sel') then i:=$06800fb0;
if (c='shadd16') then i:=$06300f10;
if (c='shadd8') then i:=$06300f90;
if (c='shaddsubx') then i:=$06300f30;
if (c='shsub16') then i:=$06300f70;
if (c='shsub8') then i:=$06300ff0;
if (c='shsubaddx') then i:=$06300f50;
if (c='ssub16') then i:=$06100f70;
if (c='ssub8') then i:=$06100ff0;
if (c='ssubaddx') then i:=$06100f50;
if (c='sxtab') then i:=$06a00070;
if (c='sxtab16') then i:=$06800070;
if (c='sxtah') then i:=$06b00070;
if (c='uadd16') then i:=$06500f10;
if (c='uadd8') then i:=$06500f90;
if (c='uaddsubx') then i:=$06500f30;
if (c='uhadd16') then i:=$06700f10;
if (c='uhadd8') then i:=$06700f90;
if (c='uhaddsubx') then i:=$06700f30;
if (c='uhsub16') then i:=$06700f70;
if (c='uhsub8') then i:=$06700ff0;
if (c='uhsubaddx') then i:=$06700f50;
if (c='uqadd16') then i:=$06600f10;
if (c='uqadd8') then i:=$06600f90;
if (c='uqaddsubx') then i:=$06600f30;
if (c='uqsub16') then i:=$06600f70;
if (c='uqsub8') then i:=$06600ff0;
if (c='uqsubaddx') then i:=$06600f50;
if (c='usub16') then i:=$06500f70;
if (c='usub8') then i:=$06500ff0;
if (c='usubaddx') then i:=$06500f50;
if (c='uxtab') then i:=$06e00070;
if (c='uxtab16') then i:=$06c00070;
if (c='uxtah') then i:=$06f00070;
if (i<0) then exit;
if (p and 1<>0) then xchgParam(p2,p3);
inc(i,o);
inc(i,p1.Reg shl 12);
inc(i,p2.Reg shl 16);
inc(i,p3.Reg);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
OutOk:=True;
End;


Const
  Str_rfe='|rfe|';
Procedure Inst_rfe;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (p2.Typ<>0) then Exit;
c:=copy(cmd,1,3);
o:=0;
if (c='rfe') then o:=$f8100a00;
if (o=0) then exit;
i:=AnalizeMultpl(copy(cmd,4,666),false);
if (i<0) then exit;
o:=o or i;
c:=kicsi(p1.Str);
if (copy(c,length(c),666)='!') then begin;
  c:=copy(c,1,length(c)-1);
  o:=o or $00200000;
  end;
AnalizeCpuRegs(c,i);
if (i<0) then exit;
o:=(i shl 16) or o;
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o)),4);
OutOk:=True;
End;


Const
  Str_setend='|setend|';
Procedure Inst_setend;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>0) then Exit;
o:=0;
if (Cmd='setend') then o:=$f1010000;
if (o=0) then exit;
c:=kicsi(p1.Str);
i:=-1;
if (c='le') then i:=0;
if (c='be') then i:=1;
if (i<0) then exit;
inc(o,i shl 9);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o)),4);
OutOk:=True;
End;


Const
  Str_smlal='|smlal|smull|umaal|umlal|umull|';
Procedure Inst_smlal;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>1) then Exit;
if (p5.Typ<>0) then Exit;
c:=cmd;
o:=AnalizeCondS(c,5) shl 16;
i:=0;
if (c='smlal') then i:=$00e00090;
if (c='smlalbb') then i:=$01400080;
if (c='smlalbt') then i:=$014000c0;
if (c='smlaltb') then i:=$014000a0;
if (c='smlaltt') then i:=$014000e0;
if (c='smlald') then i:=$07400010;
if (c='smlaldx') then i:=$07400030;
if (c='smull') then i:=$00c00090;
if (c='umaal') then i:=$00400090;
if (c='umlal') then i:=$00a00090;
if (c='umull') then i:=$00800090;
if (i=0) then exit;
o:=o or i;
inc(o,p1.Reg shl 12);
inc(o,p2.Reg shl 16);
inc(o,p3.Reg);
inc(o,p4.Reg shl 8);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o)),4);
OutOk:=True;
End;


Const
  Str_srs='|srs|';
Procedure Inst_srs;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>0) then Exit;
c:=copy(cmd,1,3);
o:=0;
if (c='srs') then o:=$f84d0500;
if (o=0) then exit;
i:=AnalizeMultpl(copy(cmd,4,666),true);
if (i<0) then exit;
o:=o or i;
inc(o,BVal('$'+p1.Num) and $1f);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o)),4);
OutOk:=True;
End;


Const
  Str_ssat='|ssat|usat|';
Procedure Inst_ssat;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>2) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 28;
i:=0;
if (c='ssat') then i:=$06a00010;
if (c='ssat16') then i:=$06a00f30;
if (c='usat') then i:=$06e00010;
if (c='usat16') then i:=$06e00f30;
if (i=0) then exit;
o:=o or i;
inc(o,p1.Reg shl 12);
inc(o,(BVal('$'+p2.Num) and $1f) shl 16);
inc(o,p3.Reg);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o)),4);
OutOk:=True;
End;


Const
  Str_strex='|strex|swp|';
Procedure Inst_strex;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>5) then Exit;
if (p4.Typ<>0) then Exit;
if (p3.Mem.Typ<>5) then exit;
c:=cmd;
if (copy(c,1,5)='strex') then c:=copy(c,1,3)+copy(c,6,666)+'ex';
i:=AnalizeCond(copy(c,4,2));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,3)+copy(c,6,666);
o:=i shl 28;
i:=0;
if (c='strex') then i:=$01800f90;
if (c='swp') then i:=$01000090;
if (c='swpb') then i:=$01400090;
if (i=0) then exit;
o:=o or i;
inc(o,p1.Reg shl 12);
inc(o,p2.Reg);
inc(o,p3.Mem.Bas shl 16);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(o)),4);
OutOk:=True;
End;


Const
  Str_swi='|swi|';
Procedure Inst_swi;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,3]) then Exit;
if (p2.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 28;
i:=0;
if (c='swi') then i:=$0f000000;
if (i=0) then exit;
o:=o or i;
OutStr:=Convert2posNumLabConst(p1,4,o,0,24);
if (OutStr='') then exit;
OutOk:=True;
End;


Const
  Str_teq='|teq|tst|';
Procedure Inst_teq;
Var
  i,o,p:LongInt;
  c:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
{if (p2.Typ=0) then Exit;}
if (p3.Typ<>0) then Exit;
c:=cmd;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i<0) then i:=AnalizeCond('al') else c:=copy(c,1,length(c)-2);
o:=i shl 12;
i:=-1;
if (c='teq') then i:=$0130;
if (c='tst') then i:=$0110;
if (i<0) then exit;
i:=(i+o) shl 16;
inc(i,p1.Reg shl 16);
OutStr:=ConvertMemPointData(p2.Str,i);
if (OutStr='') then exit;
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
getParam(p6,b);
AnalizeParameter(b,p7);
OutStr:='er';
if (Cmd='') then Exit;

if EzzelKezd(Str_mov,Cmd) then Inst_mov;
if EzzelKezd(Str_adc,Cmd) then Inst_adc;
if EzzelKezd(Str_bbl,Cmd) then Inst_bbl;
if EzzelKezd(Str_blx,Cmd) then Inst_blx;
if BenneVan(Str_bkpt,Cmd) then Inst_bkpt;
if EzzelKezd(Str_cdp,Cmd) then Inst_cdp;
if EzzelKezd(Str_clz,Cmd) then Inst_clz;
if EzzelKezd(Str_cmn,Cmd) then Inst_cmn;
if BenneVan(Str_cps,Cmd) then Inst_cps;
if BenneVan(Str_cpsie,Cmd) then Inst_cpsie;
if EzzelKezd(Str_ldc,Cmd) then Inst_ldc;
if EzzelKezd(Str_ldm,Cmd) then Inst_ldm;
if EzzelKezd(Str_ldr1,Cmd) then Inst_ldr1;
if EzzelKezd(Str_ldr2,Cmd) then Inst_ldr2;
if EzzelKezd(Str_ldr3,Cmd) then Inst_ldr3;
if EzzelKezd(Str_mcr,Cmd) then Inst_mcr;
if EzzelKezd(Str_mcrr,Cmd) then Inst_mcrr;
if EzzelKezd(Str_mla,Cmd) then Inst_mla;
if EzzelKezd(Str_mrs,Cmd) then Inst_mrs;
if EzzelKezd(Str_msr,Cmd) then Inst_msr;
if EzzelKezd(Str_mul,Cmd) then Inst_mul;
if EzzelKezd(Str_pkhbt,Cmd) then Inst_pkhbt;
if BenneVan(Str_pld,Cmd) then Inst_pld;
if EzzelKezd(Str_qadd,Cmd) then Inst_qadd;
if EzzelKezd(Str_rfe,Cmd) then Inst_rfe;
if EzzelKezd(Str_setend,Cmd) then Inst_setend;
if EzzelKezd(Str_smlal,Cmd) then Inst_smlal;
if EzzelKezd(Str_srs,Cmd) then Inst_srs;
if EzzelKezd(Str_ssat,Cmd) then Inst_ssat;
if EzzelKezd(Str_strex,Cmd) then Inst_strex;
if EzzelKezd(Str_swi,Cmd) then Inst_swi;
if EzzelKezd(Str_teq,Cmd) then Inst_teq;

if (OutOk=False) then OutStr:='';
End;
