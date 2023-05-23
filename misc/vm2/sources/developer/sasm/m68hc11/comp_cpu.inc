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
  Str_aba1='|aba|abx|aby|asla|aslb|asld|asra|asrb|cba|clc|cli|clra|clrb|clv|'+
           'coma|comb|daa|deca|decb|des|dex|dey|fdiv|idiv|inca|incb|nop|mul|'+
           'ins|inx|iny|lsla|lslb|lsld|lsra|lsrb|lsrd|nega|negb|psha|pshb|';
  Str_aba2='|pshx|pshy|pula|pulb|pulx|puly|rola|rolb|rora|rorb|rti|rts|sba|'+
           'sec|sei|sev|stop|swi|tab|tap|tba|test|tpa|tsta|tstb|tsx|tsy|'+
           'txs|tys|wai|xgdx|xgdy|';
Procedure Inst_aba;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='aba') then i:=$1b;
if (Cmd='abx') then i:=$3a;
if (Cmd='aby') then i:=$183a;
if (Cmd='asla') then i:=$48;
if (Cmd='aslb') then i:=$58;
if (Cmd='asld') then i:=$05;
if (Cmd='asra') then i:=$47;
if (Cmd='asrb') then i:=$57;
if (Cmd='cba') then i:=$11;
if (Cmd='clc') then i:=$0c;
if (Cmd='cli') then i:=$0e;
if (Cmd='clra') then i:=$4f;
if (Cmd='clrb') then i:=$5f;
if (Cmd='clv') then i:=$0a;
if (Cmd='coma') then i:=$43;
if (Cmd='comb') then i:=$53;
if (Cmd='daa') then i:=$19;
if (Cmd='deca') then i:=$4a;
if (Cmd='decb') then i:=$5a;
if (Cmd='des') then i:=$34;
if (Cmd='dex') then i:=$09;
if (Cmd='dey') then i:=$1809;
if (Cmd='fdiv') then i:=$03;
if (Cmd='idiv') then i:=$02;
if (Cmd='inca') then i:=$4c;
if (Cmd='incb') then i:=$5c;
if (Cmd='ins') then i:=$31;
if (Cmd='inx') then i:=$08;
if (Cmd='iny') then i:=$1808;
if (Cmd='lsla') then i:=$48;
if (Cmd='lslb') then i:=$58;
if (Cmd='lsld') then i:=$05;
if (Cmd='lsra') then i:=$44;
if (Cmd='lsrb') then i:=$54;
if (Cmd='lsrd') then i:=$04;
if (Cmd='mul') then i:=$3d;
if (Cmd='nega') then i:=$40;
if (Cmd='negb') then i:=$50;
if (Cmd='nop') then i:=$01;
if (Cmd='psha') then i:=$36;
if (Cmd='pshb') then i:=$37;
if (Cmd='pshx') then i:=$3c;
if (Cmd='pshy') then i:=$183c;
if (Cmd='pula') then i:=$32;
if (Cmd='pulb') then i:=$33;
if (Cmd='pulx') then i:=$38;
if (Cmd='puly') then i:=$1838;
if (Cmd='rola') then i:=$49;
if (Cmd='rolb') then i:=$59;
if (Cmd='rora') then i:=$46;
if (Cmd='rorb') then i:=$56;
if (Cmd='rti') then i:=$3b;
if (Cmd='rts') then i:=$39;
if (Cmd='sba') then i:=$10;
if (Cmd='sec') then i:=$0d;
if (Cmd='sei') then i:=$0f;
if (Cmd='sev') then i:=$0b;
if (Cmd='stop') then i:=$cf;
if (Cmd='swi') then i:=$3f;
if (Cmd='tab') then i:=$16;
if (Cmd='tap') then i:=$06;
if (Cmd='tba') then i:=$17;
if (Cmd='test') then i:=$00;
if (Cmd='tpa') then i:=$07;
if (Cmd='tsta') then i:=$4d;
if (Cmd='tstb') then i:=$5d;
if (Cmd='tsx') then i:=$30;
if (Cmd='tsy') then i:=$1830;
if (Cmd='txs') then i:=$35;
if (Cmd='tys') then i:=$1835;
if (Cmd='wai') then i:=$3e;
if (Cmd='xgdx') then i:=$8f;
if (Cmd='xgdy') then i:=$188f;
if (i<0) then exit;
OutStr:=b2h(i);
if (i and $ff00<>0) then OutStr:=b2h(i shr 8)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_adca='|adca|adcb|adda|addb|addd|anda|andb|bita|bitb|cmpa|cmpb|'+
           'cpd|cpx|cpy|eora|eorb|ldaa|ldab|ldd|lds|ldx|ldy|oraa|orab|'+
           'sbca|sbcb|suba|subb|subd|';
Procedure Inst_adca;
Label vege;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ=0) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
o:=1;
if (Cmd='adca') then i:=$1889;
if (Cmd='adcb') then i:=$18c9;
if (Cmd='adda') then i:=$188b;
if (Cmd='addb') then i:=$18cb;
if (Cmd='addd') then begin; o:=2;i:=$18c3; end;
if (Cmd='anda') then i:=$1884;
if (Cmd='andb') then i:=$18c4;
if (Cmd='bita') then i:=$1885;
if (Cmd='bitb') then i:=$18c5;
if (Cmd='cmpa') then i:=$1881;
if (Cmd='cmpb') then i:=$18c1;
if (Cmd='cpd') then begin; o:=2;i:=$1a1acd83; end;
if (Cmd='cpx') then begin; o:=2;i:=$cd8c; end;
if (Cmd='cpy') then begin; o:=2;i:=$181a188c; end;
if (Cmd='eora') then i:=$1888;
if (Cmd='eorb') then i:=$18c8;
if (Cmd='ldaa') then i:=$1886;
if (Cmd='ldab') then i:=$18c6;
if (Cmd='ldd') then begin; o:=2;i:=$18cc; end;
if (Cmd='lds') then begin; o:=2;i:=$188e; end;
if (Cmd='ldx') then begin; o:=2;i:=$cdce; end;
if (Cmd='ldy') then begin; o:=2;i:=$181a18ce; end;
if (Cmd='oraa') then i:=$188a;
if (Cmd='orab') then i:=$18ca;
if (Cmd='sbca') then i:=$1882;
if (Cmd='sbcb') then i:=$18c2;
if (Cmd='suba') then i:=$1880;
if (Cmd='subb') then i:=$18c0;
if (Cmd='subd') then begin; o:=2;i:=$1883; end;
if (i<0) then exit;
if (p1.Typ in [2,4]) then begin;
  OutStr:=b2h(i)+SepCh+ConvertNumOrLabDatas(p1,o and 3);
  o:=i shr 24;
  goto vege;
  end;
if (p1.Typ<>3) then exit;
if (p1.Mem.Reg<>0) then begin;
  OutStr:=b2h(i+$20)+SepCh+ConvertMemoryDatas(p1.Mem,1);
  if (p1.Mem.Reg=2) then o:=i shr 8 else o:=i shr 16;
  goto vege;
  end;
if (p1.Mem.Typ=1) and (p1.Mem.Num and $ffffff00=0) then begin;
  inc(i,$10);
  p:=1;
  end else begin;
  inc(i,$30);
  p:=2;
  end;
OutStr:=b2h(i)+SepCh+ConvertMemoryDatas(p1.Mem,p);
o:=i shr 24;
vege:
o:=o and $ff;
if (o>0) then OutStr:=b2h(o)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_asl='|asl|asr|clr|com|dec|inc|jmp|jsr|lsl|lsr|neg|rol|ror|staa|stab|'+
          'std|sts|stx|sty|tst|';
Procedure Inst_asl;
Label vege;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
o:=0;
if (Cmd='asl') then i:=$1878;
if (Cmd='asr') then i:=$1877;
if (Cmd='clr') then i:=$187f;
if (Cmd='com') then i:=$1873;
if (Cmd='dec') then i:=$187a;
if (Cmd='inc') then i:=$187c;
if (Cmd='jmp') then i:=$187e;
if (Cmd='jsr') then begin; o:=1;i:=$18bd; end;
if (Cmd='lsl') then i:=$1878;
if (Cmd='lsr') then i:=$1874;
if (Cmd='neg') then i:=$1870;
if (Cmd='rol') then i:=$1879;
if (Cmd='ror') then i:=$1876;
if (Cmd='staa') then begin; o:=1;i:=$18b7; end;
if (Cmd='stab') then begin; o:=1;i:=$18f7; end;
if (Cmd='std') then begin; o:=1;i:=$18fd; end;
if (Cmd='sts') then begin; o:=1;i:=$18bf; end;
if (Cmd='stx') then begin; o:=1;i:=$cdff; end;
if (Cmd='sty') then begin; o:=1;i:=$181a18ff; end;
if (Cmd='tst') then i:=$187d;
if (i<0) then exit;
if (p1.Mem.Reg<>0) then begin;
  OutStr:=b2h(i-$10)+SepCh+ConvertMemoryDatas(p1.Mem,1);
  if (p1.Mem.Reg=2) then o:=i shr 8 else o:=i shr 16;
  end else begin;
  p:=2;
  if (i and 1<>0) then if (p1.Mem.Typ=1) and (p1.Mem.Num and $ffffff00=0) then begin;
    dec(i,$20);
    p:=1;
    end;
  OutStr:=b2h(i)+SepCh+ConvertMemoryDatas(p1.Mem,p);
  o:=i shr 24;
  end;
vege:
o:=o and $ff;
if (o>0) then OutStr:=b2h(o)+SepCh+OutStr;
OutOk:=True;
End;



Const
  Str_bra='|bra|brn|bhi|bls|bhs|bcc|blo|bcs|bne|beq|bvc|bvs|bpl|bmi|bge|'+
          'blt|bgt|ble|bsr|';
Procedure Inst_bra;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,4]) then Exit;
if (p2.Typ<>0) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='bra') then i:=$20;
if (Cmd='brn') then i:=$21;
if (Cmd='bhi') then i:=$22;
if (Cmd='bls') then i:=$23;
if (Cmd='bhs') or (Cmd='bcc') then i:=$24;
if (Cmd='blo') or (Cmd='bcs') then i:=$25;
if (Cmd='bne') then i:=$26;
if (Cmd='beq') then i:=$27;
if (Cmd='bvc') then i:=$28;
if (Cmd='bvs') then i:=$29;
if (Cmd='bpl') then i:=$2a;
if (Cmd='bmi') then i:=$2b;
if (Cmd='bge') then i:=$2c;
if (Cmd='blt') then i:=$2d;
if (Cmd='bgt') then i:=$2e;
if (Cmd='ble') then i:=$2f;
if (Cmd='bsr') then i:=$8d;
if (i<0) then exit;
OutStr:='';
case p1.Typ of
  2:OutStr:='numofs'+SpxCh+p1.Num;
  4:OutStr:='ofs?'+SpxCh+p1.Lab;
  else exit;
  end;
OutStr:=b2h(i)+SepCh+'byte'+SpxCh+'rela2end'+SpxCh+'signed'+SpxCh+OutStr;
OutOk:=True;
End;



Const
  Str_bclr='|bclr|bset|';
Procedure Inst_bclr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if not (p2.Typ in [2,4]) then Exit;
if (p3.Typ<>0) then Exit;
i:=-1;
if (Cmd='bclr') then i:=$15;
if (Cmd='bset') then i:=$14;
if (i<0) then exit;
if (p1.Mem.Reg=0) then begin;
  OutStr:=b2h(i)+SepCh+ConvertMemoryDatas(p1.Mem,1);
  end else begin;
  OutStr:=b2h(i+$08)+SepCh+ConvertMemoryDatas(p1.Mem,1);
  if (p1.Mem.Reg=2) then OutStr:=b2h($18)+SepCh+OutStr;
  end;
OutStr:=OutStr+SepCh+ConvertNumOrLabDatas(p2,1);
OutOk:=True;
End;



Const
  Str_brclr='|brclr|brset|';
Procedure Inst_brclr;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if not (p2.Typ in [2,4]) then Exit;
if not (p3.Typ in [2,4]) then Exit;
if (p4.Typ<>0) then Exit;
i:=-1;
if (Cmd='brclr') then i:=$13;
if (Cmd='brset') then i:=$12;
if (i<0) then exit;
if (p1.Mem.Reg=0) then begin;
  OutStr:=b2h(i)+SepCh+ConvertMemoryDatas(p1.Mem,1);
  end else begin;
  OutStr:=b2h(i+$0c)+SepCh+ConvertMemoryDatas(p1.Mem,1);
  if (p1.Mem.Reg=2) then OutStr:=b2h($18)+SepCh+OutStr;
  end;
OutStr:=OutStr+SepCh+ConvertNumOrLabDatas(p2,1);
a:='';
case p3.Typ of
  2:a:='numofs'+SpxCh+p3.Num;
  4:a:='ofs?'+SpxCh+p3.Lab;
  else exit;
  end;
OutStr:=OutStr+SepCh+'byte'+SpxCh+'rela2end'+SpxCh+'signed'+SpxCh+a;
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

if BenneVan(Str_aba1,Cmd) then Inst_aba;
if BenneVan(Str_aba2,Cmd) then Inst_aba;
if BenneVan(Str_adca,Cmd) then Inst_adca;
if BenneVan(Str_asl,Cmd) then Inst_asl;
if BenneVan(Str_bra,Cmd) then Inst_bra;
if BenneVan(Str_bclr,Cmd) then Inst_bclr;
if BenneVan(Str_brclr,Cmd) then Inst_brclr;

if (OutOk=False) then OutStr:='';
End;
