Procedure CompileOneCpuLine;
Var
  Cmd:String;
  p1:OneParameterRec;
  p2:OneParameterRec;
  p3:OneParameterRec;
  p4:OneParameterRec;
  p5:OneParameterRec;

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
  Str_add='|add|addcc|addx|addxcc|and|andcc|andn|andncc|jmpl|ld|ldd|ldsb|'+
          'ldsh|ldstub|ldub|lduh|mulscc|or|orcc|orn|orncc|restore|rett|save|';
  Str_sub='|sll|sra|srl|sub|subcc|subx|subxcc|swap|taddcc|taddcctv|tsubcc|'+
          'tsubcctv|xnor|xnorcc|xor|xorcc|';
Procedure Inst_add;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='add') then i:=$80000000;
if (Cmd='addcc') then i:=$80800000;
if (Cmd='addx') then i:=$80400000;
if (Cmd='addxcc') then i:=$80c00000;
if (Cmd='and') then i:=$80080000;
if (Cmd='andcc') then i:=$80880000;
if (Cmd='andn') then i:=$80280000;
if (Cmd='andncc') then i:=$80a80000;
if (Cmd='jmpl') then i:=$81c00000;
if (Cmd='ld') then i:=$c0000000;
if (Cmd='ldd') then i:=$c0180000;
if (Cmd='ldsb') then i:=$c0480000;
if (Cmd='ldsh') then i:=$c0500000;
if (Cmd='ldstub') then i:=$c0680000;
if (Cmd='ldub') then i:=$c0080000;
if (Cmd='lduh') then i:=$c0100000;
if (Cmd='mulscc') then i:=$81200000;
if (Cmd='or') then i:=$80100000;
if (Cmd='orcc') then i:=$80900000;
if (Cmd='orn') then i:=$80300000;
if (Cmd='orncc') then i:=$80b00000;
if (Cmd='restore') then i:=$81e80000;
if (Cmd='rett') then i:=$81c80000;
if (Cmd='save') then i:=$81e00000;
if (Cmd='sll') then i:=$81280000;
if (Cmd='sra') then i:=$81380000;
if (Cmd='srl') then i:=$81300000;
if (Cmd='sub') then i:=$80200000;
if (Cmd='subcc') then i:=$80a00000;
if (Cmd='subx') then i:=$80600000;
if (Cmd='subxcc') then i:=$80e00000;
if (Cmd='swap') then i:=$c0780000;
if (Cmd='taddcc') then i:=$81000000;
if (Cmd='taddcctv') then i:=$81100000;
if (Cmd='tsubcc') then i:=$81080000;
if (Cmd='tsubcctv') then i:=$81180000;
if (Cmd='xnor') then i:=$80380000;
if (Cmd='xnorcc') then i:=$80b80000;
if (Cmd='xor') then i:=$80180000;
if (Cmd='xorcc') then i:=$80980000;
if (i=0) then exit;
inc(i,p1.Reg shl 14);
inc(i,p3.Reg shl 25);
OutStr:=ConvertRegOrSimm13(p2,i);
OutOk:=True;
End;



Const
  Str_b='|b|';
Procedure Inst_b;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,3]) then Exit;
if (p2.Typ<>0) then Exit;
i:=AnalizeCond(copy(cmd,2,666));
if (i<0) then exit;
case p1.Typ of
  2:a:='numofs'+SpxCh+p1.Num;  {Num}
  3:a:='ofs'+SpxCh+p1.Lab; {Label}
  else exit;
  end;
i:=(i shl 25) or $800000;
OutStr:='msb'+SpxCh+'dword'+SpxCh+'shr'+SpxCh+'2'+SpxCh+'and'+SpxCh+'3FFFFF';
OutStr:=OutStr+SpxCh+'rela2beg'+SpxCh+'or'+SpxCh+AnalizeNumbers(bstr(i))+SpxCh+a;
OutOk:=True;
End;



Const
  Str_call='|call|';
Procedure Inst_call;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,3]) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='call') then i:=$40000000;
if (i=0) then exit;
case p1.Typ of
  2:a:='numofs'+SpxCh+p1.Num;  {Num}
  3:a:='ofs'+SpxCh+p1.Lab; {Label}
  else exit;
  end;
OutStr:='msb'+SpxCh+'dword'+SpxCh+'shr'+SpxCh+'2'+SpxCh+'and'+SpxCh+'3FFFFFFF';
OutStr:=OutStr+SpxCh+'rela2beg'+SpxCh+'or'+SpxCh+AnalizeNumbers(bstr(i))+SpxCh+a;
OutOk:=True;
End;



Const
  Str_iflush='|iflush|';
Procedure Inst_iflush;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='iflush') then i:=$81d80000;
if (i=0) then exit;
inc(i,p1.Reg shl 14);
OutStr:=ConvertRegOrSimm13(p2,i);
OutOk:=True;
End;



Const
  Str_lda='|lda|ldda|ldsba|ldsha|ldstuba|lduba|lduha|swapa|';
Procedure Inst_lda;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>2) then Exit;
if (p4.Typ<>1) then Exit;
if (p5.Typ<>0) then Exit;
i:=0;
if (Cmd='lda') then i:=$c0800000;
if (Cmd='ldda') then i:=$c0980000;
if (Cmd='ldsba') then i:=$c0c80000;
if (Cmd='ldsha') then i:=$c0d00000;
if (Cmd='ldstuba') then i:=$c0e80000;
if (Cmd='lduba') then i:=$c0880000;
if (Cmd='lduha') then i:=$c0900000;
if (Cmd='swapa') then i:=$c0f80000;
if (i=0) then exit;
inc(i,p1.Reg shl 14);
inc(i,p2.Reg);
inc(i,BVal('$'+p3.num) shl 5);
inc(i,p4.Reg shl 25);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
OutOk:=True;
End;



Const
  Str_rdpsr='|rd|';
Procedure Inst_rdpsr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>3) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='rd') then i:=$81400000;
if (i=0) then exit;
o:=-1;
if (p1.Lab='psr') then o:=$00080000;
if (p1.Lab='tbr') then o:=$00180000;
if (p1.Lab='wim') then o:=$00100000;
if (p1.Lab='y') then o:=$00000000;
if (o<0) then exit;
inc(i,p2.Reg shl 25);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i xor o)),4);
OutOk:=True;
End;



Const
  Str_sethi='|sethi|';
Procedure Inst_sethi;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,3]) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='sethi') then i:=$01000000;
if (i=0) then exit;
inc(i,p2.Reg shl 25);
case p1.Typ of
  2:begin; {Num}
    inc(i,BVal('$'+p1.Num) shr 10);
    OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
    end;
  3:begin; {Label}
    OutStr:=SpxCh+AnalizeNumbers(bstr(i))+SpxCh+p1.Lab;
    OutStr:='msb'+SpxCh+'dword'+SpxCh+'shr'+SpxCh+'A'+SpxCh+'or'+OutStr;
    end;
  else exit;
  end;
OutOk:=True;
End;



Const
  Str_st='|st|stb|std|sth|';
Procedure Inst_st;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ=0) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='st') then i:=$c0200000;
if (Cmd='stb') then i:=$c0280000;
if (Cmd='std') then i:=$c0380000;
if (Cmd='sth') then i:=$c0300000;
if (i=0) then exit;
inc(i,p1.Reg shl 25);
inc(i,p2.Reg shl 14);
OutStr:=ConvertRegOrSimm13(p3,i);
OutOk:=True;
End;



Const
  Str_sta='|sta|stba|stda|stha|';
Procedure Inst_sta;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>2) then Exit;
if (p5.Typ<>0) then Exit;
i:=0;
if (Cmd='sta') then i:=$c0a00000;
if (Cmd='stba') then i:=$c0a80000;
if (Cmd='stda') then i:=$c0b80000;
if (Cmd='stha') then i:=$c0b00000;
if (i=0) then exit;
inc(i,p1.Reg shl 25);
inc(i,p2.Reg shl 14);
inc(i,p3.Reg);
inc(i,BVal('$'+p4.num) shl 5);
OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
OutOk:=True;
End;



Const
  Str_t='|t|';
Procedure Inst_t;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then exit;
if (p2.Typ=0) then exit;
if (p3.Typ<>0) then Exit;
i:=AnalizeCond(copy(cmd,2,666));
if (i<0) then exit;
i:=(i shl 25) or $81d00000;
inc(i,p1.Reg shl 14);
OutStr:=ConvertRegOrSimm13(p2,i);
OutOk:=True;
End;



Const
  Str_unimp='|unimp|';
Procedure Inst_unimp;
Var
  i,o,p:LongInt;
  a:String;
Begin;
if (OutOk=True) then Exit;
if not (p1.Typ in [2,3]) then Exit;
if (p2.Typ<>0) then Exit;
i:=-1;
if (Cmd='unimp') then i:=$00000000;
if (i<0) then exit;
case p1.Typ of
  2:begin; {Num}
    inc(i,BVal('$'+p1.Num));
    OutStr:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
    end;
  3:begin; {Label}
    OutStr:=SpxCh+AnalizeNumbers(bstr(i))+SpxCh+p1.Lab;
    OutStr:='msb'+SpxCh+'dword'+SpxCh+'or'+OutStr;
    end;
  else exit;
  end;
OutOk:=True;
End;



Const
  Str_wrpsr='|wr|';
Procedure Inst_wrpsr;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>1) then Exit;
if (p2.Typ=0) then Exit;
if (p3.Typ<>3) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='wr') then i:=$81800000;
if (i=0) then exit;
o:=-1;
if (p3.Lab='psr') then o:=$00080000;
if (p3.Lab='tbr') then o:=$00180000;
if (p3.Lab='wim') then o:=$00100000;
if (p3.Lab='y') then o:=$00000000;
if (o<0) then exit;
inc(i,p1.Reg shl 14);
OutStr:=ConvertRegOrSimm13(p2,i xor o);
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
AnalizeParameter(b,p5);
OutStr:='er';
if (Cmd='') then Exit;

if BenneVan(Str_add,Cmd) then Inst_add;
if BenneVan(Str_sub,Cmd) then Inst_add;
if EzzelKezd(Str_b,Cmd) then Inst_b;
if BenneVan(Str_call,Cmd) then Inst_call;
if BenneVan(Str_iflush,Cmd) then Inst_iflush;
if BenneVan(Str_lda,Cmd) then Inst_lda;
if BenneVan(Str_rdpsr,Cmd) then Inst_rdpsr;
if BenneVan(Str_sethi,Cmd) then Inst_sethi;
if BenneVan(Str_st,Cmd) then Inst_st;
if BenneVan(Str_sta,Cmd) then Inst_sta;
if EzzelKezd(Str_t,Cmd) then Inst_t;
if EzzelKezd(Str_unimp,Cmd) then Inst_unimp;
if EzzelKezd(Str_wrpsr,Cmd) then Inst_wrpsr;

if (OutOk=False) then OutStr:='';
End;
