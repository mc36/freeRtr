Function DisAssemblerBinaryCode(src:String):String;
Label r14rimr25,r14rim,r14r0asr25,r25r14rim,r25r14r0as,vege;
Const
  cpuRegs:array[0..31] of String[2]=(
    'g0','g1','g2','g3','g4','g5','g6','g7',
    'o0','o1','o2','o3','o4','o5','o6','o7',
    'l0','l1','l2','l3','l4','l5','l6','l7',
    'i0','i1','i2','i3','i4','i5','i6','i7'    );
  cndStat:array[0..15] of String[3]=(
    'n','e','le','l','leu','cs','neg','vs',
    'a','ne','g','ge','gu','cc','pos','vc'    );

Function getR25(p:LongInt):String;
Begin;
getR25:=cpuRegs[(p shr 25) and $1f];
End;

Function getR14(p:LongInt):String;
Begin;
getR14:=cpuRegs[(p shr 14) and $1f];
End;

Function getR0(p:LongInt):String;
Begin;
getR0:=cpuRegs[p and $1f];
End;

Function getRI13(p:LongInt):String;
Begin;
if (p and $2000=0) then begin;
  getRI13:=cpuRegs[p and $1f];
  exit;
  end;
p:=p and $1fff;
if (p and $1000<>0) then p:=p or $fffff000;
getRI13:=disAsm2hex(p,2);
End;

Function getI22(p:LongInt):LongInt;
Begin;
p:=p and $3fffff;
if (p and $200000<>0) then p:=p or $ffc00000;
getI22:=p;
End;

Function getASI(p:LongInt):String;
Begin;
getASI:=BStr((p shr 5) and $ff);
End;

Var
  resD:String;
  i,o,p:LongInt;
  a,b:String;
Begin;
resD:='';
if (length(src)<4) then begin;
  DisAssemblerBinaryCode:=#1'db '+disAsm2hex(ord(src[1]),1);
  delete(src,1,1);
  exit;
  end;
p:=ReadLongMSB(src[1]);
delete(src,1,4);
resD:='dd '+disAsm2hex(p,4);

if (p shr 30=1) then begin;
  i:=(p and $3fffffff)*4;
  resD:='call '#10+BStr(i);
  goto vege;
  end;

case ((p shr 22) and 7) or ((p shr 24) and $c0) of
  $02:begin;
    i:=getI22(p)*4;
    resD:='b'+cndStat[(p shr 25) and $f];
    if (p and $20000000<>0) then resD:=resD+',a';
    resD:=resD+' '#10+BStr(i);
    goto vege;
    end;
  $04:begin;
    resD:='sethi '+disAsm2hex(getI22(p) shl 10,4)+','+getR25(p);
    goto vege;
    end;
  $00:begin;
    resD:='unimp '+disAsm2hex(getI22(p),4);
    goto vege;
    end;
  end;

case ((p shr 19) and $3f) or ((p shr 24) and $c0) of
  $80:begin; a:='add';goto r14rimr25; end;
  $90:begin; a:='addcc';goto r14rimr25; end;
  $88:begin; a:='addx';goto r14rimr25; end;
  $98:begin; a:='addxcc';goto r14rimr25; end;
  $81:begin; a:='and';goto r14rimr25; end;
  $91:begin; a:='andcc';goto r14rimr25; end;
  $85:begin; a:='andn';goto r14rimr25; end;
  $95:begin; a:='andncc';goto r14rimr25; end;
  $bb:begin; a:='iflush %';goto r14rim; end;
  $b8:begin; a:='jmpl';goto r14rimr25; end;
  $c0:begin; a:='ld';goto r14rimr25; end;
  $d0:begin; a:='lda';goto r14r0asr25; end;
  $c3:begin; a:='ldd';goto r14rimr25; end;
  $d3:begin; a:='ldda';goto r14r0asr25; end;
  $c9:begin; a:='ldsb';goto r14rimr25; end;
  $d9:begin; a:='ldsba';goto r14r0asr25; end;
  $ca:begin; a:='ldsh';goto r14rimr25; end;
  $da:begin; a:='ldsha';goto r14r0asr25; end;
  $cd:begin; a:='ldstub';goto r14rimr25; end;
  $dd:begin; a:='ldstuba';goto r14r0asr25; end;
  $c1:begin; a:='ldub';goto r14rimr25; end;
  $d1:begin; a:='lduba';goto r14r0asr25; end;
  $c2:begin; a:='lduh';goto r14rimr25; end;
  $d2:begin; a:='lduha';goto r14r0asr25; end;
  $a4:begin; a:='mulscc';goto r14rimr25; end;
  $82:begin; a:='or';goto r14rimr25; end;
  $92:begin; a:='orcc';goto r14rimr25; end;
  $86:begin; a:='orn';goto r14rimr25; end;
  $96:begin; a:='orncc';goto r14rimr25; end;
  $a9:begin; resD:='rd psr,'+getR25(p);goto vege; end;
  $ab:begin; resD:='rd tbr,'+getR25(p);goto vege; end;
  $aa:begin; resD:='rd wim,'+getR25(p);goto vege; end;
  $a8:begin; resD:='rd y,'+getR25(p);goto vege; end;
  $bd:begin; a:='restore';goto r14rimr25; end;
  $b9:begin; a:='rett %';goto r14rim; end;
  $bc:begin; a:='save';goto r14rimr25; end;
  $a5:begin; a:='sll';goto r14rimr25; end;
  $a7:begin; a:='sra';goto r14rimr25; end;
  $a6:begin; a:='srl';goto r14rimr25; end;
  $c4:begin; a:='st';goto r25r14rim; end;
  $d4:begin; a:='sta';goto r25r14r0as; end;
  $c5:begin; a:='stb';goto r25r14rim; end;
  $d5:begin; a:='stba';goto r25r14r0as; end;
  $c7:begin; a:='std';goto r25r14rim; end;
  $d7:begin; a:='stda';goto r25r14r0as; end;
  $c6:begin; a:='sth';goto r25r14rim; end;
  $d6:begin; a:='stha';goto r25r14r0as; end;
  $84:begin; a:='sub';goto r14rimr25; end;
  $94:begin; a:='subcc';goto r14rimr25; end;
  $8c:begin; a:='subx';goto r14rimr25; end;
  $9c:begin; a:='subxcc';goto r14rimr25; end;
  $cf:begin; a:='swap';goto r14rimr25; end;
  $df:begin; a:='swapa';goto r14r0asr25; end;
  $a0:begin; a:='taddcc';goto r14rimr25; end;
  $a2:begin; a:='taddcctv';goto r14rimr25; end;
  $ba:begin; a:='t'+cndStat[(p shr 25) and $f]+' %';goto r14rim; end;
  $a1:begin; a:='tsubcc';goto r14rimr25; end;
  $a3:begin; a:='tsubcctv';goto r14rimr25; end;
  $b1:begin; a:='wr %,psr';goto r14rim; end;
  $b3:begin; a:='wr %,tbr';goto r14rim; end;
  $b2:begin; a:='wr %,wim';goto r14rim; end;
  $b0:begin; a:='wr %,y';goto r14rim; end;
  $87:begin; a:='xnor';goto r14rimr25; end;
  $97:begin; a:='xnorcc';goto r14rimr25; end;
  $83:begin; a:='xor';goto r14rimr25; end;
  $93:begin; a:='xorcc';goto r14rimr25; end;
  end;
goto vege;

r14rimr25:
resD:=a+' '+getR14(p)+','+getRI13(p)+','+getR25(p);
goto vege;

r25r14rim:
resD:=a+' '+getR25(p)+','+getR14(p)+','+getRI13(p);
goto vege;

r14rim:
kicserel('%',getR14(p)+','+getRI13(p),a);
resD:=a;
goto vege;

r14r0asr25:
resD:=a+' '+getR14(p)+','+getR0(p)+','+getASI(p)+','+getR25(p);
goto vege;

r25r14r0as:
resD:=a+' '+getR25(p)+','+getR14(p)+','+getR0(p)+','+getASI(p);
goto vege;

Vege:
DisAssemblerBinaryCode:=#4+resD;
End;
