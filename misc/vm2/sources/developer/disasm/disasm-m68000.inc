Function DisAssemblerBinaryCode(src:String):String;
Label f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20;
Label f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,f31,f32,f33,f34,f35,f36,f37;
Label f38,f39,f40,f41,f42,f43,vege;
Const
  regNames:array[0..1,0..8] of String[2]=(
    ('d0','d1','d2','d3','d4','d5','d6','d7','pc'),
    ('a0','a1','a2','a3','a4','a5','a6','a7','pc')
  );
  wordLong:array[0..1] of String[2]=('.w','.l');
  scaleFct:array[0..3] of String[2]=('','*2','*4','*8');
  bytWrdLngNam:array[0..2] of String[2]=('.b','.w','.l');
  BytWrdLngSiz:array[0..2] of Byte=(1,2,4);
  WrdWrdLngSiz:array[0..2] of Byte=(2,2,4);
Var
  resD:String;
  resS:Byte;


Function GetBytes(var i:LongInt;s:Byte):Boolean;
Label vege;
Var
  buf:array[1..8] of byte absolute i;
Begin;
GetBytes:=True;
i:=0;
if (s<1) then goto vege;
if (s>sizeof(i)) then Exit;
if (length(src)<s) then Exit;
move(src[1],buf[5-s],s);
delete(src,1,s);
i:=ReadLongMSB(buf);
vege:
GetBytes:=False;
End;

Function decodeModeReg(t,s:LongInt):String;
Label f1;
Var
  os,i,o,p:LongInt;
  a,b:String;
Begin;
decodeModeReg:='???';
os:=resS;
t:=t and $3f;
case t shr 3 of
  0:begin; {d0}
    decodeModeReg:=regNames[0][t and 7];
    exit;
    end;
  1:begin; {a0}
    decodeModeReg:=regNames[1][t and 7];
    exit;
    end;
  2:begin; {(a0)}
    decodeModeReg:='('+regNames[1][t and 7]+')';
    exit;
    end;
  3:begin; {(a0)+}
    decodeModeReg:='('+regNames[1][t and 7]+')+';
    exit;
    end;
  4:begin; {-(a0)}
    decodeModeReg:='-('+regNames[1][t and 7]+')';
    exit;
    end;
  5:begin; {(1234h,a0)}
    if GetBytes(i,2) then exit;
    inc(resS,2);
    decodeModeReg:='('+disAsm2hex(i,2)+','+regNames[1][t and 7]+')';
    exit;
    end;
  6:begin; {extended}
    t:=t and 7;
    goto f1;
    end;
  7:begin; {extended}
    case t and 7 of
      0:begin; {(1234h)}
        if GetBytes(i,2) then exit;
        inc(resS,2);
        decodeModeReg:='('+disAsm2hex(i,2)+')';
        exit;
        end;
      1:begin; {(12345678h)}
        if GetBytes(i,4) then exit;
        inc(resS,4);
        decodeModeReg:='('+disAsm2hex(i,4)+')';
        exit;
        end;
      2:begin; {(1234h,pc)}
        if GetBytes(i,2) then exit;
        inc(resS,2);
        decodeModeReg:='('#10+BStr(integer(i)+os)+#10',pc)';
        exit;
        end;
      3:begin; {extended}
        t:=8;
        goto f1;
        end;
      4:begin; {#1234h}
        if (s<2) then s:=2;
        if GetBytes(i,s) then exit;
        inc(resS,s);
        decodeModeReg:=disAsm2hex(i,s);
        exit;
        end;
      else exit;
      end;
    exit;
    end;
  else exit;
  end;
Exit;
f1:
if GetBytes(i,2) then exit;
inc(resS,2);
if (i and $100=0) then begin; {(num8,addr,reg.size*scale)}
  if (t>7) then a:=#10+BStr(shortint(i)+os)+#10 else a:=disAsm2hex(i,1);
  a:=a+','+regNames[1][t];
  a:=a+','+regNames[i shr 15][(i shr 12) and 7];
  a:=a+wordLong[(i shr 11) and 1];
  a:=a+scaleFct[(i shr 9) and 3];
  decodeModeReg:='('+a+')';
  exit;
  end;
case (i shr 4) and 3 of
  1:o:=0;
  2:o:=2;
  3:o:=4;
  else exit;
  end;
if (o<1) then a:='' else begin;
  if GetBytes(p,o) then exit;
  inc(resS,o);
  if (t>7) then a:=#10+BStr(p+os)+#10 else a:=disAsm2hex(p,o);
  a:=a+',';
  end;
case i and 3 of
  0:begin;
    if (i and $80<>0) then a:=a+'z';
    a:=a+regNames[1][t];
    a:=a+',';
    if (i and $40<>0) then a:=a+'z';
    a:=a+regNames[i shr 15][(i shr 12) and 7];
    a:=a+wordLong[(i shr 11) and 1];
    a:=a+scaleFct[(i shr 9) and 3];
    decodeModeReg:='('+a+')';
    exit;
    end;
  1:o:=0;
  2:o:=2;
  3:o:=4;
  else exit;
  end;
if (o<1) then b:='' else begin;
  if GetBytes(p,o) then exit;
  inc(resS,o);
  b:=disAsm2hex(p,o);
  b:=','+b;
  end;
if (i and $80<>0) then a:=a+'z';
a:=a+regNames[1][t];
if (i and 4<>0) then a:=a+']';
a:=a+',';
if (i and $40<>0) then a:=a+'z';
a:=a+regNames[i shr 15][(i shr 12) and 7];
a:=a+wordLong[(i shr 11) and 1];
a:=a+scaleFct[(i shr 9) and 3];
if (i and 4=0) then a:=a+']';
a:=a+b;
decodeModeReg:='(['+a+')';
End;

Function decodeCond(i:LongInt):String;
Var a:String;
Begin;
i:=i and $f;
a:='???';
case i of
  0:a:='t';
  1:a:='f';
  2:a:='hi';
  3:a:='ls';
  4:a:='cc';
  5:a:='cs';
  6:a:='ne';
  7:a:='eq';
  8:a:='vc';
  9:a:='vs';
  10:a:='pl';
  11:a:='mi';
  12:a:='ge';
  13:a:='lt';
  14:a:='gt';
  15:a:='le';
  end;
decodeCond:=a;
End;

Function decodeRegs(o:LongInt):String;
Var
  a:String;
  i:LongInt;
Begin;
a:='';
for i:=0 to 7 do begin;
  if (o and 1<>0) then a:=a+',d'+BStr(i);
  o:=o shr 1;
  end;
for i:=0 to 7 do begin;
  if (o and 1<>0) then a:=a+',a'+BStr(i);
  o:=o shr 1;
  end;
decodeRegs:='<'+copy(a,2,255)+'>';
End;



Var
  i,o,p,q:LongInt;
  a,b,c:String;
Begin;
if GetBytes(i,2) then begin;
  GetBytes(i,1);
  resD:='db '+disAsm2hex(i,1);
  resS:=1;
  goto vege;
  end;
resD:='dw '+disAsm2hex(i,2);
resS:=2;

case i of
  $023c:begin; a:='andi';b:='ccr';goto f6; end;
  $0a3c:begin; a:='eori';b:='ccr';goto f6; end;
  $4afc:begin; resD:='illegal';goto vege; end;
  $4e71:begin; resD:='nop';goto vege; end;
  $003c:begin; a:='ori';b:='ccr';goto f6; end;
  $4e74:begin; a:='rtd';goto f39; end;
  $4e77:begin; resD:='rtr';goto vege; end;
  $4e75:begin; resD:='rts';goto vege; end;
  $4e76:begin; resD:='trapv';goto vege; end;
  end;

case i shr 3 of
  $0909:begin; a:='bkpt';goto f16; end;
  $0910:begin; a:='ext.w';goto f27; end;
  $0918:begin; a:='ext.l';goto f27; end;
  $0938:begin; a:='extb.l';goto f27; end;
  $1ec4:begin; a:='move16';goto f33; end;
  $1ec0..$1ec3:begin; a:='move16';goto f34; end;
  $00d8..$00d9:begin; a:='rtm';goto f40; end;
  $0908:begin; a:='swap';goto f27; end;
  $09c8..$09c9:begin; a:='trap';goto f42; end;
  $09cb:begin; a:='unlk';goto f40; end;
  end;

case i shr 6 of
  $122:begin; a:='movem.w';goto f35; end;
  $123:begin; a:='movem.l';goto f35; end;
  $132:begin; a:='movem.w';goto f35; end;
  $133:begin; a:='movem.l';goto f35; end;
  $10b:begin; a:='move';b:='ccr,';c:='';goto f32; end;
  $113:begin; a:='move';b:='';c:=',ccr';goto f32; end;
  $103:begin; a:='move';b:='sr,';c:='';goto f32; end;
  $387:begin; a:='asl';goto f8; end;
  $383:begin; a:='asr';goto f8; end;
  $38f:begin; a:='lsl';goto f8; end;
  $38b:begin; a:='lsr';goto f8; end;
  $39f:begin; a:='rol';goto f8; end;
  $39b:begin; a:='ror';goto f8; end;
  $397:begin; a:='roxl';goto f8; end;
  $393:begin; a:='roxr';goto f8; end;
  $13b:begin; a:='jmp';goto f8; end;
  $13a:begin; a:='jsr';goto f8; end;
  $021:begin; a:='bchg';b:=decodeModeReg(i,2);goto f6; end;
  $022:begin; a:='bclr';b:=decodeModeReg(i,2);goto f6; end;
  $023:begin; a:='bset';b:=decodeModeReg(i,2);goto f6; end;
  $020:begin; a:='btst';b:=decodeModeReg(i,2);goto f6; end;
  $01b:begin; a:='callm';b:=decodeModeReg(i,2);goto f6; end;
  $3ab:begin; a:='bfchg';goto f10; end;
  $3b3:begin; a:='bfclr';goto f10; end;
  $3bb:begin; a:='bfset';goto f10; end;
  $3a3:begin; a:='bftst';goto f10; end;
  $3af:begin; a:='bfexts';goto f11; end;
  $3a7:begin; a:='bfextu';goto f11; end;
  $3b7:begin; a:='bfffo';goto f11; end;
  $3bf:begin; a:='bfins';goto f11; end;
  $131:begin; a:='div';goto f24; end;
  $130:begin; a:='mul';goto f24; end;
  $120:begin; a:='nbcd';goto f8; end;
  $121:begin; a:='pea';goto f8; end;
  $12b:begin; a:='tas';goto f8; end;
  end;

case i shr 11 of
  $1:begin;
    if (i and $1ff=$fc) then begin; a:='cas2';goto f14; end;
    if (i and $1c0=$c0) then begin; a:='cas';goto f13; end;
    end;
  $0:begin;
    if (i and $1c0=$c0) then goto f17;
    end;
  $9:begin;
    case (i shr 3) and $ff of
      $ca:begin; a:='link';p:=1;goto f29; end;
      $01:begin; a:='link';p:=2;goto f29; end;
      end;
    end;
  end;

case i shr 8 of
  $06:begin; a:='addi';goto f3; end;
  $02:begin; a:='andi';goto f3; end;
  $60:begin; a:='bra';goto f12; end;
  $61:begin; a:='bsr';goto f12; end;
  $42:begin; a:='clr';goto f18; end;
  $0c:begin; a:='cmpi';goto f3; end;
  $0a:begin; a:='eori';goto f3; end;
  $44:begin; a:='neg';goto f18; end;
  $40:begin; a:='negx';goto f18; end;
  $46:begin; a:='not';goto f18; end;
  $00:begin; a:='ori';goto f3; end;
  $04:begin; a:='subi';goto f3; end;
  $4a:begin; a:='tst';goto f18; end;
  end;

case i shr 12 of
  $c:begin;
    if (i and $1c0=$1c0) then begin; a:='muls';goto f23; end;
    if (i and $1c0=$0c0) then begin; a:='mulu';goto f23; end;
    if (i and $1f0=$100) then begin; a:='abcd';goto f1; end;
    case (i shr 3) and $3f of
      $28,$29,$31:begin; a:='exg';goto f26; end;
      end;
    if ((i and $c0) in [$00,$40,$80]) then begin; a:='and';goto f2; end;
    end;
  $d:begin;
    if ((i shr 6) and 7 in [3,7]) then begin; a:='adda';goto f5; end;
    if (i and $130=$100) then begin; a:='addx';goto f1; end;
    a:='add';
    goto f2;
    end;
  $5:begin;
    if (i and $ff in [$fa..$fc]) then begin; a:='trap';goto f43; end;
    if (i and $f8=$c8) then begin; a:='db';goto f22; end;
    if (i and $c0=$c0) then begin; a:='s';goto f41; end;
    if (i and $100=0) then begin; a:='addq';goto f4; end;
    if (i and $100<>0) then begin; a:='subq';goto f4; end;
    end;
  $e:begin;
    case i and $118 of
      $000:begin; a:='asr';goto f7; end;
      $100:begin; a:='asl';goto f7; end;
      $008:begin; a:='lsr';goto f7; end;
      $108:begin; a:='lsl';goto f7; end;
      $018:begin; a:='ror';goto f7; end;
      $118:begin; a:='rol';goto f7; end;
      $010:begin; a:='roxr';goto f7; end;
      $110:begin; a:='roxl';goto f7; end;
      end;
    end;
  $6:begin;
    p:=i and $ff;
    o:=shortint(p);
    p:=0;
    if (o=0) then begin;
      if GetBytes(o,2) then goto vege;
      o:=Integer(o);
      inc(resS,2);
      p:=1;
      end else if (o=-1) then begin;
      if GetBytes(o,4) then goto vege;
      inc(resS,4);
      p:=2;
      end;
    resD:='b'+decodeCond(i shr 8)+bytWrdLngNam[p]+' '#10+BStr(o+2)+#10;
    end;
  $0:begin;
    if (i and $138=$108) then begin; a:='movep';goto f36; end;
    case i and $1c0 of
      $140:begin; a:='bchg';goto f9; end;
      $180:begin; a:='bclr';goto f9; end;
      $1c0:begin; a:='bset';goto f9; end;
      $100:begin; a:='btst';goto f9; end;
      end;
    end;
  $4:begin;
    if (i and $40=0) then begin; a:='chk';goto f15; end;
    if (i and $1c0=$1c0) then begin; a:='lea';goto f28; end;
    end;
  $b:begin;
    if (i and $138=$108) then begin; a:='cmpm';goto f21; end;
    case i and $1c0 of
      $1c0,$c0:begin; a:='cmpa';goto f20; end;
      $100,$140,$180:begin; a:='eor';goto f25;end;
      end;
    a:='cmp';
    goto f19;
    end;
  $8:begin;
    if (i and $1c0=$1c0) then begin; a:='divs';goto f23; end;
    if (i and $1c0=$0c0) then begin; a:='divu';goto f23; end;
    if (i and $1f0=$140) then begin; a:='pack';goto f38; end;
    if (i and $1f0=$180) then begin; a:='unpk';goto f38; end;
    if (i and $1f0=$100) then begin; a:='sbcd';goto f1; end;
    if ((i and $c0) in [$00,$40,$80]) then begin; a:='or';goto f2; end;
    end;
  $1:begin;
    a:='move';
    p:=0;
    goto f30;
    end;
  $3:begin;
    a:='move';
    p:=1;
    if (i and $1c0=$40) then begin; a:='movea';goto f31; end;
    goto f30;
    end;
  $2:begin;
    a:='move';
    p:=2;
    if (i and $1c0=$40) then begin; a:='movea';goto f31; end;
    goto f30;
    end;
  $7:begin;
    a:='moveq';
    goto f37;
    end;
  $9:begin;
    if ((i shr 6) and 7 in [3,7]) then begin; a:='suba';goto f5; end;
    if (i and $130=$100) then begin; a:='subx';goto f1; end;
    a:='sub';
    goto f2;
    end;
  end;
goto Vege;

f1:
if (i and 8=0) then begin; b:='';c:='';o:=0; end else begin; b:='-(';c:=')';o:=1; end;
resD:=a+' '+b+regNames[o][i and 7]+c+','+b+regNames[o][(i shr 9) and 7]+c;
goto vege;

f2:
o:=(i shr 6) and 3;
a:=a+bytWrdLngNam[o]+' ';
b:=decodeModeReg(i,BytWrdLngSiz[o]);
c:=regNames[0][(i shr 9) and 7];
if (i and $100=0) then resD:=a+b+','+c else resD:=a+c+','+b;
goto vege;

f3:
o:=(i shr 6) and 3;
if (o>2) then goto vege;
p:=WrdWrdLngSiz[o];
if GetBytes(q,p) then goto vege;
inc(resS,p);
b:=decodeModeReg(i,p);
c:=disAsm2hex(q,p);
resD:=a+bytWrdLngNam[o]+' '+c+','+b;
goto vege;

f4:
q:=(i shr 9) and 7;
if (q=0) then q:=8;
o:=(i shr 6) and 3;
if (o>2) then goto vege;
b:=decodeModeReg(i,BytWrdLngSiz[o]);
resD:=a+bytWrdLngNam[o]+' '+BStr(q)+','+b;
goto vege;

f5:
if (i and $100<>0) then o:=2 else o:=1;
a:=a+bytWrdLngNam[o]+' ';
b:=decodeModeReg(i,BytWrdLngSiz[o]);
c:=regNames[1][(i shr 9) and 7];
resD:=a+b+','+c;
goto vege;

f6:
if GetBytes(i,2) then goto vege;
inc(resS,2);
resD:=a+' '+BStr(i)+','+b;
goto vege;

f7:
o:=(i shr 6) and 3;
if (o>2) then goto vege;
p:=(i shr 9) and 7;
if (i and $20=0) then begin;
  if (p=0) then p:=8;
  b:=BStr(p);
  end else begin;
  b:=regNames[0][p];
  end;
resD:=a+bytWrdLngNam[o]+' '+b+','+regNames[0][i and 7];
goto vege;

f8:
b:=decodeModeReg(i,2);
resD:=a+' '+b;
goto vege;

f9:
b:=decodeModeReg(i,2);
resD:=a+' '+regNames[0][(i shr 9) and 7]+','+b;
goto vege;

f10:
if GetBytes(o,2) then exit;
inc(resS,2);
b:=decodeModeReg(i,2);
p:=(o shr 6) and $1f;
q:=o and $1f;
b:=b+',';
if (o and $800<>0) then begin;
  b:=b+regNames[0][p and 7];
  end else begin;
  b:=b+BStr(p);
  end;
b:=b+',';
if (o and $20<>0) then begin;
  b:=b+regNames[0][q and 7];
  end else begin;
  b:=b+BStr(q);
  end;
resD:=a+' '+b;
goto vege;

f11:
if GetBytes(o,2) then exit;
inc(resS,2);
b:=decodeModeReg(i,2);
p:=(o shr 6) and $1f;
q:=o and $1f;
b:=b+',';
if (o and $800<>0) then begin;
  b:=b+regNames[0][p and 7];
  end else begin;
  b:=b+BStr(p);
  end;
b:=b+',';
if (o and $20<>0) then begin;
  b:=b+regNames[0][q and 7];
  end else begin;
  b:=b+BStr(q);
  end;
b:=b+','+regNames[0][(o shr 12) and 7];
resD:=a+' '+b;
goto vege;

f12:
p:=i and $ff;
o:=shortint(p);
p:=0;
if (o=0) then begin;
  if GetBytes(o,2) then goto vege;
  o:=Integer(o);
  inc(resS,2);
  p:=1;
  end else if (o=-1) then begin;
  if GetBytes(o,4) then goto vege;
  inc(resS,4);
  p:=2;
  end;
resD:=a+bytWrdLngNam[p]+' '#10+BStr(o+2)+#10;
goto vege;

f13:
p:=((i shr 9) and 3)-1;
if GetBytes(o,2) then goto vege;
inc(resS,2);
b:=decodeModeReg(i,BytWrdLngSiz[p]);
resD:=a+bytWrdLngNam[p]+' '+b+','+regNames[0][o and 7]+','+regNames[0][(o shr 6) and 7];
goto vege;

f14:
p:=((i shr 9) and 3)-1;
resD:=a+bytWrdLngNam[p]+' ';
if GetBytes(o,2) then goto vege;
inc(resS,2);
p:=i shr 15;
a:=regNames[p][o and 7]+',';
b:=regNames[p][(o shr 6) and 7]+',';
c:=regNames[p][(o shr 12) and 7]+',';
if GetBytes(o,2) then goto vege;
inc(resS,2);
p:=i shr 15;
a:=a+regNames[p][o and 7];
b:=b+regNames[p][(o shr 6) and 7];
c:=c+regNames[p][(o shr 12) and 7];
resD:=resD+a+','+b+','+c;
goto vege;

f15:
p:=4-((i shr 7) and 3);
b:=decodeModeReg(i,BytWrdLngSiz[p]);
resD:=a+bytWrdLngNam[p]+' '+b+','+regNames[0][(i shr 9) and 7];
goto vege;

f16:
resD:=a+' '+BStr(i and 7);
goto vege;

f17:
p:=(i shr 9) and 3;
if GetBytes(o,2) then goto vege;
inc(resS,2);
if (o and $800=0) then a:='cmp2' else a:='chk2';
b:=decodeModeReg(i,BytWrdLngSiz[p]);
resD:=a+bytWrdLngNam[p]+' '+b+','+regNames[o shr 15][(o shr 12) and 7];
goto vege;

f18:
p:=(i shr 6) and 3;
b:=decodeModeReg(i,BytWrdLngSiz[p]);
resD:=a+bytWrdLngNam[p]+' '+b;
goto vege;

f19:
p:=(i shr 6) and 3;
b:=decodeModeReg(i,BytWrdLngSiz[p]);
resD:=a+bytWrdLngNam[p]+' '+b+','+regNames[0][(i shr 9) and 7];
goto vege;

f20:
p:=((i shr 8) and 1)+1;
b:=decodeModeReg(i,BytWrdLngSiz[p]);
resD:=a+bytWrdLngNam[p]+' '+b+','+regNames[1][(i shr 9) and 7];
goto vege;

f21:
p:=(i shr 6) and 3;
resD:=a+bytWrdLngNam[p]+' ('+regNames[1][i and 7]+')+,('+regNames[1][(i shr 9) and 7]+')+';
goto vege;

f22:
a:=a+decodeCond(i shr 8)+' '+regNames[0][i and 7];
if GetBytes(o,2) then goto vege;
inc(resS,2);
resD:=a+','#10+BStr(integer(o)+2)+#10;
goto vege;

f23:
b:=decodeModeReg(i,2);
resD:=a+'.w '+b+','+regNames[0][(i shr 9) and 7];
goto vege;

f24:
if GetBytes(o,2) then goto vege;
inc(resS,2);
if (o and $800<>0) then a:=a+'s' else a:=a+'u';
if (o and $400<>0) then a:=a+'l';
b:=decodeModeReg(i,4);
resD:=a+'.l '+b+','+regNames[0][o and 7]+','+regNames[0][(o shr 12) and 7];
goto vege;

f25:
p:=(i shr 6) and 3;
b:=decodeModeReg(i,BytWrdLngSiz[p]);
resD:=a+bytWrdLngNam[p]+' '+regNames[0][(i shr 9) and 7]+','+b;
goto vege;

f26:
case (i shr 3) and $1f of
  $08:begin; o:=0;p:=0; end;
  $09:begin; o:=1;p:=1; end;
  $11:begin; o:=1;p:=0; end;
  else goto vege;
  end;
resD:=a+' '+regNames[p][i and 7]+','+regNames[o][(i shr 9) and 7];
goto vege;

f27:
resD:=a+' '+regNames[0][i and 7];
goto vege;

f28:
b:=decodeModeReg(i,4);
resD:=a+' '+b+','+regNames[1][(i shr 9) and 7];
goto vege;

f29:
o:=BytWrdLngSiz[p];
if GetBytes(q,o) then goto vege;
inc(resS,o);
resD:=a+bytWrdLngNam[p]+' '+regNames[1][i and 7]+','+disAsm2hex(q,o);
goto vege;

f30:
o:=BytWrdLngSiz[p];
b:=decodeModeReg(i,o);
i:=i shr 6;
i:=((i shr 3) and 7) or ((i and 7) shl 3);
c:=decodeModeReg(i,o);
resD:=a+bytWrdLngNam[p]+' '+b+','+c;
goto vege;

f31:
b:=decodeModeReg(i,BytWrdLngSiz[p]);
resD:=a+bytWrdLngNam[p]+' '+b+','+regNames[1][(i shr 9) and 7];
goto vege;

f32:
resD:=a+' '+b+decodeModeReg(i,2)+c;
goto vege;

f33:
if GetBytes(o,2) then goto vege;
inc(resS,2);
resD:=a+' ('+regNames[1][i and 7]+')+,('+regNames[1][(o shr 12) and 7]+')+';
goto vege;

f34:
if GetBytes(o,4) then goto vege;
inc(resS,4);
b:='('+disAsm2hex(o,4)+')';
c:='('+regNames[1][i and 7]+')';
if (i and $10=0) then c:=c+'+';
if (i and 8=0) then resD:=a+' '+c+','+b else resD:=a+' '+b+','+c;
goto vege;

f35:
if GetBytes(o,2) then goto vege;
inc(resS,2);
b:=decodeModeReg(i,2);
c:=decodeRegs(o);
if (i and $400=0) then resD:=a+' '+c+','+b else resD:=a+' '+b+','+c;
goto vege;

f36:
if (i and $40<>0) then a:=a+'.l' else a:=a+'.w';
if GetBytes(o,2) then goto vege;
inc(resS,2);
b:='('+disAsm2hex(o,2)+','+regNames[1][i and 7]+')';
c:=regNames[0][(i shr 9) and 7];
if (i and $80<>0) then resD:=a+' '+c+','+b else resD:=a+' '+b+','+c;
goto vege;

f37:
resD:=a+' '+disAsm2hex(i,1)+','+regNames[0][(i shr 9) and 7];
goto vege;

f38:
if GetBytes(o,2) then goto vege;
inc(resS,2);
if (i and 8=0) then begin;
  b:=regNames[0][i and 7]+','+regNames[0][(i shr 9) and 7]+',';
  end else begin;
  b:='-('+regNames[1][i and 7]+'),-('+regNames[1][(i shr 9) and 7]+'),';
  end;
resD:=a+' '+b+disAsm2hex(o,2);
goto vege;

f39:
if GetBytes(i,2) then goto vege;
inc(resS,2);
resD:=a+' '+BStr(i);
goto vege;

f40:
resD:=a+' '+regNames[(i shr 3) and 1][i and 7];
goto vege;

f41:
b:=decodeModeReg(i,2);
c:=decodeCond(i shr 8);
resD:=a+c+' '+b;
goto vege;

f42:
resD:=a+' '+BStr(i and $0f);
goto vege;

f43:
case i and 7 of
  2:p:=2;
  3:p:=4;
  4:p:=0;
  end;
if (p<1) then b:='' else begin;
  if GetBytes(o,p) then goto vege;
  inc(resS,p);
  b:=' '+disAsm2hex(o,p);
  if (p<4) then b:='.w'+b else b:='.l'+b;
  end;
resD:=a+decodeCond(i shr 8)+b;
goto vege;

Vege:
DisAssemblerBinaryCode:=chr(resS)+resD;
End;
