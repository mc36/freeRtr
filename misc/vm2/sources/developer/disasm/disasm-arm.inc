Function DisAssemblerBinaryCode(src:String):String;
Const
  conds:array[0..15] of string[2]=('eq','ne','cs','cc','mi',
    'pl','vs','vc','hi','ls','ge','lt','gt','le','','??');
  cpuR:array[0..15] of String[3]=('r0','r1','r2','r3','r4',
    'r5','r6','r7','r8','r9','r10','r11','r12','sp','lr','pc');
  fpuR:array[0..15] of String[3]=('c0','c1','c2','c3','c4',
    'c5','c6','c7','c8','c9','c10','c11','c12','c13','c14','c15');
  rots:array[0..3] of String[3]=('lsl','lsr','asr','ror');
  abdi:array[0..3] of String[2]=('da','ia','db','ib');


Function decCond(i:LongInt):String;
Begin;
decCond:=conds[i shr 28];
End;


Function decData(i:LongInt):String;
Begin;
if (i and $02000000<>0) then begin; {immed}
  decData:=disAsm2hex((i and $ff) shl (((16-(i shr 8)) and $f) shl 1),4);
  exit;
  end;
if (i and $10<>0) then begin; {reg shift reg}
  decData:=cpuR[i and $f]+' '+rots[(i shr 5) and 3]+' '+cpuR[(i shr 8) and $f];
  exit;
  end;
if (i and $ff0=0) then begin; {reg}
  decData:=cpuR[i and $f];
  exit;
  end;
{reg shift num}
decData:=cpuR[i and $f]+' '+rots[(i shr 5) and 3]+' '+BStr((i shr 7) and $1f);
End;


Function decWord(i:LongInt):String;
Label f1,f2;
Var a:String;
Begin;
if (i and $02000000=0) then begin; {reg+-immed}
  if (i and $fff=0) then begin; a:='';goto f2; end;
  a:=disAsm2hex(i and $fff,4);
  goto f1;
  end;
if (i and $ff0=0) then begin; {reg+-reg}
  a:=cpuR[i and $f];
  goto f1;
  end;
{reg+-reg shift num}
a:=cpuR[i and $f]+' '+rots[(i shr 5) and 3]+' '+BStr((i shr 7) and $1f);
f1:
if (i and $00800000=0) then a:='-'+a else a:='+'+a;
f2:
a:=cpuR[(i shr 16) and $f]+a;
a:='['+a+']';
if (i and $00200000<>0) then a:=a+'!';
if (i and $01000000=0) then a:=a+'~';
decWord:=a;
End;


Function decCopr(i:LongInt):String;
Label f1;
Var a:String;
Begin;
if (i and $ff=0) then begin; a:='';goto f1; end;
a:=disAsm2hex((i and $ff)*4,4);
if (i and $00800000=0) then a:='-'+a else a:='+'+a;
f1:
a:=cpuR[(i shr 16) and $f]+a;
a:='['+a+']';
if (i and $00200000<>0) then a:=a+'!';
if (i and $01000000=0) then a:=a+'~';
decCopr:=a;
End;


Function decRegL(o,p,q:LongInt):String;
Var
  a:String;
  i:LongInt;
Begin;
a:='';
for i:=p to q do if ((1 shl i) and o<>0) then a:=a+','+cpuR[i];
decRegL:='{'+copy(a,2,666)+'}';
End;


Function decMisc(i:LongInt):String;
Label f1,f2;
Var a:String;
Begin;
if (i and $00400000=0) then begin; {reg+-reg}
  a:=cpuR[i and $f];
  goto f1;
  end;
{reg+-num}
if (i and $f0f=0) then begin; a:='';goto f2; end;
a:=disAsm2hex(((i and $f00) shr 4) or (i and $f),4);
f1:
if (i and $00800000=0) then a:='-'+a else a:='+'+a;
f2:
a:=cpuR[(i shr 16) and $f]+a;
a:='['+a+']';
if (i and $00200000<>0) then a:=a+'!';
if (i and $01000000=0) then a:=a+'~';
decMisc:=a;
End;



Label cndSrdRnDat,cndRm,cndRdRm,cndRnDat,cndRdWrd,cndRdMsc,cndRdAn,Vege;
Label cndSrdRmRsRn,cndSrdDat,cndCpOpRdRnRmOp,cndCpOpRdRnRm,cndRdPSR;
Label cndSrdRmRs,cndRdRnRmImm,cndRdRmRs,cndRdRmRs2,cndRdRnRm,cndSrdLoHiRmRs;
Label cndRdImmRmShft,cndRdImmRm,cndCpCdAdr,cndAdrRnRglst,cndRdRmRn;
Label cndRdRmRsRot,cndRdRmRot;
Var
  resD:String;
  int:Integer;
  i,o,p:LongInt;
  a,b:String;
Begin;
resD:='';
if (length(src)<4) then begin;
  DisAssemblerBinaryCode:=#1'db '+disAsm2hex(ord(src[1]),1);
  delete(src,1,1);
  exit;
  end;
p:=ReadLongLSB(src[1]);
delete(src,1,4);
resD:='dd '+disAsm2hex(p,4);

case p and $ffff00f0 of
  $f1010000:begin;
    resD:='setend ';
    if (p and $200<>0) then resD:=resD+'be' else resD:=resD+'le';
    goto vege;
    end;
  end;

case p and $fff000f0 of
  $e1200070:begin;
    i:=((p and $fff00) shr 4) or (p and $f);
    resD:='bkpt '+disAsm2hex(i,2);
    goto vege;
    end;
  end;

case p and $fff00000 of
  $f1000000:begin;
    resD:='cps';
    i:=1;
    case (p shr 18) and 3 of
      2:resD:=resD+'ie';
      3:resD:=resD+'id';
      else i:=0;
      end;
    resD:=resD+' ';
    if (i<>0) then begin;
      if (p and $100<>0) then resD:=resD+'a';
      if (p and $80<>0) then resD:=resD+'i';
      if (p and $40<>0) then resD:=resD+'f';
      end;
    if (p and $20000<>0) then begin;
      if (i<>0) then resD:=resD+',';
      resD:=resD+BStr(p and $1f);
      end;
    goto vege;
    end;
  end;

case p and $fd70f000 of
  $f550f000:begin;
    resD:='pld '+decWord(p);
    goto vege;
    end;
  end;

case p and $0ff00ff0 of
  $01a00000:begin; a:='cpy%';goto cndRdRm; end;
  $06b00f30:begin; a:='rev%';goto cndRdRm; end;
  $06b00fb0:begin; a:='rev16%';goto cndRdRm; end;
  $06f00fb0:begin; a:='revsh%';goto cndRdRm; end;
  end;

case p and $0fff00f0 of
  $06af0070:begin; a:='sxtb';goto cndRdRmRot; end;
  $068f0070:begin; a:='sxtb16';goto cndRdRmRot; end;
  $06bf0070:begin; a:='sxth';goto cndRdRmRot; end;
  $06ef0070:begin; a:='uxtb';goto cndRdRmRot; end;
  $06cf0070:begin; a:='uxtb16';goto cndRdRmRot; end;
  $06ff0070:begin; a:='uxth';goto cndRdRmRot; end;
  end;

case p and $0ff0f0f0 of
  $0750f010:begin; a:='smmul';goto cndRdRmRs2; end;
  $0750f030:begin; a:='smmulr';goto cndRdRmRs2; end;
  $0700f010:begin; a:='smuad';goto cndRdRmRs2; end;
  $0700f030:begin; a:='smuadx';goto cndRdRmRs2; end;
  $01600080:begin; a:='smulbb';goto cndRdRmRs2; end;
  $016000a0:begin; a:='smulbt';goto cndRdRmRs2; end;
  $016000c0:begin; a:='smultb';goto cndRdRmRs2; end;
  $016000e0:begin; a:='smultt';goto cndRdRmRs2; end;
  $012000a0:begin; a:='smulwb';goto cndRdRmRs2; end;
  $012000e0:begin; a:='smulwt';goto cndRdRmRs2; end;
  $0700f050:begin; a:='smusd';goto cndRdRmRs2; end;
  $0700f070:begin; a:='smusdx';goto cndRdRmRs2; end;
  $0780f010:begin; a:='usad8%';goto cndRdRmRs2; end;
  end;

case p and $0ff000f0 of
  $01200030:begin; a:='blx%';goto cndRm; end;
  $01200010:begin; a:='bx%';goto cndRm; end;
  $01200020:begin; a:='bxj%';goto cndRm; end;
  $01600010:begin; a:='clz%';goto cndRdRm; end;
  $01900090:begin; a:='ldrex%';goto cndRdAn; end;
  $01800090:begin; a:='strex%';goto cndRdAn; end;
  end;

case p and $0ff00070 of
  $06800010:begin; a:='pkhbt';goto cndRdRnRmImm; end;
  $06800050:begin; a:='pkhtb';goto cndRdRnRmImm; end;
  end;

case p and $0ff000f0 of
  $01000080:begin; a:='smlabb';goto cndSrdRmRsRn; end;
  $010000c0:begin; a:='smlabt';goto cndSrdRmRsRn; end;
  $010000a0:begin; a:='smlatb';goto cndSrdRmRsRn; end;
  $010000e0:begin; a:='smlatt';goto cndSrdRmRsRn; end;
  $07000010:begin; a:='smlad';goto cndSrdRmRsRn; end;
  $07000030:begin; a:='smladx';goto cndSrdRmRsRn; end;
  $01400080:begin; a:='smlalbb';goto cndSrdLoHiRmRs; end;
  $014000c0:begin; a:='smlalbt';goto cndSrdLoHiRmRs; end;
  $014000a0:begin; a:='smlaltb';goto cndSrdLoHiRmRs; end;
  $014000e0:begin; a:='smlaltt';goto cndSrdLoHiRmRs; end;
  $07400010:begin; a:='smlald';goto cndSrdLoHiRmRs; end;
  $07400030:begin; a:='smlaldx';goto cndSrdLoHiRmRs; end;
  $01200080:begin; a:='smlawb';goto cndSrdRmRsRn; end;
  $012000c0:begin; a:='smlawt';goto cndSrdRmRsRn; end;
  $07000050:begin; a:='smlsd';goto cndSrdRmRsRn; end;
  $07000070:begin; a:='smlsdx';goto cndSrdRmRsRn; end;
  $07400050:begin; a:='smlsld';goto cndSrdLoHiRmRs; end;
  $07400070:begin; a:='smlsldx';goto cndSrdLoHiRmRs; end;
  $07500010:begin; p:=p and $ffefffff;a:='smmla';goto cndSrdRmRsRn; end;
  $07500030:begin; p:=p and $ffefffff;a:='smmlar';goto cndSrdRmRsRn; end;
  $075000d0:begin; p:=p and $ffefffff;a:='smmls';goto cndSrdRmRsRn; end;
  $075000f0:begin; p:=p and $ffefffff;a:='smmlsr';goto cndSrdRmRsRn; end;
  $00c00090:begin; a:='smull';goto cndSrdLoHiRmRs; end;
  $00400090:begin; a:='umaal';goto cndSrdLoHiRmRs; end;
  $07800010:begin; a:='usada8';goto cndSrdRmRsRn; end;
  end;

case p and $0fe000f0 of
  $00200090:begin; a:='mla';goto cndSrdRmRsRn; end;
  $00e00090:begin; a:='smlal';goto cndSrdLoHiRmRs; end;
  $00a00090:begin; a:='umlal';goto cndSrdLoHiRmRs; end;
  $00800090:begin; a:='umull';goto cndSrdLoHiRmRs; end;
  end;

case p and $0ff000f0 of
  $01000050:begin; a:='qadd%';goto cndRdRmRs; end;
  $06200010:begin; a:='qadd16%';goto cndRdRmRs; end;
  $06200090:begin; a:='qadd8%';goto cndRdRmRs; end;
  $06200030:begin; a:='qaddsubx%';goto cndRdRmRs; end;
  $01400050:begin; a:='qdadd%';goto cndRdRmRs; end;
  $01600050:begin; a:='qdsub%';goto cndRdRmRs; end;
  $01200050:begin; a:='qsub%';goto cndRdRmRs; end;
  $06200070:begin; a:='qsub16%';goto cndRdRmRs; end;
  $062000f0:begin; a:='qsub8%';goto cndRdRmRs; end;
  $06200050:begin; a:='qsubaddx%';goto cndRdRmRs; end;
  $06100010:begin; a:='sadd16%';goto cndRdRmRs; end;
  $06100090:begin; a:='sadd8%';goto cndRdRmRs; end;
  $06100030:begin; a:='saddsubx%';goto cndRdRmRs; end;
  $068000b0:begin; a:='sel%';goto cndRdRnRm; end;
  $06300010:begin; a:='shadd16%';goto cndRdRmRs; end;
  $06300090:begin; a:='shadd8%';goto cndRdRmRs; end;
  $06300030:begin; a:='shaddsubx%';goto cndRdRmRs; end;
  $06300070:begin; a:='shsub16%';goto cndRdRmRs; end;
  $063000f0:begin; a:='shsub8%';goto cndRdRmRs; end;
  $06300050:begin; a:='shsubaddx%';goto cndRdRmRs; end;
  $06a00030:begin; a:='ssat16';goto cndRdImmRm; end;
  $06100070:begin; a:='ssub16%';goto cndRdRmRs; end;
  $061000f0:begin; a:='ssub8%';goto cndRdRmRs; end;
  $06100050:begin; a:='ssubaddx%';goto cndRdRmRs; end;
  $01000090:begin; a:='swp%';goto cndRdRmRn; end;
  $01400090:begin; a:='swp%b';goto cndRdRmRn; end;
  $06a00070:begin; a:='sxtab%';goto cndRdRmRsRot; end;
  $06800070:begin; a:='sxtab16%';goto cndRdRmRsRot; end;
  $06b00070:begin; a:='sxtah%';goto cndRdRmRsRot; end;
  $06500010:begin; a:='uadd16%';goto cndRdRmRs; end;
  $06500090:begin; a:='uadd8%';goto cndRdRmRs; end;
  $06500030:begin; a:='uaddsubx%';goto cndRdRmRs; end;
  $06700010:begin; a:='uhadd16%';goto cndRdRmRs; end;
  $06700090:begin; a:='uhadd8%';goto cndRdRmRs; end;
  $06700030:begin; a:='uhaddsubx%';goto cndRdRmRs; end;
  $06700070:begin; a:='uhsub16%';goto cndRdRmRs; end;
  $067000f0:begin; a:='uhsub8%';goto cndRdRmRs; end;
  $06700050:begin; a:='uhsubaddx%';goto cndRdRmRs; end;
  $06600010:begin; a:='uqadd16%';goto cndRdRmRs; end;
  $06600090:begin; a:='uqadd8%';goto cndRdRmRs; end;
  $06600030:begin; a:='uqaddsubx%';goto cndRdRmRs; end;
  $06600070:begin; a:='uqsub16%';goto cndRdRmRs; end;
  $066000f0:begin; a:='uqsub8%';goto cndRdRmRs; end;
  $06600050:begin; a:='uqsubaddx%';goto cndRdRmRs; end;
  $06e00030:begin; a:='usat16';goto cndRdImmRm; end;
  $06500070:begin; a:='usub16%';goto cndRdRmRs; end;
  $065000f0:begin; a:='usub8%';goto cndRdRmRs; end;
  $06500050:begin; a:='usubaddx%';goto cndRdRmRs; end;
  $06e00070:begin; a:='uxtab%';goto cndRdRmRsRot; end;
  $06c00070:begin; a:='uxtab16%';goto cndRdRmRsRot; end;
  $06f00070:begin; a:='uxtah%';goto cndRdRmRsRot; end;
  end;

case p and $0fe00030 of
  $06a00010:begin; a:='ssat';goto cndRdImmRmShft; end;
  $06e00010:begin; a:='usat';goto cndRdImmRmShft; end;
  end;

case p and $0ff00000 of
  $0c400000:begin; a:='mcrr';goto cndCpOpRdRnRm; end;
  $0c500000:begin; a:='mrrc';goto cndCpOpRdRnRm; end;
  $01000000:begin; a:='mrs%';b:='cpsr';goto cndRdPSR; end;
  $01400000:begin; a:='mrs%';b:='spsr';goto cndRdPSR; end;
  end;

case p and $0fe00000 of
  $00000000:begin; a:='mul';goto cndSrdRmRs; end;
  end;

case p and $0db00000 of
  $01200000:begin;
    resD:='msr'+decCond(p)+' ';
    if (p and $00400000<>0) then resD:=resD+'c' else resD:=resD+'s';
    resD:=resD+'psr_';
    if (p and $10000<>0) then resD:=resD+'c';
    if (p and $20000<>0) then resD:=resD+'x';
    if (p and $40000<>0) then resD:=resD+'s';
    if (p and $80000<>0) then resD:=resD+'f';
    resD:=resD+',';
    if (p and $02000000<>0) then begin; {imm}
      resD:=resD+disAsm2hex((p and $ff) shl ((p shr 8) and $f) shl 2,4);
      end else begin;
      resD:=resD+cpuR[p and $f];
      end;
    goto vege;
    end;
  end;

case p and $fe5f0f00 of
  $f84d0500:begin;
    resD:='srs'+abdi[(p shr 23) and 3]+' '+BStr(p and $1f);
    if (p and $00200000<>0) then resD:=resD+'!';
    goto vege;
    end;
  end;

case p and $fe500f00 of
  $f8100a00:begin;
    resD:='rfe'+abdi[(p shr 23) and 3]+' '+cpuR[(p shr 16) and $f];
    if (p and $00200000<>0) then resD:=resD+'!';
    goto vege;
    end;
  end;

case (p and $fe000000) shr 16 of
  $fa00:begin;
    i:=((p and $ffffff) shl 8) div 64;
    o:=((p shr 24) and 1) shl 1;
    if (i<0) then dec(i,o) else inc(i,o);
    resD:='blx '#10+BStr(i+8);
    goto vege;
    end;
  end;

case p and $0e1000f0 of
  $000000d0:begin; a:='ldr%d';goto cndRdMsc; end;
  $001000b0:begin; a:='ldr%h';goto cndRdMsc; end;
  $001000d0:begin; a:='ldr%sb';goto cndRdMsc; end;
  $001000f0:begin; a:='ldr%sh';goto cndRdMsc; end;
  $000000f0:begin; a:='str%d';goto cndRdMsc; end;
  $000000b0:begin; a:='str%h';goto cndRdMsc; end;
  end;

case (p and $0d700000) shr 16 of
  $0470:begin; a:='ldr%bt';goto cndRdWrd; end;
  $0430:begin; a:='ldr%t';goto cndRdWrd; end;
  $0460:begin; a:='str%bt';goto cndRdWrd; end;
  $0420:begin; a:='str%t';goto cndRdWrd; end;
  end;

case (p and $0df00000) shr 16 of
  $0170:begin; a:='cmn%';goto cndRnDat; end;
  $0150:begin; a:='cmp%';goto cndRnDat; end;
  $0130:begin; a:='teq%';goto cndRnDat; end;
  $0110:begin; a:='tst%';goto cndRnDat; end;
  end;

case (p and $0de00000) shr 16 of
  $00a0:begin; a:='adc';goto cndSrdRnDat; end;
  $0080:begin; a:='add';goto cndSrdRnDat; end;
  $0000:begin; a:='and';goto cndSrdRnDat; end;
  $01c0:begin; a:='bic';goto cndSrdRnDat; end;
  $0020:begin; a:='eor';goto cndSrdRnDat; end;
  $01a0:begin; a:='mov';goto cndSrdDat; end;
  $01e0:begin; a:='mvn';goto cndSrdDat; end;
  $0180:begin; a:='orr';goto cndSrdRnDat; end;
  $0060:begin; a:='rsb';goto cndSrdRnDat; end;
  $00e0:begin; a:='rsc';goto cndSrdRnDat; end;
  $00c0:begin; a:='sbc';goto cndSrdRnDat; end;
  $0040:begin; a:='sub';goto cndSrdRnDat; end;
  end;

case (p and $0c500000) shr 16 of
  $0410:begin; a:='ldr%';goto cndRdWrd; end;
  $0450:begin; a:='ldr%b';goto cndRdWrd; end;
  $0400:begin; a:='str%';goto cndRdWrd; end;
  $0440:begin; a:='str%b';goto cndRdWrd; end;
  end;

case p and $0f100010 of
  $0e000010:begin; a:='mcr';goto cndCpOpRdRnRmOp; end;
  $0e100010:begin; a:='mrc';goto cndCpOpRdRnRmOp; end;
  end;

case p and $0f000010 of
  $0e000000:begin;
    resD:='cdp'+decCond(p)+' p'+BStr((p shr 8) and $f)+',';
    kicserel('??','2',resD);
    resD:=resD+BStr((p shr 20) and $f)+','+fpuR[(p shr 12) and $f]+',';
    resD:=resD+fpuR[(p shr 16) and $f]+','+fpuR[p and $f]+','+BStr((p shr 5) and 7);
    goto vege;
    end;
  end;

case (p and $0f000000) shr 16 of
  $0f00:begin;
    resD:='swi'+decCond(p)+' '+disAsm2hex(p and $ffffff,4);
    goto vege;
    end;
  end;

case (p and $0e100000) shr 16 of
  $0810:begin; a:='ldm';goto cndAdrRnRglst; end;
  $0800:begin; a:='stm';goto cndAdrRnRglst; end;
  end;

case (p and $0e100000) shr 16 of
  $0c10:begin; a:='ldc';goto cndCpCdAdr; end;
  $0c00:begin; a:='stc';goto cndCpCdAdr; end;
  end;

case (p and $0e000000) shr 16 of
  $0a00:begin;
    if (p and $01000000<>0) then a:='bl' else a:='b';
    i:=((p and $ffffff) shl 8) div 64;
    resD:=a+decCond(p)+' '#10+BStr(i+8);
    goto vege;
    end;
  end;

goto vege;

cndCpCdAdr:
resD:=a+decCond(p);
kicserel('??','2',resD);
if (p and $00400000<>0) then resD:=resD+'l';
resD:=resD+' p'+BStr((p shr 8) and $f)+',';
resD:=resD+fpuR[(p shr 12) and $f]+','+decCopr(p);
goto vege;

cndAdrRnRglst:
resD:=a+decCond(p)+abdi[(p shr 23) and 3]+' '+cpuR[(p shr 16) and $f];
if (p and $00200000<>0) then resD:=resD+'!';
resD:=resD+','+decRegL(p,0,15);
if (p and $00400000<>0) then resD:=resD+'^';
goto vege;

cndSrdRnDat:
a:=a+decCond(p);
if (p and $00100000<>0) then a:=a+'s';
resD:=a+' '+cpuR[(p shr 12) and $f]+','+cpuR[(p shr 16) and $f]+','+decData(p);
goto vege;

cndSrdDat:
a:=a+decCond(p);
if (p and $00100000<>0) then a:=a+'s';
resD:=a+' '+cpuR[(p shr 12) and $f]+','+decData(p);
goto vege;

cndSrdRmRsRn:
a:=a+decCond(p);
if (p and $00100000<>0) then a:=a+'s';
resD:=a+' '+cpuR[(p shr 16) and $f]+','+cpuR[p and $f]+',';
resD:=resD+cpuR[(p shr 8) and $f]+','+cpuR[(p shr 12) and $f];
goto vege;

cndSrdLoHiRmRs:
a:=a+decCond(p);
if (p and $00100000<>0) then a:=a+'s';
resD:=a+' '+cpuR[(p shr 12) and $f]+','+cpuR[(p shr 16) and $f]+',';
resD:=resD+cpuR[p and $f]+','+cpuR[(p shr 8) and $f];
goto vege;

cndSrdRmRs:
a:=a+decCond(p);
if (p and $00100000<>0) then a:=a+'s';
resD:=a+' '+cpuR[(p shr 16) and $f]+','+cpuR[p and $f]+',';
resD:=resD+cpuR[(p shr 8) and $f];
goto vege;

cndRdRmRn:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[(p shr 12) and $f]+','+cpuR[p and $f]+',';
resD:=resD+'['+cpuR[(p shr 16) and $f]+']';
goto vege;

cndRdRmRsRot:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[(p shr 12) and $f]+','+cpuR[p and $f]+',';
resD:=resD+cpuR[(p shr 16) and $f];
i:=((p shr 10) and 3) shl 3;
if (i>0) then resD:=resD+',ror '+BStr(i);
goto vege;

cndRdRmRs:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[(p shr 12) and $f]+','+cpuR[p and $f]+',';
resD:=resD+cpuR[(p shr 16) and $f];
goto vege;

cndRdRmRs2:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[(p shr 16) and $f]+','+cpuR[p and $f]+',';
resD:=resD+cpuR[(p shr 8) and $f];
goto vege;

cndRdRnRm:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[(p shr 12) and $f]+','+cpuR[(p shr 16) and $f]+',';
resD:=resD+cpuR[p and $f];
goto vege;

cndRdImmRmShft:
resD:=a+decCond(p)+' '+cpuR[(p shr 12) and $f]+','+BStr((p shr 16) and $1f);
if (p and $00000040<>0) then a:='asr' else a:='lsl';
resD:=resD+','+cpuR[p and $f]+','+a+' '+BStr((p shr 7) and $1f);
goto vege;

cndRdImmRm:
resD:=a+decCond(p)+' '+cpuR[(p shr 12) and $f]+','+BStr((p shr 16) and $f);
resD:=resD+','+cpuR[p and $f];
goto vege;

cndRdRnRmImm:
resD:=a+decCond(p)+' '+cpuR[(p shr 12) and $f]+','+cpuR[(p shr 16) and $f];
resD:=resD+','+cpuR[p and $f]+','+BStr((p shr 7) and $1f);
goto vege;

cndCpOpRdRnRmOp:
resD:=a+decCond(p)+' p'+BStr((p shr 8) and $f)+',';
kicserel('??','2',resD);
resD:=resD+BStr((p shr 21) and 7)+','+cpuR[(p shr 12) and $f]+',';
resD:=resD+fpuR[(p shr 16) and $f]+','+fpuR[p and $f]+','+BStr((p shr 5) and 7);
goto vege;

cndCpOpRdRnRm:
resD:=a+decCond(p)+' p'+BStr((p shr 8) and $f)+',';
kicserel('??','2',resD);
resD:=resD+BStr((p shr 4) and 7)+','+cpuR[(p shr 12) and $f]+',';
resD:=resD+cpuR[(p shr 16) and $f]+','+fpuR[p and $f];
goto vege;

cndRm:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[p and $f];
goto vege;

cndRdRm:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[(p shr 12) and $f]+','+cpuR[p and $f];
goto vege;

cndRdRmRot:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[(p shr 12) and $f]+','+cpuR[p and $f];
i:=((p shr 10) and 3) shl 3;
if (i>0) then resD:=resD+',ror '+BStr(i);
goto vege;

cndRnDat:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[(p shr 16) and $f]+','+decData(p);
goto vege;

cndRdWrd:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[(p shr 12) and $f]+','+decWord(p);
goto vege;

cndRdMsc:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[(p shr 12) and $f]+','+decMisc(p);
goto vege;

cndRdAn:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[(p shr 12) and $f]+',['+cpuR[(p shr 16) and $f]+']';
goto vege;

cndRdPSR:
kicserel('%',decCond(p),a);
resD:=a+' '+cpuR[(p shr 12) and $f]+','+b;
goto vege;

Vege:
DisAssemblerBinaryCode:=#4+resD;
End;
