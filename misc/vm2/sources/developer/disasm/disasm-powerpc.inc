Function DisAssemblerBinaryCode(src:String):String;
Label dab,dai,da,sab,sai,boi,lcab,lcai,sa,nnn,ab,dm,danb,cc,c,d,dxi,id,sannn;
Label sabnn,san,dan,nab,nai,vege;
Const
  cpuRegs:array[0..31] of String[3]=('r0','r1','r2','r3','r4','r5','r6','r7',
    'r8','r9','r10','r11','r12','r13','r14','r15','r16','r17','r18','r19',
    'r20','r21','r22','r23','r24','r25','r26','r27','r28','r29','r30','r31');
  cndRegs:array[0..7] of String[3]=('cr0','cr1','cr2','cr3','cr4','cr5','cr6','cr7');

Function condBI(i:LongInt):String;
Var a:String;
Begin;
case i of
  0:a:='lt';
  1:a:='gt';
  2:a:='eq';
  3:a:='so';
  else a:=BStr(i);
  end;
condBI:=a;
End;

Function condBO(i:LongInt):String;
Var a:String;
Begin;
case i and $fe of
  4 :a:='f';
  2 :a:='fz';
  0 :a:='fnz';
  12:a:='t';
  10:a:='tz';
  8 :a:='tnz';
  20:a:='a';
  18:a:='z';
  16:a:='nz';
  else a:=BStr(i);
  end;
if (i and 1<>0) then a:='l'+a;
condBO:=a;
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
case p shr 26 of
  $1f:begin;
    i:=p and $7ff;
    case i of
      $214:begin; a:='add';goto dab; end;
      $215:begin; a:='add.';goto dab; end;
      $614:begin; a:='addo';goto dab; end;
      $615:begin; a:='addo.';goto dab; end;
      $014:begin; a:='addc';goto dab; end;
      $015:begin; a:='addc.';goto dab; end;
      $414:begin; a:='addco';goto dab; end;
      $415:begin; a:='addco.';goto dab; end;
      $114:begin; a:='adde';goto dab; end;
      $115:begin; a:='adde.';goto dab; end;
      $514:begin; a:='addeo';goto dab; end;
      $515:begin; a:='addeo.';goto dab; end;
      $1d4:begin; a:='addme';goto da; end;
      $1d5:begin; a:='addme.';goto da; end;
      $5d4:begin; a:='addmeo';goto da; end;
      $5d5:begin; a:='addmeo.';goto da; end;
      $194:begin; a:='addze';goto da; end;
      $195:begin; a:='addze.';goto da; end;
      $594:begin; a:='addzeo';goto da; end;
      $595:begin; a:='addzeo.';goto da; end;
      $038:begin; a:='and';goto sab; end;
      $039:begin; a:='and.';goto sab; end;
      $078:begin; a:='andc';goto sab; end;
      $079:begin; a:='andc.';goto sab; end;
      $000:begin; a:='cmp';goto lcab; end;
      $040:begin; a:='cmpl';goto lcab; end;
      $034:begin; a:='cntlzw';goto sa; end;
      $035:begin; a:='cntlzw.';goto sa; end;
      $3d6:begin; a:='divw';goto dab; end;
      $3d7:begin; a:='divw.';goto dab; end;
      $7d6:begin; a:='divwo';goto dab; end;
      $7d7:begin; a:='divwo.';goto dab; end;
      $396:begin; a:='divwu';goto dab; end;
      $397:begin; a:='divwu.';goto dab; end;
      $796:begin; a:='divwuo';goto dab; end;
      $797:begin; a:='divwuo.';goto dab; end;
      $6ac:begin; resD:='eieio';goto vege; end;
      $238:begin; a:='eqv';goto sab; end;
      $239:begin; a:='eqv.';goto sab; end;
      $774:begin; a:='extsb';goto sa; end;
      $775:begin; a:='extsb.';goto sa; end;
      $734:begin; a:='extsh';goto sa; end;
      $735:begin; a:='extsh.';goto sa; end;
      $7ac:begin; a:='icbi';goto ab; end;
      $0ee:begin; a:='lbzux';goto dab; end;
      $0ae:begin; a:='lbzx';goto dab; end;
      $2ee:begin; a:='lhaux';goto dab; end;
      $2ae:begin; a:='lhax';goto dab; end;
      $62c:begin; a:='lhbrx';goto dab; end;
      $26e:begin; a:='lhzux';goto dab; end;
      $22e:begin; a:='lhzx';goto dab; end;
      $4aa:begin; a:='lswi';goto danb; end;
      $42a:begin; a:='lswx';goto dab; end;
      $028:begin; a:='lwarx';goto dab; end;
      $42c:begin; a:='lwbrx';goto dab; end;
      $06e:begin; a:='lwzux';goto dab; end;
      $02e:begin; a:='lwzx';goto dab; end;
      $400:begin; a:='mcrxr';goto c; end;
      $026:begin; a:='mfcr';goto d; end;
      $0a6:begin; a:='mfmsr';goto d; end;
      $2a6:begin; a:='mfspr';goto dxi; end;
      $2e6:begin; a:='mftb';goto dxi; end;
      $120:begin; a:='mtcrf';goto id; end;
      $124:begin; a:='mtmsr';goto d; end;
      $3a6:begin; a:='mtspr';goto dxi; end;
      $096:begin; a:='mulhw';goto dab; end;
      $097:begin; a:='mulhw.';goto dab; end;
      $016:begin; a:='mulhwu';goto dab; end;
      $017:begin; a:='mulhwu.';goto dab; end;
      $1d6:begin; a:='mullw';goto dab; end;
      $1d7:begin; a:='mullw.';goto dab; end;
      $5d6:begin; a:='mullwo';goto dab; end;
      $5d7:begin; a:='mullwo.';goto dab; end;
      $3b8:begin; a:='nand';goto sab; end;
      $3b9:begin; a:='nand.';goto sab; end;
      $0d0:begin; a:='neg';goto da; end;
      $0d1:begin; a:='neg.';goto da; end;
      $4d0:begin; a:='nego';goto da; end;
      $4d1:begin; a:='nego.';goto da; end;
      $0f8:begin; a:='nor';goto sab; end;
      $0f9:begin; a:='nor.';goto sab; end;
      $378:begin; a:='or';goto sab; end;
      $379:begin; a:='or.';goto sab; end;
      $338:begin; a:='orc';goto sab; end;
      $339:begin; a:='orc.';goto sab; end;
      $030:begin; a:='slw';goto sab; end;
      $031:begin; a:='slw.';goto sab; end;
      $630:begin; a:='sraw';goto sab; end;
      $631:begin; a:='sraw.';goto sab; end;
      $670:begin; a:='srawi';goto san; end;
      $671:begin; a:='srawi.';goto san; end;
      $430:begin; a:='srw';goto sab; end;
      $431:begin; a:='srw.';goto sab; end;
      $1ee:begin; a:='stbux';goto dab; end;
      $1ae:begin; a:='stbx';goto dab; end;
      $72c:begin; a:='sthbrx';goto dab; end;
      $36e:begin; a:='sthux';goto dab; end;
      $32e:begin; a:='sthx';goto dab; end;
      $5aa:begin; a:='stswi';goto dan; end;
      $52a:begin; a:='stswx';goto dab; end;
      $12d:begin; a:='stwcx.';goto dab; end;
      $16e:begin; a:='stwux';goto dab; end;
      $12e:begin; a:='stwx';goto dab; end;
      $52c:begin; a:='stwbrx';goto dab; end;
      $050:begin; a:='subf';goto dab; end;
      $051:begin; a:='subf.';goto dab; end;
      $450:begin; a:='subfo';goto dab; end;
      $451:begin; a:='subfo.';goto dab; end;
      $010:begin; a:='subfc';goto dab; end;
      $011:begin; a:='subfc.';goto dab; end;
      $410:begin; a:='subfco';goto dab; end;
      $411:begin; a:='subfco.';goto dab; end;
      $110:begin; a:='subfe';goto dab; end;
      $111:begin; a:='subfe.';goto dab; end;
      $510:begin; a:='subfeo';goto dab; end;
      $511:begin; a:='subfeo.';goto dab; end;
      $1d0:begin; a:='subfme';goto da; end;
      $1d1:begin; a:='subfme.';goto da; end;
      $5d0:begin; a:='subfmeo';goto da; end;
      $5d1:begin; a:='subfmeo.';goto da; end;
      $190:begin; a:='subfze';goto da; end;
      $191:begin; a:='subfze.';goto da; end;
      $590:begin; a:='subfzeo';goto da; end;
      $591:begin; a:='subfzeo.';goto da; end;
      $4ac:begin; resD:='sync';goto vege; end;
      $008:begin; a:='tw';goto nab; end;
      $278:begin; a:='xor';goto sab; end;
      $279:begin; a:='xor.';goto sab; end;
      end;
    end;
  $0e:begin; a:='addi';goto dai; end;
  $0c:begin; a:='addic';goto dai; end;
  $0d:begin; a:='addic.';goto dai; end;
  $0f:begin; a:='addis';goto dai; end;
  $1c:begin; a:='andi.';goto sai; end;
  $1d:begin; a:='andis.';goto sai; end;
  $13:begin;
    i:=p and $7ff;
    case i of
      $420:begin; a:='bcctr';goto boi; end;
      $421:begin; a:='bcctrl';goto boi; end;
      $020:begin; a:='bclr';goto boi; end;
      $021:begin; a:='bclrl';goto boi; end;
      $202:begin; a:='crand';goto nnn; end;
      $102:begin; a:='crandc';goto nnn; end;
      $242:begin; a:='creqv';goto nnn; end;
      $1c2:begin; a:='crnand';goto nnn; end;
      $042:begin; a:='crnor';goto nnn; end;
      $382:begin; a:='cror';goto nnn; end;
      $342:begin; a:='crorc';goto nnn; end;
      $182:begin; a:='crxor';goto nnn; end;
      $12c:begin; resD:='isync';goto vege; end;
      $000:begin; a:='mcrf';goto cc; end;
      $064:begin; resD:='rfi';goto vege; end;
      end;
    end;
  $0b:begin; a:='cmp';goto lcai; end;
  $0a:begin; a:='cmpl';goto lcai; end;
  $3b:begin; {fpu}
    end;
  $3f:begin; {fpu}
    end;
  $22:begin; a:='lbz';goto dm; end;
  $23:begin; a:='lbzu';goto dm; end;
  $2a:begin; a:='lha';goto dm; end;
  $2b:begin; a:='lhau';goto dm; end;
  $28:begin; a:='lhz';goto dm; end;
  $29:begin; a:='lhzu';goto dm; end;
  $2e:begin; a:='lmw';goto dm; end;
  $20:begin; a:='lwz';goto dm; end;
  $21:begin; a:='lwzu';goto dm; end;
  $07:begin; a:='mulli';goto dai; end;
  $18:begin; a:='ori';goto sai; end;
  $19:begin; a:='oris';goto sai; end;
  $14:begin; a:='rlwimi';goto sannn; end;
  $15:begin; a:='rlwinm';goto sannn; end;
  $17:begin; a:='rlwnm';goto sabnn; end;
  $11:begin; resD:='sc';goto vege; end;
  $26:begin; a:='stb';goto dm; end;
  $27:begin; a:='stbu';goto dm; end;
  $2c:begin; a:='sth';goto dm; end;
  $2d:begin; a:='sthu';goto dm; end;
  $2f:begin; a:='stmw';goto dm; end;
  $24:begin; a:='stw';goto dm; end;
  $25:begin; a:='stwu';goto dm; end;
  $08:begin; a:='subfic';goto dai; end;
  $03:begin; a:='twi';goto nai; end;
  $1a:begin; a:='xori';goto sai; end;
  $1b:begin; a:='xoris';goto sai; end;
  $12:begin;
    resD:='b';
    i:=p and $3fffffc;
    if (p and 1<>0) then resD:=resD+'l';
    if (p and 2<>0) then begin;
      resD:=resD+'a '+disAsm2hex(i,4);
      goto vege;
      end;
    resD:=resD+' '#10+disAsm2hex(i,4);
    goto vege;
    end;
  $10:begin;
    resD:='bc';
    a:=' '+condBO((p shr 21) and $1f)+','+condBI((p shr 16) and $1f)+',';
    i:=p and $fffc;
    if (p and 1<>0) then resD:=resD+'l';
    if (p and 2<>0) then begin;
      resD:=resD+'a'+a+disAsm2hex(i,4);
      goto vege;
      end;
    resD:=resD+a+#10+disAsm2hex(i,4);
    goto vege;
    end;
  end;
goto vege;

dab:
resD:=a+' '+cpuRegs[(p shr 21) and $1f]+','+cpuRegs[(p shr 16) and $1f]+','+cpuRegs[(p shr 11) and $1f];
goto vege;

dai:
resD:=a+' '+cpuRegs[(p shr 21) and $1f]+','+cpuRegs[(p shr 16) and $1f]+','+disAsm2hex(p,2);
goto vege;

da:
resD:=a+' '+cpuRegs[(p shr 21) and $1f]+','+cpuRegs[(p shr 16) and $1f];
goto vege;

sab:
resD:=a+' '+cpuRegs[(p shr 16) and $1f]+','+cpuRegs[(p shr 21) and $1f]+','+cpuRegs[(p shr 11) and $1f];
goto vege;

sai:
resD:=a+' '+cpuRegs[(p shr 16) and $1f]+','+cpuRegs[(p shr 21) and $1f]+','+disAsm2hex(p,2);
goto vege;

boi:
resD:=a+' '+condBO((p shr 21) and $1f)+','+condBI((p shr 16) and $1f);
goto vege;

lcab:
if (p and $200000<>0) then a:=a+'d' else a:=a+'w';
resD:=a+' '+cndRegs[(p shr 23) and 7]+','+cpuRegs[(p shr 16) and $1f]+','+cpuRegs[(p shr 11) and $1f];
goto vege;

lcai:
if (p and $200000<>0) then a:=a+'d' else a:=a+'w';
a:=a+'i';
resD:=a+' '+cndRegs[(p shr 23) and 7]+','+cpuRegs[(p shr 16) and $1f]+','+disAsm2hex(p,2);
goto vege;

sa:
resD:=a+' '+cpuRegs[(p shr 16) and $1f]+','+cpuRegs[(p shr 21) and $1f];
goto vege;

nnn:
resD:=a+' '+BStr((p shr 21) and $1f)+','+BStr((p shr 16) and $1f)+','+BStr((p shr 11) and $1f);
goto vege;

ab:
resD:=a+' '+cpuRegs[(p shr 16) and $1f]+','+cpuRegs[(p shr 11) and $1f];
goto vege;

dm:
resD:=a+' '+cpuRegs[(p shr 21) and $1f]+','+disAsm2hex(p,2)+'('+cpuRegs[(p shr 16) and $1f]+')';
goto vege;

danb:
resD:=a+' '+cpuRegs[(p shr 21) and $1f]+','+cpuRegs[(p shr 16) and $1f]+','+BStr((p shr 11) and $1f);
goto vege;

cc:
resD:=a+' '+cndRegs[(p shr 23) and 7]+','+cndRegs[(p shr 18) and 7];
goto vege;

c:
resD:=a+' '+cndRegs[(p shr 23) and 7];
goto vege;

d:
resD:=a+' '+cpuRegs[(p shr 21) and $1f];
goto vege;

dxi:
i:=(p shr 11) and $3ff;
i:=((i shr 5) or (i shl 5)) and $3ff;
resD:=a+' '+cpuRegs[(p shr 21) and $1f]+','+BStr(i);
goto vege;

id:
resD:=a+' '+BStr((p shr 12) and $ff)+','+cpuRegs[(p shr 21) and $1f];
goto vege;

sannn:
if (p and 1<>0) then a:=a+'.';
resD:=a+' '+cpuRegs[(p shr 16) and $1f]+','+cpuRegs[(p shr 21) and $1f]+','+
  BStr((p shr 11) and $1f)+','+BStr((p shr 6) and $1f)+','+BStr((p shr 1) and $1f);
goto vege;

sabnn:
if (p and 1<>0) then a:=a+'.';
resD:=a+' '+cpuRegs[(p shr 16) and $1f]+','+cpuRegs[(p shr 21) and $1f]+','+
  cpuRegs[(p shr 11) and $1f]+','+BStr((p shr 6) and $1f)+','+BStr((p shr 1) and $1f);
goto vege;

san:
resD:=a+' '+cpuRegs[(p shr 16) and $1f]+','+cpuRegs[(p shr 21) and $1f]+','+BStr((p shr 11) and $1f);
goto vege;

dan:
resD:=a+' '+cpuRegs[(p shr 21) and $1f]+','+cpuRegs[(p shr 16) and $1f]+','+BStr((p shr 11) and $1f);
goto vege;

nab:
resD:=a+' '+BStr((p shr 21) and $1f)+','+cpuRegs[(p shr 16) and $1f]+','+cpuRegs[(p shr 11) and $1f];
goto vege;

nai:
resD:=a+' '+BStr((p shr 21) and $1f)+','+cpuRegs[(p shr 16) and $1f]+','+disAsm2hex(p,2);
goto vege;


Vege:
DisAssemblerBinaryCode:=#4+resD;
End;
