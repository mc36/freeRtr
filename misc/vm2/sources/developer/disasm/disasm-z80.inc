Function DisAssemblerBinaryCode(src:String):String;
Label gcb,gdd,ged,gfd,gxy,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14;
Label f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,vege;
Const
  RegList1:array[0..7] of String[4]=('b','c','d','e','h','l','(hl)','a');
  RegList2:array[0..3] of String[2]=('bc','de','hl','sp');
  RegList3:array[0..3] of String[2]=('bc','de','hl','af');
  FlgList:array[0..7] of String[2]=('nz','z','nc','c','po','pe','p','m');
Var
  resD:String;
  resS:Byte;

Function GetBytes(var i:LongInt;s:Byte):Boolean;
Var
  buf:array[1..8] of byte absolute i;
Begin;
GetBytes:=True;
i:=0;
if (s>sizeof(i)) then Exit;
if (length(src)<s) then Exit;
move(src[1],buf,s);
delete(src,1,s);
i:=ReadLongLSB(buf);
GetBytes:=False;
End;



Var
  i,o,p,q:LongInt;
  a,b,c:String;
Begin;
GetBytes(i,1);
resD:='db '+disAsm2hex(i,1);
resS:=1;

case i of
  $cb:goto gcb;
  $dd:goto gdd;
  $ed:goto ged;
  $fd:goto gfd;
  $00:begin; resD:='nop';goto vege; end;
  $06,$0e,$16,$1e,$26,$2e,$36,$3e:begin; a:='ld';goto f2; end;
  $0a:begin; resD:='ld a,(bc)';goto vege; end;
  $1a:begin; resD:='ld a,(de)';goto vege; end;
  $02:begin; resD:='ld (bc),a';goto vege; end;
  $12:begin; resD:='ld (de),a';goto vege; end;
  $3a:begin; a:='ld';b:='a';goto f6; end;
  $32:begin; a:='ld';b:='a';goto f7; end;
  $01,$11,$21,$31:begin; a:='ld';goto f8; end;
  $2a:begin; a:='ld';b:='hl';goto f6; end;
  $22:begin; a:='ld';b:='hl';goto f7; end;
  $f9:begin; resD:='ld sp,hl';goto vege; end;
  $c5,$d5,$e5,$f5:begin; a:='push';goto f11; end;
  $c1,$d1,$e1,$f1:begin; a:='pop';goto f11; end;
  $eb:begin; resD:='ex de,hl';goto vege; end;
  $08:begin; resD:='ex af,af';goto vege; end;
  $d9:begin; resD:='exx';goto vege; end;
  $e3:begin; resD:='ex (sp),hl';goto vege; end;
  $80..$87:begin; a:='add a,';goto f12; end;
  $c6:begin; a:='add a,';goto f13; end;
  $88..$8f:begin; a:='adc a,';goto f12; end;
  $ce:begin; a:='adc a,';goto f13; end;
  $90..$97:begin; a:='sub a,';goto f12; end;
  $d6:begin; a:='sub a,';goto f13; end;
  $98..$9f:begin; a:='sbc a,';goto f12; end;
  $de:begin; a:='sbc a,';goto f13; end;
  $a0..$a7:begin; a:='and a,';goto f12; end;
  $e6:begin; a:='and a,';goto f13; end;
  $a8..$af:begin; a:='xor a,';goto f12; end;
  $ee:begin; a:='xor a,';goto f13; end;
  $b0..$b7:begin; a:='or a,';goto f12; end;
  $f6:begin; a:='or a,';goto f13; end;
  $b8..$bf:begin; a:='cp a,';goto f12; end;
  $fe:begin; a:='cp a,';goto f13; end;
  $04,$0c,$14,$1c,$24,$2c,$34,$3c:begin; a:='inc';b:='';goto f15; end;
  $05,$0d,$15,$1d,$25,$2d,$35,$3d:begin; a:='dec';b:='';goto f15; end;
  $27:begin; resD:='daa';goto vege; end;
  $2f:begin; resD:='cpl';goto vege; end;
  $09,$19,$29,$39:begin; a:='add';b:='hl';goto f16; end;
  $03,$13,$23,$33:begin; a:='inc';goto f17; end;
  $0b,$1b,$2b,$3b:begin; a:='dec';goto f17; end;
  $07:begin; resD:='rlca';goto vege; end;
  $17:begin; resD:='rla';goto vege; end;
  $0f:begin; resD:='rrca';goto vege; end;
  $1f:begin; resD:='rra';goto vege; end;
  $3f:begin; resD:='ccf';goto vege; end;
  $37:begin; resD:='scf';goto vege; end;
  $00:begin; resD:='nop';goto vege; end;
  $76:begin; resD:='halt';goto vege; end;
  $f3:begin; resD:='di';goto vege; end;
  $fb:begin; resD:='ei';goto vege; end;
  $40..$7f:begin; a:='ld';goto f1; end;
  $c3:begin; a:='jp';goto f21; end;
  $cd:begin; a:='call';goto f21; end;
  $e9:begin; resD:='jp hl';goto vege; end;
  $c9:begin; resD:='ret';goto vege; end;
  $c7,$cf,$d7,$df,$e7,$ef,$f7,$ff:begin; a:='rst';goto f22; end;
  $c0,$c8,$d0,$d8,$e0,$e8,$f0,$f8:begin; a:='ret';goto f23; end;
  $c4,$cc,$d4,$dc,$e4,$ec,$f4,$fc:begin; a:='call';goto f24; end;
  $c2,$ca,$d2,$da,$e2,$ea,$f2,$fa:begin; a:='jp';goto f24; end;
  $18:begin; a:='jr';goto f25; end;
  $20,$28,$30,$38:begin; dec(i,$20);a:='jr';goto f26; end;
  $10:begin; a:='djnz';goto f25; end;
  $db:begin; a:='in';b:='a';c:='';goto f27; end;
  $d3:begin; a:='out';b:='';c:='a';goto f27; end;
  end;
goto vege;

gcb:
if GetBytes(i,1) then goto vege;
inc(resS,1);
resD:=resD+','+disAsm2hex(i,1);
case i of
  $00..$07:begin; a:='rlc ';goto f12; end;
  $10..$17:begin; a:='rl ';goto f12; end;
  $08..$0f:begin; a:='rrc ';goto f12; end;
  $18..$1f:begin; a:='rr ';goto f12; end;
  $20..$27:begin; a:='sla ';goto f12; end;
  $30..$37:begin; a:='sll ';goto f12; end;
  $28..$2f:begin; a:='sra ';goto f12; end;
  $38..$3f:begin; a:='srl ';goto f12; end;
  $40..$7f:begin; a:='bit';goto f18; end;
  $80..$bf:begin; a:='res';goto f18; end;
  $c0..$ff:begin; a:='set';goto f18; end;
  end;
goto vege;

ged:
if GetBytes(i,1) then goto vege;
inc(resS,1);
resD:=resD+','+disAsm2hex(i,1);
case i of
  $57:begin; resD:='ld a,i';goto vege; end;
  $5f:begin; resD:='ld a,r';goto vege; end;
  $47:begin; resD:='ld i,a';goto vege; end;
  $4f:begin; resD:='ld r,a';goto vege; end;
  $4b:begin; a:='ld';b:='bc';goto f6; end;
  $5b:begin; a:='ld';b:='de';goto f6; end;
  $7b:begin; a:='ld';b:='sp';goto f6; end;
  $43:begin; a:='ld';b:='bc';goto f7; end;
  $53:begin; a:='ld';b:='de';goto f7; end;
  $73:begin; a:='ld';b:='sp';goto f7; end;
  $a0:begin; resD:='ldi';goto vege; end;
  $a8:begin; resD:='ldd';goto vege; end;
  $a1:begin; resD:='cpi';goto vege; end;
  $a9:begin; resD:='cpd';goto vege; end;
  $b0:begin; resD:='ldir';goto vege; end;
  $b8:begin; resD:='lddr';goto vege; end;
  $b1:begin; resD:='cpir';goto vege; end;
  $b9:begin; resD:='cpdr';goto vege; end;
  $44:begin; resD:='neg';goto vege; end;
  $4a,$5a,$6a,$7a:begin; a:='adc';b:='hl';goto f16; end;
  $42,$52,$62,$72:begin; a:='sbc';b:='hl';goto f16; end;
  $6f:begin; resD:='rld';goto vege; end;
  $67:begin; resD:='rrd';goto vege; end;
  $46:begin; resD:='im 0';goto vege; end;
  $56:begin; resD:='im 1';goto vege; end;
  $5e:begin; resD:='im 2';goto vege; end;
  $4d:begin; resD:='reti';goto vege; end;
  $45:begin; resD:='retn';goto vege; end;
  $40,$48,$50,$58,$60,$68,$70,$78:begin; a:='in';b:=',(bc)';goto f15; end;
  $41,$49,$51,$59,$61,$69,$71,$79:begin; a:='out (bc),';goto f28; end;
  $a2:begin; resD:='ini';goto vege; end;
  $aa:begin; resD:='ind';goto vege; end;
  $b2:begin; resD:='inir';goto vege; end;
  $ba:begin; resD:='indr';goto vege; end;
  $a3:begin; resD:='outi';goto vege; end;
  $ab:begin; resD:='outd';goto vege; end;
  $b3:begin; resD:='otir';goto vege; end;
  $bb:begin; resD:='otdr';goto vege; end;
  end;
goto vege;

gxy:
case i of
  $46,$4e,$56,$5e,$66,$6e,$7e:begin; a:='ld';b:='i'+c;goto f3; end;
  $70,$71,$72,$73,$74,$75,$77:begin; a:='ld';b:='i'+c;goto f4; end;
  $36:begin; a:='ld';b:='i'+c;goto f5; end;
  $21:begin; a:='ld';b:='i'+c;goto f9; end;
  $60..$67:begin; a:='ld';b:='i'+c+'h';goto f10; end;
  $68..$6f:begin; a:='ld';b:='i'+c+'l';goto f10; end;
  $2a:begin; a:='ld';b:='i'+c;goto f6; end;
  $22:begin; a:='ld';b:='i'+c;goto f7; end;
  $f9:begin; resD:='ld sp,i'+c;goto vege; end;
  $e5:begin; resD:='push i'+c;goto vege; end;
  $e1:begin; resD:='pop i'+c;goto vege; end;
  $e3:begin; resD:='ex (sp),i'+c;goto vege; end;
  $84:begin; resD:='add a,i'+c+'h';goto vege; end;
  $85:begin; resD:='add a,i'+c+'l';goto vege; end;
  $86:begin; a:='add a,(i'+c+'+';goto f14; end;
  $8c:begin; resD:='adc a,i'+c+'h';goto vege; end;
  $8d:begin; resD:='adc a,i'+c+'l';goto vege; end;
  $8e:begin; a:='adc a,(i'+c+'+';goto f14; end;
  $94:begin; resD:='sub a,i'+c+'h';goto vege; end;
  $95:begin; resD:='sub a,i'+c+'l';goto vege; end;
  $96:begin; a:='sub a,(i'+c+'+';goto f14; end;
  $9c:begin; resD:='sbc a,i'+c+'h';goto vege; end;
  $9d:begin; resD:='sbc a,i'+c+'l';goto vege; end;
  $9e:begin; a:='sbc a,(i'+c+'+';goto f14; end;
  $a4:begin; resD:='and a,i'+c+'h';goto vege; end;
  $a5:begin; resD:='and a,i'+c+'l';goto vege; end;
  $a6:begin; a:='and a,(i'+c+'+';goto f14; end;
  $ac:begin; resD:='xor a,i'+c+'h';goto vege; end;
  $ad:begin; resD:='xor a,i'+c+'l';goto vege; end;
  $ae:begin; a:='xor a,(i'+c+'+';goto f14; end;
  $b4:begin; resD:='or a,i'+c+'h';goto vege; end;
  $b5:begin; resD:='or a,i'+c+'l';goto vege; end;
  $b6:begin; a:='or a,(i'+c+'+';goto f14; end;
  $bc:begin; resD:='cp a,i'+c+'h';goto vege; end;
  $bd:begin; resD:='cp a,i'+c+'l';goto vege; end;
  $be:begin; a:='cp a,(i'+c+'+';goto f14; end;
  $24:begin; resD:='inc i'+c+'h';goto vege; end;
  $2c:begin; resD:='inc i'+c+'l';goto vege; end;
  $25:begin; resD:='dec i'+c+'h';goto vege; end;
  $2d:begin; resD:='dec i'+c+'l';goto vege; end;
  $34:begin; a:='inc (i'+c+'+';goto f14; end;
  $35:begin; a:='dec (i'+c+'+';goto f14; end;
  $44,$4c,$54,$5c,$7c:begin; a:='ld';b:=',i'+c+'h';goto f15; end;
  $45,$4d,$55,$5d,$7d:begin; a:='ld';b:=',i'+c+'l';goto f15; end;
  $09,$19,$29,$39:begin; a:='add';b:='i'+c;goto f16; end;
  $23:begin; resD:='inc i'+c;goto vege; end;
  $2b:begin; resD:='dec i'+c;goto vege; end;
  $cb:begin;
    if GetBytes(o,1) then goto vege;
    inc(resS,1);
    if GetBytes(i,1) then goto vege;
    inc(resS,1);
    resD:=resD+','+disAsm2hex(o,1)+','+disAsm2hex(i,1);
    case i of
      $06:begin; a:='rlc';goto f19; end;
      $16:begin; a:='rl';goto f19; end;
      $0e:begin; a:='rrc';goto f19; end;
      $1e:begin; a:='rr';goto f19; end;
      $26:begin; a:='sla';goto f19; end;
      $36:begin; a:='sll';goto f19; end;
      $2e:begin; a:='sra';goto f19; end;
      $3e:begin; a:='srl';goto f19; end;
      $46,$4e,$56,$5e,$66,$6e,$76,$7e:begin; a:='bit';goto f20; end;
      $c6,$ce,$d6,$de,$e6,$ee,$f6,$fe:begin; a:='set';goto f20; end;
      $86,$8e,$96,$9e,$a6,$ae,$b6,$be:begin; a:='res';goto f20; end;
      end;
    goto vege;
    end;
  $e9:begin; resD:='jp i'+c;goto vege; end;
  end;
goto vege;

gdd:
if GetBytes(i,1) then goto vege;
inc(resS,1);
resD:=resD+','+disAsm2hex(i,1);
{case i of
  else begin; c:='x';goto gxy; end;
  end;}
c:='x';
goto gxy;
goto vege;

gfd:
if GetBytes(i,1) then goto vege;
inc(resS,1);
resD:=resD+','+disAsm2hex(i,1);
{case i of
  else begin; c:='y';goto gxy; end;
  end;}
c:='y';
goto gxy;
goto vege;

f1:
resD:=a+' '+RegList1[(i shr 3) and 7]+','+RegList1[i and 7];
goto vege;

f2:
if GetBytes(o,1) then goto vege;
inc(resS,1);
resD:=a+' '+RegList1[(i shr 3) and 7]+','+disAsm2hex(o,1);
goto vege;

f3:
if GetBytes(o,1) then goto vege;
inc(resS,1);
resD:=a+' '+RegList1[(i shr 3) and 7]+',('+b+'+'+disAsm2hex(o,1)+')';
goto vege;

f4:
if GetBytes(o,1) then goto vege;
inc(resS,1);
resD:=a+' ('+b+'+'+disAsm2hex(o,1)+'),'+RegList1[i and 7];
goto vege;

f5:
if GetBytes(o,1) then goto vege;
inc(resS,1);
if GetBytes(p,1) then goto vege;
inc(resS,1);
resD:=a+' ('+b+'+'+disAsm2hex(o,1)+'),'+disAsm2hex(p,1);
goto vege;

f6:
if GetBytes(o,2) then goto vege;
inc(resS,2);
resD:=a+' '+b+',('+disAsm2hex(o,2)+')';
goto vege;

f7:
if GetBytes(o,2) then goto vege;
inc(resS,2);
resD:=a+' ('+disAsm2hex(o,2)+'),'+b;
goto vege;

f8:
if GetBytes(o,2) then goto vege;
inc(resS,2);
resD:=a+' '+RegList2[(i shr 4) and 3]+','+disAsm2hex(o,2);
goto vege;

f9:
if GetBytes(o,2) then goto vege;
inc(resS,2);
resD:=a+' '+b+','+disAsm2hex(o,2);
goto vege;

f10:
o:=i and 7;
c:=RegList1[o];
if (o in [4,5]) then c:=copy(b,1,2)+c;
resD:=a+' '+b+','+c;
goto vege;

f11:
resD:=a+' '+RegList3[(i shr 4) and 3];
goto vege;

f12:
resD:=a+RegList1[i and 7];
goto vege;

f13:
if GetBytes(o,1) then goto vege;
inc(resS,1);
resD:=a+disAsm2hex(o,1);
goto vege;

f14:
if GetBytes(o,1) then goto vege;
inc(resS,1);
resD:=a+disAsm2hex(o,1)+')';
goto vege;

f15:
resD:=a+' '+RegList1[(i shr 3) and 7]+b;
goto vege;

f16:
o:=(i shr 4) and 3;
c:=RegList2[o];
if (o=2) then c:=b;
resD:=a+' '+b+','+c;
goto vege;

f17:
resD:=a+' '+RegList2[(i shr 4) and 3];
goto vege;

f18:
resD:=a+' '+BStr((i shr 3) and 7)+','+RegList1[i and 7];
goto vege;

f19:
resD:=a+' (i'+c+'+'+disAsm2hex(o,1)+')';
goto vege;

f20:
resD:=a+' '+BStr((i shr 3) and 7)+',(i'+c+'+'+disAsm2hex(o,1)+')';
goto vege;

f21:
if GetBytes(o,2) then goto vege;
inc(resS,2);
resD:=a+' '+disAsm2hex(o,2);
goto vege;

f22:
resD:=a+' '+BStr((i shr 3) and 7);
goto vege;

f23:
resD:=a+' '+FlgList[(i shr 3) and 7];
goto vege;

f24:
if GetBytes(o,2) then goto vege;
inc(resS,2);
resD:=a+' '+FlgList[(i shr 3) and 7]+','+disAsm2hex(o,2);
goto vege;

f25:
if GetBytes(o,1) then goto vege;
inc(resS,1);
o:=shortint(o);
resD:=a+' '#10+BStr(o+2);
goto vege;

f26:
if GetBytes(o,1) then goto vege;
inc(resS,1);
o:=shortint(o);
resD:=a+' '+FlgList[(i shr 3) and 7]+','+#10+BStr(o+2);
goto vege;

f27:
if GetBytes(o,1) then goto vege;
inc(resS,1);
resD:=a+' ';
a:='('+disAsm2hex(o,1)+')';
if (b='') then b:=a;
if (c='') then c:=a;
resD:=resD+b+','+c;
goto vege;

f28:
resD:=a+RegList1[(i shr 3) and 7];
goto vege;

Vege:
DisAssemblerBinaryCode:=chr(resS)+resD;
End;
