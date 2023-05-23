Function DisAssemblerBinaryCode(src:String):String;
Label i8deii,i16deii,abeii,imm8,imm16,dir,ext,ind,br8,dirmsk,dirmskrel,vege;
Var
  resD:String;
  resS:Byte;

Function GetBytes(var i:LongInt;s:Byte):Boolean;
Var buf:array[1..8] of byte absolute i;
Begin;
GetBytes:=True;
i:=0;
if (s>sizeof(i)) then Exit;
if (length(src)<s) then Exit;
move(src[1],buf[5-s],s);
delete(src,1,s);
i:=ReadLongMSB(buf);
inc(resS,s);
GetBytes:=False;
End;


Var
  sawPrefix:Byte; {0=none, 1=18, 2=1a, 3=cd}
  defaultPt:String; {default pointer register}
  i,o,p,q:LongInt;
  a,b,c:String;
Begin;
resS:=0;
GetBytes(i,1);
resD:='db '+disAsm2hex(i,1);
sawPrefix:=0;
defaultPt:='x';

case i of
  $18:begin;
    sawPrefix:=1;
    defaultPt:='y';
    end;
  $1a:begin;
    sawPrefix:=2;
    end;
  $cd:begin;
    sawPrefix:=3;
    defaultPt:='y';
    end;
  end;
if (sawPrefix<>0) then begin;
  if GetBytes(i,1) then exit;
  resD:=resD+','+disAsm2hex(i,1);
  end;

case i of
  $1b:resD:='aba';
  $3a:resD:='ab'+defaultPt;
  $89,$99,$b9,$a9:begin; resD:='adca';o:=$89;goto i8deii; end;
  $c9,$d9,$f9,$e9:begin; resD:='adcb';o:=$c9;goto i8deii; end;
  $8b,$9b,$bb,$ab:begin; resD:='adda';o:=$8b;goto i8deii; end;
  $cb,$db,$fb,$eb:begin; resD:='addb';o:=$cb;goto i8deii; end;
  $c3,$d3,$f3,$e3:begin; resD:='addd';o:=$c3;goto i16deii; end;
  $84,$94,$b4,$a4:begin; resD:='anda';o:=$84;goto i8deii; end;
  $c4,$d4,$f4,$e4:begin; resD:='andb';o:=$c4;goto i8deii; end;
  $48,$58,$78,$68:begin; resD:='asl';o:=$48;goto abeii; end;
  $05:resD:='asld';
  $47,$57,$77,$67:begin; resD:='asr';o:=$47;goto abeii; end;
  $20:begin; resD:='bra';goto br8; end;
  $21:begin; resD:='brn';goto br8; end;
  $22:begin; resD:='bhi';goto br8; end;
  $23:begin; resD:='bls';goto br8; end;
  $24:begin; resD:='bcc';goto br8; end;
  $25:begin; resD:='bcs';goto br8; end;
  $26:begin; resD:='bne';goto br8; end;
  $27:begin; resD:='beq';goto br8; end;
  $28:begin; resD:='bvc';goto br8; end;
  $29:begin; resD:='bvs';goto br8; end;
  $2a:begin; resD:='bpl';goto br8; end;
  $2b:begin; resD:='bmi';goto br8; end;
  $2c:begin; resD:='bge';goto br8; end;
  $2d:begin; resD:='blt';goto br8; end;
  $2e:begin; resD:='bgt';goto br8; end;
  $2f:begin; resD:='ble';goto br8; end;
  $8d:begin; resD:='bsr';goto br8; end;
  $15:begin; resD:='bclr';a:='';goto dirmsk; end;
  $1d:begin; resD:='bclr';a:=','+defaultPt;goto dirmsk; end;
  $85,$95,$b5,$a5:begin; resD:='bita';o:=$85;goto i8deii; end;
  $c5,$d5,$f5,$e5:begin; resD:='bitb';o:=$c5;goto i8deii; end;
  $13:begin; resD:='brclr';a:='';goto dirmskrel; end;
  $1f:begin; resD:='brclr';a:=','+defaultPt;goto dirmskrel; end;
  $12:begin; resD:='brset';a:='';goto dirmskrel; end;
  $1e:begin; resD:='brset';a:=','+defaultPt;goto dirmskrel; end;
  $14:begin; resD:='bset';a:='';goto dirmsk; end;
  $1c:begin; resD:='bset';a:=','+defaultPt;goto dirmsk; end;
  $11:resD:='cba';
  $0c:resD:='clc';
  $0e:resD:='cli';
  $4f,$5f,$7f,$6f:begin; resD:='clr';o:=$4f;goto abeii; end;
  $0a:resD:='clv';
  $81,$91,$b1,$a1:begin; resD:='cmpa';o:=$81;goto i8deii; end;
  $c1,$d1,$f1,$e1:begin; resD:='cmpb';o:=$c1;goto i8deii; end;
  $43,$53,$73,$63:begin; resD:='com';o:=$43;goto abeii; end;
  $83,$93,$b3,$a3:case sawPrefix of
    0,1:begin; resD:='subd';o:=$83;goto i16deii; end;
    2,3:begin; resD:='cpd';o:=$83;goto i16deii; end;
    else exit;
    end;
  $8c,$9c,$bc,$ac:case sawPrefix of
    0,3:begin; resD:='cpx';o:=$8c;goto i16deii; end;
    1,2:begin; resD:='cpy';o:=$8c;goto i16deii; end;
    else exit;
    end;
  $19:resD:='daa';
  $4a,$5a,$7a,$6a:begin; resD:='dec';o:=$4a;goto abeii; end;
  $34:resD:='des';
  $09:resD:='de'+defaultPt;
  $88,$98,$b8,$a8:begin; resD:='eora';o:=$88;goto i8deii; end;
  $c8,$d8,$f8,$e8:begin; resD:='eorb';o:=$c8;goto i8deii; end;
  $03:resD:='fdiv';
  $02:resD:='idiv';
  $4c,$5c,$7c,$6c:begin; resD:='inc';o:=$4c;goto abeii; end;
  $31:resD:='ins';
  $08:resD:='in'+defaultPt;
  $7e,$6e:begin; resD:='jmp';o:=$4e;goto i8deii; end;
  $9d,$bd,$ad:begin; resD:='jsr';o:=$8d;goto i8deii; end;
  $86,$96,$b6,$a6:begin; resD:='ldaa';o:=$86;goto i8deii; end;
  $c6,$d6,$f6,$e6:begin; resD:='ldab';o:=$c6;goto i8deii; end;
  $cc,$dc,$fc,$ec:begin; resD:='ldd';o:=$cc;goto i16deii; end;
  $8e,$9e,$be,$ae:begin; resD:='lds';o:=$8e;goto i16deii; end;
  $ce,$de,$fe,$ee:case sawPrefix of
    0,3:begin; resD:='ldx';o:=$ce;goto i16deii; end;
    1,2:begin; resD:='ldy';o:=$ce;goto i16deii; end;
    else exit;
    end;
  $44,$54,$74,$64:begin; resD:='lsr';o:=$44;goto abeii; end;
  $04:resD:='lsrd';
  $3d:resD:='mul';
  $40,$50,$70,$60:begin; resD:='neg';o:=$40;goto abeii; end;
  $01:resD:='nop';
  $8a,$9a,$ba,$aa:begin; resD:='oraa';o:=$8a;goto i8deii; end;
  $ca,$da,$fa,$ea:begin; resD:='orab';o:=$ca;goto i8deii; end;
  $36:resD:='psha';
  $37:resD:='pshb';
  $3c:resD:='psh'+defaultPt;
  $32:resD:='pula';
  $33:resD:='pulb';
  $38:resD:='pul'+defaultPt;
  $49,$59,$79,$69:begin; resD:='rol';o:=$49;goto abeii; end;
  $46,$56,$76,$66:begin; resD:='ror';o:=$46;goto abeii; end;
  $3b:resD:='rti';
  $39:resD:='rts';
  $10:resD:='sba';
  $82,$92,$b2,$a2:begin; resD:='sbca';o:=$82;goto i8deii; end;
  $c2,$d2,$f2,$e2:begin; resD:='sbcb';o:=$c2;goto i8deii; end;
  $0d:resD:='sec';
  $0f:resD:='sei';
  $0b:resD:='sev';
  $97,$b7,$a7:begin; resD:='staa';o:=$87;goto i8deii; end;
  $d7,$f7,$e7:begin; resD:='stab';o:=$c7;goto i8deii; end;
  $dd,$fd,$ed:begin; resD:='std';o:=$cd;goto i16deii; end;
  $cf:resD:='stop';
  $9f,$bf,$af:begin; resD:='sts';o:=$8f;goto i16deii; end;
  $df,$ff,$ef:case sawPrefix of
    0,3:begin; resD:='stx';o:=$cf;goto i16deii; end;
    1,2:begin; resD:='sty';o:=$cf;goto i16deii; end;
    else exit;
    end;
  $80,$90,$b0,$a0:begin; resD:='suba';o:=$80;goto i8deii; end;
  $c0,$d0,$f0,$e0:begin; resD:='subb';o:=$c0;goto i8deii; end;
  $3f:resD:='swi';
  $16:resD:='tab';
  $06:resD:='tap';
  $17:resD:='tba';
  $07:resD:='tpa';
  $00:resD:='test';
  $4d,$5d,$7d,$6d:begin; resD:='tst';o:=$4d;goto abeii; end;
  $30:resD:='ts'+defaultPt;
  $35:resD:='t'+defaultPt+'s';
  $3e:resD:='wai';
  $8f:resD:='xgd'+defaultPt;
  end;
goto vege;

abeii:
case (i-o) and $3f of
  $00:begin; resD:=resD+'a';goto vege; end;
  $10:begin; resD:=resD+'b';goto vege; end;
  $20:goto ind;
  $30:goto ext;
  end;
exit;

i8deii:
case (i-o) and $3f of
  $00:goto imm8;
  $10:goto dir;
  $20:goto ind;
  $30:goto ext;
  end;
exit;

i16deii:
case (i-o) and $3f of
  $00:goto imm16;
  $10:goto dir;
  $20:goto ind;
  $30:goto ext;
  end;
exit;

imm8:
if GetBytes(i,1) then exit;
resD:=resD+' '+disAsm2hex(i,1);
goto vege;

imm16:
if GetBytes(i,2) then exit;
resD:=resD+' '+disAsm2hex(i,2);
goto vege;

dir:
if GetBytes(i,1) then exit;
resD:=resD+' ('+disAsm2hex(i,1)+')';
goto vege;

ext:
if GetBytes(i,2) then exit;
resD:=resD+' ('+disAsm2hex(i,2)+')';
goto vege;

ind:
if GetBytes(i,1) then exit;
resD:=resD+' ('+disAsm2hex(i,1)+','+defaultPt+')';
goto vege;

br8:
if GetBytes(i,1) then exit;
i:=shortint(i);
resD:=resD+' '#10+BStr(i);
goto vege;

dirmsk:
if GetBytes(i,1) then exit;
if GetBytes(o,1) then exit;
resD:=resD+' ('+disAsm2hex(i,1)+a+'),'+disAsm2hex(o,1);
goto vege;

dirmskrel:
if GetBytes(i,1) then exit;
if GetBytes(o,1) then exit;
if GetBytes(p,1) then exit;
p:=shortint(p);
resD:=resD+' ('+disAsm2hex(i,1)+a+'),'+disAsm2hex(o,1)+','#10+BStr(p);
goto vege;

Vege:
DisAssemblerBinaryCode:=chr(resS)+resD;
End;
