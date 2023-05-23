Function DisAssemblerBinaryCode(src:String):String;
Label sti,sto,so,si,oo,tis,iis,ti,dst,dts,st,sd,ss,dd,dth,ii,io,Vege;
Const
  cpuRegs:array[0..31] of String[3]=('r0','at','v0','v1','a0','a1','a2','a3',
                                     't0','t1','t2','t3','t4','t5','t6','t7',
                                     's0','s1','s2','s3','s4','s5','s6','s7',
                                     't8','t9','k0','k1','gp','sp','s8','ra');
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
p:=ReadLongMSB(src[1]);
delete(src,1,4);
resD:='dd '+disAsm2hex(p,4);

case p of
  $00:begin; resD:='noop';goto Vege; end;
  $0f:begin; resD:='sync';goto Vege; end;
  end;
i:=p shr 26;
case i of
  $08:begin; a:='addi';goto sti; end;
  $09:begin; a:='addiu';goto sti; end;
  $18:begin; a:='daddi';goto sti; end;
  $19:begin; a:='daddiu';goto sti; end;
  $0c:begin; a:='andi';goto sti; end;
  $04:begin; a:='beq';goto sto; end;
  $14:begin; a:='beql';goto sto; end;
  $05:begin; a:='bne';goto sto; end;
  $15:begin; a:='bnel';goto sto; end;
  $10..$13:begin; a:='cop'+BStr(i and 3);goto io; end;
  $02:begin; a:='j';goto oo; end;
  $03:begin; a:='jal';goto oo; end;
  $20:begin; a:='lb';goto tis; end;
  $24:begin; a:='lbu';goto tis; end;
  $21:begin; a:='lh';goto tis; end;
  $25:begin; a:='lhu';goto tis; end;
  $23:begin; a:='lw';goto tis; end;
  $30:begin; a:='ll';goto tis; end;
  $34:begin; a:='lld';goto tis; end;
  $27:begin; a:='lwu';goto tis; end;
  $37:begin; a:='ld';goto tis; end;
  $1a:begin; a:='ldl';goto tis; end;
  $1b:begin; a:='ldr';goto tis; end;
  $3f:begin; a:='sd';goto tis; end;
  $2c:begin; a:='sdl';goto tis; end;
  $2d:begin; a:='sdr';goto tis; end;
  $38:begin; a:='sc';goto tis; end;
  $3c:begin; a:='scd';goto tis; end;
  $33:begin; a:='pref';goto iis; end;
  $2f:begin; a:='cache';goto iis; end;
  $31..$32:begin; a:='lwc'+BStr(i and 3);goto tis; end;
  $35..$36:begin; a:='ldc'+BStr(i and 3);goto tis; end;
  $22:begin; a:='lwl';goto tis; end;
  $26:begin; a:='lwr';goto tis; end;
  $0f:begin; a:='lui';goto ti; end;
  $0d:begin; a:='ori';goto sti; end;
  $28:begin; a:='sb';goto tis; end;
  $29:begin; a:='sh';goto tis; end;
  $0a:begin; a:='slti';goto sti; end;
  $0b:begin; a:='sltiu';goto sti; end;
  $2b:begin; a:='sw';goto tis; end;
  $39..$3b:begin; a:='swc'+BStr(i and 3);goto tis; end;
  $3d..$3e:begin; a:='sdc'+BStr(i and 3);goto tis; end;
  $2a:begin; a:='swl';goto tis; end;
  $2e:begin; a:='swr';goto tis; end;
  $0e:begin; a:='xori';goto sti; end;
  $00:begin;
    i:=p and $3f;
    case i of
      $20:begin; a:='add';goto dst; end;
      $21:begin; a:='addu';goto dst; end;
      $2c:begin; a:='dadd';goto dst; end;
      $2d:begin; a:='daddu';goto dst; end;
      $2e:begin; a:='dsub';goto dst; end;
      $2f:begin; a:='dsubu';goto dst; end;
      $24:begin; a:='and';goto dst; end;
      $1a:begin; a:='div';goto st; end;
      $1b:begin; a:='divu';goto st; end;
      $1e:begin; a:='ddiv';goto st; end;
      $1f:begin; a:='ddivu';goto st; end;
      $09:begin; a:='jalr';goto sd; end;
      $08:begin; a:='jr';goto ss; end;
      $10:begin; a:='mfhi';goto dd; end;
      $12:begin; a:='mflo';goto dd; end;
      $11:begin; a:='mthi';goto ss; end;
      $13:begin; a:='mtlo';goto ss; end;
      $18:begin; a:='mult';goto st; end;
      $19:begin; a:='multu';goto st; end;
      $1c:begin; a:='dmult';goto st; end;
      $1d:begin; a:='dmultu';goto st; end;
      $34:begin; a:='teq';goto st; end;
      $30:begin; a:='tge';goto st; end;
      $31:begin; a:='tgeu';goto st; end;
      $32:begin; a:='tlt';goto st; end;
      $33:begin; a:='tltu';goto st; end;
      $36:begin; a:='tne';goto st; end;
      $25:begin; a:='or';goto dst; end;
      $27:begin; a:='nor';goto dst; end;
      $0b:begin; a:='movn';goto dts; end;
      $0a:begin; a:='movz';goto dts; end;
      $00:begin; a:='sll';goto dth; end;
      $04:begin; a:='sllv';goto dts; end;
      $38:begin; a:='dsll';goto dth; end;
      $3c:begin; a:='dsll32';goto dth; end;
      $14:begin; a:='dsllv';goto dts; end;
      $3b:begin; a:='dsra';goto dth; end;
      $3f:begin; a:='dsra32';goto dth; end;
      $17:begin; a:='dsrav';goto dts; end;
      $3a:begin; a:='dsrl';goto dth; end;
      $3e:begin; a:='dsrl32';goto dth; end;
      $16:begin; a:='dsrlv';goto dts; end;
      $2a:begin; a:='slt';goto dst; end;
      $2b:begin; a:='sltu';goto dst; end;
      $03:begin; a:='sra';goto dth; end;
      $07:begin; a:='srav';goto dts; end;
      $02:begin; a:='srl';goto dth; end;
      $06:begin; a:='srlv';goto dts; end;
      $22:begin; a:='sub';goto dst; end;
      $23:begin; a:='subu';goto dst; end;
      $26:begin; a:='xor';goto dst; end;
      $0c:begin; a:='syscall';goto ii; end;
      $0d:begin; a:='break';goto ii; end;
      end;
    end;
  $01:begin;
    i:=(p shr 16) and 31;
    case i of
      $00:begin; a:='bltz';goto so; end;
      $02:begin; a:='bltzl';goto so; end;
      $10:begin; a:='bltzal';goto so; end;
      $12:begin; a:='bltzall';goto so; end;
      $01:begin; a:='bgez';goto so; end;
      $03:begin; a:='bgezl';goto so; end;
      $11:begin; a:='bgezal';goto so; end;
      $13:begin; a:='bgezall';goto so; end;
      $0c:begin; a:='teqi';goto si; end;
      $08:begin; a:='tgei';goto si; end;
      $09:begin; a:='tgeiu';goto si; end;
      $0a:begin; a:='tlti';goto si; end;
      $0b:begin; a:='tltiu';goto si; end;
      $0e:begin; a:='tnei';goto si; end;
      end;
    end;
  $07:begin;
    i:=(p shr 16) and 31;
    case i of
      $00:begin; a:='bgtz';goto so; end;
      end;
    end;
  $17:begin;
    i:=(p shr 16) and 31;
    case i of
      $00:begin; a:='bgtzl';goto so; end;
      end;
    end;
  $06:begin;
    i:=(p shr 16) and 31;
    case i of
      $00:begin; a:='blez';goto so; end;
      end;
    end;
  $16:begin;
    i:=(p shr 16) and 31;
    case i of
      $00:begin; a:='blezl';goto so; end;
      end;
    end;
  end;
goto Vege;

sti:
resD:=a+' '+cpuRegs[(p shr 16) and 31]+','+cpuRegs[(p shr 21) and 31]+','+disAsm2hex(p,2);
goto Vege;

sto:
int:=p;
i:=(int*4)+4;
resD:=a+' '+cpuRegs[(p shr 21) and 31]+','+cpuRegs[(p shr 16) and 31]+','#10+BStr(i);
goto Vege;

so:
int:=p;
i:=(int*4)+4;
resD:=a+' '+cpuRegs[(p shr 21) and 31]+','#10+BStr(i);
goto Vege;

si:
resD:=a+' '+cpuRegs[(p shr 21) and 31]+','+disAsm2hex(p,2);
goto Vege;

oo:
resD:=a+' '+disAsm2hex((p and $03ffffff) shl 2,4);
goto Vege;

tis:
resD:=a+' '+cpuRegs[(p shr 16) and 31]+','+disAsm2hex(p,2)+','+cpuRegs[(p shr 21) and 31];
goto Vege;

iis:
resD:=a+' '+BStr((p shr 16) and 31)+','+disAsm2hex(p,2)+','+cpuRegs[(p shr 21) and 31];
goto Vege;

ti:
resD:=a+' '+cpuRegs[(p shr 16) and 31]+','+disAsm2hex(p,2);
goto Vege;

dst:
resD:=a+' '+cpuRegs[(p shr 11) and 31]+','+cpuRegs[(p shr 21) and 31]+','+cpuRegs[(p shr 16) and 31];
goto Vege;

dts:
resD:=a+' '+cpuRegs[(p shr 11) and 31]+','+cpuRegs[(p shr 16) and 31]+','+cpuRegs[(p shr 21) and 31];
goto Vege;

st:
resD:=a+' '+cpuRegs[(p shr 21) and 31]+','+cpuRegs[(p shr 16) and 31];
goto Vege;

sd:
resD:=a+' '+cpuRegs[(p shr 11) and 31]+','+cpuRegs[(p shr 21) and 31];
goto Vege;

ss:
resD:=a+' '+cpuRegs[(p shr 21) and 31];
goto Vege;

dd:
resD:=a+' '+cpuRegs[(p shr 11) and 31];
goto Vege;

dth:
resD:=a+' '+cpuRegs[(p shr 11) and 31]+','+cpuRegs[(p shr 16) and 31]+','+BStr((p shr 6) and 31);
goto Vege;

ii:
resD:=a+' '+disAsm2hex((p and $03ffffff) shr 6,4);
goto Vege;

io:
resD:=a+' '+disAsm2hex((p and $03ffffff),4);
goto Vege;


Vege:
DisAssemblerBinaryCode:=#4+resD;
End;
