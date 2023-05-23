Function DisAssemblerBinaryCode(src:String):String;
Label rn,ri,adr8,num8,adr11,adr8num8,rna8n8,a8n8,n8o8,rnria8n8,arnria8,arnria8an8,vege;
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
  i,o,p,q:LongInt;
  a,b,c:String;
Begin;
resS:=0;
GetBytes(i,1);
resD:='db '+disAsm2hex(i,1);

case i of
  $00:resD:='nop';
  $24..$2f:begin; resD:='add a,';goto rnria8n8; end;
  $34..$3f:begin; resD:='addc a,';goto rnria8n8; end;
  $94..$9f:begin; resD:='subb a,';goto rnria8n8; end;
  $04..$0f:begin; resD:='inc ';goto arnria8; end;
  $14..$1f:begin; resD:='dec ';goto arnria8; end;
  $52..$5f:begin; resD:='anl';goto arnria8an8; end;
  $42..$4f:begin; resD:='orl';goto arnria8an8; end;
  $62..$6f:begin; resD:='xrl';goto arnria8an8; end;
  $e5..$ef:begin; resD:='mov a,';goto rnria8n8; end;
  $74:begin; resD:='mov a,';goto rnria8n8; end;
  $f8..$ff:begin; resD:='mov';goto rna8n8; end;
  $a8..$af:begin; resD:='mov';goto rna8n8; end;
  $78..$7f:begin; resD:='mov';goto rna8n8; end;
  $f5:begin; resD:='mov ';a:=',a';goto adr8; end;
  $88..$8f:begin; resD:='mov ';a:=',r'+BStr(i and 7);goto adr8; end;
  $85:begin; GetBytes(i,1);resD:='mov ';a:=',('+disAsm2hex(i,1)+')';goto adr8; end;
  $86,$87:begin; resD:='mov ';a:=',@r'+BStr(i and 1);goto adr8; end;
  $75:begin; resD:='mov ';a:='';goto adr8num8; end;
  $f6,$a6,$76:begin; resD:='mov @r0,';a:='';goto a8n8; end;
  $f7,$a7,$77:begin; resD:='mov @r1,';a:='';goto a8n8; end;
  $c0:begin; GetBytes(i,1);resD:='push ('+disAsm2hex(i,1)+')'; end;
  $d0:begin; GetBytes(i,1);resD:='pop ('+disAsm2hex(i,1)+')'; end;
  $90:begin; GetBytes(i,2);resD:='mov dptr,'+disAsm2hex(i,2); end;
  $c8..$cf:begin; resD:='xch a,';goto rn; end;
  $c5:begin; resD:='xch a,';a:='';goto adr8; end;
  $c6,$c7:begin; resD:='xch a,@r'+BStr(i and 1); end;
  $d6,$d7:begin; resD:='xchd a,@r'+BStr(i and 1); end;
  $c3:resD:='clr c';
  $d3:resD:='setb c';
  $b3:resD:='cpl c';
  $40:begin; GetBytes(i,1);resD:='jc '#10+BStr(i); end;
  $50:begin; GetBytes(i,1);resD:='jnc '#10+BStr(i); end;
  $60:begin; GetBytes(i,1);resD:='jz '#10+BStr(i); end;
  $70:begin; GetBytes(i,1);resD:='jnz '#10+BStr(i); end;
  $20:begin; resD:='jb';goto n8o8; end;
  $30:begin; resD:='jnb';goto n8o8; end;
  $10:begin; resD:='jbc';goto n8o8; end;
  $c2:begin; resD:='clr ';a:='';goto num8; end;
  $d2:begin; resD:='setb ';a:='';goto num8; end;
  $b2:begin; resD:='cpl ';a:='';goto num8; end;
  $82:begin; resD:='anl c,';a:='';goto num8; end;
  $b0:begin; resD:='anl c,/';a:='';goto num8; end;
  $72:begin; resD:='orl c,';a:='';goto num8; end;
  $a0:begin; resD:='orl c,/';a:='';goto num8; end;
  $a2:begin; resD:='mov c,';a:='';goto num8; end;
  $92:begin; resD:='mov ';a:=',c';goto num8; end;
  $11,$31,$51,$71,$91,$b1,$d1,$f1:begin; resD:='acall';goto adr11; end;
  $01,$21,$41,$61,$81,$a1,$c1,$e1:begin; resD:='ajmp';goto adr11; end;
  $12:begin; GetBytes(i,2);resD:='lcall '+disAsm2hex(i,3); end;
  $02:begin; GetBytes(i,2);resD:='ljmp '+disAsm2hex(i,3); end;
  $80:begin; GetBytes(i,1);resD:='sjmp '#10+BStr(i); end;
  $b5:begin;
    GetBytes(i,1);
    GetBytes(o,1);
    resD:='cjne a,('+disAsm2hex(i,1)+'),'+#10+BStr(o);
    end;
  $b4:begin;
    GetBytes(i,1);
    GetBytes(o,1);
    resD:='cjne a,'+disAsm2hex(i,1)+','+#10+BStr(o);
    end;
  $b8..$bf:begin;
    GetBytes(p,1);
    GetBytes(o,1);
    resD:='cjne r'+BStr(i and 7)+','+disAsm2hex(p,1)+','+#10+BStr(o);
    end;
  $b6..$b7:begin;
    GetBytes(p,1);
    GetBytes(o,1);
    resD:='cjne @r'+BStr(i and 1)+','+disAsm2hex(p,1)+','+#10+BStr(o);
    end;
  $d8..$df:begin;
    GetBytes(o,1);
    resD:='djnz r'+BStr(i and 7)+','+#10+BStr(o);
    end;
  $d5:begin;
    GetBytes(i,1);
    GetBytes(o,1);
    resD:='djnz ('+disAsm2hex(i,1)+'),'+#10+BStr(o);
    end;
  $22:resD:='ret';
  $32:resD:='reti';
  $73:resD:='jmp @a+dptr';
  $a3:resD:='inc dptr';
  $a4:resD:='mul ab';
  $84:resD:='div ab';
  $d4:resD:='da a';
  $e4:resD:='clr a';
  $f4:resD:='cpl a';
  $23:resD:='rl a';
  $33:resD:='rlc a';
  $03:resD:='rr a';
  $13:resD:='rrc a';
  $c4:resD:='swap a';
  $93:resD:='movc a,@a+dptr';
  $83:resD:='movc a,@a+pc';
  $e2:resD:='movx a,@r0';
  $e3:resD:='movx a,@r1';
  $e0:resD:='movx a,@dptr';
  $f2:resD:='movx @r0,a';
  $f3:resD:='movx @r1,a';
  $f0:resD:='movx @dptr,a';
  end;
goto vege;

n8o8:
GetBytes(i,1);
resD:=resD+' '+disAsm2hex(i,1);
GetBytes(i,1);
resD:=resD+','#10+BStr(i);
goto vege;

rna8n8:
a:='';
resD:=resD+' r'+BStr(i and 7)+',';
a8n8:
case i shr 4 of
  $f:resD:=resD+'a';
  $a:goto adr8;
  $7:goto num8;
  end;
goto vege;

arnria8an8:
case i and $f of
  $8..$f:begin; resD:=resD+' a,';goto rn; end;
  $6..$7:begin; resD:=resD+' a,';goto ri; end;
  $5:begin; resD:=resD+' a,';a:='';goto adr8; end;
  $4:begin; resD:=resD+' a,';a:='';goto num8; end;
  $3:begin; resD:=resD+' ';a:='';goto adr8num8; end;
  $2:begin; resD:=resD+' ';a:=',a';goto adr8; end;
  end;
goto vege;

rnria8n8:
case i and $f of
  $8..$f:goto rn;
  $6..$7:goto ri;
  $5:begin; a:='';goto adr8; end;
  $4:begin; a:='';goto num8; end;
  end;
goto vege;

arnria8:
case i and $f of
  $8..$f:goto rn;
  $6..$7:goto ri;
  $5:begin; a:='';goto adr8; end;
  $4:resD:=resD+'a';
  end;
goto vege;

rn:
resD:=resD+'r'+BStr(i and 7);
goto vege;

ri:
resD:=resD+'@r'+BStr(i and 1);
goto vege;

adr8:
GetBytes(i,1);
resD:=resD+'('+disAsm2hex(i,1)+')'+a;
goto vege;

num8:
GetBytes(i,1);
resD:=resD+disAsm2hex(i,1)+a;
goto vege;

adr11:
i:=i shr 5;
GetBytes(o,1);
resD:=resD+' '+disAsm2hex((i shl 8)+o,3);
goto vege;

adr8num8:
GetBytes(i,1);
resD:=resD+'('+disAsm2hex(i,1)+'),';
GetBytes(i,1);
resD:=resD+disAsm2hex(i,1)+a;
goto vege;

Vege:
DisAssemblerBinaryCode:=chr(resS)+resD;
End;
