Procedure calcOneConst(var src,trg:oneStringType);
Var ps:LongInt;
Const
  cmd_end=$00;
  cmd_nop=$01;
  cmd_num=$02;
  cmd_add=$11;
  cmd_sub=$12;
  cmd_div=$13;
  cmd_mod=$14;
  cmd_mul=$15;
  cmd_exp=$16;
  cmd_shl=$17;
  cmd_shr=$18;
  cmd_not=$21;
  cmd_notAnd=$22;
  cmd_and=$23;
  cmd_andAnd=$24;
  cmd_xor=$25;
  cmd_xorAnd=$26;
  cmd_or=$27;
  cmd_orAnd=$28;
  cmd_Ceq=$31;
  cmd_Cle=$32;
  cmd_Cge=$33;
  cmd_Clt=$34;
  cmd_Cgt=$35;
  cmd_Cne=$36;
  cmd_brB=$41;
  cmd_brE=$42;




Function strGetByte:LongInt;
Var i:LongInt;
Begin;
inc(ps);
if (ps>oneStringMaxSize) then i:=0 else i:=src[ps];
if (i in [1..32,255]) then i:=32;
if (i=0) then dec(ps);
strGetByte:=i;
End;



Function strGetEntry:String;
Var
  a:String;
  ab:array[0..255] of byte absolute a;
  a0:byte absolute a;
  i,o,p:LongInt;
Begin;
strGetEntry:='';
repeat
  o:=strGetByte;
  until (o<>32);
case o of
  0:i:=0;
  43:i:=cmd_add;
  45:i:=cmd_sub;
  126:i:=cmd_notAnd;
  33:case strGetByte of
    61:i:=cmd_Cne;
    else begin; dec(ps);i:=cmd_not;end;
    end;
  42:case strGetByte of
    42:i:=cmd_exp;
    else begin; dec(ps);i:=cmd_mul;end;
    end;
  47:i:=cmd_div;
  37:i:=cmd_mod;
  60:case strGetByte of
    60:i:=cmd_shl;
    61:i:=cmd_Cle;
    else begin; dec(ps);i:=cmd_Clt; end;
    end;
  62:case strGetByte of
    62:i:=cmd_shr;
    61:i:=cmd_Cge;
    else begin; dec(ps);i:=cmd_Cgt; end;
    end;
  61:begin;
    if (strGetByte<>61) then exit;
    i:=cmd_Ceq;
    end;
  38:begin;
    i:=cmd_and;
    if (strGetByte=o) then inc(i) else dec(p);
    end;
  94:begin;
    i:=cmd_xor;
    if (strGetByte=o) then inc(i) else dec(p);
    end;
  124:begin;
    i:=cmd_or;
    if (strGetByte=o) then inc(i) else dec(p);
    end;
  $30..$39:begin;
    i:=strGetByte;
    case i of
      0:a:=chr(o);
      120,88:a:='$';
      else begin; dec(ps);a:=chr(o); end;
      end;
    while (1=1) do begin;
      i:=strGetByte;
      if (i=0) then break;
      if not (i in [$30..$39,97..102,65..70]) then begin; dec(ps);break; end;
      a:=a+chr(i);
      end;
    i:=BVal(a);
    move(i,ab[1],sizeof(i));
    a0:=sizeof(i);
    strGetEntry:=chr(cmd_num)+a;
    exit;
    end;
  40:i:=cmd_brB;
  41:i:=cmd_brE;
  else exit;
  end;
strGetEntry:=chr(i);
End;



Procedure getEnt(var ps,ed,tp,vl:LongInt);
Label f1;
Begin;
vl:=0;
f1:
if (ps>ed) then begin; tp:=cmd_end;exit; end;
tp:=trg[ps];
inc(ps);
if (tp=cmd_nop) then goto f1;
if (tp<>cmd_num) then exit;
move(trg[ps],vl,sizeof(vl));
inc(ps,sizeof(vl));
End;



Function getNum(var ps,ed,vl:LongInt):LongInt;
Label f1;
Var i,o,ne,no,nb:LongInt;
Begin;
getNum:=-1;
ne:=0;
no:=0;
nb:=0;
f1:
getEnt(ps,ed,i,vl);
if (i<1) then begin;
  getNum:=ne+no+nb;
  exit;
  end;
if (i=cmd_add) then begin; inc(ne,2);goto f1; end;
if (i=cmd_sub) then begin; inc(ne,1);goto f1; end;
if (i=cmd_not) then begin; inc(no,1);goto f1; end;
if (i=cmd_notAnd) then begin; inc(nb,1);goto f1; end;
if (i<>cmd_num) then exit;
if (no and 1<>0) then vl:=not vl;
if (ne and 1<>0) then vl:=-vl;
if (nb and 1<>0) then vl:=vl and 1;
getNum:=0;
End;


Function doCmd(Var c,n1,n2:LongInt):Boolean;
Var i,o:LongInt;
Begin;
doCmd:=true;
case c of
  cmd_add:inc(n1,n2);
  cmd_sub:dec(n1,n2);
  cmd_div:n1:=n1 div n2;
  cmd_mod:n1:=n1 mod n2;
  cmd_mul:n1:=n1*n2;
  cmd_exp:begin;
    o:=1;
    for i:=1 to n2 and 63 do o:=o*n1;
    n1:=o;
    end;
  cmd_shl:n1:=n1 shl n2;
  cmd_shr:n1:=n1 shr n2;
  cmd_and:n1:=n1 and n2;
  cmd_andAnd:begin; n1:=n1 and n2;if (n1<>0) then n1:=1; end;
  cmd_xor:n1:=n1 xor n2;
  cmd_xorAnd:begin; n1:=n1 xor n2;if (n1<>0) then n1:=1; end;
  cmd_or:n1:=n1 or n2;
  cmd_orAnd:begin; n1:=n1 or n2;if (n1<>0) then n1:=1; end;
  cmd_Cne:if (n1<>n2) then n1:=1 else n1:=0;
  cmd_Ceq:if (n1=n2) then n1:=1 else n1:=0;
  cmd_Cle:if (n1<=n2) then n1:=1 else n1:=0;
  cmd_Cge:if (n1>=n2) then n1:=1 else n1:=0;
  cmd_Clt:if (n1<n2) then n1:=1 else n1:=0;
  cmd_Cgt:if (n1>n2) then n1:=1 else n1:=0;
  else exit;
  end;
doCmd:=false;
End;


Function subPart(bg,ed:LongInt):String;
Label f1,f2,f3;
Var
  ps,ops,n1,n2,n3:LongInt;
  i,o,p,q:LongInt;
  a:String;

Function doSub(bg,ed,clr:Longint):Boolean;
Begin;
doSub:=true;
if (ed-bg<4) then exit;
if (clr and 1<>0) then trg[bg]:=cmd_nop;
if (clr and 2<>0) then trg[ed]:=cmd_nop;
a:=subPart(bg,ed);
if (a='') then exit;
fillchar(trg[bg],ed-bg+1,cmd_nop);
ed:=BVal(a);
trg[bg]:=cmd_num;
move(ed,trg[bg+1],sizeof(ed));
doSub:=false;
End;

Begin;
f1:
subPart:='';
ps:=bg;
p:=0;
while (1=1) do begin;
  ops:=ps;
  getEnt(ps,ed,i,o);
  if (i=cmd_end) then break;
  if (i=cmd_brB) then p:=ops;
  if (i<>cmd_brE) then continue;
  if (p=0) then exit;
  if doSub(p,ops,3) then exit;
  goto f1;
  end;
if (p<>0) then exit;
ps:=bg;
while (1=1) do begin;
  getEnt(ps,ed,o,i);
  if (o=cmd_end) then break;
  if not (o in [cmd_Ceq..cmd_Cne]) then continue;
  if doSub(bg,ps-2,0) then exit;
  if doSub(ps,ed,0) then exit;
  inc(trg[ps-1],$80);
  goto f1;
  end;
ps:=bg;
while (1=1) do begin;
  getEnt(ps,ed,o,i);
  if (o=cmd_end) then break;
  if (o<$80) then continue;
  dec(trg[ps-1],$80);
  end;

ps:=bg;
if (getNum(ps,ed,n1)<>0) then exit;
f2:
getEnt(ps,ed,o,i);
if (o=cmd_end) then begin;
  subPart:=BStr(n1);
  exit;
  end;
if (getNum(ps,ed,n2)<>0) then exit;
f3:
ops:=ps;
getEnt(ps,ed,p,i);
if (p in [cmd_add,cmd_sub,cmd_end]) then begin;
  ps:=ops;
  if doCmd(o,n1,n2) then exit;
  goto f2;
  end;
if (getNum(ps,ed,n3)<>0) then exit;
if doCmd(p,n2,n3) then exit;
goto f3;
End;




Label err,f1;
Var
  a:String;
  ab:array[0..255] of byte absolute a;
  a0:byte absolute a;
  i,o,p:LongInt;
Begin;
ps:=0;
p:=0;
while (1=1) do begin;
  a:=strGetEntry;
  if (a0=0) then goto err;
  if (ab[1]=0) then break;
  move(ab[1],trg[p+1],a0);
  inc(p,a0);
  end;
a:=subPart(1,p);
if (a0=0) then goto err;
pascal2string(a,trg);
exit;
err:
stringClear(trg);
End;
