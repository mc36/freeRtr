Const oneStringMaxSize=3*1024;
Type oneStringType=array[1..oneStringMaxSize+1] of byte;



Function stringLength(var src:oneStringType):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to oneStringMaxSize do if (src[i]=0) then goto f1;
i:=oneStringMaxSize+1;
f1:
stringLength:=i-1;
End;



Procedure stringClear(var trg:oneStringType);
Begin;
fillchar(trg,16,0);
End;



Procedure stringCopy(var trg,src:oneStringType);
Begin;
move(src,trg,sizeof(trg));
End;



Procedure stringPadup(var trg:oneStringType);
Var i:LongInt;
Begin;
i:=stringLength(trg);
fillchar(trg[i+1],oneStringMaxSize+1-i,0);
End;



Function string2pascal(var src:oneStringType):String;
Var
  b:string;
  bb:array[0..255] of byte absolute b;
  b0:byte absolute b;
Begin;
b0:=stringLength(src);
move(src[1],bb[1],b0);
string2pascal:=b;
End;

Procedure pascal2string(b:String;var trg:oneStringType);
Var
  bb:array[0..255] of byte absolute b;
  b0:byte absolute b;
Begin;
move(bb[1],trg[1],b0);
trg[b0+1]:=0;
End;



Procedure stringAppend(var trg,src:oneStringType);
Var i,o,p:LongInt;
Begin;
o:=stringLength(trg);
p:=0;
repeat
  inc(o);
  inc(p);
  if (o>oneStringMaxSize) then begin; trg[o]:=0;exit; end;
  i:=src[p];
  trg[o]:=i;
  until (i=0);
End;



Procedure stringReverse(var trg,src:oneStringType);
Var i,o,p,s:LongInt;
Begin;
s:=stringLength(src);
p:=0;
for i:=1 to s do begin;
  inc(p);
  trg[p]:=src[s];
  dec(s);
  end;
inc(p);
trg[p]:=0;
End;



Procedure stringLower(var trg,src:oneStringType);
Var i,o,p:LongInt;
Begin;
p:=0;
for o:=1 to stringLength(src) do begin;
  inc(p);
  i:=src[p];
  if (i in [65..90]) then inc(i,$20);
  trg[p]:=i;
  end;
inc(p);
trg[p]:=0;
End;



Procedure stringUpper(var trg,src:oneStringType);
Var i,o,p:LongInt;
Begin;
p:=0;
for o:=1 to stringLength(src) do begin;
  inc(p);
  i:=src[p];
  if (i in [97..122]) then dec(i,$20);
  trg[p]:=i;
  end;
inc(p);
trg[p]:=0;
End;



Function stringCompare(var v1,v2:oneStringType):LongInt;
Var p,s1,s2,d1,d2:LongInt;
Begin;
stringCompare:=0;
s1:=stringLength(v1);
s2:=stringLength(v2);
p:=0;
while (1=1) do begin;
  inc(p);
  if (s1=s2) then if (p>s1) then exit;
  if (p>s1) then begin; stringCompare:=1;exit; end;
  if (p>s2) then begin; stringCompare:=-1;exit; end;
  d1:=v1[p];
  d2:=v2[p];
  if (d1=d2) then continue;
  if (d1<d2) then stringCompare:=-1 else stringCompare:=1;
  exit;
  end;
End;



Function stringFind(var sub,src:oneStringType;ps,mv:LongInt):LongInt;
Label f1;
Var s1,s2,i,o:LongInt;
Begin;
stringFind:=-1;
if (mv=0) then exit;
s1:=stringLength(sub);
s2:=stringLength(src);
if (ps<0) then ps:=0;
i:=s2-s1-mv+1;
if (i<0) then exit;
if (ps>i) then ps:=i;
f1:
inc(ps,mv);
if (ps>s2-s1+1) then exit;
if (ps<0) then exit;
o:=ps;
for i:=1 to s1 do begin;
  if (sub[i]<>src[o]) then goto f1;
  inc(o);
  end;
stringFind:=ps-1;
End;



Procedure stringRange(var trg,src:oneStringType;beg,siz:LongInt);
Var i,o:LongInt;
Begin;
i:=stringLength(src);
if (i<beg) or (siz<1) then begin;
  stringClear(trg);
  exit;
  end;
o:=0;
repeat
  inc(o);
  if (o>oneStringMaxSize) or (siz<1) then begin;
    trg[o]:=0;
    exit;
    end;
  i:=src[beg];
  inc(beg);
  trg[o]:=i;
  dec(siz);
  until (i=0);
End;



Procedure stringTrimL(var trg,src:oneStringType);
Var i,o,p:LongInt;
Begin;
p:=0;
while (1=1) do begin;
  inc(p);
  if not (src[p] in [9,32,255,13,10]) then break;
  end;
stringRange(trg,src,p,oneStringMaxSize);
End;



Procedure stringTrimR(var trg,src:oneStringType);
Var i,o,p:LongInt;
Begin;
p:=stringLength(src);
while (p>0) do begin;
  if not (src[p] in [9,32,255,13,10]) then break;
  dec(p);
  end;
stringRange(trg,src,1,p);
End;



Function stringLineBounds(var src:oneStringType;ps:LongInt;var bg,ed:LongInt):LongInt;
Var op:LongInt;
Begin;
op:=ps;
while (1=1) do begin;
  if (ps>=oneStringMaxSize) then break;
  if (src[ps] in [0,13]) then break;
  inc(ps);
  end;
ed:=ps-1;
ps:=op;
while (1=1) do begin;
  dec(ps);
  if (ps<1) then break;
  if (src[ps] in [0,13]) then break;
  end;
bg:=ps+1;
stringLineBounds:=op-bg;
End;



Procedure stringPrint(var src:oneStringType;crlf:Boolean);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  a0:byte absolute a;
  i,o,p:LongInt;
Begin;
p:=0;
a0:=0;
while (1=1) do begin;
  if (a0>200) then begin;
    write(a);
    a0:=0;
    end;
  inc(p);
  if (p>oneStringMaxSize) then break;
  i:=src[p];
  if (i=0) then break;
  if (i=10) then begin;
    inc(a0);ab[a0]:=13;
    inc(a0);ab[a0]:=10;
    continue;
    end;
  if (i<32) then i:=46;
  inc(a0);
  ab[a0]:=i;
  end;
if (a0>0) then write(a);
if crlf then writeLn('');
End;



Function stringFindChar(var src:oneStringType;var ps:LongInt;ch:LongInt):Boolean;
Label f1;
Var
  b:oneStringType;
  i,o,p,siz:LongInt;

function getByte:LongInt;
var i:LongInt;
begin;
inc(ps);
if (ps>oneStringMaxSize) then i:=0 else i:=src[ps];
if (i=0) then dec(ps);
getByte:=i;
End;


Begin;
stringFindChar:=True;
siz:=0;
f1:
i:=getByte;
if (siz=0) then if (ch=i) then begin; stringFindChar:=false;exit; end;
case i of
  0:exit;
  92:getByte; {\}
  34:begin; {"}
    repeat until (getByte in [0,34]);
    end;
  35:begin; {#}
    repeat until (getByte in [0,13]);
    end;
  123:begin; {shift+[}
    inc(siz);
    b[siz]:=1;
    end;
  125:begin; {shift+]}
    if (siz<1) then exit;
    if (b[siz]<>1) then exit;
    dec(siz);
    end;
  91:begin; {[}
    inc(siz);
    b[siz]:=2;
    end;
  93:begin; {]}
    if (siz<1) then exit;
    if (b[siz]<>2) then exit;
    dec(siz);
    end;
  40:begin; {(}
    inc(siz);
    b[siz]:=3;
    end;
  41:begin; {)}
    if (siz<1) then exit;
    if (b[siz]<>3) then exit;
    dec(siz);
    end;
  end;
goto f1;
End;
