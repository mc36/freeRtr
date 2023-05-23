Function convertStr2oid(b:String):String;

Function get:LongInt;
Var
  i:LongInt;
  a:String;
Begin;
get:=-1;
i:=pos('.',b);
a:=copy(b,1,i-1);
b:=copy(b,i+1,666);
if (a='0') then begin; get:=0;exit; end;
i:=BVal(a);
if (i=0) then exit;
get:=i;
End;

Var
  i,o:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
convertStr2oid:='';
b:=b+'.';
i:=get;
o:=get;
if (i<0) or (o<0) then exit;
ab0:=1;
ab[1]:=i*40+o;
while (b<>'') do begin;
  i:=get;
  if (i<0) then exit;
  inc(ab0);
  ab[ab0]:=i;
  end;
convertStr2oid:=a;
End;

Function convertOid2str(a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
  b:String;
Begin;
b:=BStr(ab[1] div 40)+'.'+BStr(ab[1] mod 40);
for i:=2 to ab0 do begin;
  b:=b+'.'+BStr(ab[i]);
  end;
convertOid2str:=b;
End;





Function createSNMPrequest(Var buffer;ver,typ,id:LongInt;pwd,oid:String):LongInt;
Var
  buf1:array[1..1] of byte absolute buffer;
  buf2:array[1..4096] of byte;
  bn:BigNumContextRecord;
  siz1,siz2:LongInt;
  i:LongInt;
Begin;
createSNMPrequest:=0;
siz1:=0;
writeASN1header(buf1,siz1,0,6,length(oid));
move(oid[1],buf1[siz1+1],length(oid));
inc(siz1,length(oid));
writeASN1header(buf1,siz1,0,5,0);
siz2:=0;
writeASN1header(buf2,siz2,$20,16,siz1);
move(buf1,buf2[siz2+1],siz1);
inc(siz2,siz1);
siz1:=0;
BigNumStoreInt(id,bn);
writeASN1integer(buf1,siz1,bn);
BigNumStoreInt(0,bn);
writeASN1integer(buf1,siz1,bn);
BigNumStoreInt(0,bn);
writeASN1integer(buf1,siz1,bn);
writeASN1header(buf1,siz1,$20,16,siz2);
move(buf2,buf1[siz1+1],siz2);
inc(siz1,siz2);
siz2:=0;
BigNumStoreInt(ver-1,bn);
writeASN1integer(buf2,siz2,bn);
writeASN1header(buf2,siz2,0,4,length(pwd));
move(pwd[1],buf2[siz2+1],length(pwd));
inc(siz2,length(pwd));
writeASN1header(buf2,siz2,$a0,typ,siz1);
move(buf1,buf2[siz2+1],siz1);
inc(siz2,siz1);
siz1:=0;
writeASN1header(buf1,siz1,$20,16,siz2);
move(buf2,buf1[siz1+1],siz2);
inc(siz1,siz2);
createSNMPrequest:=siz1;
End;



Function parseSNMPresponse(Var buffer;siz:LongInt;var ver,typ,id,st,idx:LongInt;var pwd,oid:String):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  bn:BigNumContextRecord;
  i,o,p,q,ps:LongInt;

Function getInt(var i:LongInt):Boolean;
Var dat:array[1..1024] of byte;
Begin;
getInt:=True;
if readASN1integer(buf,ps,bn) then exit;
fillchar(dat,sizeof(dat),0);
i:=sizeof(dat);
BigNumConvBN2LSB(bn,i,dat);
fillchar(dat[i+1],sizeof(i),0);
i:=ReadLongLSB(dat);
if (ps>siz) then exit;
getInt:=False;
End;

Function getStr(var a:String;i:LongInt):Boolean;
Const max=255;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
getStr:=True;
if (i>max) then ab0:=max else ab0:=i;
move(buf[ps+1],ab[1],ab0);
inc(ps,i);
if (ps>siz) then exit;
getStr:=False;
End;

Function getSeq:Boolean;
Begin;
getSeq:=True;
readASN1header(buf,ps,i,o,p);
inc(p,ps);
if (siz<p) then exit;
siz:=p;
getSeq:=False;
End;


Begin;
parseSNMPresponse:=-1;
ps:=0;
if getSeq then exit;
if (i<>$20) or (o<>16) then exit;
if getInt(ver) then exit;
inc(ver);
readASN1header(buf,ps,i,o,p);
if (i<>0) or (o<>4) then exit;
if getStr(pwd,p) then exit;
if getSeq then exit;
if (i<>$a0) then exit;
typ:=o;
if getInt(id) then exit;
if getInt(st) then exit;
if getInt(idx) then exit;
if getSeq then exit;
if (i<>$20) or (o<>16) then exit;
if getSeq then exit;
if (i<>$20) or (o<>16) then exit;
readASN1header(buf,ps,i,o,p);
if (i<>0) or (o<>6) then exit;
if getStr(oid,p) then exit;
q:=ps;
if getSeq then exit;
dec(siz,q);
move(buf[q+1],buf,siz);
parseSNMPresponse:=siz;
End;
