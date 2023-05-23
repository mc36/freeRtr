Procedure writeASN1integer(var buffer;var ps:LongInt;num:BigNumContextRecord);
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
BigNumConvBN2MSB(num,i,num);
writeASN1header(buf,ps,0,2,i);
move(num,buf[ps+1],i);
inc(ps,i);
End;

Function readASN1integer(var buffer;var ps:LongInt;var num:BigNumContextRecord):Boolean;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o,p:LongInt;
Begin;
readASN1integer:=True;
readASN1header(buf,ps,i,o,p);
if (i<>0) then exit;
if (o<>2) then exit;
if (p>sizeof(num)) then exit;
move(buf[ps+1],num,p);
inc(ps,p);
BigNumConvMSB2BN(p,num,num);
readASN1integer:=False;
End;
