Function analizeOneRule(var d:OneRuleRecord;b:String):LongInt;

Procedure clearWhite;
Var
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
Begin;
while (bb0>0) and (bb[1] in [0,9,32,255]) do b:=copy(b,2,666);
End;

Function gnw:string;
Var
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
  a:string;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:longint;
Begin;
clearWhite;
ab0:=0;
while (bb0>0) do begin;
  if (bb[1] in [0,9,32,255]) then break;
  inc(ab0);
  ab[ab0]:=bb[1];
  b:=copy(b,2,666);
  end;
clearWhite;
gnw:=a;
End;

Function getAddr(var adr,msk:OneAddressRecord):Boolean;
Label f1;
Var a:String;
Begin;
getAddr:=True;
a:=gnw;
if (a='any') then begin;
  fillchar(adr,sizeof(adr),$00);
  fillchar(msk,sizeof(msk),$00);
  goto f1;
  end;
if (a='host') then begin;
  if string2ipAddr(gnw,adr) then exit;
  fillchar(msk,sizeof(msk),$ff);
  goto f1;
  end;
if string2ipAddr(a,adr) then exit;
if string2ipAddr(gnw,msk) then exit;
f1:
if isAddressIPv4mask(msk) then fillchar(msk,length(IPv4addressPrefix),$ff);
truncateAddrMsk(adr,msk);
getAddr:=False;
End;

Function getPort(var first:LongInt;var last:LongInt):Boolean;
Label f1;
Var a:String;
Begin;
getPort:=True;
a:=gnw;
if (a='range') then begin;
  a:=gnw;
  first:=BVal(a);
  if (BStr(first)<>a) then exit;
  a:=gnw;
  last:=BVal(a);
  if (BStr(last)<>a) then exit;
  goto f1;
  end;
if (a='any') then begin;
  first:=0;
  last:=$ffff;
  goto f1;
  end;
first:=BVal(a);
if (BStr(first)<>a) then exit;
last:=first;
f1:
if (first and $ffff<>first) then exit;
if (last and $ffff<>last) then exit;
if (first>last) then exit;
getPort:=False;
End;

Function getAct:Boolean;
Label f1;
Var
  a:String;
  i:LongInt;
Begin;
getAct:=True;
a:=gnw;
if (a='deny') then begin;
  d.action:=0;
  i:=0;
  goto f1;
  end;
if (a='permit') then begin;
  i:=1;
  goto f1;
  end;
if (a='sa') then begin;
  if string2ipAddr(gnw,d.nSrcAdr) then exit;
  i:=2;
  goto f1;
  end;
if (a='ta') then begin;
  if string2ipAddr(gnw,d.nTrgAdr) then exit;
  i:=4;
  goto f1;
  end;
if (a='sp') then begin;
  a:=gnw;
  d.nSrcPrt:=BVal(a);
  if (BStr(d.nSrcPrt)<>a) then exit;
  i:=8;
  goto f1;
  end;
if (a='tp') then begin;
  a:=gnw;
  d.nTrgPrt:=BVal(a);
  if (BStr(d.nTrgPrt)<>a) then exit;
  i:=16;
  goto f1;
  end;
exit;
f1:
if (i<>0) then i:=i or 1;
d.action:=d.action or i;
getAct:=False;
End;

Var
  i,o:LongInt;
  a:String;
Begin;
analizeOneRule:=2;
fillchar(d,sizeof(d),0);
i:=pos(';',b);
if (i>0) then b:=copy(b,1,i-1);
clearWhite;
if (b='') then exit;
b:=kicsi(b);
analizeOneRule:=1;
if getAddr(d.srcAddr,d.srcMask) then exit;
if getAddr(d.trgAddr,d.trgMask) then exit;
a:=gnw;
i:=-1;
if (a='udp') then i:=1;
if (a='conn') then i:=2;
if (a='data') then i:=4;
if (a='tcp') then i:=6;
if (a='any') then i:=7;
if (i<0) then exit;
d.protocol:=i;
if getPort(d.srcPrtF,d.srcPrtL) then exit;
if getPort(d.trgPrtF,d.trgPrtL) then exit;
if (b='') then exit;
while (b<>'') do begin;
  if getAct then exit;
  clearWhite;
  end;
analizeOneRule:=0;
End;




Function convertOneRule(var d:OneRuleRecord):String;
Var a:String;
Begin;
a:=ipAddr2string(d.srcAddr)+' '+ipAddr2string(d.srcMask)+' ';
a:=a+ipAddr2string(d.trgAddr)+' '+ipAddr2string(d.trgMask)+' ';
case d.protocol of
  1:a:=a+'udp';
  2:a:=a+'conn';
  4:a:=a+'data';
  6:a:=a+'tcp';
  7:a:=a+'any';
  else a:=a+'?';
  end;
a:=a+' range '+BStr(d.srcPrtF)+' '+BStr(d.srcPrtL);
a:=a+' range '+BStr(d.trgPrtF)+' '+BStr(d.trgPrtL);
if (d.action=0) then a:=a+' deny';
if (d.action=1) then a:=a+' permit';
if (d.action and 2<>0) then a:=a+' sa '+ipAddr2string(d.nSrcAdr);
if (d.action and 4<>0) then a:=a+' ta '+ipAddr2string(d.nTrgAdr);
if (d.action and 8<>0) then a:=a+' sp '+BStr(d.nSrcPrt);
if (d.action and 16<>0) then a:=a+' tp '+BStr(d.nTrgPrt);
convertOneRule:=a;
End;



Procedure readUpConfiguration(a:String);
Var
  t:xtText;
  ln:LongInt;
  d:OneRuleRecord;
Begin;
WriteLn('reading '+a+'...');
RulesNumb:=0;
if (xtOpen(t,a,true)<>0) then immErr('error opening file!');
ln:=0;
{$ifdef debug}WriteLn('--- rules follow ---');{$endif}
while not xtEOF(t) do begin;
  inc(ln);
  a:=xtReadLn(t,255);
  case analizeOneRule(d,a) of
    0:begin;
      if ResizeMem(RulesNumb+1) then immErr('out of memory!');
      RulesData^[RulesNumb]:=d;
      {$ifdef debug}WriteLn(convertOneRule(d));{$endif}
      end;
    2:continue;
    else begin;
      WriteLn('invalid rule at line #'+BStr(ln)+':');
      Write('"');
      Write(a);
      immErr('"');
      end;
    end;
  end;
{$ifdef debug}WriteLn('--- end of rules ---');{$endif}
xtClose(t);
End;
