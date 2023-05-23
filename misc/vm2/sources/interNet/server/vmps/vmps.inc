Type
  OneVMPSpacketHeaderRecord=record
    cns:Byte;           {must be 1}
    typ:Byte;           {type}
    cod:Byte;           {code}
    hdr:Byte;           {headers}
    seq:LongInt;        {sequence}
    end;
  OneVMPSvalueHeaderRecord=record
    typ:LongInt;        {type}
    len:Word;           {size}
    end;



Function processOnePacket(var buffer;var siz:LongInt):String;
Label f1,f2,f3;
Var
  buf:array[1..1] of byte absolute buffer;
  hdr:OneVMPSpacketHeaderRecord absolute buffer;
  val:OneVMPSvalueHeaderRecord;
  con:OneConnectionRecord;
  adr,dom,prt:String;
  i,o,p:LongInt;
  ps:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Procedure addRec(t:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
begin;
WriteLongMSB(val.typ,t);
WriteWordMSB(val.len,ab0);
move(val,buf[siz+1],sizeof(val));
inc(siz,sizeof(val));
move(ab[1],buf[siz+1],ab0);
inc(siz,ab0);
End;

Begin;
processOnePacket:='bad request';
if (hdr.cns<>1) then exit;
if not (hdr.typ in [1,3]) then exit;
if (hdr.cod<>0) then exit;
ps:=sizeof(hdr);
adr:='';
prt:='';
dom:='';
f1:
if (ps>=siz) then goto f2;
move(buf[ps+1],val,sizeof(val));
inc(ps,sizeof(val));
o:=ReadWordMSB(val.len);
if (o>255) then ab0:=255 else ab0:=o;
move(buf[ps+1],ab[1],ab0);
inc(ps,o);
case ReadLongMSB(val.typ) of
  $0c01:;                       {client ip address}
  $0c02:prt:=a;                 {port name}
  $0c03:;                       {vlan name}
  $0c04:dom:=a;                 {vtp domain}
  $0c05:adr:=copy(a,7,6);       {ethernet packet}
  $0c06:adr:=a;                 {mac address}
  $0c07:;                       {unknown, 0}
  $0c08:adr:=a;                 {mac address}
  end;
goto f1;
f2:
dom:=dom;
a:='mac='+convertAddr2str(adr[1])+'; prt='+prt+'; dom='+dom+'; ';
p:=FindOneHardwareAddr(adr[1]);
if (p<1) then begin;
  con:=ConnectionDef;
  a:=a+'def ';
  end else con:=ConnectionDat^[p];
inc(hdr.typ);
siz:=sizeof(hdr);
hdr.hdr:=0;
if (con.lan=#13) then begin;
  a:=a+'deny';
  hdr.cod:=3;
  goto f3;
  end;
if (con.lan=#10) then begin;
  a:=a+'shutdown';
  hdr.cod:=4;
  goto f3;
  end;
a:=a+'allow '+con.lan;
hdr.cod:=0;
hdr.hdr:=2;
addRec($0c03,con.lan);
addRec($0c08,adr);
f3:
processOnePacket:=a;
End;
