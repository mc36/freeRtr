Type
  dhcpMessageHeader=record
    typ:Byte;                   {message type}
    tid:array[1..3] of byte;    {transaction id}
    end;
  dhcpRelayMessageHeader=record
    typ:Byte;                   {message type}
    hop:byte;                   {transaction id}
    lnk:OneTCPaddressRecord;    {link address}
    per:OneTCPaddressRecord;    {peer address}
    end;


Function getMsgNam(i:LongInt):String;
Var a:String;
Begin;
case i of
   1:a:='solicit';
   2:a:='advertise';
   3:a:='request';
   4:a:='confirm';
   5:a:='renew';
   6:a:='rebind';
   7:a:='reply';
   8:a:='release';
   9:a:='decline';
  10:a:='reconfigure';
  11:a:='information request';
  12:a:='relay forw';
  13:a:='relay reply';
  14:a:='leasequery';
  15:a:='leasequery reply';
  else a:='unknown:'+BStr(i);
  end;
getMsgNam:=a;
End;

Function getOptNam(i:LongInt):String;
Var a:String;
Begin;
case i of
   1:a:='client identifier';
   2:a:='server identifier';
   3:a:='identity association for non temporary address';
   4:a:='identity association for temporary address';
   5:a:='ia address';
   6:a:='option request';
   7:a:='preference';
   8:a:='elapsed time';
   9:a:='relay message';
  10:a:='server message';
  11:a:='authentication';
  12:a:='server unicast';
  13:a:='status code';
  14:a:='rapid commit';
  15:a:='user class';
  16:a:='vendor class';
  17:a:='vendor specific information';
  18:a:='interface id';
  19:a:='reconfigure message';
  20:a:='reconfigure accept';
  21:a:='sip server domain name list';
  22:a:='sip servers ipv6 address list';
  23:a:='dns recursive name server';
  24:a:='domain search list';
  25:a:='identity association for prefix delegation';
  26:a:='ia prefix';
  27:a:='network information server';
  28:a:='network information server v2';
  29:a:='network information server domain name';
  30:a:='network information server v2 domain name';
  31:a:='simple network time protocol server';
  32:a:='lifetime';
  33:a:='bcmcs server domain';
  34:a:='bcmcs servers ipv6 address list';
  36:a:='geoconf civic address';
  37:a:='remote identifier';
  38:a:='subscriber identifier';
  39:a:='fully qualified domain name';
  40:a:='pana agents ipv6 address list';
  41:a:='time zone';
  42:a:='time zone database';
  43:a:='echo request option';
  44:a:='leasequery query';
  45:a:='leasequery client data';
  46:a:='client last transaction time';
  47:a:='leasequery relay data';
  48:a:='leasequery client link address list';
  else a:='unknown:'+BStr(i);
  end;
getOptNam:=a;
End;



Function extractDUID(a:String):String;
Var i,o:LongInt;
Begin;
extractDUID:='';
i:=ReadWordMSB(a[1]);
o:=ReadWordMSB(a[3]);
if (o<>intfAdrT) then exit;
case i of
  1:a:=copy(a,9,666);
  3:a:=copy(a,5,666);
  end;
if (length(a)<>intfAdrS) then exit;
extractDUID:=a;
End;



Function extractFQDN(var a:String):String;
Label f1,f2;
Var b,c:String;
Begin;
b:='';
f1:
if (a='') then goto f2;
move(a[1],c,sizeof(c));
a:=copy(a,length(c)+2,666);
if (c='') then goto f1;
b:=b+'.'+c;
goto f1;
f2:
extractFQDN:=copy(b,2,666);
End;



Function convertDUID(ad:OneTCPaddressRecord):String;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
ab0:=intfAdrS+4;
WriteWordMSB(ab[1],3);
WriteWordMSB(ab[3],intfAdrT);
move(ad,ab[5],intfAdrS);
convertDUID:=a;
End;



Function convertFQDN(b:String):String;
Var
  a:String;
  i,o:LongInt;
Begin;
a:='';
while (b<>'') do begin;
  i:=pos('.',b);
  if (i<1) then i:=length(b)+1;
  a:=a+chr(i-1)+copy(b,1,i-1);
  b:=copy(b,i+1,666);
  end;
convertFQDN:=a+#0;
End;






Function ReceivePacket:Boolean;
Label f1,f2,f3;
Var
  bufD:array[1..4*1024] of byte;
  bufS:LongInt;
  bufP:LongInt;
  ad:OneTCPaddressRecord;
  conNum:LongInt;
  dhcpTyp:LongInt;
  hdr:dhcpMessageHeader absolute bufD;
  hdr2:dhcpRelayMessageHeader absolute bufD;
  i,o,p,q:LongInt;
  a,b:String;


Function getOpt(var p:LongInt):String;
Var
  i,o,q:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
q:=bufP;
p:=-1;
if (bufP>=bufS) then exit;
p:=ReadWordMSB(bufD[bufP+1]);
o:=ReadWordMSB(bufD[bufP+3]);
inc(bufP,4);
if (o>255) then i:=255 else i:=o;
ab0:=i;
move(bufD[bufP+1],ab[1],i);
inc(bufP,o);
case p of
  3:bufP:=q+16; {ia na}
  4:bufP:=q+8; {ia ta}
  5:bufP:=q+12+sizeof(OneTCPaddressRecord); {ia addr}
  25:bufP:=q+16; {ia pd}
  end;
getOpt:=a;
End;

Procedure addOpt(p:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
WriteWordMSB(bufD[bufS+1],p);
WriteWordMSB(bufD[bufS+3],ab0);
inc(bufS,4);
move(ab[1],bufD[bufS+1],ab0);
inc(bufS,ab0);
End;

Function mbdOpt(p:LongInt;a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
a:='1234'+a;
WriteWordMSB(a[1],p);
WriteWordMSB(a[3],ab0-4);
mbdOpt:=a;
End;

Function ip2bin(ad:OneTCPaddressRecord):String;
Var a:String;
Begin;
a[0]:=chr(sizeof(ad));
move(ad,a[1],sizeof(ad));
ip2bin:=a;
End;

Procedure getBeg(i:LongInt);
Begin;
if (i in [12,13]) then bufP:=sizeof(dhcpRelayMessageHeader) else bufP:=sizeof(dhcpMessageHeader);
End;

Function findOpt(o:LongInt):String;
Label f1;
Var
  a:String;
  i:LongInt;
Begin;
findOpt:='';
getBeg(hdr.typ);
f1:
a:=getOpt(i);
if (i<0) then exit;
if (i<>o) then goto f1;
findOpt:=a;
End;

Procedure dumpPack;
Label f1,f2,f3,f4,f5,f6,f7;
Const beg='  ';
Var
  a,b:String;
  ha:OneTCPaddressRecord;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
a:=getMsgNam(hdr.typ);
if (hdr.typ in [12,13]) then begin;
  WriteLn(beg+'type='+a+'  hop='+BStr(hdr2.hop));
  WriteLn(beg+'link='+ipAddr2string(hdr2.lnk)+'  peer='+ipAddr2string(hdr2.per));
  end else begin;
  WriteLn(beg+'transaction='+BStr(ReadLongMSB(hdr.tid) shr 8)+'  type='+a);
  end;
getBeg(hdr.typ);

f1:
a:=getOpt(i);
if (i<0) then exit;
b:=getOptNam(i);
case i of
  1:begin; {client id}
    f2:
    a:=extractDUID(a);
    move(a[1],ha,sizeof(ha));
    b:=b+': '+convertHWaddr(ha);
    end;
  2:begin; {server id}
    goto f2;
    end;
  5:begin; {address}
    a:=copy(a,1,sizeof(ha));
    goto f3;
    end;
  6:begin; {requested options}
    f5:
    b:=b+':';
    while (a<>'') do begin;
      b:=b+' '+BStr(ReadWordMSB(a[1]));
      a:=copy(a,3,666);
      end;
    end;
  7:begin; {preference}
    f6:
    b:=b+':';
    while (a<>'') do begin;
      b:=b+' '+BStr(ab[1]);
      a:=copy(a,2,666);
      end;
    end;
  8:begin; {elapsed time}
    goto f5;
    end;
  12:begin; {server unicast}
    goto f3;
    end;
  21:begin; {sip servers name}
    goto f4;
    end;
  22:begin; {sip servers addr}
    goto f3;
    end;
  23:begin; {dns server list}
    f3:
    b:=b+':';
    while (a<>'') do begin;
      move(a[1],ha,sizeof(ha));
      b:=b+' '+ipAddr2string(ha);
      a:=copy(a,sizeof(ha)+1,666);
      end;
    end;
  24:begin; {domain list}
    f4:
    b:=b+':';
    while (a<>'') do b:=b+' '+extractFQDN(a);
    end;
  26:begin; {ia prefix}
    move(a[10],ha,sizeof(ha));
    b:=b+' '+ipAddr2string(ha)+'/'+BStr(ab[9]);
    end;
  27:begin; {nis servers}
    goto f3;
    end;
  28:begin; {nis+ servers}
    goto f3;
    end;
  29:begin; {nis servers name}
    goto f4;
    end;
  30:begin; {nis+ servers name}
    goto f4;
    end;
  31:begin; {ntp servers}
    goto f3;
    end;
  32:begin; {info refresh time}
    f7:
    b:=b+':';
    while (a<>'') do begin;
      b:=b+' '+BStr(ReadLongMSB(a[1]));
      a:=copy(a,5,666);
      end;
    end;
  43:begin; {echo request}
    goto f5;
    end;
  end;
WriteLn(beg+b);
goto f1;
End;


Begin;
ReceivePacket:=False;
bufS:=sizeof(bufD);
if UDPreceivePacket(pipeCmd,a,o,bufD,bufS) then exit;
ReceivePacket:=True;
if (o<>portCmd-1) then begin;
  WriteLn('got from invalid port!');
  exit;
  end;
if (bufS<sizeof(hdr)) then begin;
  WriteLn('got a too short message!');
  exit;
  end;
WriteLn('got packet:');
dumpPack;

a:=findOpt(1);
if (a='') then begin;
  WriteLn('client id not found!');
  exit;
  end;
a:=extractDUID(a);
if (a='') then begin;
  WriteLn('bad client id!');
  exit;
  end;
fillchar(ad,sizeof(ad),0);
move(a[1],ad,length(a));

p:=hdr.typ;
case p of
  1:i:=2; {solicit}
  3:i:=7; {request}
  5:i:=7; {renew}
  8:i:=7; {release}
  11:i:=7; {info req}
  else begin;
    WriteLn('invalid message type!');
    exit;
    end;
  end;
inc(p,i shl 8);

i:=0;q:=0;
a:=findOpt(4);if (a<>'') then begin; move(a[1],q,sizeof(q));i:=1; end;{temp address}
a:=findOpt(3);if (a<>'') then begin; move(a[1],q,sizeof(q));i:=2; end;{perm address}
a:=findOpt(25);if (a<>'') then begin; move(a[1],q,sizeof(q));i:=3; end;{prefix delegation}
if (i=0) then WriteLn('no address type requested!');
inc(p,i shl 16);

conNum:=FindOneHardwareAddr(ad);
if (conNum=0) then conNum:=FindOneFreeAddress;
if (conNum=0) then begin;
  WriteLn('no more free addresses, cannot reply...');
  exit;
  end;
ConnectionDat^[conNum].hwA:=ad;
if (ConnectionDat^[conNum].typ=0) then ConnectionDat^[conNum].typ:=1;
ConnectionDat^[conNum].tim:=currentTime;
ad:=ConnectionDat^[conNum].ipA;

WriteLn('delegated='+ipAddr2string(ad));
if (p and $ff=8) then begin;
  if (ConnectionDat^[conNum].typ=1) then ConnectionDat^[conNum].typ:=0;
  WriteLn('ip address now released!');
  end;

a:='adradd--';
move(ConnectionDat^[conNum].ipA,a[9],sizeof(ad));
move(ConnectionDat^[conNum].hwA,a[9+sizeof(ad)],sizeof(ad));
a[0]:=#120;
lowLevelQuery(a);
if (a<>'ok') then begin;
  WriteLn('lower level refused to accept address, cannot reply...');
  exit;
  end;

a:=findOpt(1);
hdr.typ:=(p shr 8) and $ff;
getBeg(hdr.typ);
bufS:=bufP;

addOpt(2,convertDUID(intfAdrH));
addOpt(1,a);
addOpt(23,dnsLists);
addOpt(24,convertFQDN(domainNam));

case (p shr 16) and $ff of
  1:begin; {temp addr}
    a:=ip2bin(ad)+'12341234';
    WriteLongMSB(a[sizeof(ad)+1],leaseTime);
    WriteLongMSB(a[sizeof(ad)+5],leaseTime);
    a:='1234'+mbdOpt(5,a);
    move(q,a[1],sizeof(q));
    addOpt(4,a);
    end;
  2:begin; {perm addr}
    a:=ip2bin(ad)+'12341234';
    WriteLongMSB(a[sizeof(ad)+1],leaseTime);
    WriteLongMSB(a[sizeof(ad)+5],leaseTime);
    a:='123412341234'+mbdOpt(5,a);
    move(q,a[1],sizeof(q));
    WriteLongMSB(a[5],leaseTime shr 1);
    WriteLongMSB(a[9],leaseTime-60);
    addOpt(3,a);
    end;
  3:begin; {prefix}
    a:='12341234'#128+ip2bin(ad);
    WriteLongMSB(a[1],leaseTime);
    WriteLongMSB(a[5],leaseTime);
    a:='123412341234'+mbdOpt(26,a);
    move(q,a[1],sizeof(q));
    WriteLongMSB(a[5],leaseTime shr 1);
    WriteLongMSB(a[9],leaseTime-60);
    addOpt(25,a);
    end;
  end;

UDPsendPacket(pipeCmd,ad,portCmd-1,bufD,bufS);
WriteLn('sent packet:');
dumpPack;
End;
