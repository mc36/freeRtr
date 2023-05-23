Type
  OneAddressRecord=array[1..6] of byte;
  OnePacketRecord=record
    adr:OneAddressRecord;
    typ:Word;
    ver:Byte;
    cod:Byte;
    id:Word;
    len:Word;
    dat:array[1..2*1024] of byte;
    end;
Var
  etherPipe:LongInt;
  upperPipe:LongInt;
  serviceName:String;
  localEth:OneAddressRecord;
  broadEth:OneAddressRecord;
  acessEth:OneAddressRecord;
  connectID:LongInt;

Function convEtherAddr(var d:OneAddressRecord):String;
Var
  i:byte;
  a:String;
Begin;
a:='';
for i:=1 to sizeof(d) do a:=a+'-'+byte2hextype(d[i]);
convEtherAddr:=copy(a,2,255);
End;

Function equalEtherAddr(var d1,d2:OneAddressRecord):Boolean;
Var i:LongInt;
Begin;
equalEtherAddr:=True;
for i:=1 to sizeof(d1) do if (d1[i]<>d2[i]) then exit;
equalEtherAddr:=False;
End;

Function getNextTag(var pck:OnePacketRecord;var p:LongInt;var a:String):Word;
Var
  as:byte absolute a;
  i:LongInt;
Begin;
getNextTag:=ReadWordMSB(pck.dat[p+1]);
i:=ReadWordMSB(pck.dat[p+3]);
as:=i;
move(pck.dat[p+5],a[1],as);
inc(p,i+4);
End;


Procedure parseAllTags(var pck:OnePacketRecord;siz:LongInt;var srvn,acnm,hsti,acck,rlyi,err:String);
Label f1;
Var
  i,o,p:LongInt;
  a:String;
Begin;
srvn:='';
acnm:='';
hsti:='';
acck:='';
rlyi:='';
err:='';
p:=0;
f1:
if (p>=siz) then exit;
i:=getNextTag(pck,p,a);
case i of
  $0000:exit; {end-of-list}
  $0101:srvn:=a; {service-name}
  $0102:acnm:=a; {ac-name}
  $0103:hsti:=a; {host-uniq}
  $0104:acck:=a; {ac-cookie}
  $0105:; {vendor-specific}
  $0110:rlyi:=a; {relay-session-id}
  $0201:err:='service-name-error';
  $0202:err:='ac-system-error';
  $0203:err:='generic-error';
  end;
goto f1;
End;

Procedure putOneTag(var pck:OnePacketRecord;var p:LongInt;t:LongInt;a:String);
Var as:byte absolute a;
Begin;
WriteWordMSB(pck.dat[p+1],t);
WriteWordMSB(pck.dat[p+3],as);
move(a[1],pck.dat[p+5],as);
inc(p,as+4);
End;

Procedure putStandardTags(var pck:OnePacketRecord;var p:LongInt;var acck,rlyi:String);
Begin;
if (acck<>'') then putOneTag(pck,p,$0104,acck); {ac-cookie}
if (rlyi<>'') then putOneTag(pck,p,$0110,rlyi); {relay-session-id}
putOneTag(pck,p,$0103,'cookie'); {host-uniq}
putOneTag(pck,p,0,''); {end-of-list}
End;


Procedure EthernetOpen;
Var
  i,o:LongInt;
  buf:array[1..4*1024] of byte;
  a:String;
Begin;
if (pipeLineCreate(etherPipe,etherPipe,65536,true)<>0) then immErr('unabled to create pipeline!');
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(etherPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],i,sizeof(i));
move(buf[5],i,sizeof(i));
move(buf[9],i,sizeof(i));
move(buf[13],i,sizeof(i));
o:=17;
move(buf[o],localEth,sizeof(localEth));inc(o,sizeof(localEth));
move(buf[o],broadEth,sizeof(broadEth));inc(o,sizeof(broadEth));
a:='';
while (buf[o]<>0) do begin;
  a:=a+chr(buf[o]);
  inc(o);
  end;
writeln('   device: "'+a+'"');
WriteLn('  address: '+convEtherAddr(localEth));
WriteLn('broadcast: '+convEtherAddr(broadEth));
End;


Procedure tryConnecting;
Label f1,f2,f3,f4;
Var
  pck:OnePacketRecord;
  i,o,t:LongInt;
  h,r,e:String;
  a,b,c:String;
Begin;
timer2start;

f1:
WriteLn('sending padi...');
pck.adr:=broadEth;
WriteWordMSB(pck.typ,$8863);
pck.ver:=$11;
pck.cod:=$09;
pck.id:=0;
o:=0;
putOneTag(pck,o,$0101,serviceName); {service name}
a:='';
putStandardTags(pck,o,a,a);
WriteWordMSB(pck.len,o);
pipeLineSend(etherPipe,pck,o+14);
t:=currentTime;

f2:
relequish;
timer2start;
if (getTimePast(t)>1) then goto f1;
o:=sizeof(pck);
if (pipeLineRecv(etherPipe,pck,o)<>0) then o:=0;
dec(o,14);
if (o<0) then goto f2;
if (ReadWordMSB(pck.typ)<>$8863) then goto f2;
if (pck.ver<>$11) then goto f2;
if (pck.cod<>$07) then goto f2;
parseAllTags(pck,o,a,b,c,h,r,e);
if (e<>'') then begin;
  WriteLn('got error ('+e+') from '+convEtherAddr(acessEth));
  goto f2;
  end;
if (serviceName='') then serviceName:=a;
if (a<>serviceName) then begin;
  WriteLn('got service ('+a+') from '+convEtherAddr(acessEth));
  goto f2;
  end;
acessEth:=pck.adr;
writeln('got pado...');
WriteLn('ac name: '+b);
WriteLn('service: '+serviceName);
WriteLn('address: '+convEtherAddr(acessEth));

f3:
WriteLn('sending padr...');
pck.adr:=acessEth;
WriteWordMSB(pck.typ,$8863);
pck.ver:=$11;
pck.cod:=$19;
pck.id:=0;
o:=0;
putOneTag(pck,o,$0101,serviceName); {service name}
putStandardTags(pck,o,h,r);
WriteWordMSB(pck.len,o);
pipeLineSend(etherPipe,pck,o+14);
t:=currentTime;

f4:
relequish;
timer2start;
if (getTimePast(t)>1) then goto f3;
o:=sizeof(pck);
if (pipeLineRecv(etherPipe,pck,o)<>0) then o:=0;
dec(o,14);
if (o<0) then goto f4;
if (ReadWordMSB(pck.typ)<>$8863) then goto f4;
if (pck.ver<>$11) then goto f4;
if (pck.cod<>$65) then goto f4;
if equalEtherAddr(pck.adr,acessEth) then goto f4;
parseAllTags(pck,o,a,b,c,h,r,e);
if (e<>'') then begin;
  WriteLn('got error: '+e);
  goto f4;
  end;
if (a<>serviceName) then begin;
  WriteLn('got service: '+a);
  goto f4;
  end;
connectID:=pck.id;
if (connectID=0) then goto f4;
writeln('got pads...');
End;



Procedure WaitForUpperLayer;
Var
  i,o,p:LongInt;
  a:String;
Begin;
Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
while (pipeLineGetIncoming(upperPipe)<>0) do relequish;
pipeLineEndListen;
a:='pppoe with '+convEtherAddr(acessEth);
BugOS_MyProcessInfo(i,o,p);
i:=i xor p xor o;
p:=((i shr 24) xor (i shr 16) xor (i shr 8) xor i) and $ff;
a:='12341234'#0#0#0#0#0#0#0#0+chr(p)+#255+a+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1490;move(i,a[5],sizeof(i));
pipeLineSend(upperPipe,a[1],length(a));
WriteLn(' done!');
End;



Procedure doConnection;
Label f1,f2;
Var
  pck:OnePacketRecord;
  i,o,p,t:LongInt;
  a:String;

Function srv:Boolean;
Begin;
srv:=false;
if equalEtherAddr(pck.adr,acessEth) then exit;
case ReadWordMSB(pck.typ) of
  $8864:begin;
    if (pck.ver<>$11) then exit;
    if (pck.cod<>$00) then exit;
    if (pck.id<>connectID) then exit;
    p:=ReadWordMSB(pck.len);
    if (p>o) then exit;
    WriteWordMSB(pck.len,$ff03);
    pipeLineSend(upperPipe,pck.len,p+2);
    t:=currentTime;
    end;
  $8863:begin;
    if (pck.ver<>$11) then exit;
    if (pck.cod<>$a7) then exit;
    if (pck.id<>connectID) then exit;
    p:=ReadWordMSB(pck.len);
    if (p>o) then exit;
    srv:=true;
    end;
  else begin;
    i:=ReadWordMSB(pck.typ);
    WriteLn('got invalid type ($'+byte2hextype(i shr 8)+byte2hextype(i)+') packet');
    end;
  end;
End;

Procedure upp;
Begin;
pck.adr:=acessEth;
WriteWordMSB(pck.typ,$8864);
pck.ver:=$11;
pck.cod:=$00;
pck.id:=connectID;
WriteWordMSB(pck.len,o);
pipeLineSend(etherPipe,pck,o+14);
End;

Begin;
WriteLn('connected...');
t:=currentTime;
f1:
relequish;
timer2start;
if (getTimePast(t)>5*60) then begin;
  Write('timeout');
  goto f2;
  end;
o:=sizeof(pck);
if (pipeLineRecv(etherPipe,pck,o)<>0) then o:=0;
dec(o,14);
if (o>=0) then begin;
  if srv then begin; Write('remote closed');goto f2; end;
  end else begin;
  pipeLineStats(etherPipe,o,i,i);
  if (o=0) then begin; Write('interface exited');goto f2; end;
  end;
o:=sizeof(pck.dat);
if (pipeLineRecv(upperPipe,pck.len,o)<>0) then o:=0;
dec(o,2);
if (o>=0) then begin;
  upp;
  end else begin;
  pipeLineStats(upperPipe,o,i,i);
  if (o=0) then begin; Write('upper exited');goto f2; end;
  end;
goto f1;
f2:
Write(', disconnecting...');
pck.adr:=acessEth;
WriteWordMSB(pck.typ,$8863);
pck.ver:=$11;
pck.cod:=$a7;
pck.id:=connectID;
a:='';
p:=0;
putStandardTags(pck,p,a,a);
WriteWordMSB(pck.len,p);
inc(p,14);
pipeLineSend(etherPipe,pck,p);
for i:=1 to 8 do relequish;
WriteLn('');
End;
