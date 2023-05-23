Type
  OnePacketRecord=record
    adr:OneAddressRecord;
    typ:Word;
    ver:Byte;
    cod:Byte;
    id:Word;
    len:Word;
    dat:array[1..2*1024] of byte;
    end;

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

Procedure parseAllTags(var pck:OnePacketRecord;siz:LongInt;var srvn,acnm,hsti,acck,rlyi:String);
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

Procedure putStandardTags(var pck:OnePacketRecord;var p:LongInt;var hsti,rlyi:String);
Begin;
putOneTag(pck,p,$0102,proggyName+' for BugOS (mac='+convEtherAddr(localEth)+')'); {ac name}
if (hsti<>'') then putOneTag(pck,p,$0103,hsti); {host-uniq}
if (rlyi<>'') then putOneTag(pck,p,$0110,rlyi); {relay-session-id}
putOneTag(pck,p,$0104,'cookie'); {ac-cookie}
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



Procedure sendDisconnect(adr:OneAddressRecord;id:LongInt);
Var
  pck:OnePacketRecord;
  p:LongInt;
  a:String;
Begin;
pck.adr:=adr;
WriteWordMSB(pck.typ,$8863);
pck.ver:=$11;
pck.cod:=$a7;
pck.id:=id;
a:='';
p:=0;
putStandardTags(pck,p,a,a);
WriteWordMSB(pck.len,p);
pipeLineSend(etherPipe,pck,p+14);
End;


Procedure gotOnePacket(var pck:OnePacketRecord;siz:LongInt);
Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
  h,r:String;
Begin;
if (siz<0) then exit;
case ReadWordMSB(pck.typ) of
  $8864:begin; {data}
    o:=ReadWordMSB(pck.len);
    if (o>siz) then exit;
    if (pck.ver<>$11) then exit;
    if (pck.cod<>0) then exit;
    i:=FindConnectByID(pck.id);
    if (i<1) then begin;
      f1:
      {$ifdef debug}WriteLn('got invalid data from '+convEtherAddr(pck.adr));{$endif}
      sendDisconnect(pck.adr,pck.id);
      exit;
      end;
    if equalEtherAddr(pck.adr,ConnectionDat^[i].addr) then goto f1;
    ConnectionDat^[i].time:=currentTime;
    WriteWordMSB(pck.len,$ff03);
    pipeLineSend(ConnectionDat^[i].pipe,pck.len,o+2);
    exit;
    end;
  $8863:begin; {discover}
    o:=ReadWordMSB(pck.len);
    if (o>siz) then exit;
    if (pck.ver<>$11) then exit;
    case pck.cod of
      $09:begin; {padi-active discovery initiation}
        {$ifdef debug}WriteLn('got padi from '+convEtherAddr(pck.adr));{$endif}
        if (pck.id<>0) then exit;
        parseAllTags(pck,o,b,a,h,a,r);
        if (b<>serviceName) and (b<>'') then exit;
        o:=0;
        putOneTag(pck,o,$0101,serviceName); {service name}
        putStandardTags(pck,o,h,r);
        pck.cod:=$07;
        WriteWordMSB(pck.len,o);
        pipeLineSend(etherPipe,pck,o+14);
        end;
      $07:begin; {pado-active discovery offer}
        {$ifdef debug}WriteLn('got pado from '+convEtherAddr(pck.adr));{$endif}
        end;
      $19:begin; {padr-active discovery request}
        {$ifdef debug}WriteLn('got padr from '+convEtherAddr(pck.adr));{$endif}
        if (pck.id<>0) then exit;
        parseAllTags(pck,o,b,a,h,a,r);
        if (b<>serviceName) and (b<>'') then exit;
        p:=FindConnectByStat(0);
        if (p<1) then begin;
          WriteLn('out of available connection slots!');
          exit;
          end;
        repeat
          nextConnID:=(nextConnID+1) and $ffff;
          if (nextConnID and $ff in [$00,$ff]) then continue;
          until (FindConnectByID(nextConnID)=0);
        con:=ConnectionDat^[p];
        a:=con.cmnd;
        i:=pos(' ',a);
        if (i<1) then i:=666;
        if (xExecBgnd(copy(a,1,i-1),copy(a,i+1,255),o)<>0) then o:=0;
        if (o=0) then begin;
          WriteLn('failed to start upper process!');
          exit;
          end;
        con.stat:=1;
        con.time:=currentTime;
        con.cnid:=nextConnID;
        con.pipe:=0;
        con.addr:=pck.adr;
        ConnectionDat^[p]:=con;
        o:=0;
        putOneTag(pck,o,$0101,serviceName); {service name}
        putStandardTags(pck,o,h,r);
        pck.id:=nextConnID;
        pck.cod:=$65;
        WriteWordMSB(pck.len,o);
        pipeLineSend(etherPipe,pck,o+14);
        WriteLn('incoming connection from '+convEtherAddr(con.addr));
        end;
      $65:begin; {pads-active discovery session-confirmation}
        {$ifdef debug}WriteLn('got pads from '+convEtherAddr(pck.adr));{$endif}
        end;
      $a7:begin; {padt-active discovery terminate}
        {$ifdef debug}WriteLn('got padt from '+convEtherAddr(pck.adr));{$endif}
        i:=FindConnectByID(pck.id);
        if (i<1) then exit;
        if equalEtherAddr(pck.adr,ConnectionDat^[i].addr) then exit;
        pipeLineClose(ConnectionDat^[i].pipe);
        WriteLn('connection closed by '+convEtherAddr(con.addr));
        end;
      else WriteLn('got invalid code ($'+byte2hextype(pck.cod)+') packet from '+convEtherAddr(pck.adr));
      end;
    end;
  else begin;
    i:=ReadWordMSB(pck.typ);
    WriteLn('got invalid type ($'+byte2hextype(i shr 8)+byte2hextype(i)+') packet from '+convEtherAddr(pck.adr));
    end;
  end;
End;




Procedure relequishConn(var con:OneConnectionRecord);
Label f1;
Var
  pck:OnePacketRecord;
  i,o:LongInt;
Begin;
if (con.stat=0) then exit;
if (getTimePast(con.time)>5*60) then begin;
  WriteLn('connection timed out with '+convEtherAddr(con.addr));
  f1:
  sendDisconnect(con.addr,con.cnid);
  pipeLineClose(con.pipe);
  con.stat:=0;
  exit;
  end;
if (con.stat=1) then exit;
o:=sizeof(pck.dat);
if (pipeLineRecv(con.pipe,pck.len,o)<>0) then o:=0;
if (o=0) then begin;
  pipeLineStats(con.pipe,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper terminated with '+convEtherAddr(con.addr));
  goto f1;
  end;
dec(o,2);
if (o<0) then exit;
pck.adr:=con.addr;
WriteWordMSB(pck.typ,$8864);
pck.ver:=$11;
pck.cod:=$00;
pck.id:=con.cnid;
WriteWordMSB(pck.len,o);
pipeLineSend(etherPipe,pck,o+14);
End;
