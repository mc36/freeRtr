Var
  IPCP4working:Boolean;
  IPCP4neededOptions:Byte; {1-addrs, 2-addr, 4-dns1, 8-dns2}
  IPCP4lastidSent:Byte;
  IPCP4sentAck:Boolean;
  IPCP4gotAck:Boolean;

Procedure IPCP4startNow;
Begin;
IPCP4neededOptions:=$ff;
IPCP4lastidSent:=0;
IPCP4working:=True;
IPCP4sentAck:=False;
IPCP4gotAck:=False;
WriteLn('starting IPCPv4 layer...');
lastSent:=-99999;
End;


Procedure IPCP4displayConf(var pck:OnePPPpacketRecord;var pck2:OnePPPpacketRecord);
Var a,b:String;
Begin;
PPPoptionStart(pck);
while PPPoptionGet(pck,pck2) do case pck2.i of
  1:begin;
    a:=IPv4addressPrefix;
    b:=IPv4addressPrefix;
    move(pck2.d[1],a[13],4);
    move(pck2.d[5],b[13],4);
    WriteLn('  IP-Addresses: source='+ipAddr2string(a[1])+' target='+ipAddr2string(b[1]));
    end;
  2:begin;
    case ReadWordMSB(pck2.d) of
      $002d:a:='Van Jacobson Compressed TCP/IP';
      else a:='unknown: '+conv2hex(ReadWordMSB(pck2.d),2);
      end;
    WriteLn('  IP-Compression-Protocol: '+a);
    end;
  3:begin;
    a:=IPv4addressPrefix;
    move(pck2.d,a[13],4);
    WriteLn('  IP-Address: '+ipAddr2string(a[1]));
    end;
  4:begin;
    a:=IPv4addressPrefix;
    move(pck2.d,a[13],4);
    WriteLn('  Mobile-Node-Home-Address: '+ipAddr2string(a[1]));
    end;
  129:begin;
    a:=IPv4addressPrefix;
    move(pck2.d,a[13],4);
    WriteLn('  Primary-DNS-Address: '+ipAddr2string(a[1]));
    end;
  130:begin;
    a:=IPv4addressPrefix;
    move(pck2.d,a[13],4);
    WriteLn('  Primary-NBNS-Address: '+ipAddr2string(a[1]));
    end;
  131:begin;
    a:=IPv4addressPrefix;
    move(pck2.d,a[13],4);
    WriteLn('  Secondary-DNS-Address: '+ipAddr2string(a[1]));
    end;
  132:begin;
    a:=IPv4addressPrefix;
    move(pck2.d,a[13],4);
    WriteLn('  Secondary-NBNS-Address: '+ipAddr2string(a[1]));
    end;
  else WriteLn('  unknown: '+conv2hex(pck2.i,1));
  end;
if (pck.siz<>pck.pos) then WriteLn('  dummy data at the ending!');
End;

Function IPCP4gotPacket(var pck:OnePPPpacketRecord):Boolean;
Label f1,f2,f3;
Var
  pck2:OnePPPpacketRecord;
  pck3:OnePPPpacketRecord;
  pck4:OnePPPpacketRecord;
  i,o,p:LongInt;
  a:String;
Begin;
IPCP4gotPacket:=False;
Write('IPCPv4 got ');
case pck.c of
  1:begin;
    IPCP4gotPacket:=True;
    WriteLn('Configure-Request; id='+conv2hex(pck.i,1));
    IPCP4displayConf(pck,pck2);
    PPPoptionStart(pck);
    PPPoptionStart(pck3); {nak}
    PPPoptionStart(pck4); {rej}
    pck3.c:=3;pck3.i:=pck.i;
    pck4.c:=4;pck4.i:=pck.i;
    while PPPoptionGet(pck,pck2) do case pck2.i of
      1:begin; {IP-Addresses}
        i:=0;o:=0;p:=0;
        move(pck2.d[1],i,4);
        move(RemIP[13],o,4);
        if (i=0) then inc(p);
        if serverMode and (i<>o) then inc(p);
        move(pck2.d[5],i,4);
        move(LocIP[13],o,4);
        if (i=0) then inc(p);
        if serverMode and (i<>o) then inc(p);
        if (p=0) then begin;
          if serverMode then continue;
          RemIP:=IPv4addressPrefix;
          LocIP:=IPv4addressPrefix;
          move(pck2.d[1],RemIP[13],4);
          move(pck2.d[5],LocIP[13],4);
          continue;
          end;
        move(RemIP[13],pck2.d[1],4);
        move(LocIP[13],pck2.d[5],4);
        PPPoptionPut(pck3,pck2);
        end;
      2:goto f2; {IP-Compression-Protocol}
      3:begin; {IP-Address}
        i:=0;p:=0;
        move(pck2.d,i,4);
        move(RemIP[13],o,4);
        if (i=0) then inc(p);
        if serverMode and (i<>o) then inc(p);
        if (p=0) then begin;
          if serverMode then continue;
          RemIP:=IPv4addressPrefix;
          move(pck2.d,RemIP[13],4);
          continue;
          end;
        move(RemIP[13],pck2.d,4);
        PPPoptionPut(pck3,pck2);
        end;
      129:begin; {Primary-DNS-Address}
        i:=0;p:=0;
        move(pck2.d,i,4);
        move(dns1ip[13],o,4);
        if (i=0) then inc(p);
        if serverMode and (i<>o) then inc(p);
        if (p=0) then begin;
          if serverMode then continue;
          dns1ip:=IPv4addressPrefix;
          move(pck2.d,dns1ip[13],4);
          continue;
          end;
        move(dns1ip[13],pck2.d,4);
        PPPoptionPut(pck3,pck2);
        end;
      130:goto f2; {Primary-NBNS-Address}
      131:begin; {Secondary-DNS-Address}
        i:=0;p:=0;
        move(pck2.d,i,4);
        move(dns2ip[13],o,4);
        if serverMode and (i<>o) then inc(p);
        if (p=0) then begin;
          if serverMode then continue;
          dns2ip:=IPv4addressPrefix;
          move(pck2.d,dns2ip[13],4);
          continue;
          end;
        move(dns2ip[13],pck2.d,4);
        PPPoptionPut(pck3,pck2);
        end;
      130:goto f2; {Secondary-NBNS-Address}
      else begin; {unknown}
        f2:
        PPPoptionPut(pck4,pck2);
        end;
      end;
    a:='Configure-Ack';
    IPCP4sentAck:=True;
    pck.c:=2;
    if (pck3.pos<>0) then begin;
      a:='Configure-Nak';
      IPCP4sentAck:=False;
      pck:=pck3;
      end;
    if (pck4.pos<>0) then begin;
      a:='Configure-Reject';
      IPCP4sentAck:=False;
      pck:=pck4;
      end;
    WriteLn('IPCPv4 sends '+a);
    pck.a:=addressField;
    pck.x:=controlField;
    WriteWordMSB(pck.p,IPCPV4protocolNumber);
    i:=pck.pos+4;
    if (i>$400) then i:=$400;
    WriteWordMSB(pck.s,i);
    pipeLineSend(pipe,pck,i+4);
    IPCP4displayConf(pck,pck2);
    goto f3;
    end;
  2:begin;
    WriteLn('Configure-Ack; id='+conv2hex(pck.i,1));
    if (pck.i<>IPCP4lastidSent) then goto f3;
    IPCP4gotAck:=True;
    IPCP4displayConf(pck,pck2);
    goto f3;
    end;
  3:begin;
    WriteLn('Configure-Nak; id='+conv2hex(pck.i,1));
    if (pck.i<>IPCP4lastidSent) then goto f3;
    PPPoptionStart(pck);
    while PPPoptionGet(pck,pck2) do case pck2.i of
      1:if not serverMode then begin; {addrs}
        LocIP:=IPv4addressPrefix;
        move(pck2.d[1],LocIP[13],4);
        RemIP:=IPv4addressPrefix;
        move(pck2.d[5],RemIP[13],4);
        end;
      3:if not serverMode then begin; {addr}
        LocIP:=IPv4addressPrefix;
        move(pck2.d,LocIP[13],4);
        end;
      129:if not serverMode then begin; {dns1}
        dns1ip:=IPv4addressPrefix;
        move(pck2.d,dns1ip[13],4);
        end;
      131:if not serverMode then begin; {dns2}
        dns2ip:=IPv4addressPrefix;
        move(pck2.d,dns2ip[13],4);
        end;
      end;
    lastSent:=-99999;
    IPCP4gotAck:=False;
    IPCP4displayConf(pck,pck2);
    goto f3;
    end;
  4:begin;
    WriteLn('Configure-Reject; id='+conv2hex(pck.i,1));
    if (pck.i<>IPCP4lastidSent) then goto f3;
    PPPoptionStart(pck);
    while PPPoptionGet(pck,pck2) do case pck2.i of
      1:IPCP4neededOptions:=IPCP4neededOptions and $fe; {addrs}
      3:IPCP4neededOptions:=IPCP4neededOptions and $fd; {addr}
      129:IPCP4neededOptions:=IPCP4neededOptions and $fb; {dns1}
      131:IPCP4neededOptions:=IPCP4neededOptions and $f7; {dns2}
      end;
    lastSent:=-99999;
    IPCP4gotAck:=False;
    IPCP4displayConf(pck,pck2);
    goto f3;
    end;
  5:begin;
    WriteLn('Terminate-Request');
    pck2.a:=addressField;
    pck2.x:=controlField;
    WriteWordMSB(pck2.p,IPCPV4protocolNumber);
    pck2.c:=6;
    pck2.i:=pck.i;
    WriteWordMSB(pck2.s,4);
    pipeLineSend(pipe,pck2,8);
    WriteLn('IPCPv4 sends Terminate-Ack');
    IPCP4startNow;
    goto f3;
    end;
  6:begin;
    WriteLn('Terminate-Ack');
    IPCP4startNow;
    goto f3;
    end;
  7:begin;
    WriteLn('Code-Reject; code='+conv2hex(pck.d[1],1));
    goto f3;
    end;
  else begin;
    WriteLn('unknown code: '+conv2hex(pck.c,1));
    f1:
    i:=ReadWordMSB(pck.s);
    if (i>$400) then i:=$400;
    pck2.a:=addressField;
    pck2.x:=controlField;
    WriteWordMSB(pck2.p,IPCPV4protocolNumber);
    pck2.c:=7;
    pck2.i:=Random($100);
    WriteWordMSB(pck2.s,i+4);
    move(pck.c,pck2.d,i);
    pipeLineSend(pipe,pck2,i+8);
    WriteLn('IPCPv4 sends Code-Reject');
    goto f3;
    end;
  end;
f3:
IPCP4working:=not (IPCP4sentAck and IPCP4gotAck);
End;

Procedure IPCP4sendConfReq;
Var
  pck:OnePPPpacketRecord;
  pck2:OnePPPpacketRecord;
  i:LongInt;
Begin;
IPCP4lastidSent:=Random($100);
IPCP4gotAck:=False;
PPPoptionStart(pck);
if (IPCP4neededOptions and 1<>0) then begin; {addrs}
  pck2.i:=1;
  pck2.s:=8;
  move(LocIP[13],pck2.d[1],4);
  move(RemIP[13],pck2.d[5],4);
  PPPoptionPut(pck,pck2);
  end;
if (IPCP4neededOptions and 2<>0) then begin; {addr}
  pck2.i:=3;
  pck2.s:=4;
  move(LocIP[13],pck2.d[1],4);
  PPPoptionPut(pck,pck2);
  end;
if (IPCP4neededOptions and 4<>0) then begin; {dns1}
  pck2.i:=129;
  pck2.s:=4;
  move(dns1ip[13],pck2.d[1],4);
  PPPoptionPut(pck,pck2);
  end;
if (IPCP4neededOptions and 8<>0) then begin; {dns2}
  pck2.i:=131;
  pck2.s:=4;
  move(dns2ip[13],pck2.d[1],4);
  PPPoptionPut(pck,pck2);
  end;
pck.a:=addressField;
pck.x:=controlField;
WriteWordMSB(pck.p,IPCPV4protocolNumber);
pck.c:=1;
pck.i:=IPCP4lastidSent;
i:=pck.pos+4;
if (i>$400) then i:=$400;
WriteWordMSB(pck.s,i);
pipeLineSend(pipe,pck,i+4);
WriteLn('IPCPv4 sends Configure-Request; id='+conv2hex(pck.i,1));
IPCP4displayConf(pck,pck2);
End;
