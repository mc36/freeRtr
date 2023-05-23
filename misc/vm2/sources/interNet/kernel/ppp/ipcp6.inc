Var
  IPCP6working:Boolean;
  IPCP6neededOptions:Byte;
  IPCP6lastidSent:Byte;
  IPCP6sentAck:Boolean;
  IPCP6gotAck:Boolean;
  IPCP6ifIdLoc:array[1..8] of byte;
  IPCP6ifIdRem:array[1..8] of byte;

Procedure IPCP6startNow;
Var i:LongInt;
Begin;
IPCP6neededOptions:=$ff;
IPCP6lastidSent:=0;
IPCP6working:=True;
IPCP6sentAck:=False;
IPCP6gotAck:=False;
for i:=1 to sizeof(IPCP6ifIdLoc) do IPCP6ifIdLoc[i]:=random($100);
fillchar(IPCP6ifIdRem,sizeof(IPCP6ifIdRem),0);
WriteLn('starting IPCPv6 layer...');
lastSent:=-99999;
End;


Procedure IPCP6displayConf(var pck:OnePPPpacketRecord;var pck2:OnePPPpacketRecord);
Var
  a,b:String;
  i:LongInt;
Begin;
PPPoptionStart(pck);
while PPPoptionGet(pck,pck2) do case pck2.i of
  1:begin;
    a:='';
    for i:=1 to pck2.s do a:=a+byte2hextype(pck2.d[i]);
    WriteLn('Interface-Identifier: '+a);
    end;
  2:begin;
    WriteLn('IPv6-Compression-Protocol: '+conv2hex(ReadWordMSB(pck2.d),2));
    end;
  else WriteLn('  unknown: '+conv2hex(pck2.i,1));
  end;
if (pck.siz<>pck.pos) then WriteLn('  dummy data at the ending!');
End;

Function IPCP6gotPacket(var pck:OnePPPpacketRecord):Boolean;
Label f1,f2,f3;
Var
  pck2:OnePPPpacketRecord;
  pck3:OnePPPpacketRecord;
  pck4:OnePPPpacketRecord;
  i,o,p:LongInt;
  a:String;
Begin;
IPCP6gotPacket:=False;
Write('IPCPv6 got ');
case pck.c of
  1:begin;
    IPCP6gotPacket:=True;
    WriteLn('Configure-Request; id='+conv2hex(pck.i,1));
    IPCP6displayConf(pck,pck2);
    PPPoptionStart(pck);
    PPPoptionStart(pck3); {nak}
    PPPoptionStart(pck4); {rej}
    pck3.c:=3;pck3.i:=pck.i;
    pck4.c:=4;pck4.i:=pck.i;
    while PPPoptionGet(pck,pck2) do case pck2.i of
      1:begin; {Interface-Identifier}
        move(pck2.d,IPCP6ifIdRem,sizeof(IPCP6ifIdRem));
        end;
      2:goto f2; {IPv6-Compression-Protocol}
      else begin; {unknown}
        f2:
        PPPoptionPut(pck4,pck2);
        end;
      end;
    a:='Configure-Ack';
    IPCP6sentAck:=True;
    pck.c:=2;
    if (pck3.pos<>0) then begin;
      a:='Configure-Nak';
      IPCP6sentAck:=False;
      pck:=pck3;
      end;
    if (pck4.pos<>0) then begin;
      a:='Configure-Reject';
      IPCP6sentAck:=False;
      pck:=pck4;
      end;
    WriteLn('IPCPv6 sends '+a);
    pck.a:=addressField;
    pck.x:=controlField;
    WriteWordMSB(pck.p,IPCPV6protocolNumber);
    i:=pck.pos+4;
    if (i>$400) then i:=$400;
    WriteWordMSB(pck.s,i);
    pipeLineSend(pipe,pck,i+4);
    IPCP6displayConf(pck,pck2);
    goto f3;
    end;
  2:begin;
    WriteLn('Configure-Ack; id='+conv2hex(pck.i,1));
    if (pck.i<>IPCP6lastidSent) then goto f3;
    IPCP6gotAck:=True;
    IPCP6displayConf(pck,pck2);
    goto f3;
    end;
  3:begin;
    WriteLn('Configure-Nak; id='+conv2hex(pck.i,1));
    if (pck.i<>IPCP6lastidSent) then goto f3;
    PPPoptionStart(pck);
    while PPPoptionGet(pck,pck2) do case pck2.i of
      1:begin; {Interface-Identifier}
        move(pck2.d,IPCP6ifIdLoc,sizeof(IPCP6ifIdLoc));
        end;
      2:; {IPv6-Compression-Protocol}
      end;
    lastSent:=-99999;
    IPCP6gotAck:=False;
    IPCP6displayConf(pck,pck2);
    goto f3;
    end;
  4:begin;
    WriteLn('Configure-Reject; id='+conv2hex(pck.i,1));
    if (pck.i<>IPCP6lastidSent) then goto f3;
    PPPoptionStart(pck);
    while PPPoptionGet(pck,pck2) do case pck2.i of
      1:; {Interface-Identifier}
      2:; {IPv6-Compression-Protocol}
      end;
    lastSent:=-99999;
    IPCP6gotAck:=False;
    IPCP6displayConf(pck,pck2);
    goto f3;
    end;
  5:begin;
    WriteLn('Terminate-Request');
    pck2.a:=addressField;
    pck2.x:=controlField;
    WriteWordMSB(pck2.p,IPCPV6protocolNumber);
    pck2.c:=6;
    pck2.i:=pck.i;
    WriteWordMSB(pck2.s,4);
    pipeLineSend(pipe,pck2,8);
    WriteLn('IPCPv6 sends Terminate-Ack');
    IPCP6startNow;
    goto f3;
    end;
  6:begin;
    WriteLn('Terminate-Ack');
    IPCP6startNow;
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
    WriteWordMSB(pck2.p,IPCPV6protocolNumber);
    pck2.c:=7;
    pck2.i:=Random($100);
    WriteWordMSB(pck2.s,i+4);
    move(pck.c,pck2.d,i);
    pipeLineSend(pipe,pck2,i+8);
    WriteLn('IPCPv6 sends Code-Reject');
    goto f3;
    end;
  end;
f3:
IPCP6working:=not (IPCP6sentAck and IPCP6gotAck);
End;

Procedure IPCP6sendConfReq;
Var
  pck:OnePPPpacketRecord;
  pck2:OnePPPpacketRecord;
  i:LongInt;
Begin;
IPCP6lastidSent:=Random($100);
IPCP6gotAck:=False;
PPPoptionStart(pck);
pck2.i:=1;
pck2.s:=sizeof(IPCP6ifIdLoc);
move(IPCP6ifIdLoc,pck2.d,sizeof(IPCP6ifIdLoc));
PPPoptionPut(pck,pck2);
pck.a:=addressField;
pck.x:=controlField;
WriteWordMSB(pck.p,IPCPV6protocolNumber);
pck.c:=1;
pck.i:=IPCP6lastidSent;
i:=pck.pos+4;
if (i>$400) then i:=$400;
WriteWordMSB(pck.s,i);
pipeLineSend(pipe,pck,i+4);
WriteLn('IPCPv6 sends Configure-Request; id='+conv2hex(pck.i,1));
IPCP6displayConf(pck,pck2);
End;
