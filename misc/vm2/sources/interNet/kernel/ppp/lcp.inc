Var
  LCPworking:Boolean;
  LCPneededOptions:Byte; {1-mru, 2-accm, 4-magic}
  LCPlastidSent:Byte;
  LCPouterEchoes:Byte;
  LCPlocAuth:Byte; {1-pap, 2-chap, 3-eap}
  LCPremAuth:Byte; {1-pap, 2-chap, 3-eap}
  LCPsentAck:Boolean;
  LCPgotAck:Boolean;

Procedure LCPstartNow;
Begin;
LCPneededOptions:=$ff;
LCPlastidSent:=0;
LCPremAuth:=0;
LCPlocAuth:=0;
if serverMode then LCPremAuth:=3;
LCPworking:=True;
LCPsentAck:=False;
LCPgotAck:=False;
WriteLn('starting LCP layer...');
lastSent:=-99999;
End;

Procedure LCPsendProtoRej(var pck:OnePPPpacketRecord;i:LongInt);
Var pck2:OnePPPpacketRecord;
Begin;
i:=i-2;
if (i<1) then exit;
if (i>$400) then i:=$400;
pck2.a:=addressField;
pck2.x:=controlField;
WriteWordMSB(pck2.p,LCPprotocolNumber);
pck2.c:=8;
pck2.i:=Random($100);
WriteWordMSB(pck2.s,i+4);
move(pck.p,pck2.d,i);
pipeLineSend(pipe,pck2,i+8);
WriteLn('LCP sends Protocol-Reject');
End;

Procedure LCPdisplayConf(var pck:OnePPPpacketRecord;var pck2:OnePPPpacketRecord);
Var a:String;
Begin;
PPPoptionStart(pck);
while PPPoptionGet(pck,pck2) do case pck2.i of
  1:WriteLn('  Maximum-Receive-Unit: '+BStr(ReadWordMSB(pck2.d)));
  2:WriteLn('  Async-Control-Character-Map: '+conv2hex(ReadLongMSB(pck2.d),4));
  3:begin;
    case ReadWordMSB(pck2.d) of
      LCPauthPAP:a:='PAP';
      LCPauthCHAP:a:='CHAP';
      LCPauthEAP:a:='EAP';
      else a:='unknown: '+conv2hex(ReadWordMSB(pck2.d),2);
      end;
    WriteLn('  Authentication-Protocol: '+a);
    end;
  4:begin;
    case ReadWordMSB(pck2.d) of
      $c025:a:='Line Quality Report';
      else a:='unknown: '+conv2hex(ReadWordMSB(pck2.d),2);
      end;
    WriteLn('  Quality-Protocol: '+a);
    end;
  5:WriteLn('  Magic-Number: '+conv2hex(ReadLongMSB(pck2.d),4));
  7:WriteLn('  Protocol-Field-Compression');
  8:WriteLn('  Address-and-Control-Field-Compression');
  9:begin;
    case pck2.d[1] of
      1:a:='Null FCS';
      2:a:='CCITT 16-bit FCS';
      4:a:='CCITT 32-bit FCS';
      else a:='unkonwn: '+conv2hex(pck2.d[1],1);
      end;
    WriteLn('  FCS-Alternatives: '+a);
    end;
  10:WriteLn('  Self-Describing-Padding: '+BStr(pck2.d[1])+' padding octets');
  13:begin;
    case pck2.d[1] of
      0:a:='location from authentication';
      1:a:='dialing string';
      2:a:='location identifier';
      3:a:='E.164 number';
      4:a:='distinguished name';
      else a:='unkonwn: '+conv2hex(pck2.d[1],1);
      end;
    WriteLn('  Callback: '+a);
    end;
  15:WriteLn('  Compound-Frames');
  23:WriteLn('  Link-Discriminator: '+conv2hex(ReadWordMSB(pck2.d),2));
  28:begin;
    PPPoptionStart(pck2);
    inc(pck2.pos,2);
    WriteLn('  Internationalization: '+PPPbinaryGet(pck2));
    end;
  else WriteLn('  unknown: '+conv2hex(pck2.i,1));
  end;
if (pck.siz<>pck.pos) then WriteLn('  dummy data at the ending!');
End;

Function LCPgotPacket(var pck:OnePPPpacketRecord):Boolean;
Label f1,f2,f3;
Var
  pck2:OnePPPpacketRecord;
  pck3:OnePPPpacketRecord;
  pck4:OnePPPpacketRecord;
  i,o:LongInt;
  a:String;
Begin;
LCPgotPacket:=False;
Write('LCP got ');
case pck.c of
  1:begin;
    LCPgotPacket:=True;
    WriteLn('Configure-Request; id='+conv2hex(pck.i,1));
    LCPdisplayConf(pck,pck2);
    PPPoptionStart(pck);
    PPPoptionStart(pck3); {nak}
    PPPoptionStart(pck4); {rej}
    pck3.c:=3;pck3.i:=pck.i;
    pck4.c:=4;pck4.i:=pck.i;
    LCPlocAuth:=0;
    RemChrMap:=$ffffffff;
    RemRecvUnit:=LocRecvUnit;
    while PPPoptionGet(pck,pck2) do case pck2.i of
      1:RemRecvUnit:=ReadWordMSB(pck2.d); {Maximum-Receive-Unit}
      2:RemChrMap:=ReadLongMSB(pck2.d); {Async-Control-Character-Map}
      3:begin; {Authentication-Protocol}
        LCPlocAuth:=0;
        case ReadWordMSB(pck2.d) of
          LCPauthPAP:LCPlocAuth:=1;
          LCPauthCHAP:if (pck2.d[3]=5) then LCPlocAuth:=2;
          LCPauthEAP:LCPlocAuth:=3;
          else LCPlocAuth:=0;
          end;
        if (LCPlocAuth<>0) then continue;
        pck2.s:=2;
        WriteWordMSB(pck2.d,LCPauthPAP);
        PPPoptionPut(pck3,pck2);
        end;
      4:goto f2; {Quality-Protocol}
      5:begin; {Magic-Number}
        RemMagicNum:=ReadLongMSB(pck2.d);
        if (LocMagicNum<>RemMagicNum) then continue;
        LocMagicNum:=Random($ffffffff);
        WriteLongMSB(pck2.d,Random($ffffffff));
        PPPoptionPut(pck3,pck2);
        end;
      7:goto f2; {Protocol-Field-Compression}
      8:goto f2; {Address-and-Control-Field-Compression}
      9:goto f2; {FCS-Alternatives}
      10:goto f2; {Self-Describing-Padding}
      13:goto f2; {Callback}
      15:goto f2; {Compound-Frames}
      else begin; {unknown}
        f2:
        PPPoptionPut(pck4,pck2);
        end;
      end;
    a:='Configure-Ack';
    LCPsentAck:=True;
    pck.c:=2;
    if (pck3.pos<>0) then begin;
      a:='Configure-Nak';
      LCPsentAck:=False;
      pck:=pck3;
      end;
    if (pck4.pos<>0) then begin;
      a:='Configure-Reject';
      LCPsentAck:=False;
      pck:=pck4;
      end;
    WriteLn('LCP sends '+a);
    pck.a:=addressField;
    pck.x:=controlField;
    WriteWordMSB(pck.p,LCPprotocolNumber);
    i:=pck.pos+4;
    if (i>$400) then i:=$400;
    WriteWordMSB(pck.s,i);
    pipeLineSend(pipe,pck,i+4);
    LCPdisplayConf(pck,pck2);
    goto f3;
    end;
  2:begin;
    WriteLn('Configure-Ack; id='+conv2hex(pck.i,1));
    if (pck.i<>LCPlastidSent) then goto f3;
    LCPgotAck:=True;
    LCPdisplayConf(pck,pck2);
    goto f3;
    end;
  3:begin;
    WriteLn('Configure-Nak; id='+conv2hex(pck.i,1));
    if (pck.i<>LCPlastidSent) then goto f3;
    PPPoptionStart(pck);
    while PPPoptionGet(pck,pck2) do case pck2.i of
      1:LCPneededOptions:=LCPneededOptions and $fe; {mru}
      2:LCPneededOptions:=LCPneededOptions and $fd; {accm}
      3:begin; {auth}
        inc(LCPremAuth);
        if (LCPremAuth>3) then LCPremAuth:=1;
        end;
      5:LocMagicNum:=Random($ffffffff); {magic}
      end;
    lastSent:=-99999;
    LCPgotAck:=False;
    LCPdisplayConf(pck,pck2);
    goto f3;
    end;
  4:begin;
    WriteLn('Configure-Reject; id='+conv2hex(pck.i,1));
    if (pck.i<>LCPlastidSent) then goto f3;
    PPPoptionStart(pck);
    while PPPoptionGet(pck,pck2) do case pck2.i of
      1:LCPneededOptions:=LCPneededOptions and $fe; {mru}
      2:LCPneededOptions:=LCPneededOptions and $fd; {accm}
      3:begin; {auth}
        inc(LCPremAuth);
        if (LCPremAuth>3) then LCPremAuth:=1;
        end;
      5:LCPneededOptions:=LCPneededOptions and $fb; {magic}
      end;
    lastSent:=-99999;
    LCPgotAck:=False;
    LCPdisplayConf(pck,pck2);
    goto f3;
    end;
  5:begin;
    WriteLn('Terminate-Request');
    pck2.a:=addressField;
    pck2.x:=controlField;
    WriteWordMSB(pck2.p,LCPprotocolNumber);
    pck2.c:=6;
    pck2.i:=pck.i;
    WriteWordMSB(pck2.s,4);
    pipeLineSend(pipe,pck2,8);
    WriteLn('LCP sends Terminate-Ack');
    LCPstartNow;
    goto f3;
    end;
  6:begin;
    WriteLn('Terminate-Ack');
    LCPstartNow;
    goto f3;
    end;
  7:begin;
    WriteLn('Code-Reject; code='+conv2hex(pck.d[1],1));
    goto f3;
    end;
  8:begin;
    WriteLn('Protocol-Reject; protocol='+PPPgetProtocolName(ReadWordMSB(pck.d)));
    goto f3;
    end;
  9:begin;
    WriteLn('Echo-Request; id='+conv2hex(pck.i,1));
    i:=ReadWordMSB(pck.s);
    if (i>$400) then i:=$400;
    pck.a:=addressField;
    pck.x:=controlField;
    WriteWordMSB(pck.p,LCPprotocolNumber);
    pck.c:=10;
    WriteWordMSB(pck.s,i);
{    if (ReadLongMSB(pck.d[1])<>RemMagicNum) then goto f3;}
    WriteLongMSB(pck.d[1],LocMagicNum);
    pipeLineSend(pipe,pck,i+4);
    WriteLn('LCP sends Echo-Reply');
    goto f3;
    end;
  10:begin;
    WriteLn('Echo-Reply; id='+conv2hex(pck.i,1));
    if (pck.i<>LCPlastidSent) then goto f3;
{    if (ReadLongMSB(pck.d[1])<>RemMagicNum) then goto f3;}
    LCPouterEchoes:=0;
    goto f3;
    end;
  11:begin;
    WriteLn('Discard-Request; id='+conv2hex(pck.i,1));
    goto f3;
    end;
  12:begin;
    WriteLn('Identification');
    PPPoptionStart(pck);
    inc(pck.pos,4);
    WriteLn('  message: '+PPPbinaryGet(pck));
    goto f3;
    end;
  13:begin;
    WriteLn('Time-Remaining');
    PPPoptionStart(pck);
    WriteLn('  seconds: '+BStr(ReadLongMSB(pck.d[5])));
    inc(pck.pos,8);
    WriteLn('  message: '+PPPbinaryGet(pck));
    goto f3;
    end;
  else begin;
    WriteLn('unknown code: '+conv2hex(pck.c,1));
    f1:
    i:=ReadWordMSB(pck.s);
    if (i>$400) then i:=$400;
    pck2.a:=addressField;
    pck2.x:=controlField;
    WriteWordMSB(pck2.p,LCPprotocolNumber);
    pck2.c:=7;
    pck2.i:=Random($100);
    WriteWordMSB(pck2.s,i+4);
    move(pck.c,pck2.d,i);
    pipeLineSend(pipe,pck2,i+8);
    WriteLn('LCP sends Code-Reject');
    goto f3;
    end;
  end;
f3:
LCPworking:=not (LCPsentAck and LCPgotAck);
End;

Procedure LCPsendConfReq;
Var
  pck:OnePPPpacketRecord;
  pck2:OnePPPpacketRecord;
  i:LongInt;
Begin;
LCPlastidSent:=Random($100);
LCPgotAck:=False;
PPPoptionStart(pck);
if (LCPneededOptions and 1<>0) then begin; {mru}
  pck2.i:=1;
  pck2.s:=2;
  WriteWordMSB(pck2.d,LocRecvUnit);
  PPPoptionPut(pck,pck2);
  end;
if (LCPneededOptions and 2<>0) then begin; {accm}
  pck2.i:=2;
  pck2.s:=4;
  WriteLongMSB(pck2.d,LocChrMap);
  PPPoptionPut(pck,pck2);
  end;
if (LCPneededOptions and 4<>0) then begin; {magic}
  pck2.i:=5;
  pck2.s:=4;
  WriteLongMSB(pck2.d,LocMagicNum);
  PPPoptionPut(pck,pck2);
  end;
if serverMode then begin; {auth}
  pck2.i:=3;
  case LCPremAuth of
    1:begin;
      pck2.s:=2;
      WriteWordMSB(pck2.d,LCPauthPAP);
      end;
    2:begin;
      pck2.s:=3;
      WriteWordMSB(pck2.d,LCPauthCHAP);
      pck2.d[3]:=5;
      end;
    3:begin;
      pck2.s:=2;
      WriteWordMSB(pck2.d,LCPauthEAP);
      end;
    end;
  PPPoptionPut(pck,pck2);
  end;
pck.a:=addressField;
pck.x:=controlField;
WriteWordMSB(pck.p,LCPprotocolNumber);
pck.c:=1;
pck.i:=LCPlastidSent;
i:=pck.pos+4;
if (i>$400) then i:=$400;
WriteWordMSB(pck.s,i);
pipeLineSend(pipe,pck,i+4);
WriteLn('LCP sends Configure-Request; id='+conv2hex(pck.i,1));
LCPdisplayConf(pck,pck2);
End;

Procedure LCPsendEchoReq;
Var
  pck:OnePPPpacketRecord;
  i:LongInt;
Begin;
LCPlastidSent:=Random($100);
inc(LCPouterEchoes);
PPPoptionStart(pck);
pck.a:=addressField;
pck.x:=controlField;
WriteWordMSB(pck.p,LCPprotocolNumber);
pck.c:=9;
pck.i:=LCPlastidSent;
WriteLongMSB(pck.d[1],LocMagicNum);
WriteLongMSB(pck.d[5],RemMagicNum);
i:=12;
WriteWordMSB(pck.s,i);
pipeLineSend(pipe,pck,i+4);
WriteLn('LCP sends Echo-Request; id='+conv2hex(pck.i,1));
End;
