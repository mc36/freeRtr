Procedure relequish2lower;
Label f1,f2;
Var
  buf:array[1..4096] of byte;
  i,o,p,q:LongInt;
Begin;
f1:
p:=sizeof(buf);
pipeLineRecv(lowerPipe,buf,p);
if (p<1) then begin;
  pipeLineStats(lowerPipe,o,i,i);
  if (o=0) then immErr('lower closed pipeline!');
  exit;
  end;
if (ReadWordMSB(buf[sizeof(lowerAddr)+1])<>$8847) then begin;
  pipeLineSend(upperPipe,buf,p);
  goto f1;
  end;
o:=sizeof(lowerAddr)+3;
f2:
if (o>p) then goto f1;
i:=ReadLongMSB(buf[o]);
inc(o,4);
if (i and $100=0) then goto f2;
q:=i shr 12;
if (q in [0,2]) then begin;
  if (q=0) then i:=$800 else i:=$86dd;
  dec(o,2);
  WriteWordMSB(buf[o],i);
  dec(o,sizeof(lowerAddr));
  move(buf,buf[o],sizeof(lowerAddr));
  pipeLineSend(upperPipe,buf[o],p-o+1);
  goto f1;
  end;
dec(q,labelConnection);
if (q<1) or (q>SessionNum) then begin;
  WriteLn('got invalid label!');
  exit;
  end;
inc(o,4);
case SessionDat[q].mode of
  11:begin;   {ip}
    dec(o);
    buf[o]:=11;
    end;
  4,5:begin;  {vlan, eth}
    inc(o,sizeof(lowerAddr));
    end;
  6:begin;    {hdlc}
    end;
  7:begin;    {ppp}
    dec(o);
    buf[o]:=3;
    dec(o);
    buf[o]:=11;
    end;
  else exit;
  end;
pipeLineSend(SessionDat[q].pipe,buf[o],p-o+1);
goto f1;
End;



Procedure relequish2upper;
Label f1;
Var
  buf:array[1..4096] of byte;
  i,o,p:LongInt;
Begin;
f1:
p:=sizeof(buf);
pipeLineRecv(upperPipe,buf,p);
if (p<1) then begin;
  pipeLineStats(upperPipe,o,i,i);
  if (o=0) then immErr('upper closed pipeline!');
  exit;
  end;
pipeLineSend(lowerPipe,buf,p);
goto f1;
End;



Procedure relequish2sess(var ses:OneSessionRecord);
Label f1;
Const bufBeg=32;
Var
  buf:array[1..4096] of byte;
  hdr:OneLDPmessageHeader absolute buf;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
case ses.stat of
  0:begin; {init}
    if (ConnectionDat[ses.conn].stat<>3) then goto f1;
    pipeLineStats(ConnectionDat[ses.conn].pipe,p,i,o);
    if (o<1024) then goto f1;
    inc(ConnectionDat[ses.conn].sndM);p:=0;
    a:=#128'tp'#12#0#0#0#0'vcid'#1#4'mt'#12#4#1#0;
    WriteWordMSB(ab[2],ses.mode or $8000);
    WriteLongMSB(ab[9],ses.vcid);
    WriteWordMSB(ab[15],ses.mttu);
    addOneTLV(buf,p,$100,a);
    WriteLongMSB(i,ses.sess+labelConnection);
    addOneTLV(buf,p,$200,convertLong2str(i));
    addOneMsgHeader(buf,p,$400,ConnectionDat[ses.conn].sndM);
    sendOneConnPacket(ConnectionDat[ses.conn],buf,p,0);
    ses.stat:=1;
    end;
  1:begin; {ready}
    end;
  end;
f1:
if (ses.pipe=0) then exit;
p:=sizeof(buf);
pipeLineRecv(ses.pipe,buf[bufBeg],p);
if (p<1) then begin;
  pipeLineStats(ses.pipe,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper logged out!');
  pipeLineClose(ses.pipe);
  ses.pipe:=0;
  exit;
  end;
if (ses.labl<0) then exit;
inc(p,bufBeg);
o:=bufBeg;
case ses.mode of
  11:begin;   {ip}
    inc(o);
    end;
  4,5:begin;  {vlan, eth}
    i:=o;
    dec(o,sizeof(virtMacAd));
    move(buf[i],buf[o],sizeof(virtMacAd));
    move(virtMacAd,buf[i],sizeof(virtMacAd));
    end;
  6:begin;    {hdlc}
    end;
  7:begin;    {ppp}
    inc(o,2);
    end;
  else exit;
  end;
dec(o,4);
ses.pack:=(ses.pack+1) and $ffff;
WriteLongMSB(buf[o],ses.pack);
dec(o,4);
WriteLongMSB(buf[o],(ses.labl shl 12) or $1ff);
i:=ConnectionDat[ses.conn].labl;
if (i<0) then exit;
dec(o,4);
WriteLongMSB(buf[o],(i shl 12) or $ff);
dec(o,2);
WriteWordMSB(buf[o],$8847);
dec(o,sizeof(firstHopM));
move(firstHopM,buf[o],sizeof(firstHopM));
pipeLineSend(lowerPipe,buf[o],p-o);
End;
