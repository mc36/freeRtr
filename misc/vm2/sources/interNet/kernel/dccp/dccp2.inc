Function relequish2connection(var con:OneConnectionRecord):Boolean;
Label f1;
Var
  tcpDat:OneTCPcommandHeader;
  buf:array[1..4*1024] of byte;
  hdrES:OneDCCPheaderExtSngRecord absolute buf;
  hdrED:OneDCCPheaderExtDblRecord absolute buf;
  hdrSS:OneDCCPheaderStdSngRecord absolute buf;
  hdrSD:OneDCCPheaderStdDblRecord absolute buf;
  i,o,p:LongInt;
  a:String;
Begin;
relequish2connection:=False;
case con.stat of
  3:begin; {opened}
    if (getTimePast(con.timeT)>10) then begin;
      if (getTimePast(con.timeR)>180) then goto f1;
      sendOneDCCPpacket(con,'',a,$303,0);
      con.timeT:=currentTime;
      con.nxtOp:='';
      end;
    o:=sizeof(buf);
    pipeLineRecv(con.pipe,buf,o);
    if (o<1) then begin;
      pipeLineStats(con.pipe,o,i,i);
      if (o<>0) then exit;
      con.timeT:=-9999999;
      con.timeR:=currentTime;
      con.stat:=5;
      exit;
      end;
    sendOneDCCPpacket(con,con.nxtOp,buf,$304,o);
    con.nxtOp:='';
    con.timeT:=currentTime;
    end;
  5:begin; {closing}
    if (getTimePast(con.timeT)<5) then exit;
    if (getTimePast(con.timeR)>60) then goto f1;
    con.timeT:=currentTime;
    sendOneDCCPpacket(con,'',a,$306,0);
    end;
  4:begin; {connecting}
    if (getTimePast(con.timeT)<5) then exit;
    if (getTimePast(con.timeR)>60) then goto f1;
    con.timeT:=currentTime;
    a:='1234';
    WriteLongMSB(a[1],con.srvc);
    putOneOption(a,#$22#1#2);
    putOneOption(a,#$20#1#2);
    sendOneDCCPpacket(con,a,a,$100,0);
    end;
  2:begin; {listening}
    pipeLineStats(con.pipe,o,i,i);
    if (o=con.proc) then exit;
    WriteLn('listening closed on port '+BStr(con.portL));
    goto f1;
    end;
  1:begin; {command}
    pipeLineStats(con.pipe,i,o,p);
    if (o<sizeof(tcpDat)) then exit;
    i:=sizeof(tcpDat);
    pipeLineRecv(con.pipe,tcpDat,i);
    if (i<>sizeof(tcpDat)) then exit;
    if (tcpDat.cmd=2) then begin;
      BugOS_ProcessName(con.proc,buf,i,i,o);
      p:=tcpDat.prt;
      if (o and $40=0) then p:=0;
      while (p<1) or (p>$ffff) or (findOneListening(p)<>0) do p:=random($3000)+$a000;
      con.portL:=p;
      con.addr:=tcpDat.adr;
      fillchar(tcpDat,sizeof(tcpDat),0);
      tcpDat.cmd:=3;
      move(localAddr,tcpDat.adr,sizeof(tcpDat.adr));
      tcpDat.prt:=con.portL;
      pipeLineSend(con.pipe,tcpDat,sizeof(tcpDat));
      WriteLn('listening opened on port '+BStr(con.portL));
      con.stat:=2;
      exit;
      end;
    if (tcpDat.cmd<>6) then goto f1;
    con.addr:=tcpDat.adr;
    con.portR:=tcpDat.prt;
    repeat
      p:=random($3000)+$d000;
      until (findOnePeerAddr(con.addr,p,con.portR)=0);
    con.portL:=p;
    con.seqTa:=random($fffffff);
    con.seqTb:=random($fff);
    con.stat:=6;
    end;
  6:begin; {service code}
    i:=sizeof(o);
    pipeLineRecv(con.pipe,o,i);
    if (i<>sizeof(o)) then exit;
    con.srvc:=o;
    con.timeR:=currentTime;
    con.timeT:=-9999999;
    WriteLn('connecting to '+ipAddr2string(con.addr)+' '+BStr(con.portR));
    con.stat:=4;
    end;
  else begin; f1:con.stat:=0;relequish2connection:=true;exit; end;
  end;
End;



Procedure gotOnePacket(siz:LongInt;var addr,pack);
Label f1,reset,update;
Var
  buf:array[1..1] of byte absolute pack;
  adr:OneTCPaddressRecord absolute addr;
  hdrSS:OneDCCPheaderStdSngRecord absolute pack;
  hdrSD:OneDCCPheaderStdDblRecord absolute pack;
  hdrES:OneDCCPheaderExtSngRecord absolute pack;
  hdrED:OneDCCPheaderExtDblRecord absolute pack;
  tcpDat:OneTCPcommandHeader;
  con:OneConnectionRecord;
  num:LongInt;
  beg:LongInt;
  i,o,p:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Procedure getSeq;
Begin;
if (hdrSS.typ and 1=1) then begin;
  con.seqRa:=ReadLongMSB(hdrES.seqLo);
  con.seqRb:=ReadWordMSB(hdrES.seqHi);
  end else begin;
  con.seqRa:=(con.seqRa and $ff000000) or (ReadLongMSB(hdrSS.typ) and $ffffff);
  end;
End;

Function processOptions(a:String):String;
Label f1,f2;
Var
  b,c:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
  cb:array[0..1] of byte absolute c;
  cb0:byte absolute c;
Begin;
bb0:=0;
f1:
c:=getOneOption(a);
if (cb0<1) then goto f2;
case cb[1] of
  32:begin; {changeL}
    {$ifdef debug}writeln('    '+decodeFeatureCode(cb[2]));{$endif}
    case cb[2] of
      1:begin; {ccid}
        i:=cb[3];
        if (i<2) or (i>3) then i:=2;
        putOneOption(b,#35#1+chr(i));
        end;
      end;
    end;
  33:begin; {confirmL}
    {$ifdef debug}writeln('    '+decodeFeatureCode(cb[2]));{$endif}
    end;
  34:begin; {changeR}
    {$ifdef debug}writeln('    '+decodeFeatureCode(cb[2]));{$endif}
    case cb[2] of
      1:begin; {ccid}
        i:=cb[3];
        if (i<2) or (i>3) then i:=2;
        putOneOption(b,#33#1+chr(i));
        end;
      end;
    end;
  35:begin; {confirmR}
    {$ifdef debug}writeln('    '+decodeFeatureCode(cb[2]));{$endif}
    end;
  end;

goto f1;
f2:
processOptions:=b;
End;

Begin;
{$ifdef debug}writeLn('rx '+ipAddr2string(addr)+' '+BStr(ReadWordMSB(hdrSS.src))+' '+BStr(ReadWordMSB(hdrSS.trg))+' '+decodeOneTypeCode((hdrSS.typ shr 1)));{$endif}
if (siz<sizeof(hdrSS)) then begin;
  WriteLn('got too small packet from '+ipAddr2string(addr));
  exit;
  end;
beg:=hdrSS.datOfs shl 2;
if (siz<beg) then begin;
  WriteLn('got truncated packet from '+ipAddr2string(addr));
  exit;
  end;
p:=hdrSS.ccCS and $f;
if (p=0) then p:=siz else p:=((p-1)*4)+beg;
if (TestPseudoSum(localAddr,adr,buf,ipProto,p)<>$ffff) then begin;
  WriteLn('got bad checksum from '+ipAddr2string(addr));
  exit;
  end;
if ((hdrSS.typ shr 1) and $f in [1,3..7]) then begin;
  if (hdrSS.typ and 1=0) then p:=sizeof(hdrSD) else p:=sizeof(hdrED);
  end else begin;
  if (hdrSS.typ and 1=0) then p:=sizeof(hdrSS) else p:=sizeof(hdrES);
  end;
i:=beg-p;
if (i<0) then begin;
  WriteLn('got too short header from '+ipAddr2string(addr));
  exit;
  end;
ab0:=i;
move(buf[p+1],ab[1],ab0);
dec(siz,beg);
inc(beg);
if (siz<0) then begin;
  WriteLn('got truncated header from '+ipAddr2string(addr));
  exit;
  end;
num:=findOnePeerAddr(addr,ReadWordMSB(hdrSS.trg),ReadWordMSB(hdrSS.src));
if (num>0) then begin;
  con:=ConnectionDat^[num];
  getSeq;
  end;

case (hdrSS.typ shr 1) and $f of
  4:begin; {dataack}
    if (num<1) then begin; i:=3;goto reset; end;
    con.timeR:=currentTime;
    con.timeT:=-9999999;
    pipeLineSend(con.pipe,buf[beg],siz);
    con.nxtOp:=processOptions(a);
    goto update;
    end;
  2:begin; {data}
    if (num<1) then begin; i:=3;goto reset; end;
    con.timeR:=currentTime;
    con.timeT:=-9999999;
    pipeLineSend(con.pipe,buf[beg],siz);
    con.nxtOp:=processOptions(a);
    goto update;
    end;
  3:begin; {ack}
    if (num<1) then begin; i:=3;goto reset; end;
    con.timeR:=currentTime;
    con.nxtOp:=processOptions(a);
    goto update;
    end;
  5,6:begin; {closereq, close}
    if (num<1) then begin; i:=3;goto reset; end;
    con.stat:=0;
    sendOneDCCPpacket(con,#1#0#0#0+processOptions(a),a,$307,0);
    con.nxtOp:='';
    goto update;
    end;
  7:begin; {reset}
    if (num<1) then exit;
    processOptions(copy(a,5,666));
    con.nxtOp:='';
    con.stat:=0;
    goto update;
    end;
  8:begin; {sync}
    if (num<1) then begin; i:=3;goto reset; end;
    sendOneDCCPpacket(con,processOptions(a),a,$309,0);
    con.nxtOp:='';
    goto update;
    end;
  9:begin; {syncack}
    if (num<1) then begin; i:=3;goto reset; end;
    con.nxtOp:=processOptions(a);
    goto update;
    end;
  1:begin; {response}
    if (num<1) then begin; i:=3;goto reset; end;
    if (con.stat<>4) then exit;
    fillchar(tcpDat,sizeof(tcpDat),0);
    tcpDat.cmd:=3;
    move(localAddr,tcpDat.adr,sizeof(tcpDat.adr));
    tcpDat.prt:=con.portL;
    tcpDat.dat:=0;
    pipeLineSend(con.pipe,tcpDat,sizeof(tcpDat));
    con.stat:=3;
    con.timeR:=currentTime;
    con.timeT:=-9999999;
    con.nxtOp:=processOptions(copy(a,5,666));
    goto update;
    end;
  0:begin; {request}
    p:=ReadWordMSB(hdrSS.trg);
    if (num<1) then begin;
      num:=findOneListening(p);
      if (num<1) then begin; i:=7;goto reset; end;
      con:=ConnectionDat^[num];
      if ResizeMem(ConnectionNum+1) then begin; i:=9;goto reset; end;
      num:=ConnectionNum;
      con.seqTa:=random($fffffff);
      con.seqTb:=random($fff);
      o:=ReadWordMSB(hdrSS.src);
      con.portL:=p;
      con.portR:=o;
      WriteLn('connection accepted from '+ipAddr2string(addr)+' '+BStr(o)+' to '+BStr(p));
      fillchar(tcpDat,sizeof(tcpDat),0);
      tcpDat.cmd:=3;
      move(addr,tcpDat.adr,sizeof(tcpDat.adr));
      tcpDat.prt:=o;
      tcpDat.dat:=p;
      move(con.addr,i,sizeof(i));
      if (pipeLineCreate(con.pipe,con.proc,i,true)<>0) then begin;
        ResizeMem(ConnectionNum-1);
        i:=9;
        goto reset;
        end;
      pipeLineSend(con.pipe,tcpDat,sizeof(tcpDat));
      move(addr,con.addr,sizeof(con.addr));
      con.timeR:=currentTime;
      con.timeT:=currentTime;
      con.stat:=3;
      end;
    con.srvc:=ReadLongMSB(a[1]);
    getSeq;
    sendOneDCCPpacket(con,copy(a,1,4)+processOptions(copy(a,5,666)),a,$301,0);
    con.nxtOp:='';
    goto update;
    end;
  else begin; i:=4;goto reset; end;
  end;

i:=0;
reset:
if (num>0) then goto f1;
move(addr,con.addr,sizeof(con.addr));
con.portL:=ReadWordMSB(hdrSS.trg);
con.portR:=ReadWordMSB(hdrSS.src);
con.seqRa:=0;
con.seqRb:=0;
getSeq;
if not ((hdrSS.typ shr 1) and $f in [1,3..7]) then begin;
  con.seqTa:=0;
  con.seqTb:=0;
  goto f1;
  end;
if (hdrSS.typ and 1=1) then begin;
  con.seqTa:=ReadLongMSB(hdrED.ackLo);
  con.seqTb:=ReadWordMSB(hdrED.ackHi);
  end else begin;
  con.seqTa:=ReadLongMSB(hdrSD.res1) and $ffffff;
  con.seqTb:=0;
  end;
f1:
sendOneDCCPpacket(con,chr(i)+#0#0#0,a,$307,0);
exit;
update:
ConnectionDat^[num]:=con;
End;
