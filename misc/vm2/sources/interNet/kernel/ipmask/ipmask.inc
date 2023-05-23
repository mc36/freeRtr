Type
  OneTCPheaderRecord=record
    PrtSrc:Word;                {source port}
    PrtTrg:Word;                {target port}
    PckSeq:LongInt;             {data sequence}
    PckAck:LongInt;             {ack sequence}
    DatOfs:Byte;                {upper 4 bits}
    Flags:Byte;                 {lower 6 bits}
    Window:Word;                {window size}
    ChkSum:Word;                {checksum}
    Urgent:Word;                {urgent pointer}
    end;
  OneUDPheaderRecord=record
    Src:Word;                   {source port}
    Trg:Word;                   {traget port}
    Len:Word;                   {length}
    Sum:Word;                   {checksum}
    end;


Procedure ProcessUpperStream(var pck:OnePacketRecord);
Label f1;
Var i:LongInt;
Begin;
if (PrtclPipe=0) then exit;
f1:
i:=sizeof(pck);
if (pipeLineRecv(PrtclPipe,pck,i)<>0) then exit;
if (i<1) then exit;
pipeLineSend(LowerPipeline,pck,i);
{$ifdef debug}WriteLn('upper sent '+ipAddr2string(pck.b)+' --> '+ipAddr2string(pck.a));{$endif}
lastSent:=LowerProcess;
goto f1;
End;



Procedure gotOnePacket(var pck:OnePacketRecord;proto:LongInt);
Label loc,rem,sup,snd,loc1,loc2;
Var
  tcp:OneTCPheaderRecord;
  udp:OneUDPheaderRecord;
  con:OneConnectionRecord;
  prtSrc,prtTrg:LongInt;
  i,o,p:LongInt;
Begin;
case proto of
  1:begin; {tcp}
    if (pck.s<sizeof(tcp)) then begin;
      WriteLn('TCP got a too short packet from '+ipAddr2string(pck.a));
      exit;
      end;
    if (TestPseudoSum(pck)<>$ffff) then begin;
      WriteLn('TCP got wrong checksum from '+ipAddr2string(pck.a));
      exit;
      end;
    move(pck.d,tcp,sizeof(tcp));
    o:=(tcp.DatOfs shr 4)*4;
    p:=pck.s-o;
    if (o<sizeof(tcp)) or (p<0) then begin;
      WriteLn('TCP got wrong data offset from '+ipAddr2string(pck.a));
      exit;
      end;
    move(pck.d[o+1],pck.d[sizeof(tcp)+1],p);
    o:=sizeof(tcp);
    if (tcp.Flags and $02<>0) then begin; {syn}
      move(pck.d[o+1],pck.d[o+5],p);
      pck.d[o+1]:=2;
      pck.d[o+2]:=4;
      WriteWordMSB(pck.d[o+3],1024);
      inc(o,4);
      end;
    tcp.DatOfs:=(o shr 2) shl 4;
    tcp.window:=$6060;
    pck.s:=o+p;
    prtTrg:=ReadWordMSB(tcp.prtTrg);
    prtSrc:=ReadWordMSB(tcp.prtSrc);
    end;
  2:begin; {udp}
    if (pck.s<sizeof(udp)) then begin;
      WriteLn('UDP got a too short packet from '+ipAddr2string(pck.a));
      exit;
      end;
    move(pck.d,udp,sizeof(udp));
    p:=ReadWordMSB(udp.len);
    if (p<sizeof(udp)) then begin;
      WriteLn('UDP got a too short packet from '+ipAddr2string(pck.a));
      exit;
      end;
    pck.s:=p;
    if (udp.sum=0) then begin;
      end else if (TestPseudoSum(pck)<>$ffff) then begin;
      WriteLn('UDP got wrong checksum from '+ipAddr2string(pck.a));
      exit;
      end;
    prtTrg:=ReadWordMSB(udp.trg);
    prtSrc:=ReadWordMSB(udp.src);
    end;
  else goto sup;
  end;

if IsAddressInMask(pck.b,localAddr,localMask) then goto sup;
if IsAddressInMask(pck.a,LocalAddr,LocalMask) then goto loc else goto rem;
exit;

sup:
pck.b:=XtrnlAddr;
lastSent:=PrtclProc;
p:=PrtclPipe;
snd:
if (p=0) then exit;
case proto of
  1:begin; {tcp}
    WriteWordMSB(tcp.prtSrc,prtSrc);
    WriteWordMSB(tcp.prtTrg,prtTrg);
    tcp.ChkSum:=0;
    move(tcp,pck.d,sizeof(tcp));
    tcp.ChkSum:=not TestPseudoSum(pck);
    move(tcp,pck.d,sizeof(tcp));
    end;
  2:begin; {udp}
    WriteWordMSB(udp.src,prtSrc);
    WriteWordMSB(udp.trg,prtTrg);
    udp.sum:=0;
    move(udp,pck.d,sizeof(udp));
    udp.sum:=not TestPseudoSum(pck);
    move(udp,pck.d,sizeof(udp));
    end;
  end;
pipeLineSend(p,pck,pck.s+33);
exit;


rem:
{$ifdef debug}WriteLn('got packet from external: '+ipAddr2string(pck.a)+' --> '+ipAddr2string(pck.b));{$endif}
p:=FindOneExternalSide(pck.a,prtSrc,prtTrg,proto);
if (p=0) then goto sup;
ConnectionDat^[p].time:=currentTime;
con:=ConnectionDat^[p];
pck.b:=con.extA;
pck.a:=con.locA;
prtSrc:=con.extP;
prtTrg:=con.locP;
{$ifdef debug}WriteLn('sending '+ipAddr2string(pck.b)+' --> '+ipAddr2string(pck.a));{$endif}
lastSent:=LowerProcess;
p:=LowerPipeline;
goto snd;

loc:
{$ifdef debug}WriteLn('got packet from local: '+ipAddr2string(pck.a)+' --> '+ipAddr2string(pck.b));{$endif}
if IsAddressInMask(pck.b,XtrnlAddr,WholeMask) then goto sup;
p:=FindOneLocalSide(pck.a,pck.b,prtSrc,prtTrg,proto);
if (p<>0) then goto loc1;
fillchar(con,sizeof(con),0);
con.prot:=proto;
con.time:=currentTime;
con.locA:=pck.a;
con.extA:=pck.b;
con.locP:=prtSrc;
con.extP:=prtTrg;
i:=32;
loc2:
dec(i);
if (i<1) then begin;
  WriteLn('failed to allocate port!');
  exit;
  end;
p:=$7ff0+Random($8000);
if (FindOneExternalSide(con.extA,prtTrg,p,proto)<>0) then goto loc2;
con.emuP:=p;
if ResizeMem(ConnectionNum+1) then begin;
  WriteLn('failed to allocate memory!');
  exit;
  end;
p:=ConnectionNum;
ConnectionDat^[p]:=con;
{$ifdef debug}WriteLn('generated new context!');{$endif}
loc1:
ConnectionDat^[p].time:=currentTime;
con:=ConnectionDat^[p];
pck.b:=XtrnlAddr;
pck.a:=con.extA;
prtSrc:=con.emuP;
prtTrg:=con.extP;
{$ifdef debug}WriteLn('sending '+ipAddr2string(pck.b)+' --> '+ipAddr2string(pck.a));{$endif}
lastSent:=LowerProcess;
p:=LowerPipeline;
goto snd;
End;
