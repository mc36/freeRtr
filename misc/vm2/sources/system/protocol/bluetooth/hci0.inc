Type
  oneAddressRecord=array[1..6] of byte;
  oneCommandHeaderRecord=record
    hci:byte; {1}
    opc:word;
    len:byte;
    end;
  oneACLdataHeaderRecord=record
    hci:byte; {2}
    hnd:word;
    len:word;
    end;
  oneSCOdataHeaderRecord=record
    hci:byte; {3}
    hnd:word;
    len:byte;
    end;
  oneEventHeaderRecord=record
    hci:byte; {4}
    opc:byte;
    len:byte;
    end;
  oneLCPdataHeaderRecord=record
    hdr:oneACLdataHeaderRecord;
    len:word;
    chn:word;
    end;
  oneSIGdataHeaderRecord=record
    hdr:oneLCPdataHeaderRecord;
    cod:byte;
    id:byte;
    len:word;
    end;
  oneHCIconnectionRecord=record
    stt:LongInt;                {1-ready, 2-closing, 11-incoming, 21-outgoing}
    adr:oneAddressRecord;       {address}
    hnd:LongInt;                {handle}
    tim:LongInt;                {time}
    slt:LongInt;                {slots ready}
    end;
  oneLCPconnectionRecord=record
    stt:LongInt;                {0-off, 1-ready, 2-disc, 3-dscBeg}
                                {11-config}
                                {21-cmd, 22-scan, 23-name}
                                {31-handle, 32-connreq}
    saw:LongInt;                {1-cfgReq, 2-cfgRep}
    adr:oneAddressRecord;       {address}
    hnd:LongInt;                {handle}
    chd:LongInt;                {cached}
    psm:LongInt;                {psm value}
    loc:LongInt;                {local channel}
    rem:LongInt;                {remote channel}
    tim:LongInt;                {time}
    try:LongInt;                {retry}
    pip:LongInt;                {pipe}
    end;
  onePSMconnectionRecord=record
    psm:LongInt;                {psm value}
    pip:LongInt;                {pipe}
    prc:LongInt;                {process number}
    end;
Const
  maxHCIconns=64;
  maxLCPconns=512;
  maxPSMconns=32;
Var
  localPipe:LongInt;
  localName:String;
  localDevice:String;
  localPinCode:String;
  localAddr:oneAddressRecord;
  localAmtu,localSmtu:LongInt;
  localApkt,localSpkt:LongInt;
  hciConnNum:LongInt;
  hciConnDat:array[1..maxHCIconns] of oneHCIconnectionRecord;
  lcpConnNum:LongInt;
  lcpConnDat:array[1..maxLCPconns] of oneLCPconnectionRecord;
  psmConnNum:LongInt;
  psmConnDat:array[1..maxPSMconns] of onePSMconnectionRecord;



Function hciDecodeError(i:LongInt):String;
Var a:String;
Begin;
case i of
  $01:a:='Unknown HCI Command';
  $02:a:='Unknown Connection Identifier';
  $03:a:='Hardware Failure';
  $04:a:='Page Timeout';
  $05:a:='Authentication Failure';
  $06:a:='PIN or key Missing';
  $07:a:='Memory Capacity Exceeded';
  $08:a:='Connection Timeout';
  $09:a:='Connection Limit Exceeded';
  $0A:a:='Synchronous Connection Limit to a Device Exceeded';
  $0B:a:='ACL Connection Already Exists';
  $0C:a:='Command Disallowed';
  $0D:a:='Connection Rejected due to Limited Resources';
  $0E:a:='Connection Rejected due to Security Reasons';
  $0F:a:='Connection Rejected due to Unacceptable BD_ADDR';
  $10:a:='Connection Accept Timeout Exceeded';
  $11:a:='Unsupported Feature or Parameter Value';
  $12:a:='Invalid HCI Command Parameters';
  $13:a:='Remote User Terminated Connection';
  $14:a:='Remote Device Terminated Connection due to Low Resources';
  $15:a:='Remote Device Terminated Connection due to Power Off';
  $16:a:='Connection Terminated by Local Host';
  $17:a:='Repeated Attempts';
  $18:a:='Pairing not Allowed';
  $19:a:='Unknown LMP PDU';
  $1A:a:='Unsupported Remote Feature / Unsupported LMP Feature';
  $1B:a:='SCO Offset Rejected';
  $1C:a:='SCO Interval Rejected';
  $1D:a:='SCO Air Mode Rejected';
  $1E:a:='Invalid LMP Parameters';
  $1F:a:='Unspecified Error';
  $20:a:='Unsupported LMP Parameter Value';
  $21:a:='Role Change Not Allowed';
  $22:a:='LMP Response Timeout';
  $23:a:='LMP Error Transaction Collision';
  $24:a:='LMP PDU Not Allowed';
  $25:a:='Encryption Mode Not Acceptable';
  $26:a:='Link Key Can Not be Changed';
  $27:a:='Requested QoS Not Supported';
  $28:a:='Instant Passed';
  $29:a:='Pairing with Unit Key Not Supported';
  $2A:a:='Different Transaction Collision';
  $2C:a:='QoS Unacceptable Parameter';
  $2D:a:='QoS Rejected';
  $2E:a:='Channel Assessment Not Supported';
  $2F:a:='Insufficient Security';
  $30:a:='Parameter out of Mandatory Range';
  $32:a:='Role Switch Pending';
  $34:a:='Reserved Slot Violation';
  $35:a:='Role Switch Failed';
  $36:a:='Extended Inquiry Response Too Large';
  $37:a:='Simple Pairing Not Supported By Host';
  $38:a:='Host Busy-Pairing';
  $39:a:='Connection Rejected Due To No Suitable Channel Found';
  $3A:a:='Controller Busy';
  else a:='unknown error:'+BStr(i);
  end;
hciDecodeError:=a;
End;


Procedure byteSwapHCIaddress(var srcaddr,trgaddr);
Var
  src:oneAddressRecord absolute srcaddr;
  trg:oneAddressRecord absolute trgaddr;
  i:LongInt;
Begin;
for i:=1 to sizeof(src) do trg[sizeof(src)+1-i]:=src[i];
end;



Function convertHCIaddress(var buf):String;
Var
  adr:oneAddressRecord absolute buf;
  i:LongInt;
  a:String;
Begin;
a:='';
for i:=1 to sizeof(adr) do a:=a+byte2hextype(adr[i]);
convertHCIaddress:=a;
End;



Function compareHCIaddress(var adr1;var adr2):Boolean;
Var
  a:oneAddressRecord absolute adr1;
  b:oneAddressRecord absolute adr2;
  i:LongInt;
Begin;
compareHCIaddress:=false;
for i:=1 to sizeof(a) do if (a[i]<>b[i]) then exit;
compareHCIaddress:=true;
End;



Function findOneHCIconnA(var addr):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to hciConnNum do if compareHCIaddress(addr,hciConnDat[i].adr) then goto f1;
i:=0;
f1:
findOneHCIconnA:=i;
End;



Function findOneHCIconnH(hnd:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to hciConnNum do if (hnd=hciConnDat[i].hnd) then goto f1;
i:=0;
f1:
findOneHCIconnH:=i;
End;



Function findOnePSMconn(psm:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to psmConnNum do if (psmConnDat[i].psm=psm) then goto f1;
i:=0;
f1:
findOnePSMconn:=i;
End;



Procedure closeHCIconnLCPs(hnd:LongInt);
Var i:LongInt;
Begin;
if (hnd=0) then exit;
for i:=lcpConnNum downto 1 do begin;
  if (lcpConnDat[i].hnd<>hnd) then continue;
  pipeLineClose(lcpConnDat[i].pip);
  lcpConnDat[i]:=lcpConnDat[lcpConnNum];
  dec(lcpConnNum);
  end;
End;




Function findOneLCPconnHR(hnd,cid:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to lcpConnNum do begin;
  if (lcpConnDat[i].hnd<>hnd) then continue;
  if (lcpConnDat[i].rem<>cid) then continue;
  goto f1;
  end;
i:=0;
f1:
findOneLCPconnHR:=i;
End;



Function findOneLCPconnL(cid:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
for i:=1 to lcpConnNum do if (lcpConnDat[i].loc=cid) then goto f1;
i:=0;
f1:
findOneLCPconnL:=i;
End;



Function findUnusedConnID:LongInt;
Label f1;
Var i:LongInt;
Begin;
f1:
i:=random($f000)+$100;
if (findOneLCPconnL(i)<>0) then goto f1;
findUnusedConnID:=i;
End;





Procedure acceptIncomingConns;
Var i,o,p:LongInt;
Begin;
if (lcpConnNum>=maxLCPconns) then exit;
if (pipeLineGetIncoming(p)<>0) then p:=0;
if (p=0) then exit;
inc(lcpConnNum);
fillchar(lcpConnDat[lcpConnNum],sizeof(lcpConnDat[lcpConnNum]),0);
lcpConnDat[lcpConnNum].stt:=21;
lcpConnDat[lcpConnNum].pip:=p;
lcpConnDat[lcpConnNum].tim:=-99999999;
End;



Procedure psmReleq2conns;
Var i,o,p:LongInt;
Begin;
for i:=psmConnNum downto 1 do begin;
  pipeLineStats(psmConnDat[i].pip,p,o,o);
  if (p<>0) then continue;
  pipeLineClose(psmConnDat[i].pip);
  psmConnDat[i]:=psmConnDat[psmConnNum];
  dec(psmConnNum);
  end;
End;
