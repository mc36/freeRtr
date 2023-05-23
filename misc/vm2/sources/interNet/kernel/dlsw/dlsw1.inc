Type
  OneDLSWheaderRecord=record
    version:Byte;                       {version}
    headLen:Byte;                       {header size}
    dataLen:Word;                       {message size}
    remDLC:LongInt;                     {remote dlc id}
    remDLCpid:LongInt;                  {remote dlc pid}
    res1:Word;                          {reserved}
    msgType:Byte;                       {message type}
    flowCtrl:Byte;                      {flow control}
    protID:Byte;                        {protocol id}
    headNum:Byte;                       {header number}
    res2:Word;                          {reserved}
    maxFrane:Byte;                      {largest frame size}
    sspFlags:Byte;                      {ssp flags}
    circPri:Byte;                       {circuit priority}
    oldMsgTp:Byte;                      {old message type}
    trgMac:oneMacAddressRecord;         {target mac address}
    srcMac:oneMacAddressRecord;         {source mac address}
    srcSap:Byte;                        {source sap value}
    trgSap:Byte;                        {target sap value}
    direct:Byte;                        {frame direction}
    res3:array[1..3] of byte;           {reserved}
    dlcHedLen:Word;                     {dlc header length}
    srcDLCprt:LongInt;                  {source dlc port id}
    srcDLC:LongInt;                     {source dlc id}
    srcDLCtrn:LongInt;                  {source dlc transport}
    trgDLCprt:LongInt;                  {source dlc port id}
    trgDLC:LongInt;                     {source dlc id}
    trgDLCtrn:LongInt;                  {source dlc transport}
    res4:LongInt;                       {reserved}
    end;


Procedure addTLVdata(var buffer;var ps:LongInt;t:Byte;a:String);
Var
  buf:Array[1..1] of byte absolute buffer;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
inc(ps);
buf[ps]:=ab0+2;
inc(ps);
buf[ps]:=t;
move(ab[1],buf[ps+1],ab0);
inc(ps,ab0);
End;




Procedure gotIncoming(p:LongInt);
Var
  i,o:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
pipeLineStats(p,o,i,i);
if (o=TCPprocessId) then begin;
  CurrentPipeLine:=p;
  exit;
  end;
o:=0;
for i:=ConnectionNum downto 1 do if (ConnectionDat^[i].uppr=0) then o:=i;
if (o<1) then begin;
  pipeLineClose(p);
  exit;
  end;
ConnectionDat^[o].uppr:=p;
ConnectionDat^[o].stat:=3;
ab0:=sizeof(myMACaddress);
move(myMACaddress,ab[1],sizeof(myMACaddress));
a:='12341234'#0#0#0#0#0#0#0#0+a+#255#255#255#255#255#255'dlsw with '+ipAddr2string(ConnectionDat^[o].addr)+#0;
i:=6;move(i,a[1],sizeof(i));
i:=1500;move(i,a[5],sizeof(i));
pipeLineSend(p,a[1],length(a));
End;




Procedure releq2upp(var con:OneConnectionRecord);
Var
  buf:array[1..4*1024] of byte;
  hdr:OneDLSWheaderRecord;
  i,o,p:LongInt;
Begin;
if (con.uppr=0) then exit;
p:=sizeof(buf);
if (pipeLineRecv(con.uppr,buf,p)<>0) then p:=0;
if (p<1) then begin;
  pipeLineStats(con.uppr,o,i,i);
  if (o<>0) then exit;
  WriteLn('upper exited with '+ipAddr2string(con.addr));
  pipeLineClose(con.uppr);
  con.uppr:=0;
  exit;
  end;
fillchar(hdr,sizeof(hdr),0);
hdr.version:=$31;
hdr.headLen:=sizeof(hdr);
WriteWordMSB(hdr.dataLen,p-4);
hdr.msgType:=$14;
hdr.flowCtrl:=0;
hdr.protID:=$42;
hdr.headNum:=1;
hdr.maxFrane:=57;
hdr.oldMsgTp:=$14;
move(buf,hdr.trgMac,sizeof(myMACaddress));
move(myMACaddress,hdr.srcMac,sizeof(myMACaddress));
hdr.srcSap:=$aa;
hdr.trgSap:=$aa;
hdr.direct:=1;
pipeLineSend(con.pipe2,hdr,sizeof(hdr));
pipeLineSend(con.pipe2,buf[5],p-4);
con.time:=currentTime;
End;
