Var
  DriveSize:LongInt;
  DrivePipe:LongInt;
  ServerPipe:LongInt;
  commandSeq:LongInt;
  targetName:String;
  targetDevid:String;
  targetLun:LongInt;
Type OneSectorRecord=array[1..512] of byte;


Function doOneSCSIrequest(var buffer;var size:LongInt;read:LongInt):LongInt;
Label f1;
Var
  buf:array[1..1] of byte absolute buffer;
  scsiReq:OneScsiReqHeaderRecord absolute buffer;
  scsiRep:OneScsiRepHeaderRecord absolute buffer;
  scsiDat:OneScsiDataHeaderRecord absolute buffer;
  i,o,p:LongInt;
Begin;
doOneSCSIrequest:=-1;
inc(commandSeq);
move(buf,scsiReq.opcodat,size+16);
if (size<0) then size:=0;
fillchar(scsiReq,sizeof(scsiReq)-16,0);
scsiReq.opcode:=$01;
if (read>0) then scsiReq.attrib:=$c0 else scsiReq.attrib:=$a1;
WriteLongMSB(scsiReq.ashlen,size);
WriteLongMSB(scsiReq.task,commandSeq);
if (read<0) then WriteLongMSB(scsiReq.expDatLen,-read) else WriteLongMSB(scsiReq.expDatLen,read);
scsiReq.cmdSN:=scsiReq.task;
WriteLongMSB(scsiReq.statSN,commandSeq+1);
pipeLineSend(ServerPipe,buf,size+sizeof(scsiReq));
size:=0;
p:=pipeGetPacket(buf,ServerPipe);
if (p<1) then immErr('error completing request!');
if (buf[1] and $3f<>$25) then goto f1;
if (ReadLongMSB(scsiRep.task)<>commandSeq) then immErr('got bad task id!');
size:=p-sizeof(scsiReq);
move(buf[sizeof(scsiReq)+1],buf,size);
p:=pipeGetPacket(buf[size+1],ServerPipe);
if (p<1) then immErr('error completing request!');
f1:
if (buf[size+1] and $3f<>$21) then immErr('invalid response arrived!');
doOneSCSIrequest:=ReadWordMSB(buf[size+3]);
End;



Procedure OpenOneConnection;
Var
  a:String;
  i,o:LongInt;
Begin;
pipeLineClose(ServerPipe);
Write('connecting '+ipAddr2string(serverAddr)+' '+BStr(serverPort));
TCPbeginConnect(ServerPipe,65536,serverAddr,serverPort);
while TCPlookConnected(ServerPipe,a,i,o) do begin;
  if (ServerPipe=0) then immErr(' failed!');
  relequish;
  end;
WriteLn(' ok!');
WriteLn('local side is '+ipAddr2string(a)+' '+BStr(i)+'...');
End;



Procedure doOneLoginProcedure;
Var
  buf:array[1..4*1024] of byte;
  hdrBas:OneBasicHeaderRecord absolute buf;
  loginReq:OneLoginReqHeaderRecord absolute buf;
  loginRep:OneLoginRepHeaderRecord absolute buf;
  textReq:OneTextReqHeaderRecord absolute buf;
  textRep:OneTextRepHeaderRecord absolute buf;
  logoutReq:OneLogoutReqHeaderRecord absolute buf;
  logoutRep:OneLogoutRepHeaderRecord absolute buf;
  i,o,p,s:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Function getNextAsciiz(var ps:LongInt):String;
Label f1;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
ab0:=0;
f1:
inc(ps);
i:=buf[ps];
if (ps>s) then i:=0;
if (i<>0) then begin;
  inc(ab0);
  ab[ab0]:=i;
  goto f1;
  end;
getNextAsciiz:=a;
End;

Function getTextFieldsSaw:LongInt;
Var i,o,p:LongInt;
Begin;
p:=sizeof(hdrBas);
o:=0;
while (p<s) do o:=o or iscsiTextFieldToNumber(getNextAsciiz(p));
getTextFieldsSaw:=o;
End;

Function getValueOfTextField(c:String):String;
Var
  i,o,p:LongInt;
  a,b:String;
Begin;
p:=sizeof(hdrBas);
c:=kicsi(c);
while (p<s) do begin;
  b:=getNextAsciiz(p);
  i:=pos('=',b);
  if (i<1) then i:=666;
  a:=kicsi(copy(b,1,i-1));
  if (a<>c) then continue;
  getValueOfTextField:=copy(b,i+1,666);
  exit;
  end;
getValueOfTextField:='';
End;

Procedure addOneTextLine(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
inc(p);
move(ab[1],buf[p],ab0);
inc(p,ab0);
buf[p]:=0;
End;

Procedure doExchange;
Begin;
inc(commandSeq);
Write('doing exchange...');
WriteLongMSB(loginReq.task,commandSeq);
loginReq.cmdSN:=loginReq.task;
loginReq.expStatSN:=loginReq.task;
WriteLongMSB(loginReq.ashlen,p-sizeof(hdrBas));
while (p and 3<>0) do addOneTextLine('');
pipeLineSend(ServerPipe,buf,p);
s:=pipeGetPacket(buf,ServerPipe);
if (s<1) then immErr(' error!');
WriteLn(' ok!');
End;

Begin;
OpenOneConnection;

fillchar(hdrBas,sizeof(hdrBas),0);
p:=sizeof(hdrBas);
loginReq.opcode:=$43;
loginReq.attrib:=$81;
for i:=1 to 6 do loginReq.isid[i]:=i;
WriteWordMSB(loginReq.cid,1);
addOneTextLine('InitiatorName=iqn.2000-05.hu.nop:'+serverName);
addOneTextLine('SessionType=Discovery');
addOneTextLine('AuthMethod=None');
doExchange;dec(commandSeq);
WriteLn('portal='+getValueOfTextField('TargetPortalGroupTag'));

fillchar(hdrBas,sizeof(hdrBas),0);
p:=sizeof(hdrBas);
loginReq.opcode:=$43;
loginReq.attrib:=$87;
for i:=1 to 6 do loginReq.isid[i]:=i;
WriteWordMSB(loginReq.cid,1);
addOneTextLine('HeaderDigest=None');
addOneTextLine('DataDigest=None');
addOneTextLine('MaxRecvDataSegmentLength=65536');
addOneTextLine('DefaultTime2Wait=0');
addOneTextLine('DefaultTime2Retain=60');
doExchange;dec(commandSeq);

fillchar(hdrBas,sizeof(hdrBas),0);
p:=sizeof(hdrBas);
textReq.opcode:=$04;
textReq.attrib:=$80;
textReq.target:=-1;
addOneTextLine('SendTargets=All');
doExchange;
targetName:=getValueOfTextField('TargetName');
WriteLn('target='+targetName);

OpenOneConnection;

fillchar(hdrBas,sizeof(hdrBas),0);
p:=sizeof(hdrBas);
loginReq.opcode:=$43;
loginReq.attrib:=$81;
for i:=1 to 6 do loginReq.isid[i]:=i;
WriteWordMSB(loginReq.cid,1);
addOneTextLine('InitiatorName=iqn.2000-05.hu.nop:'+serverName);
addOneTextLine('SessionType=Normal');
addOneTextLine('TargetName='+targetName);
addOneTextLine('AuthMethod=None');
doExchange;dec(commandSeq);

fillchar(hdrBas,sizeof(hdrBas),0);
p:=sizeof(hdrBas);
loginReq.opcode:=$43;
loginReq.attrib:=$87;
for i:=1 to 6 do loginReq.isid[i]:=i;
WriteWordMSB(loginReq.cid,1);
addOneTextLine('HeaderDigest=None');
addOneTextLine('DataDigest=None');
addOneTextLine('ErrorRecoveryLevel=2');
addOneTextLine('InitialR2T=No');
addOneTextLine('ImmediateData=Yes');
addOneTextLine('MaxRecvDataSegmentLength=65536');
addOneTextLine('MaxBurstLength=65536');
addOneTextLine('FirstBurstLength=65536');
addOneTextLine('MaxConnections=1');
addOneTextLine('DataPDUInOrder=Yes');
addOneTextLine('DataSequenceInOrder=Yes');
addOneTextLine('DefaultTime2Wait=0');
addOneTextLine('DefaultTime2Retain=60');
addOneTextLine('MaxOutstandingR2T=16');
doExchange;dec(commandSeq);

fillchar(buf,32,0);i:=0;
buf[1]:=$a0;
buf[10]:=$10;
if (doOneSCSIrequest(buf,i,16)<>0) then immErr('error getting lun!');
targetLun:=buf[10];
WriteLn('lun='+BStr(targetLun));

fillchar(buf,32,0);i:=0;
buf[1]:=$12;
buf[2]:=targetLun shl 5;
buf[5]:=$24;
if (doOneSCSIrequest(buf,i,buf[5])<>0) then immErr('error getting inquiry!');
ab0:=28;
move(buf[9],ab[1],ab0);
for i:=1 to ab0 do if (ab[0] in [0,255]) then ab[i]:=32;
targetDevid:=copy(a,1,8)+' '+copy(a,9,16)+' '+copy(a,25,4);
WriteLn('device='+targetDevid);

fillchar(buf,32,0);i:=0;
buf[1]:=$25;
buf[2]:=targetLun shl 5;
if (doOneSCSIrequest(buf,i,8)<>0) then immErr('error getting capacity!');
DriveSize:=ReadLongMSB(buf[1]);
if (ReadLongMSB(buf[5])<>sizeof(OneSectorRecord)) then immErr('invalid block size!');
WriteLn('blocks='+BStr(DriveSize));
End;



Function doOneRead(sec:LongInt;var buffer):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o:LongInt;
Begin;
doOneRead:=1;
if (sec<0) or (sec>=DriveSize) then exit;
fillchar(buf,16,0);
buf[1]:=$28;
buf[2]:=targetLun shl 5;
buf[9]:=1;
WriteLongMSB(buf[3],sec);
i:=0;
if (doOneSCSIrequest(buf,i,sizeof(OneSectorRecord))<>0) then exit;
if (i<>sizeof(OneSectorRecord)) then exit;
doOneRead:=0;
End;


Function doOneWrite(sec:LongInt;var buffer):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o:LongInt;
Begin;
doOneWrite:=1;
if (sec<0) or (sec>=DriveSize) then exit;
move(buf,buf[17],sizeof(OneSectorRecord));
fillchar(buf,16,0);
buf[1]:=$2a;
buf[2]:=targetLun shl 5;
buf[9]:=1;
WriteLongMSB(buf[3],sec);
i:=sizeof(OneSectorRecord);
if (doOneSCSIrequest(buf,i,-sizeof(OneSectorRecord))<>0) then exit;
if (i<>0) then exit;
doOneWrite:=0;
End;



Procedure serveOneClient(pipe:LongInt);
Label f1;
Const
  MaxHed=2;
  MaxSec=18;
Var
  buf:array[1..4*1024] of byte;
  cmd:longint absolute buf;
  i,o,p:LongInt;
  a:String;
Begin;
WriteLn('incoming connection...');
i:=1;pipeLineSend(pipe,i,sizeof(i));
p:=16;
repeat
  dec(p);
  if (p<0) then exit;
  i:=sizeof(buf);
  pipeLineRecv(pipe,buf,i);
  if (i=0) then begin; relequish;continue; end;
  if (i<>4) then exit;
  break;
  until (i=0);
WriteLn('serving drive...');

f1:
i:=sizeof(buf);
pipeLineRecv(pipe,buf,i);
if (i=0) then begin;
  if (pipeLineStats(pipe,i,o,p)<>0) then exit;
  if (i=0) then exit;
  relequish;
  goto f1;
  end;
case cmd of
  1:begin;{read one sector}
    move(buf[5],p,sizeof(p));
    {$ifdef debug}Write('read #'+BStr(p));{$endif}
    cmd:=doOneRead(p,buf[9]);
    {$ifdef debug}WriteLn(' res='+BStr(cmd));{$endif}
    if (pipeLineSend(pipe,buf,520)<>0) then exit;
    end;
  2:begin;{write one sector}
    move(buf[5],p,sizeof(p));
    {$ifdef debug}Write('write #'+BStr(p));{$endif}
    cmd:=doOneWrite(p,buf[9]);
    {$ifdef debug}WriteLn(' res='+BStr(cmd));{$endif}
    if (pipeLineSend(pipe,buf,8)<>0) then exit;
    end;
  0:begin;{identify drive}
    i:=0;
    move(i,buf[5],sizeof(i));
    i:=(DriveSize div (MaxHed*MaxSec))+1;
    move(i,buf[9],sizeof(i));
    i:=MaxHed;
    move(i,buf[13],sizeof(i));
    i:=MaxSec;
    move(i,buf[17],sizeof(i));
    i:=0;
    move(i,buf[21],sizeof(i));
    move(DriveSize,buf[25],sizeof(DriveSize));
    a:='iscsi with '+ipAddr2string(serverAddr)+' '+BStr(serverPort)
    move(a,buf[29],sizeof(a));
    a:=targetName;
    move(a,buf[285],sizeof(a));
    a:=targetDevid;
    move(a,buf[541],sizeof(a));
    cmd:=0;
    if (pipeLineSend(pipe,buf,$31c)<>0) then exit;
    end;
  else begin;
    writeln('got unknown command: '+BStr(cmd));
    exit;
    end;
  end;
goto f1;
End;
